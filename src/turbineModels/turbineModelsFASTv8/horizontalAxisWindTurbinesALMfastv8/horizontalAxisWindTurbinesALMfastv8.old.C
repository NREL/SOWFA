/*---------------------------------------------------------------------------*\
This file was modified or created at the National Renewable Energy
Laboratory (NREL) on January 6, 2012 in creating the SOWFA (Simulator for
Offshore Wind Farm Applications) package of wind plant modeling tools that
are based on the OpenFOAM software. Access to and use of SOWFA imposes
obligations on the user, as set forth in the NWTC Design Codes DATA USE
DISCLAIMER AGREEMENT that can be found at
<http://wind.nrel.gov/designcodes/disclaimer.html>.
\*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2011 OpenFOAM Foundation
     \\/     M anipulation  |
-------------------------------------------------------------------------------
License
    This file is part of OpenFOAM.

    OpenFOAM is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    OpenFOAM is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License
    along with OpenFOAM.  If not, see <http://www.gnu.org/licenses/>.

\*---------------------------------------------------------------------------*/

#include "horizontalAxisWindTurbinesALMfastv8.H"
#include "interpolateXY.H"

namespace Foam
{
namespace turbineModels
{

// * * * * * * * * * * * * * *  Constructor  * * * * * * * * * * * * * * * * //

horizontalAxisWindTurbinesALMfastv8::horizontalAxisWindTurbinesALMfastv8
(
    const volVectorField& U
)
:
    // Set the pointer to runTime
    runTime_(U.time()),

    // Set the pointer to the mesh
    mesh_(U.mesh()),

    // Set the pointer to the velocity field
    U_(U),

    // Set the degrees to radians convesion factor.
    degRad((Foam::constant::mathematical::pi)/180.0),

    // Set the revolutions/s to radians/s conversion factor.
    rpsRadSec(2.0*(Foam::constant::mathematical::pi)),

    // Set the revolutions/min to radians/s conversion factor.
    rpmRadSec(2.0*(Foam::constant::mathematical::pi)/60.0),

    // Set the time step size.
    dt(runTime_.deltaT().value()),

    // Set the current simulation time.
    time(runTime_.timeName()),
    t(runTime_.value()),

    // Set the current simulation start and stop time.
    tStart(runTime_.startTime().value()),
    tEnd(runTime_.endTime().value()),

    // Set the pastFirstTimeStep flag to false
    pastFirstTimeStep(false),


    // Initialize the velocity gradient.
    gradU
    (
        IOobject
        (
            "gradU",
            time,
            mesh_,
            IOobject::NO_READ,
            IOobject::NO_WRITE
        ),
        mesh_,
        dimensionedTensor("gradU",dimVelocity/dimLength,tensor::zero)
    ),

    // Initialize the body force.
    bodyForce
    (
        IOobject
        (
            "bodyForce",
            time,
            mesh_,
            IOobject::NO_READ,
            IOobject::AUTO_WRITE
        ),
        mesh_,
        dimensionedVector("bodyForce",dimForce/dimVolume/dimDensity,vector::zero)
    ),
    
    // Initialize the summed distribution function (not needed for calculation,
    // but helps in understanding distribution function shape).
    gBlade
    (
        IOobject
        (
            "gBlade",
            time,
            mesh_,
            IOobject::NO_READ,
            IOobject::AUTO_WRITE
        ),
        mesh_,
        dimensionedScalar("gBlade",dimless/dimVolume,0.0)
    ),

    // Initialize the search cell index (not needed for actual calculation, just 
    // for checking that search cell identification is correct).
    searchCells
    (
        IOobject
        (
            "searchCells",
            time,
            mesh_,
            IOobject::NO_READ,
            IOobject::AUTO_WRITE
        ),
        mesh_,
        dimensionedScalar("searchCells",dimless,0.0)
    ),

    // Initialize the radius from the main shaft axis field.
    rFromShaft
    (
        IOobject
        (
            "rFromShaft",
            time,
            mesh_,
            IOobject::NO_READ,
            IOobject::AUTO_WRITE
        ),
        mesh_,
        dimensionedScalar("rFromShaft",dimLength,0.0)
    ),
    
    // Initialize the relative velocity field.
    Urel
    (
        IOobject
        (
            "Urel",
            time,
            mesh_,
            IOobject::NO_READ,
            IOobject::AUTO_WRITE
        ),
        mesh_,
        dimensionedVector("Urel",dimLength/dimTime,vector::zero)
    )


{   
    // Define dictionary that defines the turbine array.
    IOdictionary turbineArrayProperties
    (
        IOobject
        (
            "turbineArrayProperties",
            runTime_.constant(),
            mesh_,
            IOobject::MUST_READ,
            IOobject::NO_WRITE
        )
    );
    
    // Read in the turbine array properties dictionary.  This is the uppermost level dictionary
    // that describes where the turbines are, what kind they are, their initial state, and 
    // information about how the actuator line method is applied to each turbine.
    {
        List<word> listTemp = turbineArrayProperties.toc();
        for (int i = 0; i < listTemp.size(); i++)
        {
            if (listTemp[i] != "globalProperties")
            {
                turbineName.append(listTemp[i]);
            }
        }
    }

    numTurbines = turbineName.size();

    outputControl = turbineArrayProperties.subDict("globalProperties").lookupOrDefault<word>("outputControl","timeStep");
    outputInterval = turbineArrayProperties.subDict("globalProperties").lookupOrDefault<scalar>("outputInterval",1);
    lastOutputTime = runTime_.startTime().value();
    outputIndex = 0;

    dryRun = readBool(turbineArrayProperties.subDict("globalProperties").lookup("dryRun"));
    restart = readBool(turbineArrayProperties.subDict("globalProperties").lookup("restart"));
    superControllerOn = readBool(turbineArrayProperties.subDict("globalProperties").lookup("superControllerOn"));
    superControllerLib = word(turbineArrayProperties.subDict("globalProperties").lookup("superControllerLib"));
    numSuperControllerInputs = int(readScalar(turbineArrayProperties.subDict("globalProperties").lookup("numSuperControllerInputs")));
    numSuperControllerOutputs = int(readScalar(turbineArrayProperties.subDict("globalProperties").lookup("numSuperControllerOutputs")));
    timeSimulationStart = readScalar(turbineArrayProperties.subDict("globalProperties").lookup("timeSimulationStart"));
    timeSimulationEnd = readScalar(turbineArrayProperties.subDict("globalProperties").lookup("timeSimulationEnd"));
    checkPointInterval = readScalar(turbineArrayProperties.subDict("globalProperties").lookup("checkPointInterval"));

    perturb = turbineArrayProperties.subDict("globalProperties").lookupOrDefault<scalar>("perturb",1E-5);

    includeNacelleSomeTrue = false;
    includeTowerSomeTrue = false;

    forAll(turbineName,i)
    {
        turbineType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("turbineType")));
        FASTInputFileName.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("FASTInputFileName")));
        FASTRestartFileName.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("FASTRestartFileName")));
        includeNacelle.append(readBool(turbineArrayProperties.subDict(turbineName[i]).lookup("includeNacelle")));
        includeTower.append(readBool(turbineArrayProperties.subDict(turbineName[i]).lookup("includeTower")));

        baseLocation.append(vector(turbineArrayProperties.subDict(turbineName[i]).lookup("baseLocation")));

        numBladePoints.append(int(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("numBladePoints"))));
        numNacellePoints.append(int(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("numNacellePoints"))));
        numTowerPoints.append(int(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("numTowerPoints"))));

        bladePointDistType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("bladePointDistType")));
        nacellePointDistType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("nacellePointDistType")));
        towerPointDistType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("towerPointDistType")));

        bladeActuatorPointInterpType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("bladeActuatorPointInterpType")));
        nacelleActuatorPointInterpType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("nacelleActuatorPointInterpType")));
        towerActuatorPointInterpType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("towerActuatorPointInterpType")));

        bladeSearchCellMethod.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("bladeSearchCellMethod")));

        actuatorUpdateType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("actuatorUpdateType")));

        bladeForceProjectionType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("bladeForceProjectionType")));
        nacelleForceProjectionType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("nacelleForceProjectionType")));
        towerForceProjectionType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("towerForceProjectionType")));

        bladeForceProjectionDirection.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("bladeForceProjectionDirection")));

        bladeEpsilon.append(vector(turbineArrayProperties.subDict(turbineName[i]).lookup("bladeEpsilon")));
        nacelleEpsilon.append(vector(turbineArrayProperties.subDict(turbineName[i]).lookup("nacelleEpsilon")));
        towerEpsilon.append(vector(turbineArrayProperties.subDict(turbineName[i]).lookup("towerEpsilon")));

        nacelleSampleDistance.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("nacelleSampleDistance"))));
        towerSampleDistance.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("towerSampleDistance"))));

        nacelleLength.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("nacelleLength"))));
        nacelleFrontalArea.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("nacelleFrontalArea"))));
        nacelleCd.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("nacelleCd"))));

        tipRootLossCorrType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("tipRootLossCorrType")));

        velocityDragCorrType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("velocityDragCorrType")));

        rotationDir.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("rotationDir")));

        rotorSpeed.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("RotSpeed"))));
        rotorSpeedF.append(rotorSpeed[i]);
        speedError.append(0.0);
        intSpeedError.append(0.0);

        rotorAzimuth.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("Azimuth"))));

        generatorTorque.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("TorqueGen"))));

        bladePitch.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("Pitch"))));

        nacYaw.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("NacYaw"))));

        fluidDensity.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("fluidDensity")))); 


        if(includeNacelle[i])   //  if any of the nacelles are active, set this global boolean to true.
        {
            includeNacelleSomeTrue = true;
        }
        else   // set the number of nacelle points to 1, if the nacelle is not active.
        {
            numNacellePoints[i] = 1;
        }


        if(includeTower[i])   //  if any of the nacelles are active, set this global boolean to true.
        {
            includeTowerSomeTrue = true;
        }
        else   // set the number of tower points to 1, if the tower is not active.
        {
            numTowerPoints[i] = 1;
        }

        if((nacelleForceProjectionType[i] == "advanced1") || (nacelleForceProjectionType[i] == "advanced2")) // for the advanced nacelle force projection, only 1 nacelle point can be handled
        {
            numNacellePoints[i] = 1;
        }
    }


    // Send global inputs to FAST.
    FAST.setGlobalInputs
    (
         numTurbines,
         dryRun,
         superControllerOn,
         superControllerLib,
         restart,
         tStart-timeSimulationStart,
         int((tStart-timeSimulationStart)/dt),
         tEnd-timeSimulationStart,
         int((tEnd-timeSimulationStart)/dt),
         timeSimulationEnd-timeSimulationStart,
         dt,
         checkPointInterval,
         numSuperControllerOutputs,
         numSuperControllerInputs
    );

    // Set individual turbine inputs to FAST.
    forAll(turbineName,i)
    {
        if (Pstream::myProcNo() == i)
        {
            std::vector<double> baseLoc;
            baseLoc.push_back(baseLocation[i].x());
            baseLoc.push_back(baseLocation[i].y());
            baseLoc.push_back(baseLocation[i].z());

            std::cout << "baseLoc: " << baseLoc[0] << " " << baseLoc[1] << " " << baseLoc[2] << std::endl;

            FAST.setTurbineData
            (
                int(i),
                FASTInputFileName[i],
                FASTRestartFileName[i],
                baseLoc
            );
        }
   }


   // Initialize FAST.
   if (! FAST.isDryRun())
   {
       FAST.init();

       FAST.solution0();
   }


   // Get the number of blades, blade points, tower points, and shaft vectors all in order and onto all cores. 
   forAll(turbineName,i)
   {
      NumBl.append(0);
      numBladePointsFAST.append(0);
      numTowerPointsFAST.append(0);
      uvShaft.append(vector::zero);
      rotorApex.append(vector::zero);
   }
 
   getNumBladesFAST();
   getNumBladePointsFAST();
   getNumTowerPointsFAST();
 //updateMainShaftVectorFAST();

   Pout << "Number of blades: " << NumBl << endl;
   Pout << "Number of actuator line/FAST blade points: " << numBladePoints << "/" << numBladePointsFAST << endl;
   Pout << "Number of actuator line/FAST tower points: " << numTowerPoints << "/" << numTowerPointsFAST <<  endl;
 //Pout << "uvShaft: " << uvShaft << endl;
 


/*---------------------------------------------------------------------------*\
    // Catalog the various types of turbines.  For example if three turbines are GE 1.5 and 
    // two turbines are Siemens 2.3 machines, then assign the GE 1.5 an ID of 0 and the Siemens
    // 2.3 an ID of 1.
    numTurbinesDistinct = 1;
    {
        turbineTypeDistinct.append(turbineType[0]);
        forAll(turbineType,i)
        {
            bool flag = false;
            for(int j = 0; j < numTurbinesDistinct; j++)
            {
                if(turbineType[i] == turbineTypeDistinct[j])
                {
                   flag = true;
                }
            }
            if(flag == false)
            {
                numTurbinesDistinct++;
                turbineTypeDistinct.append(turbineType[i]);
            }
        }
    }
    forAll(turbineType,i)
    {
        for(int j = 0; j < numTurbinesDistinct; j++)
        {
            if(turbineType[i] == turbineTypeDistinct[j])
            {
                turbineTypeID.append(j);
            }
        }
    }


    // For each distinct turbine, read in properties of that turbine from separate
    // dictionaries.

    for(int i = 0; i < numTurbinesDistinct; i++)
    {
        // Declare the turbineProperties dictionary for the ith turbine.
        IOdictionary turbineProperties
        (
            IOobject
            (
                turbineTypeDistinct[i],
                runTime_.constant(),"turbineProperties",
                mesh_,
                IOobject::MUST_READ,
                IOobject::NO_WRITE
            )
        );

        // Read in the data.
        NumBl.append(scalar(readScalar(turbineProperties.lookup("NumBl"))));
        TipRad.append(scalar(readScalar(turbineProperties.lookup("TipRad"))));
        HubRad.append(scalar(readScalar(turbineProperties.lookup("HubRad"))));
        UndSling.append(scalar(readScalar(turbineProperties.lookup("UndSling"))));
        OverHang.append(scalar(readScalar(turbineProperties.lookup("OverHang"))));
        nacelleLength.append(scalar(readScalar(turbineProperties.lookup("nacelleLength"))));
        nacelleFrontalArea.append(scalar(readScalar(turbineProperties.lookup("nacelleFrontalArea"))));
        nacelleCd.append(scalar(readScalar(turbineProperties.lookup("nacelleCd"))));
        TowerHt.append(scalar(readScalar(turbineProperties.lookup("TowerHt"))));
        Twr2Shft.append(scalar(readScalar(turbineProperties.lookup("Twr2Shft"))));
        ShftTilt.append(scalar(readScalar(turbineProperties.lookup("ShftTilt"))));
        PreCone.append(turbineProperties.lookup("PreCone"));
        GBRatio.append(scalar(readScalar(turbineProperties.lookup("GBRatio"))));
        GBEfficiency.append(scalar(readScalar(turbineProperties.lookup("GBEfficiency"))));
        GenEfficiency.append(scalar(readScalar(turbineProperties.lookup("GenEfficiency"))));
        RatedRotSpeed.append(scalar(readScalar(turbineProperties.lookup("RatedRotSpeed"))));
        GenIner.append(scalar(readScalar(turbineProperties.lookup("GenIner"))));
        HubIner.append(scalar(readScalar(turbineProperties.lookup("HubIner"))));
        BladeIner.append(scalar(readScalar(turbineProperties.lookup("BladeIner"))));
        DriveTrainIner.append(NumBl[i]*BladeIner[i] + HubIner[i] + GBRatio[i]*GBRatio[i]*GenIner[i]);
        GenTorqueControllerType.append(word(turbineProperties.lookup("GenTorqueControllerType")));
        NacYawControllerType.append(word(turbineProperties.lookup("NacYawControllerType")));
        BladePitchControllerType.append(word(turbineProperties.lookup("BladePitchControllerType")));
        RotSpeedLimiter.append(bool(readBool(turbineProperties.lookup("RotSpeedLimiter"))));
        GenTorqueRateLimiter.append(bool(readBool(turbineProperties.lookup("GenTorqueRateLimiter"))));
        NacYawRateLimiter.append(bool(readBool(turbineProperties.lookup("NacYawRateLimiter"))));
        BladePitchRateLimiter.append(bool(readBool(turbineProperties.lookup("BladePitchRateLimiter"))));
        SpeedFilterCornerFrequency.append(scalar(readScalar(turbineProperties.lookup("SpeedFilterCornerFrequency"))));
        NacelleEquivalentRadius.append(Foam::sqrt(nacelleFrontalArea[i]/Foam::constant::mathematical::pi));


        RateLimitGenTorque.append(readScalar(turbineProperties.subDict("GenTorqueControllerParams").lookup("RateLimitGenTorque")));
        if (GenTorqueControllerType[i] == "none")
        {
            // Read nothing.
        }
        else if (GenTorqueControllerType[i] == "fiveRegion")
        {
            CutInGenSpeed.append(readScalar(turbineProperties.subDict("GenTorqueControllerParams").lookup("CutInGenSpeed")));
            Region2StartGenSpeed.append(readScalar(turbineProperties.subDict("GenTorqueControllerParams").lookup("Region2StartGenSpeed")));
            Region2EndGenSpeed.append(readScalar(turbineProperties.subDict("GenTorqueControllerParams").lookup("Region2EndGenSpeed")));
            CutInGenTorque.append(readScalar(turbineProperties.subDict("GenTorqueControllerParams").lookup("CutInGenTorque")));
            RatedGenTorque.append(readScalar(turbineProperties.subDict("GenTorqueControllerParams").lookup("RatedGenTorque")));
            RateLimitGenTorque.append(readScalar(turbineProperties.subDict("GenTorqueControllerParams").lookup("RateLimitGenTorque")));
            KGen.append(readScalar(turbineProperties.subDict("GenTorqueControllerParams").lookup("KGen")));
        }
        else if (GenTorqueControllerType[i] == "speedTorqueTable")
        {
            SpeedTorqueTable.append(turbineProperties.subDict("GenTorqueControllerParams").lookup("SpeedTorqueTable"));
            DynamicList<scalar> speedInt;
            DynamicList<scalar> torqueInt;
 
            forAll(SpeedTorqueTable[i],j)
            {
                speedInt.append(SpeedTorqueTable[i][j][0]);
                torqueInt.append(SpeedTorqueTable[i][j][1]);
            }

            SpeedGenProfile.append(speedInt);
            TorqueGenProfile.append(torqueInt);

            speedInt.clear();
            torqueInt.clear();
        }
        
       

        RateLimitBladePitch.append(readScalar(turbineProperties.subDict("BladePitchControllerParams").lookup("RateLimitBladePitch")));
        if (BladePitchControllerType[i] == "none")
        {
            // Read nothing.
        }
        else if (BladePitchControllerType[i] == "PID")
        {
            PitchK.append(readScalar(turbineProperties.subDict("BladePitchControllerParams").lookup("PitchK")));
            PitchMin.append(readScalar(turbineProperties.subDict("BladePitchControllerParams").lookup("PitchMin")));
            PitchMax.append(readScalar(turbineProperties.subDict("BladePitchControllerParams").lookup("PitchMax")));
            PitchControlKP.append(readScalar(turbineProperties.subDict("BladePitchControllerParams").lookup("PitchControlKP")));
            PitchControlKI.append(readScalar(turbineProperties.subDict("BladePitchControllerParams").lookup("PitchControlKI")));
            PitchControlKD.append(readScalar(turbineProperties.subDict("BladePitchControllerParams").lookup("PitchControlKD")));
        }



        RateLimitNacYaw.append(readScalar(turbineProperties.subDict("NacYawControllerParams").lookup("RateLimitNacYaw")));
        if (NacYawControllerType[i] == "none")
        {
            // Read nothing.
        }
        else if (NacYawControllerType[i] == "timeYawTable")
        {
        }



        AirfoilType.append(turbineProperties.lookup("Airfoils"));



        BladeData.append(turbineProperties.lookup("BladeData"));
        {
           DynamicList<scalar> station;
           DynamicList<scalar> chord;
           DynamicList<scalar> twist;
           DynamicList<scalar> thickness;
           DynamicList<label> id;
           DynamicList<scalar> userDef;

           forAll(BladeData[i], j)
           {
               station.append(BladeData[i][j][0]);
               chord.append(BladeData[i][j][1]);
               twist.append(BladeData[i][j][2]);
               thickness.append(BladeData[i][j][3]);
               userDef.append(BladeData[i][j][4]);
               id.append(BladeData[i][j][5]);
           }

           BladeStation.append(station);
           BladeChord.append(chord);
           BladeTwist.append(twist);
           BladeThickness.append(thickness);
           BladeUserDef.append(userDef);
           BladeAirfoilTypeID.append(id);

           station.clear();
           chord.clear();
           twist.clear();
           thickness.clear();
           userDef.clear();
           id.clear();
        }



        TowerData.append(turbineProperties.lookup("TowerData"));
        {
           DynamicList<scalar> height;
           DynamicList<scalar> chord;
           DynamicList<scalar> twist;
           DynamicList<scalar> thickness;
           DynamicList<scalar> userDef;
           DynamicList<label> id;
           forAll(TowerData[i], j)
           {
               height.append(TowerData[i][j][0]);
               chord.append(TowerData[i][j][1]);
               twist.append(TowerData[i][j][2]);
               thickness.append(TowerData[i][j][3]);
               userDef.append(TowerData[i][j][4]);
               id.append(TowerData[i][j][5]);
           }

           TowerStation.append(height);
           TowerChord.append(chord);
           TowerTwist.append(twist);
           TowerThickness.append(thickness);
           TowerUserDef.append(userDef);
           TowerAirfoilTypeID.append(id);

           height.clear();
           chord.clear();
           twist.clear();
           thickness.clear();
           userDef.clear();
           id.clear();
        }
    }

    // Catalog the various distinct types of airfoils used in the various
    // distinct types of turbines.
    int numAirfoilsDistinct = 1;
    {
        airfoilTypesDistinct.append(AirfoilType[0][0]);
        forAll(AirfoilType,i)
        {
            forAll(AirfoilType[i],j)
            {
                bool flag = false;
                for(int k = 0; k < numAirfoilsDistinct; k++)
                {
                    if(AirfoilType[i][j] == airfoilTypesDistinct[k])
                    {
                        flag = true;
                    }
                }
                if(flag == false)
                {
                    numAirfoilsDistinct++;
                    airfoilTypesDistinct.append(AirfoilType[i][j]);
                }
            }
        }
    }

    // Reassign airfoil type IDs to blades of each turbine based on the global
    // distinct list of airfoils.
    forAll(BladeAirfoilTypeID,i)
    {
        forAll(BladeAirfoilTypeID[i],j)
        {
            for(int k = 0; k < numAirfoilsDistinct; k++)
            {
                if(AirfoilType[i][BladeAirfoilTypeID[i][j]] == airfoilTypesDistinct[k])
                {
                    BladeAirfoilTypeID[i][j] = k;
                    k = numAirfoilsDistinct;
                }
            }
        }
    }
    // Reassign airfoil type IDs to tower of each turbine based on the global
    // distinct list of airfoils.
    forAll(TowerAirfoilTypeID,i)
    {
        forAll(TowerAirfoilTypeID[i],j)
        {
            for(int k = 0; k < numAirfoilsDistinct; k++)
            {
                if(AirfoilType[i][TowerAirfoilTypeID[i][j]] == airfoilTypesDistinct[k])
                {
                    TowerAirfoilTypeID[i][j] = k;
                    k = numAirfoilsDistinct;
                }
            }
        }
    }
   


    // For each distinct airfoil, read in the bladePointLift and drag versus angle
    // of attack data.
    for(int i = 0; i < numAirfoilsDistinct; i++)
    {
        // Declare the airfoilsProperties dictionary for the ith airfoil.
        IOdictionary airfoilProperties
        (
            IOobject
            (
                airfoilTypesDistinct[i],
                runTime_.constant(),"airfoilProperties",
                mesh_,
                IOobject::MUST_READ,
                IOobject::NO_WRITE
            )
        );

        // Read in the data.
        airfoilData.append(airfoilProperties.lookup("airfoilData"));

        DynamicList<scalar> alphaInt;
        DynamicList<scalar> ClInt;
        DynamicList<scalar> CdInt;

        forAll(airfoilData[i],j)
        {
            alphaInt.append(airfoilData[i][j][0]);
            ClInt.append(airfoilData[i][j][1]);
            CdInt.append(airfoilData[i][j][2]);
        }

        airfoilAlpha.append(alphaInt);
        airfoilCl.append(ClInt);
        airfoilCd.append(CdInt);

        alphaInt.clear();
        ClInt.clear();
        CdInt.clear();
    }




    // Convert nacelle yaw from compass directions to the standard
    // convention of 0 degrees on the + x axis with positive degrees
    // in the counter-clockwise direction.
    forAll(nacYaw,i)
    {
        nacYaw[i] = compassToStandard(nacYaw[i]);
    }



    // Convert quantities in degrees into radians (dynamic lists
    // have to be done in loops).
    rotorAzimuth   = degRad * rotorAzimuth;
    rotorSpeed  = rpmRadSec * rotorSpeed;
    rotorSpeedF = rpmRadSec * rotorSpeedF;
    nacYaw    = degRad * nacYaw;
    ShftTilt  = degRad * ShftTilt;
    SpeedFilterCornerFrequency = rpsRadSec * SpeedFilterCornerFrequency;
    RatedRotSpeed = rpmRadSec * RatedRotSpeed;
    PitchK = degRad * PitchK;
    PitchMin = degRad * PitchMin;
    PitchMax = degRad * PitchMax;
    forAll(PreCone,i)
    {
        PreCone[i] = degRad * PreCone[i];
    }

    


    // Calculate tower shaft intersection and rotor apex locations. (The
    // i-index is at the turbine array level for each turbine and the j-
    // index is for each type of turbine--if all turbines are the same, j-
    // is always 0.)  The rotor apex is not yet rotated for initial yaw;
    // that is done below.
    for(int i = 0; i < numTurbines; i++)
    {
        int j = turbineTypeID[i];
        towerShaftIntersect.append(baseLocation[i]);
        towerShaftIntersect[i].z() = towerShaftIntersect[i].z() + TowerHt[j] + Twr2Shft[j];
        rotorApex.append(towerShaftIntersect[i]);
        rotorApex[i].x() = rotorApex[i].x() + ((OverHang[j] + UndSling[j]) * Foam::cos(ShftTilt[j]));
        rotorApex[i].z() = rotorApex[i].z() +  (OverHang[j] + UndSling[j]) * Foam::sin(ShftTilt[j]);
    }

    \*---------------------------------------------------------------------------*/


    // Create the actuator line points (not yet rotated for initial nacelle
    // yaw or initial rotor azimuth), the actuator tower points, and the
    // actuator nacelle points. i-index is at array level, j-index is
    // for the type of turbine, k-index is for each blade, and m-index is
    // for each actuator point.  Also create other important vectors, and
    // initialize the forces, blade aligned coordinate system, and
    // wind vectors to zero.
    totBladePoints = 0;
    totNacellePoints = 0;
    totTowerPoints = 0;
    Random rndGen(123456);

    for(int i = 0; i < numTurbines; i++)
    {
//      int j = turbineTypeID[i];

        // Define which way the shaft points to distinguish between
        // upwind and downwind turbines.
      //uvShaftDir.append(OverHang[j]/mag(OverHang[j]))
      //uvShaftDir.append(-1);

        // Define the vector along the shaft pointing in the
        // direction of the wind.
      //uvShaft.append(rotorApex[i] - towerShaftIntersect[i]);
      //uvShaft[i] = (uvShaft[i]/mag(uvShaft[i])) * uvShaftDir[i];

        // Define the vector aligned with the tower pointing from
        // the ground to the nacelle.
      //uvTower.append(towerShaftIntersect[i] - baseLocation[i]);
      //uvTower[i] = uvTower[i]/mag(uvTower[i]);

        uvTower.append(vector::zero);
        uvTower[i].x() = 0.0;
        uvTower[i].y() = 0.0;
        uvTower[i].z() = 1.0;

        // Now calculate the actuator section center points for each blade
        // of each turbine in the array.  All blades points will be calculated
        // at zero azimuth (blade pointing up), and then rotated to its correct
        // position before doing a global rotation to the initial azimuth of
        // the rotor.  Also calculate the radius of each point (not including coning).


        Info << "Getting the blade points set up..." << endl;
        // Calculate the fractional width of each blade actuator section.  This will be usd
        // to interpolate between the actuator line and Aerodyn points.
        bladeDs.append(DynamicList<scalar>(0));
        if(bladePointDistType[i] == "uniform")
        {
          //scalar actuatorWidth = (TipRad[j]-HubRad[j])/numBladePoints[i];
            scalar actuatorWidth = 1.0/numBladePoints[i];
            for(int m = 0; m < numBladePoints[i]; m++)
            {
                bladeDs[i].append(actuatorWidth);
            }
        }
        // Add other point distribution types here, such as cosine, tanh.
    
        // Calculate the actual locations of the blade actuator elemens and
        // interpolate blade properties to these points.

        bladePoints.append(List<List<vector> >(NumBl[i], List<vector>(numBladePoints[i],vector::zero)));
        bladePointsFAST.append(List<List<vector> >(NumBl[i], List<vector>(numBladePointsFAST[i],vector::zero)));
        bladeSamplePoints.append(List<List<vector> >(NumBl[i], List<vector>(numBladePoints[i],vector::zero)));
        bladePointRadius.append(List<List<scalar> >(NumBl[i], List<scalar>(numBladePoints[i],0.0)));

//      bladePointChord.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
//      bladePointTwist.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
//      bladePointThickness.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
//      bladePointUserDef.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
//      bladePointAirfoil.append(List<List<label> >(NumBl[j], List<label>(numBladePoints[i],0)));
/*      for(int k = 0; k < NumBl[i]; k++)
        {
          //vector root = rotorApex[i];
          //scalar beta = PreCone[j][k] - ShftTilt[j];
          //root.x() = root.x() + HubRad[j]*Foam::sin(beta);
          //root.z() = root.z() + HubRad[j]*Foam::cos(beta);
          //scalar dist = HubRad[j];
            scalar dist = 0.0;
            for(int m = 0; m < numBladePoints[i]; m++)
            {
               dist = dist + 0.5*bladeDs[i][m];
               bladePoints[i][k][m].x() = root.x() + dist*Foam::sin(beta);
               bladePoints[i][k][m].y() = root.y();
               bladePoints[i][k][m].z() = root.z() + dist*Foam::cos(beta);
               bladeSamplePoints[i][k][m] = bladePoints[i][k][m];
             //bladePointRadius[i][k][m] = dist;
               bladePointRadius[i][k][m] = HubRad[j] + dist;
               totBladePoints++;
               dist = dist + 0.5*bladeDs[i][m];

               bladePointChord[i][k][m] = interpolate(bladePointRadius[i][k][m], BladeStation[j], BladeChord[j]);
               bladePointTwist[i][k][m] = interpolate(bladePointRadius[i][k][m], BladeStation[j], BladeTwist[j]);
               bladePointThickness[i][k][m] = interpolate(bladePointRadius[i][k][m], BladeStation[j], BladeThickness[j]);
               bladePointUserDef[i][k][m] = interpolate(bladePointRadius[i][k][m], BladeStation[j], BladeUserDef[j]);
               label maxIndex = BladeAirfoilTypeID[j].size() - 1;
               label airfoil = interpolate(bladePointRadius[i][k][m], BladeStation[j], BladeAirfoilTypeID[j]);
               bladePointAirfoil[i][k][m] = min(max(0,airfoil),maxIndex);
            }

            // Apply rotation to get blades, other than blade 1, in the right
            // place.
            if (k > 0)
            {
                for(int m = 0; m < numBladePoints[i]; m++)
                {
                    bladePoints[i][k][m] = rotateVector(bladePoints[i][k][m], rotorApex[i], uvShaft[i], (360.0/NumBl[j])*k*degRad);
                    bladeSamplePoints[i][k][m] = rotateVector(bladeSamplePoints[i][k][m], rotorApex[i], uvShaft[i], (360.0/NumBl[j])*k*degRad);
                }
            }
        }
*/

        Info << "Getting the tower points set up..." << endl;
        // Compute the location of the tower section center points for each
        // turbine tower in the array.

        // Calculate the width of each tower actuator section.
        towerDs.append(DynamicList<scalar>(0));
        if(towerPointDistType[i] == "uniform")
        {
          //scalar actuatorWidth = TowerHt[j]/numTowerPoints[i];
            scalar actuatorWidth = 1.0/numTowerPoints[i];
            for(int m = 0; m < numTowerPoints[i]; m++)
            {
                towerDs[i].append(actuatorWidth);
            }
        }
        // Add other point distribution types here, such as cosine, tanh.

        // Compute the actual locations of the tower actuator elements and 
        // interpolate tower properties to these points.
        towerPoints.append(List<vector>(numTowerPoints[i],vector::zero));
        towerPointsFAST.append(List<vector>(numTowerPointsFAST[i],vector::zero));
        towerSamplePoints.append(List<vector>(numTowerPoints[i],vector::zero));
        towerPointHeight.append(List<scalar>(numTowerPoints[i],0.0));
      //towerPointChord.append(List<scalar>(numTowerPoints[i],0.0));
      //towerPointTwist.append(List<scalar>(numTowerPoints[i],0.0));
      //towerPointThickness.append(List<scalar>(numTowerPoints[i],0.0));
      //towerPointUserDef.append(List<scalar>(numTowerPoints[i],0.0));
      //towerPointAirfoil.append(List<label>(numTowerPoints[i],0));
/*
        {
           towerPoints[i][0] = baseLocation[i];
         //towerPoints[i][0].z() += 0.5*towerDs[i][0];
           towerSamplePoints[i][0] = towerPoints[i][0];
           towerSamplePoints[i][0].x() -= towerSampleDistance[i];
           totTowerPoints++;
           for(int m = 1; m < numTowerPoints[i]; m++)
           {
               towerPoints[i][m] = baseLocation[i];
               towerPoints[i][m].z() = towerPoints[i][m-1].z() + towerDs[i][m];
               towerSamplePoints[i][m] = towerPoints[i][m];
               towerSamplePoints[i][m].x() -= towerSampleDistance[i];
               towerPointHeight[i][m] = towerPoints[i][m].z() - baseLocation[i].z();
               totTowerPoints++;

               towerPointChord[i][m] = interpolate(towerPointHeight[i][m], TowerStation[j], TowerChord[j]);
               towerPointTwist[i][m] = interpolate(towerPointHeight[i][m], TowerStation[j], TowerTwist[j]);
               towerPointThickness[i][m] = interpolate(towerPointHeight[i][m], TowerStation[j], TowerThickness[j]);
               towerPointUserDef[i][m] = interpolate(towerPointHeight[i][m], TowerStation[j], TowerUserDef[j]);
               label maxIndex = TowerAirfoilTypeID[j].size() - 1;
               label airfoil = interpolate(towerPointHeight[i][m], TowerStation[j], TowerAirfoilTypeID[j]);
               towerPointAirfoil[i][m] = min(max(0,airfoil),maxIndex);

           }
        }
*/



        Info << "Getting the nacelle points set up..." << tab << nacelleLength[i] << tab << numNacellePoints[i] << endl;
        // Compute the location of the nacelle section points for each turbine
        // in the array.

        // Calculate the width of each nacelle actuator section.
        nacelleDs.append(DynamicList<scalar>(0));
        if(nacellePointDistType[i] == "uniform")
        {
          //scalar actuatorWidth = nacelleLength[j]/numNacellePoints[i];
            scalar actuatorWidth = nacelleLength[i]/numNacellePoints[i];
            for(int m = 0; m < numNacellePoints[i]; m++)
            {
                nacelleDs[i].append(actuatorWidth);
            }
        }
        // Add other point distribution types here, such as cosine, tanh.

        // Compute the actual nacelle location points.
        nacellePoints.append(List<vector>(numNacellePoints[i],vector::zero));
        nacelleSamplePoint.append(vector::zero);
/*
        {
           nacellePoints[i][0] = rotorApex[i];
           nacelleSamplePoint[i] = nacellePoints[i][0];
           nacelleSamplePoint[i].x() -= nacelleSampleDistance[i];
           totNacellePoints++;
           for(int m = 1; m < numNacellePoints[i]; m++)
           {
              nacellePoints[i][m] = nacellePoints[i][m-1] + nacelleDs[i][m] * uvShaft[i];
              totNacellePoints++;
           }
        }
*/


        // Generate random numbers for the point perturbation during control
        // processor identification.  This does not affect the actual location--it is
        // just there to break ties and make sure > 1 processors don't account for a
        // single actuator point.
        bladePointsPerturbVector.append(List<List<vector> >(NumBl[i], List<vector>(numBladePoints[i],vector::zero)));
        if(Pstream::myProcNo() == 0)
        {
            for(int k =  0; k < NumBl[i]; k++)
            {
                for(int m = 0; m < numBladePoints[i]; m++)
                {
                    bladePointsPerturbVector[i][k][m] = perturb*(2.0*rndGen.vector01()-vector::one); 
                }
            }
        }

        towerPointsPerturbVector.append(List<vector>(numTowerPoints[i],vector::zero));
        if(Pstream::myProcNo() == 0)
        {
            for(int m = 0; m < numTowerPoints[i]; m++)
            {
                towerPointsPerturbVector[i][m] = perturb*(2.0*rndGen.vector01()-vector::one); 
            }
        }

        nacellePointPerturbVector.append(vector::zero);
        if(Pstream::myProcNo() == 0)
        {
            nacellePointPerturbVector[i] = perturb*(2.0*rndGen.vector01()-vector::one);
        }




        // Define the size of the blade, nacelle, and tower force arrays and set to zero.
        bladePointForce.append(List<List<vector> >(NumBl[i], List<vector>(numBladePoints[i],vector::zero)));
        towerPointForce.append(List<vector>(numTowerPoints[i],vector::zero));
        nacellePointForce.append(List<vector>(numNacellePoints[i],vector::zero));
  
        // Define the size of the blade, nacelle, and tower aligned vectors array and set to zero.
        bladeAlignedVectors.append(List<List<vector> >(NumBl[i],List<vector>(3,vector::zero)));

        // Define the actuator element wind vector arrays and set them to zero.
        bladeWindVectors.append(List<List<vector> >(NumBl[i],List<vector>(numBladePoints[i],vector::zero)));
        towerWindVectors.append(List<vector>(numTowerPoints[i],vector::zero));
        nacelleWindVector.append(vector::zero);

        // Define the size of the deltaNacYaw, deltaAzimuth, and deltaPitch lists and set to zero.
        deltaNacYaw.append(0.0);
        deltaAzimuth.append(0.0);

        // Define the size of the angle of attack lists and set to zero.
        bladePointAlpha.append(List<List<scalar> >(NumBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointAlpha.append(List<scalar>(numTowerPoints[i],0.0));

        // Define the size of the wind speed magnitude lists and set to zero.
        bladePointVmag.append(List<List<scalar> >(NumBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointVmag.append(List<scalar>(numTowerPoints[i],0.0));
        nacellePointVmag.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the coefficient of bladePointLift lists and set to zero.
        bladePointCl.append(List<List<scalar> >(NumBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointCl.append(List<scalar>(numTowerPoints[i],0.0));

        // Define the size of the coefficient of drag lists and set to zero.
        bladePointCd.append(List<List<scalar> >(NumBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointCd.append(List<scalar>(numTowerPoints[i],0.0));
        nacellePointCd.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the bladePointLift lists and set to zero.
        bladePointLift.append(List<List<scalar> >(NumBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointLift.append(List<scalar>(numTowerPoints[i],0.0));

        // Define the size of the drag lists and set to zero.
        bladePointDrag.append(List<List<scalar> >(NumBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointDrag.append(List<scalar>(numTowerPoints[i],0.0));
        nacellePointDrag.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the axial force lists and set to zero.
        bladePointAxialForce.append(List<List<scalar> >(NumBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointAxialForce.append(List<scalar>(numTowerPoints[i],0.0));
        nacellePointAxialForce.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the horizontal force lists and set to zero.
        bladePointHorizontalForce.append(List<List<scalar> >(NumBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointHorizontalForce.append(List<scalar>(numTowerPoints[i],0.0));
        nacellePointHorizontalForce.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the horizontal force lists and set to zero.
        bladePointVerticalForce.append(List<List<scalar> >(NumBl[i], List<scalar>(numBladePoints[i],0.0)));
        nacellePointVerticalForce.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the torque lists and set to zero.
        bladePointTorque.append(List<List<scalar> >(NumBl[i], List<scalar>(numBladePoints[i],0.0)));

        // Define the size of the global axial force lists and set to zero.
        rotorAxialForce.append(0.0);
        towerAxialForce.append(0.0);
        nacelleAxialForce.append(0.0);

        // Define the size of the global horizontal force lists and set to zero.
        rotorHorizontalForce.append(0.0);
        towerHorizontalForce.append(0.0);
        nacelleHorizontalForce.append(0.0);

        // Define the size of the global vertical force lists and set to zero.
        rotorVerticalForce.append(0.0);
        nacelleVerticalForce.append(0.0);

        // Define the size of the aerodynamic torque lists and set to zero.
        rotorTorque.append(0.0);

        // Define the size of the rotor power lists and set to zero.
        rotorPower.append(0.0);

        // Define the size of the generator power lists and set to zero.
        generatorPower.append(0.0);

        // Define the size of the cell-containing-actuator-point-sampling ID list and set to -1.
        bladeMinDisCellID.append(List<List<label> >(NumBl[i], List<label>(numBladePoints[i],-1)));
        nacelleMinDisCellID.append(-1);
        towerMinDisCellID.append(List<label>(numTowerPoints[i],-1));

        DynamicList<label> influenceCellsI;
        bladeInfluenceCells.append(influenceCellsI);
        nacelleInfluenceCells.append(influenceCellsI);
        towerInfluenceCells.append(influenceCellsI);

        bladeProjectionRadius.append(0.0);
        nacelleProjectionRadius.append(0.0);
        towerProjectionRadius.append(0.0);
    }
    Pstream::scatter(bladePointsPerturbVector);
    Pstream::scatter(nacellePointPerturbVector);
    Pstream::scatter(towerPointsPerturbVector);

    updatePositionsFAST();


/*
    // Define the sets of search cells when sampling velocity and projecting
    // the body force.
    for(int i = 0; i < numTurbines; i++)
    {
        findRotorSearchCells(i);
        if (includeNacelle[i])
        {
            findNacelleSearchCells(i);
        }
        if (includeTower[i])
        {
            findTowerSearchCells(i);
        }
    }

    // If there are search cells for a particular turbine, then this processor
    // must do calculations for this turbine, so check for that.
    updateTurbinesControlled();

    // Compute the blade aligned vectors.
    computeBladeAlignedVectors();

    // If the blade body force projection is aligned with streamlines, then
    // compute the radius from the main shaft axis of the CFD mesh cells.
    for (int i = 0; i < numTurbines; i++)
    {
        if (bladeForceProjectionDirection[i] == "localVelocityAligned")
        {
            updateRadius(i);
        }
    }

    // Find out which processors control each actuator line point.
    findBladePointControlProcNo();
    findNacellePointControlProcNo();
    findTowerPointControlProcNo();

    // Compute the wind vectors at this initial time step.
    computeBladePointWindVectors();
    computeNacellePointWindVectors();
    computeTowerPointWindVectors();

    // Compute the blade forces due to this wind at the initial time step.
    computeBladePointForce();
    computeNacellePointForce();
    computeTowerPointForce();

    // Compute the resultant body force at this initial time step.
    computeBladeBodyForce();
    computeNacelleBodyForce();
    computeTowerBodyForce();

    // Open the turbine data output files and print initial information.
    openOutputFiles();
    printOutputFiles();
*/
}

// * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * * * //

void horizontalAxisWindTurbinesALMfastv8::updateFAST()
{
}


void horizontalAxisWindTurbinesALMfastv8::updatePositionsFAST()
{
  // Local point location vector of doubles for communication with FAST.
  double pointLocation[3] = {};

  // Local main shaft unit vector vector of doubles for communication with FAST.
  double uvShaftFAST[3] = {};
 
  // Get this processor's number.
  int p = Pstream::myProcNo();
 
  // Get the total number of FAST points for this processor's turbine.
  int localNumPointsFAST = (NumBl[p] * numBladePointsFAST[p]) + numTowerPointsFAST[p] + 1;;

  // Get the total number of FAST points for all turbines.
  int totalNumPointsFAST = 0;
   forAll(NumBl,i)
   {
       totalNumPointsFAST += (NumBl[i] * numBladePointsFAST[i]) + numTowerPointsFAST[i] + 1;
   }
 
   // Create a local FAST points list.
   List<vector> pointsFAST_(totalNumPointsFAST,vector::zero);

   // Find the start index in that list that belongs to this processor.
   int startIndex = 0;
   for(int i = 0; i < p; i++)
   {
       startIndex += (NumBl[i] * numBladePointsFAST[i]) + numTowerPointsFAST[i] + 1;
   }

   // Call FAST to populate this processor's part of the FAST point list.
   for (int i = 0; i < localNumPointsFAST; i++)
   {
       FAST.getCoordinates(pointLocation,i);
       pointsFAST_[startIndex + i].x() = pointLocation[0] + baseLocation[p].x();
       pointsFAST_[startIndex + i].y() = pointLocation[1] + baseLocation[p].y();
       pointsFAST_[startIndex + i].z() = pointLocation[2] + baseLocation[p].z();     
   }


   // Call FAST to get the main shaft axis for the turbine that this processor's
   // instance of FAST controls.
   FAST.getHubShaftDirection(uvShaftFAST);
   uvShaft[p].x() = uvShaftFAST[0];
   uvShaft[p].y() = uvShaftFAST[1];
   uvShaft[p].z() = uvShaftFAST[2];


   // Parallel sum the list and send back out to all cores.
   Pstream::gather(pointsFAST_,sumOp<List<vector> >());
   Pstream::scatter(pointsFAST_);
   Pstream::gather(uvShaft,sumOp<List<vector> >());
   Pstream::scatter(uvShaft);


   // Put the local FAST points vector entries into the nice ordered
   // lists of FAST points.
   startIndex = 0;
   forAll(bladePointsFAST,i)
   {
       int m = 0;

       // Get rotor apex.
       rotorApex[i] = pointsFAST_[startIndex];
       m++;

       // Get FAST blade points.
       forAll(bladePointsFAST[i],j)
       {
           forAll(bladePointsFAST[i][j],k)
           {
                bladePointsFAST[i][j][k] = pointsFAST_[startIndex + m];
                m++;
           }
       }

       // Get FAST tower points.
       forAll(towerPointsFAST[i],j)
       {
           towerPointsFAST[i][j] = pointsFAST_[startIndex + m];
           m++;
       }

       startIndex += (NumBl[i] * numBladePointsFAST[i]) + numTowerPointsFAST[i] + 1;
   }
}


void horizontalAxisWindTurbinesALMfastv8::updateForcesFAST()
{
}


void horizontalAxisWindTurbinesALMfastv8::getNumBladesFAST()
{
   // Zero the list.
   forAll(NumBl,i)
   {
      NumBl[i] = 0;
   }
   
   // Get the number of blades of the turbine that this processor's instance
   // of FAST controls.
   NumBl[Pstream::myProcNo()] = FAST.get_numBlades(0);

   // Parallel sum and scatter out the list so that all processors know.
   Pstream::gather(NumBl,sumOp<List<int> >());
   Pstream::scatter(NumBl);
}


void horizontalAxisWindTurbinesALMfastv8::getNumBladePointsFAST()
{
   // Zero the list.
   forAll(numBladePointsFAST,i)
   {
      numBladePointsFAST[i] = 0;
   }

   // Get the number of FAST points/blade of the turbine that this processor's
   // instance of FAST controls.
   numBladePointsFAST[Pstream::myProcNo()] = FAST.get_numNodesPerBlade(0);

   // Parallel sum and scatter out the list so that all processors know.
   Pstream::gather(numBladePointsFAST,sumOp<List<int> >());
   Pstream::scatter(numBladePointsFAST);
}


void horizontalAxisWindTurbinesALMfastv8::getNumTowerPointsFAST()
{
   // Zero the list.
   forAll(numTowerPointsFAST,i)
   {
      numTowerPointsFAST[i] = 0;
   }

   // Get the number of FAST points/tower of the turbine that this processor's
   // instance of FAST controls.
   numTowerPointsFAST[Pstream::myProcNo()] = FAST.get_numTwrNodes(0);

   // Parallel sum and scatter out the list so that all processors know.
   Pstream::gather(numTowerPointsFAST,sumOp<List<int> >());
   Pstream::scatter(numTowerPointsFAST);
}


void horizontalAxisWindTurbinesALMfastv8::mapBladePoints()
{
}


void horizontalAxisWindTurbinesALMfastv8::mapTowerPoints()
{
}


void horizontalAxisWindTurbinesALMfastv8::mapBladeForces()
{
}


void horizontalAxisWindTurbinesALMfastv8::mapTowerForces()
{
}


scalar horizontalAxisWindTurbinesALMfastv8::uniformGaussian3D(scalar epsilon, scalar d)
{
    // Compute the 3-dimensional Gaussian.
    scalar f = (1.0 / (Foam::pow(epsilon,3)*Foam::pow(Foam::constant::mathematical::pi,1.5))) * Foam::exp(-Foam::sqr(d/epsilon));
    return f;
}


scalar horizontalAxisWindTurbinesALMfastv8::generalizedGaussian3D(vector epsilon, vector d, vector dir0, vector dir1, vector dir2)
{
    // Compute the 3-dimensional Gaussian that has different spreading in each direction.
    scalar d0 = d & dir0;
    scalar d1 = d & dir1;
    scalar d2 = d & dir2;
  //Info << "epsilon = " << epsilon << endl;
  //Info << "dVector = " << d << endl;
  //Info << "dir0    = " << dir0 << endl;
  //Info << "dir1    = " << dir1 << endl;
  //Info << "dir2    = " << dir2 << endl;
  //Info << "d0      = " << d0 << endl;
  //Info << "d1      = " << d1 << endl;
  //Info << "d2      = " << d2 << endl;
    scalar c = (1.0 / (epsilon[0]*epsilon[1]*epsilon[2]*Foam::pow(Foam::constant::mathematical::pi,1.5)));
    scalar g = Foam::exp( -Foam::sqr(d0/epsilon[0]) -Foam::sqr(d1/epsilon[1]) -Foam::sqr(d2/epsilon[2]) );
    scalar f = c*g;
    return f;
}

scalar horizontalAxisWindTurbinesALMfastv8::generalizedGaussian2D(vector epsilon, vector d, vector dir0, vector dir1)
{
    // Compute the 3-dimensional Gaussian that has different spreading in each direction.
    scalar d0 = d & dir0;
    scalar d1 = d & dir1;
  //Info << "epsilon = " << epsilon << endl;
  //Info << "dVector = " << d << endl;
  //Info << "dir0    = " << dir0 << endl;
  //Info << "dir1    = " << dir1 << endl;
  //Info << "d0      = " << d0 << endl;
  //Info << "d1      = " << d1 << endl;
    scalar c = (1.0 / (epsilon[0]*epsilon[1]*Foam::constant::mathematical::pi));
    scalar g = Foam::exp( -Foam::sqr(d0/epsilon[0]) -Foam::sqr(d1/epsilon[1]) );
    scalar f = c*g;
    return f;
}

scalar horizontalAxisWindTurbinesALMfastv8::diskGaussian(scalar rEpsilon, scalar xEpsilon, vector u, scalar r0, vector d)
{
    // Compute a spreading function that is constant over some width radially, then dies off like a Gaussian,
    // but is also Gaussian in the axial direction.

    // Get the distances between the origin and the point in radial and axial direction.
    scalar dx = d & u;
    vector drVec = d - (dx * u);
    dx = mag(dx);
    scalar dr = mag(drVec);

    // Compute the spreading function
    scalar coeff = 1.0 / (r0 * r0 * xEpsilon * Foam::pow(Foam::constant::mathematical::pi,1.5) +
                  xEpsilon * rEpsilon * rEpsilon * Foam::pow(Foam::constant::mathematical::pi,1.5) + 
                  r0 * xEpsilon * rEpsilon * Foam::sqr(Foam::constant::mathematical::pi));
    scalar f = 1.0;
    if (dr <= r0)
    {
        f = coeff * Foam::exp(-Foam::sqr(dx/xEpsilon));
    }
    else if (dr > r0)
    {
        f = coeff *  Foam::exp(-Foam::sqr(dx/xEpsilon)) * Foam::exp(-Foam::sqr((dr - r0)/rEpsilon));
    }

    return f;
}

scalar horizontalAxisWindTurbinesALMfastv8::ringGaussian(scalar rEpsilon, scalar xEpsilon, vector u, scalar r0, vector d)
{
    // Compute a spreading function that is a ring of Gaussian distribution,
    // but is also Gaussian in the axial direction.

    // Get the distances between the origin and the point in radial and axial direction.
    scalar dx = d & u;
    vector drVec = d - (dx * u);
    dx = mag(dx);
    scalar dr = mag(drVec);

    // Compute the spreading function
    scalar coeff = 1.0 / (xEpsilon * rEpsilon * rEpsilon * Foam::pow(Foam::constant::mathematical::pi,1.5) * exp(-sqr(r0/rEpsilon)) + 
                          r0 * xEpsilon * rEpsilon * Foam::sqr(Foam::constant::mathematical::pi) * (1.0 + erf(r0/rEpsilon)));
    scalar f = 1.0;
    f = coeff *  Foam::exp(-Foam::sqr(dx/xEpsilon)) * Foam::exp(-Foam::sqr((dr - r0)/rEpsilon));

    return f;
}

scalar horizontalAxisWindTurbinesALMfastv8::gaussian1D(scalar x, scalar x0, scalar epsilon, scalar coeff)
{
    // Compute a 1D Gaussian function centered about x0 with width epsilon and scaled by coeff.
    scalar f = coeff * Foam::exp(-Foam::sqr((x - x0)/epsilon));

    return f;
}



vector horizontalAxisWindTurbinesALMfastv8::rotateVector(vector v, vector translation, vector axis, scalar angle)
{
    // Declare and define the rotation matrix.
    tensor RM;
    RM.xx() = Foam::sqr(axis.x()) + (1.0 - Foam::sqr(axis.x())) * Foam::cos(angle); 
    RM.xy() = axis.x() * axis.y() * (1.0 - Foam::cos(angle)) - axis.z() * Foam::sin(angle); 
    RM.xz() = axis.x() * axis.z() * (1.0 - Foam::cos(angle)) + axis.y() * Foam::sin(angle);
    RM.yx() = axis.x() * axis.y() * (1.0 - Foam::cos(angle)) + axis.z() * Foam::sin(angle); 
    RM.yy() = Foam::sqr(axis.y()) + (1.0 - Foam::sqr(axis.y())) * Foam::cos(angle);
    RM.yz() = axis.y() * axis.z() * (1.0 - Foam::cos(angle)) - axis.x() * Foam::sin(angle);
    RM.zx() = axis.x() * axis.z() * (1.0 - Foam::cos(angle)) - axis.y() * Foam::sin(angle);
    RM.zy() = axis.y() * axis.z() * (1.0 - Foam::cos(angle)) + axis.x() * Foam::sin(angle);
    RM.zz() = Foam::sqr(axis.z()) + (1.0 - Foam::sqr(axis.z())) * Foam::cos(angle);

    // Rotation matrices make a rotation about the origin, so need to subtract rotation point
    // off the point to be rotated.
    v = v - translation;

    // Perform the rotation.
    v = RM & v;

    // Return the rotated point to its new location relative to the rotation point.
    v = v + translation;

    return v;
}



vector horizontalAxisWindTurbinesALMfastv8::transformVectorCartToLocal(vector v, vector xP, vector yP, vector zP)
{
    // Transform from the Cartesian (x,y,z) system into the local (x',y',z')
    // system using v' = T'v
    //
    //    x' is aligned with the flow
    //    y' is the cross product of z' and x'
    //    z' is in the boundary face normal direction
    //
    // These vectors are unit vectors.  The vectors make up the rows of
    // the rotation matrix T', which rotates from (x,y,z) to (x',y',z').

    scalar zPMag;
    zPMag = mag(zP);
    zP = zP/zPMag;

    scalar xPMag;
    xPMag = mag(xP);
    xP = xP/xPMag;

    scalar yPMag;
    yPMag = mag(yP);
    yP = yP/yPMag;

    // Create T'
    tensor TP;
    TP.xx() = xP.x();
    TP.xy() = xP.y();
    TP.xz() = xP.z();
    TP.yx() = yP.x();
    TP.yy() = yP.y();
    TP.yz() = yP.z();
    TP.zx() = zP.x();
    TP.zy() = zP.y();
    TP.zz() = zP.z();

    // Transform the vector from Cartesian to local
    vector vP = TP & v;

    return vP;
}




vector horizontalAxisWindTurbinesALMfastv8::transformVectorLocalToCart(vector vP, vector xP, vector yP, vector zP)
{
    // Transform from the local (x',y',z') system to the Cartesian (x,y,z) system 
    // using v = Tv'
    //
    //    x' is aligned with the flow
    //    y' is the cross product of z' and x'
    //    z' is in the boundary face normal direction
    //
    // These vectors are unit vectors.  The vectors make up the rows of
    // the rotation matrix T', which rotates from (x,y,z) to (x',y',z').
    // T can be recovered from T' because it is the inverse. T' is
    // such that the (T')^-1 = transpose(T') because it is made up of
    // orthogonal basis vectors. 

    scalar zPMag;
    zPMag = mag(zP);
    zP = zP/zPMag;

    scalar xPMag;
    xPMag = mag(xP);
    xP = xP/xPMag;

    scalar yPMag;
    yPMag = mag(yP);
    yP = yP/yPMag;

    // Create T'
    tensor TP;
    TP.xx() = xP.x();
    TP.xy() = xP.y();
    TP.xz() = xP.z();
    TP.yx() = yP.x();
    TP.yy() = yP.y();
    TP.yz() = yP.z();
    TP.zx() = zP.x();
    TP.zy() = zP.y();
    TP.zz() = zP.z();

    // Create T
    tensor T = TP.T();

    // Transform the vector from Cartesian to local
    vector v = T & vP;

    return v;
}




scalar horizontalAxisWindTurbinesALMfastv8::interpolate(scalar xNew, DynamicList<scalar>& xOld, DynamicList<scalar>& yOld)
{
    label index = 0;
    label indexP = 0;
    label indexM = 0;
    scalar error = 1.0E30;
    forAll(xOld, i)
    {
        scalar diff = mag(xNew - xOld[i]);
        if(diff < error)
        {
            index = i;
            error = diff;
        }
    }
    if (xNew < xOld[index])
    {
        if (index == 0)
        {
            indexP = 1;
            indexM = indexP - 1;
        }
        else
        {
            indexP = index;
            indexM = indexP - 1;
        }
        return yOld[indexM] + ((yOld[indexP] - yOld[indexM])/(xOld[indexP] - xOld[indexM]))*(xNew - xOld[indexM]);
    }
    else if (xNew > xOld[index])
    {
        if (index == xOld.size() - 1)
        {
            indexP = xOld.size() - 1;
            indexM = indexP - 1;
        }
        else
        {
            indexP = index + 1;
            indexM = indexP - 1;
        }
        return yOld[indexM] + ((yOld[indexP] - yOld[indexM])/(xOld[indexP] - xOld[indexM]))*(xNew - xOld[indexM]);
    }
    else if (xNew == xOld[index])
    {
        return yOld[index];
    }
    else
    {
        return 0.0;
    }
}


label horizontalAxisWindTurbinesALMfastv8::interpolate(scalar xNew, DynamicList<scalar>& xOld, DynamicList<label>& yOld)
{
    label index = 0;
    label indexP = 0;
    label indexM = 0;
    scalar error = 1.0E30;
    forAll(xOld, i)
    {
        scalar diff = mag(xNew - xOld[i]);
        if(diff < error)
        {
            index = i;
            error = diff;
        }
    }
    if (xNew < xOld[index])
    {
        if (index == 0)
        {
            indexP = 1;
            indexM = indexP - 1;
        }
        else
        {
            indexP = index;
            indexM = indexP - 1;
        }
        return round(yOld[indexM] + ((yOld[indexP] - yOld[indexM])/(xOld[indexP] - xOld[indexM]))*(xNew - xOld[indexM]));
    }
    else if (xNew > xOld[index])
    {
        if (index == xOld.size() - 1)
        {
            indexP = xOld.size() - 1;
            indexM = indexP - 1;
        }
        else
        {
            indexP = index + 1;
            indexM = indexP - 1;
        }
        return round(yOld[indexM] + ((yOld[indexP] - yOld[indexM])/(xOld[indexP] - xOld[indexM]))*(xNew - xOld[indexM]));
    }
    else if (xNew == xOld[index])
    {
        return yOld[index];
    }
    else
    {
        return 0.0;
    }
}

scalar horizontalAxisWindTurbinesALMfastv8::compassToStandard(scalar dir)
{
    dir += 180.0;
    if (dir >= 360.0)
    {
       dir -= 360.0;
    }
    dir = 90.0 - dir;
    if (dir < 0.0)
    {
        dir = dir + 360.0;
    }
    return dir;
}

scalar horizontalAxisWindTurbinesALMfastv8::standardToCompass(scalar dir)
{
    dir = 90.0 - dir;
    if (dir < 0.0)
    {
        dir += 360.0;
    }
    dir += 180.0;
    if (dir >= 360.0)
    {
        dir -= 360.0;
    }
    return dir;
}
    
void horizontalAxisWindTurbinesALMfastv8::update()
{
    // Update the time step size.
    dt = runTime_.deltaT().value();

    // Update the current simulation time.
    time = runTime_.timeName();
    t = runTime_.value();

/*
    if(actuatorUpdateType[0] == "oldPosition")
    {
        // Find out which processor controls which actuator point,
        // and with that informatio sample the wind at the actuator
        // points.
        findBladePointControlProcNo();
        findNacellePointControlProcNo();
        findTowerPointControlProcNo();

        computeBladePointWindVectors();
        computeNacellePointWindVectors();
        computeTowerPointWindVectors();

        // Update the rotor state.
      //filterRotSpeed();
      //controlGenTorque();
      //controlBladePitch();
      //controlNacYaw();
      //computeRotSpeed();
      //rotateBlades();
      //yawNacelle();

        // Find search cells.
        for(int i = 0; i < numTurbines; i++)
        {
            if (deltaNacYaw[i] != 0.0)
            {
                findRotorSearchCells(i);
                if (includeNacelle[i])
                {
                    findNacelleSearchCells(i);
                }
                if (includeTower[i])
                {
                    findTowerSearchCells(i);
                }
            }
        }
        updateTurbinesControlled();

        // Recompute the blade-aligned coordinate system.
        computeBladeAlignedVectors();

        // Recompute radius from main shaft axis if body force
        // is projected normal to the streamlines
        for (int i = 0; i < numTurbines; i++)
        {
            if ((bladeForceProjectionDirection[i] == "localVelocityAligned") &&
                (deltaNacYaw[i] != 0.0))
            {
                updateRadius(i);
            }
        }
    }
    else if(actuatorUpdateType[0] == "newPosition")
    {
        // Update the rotor state.
      //filterRotSpeed();
      //controlGenTorque();
      //controlBladePitch();
      //controlNacYaw();
      //computeRotSpeed();
      //rotateBlades();
      //yawNacelle();

        // Find search cells.
        for(int i = 0; i < numTurbines; i++)
        {
            if (deltaNacYaw[i] != 0.0)
            {
                findRotorSearchCells(i);
                if (includeNacelle[i])
                {
                    findNacelleSearchCells(i);
                }
                if (includeTower[i])
                {
                    findTowerSearchCells(i);
                }
            }
        }

        updateTurbinesControlled();

        // Recompute the blade-aligned coordinate system.
        computeBladeAlignedVectors();

        // Recompute radius from main shaft axis if body force
        // is projected normal to the streamlines
        for (int i = 0; i < numTurbines; i++)
        {
            if ((bladeForceProjectionDirection[i] == "localVelocityAligned") &&
                (deltaNacYaw[i] != 0.0))
            {
                updateRadius(i);
            }
        }

        // Find out which processor controls which actuator point,
        // and with that information sample the wind at the actuator
        // points.
        findBladePointControlProcNo();
        findNacellePointControlProcNo();
        findTowerPointControlProcNo();

        computeBladePointWindVectors();
        computeNacellePointWindVectors();
        computeTowerPointWindVectors();
    }

    // Compute the actuator point forces.
    computeBladePointForce();
    computeNacellePointForce();
    computeTowerPointForce();

    // Zero out the body forces.
    bodyForce *= 0.0;

    // Project the actuator forces as body forces.
    computeBladeBodyForce();
    computeNacelleBodyForce();
    computeTowerBodyForce();

    // Print turbine output to file.
    outputIndex++;

    if (outputControl == "timeStep")
    {
        if (outputIndex >= outputInterval)
        {
            outputIndex = 0;
            printOutputFiles();
        }
    }
    else if (outputControl == "runTime")
    {
        if ((runTime_.value() - lastOutputTime) >= outputInterval)
        {
            lastOutputTime += outputInterval;
            printOutputFiles();
        }
    }
    else
    {
        printOutputFiles();
    }

    // Now that at least the first time step is finished, set pastFirstTimeStep
    // to true.

*/

    pastFirstTimeStep = true;
}


void horizontalAxisWindTurbinesALMfastv8::openOutputFiles()
{
    if (Pstream::master())
    {
        // Create the name of the root of where turbine files get ouput.
        fileName rootDir;
        fileName postProcDir;

        if (Pstream::parRun())
        {
            postProcDir = runTime_.path()/"../postProcessing";
            rootDir = runTime_.path()/"../postProcessing/turbineOutput";
        }
        else
        {
            postProcDir = runTime_.path()/"postProcessing";
            rootDir = runTime_.path()/"postProcessing/turbineOutput";
        }

        // Check to see if the postProcessing directory exists; if not, create it.   
        if (!isDir(postProcDir))
        {
            mkDir(postProcDir);
        }

        // Check to see if the turbineOutput directory exists; if not, create it.    
        if (!isDir(rootDir))
        {
            mkDir(rootDir);
        }

        // Check to see if the start time directory exists within the turbineOutput directory; if not, create it.  
        if (!isDir(rootDir/time))
        {
            mkDir(rootDir/time);
        }


    // BLADE-POINT RELATED FILES
        bladePointAlphaFile_ = new OFstream(rootDir/time/"bladePointAlpha");
       *bladePointAlphaFile_ << "#Turbine    Blade    Time(s)    dt(s)    angle-of-attack (degrees)" << endl;

        bladePointVmagFile_ = new OFstream(rootDir/time/"bladePointVmag");
       *bladePointVmagFile_ << "#Turbine    Blade    Time(s)    dt(s)    Vmag (m/s)" << endl;
    
        bladePointVaxialFile_ = new OFstream(rootDir/time/"bladePointVaxial");
       *bladePointVaxialFile_ << "#Turbine    Blade    Time(s)    dt(s)    Vaxial (m/s)" << endl;

        bladePointVtangentialFile_ = new OFstream(rootDir/time/"bladePointVtangential");
       *bladePointVtangentialFile_ << "#Turbine    Blade    Time(s)    dt(s)    Vtangential (m/s)" << endl;

        bladePointVradialFile_ = new OFstream(rootDir/time/"bladePointVradial");
       *bladePointVradialFile_ << "#Turbine    Blade    Time(s)    dt(s)    Vradial (m/s)" << endl;

        bladePointClFile_ = new OFstream(rootDir/time/"bladePointCl");
       *bladePointClFile_ << "#Turbine    Blade    Time(s)    dt(s)    Cl" << endl;

        bladePointCdFile_ = new OFstream(rootDir/time/"bladePointCd");
       *bladePointCdFile_ << "#Turbine    Blade    Time(s)    dt(s)    Cd" << endl;

        bladePointLiftFile_ = new OFstream(rootDir/time/"bladePointLift");
       *bladePointLiftFile_ << "#Turbine    Blade    Time(s)    dt(s)    lift (N)" << endl;

        bladePointDragFile_ = new OFstream(rootDir/time/"bladePointDrag");
       *bladePointDragFile_ << "#Turbine    Blade    Time(s)    dt(s)    drag (N)" << endl;

        bladePointAxialForceFile_ = new OFstream(rootDir/time/"bladePointAxialForce");
       *bladePointAxialForceFile_ << "#Turbine    Blade    Time(s)    dt(s)    axial force (N)" << endl;

        bladePointHorizontalForceFile_ = new OFstream(rootDir/time/"bladePointHorizontalForce");
       *bladePointHorizontalForceFile_ << "#Turbine    Blade    Time(s)    dt(s)    horizontal force (N)" << endl;

        bladePointVerticalForceFile_ = new OFstream(rootDir/time/"bladePointVerticalForce");
       *bladePointVerticalForceFile_ << "#Turbine    Blade    Time(s)    dt(s)    vertical force (N)" << endl;

        bladePointTorqueFile_ = new OFstream(rootDir/time/"bladePointTorqueForce");
       *bladePointTorqueFile_ << "#Turbine    Blade    Time(s)    dt(s)    torque (N-m)" << endl;

        bladePointXFile_ = new OFstream(rootDir/time/"bladePointX");
       *bladePointXFile_ << "#Turbine    Blade    Time(s)    dt(s)    x-location (m)" << endl;

        bladePointYFile_ = new OFstream(rootDir/time/"bladePointY");
       *bladePointYFile_ << "#Turbine    Blade    Time(s)    dt(s)    y-location (m)" << endl;

        bladePointZFile_ = new OFstream(rootDir/time/"bladePointZ");
       *bladePointZFile_ << "#Turbine    Blade    Time(s)    dt(s)    z-location (m)" << endl;



    // NACELLE-POINT RELATED FILES
        nacellePointVmagFile_ = new OFstream(rootDir/time/"nacellePointVmag");
       *nacellePointVmagFile_ << "#Turbine    Time(s)    dt(s)    Vmag (m/s)" << endl;
    
        nacellePointVaxialFile_ = new OFstream(rootDir/time/"nacellePointVaxial");
       *nacellePointVaxialFile_ << "#Turbine    Time(s)    dt(s)    Vaxial (m/s)" << endl;

        nacellePointVhorizontalFile_ = new OFstream(rootDir/time/"nacellePointVhorizontal");
       *nacellePointVhorizontalFile_ << "#Turbine    Time(s)    dt(s)    Vhorizontal (m/s)" << endl;

        nacellePointVverticalFile_ = new OFstream(rootDir/time/"nacellePointVvertical");
       *nacellePointVverticalFile_ << "#Turbine    Time(s)    dt(s)    Vvertical (m/s)" << endl;

        nacellePointDragFile_ = new OFstream(rootDir/time/"nacellePointDrag");
       *nacellePointDragFile_ << "#Turbine    Time(s)    dt(s)    drag (N)" << endl;

        nacellePointAxialForceFile_ = new OFstream(rootDir/time/"nacellePointAxialForce");
       *nacellePointAxialForceFile_ << "#Turbine    Time(s)    dt(s)    axial force (N)" << endl;

        nacellePointHorizontalForceFile_ = new OFstream(rootDir/time/"nacellePointHorizontalForce");
       *nacellePointHorizontalForceFile_ << "#Turbine    Time(s)    dt(s)    horizontal force (N)" << endl;

        nacellePointVerticalForceFile_ = new OFstream(rootDir/time/"nacellePointVerticalForce");
       *nacellePointVerticalForceFile_ << "#Turbine    Time(s)    dt(s)    vertical force (N)" << endl;



    // TOWER-POINT RELATED FILES
        towerPointAlphaFile_ = new OFstream(rootDir/time/"towerPointAlpha");
       *towerPointAlphaFile_ << "#Turbine    Time(s)    dt(s)    angle-of-attack (deg)" << endl;

        towerPointVmagFile_ = new OFstream(rootDir/time/"towerPointVmag");
       *towerPointVmagFile_ << "#Turbine    Time(s)    dt(s)    Vmag (m/s)" << endl;
    
        towerPointVaxialFile_ = new OFstream(rootDir/time/"towerPointVaxial");
       *towerPointVaxialFile_ << "#Turbine    Time(s)    dt(s)    Vaxial (m/s)" << endl;

        towerPointVhorizontalFile_ = new OFstream(rootDir/time/"towerPointVhorizontal");
       *towerPointVhorizontalFile_ << "#Turbine    Time(s)    dt(s)    Vhorizontal (m/s)" << endl;

        towerPointVverticalFile_ = new OFstream(rootDir/time/"towerPointVvertical");
       *towerPointVverticalFile_ << "#Turbine    Time(s)    dt(s)    Vvertical (m/s)" << endl;
       
        towerPointClFile_ = new OFstream(rootDir/time/"towerPointCl");
       *towerPointClFile_ << "#Turbine    Time(s)    dt(s)    Cl" << endl;

        towerPointCdFile_ = new OFstream(rootDir/time/"towerPointCd");
       *towerPointCdFile_ << "#Turbine    Time(s)    dt(s)    Cd" << endl;

        towerPointLiftFile_ = new OFstream(rootDir/time/"towerPointLift");
       *towerPointLiftFile_ << "#Turbine    Time(s)    dt(s)    lift (N)" << endl;

        towerPointDragFile_ = new OFstream(rootDir/time/"towerPointDrag");
       *towerPointDragFile_ << "#Turbine    Time(s)    dt(s)    drag (N)" << endl;

        towerPointAxialForceFile_ = new OFstream(rootDir/time/"towerPointAxialForce");
       *towerPointAxialForceFile_ << "#Turbine    Time(s)    dt(s)    axial force (N)" << endl;

        towerPointHorizontalForceFile_ = new OFstream(rootDir/time/"towerPointHorizontalForce");
       *towerPointHorizontalForceFile_ << "#Turbine    Time(s)    dt(s)    horizontal force (N)" << endl;

        towerPointVerticalForceFile_ = new OFstream(rootDir/time/"towerPointVerticalForce");
       *towerPointVerticalForceFile_ << "#Turbine    Time(s)    dt(s)    vertical force (N)" << endl;




        // ROTOR-RELATED FILES
        rotorTorqueFile_ = new OFstream(rootDir/time/"rotorTorque");
       *rotorTorqueFile_ << "#Turbine    Time(s)    dt(s)    rotor torque (N-m)" << endl;

        rotorAxialForceFile_ = new OFstream(rootDir/time/"rotorAxialForce");
       *rotorAxialForceFile_ << "#Turbine    Time(s)    dt(s)    rotor axial force (N)" << endl;

        rotorHorizontalForceFile_ = new OFstream(rootDir/time/"rotorHorizontalForce");
       *rotorHorizontalForceFile_ << "#Turbine    Time(s)    dt(s)    rotor horizontal force (N)" << endl;

        rotorVerticalForceFile_ = new OFstream(rootDir/time/"rotorVerticalForce");
       *rotorVerticalForceFile_ << "#Turbine    Time(s)    dt(s)    rotor vertical force (N)" << endl;

        rotorPowerFile_ = new OFstream(rootDir/time/"rotorPower");
       *rotorPowerFile_ << "#Turbine    Time(s)    dt(s)    rotor power (W)" << endl;

        generatorPowerFile_ = new OFstream(rootDir/time/"generatorPower");
       *generatorPowerFile_ << "#Turbine    Time(s)    dt(s)    generator power (W)" << endl;

        rotorSpeedFile_ = new OFstream(rootDir/time/"rotorSpeed");
       *rotorSpeedFile_ << "#Turbine    Time(s)    dt(s)    rotor rotation rate(rpm)" << endl;

        rotorSpeedFFile_ = new OFstream(rootDir/time/"rotorSpeedFiltered");
       *rotorSpeedFFile_ << "#Turbine    Time(s)    dt(s)    filtered rotor rotation rate(rpm)" << endl;

        rotorAzimuthFile_ = new OFstream(rootDir/time/"rotorAzimuth");
       *rotorAzimuthFile_ << "#Turbine    Time(s)    dt(s)    blade 1 azimuth angle (degrees)" << endl;



    // NACELLE-RELATED FILES 
        nacelleAxialForceFile_ = new OFstream(rootDir/time/"nacelleAxialForce");
       *nacelleAxialForceFile_ << "#Turbine    Time(s)    dt(s)    nacelle axial force (N)" << endl;

        nacelleHorizontalForceFile_ = new OFstream(rootDir/time/"nacelleHorizontalForce");
       *nacelleHorizontalForceFile_ << "#Turbine    Time(s)    dt(s)    nacelle horizontal force (N)" << endl;

        nacelleVerticalForceFile_ = new OFstream(rootDir/time/"nacelleVerticalForce");
       *nacelleVerticalForceFile_ << "#Turbine    Time(s)    dt(s)    nacelle vertical force (N)" << endl;

        nacelleYawFile_ = new OFstream(rootDir/time/"nacelleYaw");
       *nacelleYawFile_ << "#Turbine    Time(s)    dt(s)    nacelle yaw angle (degrees)" << endl;



    // TOWER-RELATED FILES 
        towerAxialForceFile_ = new OFstream(rootDir/time/"towerAxialForce");
       *towerAxialForceFile_ << "#Turbine    Time(s)    dt(s)    tower axial force (N)" << endl;

        towerHorizontalForceFile_ = new OFstream(rootDir/time/"towerHorizontalForce");
       *towerHorizontalForceFile_ << "#Turbine    Time(s)    dt(s)    tower horizontal force (N)" << endl;


    
    // BLADE-RELATED FILES
        bladePitchFile_ = new OFstream(rootDir/time/"bladePitch");
       *bladePitchFile_ << "#Turbine    Time(s)    dt(s)    blade pitch angle (degrees)" << endl;

       

    // GENERATOR-RELATED FILES
        generatorTorqueFile_ = new OFstream(rootDir/time/"generatorTorque");
       *generatorTorqueFile_ << "#Turbine    Time(s)    dt(s)    generator torque (N-m)" << endl;

    }
}


void horizontalAxisWindTurbinesALMfastv8::printOutputFiles()
{
    if (Pstream::master())
    {
        forAll(rotorPower,i)
        {
            // Get necessary axes.
            vector axialVector = uvShaft[i];
            axialVector.z() = 0.0;
            axialVector = axialVector / mag(axialVector);
            vector verticalVector = vector::zero;
            verticalVector.z() = 1.0;
            vector horizontalVector = -(axialVector ^ verticalVector);
            horizontalVector = horizontalVector / mag(horizontalVector);


            // Write out time and delta t for non-blade point quantities.
            *nacellePointVmagFile_ << i << " " << time << " " << dt << " ";
            *nacellePointVaxialFile_ << i << " " << time << " " << dt << " ";
            *nacellePointVhorizontalFile_ << i << " " << time << " " << dt << " ";
            *nacellePointVverticalFile_ << i << " " << time << " " << dt << " ";
            *nacellePointDragFile_ << i << " " << time << " " << dt << " ";
            *nacellePointAxialForceFile_ << i << " " << time << " " << dt << " ";
            *nacellePointHorizontalForceFile_ << i << " " << time << " " << dt << " ";
            *nacellePointVerticalForceFile_ << i << " " << time << " " << dt << " ";
            *towerPointAlphaFile_ << i << " " << time << " " << dt << " ";
            *towerPointVmagFile_ << i << " " << time << " " << dt << " ";
            *towerPointVaxialFile_ << i << " " << time << " " << dt << " ";
            *towerPointVhorizontalFile_ << i << " " << time << " " << dt << " ";
            *towerPointVverticalFile_ << i << " " << time << " " << dt << " ";
            *towerPointClFile_ << i << " " << time << " " << dt << " ";
            *towerPointCdFile_ << i << " " << time << " " << dt << " ";
            *towerPointLiftFile_ << i << " " << time << " " << dt << " ";
            *towerPointDragFile_ << i << " " << time << " " << dt << " ";
            *towerPointAxialForceFile_ << i << " " << time << " " << dt << " ";
            *towerPointHorizontalForceFile_ << i << " " << time << " " << dt << " ";
            *towerPointVerticalForceFile_ << i << " " << time << " " << dt << " ";
            *rotorTorqueFile_ << i << " " << time << " " << dt << " ";
            *rotorAxialForceFile_ << i << " " << time << " " << dt << " ";
            *rotorHorizontalForceFile_ << i << " " << time << " " << dt << " ";
            *rotorVerticalForceFile_ << i << " " << time << " " << dt << " ";
            *rotorPowerFile_ << i << " " << time << " " << dt << " ";
            *generatorPowerFile_ << i << " " << time << " " << dt << " ";
            *rotorSpeedFile_ << i << " " << time << " " << dt << " ";
            *rotorSpeedFFile_ << i << " " << time << " " << dt << " ";
            *rotorAzimuthFile_ << i << " " << time << " " << dt << " ";
            *nacelleAxialForceFile_ << i << " " << time << " " << dt << " ";
            *nacelleHorizontalForceFile_ << i << " " << time << " " << dt << " ";
            *nacelleVerticalForceFile_ << i << " " << time << " " << dt << " ";
            *nacelleYawFile_ << i << " " << time << " " << dt << " ";
            *towerAxialForceFile_ << i << " " << time << " " << dt << " ";
            *towerHorizontalForceFile_ << i << " " << time << " " << dt << " ";
            *generatorTorqueFile_ << i << " " << time << " " << dt << " ";
            *bladePitchFile_ << i << " " << time << " " << dt << " ";


            // Write out bulk information for each turbine (i.e., quantities not distributed
            // along points).
            *rotorTorqueFile_ << rotorTorque[i]*fluidDensity[i] << endl;
            *rotorAxialForceFile_ << rotorAxialForce[i]*fluidDensity[i] << endl;
            *rotorHorizontalForceFile_ << rotorHorizontalForce[i]*fluidDensity[i] << endl;
            *rotorVerticalForceFile_ << rotorVerticalForce[i]*fluidDensity[i] << endl;
            *rotorPowerFile_ << rotorPower[i]*fluidDensity[i] << endl;
            *generatorPowerFile_ << generatorPower[i]*fluidDensity[i] << endl;
            *rotorSpeedFile_ << rotorSpeed[i]/rpmRadSec << endl;
            *rotorSpeedFFile_ << rotorSpeedF[i]/rpmRadSec << endl;
            *rotorAzimuthFile_ << rotorAzimuth[i]/degRad << endl;
            *nacelleAxialForceFile_ << nacelleAxialForce[i]*fluidDensity[i] << endl;
            *nacelleHorizontalForceFile_ << nacelleHorizontalForce[i]*fluidDensity[i] << endl;
            *nacelleVerticalForceFile_ << nacelleVerticalForce[i]*fluidDensity[i] << endl;
            *nacelleYawFile_ <<  standardToCompass(nacYaw[i]/degRad) << endl;
            *towerAxialForceFile_ << towerAxialForce[i]*fluidDensity[i] << endl;
            *towerHorizontalForceFile_ << towerHorizontalForce[i]*fluidDensity[i] << endl;
            *generatorTorqueFile_ << generatorTorque[i] << endl;
            *bladePitchFile_ << bladePitch[i] << endl;


            // Write out information for the nacelle, which is distributed along a points, but along
            // only one set of points (as opposed to blades of which there are multiple sets of points)
            forAll(nacellePoints[i], k)
            {
                *nacellePointVmagFile_ << nacellePointVmag[i][k] << " ";
                *nacellePointVaxialFile_ << (nacelleWindVector[i] & axialVector) << " ";
                *nacellePointVhorizontalFile_ << (nacelleWindVector[i] & horizontalVector) << " ";
                *nacellePointVverticalFile_ << (nacelleWindVector[i] & verticalVector) << " ";
                *nacellePointDragFile_ << nacellePointDrag[i][k]*fluidDensity[i] << " ";
                *nacellePointAxialForceFile_ << nacellePointAxialForce[i][k]*fluidDensity[i] << " ";
                *nacellePointHorizontalForceFile_ << nacellePointHorizontalForce[i][k]*fluidDensity[i] << " ";
                *nacellePointVerticalForceFile_ << nacellePointVerticalForce[i][k]*fluidDensity[i] << " ";
            }
            *nacellePointVmagFile_ << endl;
            *nacellePointVaxialFile_ << endl;
            *nacellePointVhorizontalFile_ << endl;;
            *nacellePointVverticalFile_ << endl;
            *nacellePointDragFile_ << endl;
            *nacellePointAxialForceFile_ << endl;
            *nacellePointHorizontalForceFile_ << endl;
            *nacellePointVerticalForceFile_ << endl;



        // Write out information for the tower, which is distributed along a points, but along
        // only one set of points (as opposed to blades of which there are multiple sets of points)
        forAll(towerPoints[i], k)
            {
                *towerPointAlphaFile_ << towerPointAlpha[i][k] << " "; 
                *towerPointVmagFile_ << towerPointVmag[i][k] << " "; 
                *towerPointVaxialFile_ << (towerWindVectors[i][k] & axialVector) << " "; 
                *towerPointVhorizontalFile_ << (towerWindVectors[i][k] & horizontalVector) << " "; 
                *towerPointVverticalFile_ << (towerWindVectors[i][k] & verticalVector) << " "; 
                *towerPointClFile_ << towerPointCl[i][k] << " "; 
                *towerPointCdFile_ << towerPointCd[i][k] << " "; 
                *towerPointLiftFile_ << towerPointLift[i][k]*fluidDensity[i] << " "; 
                *towerPointDragFile_ << towerPointDrag[i][k]*fluidDensity[i] << " "; 
                *towerPointAxialForceFile_ << towerPointAxialForce[i][k]*fluidDensity[i] << " "; 
                *towerPointHorizontalForceFile_ << towerPointHorizontalForce[i][k]*fluidDensity[i] << " "; 
            }
            *towerPointAlphaFile_ << endl;
            *towerPointVmagFile_ << endl;
            *towerPointVaxialFile_ << endl;
            *towerPointVhorizontalFile_ << endl;
            *towerPointVverticalFile_ << endl;
            *towerPointClFile_ << endl;
            *towerPointCdFile_ << endl;
            *towerPointLiftFile_ << endl;
            *towerPointDragFile_ << endl;
            *towerPointAxialForceFile_ << endl;
            *towerPointHorizontalForceFile_ << endl;
            *towerPointVerticalForceFile_ << endl;


            // Write out blade point quantitities.  Go blade by blade.
            forAll(bladePoints[i], j)
            {
                // Write out time and delta t.
                *bladePointAlphaFile_ << i << " " << j << " " << time << " " << dt << " ";
                *bladePointVmagFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointVaxialFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointVtangentialFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointVradialFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointClFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointCdFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointLiftFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointDragFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointAxialForceFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointHorizontalForceFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointVerticalForceFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointTorqueFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointXFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointYFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointZFile_ << i << " " << j << " " <<  time << " " << dt << " ";

                // Write the information for each point on the blade.
                forAll(bladePoints[i][j], k)
                {   
                    *bladePointAlphaFile_ << bladePointAlpha[i][j][k] << " ";
                    *bladePointVmagFile_ << bladePointVmag[i][j][k] << " ";
                    *bladePointVaxialFile_ << bladeWindVectors[i][j][k].x() << " ";
                    *bladePointVtangentialFile_ << bladeWindVectors[i][j][k].y() << " ";
                    *bladePointVradialFile_ << bladeWindVectors[i][j][k].z() << " ";
                    *bladePointClFile_ << bladePointCl[i][j][k] << " ";
                    *bladePointCdFile_ << bladePointCd[i][j][k] << " ";
                    *bladePointLiftFile_ << bladePointLift[i][j][k]*fluidDensity[i] << " ";
                    *bladePointDragFile_ << bladePointDrag[i][j][k]*fluidDensity[i] << " ";
                    *bladePointAxialForceFile_ << bladePointAxialForce[i][j][k]*fluidDensity[i] << " ";
                    *bladePointHorizontalForceFile_ << bladePointHorizontalForce[i][j][k]*fluidDensity[i] << " ";
                    *bladePointVerticalForceFile_ << bladePointVerticalForce[i][j][k]*fluidDensity[i] << " ";
                    *bladePointTorqueFile_ << bladePointTorque[i][j][k]*fluidDensity[i] << " ";
                    *bladePointXFile_ << bladePoints[i][j][k].x() << " ";
                    *bladePointYFile_ << bladePoints[i][j][k].y() << " ";
                    *bladePointZFile_ << bladePoints[i][j][k].z() << " ";
                }

                // End the line so we can go on to the next blade.
                *bladePointAlphaFile_ << endl;
                *bladePointVmagFile_ << endl;
                *bladePointVaxialFile_ << endl;
                *bladePointVtangentialFile_ << endl;
                *bladePointVradialFile_ << endl;
                *bladePointClFile_ << endl;
                *bladePointCdFile_ << endl;
                *bladePointLiftFile_ << endl;
                *bladePointDragFile_ << endl;
                *bladePointAxialForceFile_ << endl;
                *bladePointHorizontalForceFile_ << endl;
                *bladePointVerticalForceFile_ << endl;
                *bladePointTorqueFile_ << endl;
                *bladePointXFile_ << endl;
                *bladePointYFile_ << endl;
                *bladePointZFile_ << endl;
            }
        }




        // Add another space to denote that we are done with a timestep.  The output is blocks
        // of data for each time step, separated by a blank line between time steps.
        *rotorTorqueFile_ << endl;
        *rotorAxialForceFile_ << endl;
        *rotorHorizontalForceFile_ << endl;
        *rotorVerticalForceFile_ << endl;
        *rotorPowerFile_ << endl;
        *generatorPowerFile_ << endl;
        *rotorSpeedFile_ << endl;
        *rotorSpeedFFile_ << endl;
        *rotorAzimuthFile_ << endl;
        *nacelleAxialForceFile_ << endl;
        *nacelleHorizontalForceFile_ << endl;
        *nacelleVerticalForceFile_ << endl;
        *nacelleYawFile_ << endl;
        *towerAxialForceFile_ << endl;
        *towerHorizontalForceFile_ << endl;
        *generatorTorqueFile_ << endl;
        *bladePitchFile_ << endl;

        *bladePointAlphaFile_ << endl;
        *bladePointVmagFile_ << endl;
        *bladePointVaxialFile_ << endl;
        *bladePointVtangentialFile_ << endl;
        *bladePointVradialFile_ << endl;
        *bladePointClFile_ << endl;
        *bladePointCdFile_ << endl;
        *bladePointLiftFile_ << endl;
        *bladePointDragFile_ << endl;
        *bladePointAxialForceFile_ << endl;
        *bladePointTorqueFile_ << endl;
        *bladePointXFile_ << endl;
        *bladePointYFile_ << endl;
        *bladePointZFile_ << endl;

        *nacellePointVmagFile_ << endl;
        *nacellePointVaxialFile_ << endl;
        *nacellePointVhorizontalFile_ << endl;;
        *nacellePointVverticalFile_ << endl;
        *nacellePointDragFile_ << endl;
        *nacellePointAxialForceFile_ << endl;
        *nacellePointHorizontalForceFile_ << endl;
        *nacellePointVerticalForceFile_ << endl;

        *towerPointAlphaFile_ << endl;
        *towerPointVmagFile_ << endl;
        *towerPointVaxialFile_ << endl;
        *towerPointVhorizontalFile_ << endl;
        *towerPointVverticalFile_ << endl;
        *towerPointClFile_ << endl;
        *towerPointCdFile_ << endl;
        *towerPointLiftFile_ << endl;
        *towerPointDragFile_ << endl;
        *towerPointAxialForceFile_ << endl;
        *towerPointHorizontalForceFile_ << endl;
        *towerPointVerticalForceFile_ << endl;
    }
}
   
     
void horizontalAxisWindTurbinesALMfastv8::printDebug()
{
    Info << "Print Debugging Information" << endl;
    Info << "turbineType = " << turbineType << endl;
    Info << "includeNacelle = " << includeNacelle << endl;
    Info << "includeTower = " << includeTower << endl;
    Info << "baseLocation = " << baseLocation << endl;
    Info << "numBladePoints = " << numBladePoints << endl;
    Info << "numNacellePoints = " << numNacellePoints << endl;
    Info << "numTowerPoints = " << numTowerPoints << endl;
    Info << "bladePointDistType = " << bladePointDistType << endl;
    Info << "nacellePointDistType = " << nacellePointDistType << endl;
    Info << "towerPointDistType = " << towerPointDistType << endl;
    Info << "bladeActuatorPointInterpType = " << bladeActuatorPointInterpType << endl;
    Info << "nacelleActuatorPointInterpType = " << nacelleActuatorPointInterpType << endl;
    Info << "towerActuatorPointInterpType = " << towerActuatorPointInterpType << endl;
    Info << "actuatorUpdateType = " << actuatorUpdateType << endl;
    Info << "bladeEpsilon = " << bladeEpsilon << endl;
    Info << "nacelleEpsilon = " << nacelleEpsilon << endl;
    Info << "towerEpsilon = " << towerEpsilon << endl;
    Info << "nacelleSampleDistance = " << nacelleSampleDistance << endl;
    Info << "towerSampleDistance = " << towerSampleDistance << endl;
    Info << "bladeProjectionRadius = " << bladeProjectionRadius << endl;
    Info << "tipRootLossCorrType = " << tipRootLossCorrType << endl;
    Info << "rotationDir = " << rotationDir << endl;
    Info << "rotorSpeed = " << rotorSpeed << endl;
    Info << "rotorSpeedF = " << rotorSpeedF << endl;
    Info << "speedError = " << speedError << endl;
    Info << "intSpeedError = " << intSpeedError << endl;
    Info << "rotorAzimuth = " << rotorAzimuth << endl;
    Info << "generatorTorque = " << generatorTorque << endl;
    Info << "bladePitch = " << bladePitch << endl;
    Info << "nacYaw = " << nacYaw << endl;
    Info << "fluidDensity = " << fluidDensity << endl << endl << endl;
    
    Info << "numTurbinesDistinct = " << numTurbinesDistinct << endl;
    Info << "turbineTypeDistinct = " << turbineTypeDistinct << endl;
    Info << "turbineTypeID = " << turbineTypeID << endl << endl << endl;;

    Info << "NumBl = " << NumBl << endl;
    Info << "TipRad = " << TipRad << endl;
    Info << "HubRad = " << HubRad << endl;
    Info << "UndSling = " << UndSling << endl;
    Info << "OverHang = " << OverHang << endl;
    Info << "nacelleLength = " << nacelleLength << endl;
    Info << "TowerHt = " << TowerHt << endl;
    Info << "Twr2Shft = " << Twr2Shft << endl;
    Info << "ShftTilt = " << ShftTilt << endl;
    Info << "PreCone = " << PreCone << endl;
    Info << "GBRatio = " << GBRatio << endl;
    Info << "GBEfficiency = " << GBEfficiency << endl;
    Info << "GenEfficiency = " << GenEfficiency << endl;
    Info << "RatedRotSpeed = " << RatedRotSpeed << endl;
    Info << "HubIner = " << HubIner << endl;
    Info << "GenIner = " << GenIner << endl;
    Info << "BladeIner = " << BladeIner << endl;
    Info << "GenTorqueControllerType = " << GenTorqueControllerType << endl;
    Info << "NacYawControllerType = " << NacYawControllerType << endl;
    Info << "BladePitchControllerType = " << BladePitchControllerType << endl;
    Info << "RotSpeedLimiter = " << RotSpeedLimiter << endl;
    Info << "GenTorqueRateLimiter = " << GenTorqueRateLimiter << endl;
    Info << "NacYawRateLimiter = " << NacYawRateLimiter << endl;
    Info << "BladePitchRateLimiter = " << BladePitchRateLimiter << endl;
    Info << "SpeedFilterCornerFrequency = " << SpeedFilterCornerFrequency << endl;
    Info << "AirfoilType = " << AirfoilType << endl;
    Info << "BladeData = " << BladeData << endl;
    Info << "BladeStation = " << BladeStation << endl;
    Info << "BladeChord = " << BladeChord << endl;
    Info << "BladeTwist = " << BladeTwist << endl;
    Info << "AirfoilTypesDistinct = " << airfoilTypesDistinct << endl;
    Info << "BladeAirfoilTypeID = " << BladeAirfoilTypeID << endl << endl << endl;

    Info << "airfoilAlpha = " << airfoilAlpha << endl;
    Info << "airfoilCl = " << airfoilCl << endl;
    Info << "airfoilCd = " << airfoilCd << endl;

    Info << "bladeInfluenceCells = " << bladeInfluenceCells << endl << endl << endl;
    Info << "nacelleInfluenceCells = " << nacelleInfluenceCells << endl << endl << endl;
    Info << "towerInfluenceCells = " << towerInfluenceCells << endl << endl << endl;

    Info << "bladeDs = " << bladeDs << endl;
    Info << "towerDs = " << towerDs << endl;
    Info << "nacelleDs = " << nacelleDs << endl;
    Info << "bladePoints = " << bladePoints << endl;
    Info << "towerPoints = " << towerPoints << endl;
    Info << "nacellePoints = " << nacellePoints << endl;
    Info << "bladePointRadius = " << bladePointRadius << endl;
    Info << "towerPointHeight = " << towerPointHeight << endl;
    Info << "towerShaftIntersect = " << towerShaftIntersect << endl;
    Info << "rotorApex = " << rotorApex << endl;
    Info << "uvShaft = " << uvShaft << endl;
//  Info << "uvShaftDir = " << uvShaftDir << endl;
    Info << "uvTower = " << uvTower << endl;
    Info << "deltaNacYaw = " << deltaNacYaw << endl;
    Info << "deltaAzimuth = " << deltaAzimuth << endl;

    Info << "bladePointForce = " << bladePointForce << endl;
    Info << "nacellePointForce = " << nacellePointForce << endl;
    Info << "towerPointForce = " << towerPointForce << endl;
    Info << "bladeWindVectors = " << bladeWindVectors << endl;
    Info << "nacelleWindVector = " << nacelleWindVector << endl;
    Info << "towerWindVectors = " << towerWindVectors << endl;
    Info << "bladeAlignedVectors = " << bladeAlignedVectors << endl;
}


volVectorField& horizontalAxisWindTurbinesALMfastv8::force()
{
    // Return the body force field to the solver
    return bodyForce;
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace turbineModels
} // End namespace Foam

// ************************************************************************* //

