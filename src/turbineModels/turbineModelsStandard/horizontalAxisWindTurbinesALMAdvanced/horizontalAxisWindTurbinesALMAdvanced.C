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

#include "horizontalAxisWindTurbinesALMAdvanced.H"
#include "interpolateXY.H"

namespace Foam
{
namespace turbineModels
{

// * * * * * * * * * * * * * *  Constructor  * * * * * * * * * * * * * * * * //

horizontalAxisWindTurbinesALMAdvanced::horizontalAxisWindTurbinesALMAdvanced
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
    perturb = turbineArrayProperties.subDict("globalProperties").lookupOrDefault<scalar>("perturb",1E-5);
    lastOutputTime = runTime_.startTime().value();
    outputIndex = 0;

    includeNacelleSomeTrue = false;
    includeTowerSomeTrue = false;

    forAll(turbineName,i)
    {
        turbineType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("turbineType")));
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
        NacelleLength.append(scalar(readScalar(turbineProperties.lookup("NacelleLength"))));
        NacelleFrontalArea.append(scalar(readScalar(turbineProperties.lookup("NacelleFrontalArea"))));
        NacelleCd.append(scalar(readScalar(turbineProperties.lookup("NacelleCd"))));
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
        NacelleEquivalentRadius.append(Foam::sqrt(NacelleFrontalArea[i]/Foam::constant::mathematical::pi));


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
            TimeYawTable.append(turbineProperties.subDict("NacYawControllerParams").lookup("TimeYawTable"));
            DynamicList<scalar> timeInt;
            DynamicList<scalar> yawInt;
            forAll(TimeYawTable[i],j)
            {
                timeInt.append(TimeYawTable[i][j][0]);
                yawInt.append(TimeYawTable[i][j][1]);
            }

            TimeYawProfile.append(timeInt);
            YawYawProfile.append(yawInt);

            timeInt.clear();
            yawInt.clear();
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
        int j = turbineTypeID[i];

        // Define which way the shaft points to distinguish between
        // upwind and downwind turbines.
        uvShaftDir.append(OverHang[j]/mag(OverHang[j]));

        // Define the vector along the shaft pointing in the
        // direction of the wind.
        uvShaft.append(rotorApex[i] - towerShaftIntersect[i]);
        uvShaft[i] = (uvShaft[i]/mag(uvShaft[i])) * uvShaftDir[i];
        // Define the vector aligned with the tower pointing from
        // the ground to the nacelle.
        uvTower.append(towerShaftIntersect[i] - baseLocation[i]);
        uvTower[i] = uvTower[i]/mag(uvTower[i]);

        // Now calculate the actuator section center points for each blade
        // of each turbine in the array.  All blades points will be calculated
        // at zero azimuth (blade pointing up), and then rotated to its correct
        // position before doing a global rotation to the initial azimuth of
        // the rotor.  Also calculate the radius of each point (not including coning).

        // Calculate the width of each blade actuator section.
        bladeDs.append(DynamicList<scalar>(0));
        if(bladePointDistType[i] == "uniform")
        {
            scalar actuatorWidth = (TipRad[j]-HubRad[j])/numBladePoints[i];
            for(int m = 0; m < numBladePoints[i]; m++)
            {
                bladeDs[i].append(actuatorWidth);
            }
        }
        // Add other point distribution types here, such as cosine, tanh.
    
        // Calculate the actual locations of the blade actuator elemens and
        // interpolate blade properties to these points.
        bladePoints.append(List<List<vector> >(NumBl[j], List<vector>(numBladePoints[i],vector::zero)));
        bladeSamplePoints.append(List<List<vector> >(NumBl[j], List<vector>(numBladePoints[i],vector::zero)));
        bladePointRadius.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
        bladePointChord.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
        bladePointTwist.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
        bladePointThickness.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
        bladePointUserDef.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
        bladePointAirfoil.append(List<List<label> >(NumBl[j], List<label>(numBladePoints[i],0)));
        for(int k = 0; k < NumBl[j]; k++)
        {
            vector root = rotorApex[i];
            scalar beta = PreCone[j][k] - ShftTilt[j];
            root.x() = root.x() + HubRad[j]*Foam::sin(beta);
            root.z() = root.z() + HubRad[j]*Foam::cos(beta);
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


        // Compute the location of the tower section center points for each
        // turbine tower in the array.

        // Calculate the width of each tower actuator section.
        towerDs.append(DynamicList<scalar>(0));
        if(towerPointDistType[i] == "uniform")
        {
            scalar actuatorWidth = TowerHt[j]/numTowerPoints[i];
            for(int m = 0; m < numTowerPoints[i]; m++)
            {
                towerDs[i].append(actuatorWidth);
            }
        }
        // Add other point distribution types here, such as cosine, tanh.

        // Compute the actual locations of the tower actuator elements and 
        // interpolate tower properties to these points.
        towerPoints.append(List<vector>(numTowerPoints[i],vector::zero));
        towerSamplePoints.append(List<vector>(numTowerPoints[i],vector::zero));
        towerPointHeight.append(List<scalar>(numTowerPoints[i],0.0));
        towerPointChord.append(List<scalar>(numTowerPoints[i],0.0));
        towerPointTwist.append(List<scalar>(numTowerPoints[i],0.0));
        towerPointThickness.append(List<scalar>(numTowerPoints[i],0.0));
        towerPointUserDef.append(List<scalar>(numTowerPoints[i],0.0));
        towerPointAirfoil.append(List<label>(numTowerPoints[i],0));
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



        // Compute the location of the nacelle section points for each turbine
        // in the array.

        // Calculate the width of each nacelle actuator section.
        nacelleDs.append(DynamicList<scalar>(0));
        if(nacellePointDistType[i] == "uniform")
        {
            scalar actuatorWidth = NacelleLength[j]/numNacellePoints[i];
            for(int m = 0; m < numNacellePoints[i]; m++)
            {
                nacelleDs[i].append(actuatorWidth);
            }
        }
        // Add other point distribution types here, such as cosine, tanh.

        // Compute the actual nacelle location points.
        nacellePoints.append(List<vector>(numNacellePoints[i],vector::zero));
        nacelleSamplePoint.append(vector::zero);
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



        // Generate random numbers for the point perturbation during control
        // processor identification.  This does not affect the actual location--it is
        // just there to break ties and make sure > 1 processors don't account for a
        // single actuator point.
        bladePointsPerturbVector.append(List<List<vector> >(NumBl[j], List<vector>(numBladePoints[i],vector::zero)));
        if(Pstream::myProcNo() == 0)
        {
            for(int k =  0; k < NumBl[j]; k++)
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
        bladePointForce.append(List<List<vector> >(NumBl[j], List<vector>(numBladePoints[i],vector::zero)));
        towerPointForce.append(List<vector>(numTowerPoints[i],vector::zero));
        nacellePointForce.append(List<vector>(numNacellePoints[i],vector::zero));
  
        // Define the size of the blade, nacelle, and tower aligned vectors array and set to zero.
        bladeAlignedVectors.append(List<List<vector> >(NumBl[j],List<vector>(3,vector::zero)));

        // Define the actuator element wind vector arrays and set them to zero.
        bladeWindVectors.append(List<List<vector> >(NumBl[j],List<vector>(numBladePoints[i],vector::zero)));
        towerWindVectors.append(List<vector>(numTowerPoints[i],vector::zero));
        nacelleWindVector.append(vector::zero);

        // Define the size of the deltaNacYaw, deltaAzimuth, and deltaPitch lists and set to zero.
        deltaNacYaw.append(0.0);
        deltaAzimuth.append(0.0);

        // Define the size of the angle of attack lists and set to zero.
        bladePointAlpha.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
        towerPointAlpha.append(List<scalar>(numTowerPoints[i],0.0));

        // Define the size of the wind speed magnitude lists and set to zero.
        bladePointVmag.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
        towerPointVmag.append(List<scalar>(numTowerPoints[i],0.0));
        nacellePointVmag.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the coefficient of bladePointLift lists and set to zero.
        bladePointCl.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
        towerPointCl.append(List<scalar>(numTowerPoints[i],0.0));

        // Define the size of the coefficient of drag lists and set to zero.
        bladePointCd.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
        towerPointCd.append(List<scalar>(numTowerPoints[i],0.0));
        nacellePointCd.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the bladePointLift lists and set to zero.
        bladePointLift.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
        towerPointLift.append(List<scalar>(numTowerPoints[i],0.0));

        // Define the size of the drag lists and set to zero.
        bladePointDrag.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
        towerPointDrag.append(List<scalar>(numTowerPoints[i],0.0));
        nacellePointDrag.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the axial force lists and set to zero.
        bladePointAxialForce.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
        towerPointAxialForce.append(List<scalar>(numTowerPoints[i],0.0));
        nacellePointAxialForce.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the horizontal force lists and set to zero.
        bladePointHorizontalForce.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
        towerPointHorizontalForce.append(List<scalar>(numTowerPoints[i],0.0));
        nacellePointHorizontalForce.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the horizontal force lists and set to zero.
        bladePointVerticalForce.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));
        nacellePointVerticalForce.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the torque lists and set to zero.
        bladePointTorque.append(List<List<scalar> >(NumBl[j], List<scalar>(numBladePoints[i],0.0)));

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
        bladeMinDisCellID.append(List<List<label> >(NumBl[j], List<label>(numBladePoints[i],-1)));
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



    // Yaw the nacelle to initial position.
    deltaNacYaw = nacYaw;
    yawNacelle();

    // Rotate the rotor to initial azimuth angle.
    deltaAzimuth =  rotorAzimuth;
    rotateBlades(); 

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
}

// * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * * * //

void horizontalAxisWindTurbinesALMAdvanced::findRotorSearchCells(int turbineNumber)
{
    // Define the cells that can possibly be influenced by the force
    // exerted each turbine by the rotor.  In otherwords, define a set 
    // of cell IDs around each turbine that will be saved into memory 
    // so that the entire domain need not be passed through when  
    // applying the force field.  (The i-index is at the turbine array  
    // level for each turbine, the m-index is for each type of turbine--
    // if all turbines are the same, m is always 0, and the j-index is 
    // at the individual blade level.)
    int i = turbineNumber;
    
    int m = turbineTypeID[i];

    // First compute the radius of the force projection (to the radius
    // where the projection is only 0.001 its maximum value - this seems
    // recover 99.9% of the total forces when integrated).
    scalar bladeEpsilonMax = -1.0E6;
    for (int j = 0; j < 3; j++)
    {
        if(bladeEpsilon[i][j] > bladeEpsilonMax)
        {
            bladeEpsilonMax = bladeEpsilon[i][j];
        }
    }

    scalar bladeChordMax = -1.0E6;
    forAll (BladeChord[m],j)
    {
        if(BladeChord[m][j] > bladeChordMax)
        {
            bladeChordMax = BladeChord[m][j];
        }
    }

    scalar bladeUserDefMax = -1.0E6;
    forAll (BladeUserDef[m],j)
    {
        if(BladeUserDef[m][j] > bladeUserDefMax)
        {
            bladeUserDefMax = BladeUserDef[m][j];
        }
    }

    if ((bladeForceProjectionType[i] == "uniformGaussian") ||
        (bladeForceProjectionType[i] == "generalizedGaussian") ||
        (bladeForceProjectionType[i] == "generalizedGaussian2D"))
    {
        bladeProjectionRadius[i] = bladeEpsilonMax * Foam::sqrt(Foam::log(1.0/0.001));
    }
    else if (bladeForceProjectionType[i] == "variableUniformGaussianUserDef")
    {
        bladeProjectionRadius[i] = bladeUserDefMax * bladeEpsilonMax * Foam::sqrt(Foam::log(1.0/0.001));
    }
    else if ((bladeForceProjectionType[i] == "variableUniformGaussianChord") ||
             (bladeForceProjectionType[i] == "chordThicknessGaussian") ||
             (bladeForceProjectionType[i] == "chordThicknessGaussian2D"))
    {
        bladeProjectionRadius[i] = bladeChordMax * bladeEpsilonMax * Foam::sqrt(Foam::log(1.0/0.001));
    }
        
    // Defines the search cell set a all cells within a sphere that occupies
    // the volume that the rotor lies in for any yaw angle.  This is a fairly
    // large volume, so possibly inefficient for fine meshes.  See the "disk"
    // option below.
    DynamicList<label> influenceCellsI;
    if (bladeSearchCellMethod[i] == "sphere")
    {
        scalar sphereRadius = 0.0;
        forAll(PreCone[m],j)
        {
            scalar sphereRadiusI = Foam::sqrt(Foam::sqr((OverHang[m] + UndSling[m]) + TipRad[m]*Foam::sin(PreCone[m][j])) + Foam::sqr(TipRad[m]*Foam::cos(PreCone[m][j])));
            if(sphereRadiusI > sphereRadius)
            {
                sphereRadius = sphereRadiusI;
            }
        } 
        sphereRadius += bladeProjectionRadius[i];

        // Find the cells within the region of influence.
        forAll(U_.mesh().cells(),cellI)
        {
            if (mag(U_.mesh().C()[cellI] - towerShaftIntersect[i]) <= sphereRadius)
            {
                influenceCellsI.append(cellI);
                searchCells[cellI] = 1;
            }
        }
    }
    // Defines the search cell set as a disk with thickness that surrounds the 
    // rotor revolution plane.  It isn't really a disk because it accounts for
    // rotor precone, so it is more of a disk that is coned.
    else if (bladeSearchCellMethod[i] == "disk")
    {
        forAll(U_.mesh().cells(),cellI)
        {
            vector xP = vector::zero;
            vector yP = vector::zero;
            vector zP = vector::zero;
            vector vP = vector::zero;
            vector v = vector::zero;
            scalar r = 0.0;
            scalar xBlade = 0.0;
            v = U_.mesh().C()[cellI] - rotorApex[i];
            zP.z() = 1.0;
            xP = uvShaft[i];
            xP /= mag(xP);
            yP = zP ^ xP;
            yP /= mag(yP);
            zP = xP ^ yP;
            zP /= mag(zP);
            vP = transformVectorCartToLocal(v,xP,yP,zP);
            r = Foam::sqrt(Foam::sqr(vP.y()) + Foam::sqr(vP.z()));
            xBlade = r * Foam::sin(PreCone[m][0]);
            if (((vP.x() >= xBlade - bladeProjectionRadius[i]) && (vP.x() <= xBlade + bladeProjectionRadius[i])) &&
                (r <= TipRad[m]*Foam::cos(PreCone[m][0]) + bladeProjectionRadius[i]))
            {
                influenceCellsI.append(cellI);
                searchCells[cellI] = 1;
            }
        }     
    }
    bladeInfluenceCells[i].clear();
    bladeInfluenceCells[i] = influenceCellsI;
    influenceCellsI.clear();
}


void horizontalAxisWindTurbinesALMAdvanced::findNacelleSearchCells(int turbineNumber)
{
    // Define the cells that can possibly be influenced by the force
    // exerted each turbine by the nacelle.  In otherwords, define a set 
    // of cell IDs around each turbine that will be saved into memory 
    // so that the entire domain need not be passed through when  
    // applying the force field.  (The i-index is at the turbine array  
    // level for each turbine, the m-index is for each type of turbine--
    // if all turbines are the same, m is always 0, and the j-index is 
    // at the individual blade level.)
    int i = turbineNumber;

    int m = turbineTypeID[i];

    // First compute the radius of the force projection (to the radius
    // where the projection is only 0.001 its maximum value - this seems
    // recover 99.9% of the total forces when integrated).
    scalar nacelleEpsilonMax = -1.0E6;
    for (int j = 0; j < 3; j++)
    {
        if(nacelleEpsilon[i][j] > nacelleEpsilonMax)
        {
            nacelleEpsilonMax = nacelleEpsilon[i][j];
        }
    }
    nacelleProjectionRadius[i] = nacelleEpsilonMax * Foam::sqrt(Foam::log(1.0/0.001)) + NacelleEquivalentRadius[m];


    // Find the cells within the region of influence.  These are cells within a
    // cylinder with hemispherical end caps around the line of nacelle points, 
    // and also in a sphere around the nacelle sample point.
    DynamicList<label> influenceCellsI;
    if (includeNacelle[i])
    {
        forAll(U_.mesh().cells(),cellI)
        {
            vector xP = vector::zero;
            vector yP = vector::zero;
            vector zP = vector::zero;
            vector vP = vector::zero;
            vector v1 = vector::zero;
            vector v2 = vector::zero;
            scalar r1 = 0.0;
            scalar r2 = 0.0;
            v1 = U_.mesh().C()[cellI] - rotorApex[i];
            v2 = U_.mesh().C()[cellI] - nacelleSamplePoint[i];
            zP.z() = 1.0;
            xP = uvShaft[i];
            xP /= mag(xP);
            yP = zP ^ xP;
            yP /= mag(yP);
            zP = xP ^ yP;
            zP /= mag(zP);
            vP = transformVectorCartToLocal(v1,xP,yP,zP);
                
            // Searching for cells around the nacelle points.
            if (vP.x() < 0.0)
            {
                r1 = Foam::sqrt(Foam::sqr(vP.x()) + Foam::sqr(vP.y()) + Foam::sqr(vP.z()));
            }
            else if (vP.x() > NacelleLength[m])
            {
                r1 = Foam::sqrt(Foam::sqr(vP.x() - NacelleLength[m]) + Foam::sqr(vP.y()) + Foam::sqr(vP.z()));
            }
            else
            {
                r1 = Foam::sqrt(Foam::sqr(vP.y()) + Foam::sqr(vP.z()));
            }

            // Searching for cells around the nacelle velocity sampling points.
            r2 = Foam::sqrt(Foam::sqr(v2.x()) + Foam::sqr(v2.y()) + Foam::sqr(v2.z()));
            if ((r1 <= nacelleProjectionRadius[i]) || (r2 <= nacelleProjectionRadius[i]))
            {
                influenceCellsI.append(cellI);
                searchCells[cellI] = 2;
            }
        }
    }
    nacelleInfluenceCells[i].clear();
    nacelleInfluenceCells[i] = influenceCellsI;
    influenceCellsI.clear();
}


void horizontalAxisWindTurbinesALMAdvanced::findTowerSearchCells(int turbineNumber)
{
    // Define the cells that can possibly be influenced by the force
    // exerted each turbine by the tower.  In otherwords, define a set 
    // of cell IDs around each turbine that will be saved into memory 
    // so that the entire domain need not be passed through when  
    // applying the force field.  (The i-index is at the turbine array  
    // level for each turbine, the m-index is for each type of turbine--
    // if all turbines are the same, m is always 0, and the j-index is 
    // at the individual blade level.)
    int i = turbineNumber;

    int m = turbineTypeID[i];

    // First compute the radius of the force projection (to the radius
    // where the projection is only 0.001 its maximum value - this seems
    // recover 99.9% of the total forces when integrated).
    scalar towerEpsilonMax = -1.0E6;
    for (int j = 0; j < 3; j++)
    {
        if (towerEpsilon[i][j] > towerEpsilonMax)
        {
            towerEpsilonMax = towerEpsilon[i][j];
        }
    }
    scalar towerChordMax = 0.0;
    forAll (TowerChord[m],j)
    {
        if (TowerChord[m][j] > towerChordMax)
        {
            towerChordMax = TowerChord[m][j];
        }
    }
    towerProjectionRadius[i] = towerEpsilonMax * Foam::sqrt(Foam::log(1.0/0.001)) + 0.5*towerChordMax;


    // Find the cells within the region of influence.  This is a cylinder around the tower
    // and around the tower sampling line.
    DynamicList<label> influenceCellsI;
    if (includeTower[i])
    {
        forAll(U_.mesh().cells(),cellI)
        {
            if (U_.mesh().C()[cellI].z() <= towerShaftIntersect[i].z())
            {
                scalar cellRadius1 = Foam::sqrt(Foam::sqr(U_.mesh().C()[cellI].x() - baseLocation[i].x()) + 
                                                Foam::sqr(U_.mesh().C()[cellI].y() - baseLocation[i].y()));
                scalar cellRadius2 = Foam::sqrt(Foam::sqr(U_.mesh().C()[cellI].x() - towerSamplePoints[i][0].x()) + 
                                                Foam::sqr(U_.mesh().C()[cellI].y() - towerSamplePoints[i][0].y()));
                if ( (cellRadius1 <= towerProjectionRadius[i]) || (cellRadius2 <= towerProjectionRadius[i]) )
                {
                    influenceCellsI.append(cellI);
                    searchCells[cellI] = 3;
                }
            }
        }
    }
    towerInfluenceCells[i].clear();
    towerInfluenceCells[i] = influenceCellsI;
    influenceCellsI.clear();
}


void horizontalAxisWindTurbinesALMAdvanced::updateTurbinesControlled()
{
    // This function determines if this processor has search cells for a 
    // particular turbine.  It creates a list of turbines that this processor
    // has search cells for, and should therefore perform searches and body
    // force projection for.

    // Clear out the blades, nacelles, and towers controlled list and recalculate.
    bladesControlled.clear();
    nacellesControlled.clear();
    towersControlled.clear();

    // If there are any blade, nacelle, or tower influence cells, than this
    // processor controls those sampling/body force distribution points.
    for(int i = 0; i < numTurbines; i++)
    {
        if (bladeInfluenceCells[i].size() > 0)
        {
            bladesControlled.append(i);
        }
        if (nacelleInfluenceCells[i].size() > 0)
        {
            nacellesControlled.append(i);
        }
        if (towerInfluenceCells[i].size() > 0)
        {
            towersControlled.append(i);
        }
    }
}



void horizontalAxisWindTurbinesALMAdvanced::rotateBlades()
{  
    // Perform rotation turbine by turbine.
    forAll(uvShaft, i)
    {   
        // Check the rotation direction first and set the local delta azimuth
        // variable accordingly.
        scalar deltaAzimuthI = 0.0;
        if (rotationDir[i] == "cw")
        {
            deltaAzimuthI =  deltaAzimuth[i];
        }
        if (rotationDir[i] == "ccw")
        {
            deltaAzimuthI = -deltaAzimuth[i];
        }

        // Rotate turbine blades, blade by blade, point by point.
        forAll(bladePoints[i], j)
        {
            forAll(bladePoints[i][j], k)
            {
                bladePoints[i][j][k] = rotateVector(bladePoints[i][j][k], rotorApex[i], uvShaft[i], deltaAzimuthI);
                bladeSamplePoints[i][j][k] = rotateVector(bladeSamplePoints[i][j][k], rotorApex[i], uvShaft[i], deltaAzimuthI);
            }
        }   

        // Calculate the new azimuth angle and make sure it isn't
        // bigger than 2*pi.
        if (pastFirstTimeStep)
        {
            rotorAzimuth[i] = rotorAzimuth[i] + deltaAzimuth[i];
            if (rotorAzimuth[i] >= 2.0 * Foam::constant::mathematical::pi)
            {
                rotorAzimuth[i] -= 2.0 * Foam::constant::mathematical::pi;
            }
        }
    }
}
        

void horizontalAxisWindTurbinesALMAdvanced::yawNacelle()
{
    // Perform rotation turbine by turbine.
    forAll(uvTower, i)
    {
        // Rotate the rotor apex first.
        rotorApex[i] = rotateVector(rotorApex[i], towerShaftIntersect[i], uvTower[i], deltaNacYaw[i]);

        // Recompute the shaft unit vector since the shaft has rotated.
        uvShaft[i] = rotorApex[i] - towerShaftIntersect[i];
        uvShaft[i] = (uvShaft[i]/mag(uvShaft[i])) * uvShaftDir[i];
    
        // Rotate turbine blade points and velocity sampling points.
        forAll(bladePoints[i], j)
        {
            forAll(bladePoints[i][j], k)
            {
                bladePoints[i][j][k] = rotateVector(bladePoints[i][j][k], towerShaftIntersect[i], uvTower[i], deltaNacYaw[i]);
                bladeSamplePoints[i][j][k] = rotateVector(bladeSamplePoints[i][j][k], towerShaftIntersect[i], uvTower[i], deltaNacYaw[i]);
            }
        }   

        // Rotate the nacelle points and the nacelle velocity sampling points.
        nacelleSamplePoint[i] = rotateVector(nacelleSamplePoint[i], towerShaftIntersect[i], uvTower[i], deltaNacYaw[i]);
        forAll(nacellePoints[i], j)
        {
            nacellePoints[i][j] = rotateVector(nacellePoints[i][j], towerShaftIntersect[i], uvTower[i], deltaNacYaw[i]);
        }

        // Rotate the tower velocity sampling points.
        forAll(towerSamplePoints[i], j)
        {
            towerSamplePoints[i][j] = rotateVector(towerSamplePoints[i][j], towerShaftIntersect[i], uvTower[i], deltaNacYaw[i]);
        }



        // Compute the new yaw angle and make sure it isn't
        // bigger than 2*pi.
        if (pastFirstTimeStep)
        {
            nacYaw[i] = nacYaw[i] + deltaNacYaw[i];
            if (nacYaw[i] >= 2.0 * Foam::constant::mathematical::pi)
            {
                nacYaw[i] -= 2.0 * Foam::constant::mathematical::pi;
            }
            if (nacYaw[i] < 0.0)
            {
                nacYaw[i] += 2.0 * Foam::constant::mathematical::pi;
            }
        }
    }
}


void horizontalAxisWindTurbinesALMAdvanced::computeRotSpeed()
{
    // Proceed turbine by turbine.
    forAll(rotorSpeed, i)
    {
        // Get the turbine type index.
        int j = turbineTypeID[i];

        // If the generator torque and blade pitch controllers are both set to "none", then
        // the rotor speed will remain fixed at its initial speed.
        if ((GenTorqueControllerType[j] == "none") && (BladePitchControllerType[j] == "none"))
        {
            // Do nothing.
        }

        // Otherwise numerically solve the dynamics of the rotor to compute the new rotor speed
        // based on the summation of aerodynamic and generator torque on the rotor.
        else
        {
            rotorSpeed[i] += (dt/DriveTrainIner[j])*(GBEfficiency[j]*rotorTorque[i]*fluidDensity[i] - GBRatio[j]*generatorTorque[i]);
        }
      //Info << "rotor sped = " << rotorSpeed[0] << endl;
      //Info << "drive train inertia = " << DriveTrainIner[j] << endl;
      //Info << "gear box efficiency = " << GBEfficiency[j] << endl;
      //Info << "generator torque = " << generatorTorque[0] << endl;
      //Info << "rotor torque = " << rotorTorque[0] << endl;
      //Info << "fluid density = " << fluidDensity[0] << endl;
      //Info << "gear box ratio = " << GBRatio[j] << endl;


        // Limit the rotor speed to be positive and such that the generator does not turn
        // faster than rated.
        if (RotSpeedLimiter[j])
        {
            #include "limiters/rotSpeedLimiter.H"
        }
 
        // Compute the change in blade azimuth angle based on the time step and current rotor speed.
        deltaAzimuth[i] = rotorSpeed[i] * dt;

    }
}


void horizontalAxisWindTurbinesALMAdvanced::filterRotSpeed()
{
    // Proceed turbine by turbine.
    forAll(rotorSpeedF, i)
    {
        // Get the turbine type index.
        int j = turbineTypeID[i];

        // Compute the filtering coefficient based on the corner frequency and time step.
        scalar alpha = exp(-dt * SpeedFilterCornerFrequency[j]);

        // Apply a simple recursive, single-pole, low-pass filter.
        rotorSpeedF[i] = (1.0 - alpha)*rotorSpeed[i] + alpha*rotorSpeedF[i];
    }
}


void horizontalAxisWindTurbinesALMAdvanced::controlGenTorque()
{
    // Proceed turbine by turbine.
    forAll(generatorTorque, i)
    {
        // Get the turbine type index.
        int j = turbineTypeID[i];

        // Get the current filtered generator speed.
        scalar genSpeedF = (rotorSpeedF[i]/rpmRadSec)*GBRatio[j];

        // Initialize the commanded generator torque variable;
        scalar generatorTorqueCommanded = generatorTorque[i];



        // Apply a controller to update the rotor speed.
        if (GenTorqueControllerType[j] == "none")
        {
            #include "controllers/genTorqueControllers/none.H"
        }

        else if (GenTorqueControllerType[j] == "fiveRegion")
        {
            #include "controllers/genTorqueControllers/fiveRegion.H"
        }

        else if (GenTorqueControllerType[j] == "speedTorqueTable")
        {
            #include "controllers/genTorqueControllers/speedTorqueTable.H"
        }

        // Limit the change in generator torque.
        if (GenTorqueRateLimiter[j])
        {
            #include "limiters/genTorqueRateLimiter.H"
        }

        // Update the torque array.
        generatorTorque[i] = generatorTorqueCommanded;
    }
}
        

void horizontalAxisWindTurbinesALMAdvanced::controlNacYaw()
{
    // Proceed turbine by turbine.
    forAll(deltaNacYaw, i)
    {
        // Get the turbine type index.
        int j = turbineTypeID[i];

        // Initialize the commanded nacelle yaw variable;
        scalar nacYawCommanded = nacYaw[i];

        
        // Apply a controller to update the nacelle yaw position.
        if (NacYawControllerType[j] == "none")
        {
            // Do nothing.
        }

        else if (NacYawControllerType[j] == "simple")
        {
            // Placeholder for when this is implemented.
        }
        
        else if (NacYawControllerType[j] == "timeYawTable")
        {
            #include "controllers/nacYawControllers/timeYawTable.H"
        }


      //Info << "nacYaw = " << nacYaw << endl;
        if (((nacYawCommanded - nacYaw[i]) / degRad) <= 180.0)
        {
            deltaNacYaw[i] = nacYawCommanded - nacYaw[i];
        }
        else
        {
            deltaNacYaw[i] = nacYaw[i] - ((360.0*degRad) - nacYawCommanded);
        }
      //Info << "deltaNacYaw = " << deltaNacYaw / degRad << endl;
   
         // Limit the change in nacelle yaw angle.
        if (NacYawRateLimiter[j])
        {
            #include "limiters/nacYawRateLimiter.H"
        }

      //Info << "deltaNacYaw = " << deltaNacYaw / degRad << endl;
    }
}
        

void horizontalAxisWindTurbinesALMAdvanced::controlBladePitch()
{
    // Proceed turbine by turbine.
    forAll(bladePitch, i)
    {

        // Get the turbine type index.
        int j = turbineTypeID[i];
        
        // Initialize the gain scheduling variable.
        scalar GK = 0.0;

        // Initialize the commanded pitch variable.
        scalar bladePitchCommanded = bladePitch[i]*degRad;


        // Apply a controller to update the blade pitch position.
        if (BladePitchControllerType[j] == "none")
        {
            #include "controllers/bladePitchControllers/none.H"
        }

        else if (BladePitchControllerType[j] == "PID")
        {
            #include "controllers/bladePitchControllers/PID.H"
        }

        // Apply pitch rate limiter.
        if (BladePitchRateLimiter[j])
        {
            #include "limiters/bladePitchRateLimiter.H"
        }

        // Update the pitch array.
        bladePitch[i] = bladePitchCommanded/degRad;
    }
}



void horizontalAxisWindTurbinesALMAdvanced::findBladePointControlProcNo()
{
    // Create a local and global list of minimum distance cells to velocity sampling 
    // points of turbines that this processor controls.  Initialize the values to huge.
    List<scalar> minDisLocalBlade(totBladePoints,1.0E30);
    List<scalar> minDisGlobalBlade(totBladePoints,1.0E30);


    forAll(bladesControlled, p)
    {
        int i = bladesControlled[p];
        int iterBlade = 0;
        if(i > 0)
        {
            for(int n = 0; n < i; n++)
            {
                iterBlade += numBladePoints[n] * NumBl[turbineTypeID[n]];
            }
        }
        
        // Blade sampling points.
        if (bladeActuatorPointInterpType[i] != "integral")
        {
            forAll(bladeSamplePoints[i], j)
            {
                forAll(bladeSamplePoints[i][j], k)
                {
                    // Find the cell that the sampling point lies within and the distance
                    // from the sampling point to that cell center.
                    label cellID = bladeInfluenceCells[i][0];
                    scalar minDis = mag(mesh_.C()[cellID] - (bladeSamplePoints[i][j][k] + bladePointsPerturbVector[i][j][k]));

                    forAll(bladeInfluenceCells[i], m)
                    {
                        scalar dis = mag(mesh_.C()[bladeInfluenceCells[i][m]] - (bladeSamplePoints[i][j][k] + bladePointsPerturbVector[i][j][k]));
                        if(dis <= minDis)
                        {
                            cellID = bladeInfluenceCells[i][m];
                        }
                        minDis = mag(mesh_.C()[cellID] - (bladeSamplePoints[i][j][k] + bladePointsPerturbVector[i][j][k]));
                    }
                    minDisLocalBlade[iterBlade] = minDis;
                    minDisGlobalBlade[iterBlade] = minDis;
                    bladeMinDisCellID[i][j][k] = cellID;
                    iterBlade++;
                }
            }
        }
    }

    // Parallel gather/scatter the global minimum distance list and reduce it by keeping 
    // only the minimum values.
    Pstream::gather(minDisGlobalBlade,minOp<List<scalar> >());
    Pstream::scatter(minDisGlobalBlade);

     // Compare the global to local lists.  Where the lists agree, this processor controls
    // the actuator line point.
    forAll(bladesControlled, p)
    {
        int i = bladesControlled[p];
        int iterBlade = 0;
        if(i > 0)
        {
            for(int n = 0; n < i; n++)
            {
                iterBlade += numBladePoints[n] * NumBl[turbineTypeID[n]];
            }
        }
        
        if (bladeActuatorPointInterpType[i] != "integral")
        {
            forAll(bladeSamplePoints[i], j)
            {
                forAll(bladeSamplePoints[i][j], k)
                {
                    if(minDisGlobalBlade[iterBlade] != minDisLocalBlade[iterBlade])
                    {
                        bladeMinDisCellID[i][j][k] = -1;
                    }
                    iterBlade++;
                }
            }
        }
    }
}   



void horizontalAxisWindTurbinesALMAdvanced::findNacellePointControlProcNo()
{
    // Create a local and global list of minimum distance cells to velocity sampling 
    // points of turbines that this processor controls.  Initialize the values to huge.
    List<scalar> minDisLocalNacelle(numTurbines,1.0E30);
    List<scalar> minDisGlobalNacelle(numTurbines,1.0E30);


    forAll(nacellesControlled, p)
    {
        int i = nacellesControlled[p];
        int iterNacelle = 0;
        if(i > 0)
        {
            for(int n = 0; n < i; n++)
            {
                iterNacelle += 1;
            }
        }
        

        // Nacelle sampling point.
        if(includeNacelleSomeTrue)
        {
            label cellID = nacelleInfluenceCells[i][0];
            scalar minDis = mag(mesh_.C()[cellID] - (nacelleSamplePoint[i] + nacellePointPerturbVector[i]));
           
            forAll(nacelleInfluenceCells[i], m)
            {
                scalar dis = mag(mesh_.C()[nacelleInfluenceCells[i][m]] - (nacelleSamplePoint[i] + nacellePointPerturbVector[i]));
                if(dis <= minDis)
                {
                    cellID = nacelleInfluenceCells[i][m];
                }
                minDis = mag(mesh_.C()[cellID] - (nacelleSamplePoint[i] + nacellePointPerturbVector[i]));
            }
            minDisLocalNacelle[iterNacelle] = minDis;
            minDisGlobalNacelle[iterNacelle] = minDis;
            nacelleMinDisCellID[i] = cellID;
            iterNacelle++;
        }

    }

    // Parallel gather/scatter the global minimum distance list and reduce it by keeping 
    // only the minimum values.
    if(includeNacelleSomeTrue)
    {
        Pstream::gather(minDisGlobalNacelle,minOp<List<scalar> >());
        Pstream::scatter(minDisGlobalNacelle);
    }

    // Compare the global to local lists.  Where the lists agree, this processor controls
    // the actuator line point.
    forAll(nacellesControlled, p)
    {
        int i = nacellesControlled[p];
        int iterNacelle = 0;
        if(i > 0)
        {
            for(int n = 0; n < i; n++)
            {
                iterNacelle += 1;
            }
        }

        if(minDisGlobalNacelle[iterNacelle] != minDisLocalNacelle[iterNacelle])
        {
            nacelleMinDisCellID[i] = -1;
        }
    }
} 


void horizontalAxisWindTurbinesALMAdvanced::findTowerPointControlProcNo()
{
    // Create a local and global list of minimum distance cells to velocity sampling 
    // points of turbines that this processor controls.  Initialize the values to huge.
    List<scalar> minDisLocalTower(totTowerPoints,1.0E30);
    List<scalar> minDisGlobalTower(totTowerPoints,1.0E30);


    forAll(towersControlled, p)
    {
        int i = towersControlled[p];
        int iterTower = 0;
        if(i > 0)
        {
            for(int n = 0; n < i; n++)
            {
                iterTower += numTowerPoints[n];
            }
        }
        

        // Tower sampling points.
        if(includeTowerSomeTrue)
        {
            forAll(towerSamplePoints[i],j)
            {
                label cellID = towerInfluenceCells[i][0];
                scalar minDis = mag(mesh_.C()[cellID] - (towerSamplePoints[i][j] + towerPointsPerturbVector[i][j]));

                forAll(towerInfluenceCells[i], m)
                {
                    scalar dis = mag(mesh_.C()[towerInfluenceCells[i][m]] - (towerSamplePoints[i][j] + towerPointsPerturbVector[i][j]));
                    if(dis <= minDis)
                    {
                        cellID = towerInfluenceCells[i][m];
                    }
                    minDis = mag(mesh_.C()[cellID] - (towerSamplePoints[i][j] + towerPointsPerturbVector[i][j]));
                }
                minDisLocalTower[iterTower] = minDis;
                minDisGlobalTower[iterTower] = minDis;
                towerMinDisCellID[i][j] = cellID;
                iterTower++;
            }
        }
    }

    // Parallel gather/scatter the global minimum distance list and reduce it by keeping 
    // only the minimum values.
    if(includeTowerSomeTrue)
    {
        Pstream::gather(minDisGlobalTower,minOp<List<scalar> >());
        Pstream::scatter(minDisGlobalTower);
    }

    // Compare the global to local lists.  Where the lists agree, this processor controls
    // the actuator line point.
    forAll(towersControlled, p)
    {
        int i = towersControlled[p];
        int iterTower = 0;
        if(i > 0)
        {
            for(int n = 0; n < i; n++)
            {
                iterTower += numTowerPoints[n];
            }
        }
        
        forAll(towerSamplePoints[i], j)
        {
            if(minDisGlobalTower[iterTower] != minDisLocalTower[iterTower])
            {
                towerMinDisCellID[i][j] = -1;
            }
            iterTower++;
        }
    }
} 


void horizontalAxisWindTurbinesALMAdvanced::updateRadius(int turbineNumber)
{
    // This computes the radius normal to turbine i's main shaft axis of
    // each grid cell within the influence cell set.  This can then be used
    // later on to compute the relative velocity from the blade reference
    // frame
    int i = turbineNumber;

    forAll(bladeInfluenceCells[i], m)
    {
        label cellI = bladeInfluenceCells[i][m];
        vector xP = vector::zero;
        vector yP = vector::zero;
        vector zP = vector::zero;
        vector vP = vector::zero;
        vector v = vector::zero;

        v = U_.mesh().C()[cellI] - rotorApex[i];
        zP.z() = 1.0;
        xP = uvShaft[i];
        xP /= mag(xP);
        yP = zP ^ xP;
        yP /= mag(yP);
        zP = xP ^ yP;
        zP /= mag(zP);
        vP = transformVectorCartToLocal(v,xP,yP,zP);
        rFromShaft[cellI] = Foam::sqrt(Foam::sqr(vP.y()) + Foam::sqr(vP.z()));
    }
}


void horizontalAxisWindTurbinesALMAdvanced::computeBladePointWindVectors()
{
    // Create a list of wind velocity in x, y, z coordinates for each blade sample point.
    List<vector> bladeWindVectorsLocal(totBladePoints,vector::zero);
    
    // If linear interpolation of the velocity from the CFD mesh to the actuator
    // points is used, we need velocity gradient information.
    gradU = fvc::grad(U_);

    forAll(bladesControlled, p)
    {
        int i = bladesControlled[p];
        int iterBlade = 0;
        if(i > 0)
        {
            for(int n = 0; n < i; n++)
            {
                iterBlade += numBladePoints[n] * NumBl[turbineTypeID[n]];
            }
        }
        
        forAll(bladePoints[i], j)
        {
            forAll(bladePoints[i][j], k)
            {
                vector velocity(vector::zero);
                vector point = bladeSamplePoints[i][j][k];
                label cellID = bladeMinDisCellID[i][j][k];

                // Cell-center or linear interpolated velocity require velocity and
                // velocity gradient information only at one cell, defined in the 
                // findControlProcNo() function above.  Therefore, information feeding
                // a particular blade point does not go across processor boundaries.
                // Integral sampling, though, does go across processor boundaries
                // and does not use information from findControlProcNo().
                if(((bladeActuatorPointInterpType[i] == "cellCenter") || (bladeActuatorPointInterpType[i] == "linear")) && 
                     cellID != -1)
                {
                    // If the velocity interpolation is "cellCenter", then just use 
                    // the velocity at the center of the cell within which this
                    // actuator point lies.  
                    if(bladeActuatorPointInterpType[i] == "cellCenter")
                    {
                        #include "velocityInterpolation/cellCenter.H"
                    }

                    // But if linear interpolation is used, use cell center plus a 
                    // correction based on the local velocity gradient.
                    else if(bladeActuatorPointInterpType[i] == "linear")
                    {
                        #include "velocityInterpolation/linear.H"
                    }
                }

                else if (bladeActuatorPointInterpType[i] == "integral")
                {
                    // Use a projection function weighted integrated velocity.
                    #include "velocityInterpolation/integral.H"
                }

                // Set the local wind vector list to the velocity sampled above
                // and move on to the next blade point.
                bladeWindVectorsLocal[iterBlade] = velocity;
                iterBlade++;
            }
        }
    }

    // Perform a parallel gather of this local list to the master processor and
    // and then parallel scatter the list back out to all the processors.
    Pstream::gather(bladeWindVectorsLocal,sumOp<List<vector> >());
    Pstream::scatter(bladeWindVectorsLocal);


    // Put the gathered/scattered wind vectors into the windVector variable.
    // Proceed turbine by turbine.
    int iterBlade = 0;
    forAll(bladeWindVectors, i)
    {
        // Proceed blade by blade.
        forAll(bladeWindVectors[i], j)
        { 
            // Proceed point by point.
            forAll(bladeWindVectors[i][j], k)
            {
                // Zero the wind vector and put in the correct velocity.
                bladeWindVectors[i][j][k] = vector::zero;
                bladeWindVectors[i][j][k] = bladeWindVectorsLocal[iterBlade];

                // ******* OVERRIDE FOR TESTING 2D CASE
                //vector v = vector::zero;
                //v.x() = 60.0;
                //bladeWindVectors[i][j][k] = v;
                // ************************************

                iterBlade++;
            }
        }
    }
}



void horizontalAxisWindTurbinesALMAdvanced::computeNacellePointWindVectors()
{
    // Create a list of wind velocity in x, y, z coordinates for each nacelle sample point.
    List<vector> nacelleWindVectorLocal(totNacellePoints,vector::zero);
    
    // If linear interpolation of the velocity from the CFD mesh to the actuator
    // points is used, we need velocity gradient information.
    gradU = fvc::grad(U_);

    forAll(nacellesControlled, p)
    {
        int i = nacellesControlled[p];
        int iterNacelle = 0;
        if(i > 0)
        {
            for(int n = 0; n < i; n++)
            {
                iterNacelle += 1;
            }
        }
                

        vector velocity(vector::zero);
        label cellID = nacelleMinDisCellID[i];
        vector point = nacelleSamplePoint[i];

        if(cellID != -1)
        {

            // If the velocity interpolation is "cellCenter", then just use 
            // the velocity at the center of the cell within which this
            // actuator point lies.  
            if (nacelleActuatorPointInterpType[i] == "cellCenter")
            {
                #include "velocityInterpolation/cellCenter.H"
            }

            // But if linear interpolation is used, use cell center plus a 
            // correction based on the local velocity gradient.
            else if (nacelleActuatorPointInterpType[i] == "linear")
            {
               #include "velocityInterpolation/linear.H"
            }

            nacelleWindVectorLocal[iterNacelle] = velocity;           
        }
    }

    // Perform a parallel gather of this local list to the master processor and
    // and then parallel scatter the list back out to all the processors.
    if(includeNacelleSomeTrue)
    {
        Pstream::gather(nacelleWindVectorLocal,sumOp<List<vector> >());
        Pstream::scatter(nacelleWindVectorLocal);
    }


    // Put the gathered/scattered wind vectors into the windVector variable.
    // Proceed turbine by turbine.
    int iterNacelle = 0;
    forAll(nacelleWindVector, i)
    {
        nacelleWindVector[i] = vector::zero;
        nacelleWindVector[i] = nacelleWindVectorLocal[iterNacelle];

        iterNacelle++;
    }
}



void horizontalAxisWindTurbinesALMAdvanced::computeTowerPointWindVectors()
{
    // Create a list of wind velocity in x, y, z coordinates for each tower sample point.
    List<vector> towerWindVectorsLocal(totTowerPoints,vector::zero);
    
    // If linear interpolation of the velocity from the CFD mesh to the actuator
    // points is used, we need velocity gradient information.
    gradU = fvc::grad(U_);

    forAll(towersControlled, p)
    {
        int i = towersControlled[p];
        int iterTower = 0;
        if(i > 0)
        {
            for(int n = 0; n < i; n++)
            {
                iterTower += numTowerPoints[n];
            }
        }  

        forAll(towerPoints[i], j)
        {
            vector velocity(vector::zero);
            label cellID = towerMinDisCellID[i][j];
            vector point = towerSamplePoints[i][j];

            if(cellID != -1)
            {

                // If the velocity interpolation is "cellCenter", then just use 
                // the velocity at the center of the cell within which this
                // actuator point lies.  
                if (towerActuatorPointInterpType[i] == "cellCenter")
                {
                    #include "velocityInterpolation/cellCenter.H"
                }

                // But if linear interpolation is used, use cell center plus a 
                // correction based on the local velocity gradient.
                else if (towerActuatorPointInterpType[i] == "linear")
                {
                    #include "velocityInterpolation/linear.H"
                }

                towerWindVectorsLocal[iterTower] = velocity;
            }
            iterTower++;
        }
    }

    // Perform a parallel gather of this local list to the master processor and
    // and then parallel scatter the list back out to all the processors.
    if(includeTowerSomeTrue)
    {
        Pstream::gather(towerWindVectorsLocal,sumOp<List<vector> >());
        Pstream::scatter(towerWindVectorsLocal);
    }


    // Put the gathered/scattered wind vectors into the windVector variable.
    // Proceed turbine by turbine.
    int iterTower = 0;
    forAll(towerWindVectors, i)
    {
        forAll(towerWindVectors[i], j)
        {
            towerWindVectors[i][j] = vector::zero;
            towerWindVectors[i][j] = towerWindVectorsLocal[iterTower];

            iterTower++;
        }
    }
}


void horizontalAxisWindTurbinesALMAdvanced::computeBladeAlignedVectors()
{
    forAll(bladePoints,i)
    {
        forAll(bladePoints[i],j)
        {
            // If clockwise rotating, this vector points along the blade toward the tip.
            // If counter-clockwise rotating, this vector points along the blade toward the root.
            if (rotationDir[i] == "cw")
            {
                bladeAlignedVectors[i][j][2] =   bladePoints[i][j][0] - rotorApex[i];
                bladeAlignedVectors[i][j][2] =   bladeAlignedVectors[i][j][2]/mag(bladeAlignedVectors[i][j][2]);
            }
            else if (rotationDir[i] == "ccw")
            {
                bladeAlignedVectors[i][j][2] = -(bladePoints[i][j][0] - rotorApex[i]);
                bladeAlignedVectors[i][j][2] =   bladeAlignedVectors[i][j][2]/mag(bladeAlignedVectors[i][j][2]);
            }

            // This vector points in the tangential direction opposite the turbines rotation type.  It is
            // set up this way because it will point in the direction of oncoming flow that the blade sees
            // due to rotation.
            bladeAlignedVectors[i][j][1] = bladeAlignedVectors[i][j][2]^uvShaft[i];
            bladeAlignedVectors[i][j][1] = bladeAlignedVectors[i][j][1]/mag(bladeAlignedVectors[i][j][1]);

            // This vector points normal to the other two and toward downwind (not exactly downwind if
            // the blade is coned).  It points in the direction of the oncoming flow due to wind that the
            // blade sees.
            bladeAlignedVectors[i][j][0] = bladeAlignedVectors[i][j][1]^bladeAlignedVectors[i][j][2];
            bladeAlignedVectors[i][j][0] = bladeAlignedVectors[i][j][0]/mag(bladeAlignedVectors[i][j][0]);

            //Info << "bladeAlignedVector = " <<  bladeAlignedVectors << endl;
        }
    }
}


void horizontalAxisWindTurbinesALMAdvanced::computeBladePointForce()
{
    // Take the x,y,z wind vectors and project them into the blade coordinate system.
    // Proceed turbine by turbine.
    forAll(bladeWindVectors, i)
    {
        int n = turbineTypeID[i];

        // Proceed blade by blade.
        forAll(bladeWindVectors[i], j)
        {
            forAll(bladeWindVectors[i][j], k)
            {
                vector bladeWindVectorsInt = bladeWindVectors[i][j][k];

                // Zero the wind vector.
                bladeWindVectors[i][j][k] = vector::zero;

                // Now put the velocity in that cell into blade-oriented coordinates and add on the
                // velocity due to blade rotation.
                bladeWindVectors[i][j][k].x() = (bladeAlignedVectors[i][j][0] & bladeWindVectorsInt);
                bladeWindVectors[i][j][k].y() = (bladeAlignedVectors[i][j][1] & bladeWindVectorsInt) + (rotorSpeed[i] * bladePointRadius[i][j][k] * cos(PreCone[n][j]));
                bladeWindVectors[i][j][k].z() = (bladeAlignedVectors[i][j][2] & bladeWindVectorsInt);
            }
        }
    }


    // Compute the blade forces at each actuator point.
    forAll(bladeWindVectors, i)
    {
        int m = turbineTypeID[i];

        // Set the total rotor forces/moments of the turbine to zero.  They will be summed on a blade-element-
        // wise basis.
        rotorTorque[i] = 0.0;
        rotorAxialForce[i] = 0.0;
        rotorHorizontalForce[i] = 0.0;
        rotorVerticalForce[i] = 0.0;

        // Get necessary axes.
        vector axialVector = uvShaft[i];
        axialVector.z() = 0.0;
        axialVector = axialVector / mag(axialVector);
        vector verticalVector = vector::zero;
        verticalVector.z() = 1.0;
        vector horizontalVector = -(axialVector ^ verticalVector);
        horizontalVector = horizontalVector / mag(horizontalVector);

        // Proceed blade by blade.
        forAll(bladeWindVectors[i], j)
        {

            // Proceed point by point.
            forAll(bladeWindVectors[i][j], k)
            {
                // Find the local velocity magnitude compose of only the axial and tangential flow (do
                // not include the radial (along blade span) flow).
                bladePointVmag[i][j][k] = Foam::pow((Foam::pow(bladeWindVectors[i][j][k].x(),2) + Foam::pow(bladeWindVectors[i][j][k].y(),2)),0.5);

                // Get the angle of the wind with respect to rotor plane tangent direction.
                scalar windAng = Foam::atan2(bladeWindVectors[i][j][k].x(),bladeWindVectors[i][j][k].y())/degRad; 

                // Angle of attack is local angle of wind with respect to rotor plane tangent minus local twist.
                bladePointAlpha[i][j][k] = windAng - bladePointTwist[i][j][k] - bladePitch[i];

                //Info << j << tab << k << tab << bladePointAlpha[i][j][k] << endl;

                // Use airfoil look-up tables to get coefficient of bladePointLift and drag.
                bladePointCl[i][j][k] = interpolate(bladePointAlpha[i][j][k], airfoilAlpha[bladePointAirfoil[i][j][k]], airfoilCl[bladePointAirfoil[i][j][k]]);
                bladePointCd[i][j][k] = interpolate(bladePointAlpha[i][j][k], airfoilAlpha[bladePointAirfoil[i][j][k]], airfoilCd[bladePointAirfoil[i][j][k]]);

                // Correct the streamwise velocity to account for drag using the Martinez correction.
                if (velocityDragCorrType[i] == "Martinez")
                {
                    scalar cd = bladePointCd[i][j][k];
                    scalar c = bladePointChord[i][j][k];
                    scalar t = bladePointThickness[i][j][k];
                    scalar ud = bladePointUserDef[i][j][k];

                    scalar epsThickness = bladeEpsilon[i][0];

                    if (bladeForceProjectionType[i] == "chordThicknessGaussian")
                    {
                        epsThickness = bladeEpsilon[i][1] * t * c;
                    }
                    else if (bladeForceProjectionType[i] == "uniformGaussian")
                    {
                        epsThickness = bladeEpsilon[i][0];
                    }
                    else if (bladeForceProjectionType[i] == "variableUniformGaussianChord")
                    {
                        epsThickness = max(min((bladeEpsilon[i][0] * c), bladeEpsilon[i][1]), bladeEpsilon[i][2]);
                    }
                    else if (bladeForceProjectionType[i] == "variableUniformGaussianUserDef")
                    {
                        epsThickness = max(min((bladeEpsilon[i][0] * ud), bladeEpsilon[i][1]), bladeEpsilon[i][2]);
                    }

                    scalar corr = 1.0 / (1.0 - ((1.0/(4.0*Foam::sqrt(Foam::constant::mathematical::pi)))*cd*c/epsThickness));

                    //Info << "j = " << j << tab << "k = " << k << tab << "corr = " << corr << endl;

                    bladePointVmag[i][j][k] *= corr;
                }
                
                // Apply tip/root-loss correction factor.
                // Tip/root-loss correction factor of Glauert.
                scalar F = 1.0;

                if(tipRootLossCorrType[i] == "none")
                {
                    F = 1.0;
                }

                else if(tipRootLossCorrType[i] == "Glauert")
                {
                    scalar g = 1.0;

                    scalar ftip  = (TipRad[m] - bladePointRadius[i][j][k])/(bladePointRadius[i][j][k] * sin(mag(windAng)*degRad));
                    scalar Ftip  = (2.0/(Foam::constant::mathematical::pi)) * acos(min(1.0, exp(-g * (NumBl[m] / 2.0) * ftip)));

                    scalar froot = (bladePointRadius[i][j][k] - HubRad[m])/(bladePointRadius[i][j][k] * sin(mag(windAng)*degRad));
                    scalar Froot = (2.0/(Foam::constant::mathematical::pi)) * acos(min(1.0, exp(-g * (NumBl[m] / 2.0) * froot)));

                    F = Ftip * Froot;
                }

                // Using Cl, Cd, wind velocity, chord, and actuator element width, calculate the
                // lift and drag per density.
                bladePointCl[i][j][k] *= F;
                bladePointCd[i][j][k] *= F;
                bladePointLift[i][j][k] = 0.5 * bladePointCl[i][j][k] * bladePointVmag[i][j][k] * bladePointVmag[i][j][k] * bladePointChord[i][j][k] * bladeDs[i][k];
                bladePointDrag[i][j][k] = 0.5 * bladePointCd[i][j][k] * bladePointVmag[i][j][k] * bladePointVmag[i][j][k] * bladePointChord[i][j][k] * bladeDs[i][k];

                // Make the scalar bladePointLift and drag quantities vectors in the Cartesian coordinate system.
                vector dragVector = bladeAlignedVectors[i][j][0]*bladeWindVectors[i][j][k].x() + bladeAlignedVectors[i][j][1]*bladeWindVectors[i][j][k].y();
                dragVector = dragVector/mag(dragVector);

                vector liftVector = dragVector^bladeAlignedVectors[i][j][2];
                liftVector = liftVector/mag(liftVector);

                liftVector = -bladePointLift[i][j][k] * liftVector;
                dragVector = -bladePointDrag[i][j][k] * dragVector;

                // Add up bladePointLift and drag to get the resultant force/density applied to this blade element.
                bladePointForce[i][j][k] = liftVector + dragVector;
                //Info << "bladePointForce = " <<  bladePointForce[i][j][k] << endl;

                // Find the component of the blade element force/density in the different directions.
                // Axial is horizontal but aligned with the shaft.
                // Horizontal is horizontal but perpendicular to axial.
                // Vertical is vertical.  Vertical = Axial X Horizontal.
                bladePointAxialForce[i][j][k] = -bladePointForce[i][j][k] & axialVector;
                bladePointHorizontalForce[i][j][k] = -bladePointForce[i][j][k] & horizontalVector;
                bladePointVerticalForce[i][j][k] = -bladePointForce[i][j][k] & verticalVector;
                bladePointTorque[i][j][k] = (bladePointForce[i][j][k] & bladeAlignedVectors[i][j][1]) * bladePointRadius[i][j][k] * cos(PreCone[m][j]);

                // Add this blade element's contribution to the total turbine forces/moments.
                rotorAxialForce[i] += bladePointAxialForce[i][j][k];
                rotorHorizontalForce[i] += bladePointHorizontalForce[i][j][k];
                rotorVerticalForce[i] += bladePointVerticalForce[i][j][k];
                rotorTorque[i] += bladePointTorque[i][j][k];
            }
        }

        // Compute rotor power based on aerodynamic torque and rotation speed.
        rotorPower[i] = rotorTorque[i] * rotorSpeed[i];

        // Compute the generator electrical power.
        generatorPower[i] = generatorTorque[i] * (rotorSpeed[i] * GBRatio[m]) * GenEfficiency[m];
    }
}



void horizontalAxisWindTurbinesALMAdvanced::computeNacellePointForce()
{
    // The tower nacelle wind vector is in the Cartesian coordinate system so no need
    // to transform it either.

   
    // Compute the nacelle forces at each actuator point.
    forAll(nacelleWindVector, i)
    {
        if (includeNacelle[i])
        {
            int m = turbineTypeID[i];

            // Set the total tower forces of the turbine to zero.  They will be summed on a tower-element-
            // wise basis.
            nacelleAxialForce[i] = 0.0;
            nacelleHorizontalForce[i] = 0.0;
            nacelleVerticalForce[i] = 0.0;

            // Get necessary axes.
            vector axialVector = uvShaft[i];
            axialVector.z() = 0.0;
            axialVector = axialVector / mag(axialVector);
            vector verticalVector = vector::zero;
            verticalVector.z() = 1.0;
            vector horizontalVector = -(axialVector ^ verticalVector);
            horizontalVector = horizontalVector / mag(horizontalVector);

            // Now go point by point to find each points contribution to the total drag.
            forAll(nacellePoints[i], j)
            {
                // Find the local velocity magnitude.
                nacellePointVmag[i][j] = Foam::pow((Foam::pow(nacelleWindVector[i].x(),2) +
                                                    Foam::pow(nacelleWindVector[i].y(),2) +
                                                    Foam::pow(nacelleWindVector[i].z(),2)),0.5);

                // We assume the nacelle creates drag only.  Divide the drag up into portions
                // for each nacelle point based on the ratio of nacelle element length to total
                // nacelle length.
                scalar contribution = nacelleDs[i][j] / NacelleLength[m];

                if (nacelleForceProjectionType[i] == "advanced2")
                {  
                    if (pastFirstTimeStep == false)
                    {  
                        nacellePointDrag[i][j] = contribution * 0.5 * NacelleCd[m] * nacellePointVmag[i][j] * nacellePointVmag[i][j] * NacelleFrontalArea[m];
                    }
                    else
                    {
                        nacellePointDrag[i][j] *= (1 + 0.002*nacelleWindVector[i].x());
                    }

                    Info << "Nacelle --- V_x: " << nacelleWindVector[i].x() << tab << "Drag: " << nacellePointDrag[i][j] << endl;
                }
                else
                {

                    nacellePointDrag[i][j] = contribution * 0.5 * NacelleCd[m] * nacellePointVmag[i][j] * nacellePointVmag[i][j] * NacelleFrontalArea[m];
                }
                
                vector dragVector = nacelleWindVector[i];
                dragVector = dragVector/mag(dragVector);

                dragVector = -nacellePointDrag[i][j] * dragVector;

                // Add up bladePointLift and drag to get the resultant force/density applied to this blade element.
                nacellePointForce[i][j] = dragVector;

                // Find the component of the tower element force/density in the different directions.
                // Axial is horizontal but aligned with the shaft.
                // Horizontal is horizontal but perpendicular to axial.
                nacellePointAxialForce[i][j] = -nacellePointForce[i][j] & axialVector;
                nacellePointHorizontalForce[i][j] = -nacellePointForce[i][j] & horizontalVector;
                nacellePointVerticalForce[i][j] = -nacellePointForce[i][j] & verticalVector;

                // Add this blade element's contribution to the total turbine forces/moments.
                nacelleAxialForce[i] += nacellePointAxialForce[i][j];
                nacelleHorizontalForce[i] += nacellePointHorizontalForce[i][j];
                nacelleVerticalForce[i] += nacellePointVerticalForce[i][j];
            }
        }
    }
}



void horizontalAxisWindTurbinesALMAdvanced::computeTowerPointForce()
{
    // The tower nacelle wind vector is in the Cartesian coordinate system so no need
    // to transform it either.

    // Compute the tower forces at each actuator point.
    forAll(towerWindVectors, i)
    {
        if (includeTower[i])
        {
            // Set the total tower forces of the turbine to zero.  They will be summed on a tower-element-
            // wise basis.
            towerAxialForce[i] = 0.0;
            towerHorizontalForce[i] = 0.0;

            // Get necessary axes.
            vector axialVector = uvShaft[i];
            axialVector.z() = 0.0;
            axialVector = axialVector / mag(axialVector);
            vector verticalVector = vector::zero;
            verticalVector.z() = 1.0;
            vector horizontalVector = -(axialVector ^ verticalVector);
            horizontalVector = horizontalVector / mag(horizontalVector);


            // Proceed point by point.
            forAll(towerWindVectors[i], j)
            {
                // Find the local velocity magnitude composed of only the horizontal part of the flow.
                towerPointVmag[i][j] = Foam::pow((Foam::pow(towerWindVectors[i][j].x(),2) + Foam::pow(towerWindVectors[i][j].y(),2)),0.5);

                // If the advanced tower model is used, use potential flow theory to scale up the sampled
                // wind speed to freestream.
                if (towerForceProjectionType[i] == "advanced" )
                {
                   scalar R = towerPointChord[i][j] / 2.0;
                   scalar r = towerSampleDistance[i];
                   if (r > 1.5*R)
                   {
                       scalar VmagOld = towerPointVmag[i][j];
                       towerPointVmag[i][j] *= 1.0/(1.0 - sqr(R/r));
                     //Info << "j: " << j << tab << "r = " << r << tab << "R = " << R << tab << VmagOld << tab << towerPointVmag[i][j] << endl;
                   }
                }

                // Get the angle of the wind.
                scalar windAng = Foam::atan2(towerWindVectors[i][j].y(),towerWindVectors[i][j].x())/degRad; 

                // Get the angle of the nacelle.
                scalar nacelleAng = Foam::atan2(-axialVector.y(),-axialVector.x())/degRad;

                // Angle of attack.
                towerPointAlpha[i][j] = windAng - nacelleAng + towerPointTwist[i][j];

                // Use airfoil look-up tables to get coefficient of bladePointLift and drag.
                towerPointCl[i][j] = interpolate(towerPointAlpha[i][j], airfoilAlpha[towerPointAirfoil[i][j]], airfoilCl[towerPointAirfoil[i][j]]);
                towerPointCd[i][j] = interpolate(towerPointAlpha[i][j], airfoilAlpha[towerPointAirfoil[i][j]], airfoilCd[towerPointAirfoil[i][j]]);

                // Using Cl, Cd, wind velocity, chord, and actuator element width, calculate the
                // lift and drag per density.
                towerPointLift[i][j] = 0.5 * towerPointCl[i][j] * towerPointVmag[i][j] * towerPointVmag[i][j] * towerPointChord[i][j] * towerDs[i][j];
                towerPointDrag[i][j] = 0.5 * towerPointCd[i][j] * towerPointVmag[i][j] * towerPointVmag[i][j] * towerPointChord[i][j] * towerDs[i][j];

                // Make the scalar lift and drag quantities vectors in the Cartesian coordinate system.
                vector dragVector = towerWindVectors[i][j];
                dragVector.z() = 0.0;
                dragVector = dragVector/mag(dragVector);

                vector liftVector = -dragVector^verticalVector;
                liftVector = liftVector/mag(liftVector);
 
                liftVector = -towerPointLift[i][j] * liftVector;
                dragVector = -towerPointDrag[i][j] * dragVector;

                // Add up bladePointLift and drag to get the resultant force/density applied to this blade element.
                towerPointForce[i][j] = liftVector + dragVector;

                // Find the component of the tower element force/density in the different directions.
                // Axial is horizontal but aligned with the shaft.
                // Horizontal is horizontal but perpendicular to axial.
                towerPointAxialForce[i][j] = -towerPointForce[i][j] & axialVector;
                towerPointHorizontalForce[i][j] = -towerPointForce[i][j] & horizontalVector;

                // Add this blade element's contribution to the total turbine forces/moments.
                towerAxialForce[i] += towerPointAxialForce[i][j];
                towerHorizontalForce[i] += towerPointHorizontalForce[i][j];
            }
        }
    }
}


scalar horizontalAxisWindTurbinesALMAdvanced::computeBladeProjectionFunction(vector disVector, int turbineNumber, int bladeNumber, int elementNumber)
{
    int i = turbineNumber;
    int j = bladeNumber;
    int k = elementNumber;
      
    scalar dis = mag(disVector);

    scalar spreading = 1.0;

    if (bladeForceProjectionType[i] == "uniformGaussian")
    {
        spreading = uniformGaussian3D(bladeEpsilon[i][0], dis);
    }
    else if (bladeForceProjectionType[i] == "variableUniformGaussianChord")
    {
        scalar epsilonScalar = bladeEpsilon[i][0];
        scalar epsilonMin = bladeEpsilon[i][1];
        scalar epsilonMax = bladeEpsilon[i][2];
        scalar epsilon = max(min((epsilonScalar * bladePointChord[i][j][k]), epsilonMax), epsilonMin);
        spreading = uniformGaussian3D(epsilon, dis);
    }
    else if (bladeForceProjectionType[i] == "variableUniformGaussianUserDef")
    {
        scalar epsilonScalar = bladeEpsilon[i][0];
        scalar epsilonMin = bladeEpsilon[i][1];
        scalar epsilonMax = bladeEpsilon[i][2];
        scalar epsilon = max(min((epsilonScalar * bladePointUserDef[i][j][k]), epsilonMax), epsilonMin);
        spreading = uniformGaussian3D(epsilon, dis);
    }
    else if (bladeForceProjectionType[i] == "generalizedGaussian")
    {
        vector dir0 = bladeAlignedVectors[i][j][1];
        vector dir1 = bladeAlignedVectors[i][j][0];
        vector dir2 = bladeAlignedVectors[i][j][2];
        dir0 = rotateVector(dir0, vector::zero, dir2, -(bladePointTwist[i][j][k] + bladePitch[i])*degRad);
        dir1 = rotateVector(dir1, vector::zero, dir2, -(bladePointTwist[i][j][k] + bladePitch[i])*degRad);
        spreading = generalizedGaussian3D(bladeEpsilon[i], disVector, dir0, dir1, dir2);
    }
    else if (bladeForceProjectionType[i] == "chordThicknessGaussian")
    {
        scalar epsilonScalar0 = bladeEpsilon[i][0];
        scalar epsilonScalar1 = bladeEpsilon[i][1];
        scalar epsilonScalar2 = bladeEpsilon[i][2];
        vector epsilon = vector::zero;
        epsilon[0] = epsilonScalar0 * bladePointChord[i][j][k];
        epsilon[1] = epsilonScalar1 * bladePointThickness[i][j][k] * bladePointChord[i][j][k];
        epsilon[2] = epsilonScalar2 * bladeDs[i][k];
        vector dir0 = bladeAlignedVectors[i][j][1];
        vector dir1 = bladeAlignedVectors[i][j][0];
        vector dir2 = bladeAlignedVectors[i][j][2];
        dir0 = rotateVector(dir0, vector::zero, dir2, -(bladePointTwist[i][j][k] + bladePitch[i])*degRad);
        dir1 = rotateVector(dir1, vector::zero, dir2, -(bladePointTwist[i][j][k] + bladePitch[i])*degRad);
      //dir0 = rotateVector(dir0, vector::zero, dir2, -(bladePointTwist[i][j][k] + bladePitch[i] + bladePointAlpha[i][j][k])*degRad);
      //dir1 = rotateVector(dir1, vector::zero, dir2, -(bladePointTwist[i][j][k] + bladePitch[i] + bladePointAlpha[i][j][k])*degRad);
        spreading = generalizedGaussian3D(epsilon, disVector, dir0, dir1, dir2);
    }
    else if (bladeForceProjectionType[i] == "generalizedGaussian2D")
    {
        vector dir0 = bladeAlignedVectors[i][j][1];
        vector dir1 = bladeAlignedVectors[i][j][0];
        vector dir2 = bladeAlignedVectors[i][j][2];
        dir0 = rotateVector(dir0, vector::zero, dir2, -(bladePointTwist[i][j][k] + bladePitch[i])*degRad);
        dir1 = rotateVector(dir1, vector::zero, dir2, -(bladePointTwist[i][j][k] + bladePitch[i])*degRad);
        spreading = generalizedGaussian2D(bladeEpsilon[i], disVector, dir0, dir1);
    }
    else if (bladeForceProjectionType[i] == "chordThicknessGaussian2D")
    {
        scalar epsilonScalar0 = bladeEpsilon[i][0];
        scalar epsilonScalar1 = bladeEpsilon[i][1];
        vector epsilon = vector::zero;
        epsilon[0] = epsilonScalar0 * bladePointChord[i][j][k];
        epsilon[1] = epsilonScalar1 * bladePointThickness[i][j][k] * bladePointChord[i][j][k];
        vector dir0 = bladeAlignedVectors[i][j][1];
        vector dir1 = bladeAlignedVectors[i][j][0];
        vector dir2 = bladeAlignedVectors[i][j][2];
      //dir0 = rotateVector(dir0, vector::zero, dir2, -(bladePointTwist[i][j][k] + bladePitch[i])*degRad);
      //dir1 = rotateVector(dir1, vector::zero, dir2, -(bladePointTwist[i][j][k] + bladePitch[i])*degRad);
        dir0 = rotateVector(dir0, vector::zero, dir2, -(bladePointTwist[i][j][k] + bladePitch[i] + bladePointAlpha[i][j][k])*degRad);
        dir1 = rotateVector(dir1, vector::zero, dir2, -(bladePointTwist[i][j][k] + bladePitch[i] + bladePointAlpha[i][j][k])*degRad);
        spreading = generalizedGaussian2D(epsilon, disVector, dir0, dir1);
    }
    else
    {
        spreading = uniformGaussian3D(bladeEpsilon[i][0], dis);
    }

    return spreading;
}


void horizontalAxisWindTurbinesALMAdvanced::computeBladeBodyForce()
{  
    // Initialize variables that are integrated forces.
    scalar rotorAxialForceSum = 0.0;
    scalar rotorTorqueSum = 0.0;
    scalar rotorAxialForceBodySum = 0.0;
    scalar rotorTorqueBodySum = 0.0;



    // Compute body force due to blades.
    gBlade *= 0.0;
    forAll(bladePointForce, i)
    {
        
        int n = turbineTypeID[i];
        
        // Proceed to compute body forces for turbine i only if there are influence cells on this processor for this turbine.
        if (bladeInfluenceCells[i].size() > 0)
        {

            // Get necessary axes.
            vector axialVector = uvShaft[i];
            axialVector.z() = 0.0;
            axialVector = axialVector / mag(axialVector);
            vector verticalVector = vector::zero;
            verticalVector.z() = 1.0;
            vector horizontalVector = -(axialVector ^ verticalVector);
            horizontalVector = horizontalVector / mag(horizontalVector);


            // For each blade.
            forAll(bladePointForce[i], j)
            {
                // For each blade point.
                forAll(bladePointForce[i][j], k)
                {
                    scalar forceLift = 0.0;
                    scalar forceDrag = 0.0;

                    scalar forceDragPosSum = 0.0;
                    scalar forceDragNegSum = 0.0;
                    scalar forceLiftSum = 0.0;
                    vector forceSum = vector::zero;

                    // For each influence cell.
                    forAll(bladeInfluenceCells[i], m)
                    {
                        vector disVector = (mesh_.C()[bladeInfluenceCells[i][m]] - bladePoints[i][j][k]);
                        scalar dis = mag(disVector);
                        if (dis <= bladeProjectionRadius[i])
                        {
                            // Compute the blade force projection at this point.
                            scalar spreading = computeBladeProjectionFunction(disVector,i,j,k);

                            // Add this spreading to the overall force projection field.
                            gBlade[bladeInfluenceCells[i][m]] += spreading;
 
                            // Get the local velocity in the fixed frame of reference.
                            vector localVelocity = U_[bladeInfluenceCells[i][m]];

                            // Add on the relative velocity due to blade rotation.
                            localVelocity += rFromShaft[bladeInfluenceCells[i][m]] * rotorSpeed[i] * bladeAlignedVectors[i][j][1];
                            Urel[bladeInfluenceCells[i][m]] = localVelocity;

                            // Compute the body force contribution.
                            if ((bladeForceProjectionDirection[i] == "localVelocityAligned") ||
                                (bladeForceProjectionDirection[i] == "localVelocityAlignedCorrected"))
                            {
                                // Get the lift component of the bodyForce and make it normal to both
                                // the local velocity vector and the blade radial vector.
                                scalar c = bladePointChord[i][j][k];
                                scalar w = bladeDs[i][k];
                                scalar Uhat = bladePointVmag[i][j][k];
                                scalar Cl = bladePointCl[i][j][k];
                                scalar Cd = bladePointCd[i][j][k];
                                vector ez = bladeAlignedVectors[i][j][2];

                                // Equation 2 from Spalart.
                                vector force = -((c*w)/2.0) * Uhat * ((localVelocity ^ (Cl*ez)) + (Cd * localVelocity)) * spreading;

                                // If we're not correcting to recover desired lift and drag, then go ahead
                                // and add on to the bodyForce field.
                                if (bladeForceProjectionDirection[i] == "localVelocityAligned")
                                {
                                    bodyForce[bladeInfluenceCells[i][m]] += force;
                                }

                                vector dragVector = bladeAlignedVectors[i][j][0]*bladeWindVectors[i][j][k].x() + bladeAlignedVectors[i][j][1]*bladeWindVectors[i][j][k].y();
                                dragVector = dragVector/mag(dragVector);

                                vector liftVector = dragVector^bladeAlignedVectors[i][j][2];
                                liftVector = liftVector/mag(liftVector);

                                forceLift = (force & liftVector) * mesh_.V()[bladeInfluenceCells[i][m]];
                                forceDrag = (force & dragVector) * mesh_.V()[bladeInfluenceCells[i][m]];
                                forceLiftSum += forceLift;
                                forceDragPosSum += max(0.0,forceDrag);
                                forceDragNegSum += min(0.0,forceDrag);
                                forceSum += force * mesh_.V()[bladeInfluenceCells[i][m]];
                            }
                            else if (bladeForceProjectionDirection[i] == "sampledVelocityAligned")
                            {
                                bodyForce[bladeInfluenceCells[i][m]] += bladePointForce[i][j][k] * spreading;
                            }
                            else
                            {
                                bodyForce[bladeInfluenceCells[i][m]] += bladePointForce[i][j][k] * spreading;
                            }

                            
                            // Compute global body-force-derived forces/moments for all force projection directions except
                            // the local-velocity-aligned method that is corrected.  We will do this after correction.
                            if (bladeForceProjectionDirection[i] != "localVelocityAlignedCorrected")
                            {
                              //rotorAxialForceBodySum += (-bodyForce[bladeInfluenceCells[i][m]] * mesh_.V()[bladeInfluenceCells[i][m]]) & axialVector;
                              //rotorTorqueBodySum += (bodyForce[bladeInfluenceCells[i][m]] * bladePointRadius[i][j][k] * cos(PreCone[n][j]) * mesh_.V()[bladeInfluenceCells[i][m]]) 
                              //                      & bladeAlignedVectors[i][j][1];

                                rotorAxialForceBodySum += (-bladePointForce[i][j][k] * spreading * mesh_.V()[bladeInfluenceCells[i][m]]) & axialVector;
                                rotorTorqueBodySum += (bladePointForce[i][j][k] * spreading * bladePointRadius[i][j][k] * cos(PreCone[n][j]) * mesh_.V()[bladeInfluenceCells[i][m]]) 
                                                      & bladeAlignedVectors[i][j][1];
                            }
                        }
                    }

                    // Parallel sum the integrated body force lift and +/- drag.
                    if (bladeForceProjectionDirection[i] == "localVelocityAlignedCorrected")
                    {
                        reduce(forceLiftSum,sumOp<scalar>());
                        reduce(forceDragPosSum,sumOp<scalar>());
                        reduce(forceDragNegSum,sumOp<scalar>());
                        reduce(forceSum,sumOp<vector>()); 
                        forceDragPosSum = max(forceDragPosSum,1.0E-20);
                        forceDragNegSum = min(forceDragNegSum,-1.0E-20); 
                        Info << "forceLiftSum = " << forceLiftSum << endl;
                        Info << "forceDragPosSum = " << forceDragPosSum << endl;
                        Info << "forceDragNegSum = " << forceDragNegSum << endl;
                        Info << "forceDragPosSum + forceDragNegSum = " << forceDragPosSum + forceDragNegSum << endl;
                        Info << "forceSum = " << forceSum << endl;

                        scalar d = bladePointLift[i][j][k] / forceLiftSum;
                        Info << "d = " << d << endl;
                        scalar rdet = 1.0/(-2.0*forceDragPosSum*forceDragNegSum);
                        Info << "rdet = " << rdet << endl;
                        scalar a = rdet * (forceDragNegSum*bladePointDrag[i][j][k] - forceDragNegSum*(forceDragPosSum-forceDragNegSum));
                        Info << "a = " << a << endl;
                        scalar b = rdet * (forceDragPosSum*bladePointDrag[i][j][k] + forceDragPosSum*(forceDragPosSum-forceDragNegSum));
                        Info << "b = " << b << endl;

                        // Where a and b scalars come from:
                        //  --> dragPos
                        //  <-- dragNeg

                        //  a*dragPos + b*dragNeg = desiredDrag
                        //  a*dragPos - b*dragNeg = dragPos - dragNeg

                        //  | dragPos  dragNeg| |a|  =  |desiredDrag      |
                        //  | dragPos -dragNeg| |b|     |dragPos - dragNeg|

                        //  |a|  =  1/(-2*dragPos*dragNeg) * |-dragNeg -dragNeg| |desiredDrag      |
                        //  |b|  =                           |-dragPos  dragPos| |dragPos - dragNeg|

                        //  a = 1/(-2*dragPos*dragNeg) * (-dragNeg*desiredDrag - dragNeg*(dragPos-dragNeg))
                        //  b = 1/(-2*dragPos*dragNeg) * (-dragPos*desiredDrag + dragPos*(dragPos-dragNeg))


                        // For each influence cell.
                        forAll(bladeInfluenceCells[i], m)
                        {
                            vector disVector = (mesh_.C()[bladeInfluenceCells[i][m]] - bladePoints[i][j][k]);
                            scalar dis = mag(disVector);
                            if (dis <= bladeProjectionRadius[i])
                            {
                                // Compute the blade force projection at this point.
                                scalar spreading = computeBladeProjectionFunction(disVector,i,j,k);

                                // Get the local velocity in the fixed frame of reference.
                                vector localVelocity = U_[bladeInfluenceCells[i][m]];

                                // Add on the relative velocity due to blade rotation.
                                localVelocity += rFromShaft[bladeInfluenceCells[i][m]] * rotorSpeed[i] * bladeAlignedVectors[i][j][1];
                                Urel[bladeInfluenceCells[i][m]] = localVelocity;

                                // Get the lift component of the bodyForce and make it normal to both
                                // the local velocity vector and the blade radial vector.
                                scalar c = bladePointChord[i][j][k];
                                scalar w = bladeDs[i][k];
                                scalar Uhat = bladePointVmag[i][j][k];
                                scalar Cl = bladePointCl[i][j][k];
                                scalar Cd = bladePointCd[i][j][k];
                                vector ez = bladeAlignedVectors[i][j][2];
                                
                                // Equation 2 from Spalart.
                                vector force = -((c*w)/2.0) * Uhat * ((localVelocity ^ (Cl*ez)) + (Cd * localVelocity)) * spreading;

                                // Transform to the local lift, drag, span coordinate system.
                                vector dragVector = bladeAlignedVectors[i][j][0]*bladeWindVectors[i][j][k].x() + bladeAlignedVectors[i][j][1]*bladeWindVectors[i][j][k].y();
                                dragVector = dragVector/mag(dragVector);

                                vector liftVector = dragVector^bladeAlignedVectors[i][j][2];
                                liftVector = liftVector/mag(liftVector);

                                vector forceP = transformVectorCartToLocal(force,liftVector,dragVector,ez);

                                // Scale the lift and drag forces.
                                forceP.x() *= -d;
                                if (forceP.y() >= 0.0)
                                {
                                    forceP.y() *= a;
                                }
                                else if (forceP.y() < 0.0)
                                {
                                    forceP.y() *= b;
                                }

                                // Transform back to the Cartesian system.
                                force = transformVectorLocalToCart(forceP,liftVector,dragVector,ez);

                                forceLift = (force & liftVector) * mesh_.V()[bladeInfluenceCells[i][m]];
                                forceDrag = (force & dragVector) * mesh_.V()[bladeInfluenceCells[i][m]];
                                forceLiftSum += forceLift;
                                forceDragPosSum += max(0.0,forceDrag);
                                forceDragNegSum += min(0.0,forceDrag);
                                forceSum += force * mesh_.V()[bladeInfluenceCells[i][m]];

                                // Add the force to the bodyForce field.
                                bodyForce[bladeInfluenceCells[i][m]] += force;

                                // Compute global body-force-derived forces/moments for all force projection directions except
                                // the local-velocity-aligned method that is corrected.  We will do this after correction.
                              //rotorAxialForceBodySum += (-bodyForce[bladeInfluenceCells[i][m]] * mesh_.V()[bladeInfluenceCells[i][m]]) & axialVector;
                              //rotorTorqueBodySum += (bodyForce[bladeInfluenceCells[i][m]] * bladePointRadius[i][j][k] * cos(PreCone[n][j]) * mesh_.V()[bladeInfluenceCells[i][m]]) 
                              //                      & bladeAlignedVectors[i][j][1];
                                rotorAxialForceBodySum += (-force * mesh_.V()[bladeInfluenceCells[i][m]]) & axialVector;
                                rotorTorqueBodySum += (force * bladePointRadius[i][j][k] * cos(PreCone[n][j]) * mesh_.V()[bladeInfluenceCells[i][m]]) 
                                                      & bladeAlignedVectors[i][j][1];
                            }
                        }
                    }


                }  
            }
        }
        // Compute global actuator-element-force-derived forces/moments
        rotorAxialForceSum += rotorAxialForce[i];
        rotorTorqueSum += rotorTorque[i];
    }
    reduce(rotorAxialForceBodySum,sumOp<scalar>());
    reduce(rotorTorqueBodySum,sumOp<scalar>());


    // Print information comparing the actual rotor thrust and torque to the integrated body force.
    Info << "Rotor Axial Force from Body Force = " << rotorAxialForceBodySum << tab << "Rotor Axial Force from Actuator = " << rotorAxialForceSum << tab
         << "Ratio = " << rotorAxialForceBodySum/max(rotorAxialForceSum,1.0E-5) << endl;
    Info << "Rotor Torque from Body Force = " << rotorTorqueBodySum << tab << "Rotor Torque from Actuator = " << rotorTorqueSum << tab 
         << "Ratio = " << rotorTorqueBodySum/max(rotorTorqueSum,1.0E-5) << endl;
}


void horizontalAxisWindTurbinesALMAdvanced::computeNacelleBodyForce()
{  
    // Initialize variables that are integrated forces.
    scalar nacelleAxialForceSum = 0.0;
    scalar nacelleAxialForceBodySum = 0.0;

    // Compute body force due to nacelle.
    forAll(nacellePointForce, i)
    {
        
        int n = turbineTypeID[i];

        if (includeNacelle[i])
        {
            forAll(nacellePointForce[i], j)
            {

                // Get necessary axes.
                vector axialVector = uvShaft[i];
                axialVector.z() = 0.0;
                axialVector = axialVector / mag(axialVector);
                vector verticalVector = vector::zero;
                verticalVector.z() = 1.0;
                vector horizontalVector = -(axialVector ^ verticalVector);
                horizontalVector = horizontalVector / mag(horizontalVector);

            
                forAll(nacelleInfluenceCells[i], m)
                {
                    vector d = mesh_.C()[nacelleInfluenceCells[i][m]] - nacellePoints[i][j];
                    scalar dis = mag(d);
                    if (dis <= nacelleProjectionRadius[i])
                    {
                        scalar spreading = 1.0;
                        scalar r = 0.0;
                        scalar theta = 0.0;
                        scalar pi = constant::mathematical::pi;
                        vector vP = vector::zero;
                        vector xP = vector::zero;
                        vector yP = vector::zero;
                        vector zP = vector::zero;

                        if (nacelleForceProjectionType[i] == "uniformGaussian")
                        {
                            spreading = uniformGaussian3D(nacelleEpsilon[i][0], dis);
                        }
                        else if (nacelleForceProjectionType[i] == "diskGaussian")
                        {
                            spreading = diskGaussian(nacelleEpsilon[i][0], nacelleEpsilon[i][1], axialVector, NacelleEquivalentRadius[n], d);
                        }
                        else if ((nacelleForceProjectionType[i] == "advanced1") || (nacelleForceProjectionType[i] == "advanced2"))
                        {
                            vector v = mesh_.C()[nacelleInfluenceCells[i][m]] - rotorApex[i];

                            zP = vector::zero;
                            zP.z() = 1.0;

                            xP = uvShaft[i];
                            xP /= mag(xP);

                            yP = zP ^ xP;
                            yP /= mag(yP);

                            zP = xP ^ yP;
                            zP /= mag(zP);

                            vP = transformVectorCartToLocal(v, xP, yP, zP);

                            if ((vP.x() > 0.0) && (vP.x() < NacelleLength[n]))
                            {
                                r = sqrt(sqr(vP.y()) + sqr(vP.z()));
                                theta = pi/2.0;
                            }
                            else if (vP.x() <= 0.0)
                            {
                                r = sqrt(sqr(vP.x()) + sqr(vP.y()) + sqr(vP.z()));
                                scalar h = sqrt(sqr(vP.y()) + sqr(vP.z()));
                                theta = atan2(h,-vP.x());
                            }
                            else if (vP.x() >= NacelleLength[n])
                            {
                                r = sqrt(sqr(vP.x() - NacelleLength[n]) + sqr(vP.y()) + sqr(vP.z()));
                                scalar h = sqrt(sqr(vP.y()) + sqr(vP.z()));
                                theta = atan2(h,-(vP.x() - NacelleLength[n]));
                            }

                            scalar L = NacelleLength[n];
                            scalar epsilonR = nacelleEpsilon[i][0];
                            scalar r0 = NacelleEquivalentRadius[n];

                            scalar coeff = 1.0 / ( L * pow(pi,1.5) * r0 * epsilonR + 
                                                   L * pi * sqr(epsilonR) * exp(-sqr(r0 / epsilonR)) + 
                                                   L * pow(pi,1.5) * r0 * epsilonR * erf(r0 / epsilonR) + 
                                                   2.0 * pow(pi,1.5) * epsilonR * (2.0*sqr(r0) + sqr(epsilonR)) * erf(r0 / epsilonR) +
                                                   2.0 * pi * sqr(epsilonR) * r0 * exp(-sqr(r0 / epsilonR)) );

                            spreading = gaussian1D(r, NacelleEquivalentRadius[n], nacelleEpsilon[i][0], coeff);
                        }
                        else
                        {
                            spreading = uniformGaussian3D(nacelleEpsilon[i][0], dis);
                        }



                        // This is the advanced nacelle body force projection that projects into a shell resembling a nacelle.
                        vector bodyForceContrib = vector::zero;
                        scalar forceBase = 0.0;

                        scalar x1 = (1.0/9.0) * NacelleLength[n];
                        scalar x2 = (8.0/9.0) * NacelleLength[n];
                        scalar theta1 = 125.0 * pi / 180.0;
                        scalar CpSide =  0.25;
                        scalar CpBack =  0.0;
                        scalar a1 = 16.0 * x1;
                        scalar a2 = 16.0 * (NacelleLength[n] - x2);
                        scalar a3 = 5.0;
                        scalar c = 1.5643;

                        if ((nacelleForceProjectionType[i] == "advanced1") || (nacelleForceProjectionType[i] == "advanced2"))
                        {
                            vector nacelleNormal = vector::zero;
                            if ((vP.x() > 0.0) && (vP.x() < NacelleLength[n]))
                            {
                                nacelleNormal = vP;
                                nacelleNormal.x() = 0.0;
                                nacelleNormal /= mag(nacelleNormal);
                                if (vP.x() < x1)
                                {
                                    forceBase = 0.5 * (CpSide - 1.25) + 0.5 * (1.25 + CpSide) * erf(a1 * (vP.x() - 0.5*x1));
                                }
                                else if (vP.x() > x2)
                                {
                                    forceBase = 0.5 * (CpSide - 1.25) - 0.5 * (1.25 + CpSide) * erf(a2 * (vP.x() - 0.5*(NacelleLength[n] + x2)));
                                }
                                else
                                {
                                    forceBase = CpSide;
                                }
                            }
                            else if (vP.x() <= 0.0)
                            {
                                nacelleNormal = vP/mag(vP);
                                forceBase = 1.0 - (9.0/4.0)*pow(sin(theta),3.0);
                            }
                            else if (vP.x() >= NacelleLength[n])
                            {
                                nacelleNormal = vP;
                                nacelleNormal.x() -= NacelleLength[n];
                                nacelleNormal /= mag(nacelleNormal);
                              //forceBase = 0.5 * (CpBack - 1.25) + 0.5 * (1.25 + CpBack) * erf(a3 * (theta - 0.5*(pi/2.0 + theta1)));
                                if (theta > theta1)
                                {
                                    forceBase = 1.0 - (9.0/4.0)*pow(sin(theta1),3.0);
                                }
                                else
                                {
                                    forceBase = 1.0 - (9.0/4.0)*pow(sin(theta),3.0);
                                }
                            }

                            nacelleNormal = transformVectorLocalToCart(nacelleNormal, xP, yP, zP);
                            forceBase /= c;

                            //bodyForceContrib = vector::zero;
                            //bodyForceContrib.x() = theta*180.0/pi;
                            //bodyForceContrib.x() = spreading;
                            bodyForceContrib = mag(nacellePointForce[i][j]) * spreading * forceBase * nacelleNormal;
                            bodyForce[nacelleInfluenceCells[i][m]] += bodyForceContrib;
                        }
                        // Otherwise make the force drag only.
                        else
                        {
                            bodyForceContrib = nacellePointForce[i][j] * spreading;
                            bodyForce[nacelleInfluenceCells[i][m]] += bodyForceContrib;
                        }
                        nacelleAxialForceBodySum += (-nacellePointForce[i][j] * spreading * mesh_.V()[nacelleInfluenceCells[i][m]]) & axialVector;
                    }
                }
            }
        }
        nacelleAxialForceSum += nacelleAxialForce[i];
    }
    reduce(nacelleAxialForceBodySum,sumOp<scalar>());

    // Print information comparing the actual tower thrust to the integrated body force.
    Info << "Nacelle Axial Force from BodyForce = " << nacelleAxialForceBodySum << tab << "Nacelle Axial Force from Actuator = " << nacelleAxialForceSum << tab
         << "Ratio = " << nacelleAxialForceBodySum/max(nacelleAxialForceSum,1E-5) << endl;

}


void horizontalAxisWindTurbinesALMAdvanced::computeTowerBodyForce()
{  
    // Initialize variables that are integrated forces.
    scalar towerAxialForceSum = 0.0;
    scalar towerAxialForceBodySum = 0.0;
    
    
    // Compute body force due to tower.
    forAll(towerPointForce, i)
    {
        
        int n = turbineTypeID[i];

        if (includeTower[i])
        {
            forAll(towerPointForce[i], j)
            {

                // Get necessary axes.
                vector axialVector = uvShaft[i];
                axialVector.z() = 0.0;
                axialVector = axialVector / mag(axialVector);
                vector verticalVector = vector::zero;
                verticalVector.z() = 1.0;
                vector horizontalVector = -(axialVector ^ verticalVector);
                horizontalVector = horizontalVector / mag(horizontalVector);

            
                forAll(towerInfluenceCells[i], m)
                {
                    vector d = mesh_.C()[towerInfluenceCells[i][m]] - towerPoints[i][j];
                    scalar dis = mag(d);
                    if (dis <= towerProjectionRadius[i])
                    {
                        scalar spreading = 1.0;
                        if (towerForceProjectionType[i] == "uniformGaussian")
                        {
                            spreading = uniformGaussian3D(towerEpsilon[i][0], dis);
                        }
                        else if (towerForceProjectionType[i] == "diskGaussian")
                        {
                            scalar r = 0.5 * interpolate(towerPointHeight[i][j], TowerStation[n], TowerChord[n]);
                            spreading = diskGaussian(towerEpsilon[i][0], towerEpsilon[i][1], verticalVector, r, d);
                        }
                        else if (towerForceProjectionType[i] == "ringGaussian" || towerForceProjectionType[i] == "advanced" )
                        {
                            scalar r = 0.5 * interpolate(towerPointHeight[i][j], TowerStation[n], TowerChord[n]);
                            spreading = ringGaussian(towerEpsilon[i][0], towerEpsilon[i][1], verticalVector, r, d);
                        }
                        else
                        {
                            spreading = uniformGaussian3D(towerEpsilon[i][0], dis);
                        }


                        // This is the advanced tower force that mimics cylinder pressure (force normal to
                        // the surface with a sine type of distribution) that has axial and side forces.
                        vector bodyForceContrib = vector::zero;
                        if (towerForceProjectionType[i] == "advanced")
                        {
                            scalar pi = constant::mathematical::pi;
                          
                            scalar windAng = Foam::atan2(-towerWindVectors[i][j].y(),-towerWindVectors[i][j].x());
                          //scalar windAng = Foam::atan2(0.0,-10.0);
                            if (windAng < 0.0)
                            {
                                windAng += 2.0*pi;
                            }

                            scalar pointAng = Foam::atan2(d.y(),d.x());
                            if (pointAng < 0.0)
                            {
                                pointAng += 2.0*pi;
                            }

                            scalar theta = windAng - pointAng;
                            if (theta < 0.0)
                            {
                                theta += 2.0*pi;
                            }

                            vector towerNormal = d;
                            towerNormal.z() = 0.0;
                            towerNormal /= mag(towerNormal);

                            scalar c = 2.08325 / (2.0 * pi);
                            scalar forcePotential =  1.0 - 4.0 * sqr(sin(theta));
                            scalar forceCorrection = 1.0 
                                                    -3.0 * exp(-sqr((theta - pi)/(pi/4.0)))
                                                    -1.0 * exp(-sqr((theta)/(pi/2.0)))
                                                    -1.0 * exp(-sqr((theta - 2.0*pi)/(pi/2.0)));
                            scalar forceBase = (forcePotential + forceCorrection) / c;


                            bodyForceContrib = mag(towerPointForce[i][j]) * spreading * forceBase * towerNormal;
                            bodyForce[towerInfluenceCells[i][m]] += bodyForceContrib;
                        }
                        // Otherwise make the force drag only.
                        else
                        {
                            bodyForceContrib = towerPointForce[i][j] * spreading;
                            bodyForce[towerInfluenceCells[i][m]] += bodyForceContrib;
                        }
                        towerAxialForceBodySum += -(bodyForceContrib * mesh_.V()[towerInfluenceCells[i][m]]) & axialVector;
                    }
                }
            }
        }
        towerAxialForceSum += towerAxialForce[i];
    }
    reduce(towerAxialForceBodySum,sumOp<scalar>());

    // Print information comparing the actual tower thrust to the integrated body force.
    Info << "Tower Axial Force from BodyForce = " << towerAxialForceBodySum << tab << "Tower Axial Force from Actuator = " << towerAxialForceSum << tab
         << "Ratio = " << towerAxialForceBodySum/max(towerAxialForceSum,1.0E-5) << endl;
}




scalar horizontalAxisWindTurbinesALMAdvanced::uniformGaussian3D(scalar epsilon, scalar d)
{
    // Compute the 3-dimensional Gaussian.
    scalar f = (1.0 / (Foam::pow(epsilon,3)*Foam::pow(Foam::constant::mathematical::pi,1.5))) * Foam::exp(-Foam::sqr(d/epsilon));
    return f;
}

scalar horizontalAxisWindTurbinesALMAdvanced::generalizedGaussian3D(vector epsilon, vector d, vector dir0, vector dir1, vector dir2)
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

scalar horizontalAxisWindTurbinesALMAdvanced::generalizedGaussian2D(vector epsilon, vector d, vector dir0, vector dir1)
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

scalar horizontalAxisWindTurbinesALMAdvanced::diskGaussian(scalar rEpsilon, scalar xEpsilon, vector u, scalar r0, vector d)
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

scalar horizontalAxisWindTurbinesALMAdvanced::ringGaussian(scalar rEpsilon, scalar xEpsilon, vector u, scalar r0, vector d)
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

scalar horizontalAxisWindTurbinesALMAdvanced::gaussian1D(scalar x, scalar x0, scalar epsilon, scalar coeff)
{
    // Compute a 1D Gaussian function centered about x0 with width epsilon and scaled by coeff.
    scalar f = coeff * Foam::exp(-Foam::sqr((x - x0)/epsilon));

    return f;
}



vector horizontalAxisWindTurbinesALMAdvanced::rotateVector(vector v, vector translation, vector axis, scalar angle)
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



vector horizontalAxisWindTurbinesALMAdvanced::transformVectorCartToLocal(vector v, vector xP, vector yP, vector zP)
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




vector horizontalAxisWindTurbinesALMAdvanced::transformVectorLocalToCart(vector vP, vector xP, vector yP, vector zP)
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




scalar horizontalAxisWindTurbinesALMAdvanced::interpolate(scalar xNew, DynamicList<scalar>& xOld, DynamicList<scalar>& yOld)
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


label horizontalAxisWindTurbinesALMAdvanced::interpolate(scalar xNew, DynamicList<scalar>& xOld, DynamicList<label>& yOld)
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

scalar horizontalAxisWindTurbinesALMAdvanced::compassToStandard(scalar dir)
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

scalar horizontalAxisWindTurbinesALMAdvanced::standardToCompass(scalar dir)
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
    
void horizontalAxisWindTurbinesALMAdvanced::update()
{
    // Update the time step size.
    dt = runTime_.deltaT().value();

    // Update the current simulation time.
    time = runTime_.timeName();
    t = runTime_.value();

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
        filterRotSpeed();
        controlGenTorque();
        controlBladePitch();
        controlNacYaw();
        computeRotSpeed();
        rotateBlades();
        yawNacelle();

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
        filterRotSpeed();
        controlGenTorque();
        controlBladePitch();
        controlNacYaw();
        computeRotSpeed();
        rotateBlades();
        yawNacelle();

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
    pastFirstTimeStep = true;
}


void horizontalAxisWindTurbinesALMAdvanced::openOutputFiles()
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


void horizontalAxisWindTurbinesALMAdvanced::printOutputFiles()
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
   
     
void horizontalAxisWindTurbinesALMAdvanced::printDebug()
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
    Info << "NacelleLength = " << NacelleLength << endl;
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
    Info << "uvShaftDir = " << uvShaftDir << endl;
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


volVectorField& horizontalAxisWindTurbinesALMAdvanced::force()
{
    // Return the body force field to the solver
    return bodyForce;
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace turbineModels
} // End namespace Foam

// ************************************************************************* //

