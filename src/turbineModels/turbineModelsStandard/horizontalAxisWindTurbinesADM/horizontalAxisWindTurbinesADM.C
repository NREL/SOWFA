/*---------------------------------------------------------------------------*\
This file was modified or created at the National Renewable Energy
Laboratory (NREL) on January 8, 2014 in creating the SOWFA (Simulator for
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

#include "horizontalAxisWindTurbinesADM.H"
#include "interpolateXY.H"

namespace Foam
{
namespace turbineModels
{

// * * * * * * * * * * * * * *  Constructor  * * * * * * * * * * * * * * * * //

horizontalAxisWindTurbinesADM::horizontalAxisWindTurbinesADM
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
            if (listTemp[i] != "globalProperties" && listTemp[i] != "sscProperties") // Edit to ignore 'sscProperties' as a turbine
            {
                turbineName.append(listTemp[i]);
            }
        }
    }

    numTurbines = turbineName.size();

	// SSC stuff goes here
	sscEnabled = turbineArrayProperties.subDict("sscProperties").lookupOrDefault<bool>("sscEnabled",false);
	if(sscEnabled) {
		printf("The SSC is enabled.\n");
	} else {
		printf("The SSC is disabled.\n");
	}
	//
	
    outputControl = turbineArrayProperties.subDict("globalProperties").lookupOrDefault<word>("outputControl","timeStep");
    outputInterval = turbineArrayProperties.subDict("globalProperties").lookupOrDefault<scalar>("outputInterval",1);
    perturb = turbineArrayProperties.subDict("globalProperties").lookupOrDefault<scalar>("perturb",1E-5);
    lastOutputTime = runTime_.startTime().value();
    outputIndex = 0;

    forAll(turbineName,i)
    {
        turbineType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("turbineType")));
        baseLocation.append(vector(turbineArrayProperties.subDict(turbineName[i]).lookup("baseLocation")));
        nRadial.append(int(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("nRadial"))));
        azimuthMaxDis.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("azimuthMaxDis"))));
        nAvgSector.append(int(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("nAvgSector"))));
        pointDistType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("pointDistType")));
        pointInterpType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("pointInterpType")));
        bladeUpdateType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("bladeUpdateType")));
        epsilon.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("epsilon"))));
        forceScalar.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("forceScalar"))));
        inflowVelocityScalar.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("inflowVelocityScalar"))));
        tipRootLossCorrType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("tipRootLossCorrType")));
        rotationDir.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("rotationDir")));
        rotSpeed.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("RotSpeed"))));
        rotSpeedF.append(rotSpeed[i]);
        speedError.append(0.0);
        intSpeedError.append(0.0);
        azimuth.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("Azimuth"))));
        torqueGen.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("TorqueGen"))));
        pitch.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("Pitch"))));
        nacYaw.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("NacYaw"))));
        fluidDensity.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("fluidDensity")))); 
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
        TowerHt.append(scalar(readScalar(turbineProperties.lookup("TowerHt"))));
        Twr2Shft.append(scalar(readScalar(turbineProperties.lookup("Twr2Shft"))));
        ShftTilt.append(scalar(readScalar(turbineProperties.lookup("ShftTilt"))));
        PreCone.append(vector(turbineProperties.lookup("PreCone")));
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

        DynamicList<scalar> station;
        DynamicList<scalar> chord;
        DynamicList<scalar> twist;
        DynamicList<label> id;


        forAll(BladeData[i], j)
        {
            station.append(BladeData[i][j][0]);
            chord.append(BladeData[i][j][1]);
            twist.append(BladeData[i][j][2]);
            id.append(BladeData[i][j][3]);
        }

        BladeStation.append(station);
        BladeChord.append(chord);
        BladeTwist.append(twist);
        BladeAirfoilTypeID.append(id);

        station.clear();
        chord.clear();
        twist.clear();
        id.clear();
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
   


  

    // For each distinct airfoil, read in the lift and drag versus angle
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
    azimuth   = degRad * azimuth;
    rotSpeed  = rpmRadSec * rotSpeed;
    rotSpeedF = rpmRadSec * rotSpeedF;
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




    // Define the cells that can possibly be influenced by the force
    // exerted each turbine.  In otherwords, define a sphere of cell IDs
    // around each turbine that will be saved into memory so that the
    // entire domain need not be passed through when applying the force 
    // field.  (The i-index is at the turbine array level for each 
    // turbine, the j-index is for each type of turbine--if all turbines
    // are the same, j is always 0, and the k-index is at the individual
    // blade level.)
    for(int i = 0; i < numTurbines; i++)
    {
        // First compute the radius of the force projection (to the radius
        // where the projection is only 0.001 its maximum value - this seems
        // recover 99.9% of the total forces when integrated).
        projectionRadius.append(epsilon[i] * Foam::sqrt(Foam::log(1.0/0.001)));

        // Calculate the sphere of influence radius.
        int j = turbineTypeID[i];
        scalar sphereRadius = Foam::sqrt(Foam::sqr((OverHang[j] + UndSling[j]) + TipRad[j]*Foam::sin(PreCone[j][0])) + Foam::sqr(TipRad[j]*Foam::cos(PreCone[j][0])));
        sphereRadius += projectionRadius[i];

        // Find the cells within the sphere of influence.
        DynamicList<label> sphereCellsI;
        forAll(U_.mesh().cells(),cellI)
        {
            if (mag(U_.mesh().C()[cellI] - towerShaftIntersect[i]) <= sphereRadius)
            {
                sphereCellsI.append(cellI);
            }
        }
        sphereCells.append(sphereCellsI);
        sphereCellsI.clear();

        // Create a list of turbines that this processor could forseeably control.
        // If sphereCells[i] is not empty, then turbine i belongs in the list.
        if (sphereCells[i].size() > 0)
        {
            turbinesControlled.append(i);
        }
    }


    
    // Create the actuator disk points (not yet rotated for initial nacelle
    // yaw). i-index is at array level, j-index is
    // for the type of turbine, k-index is for each blade, and m-index is
    // for each actuator point.  Also create other important vectors, and
    // initialize the blade force, blade aligned coordinate system, and
    // wind vectors to zero.
    totDiskPointsArray = 0;
    Random rndGen(123456);
    for(int i = 0; i < numTurbines; i++)
    {
        totDiskPoints.append(0);
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

        // Calculate the width of each actuator section.
        dr.append(DynamicList<scalar>(0));
        if(pointDistType[i] == "uniform")
        {
            scalar actuatorRadialWidth = (TipRad[j]-HubRad[j])/nRadial[i];
            for(int m = 0; m < nRadial[i]; m++)
            {
                dr[i].append(actuatorRadialWidth);
            }
        }
        // Add other point distribution types here, such as cosine, tanh.

        // Calculate how many azimuthal sections are at each radial station
        bladeRadius.append(List<scalar>(nRadial[i],0.0));
        solidity.append(List<scalar>(nRadial[i],0.0));
        nAzimuth.append(List<int>(nRadial[i],0));
        scalar dist = 0.0;
        for(int m = 0; m < nRadial[i]; m++)
        {
            dist = dist + 0.5*dr[i][m];
            bladeRadius[i][m] = HubRad[j] + dist;
            dist = dist + 0.5*dr[i][m];

            // Get the local circumference
            scalar circ = 2.0 * Foam::constant::mathematical::pi * bladeRadius[i][m];

            // Divide it by the max azimuthal distance
            nAzimuth[i][m] = circ/azimuthMaxDis[i];

            // Make sure that each sector over which averaging is done has
            // an equal number of actuator elements in the azimuth direction
            if (nAzimuth[i][m] % nAvgSector[i] != 0)
            {
                nAzimuth[i][m] = nAzimuth[i][m] - (nAzimuth[i][m] % nAvgSector[i]) + nAvgSector[i];
            }

            // Make sure that each sector has at least 1 actuator element in it.
            if (nAzimuth[i][m] < nAvgSector[i])
            {
                nAzimuth[i][m] = nAvgSector[i];
            }

            // Calculate the solidity factor at this radius.
          //scalar chord = interpolate(bladeRadius[i][m], BladeStation[j], BladeChord[j]);
          //solidity[i][m] = NumBl[j]*chord / (2.0 * Foam::constant::mathematical::pi * bladeRadius[i][m] * Foam::cos(PreCone[j][0]));
            solidity[i][m] = scalar(NumBl[j]) / scalar(nAzimuth[i][m]);
        }


        sectorIndices.append(List<List<label> >(nRadial[i]));
        bladePoints.append(List<List<vector> >(nRadial[i]));
        bladePointsPerturbVector.append(List<List<vector> >(nRadial[i]));
        elementAzimuth.append(List<List<scalar> >(nRadial[i]));
        bladeForce.append(List<List<vector> >(nRadial[i]));
        bladeAlignedVectors.append(List<List<List<vector > > >(nRadial[i]));
        windVectors.append(List<List<vector> >(nRadial[i]));
        alpha.append(List<List<scalar> >(nRadial[i]));
        Vmag.append(List<List<scalar> >(nRadial[i]));
        Cl.append(List<List<scalar> >(nRadial[i]));
        Cd.append(List<List<scalar> >(nRadial[i]));
        lift.append(List<List<scalar> >(nRadial[i]));
        drag.append(List<List<scalar> >(nRadial[i]));
        axialForce.append(List<List<scalar> >(nRadial[i]));
        tangentialForce.append(List<List<scalar> >(nRadial[i]));
        minDisCellID.append(List<List<label> >(nRadial[i]));
        deltaNacYaw.append(0.0);
        deltaAzimuth.append(0.0);
        thrust.append(0.0);
        torqueRotor.append(0.0);
        powerRotor.append(0.0);
        powerGenerator.append(0.0);
        for(int m = 0; m < nRadial[i]; m++)
        {
            sectorIndices[i][m].append(List<label>(nAzimuth[i][m],0));
            bladePoints[i][m].append(List<vector>(nAzimuth[i][m],vector::zero));
            bladePointsPerturbVector[i][m].append(List<vector>(nAzimuth[i][m],vector::zero));
            elementAzimuth[i][m].append(List<scalar>(nAzimuth[i][m],0.0));
            bladeForce[i][m].append(List<vector>(nAzimuth[i][m],vector::zero));
            bladeAlignedVectors[i][m].append(List<List<vector> >(nAzimuth[i][m]));
            for(int n = 0; n < nAzimuth[i][m]; n++)
            {
                bladeAlignedVectors[i][m][n].append(List<vector>(3,vector::zero));
            }
            windVectors[i][m].append(List<vector>(nAzimuth[i][m],vector::zero));
            alpha[i][m].append(List<scalar>(nAzimuth[i][m],0.0));
            Vmag[i][m].append(List<scalar>(nAzimuth[i][m],0.0));
            Cl[i][m].append(List<scalar>(nAzimuth[i][m],0.0));
            Cd[i][m].append(List<scalar>(nAzimuth[i][m],0.0));
            lift[i][m].append(List<scalar>(nAzimuth[i][m],0.0));
            drag[i][m].append(List<scalar>(nAzimuth[i][m],0.0));
            axialForce[i][m].append(List<scalar>(nAzimuth[i][m],0.0));
            tangentialForce[i][m].append(List<scalar>(nAzimuth[i][m],0.0));
            minDisCellID[i][m].append(List<label>(nAzimuth[i][m],-1));
        }
        


        windVectorsSecAvg.append(List<List<vector> >(nAvgSector[i]));
        alphaSecAvg.append(List<List<scalar> >(nAvgSector[i]));
        VmagSecAvg.append(List<List<scalar> >(nAvgSector[i]));
        ClSecAvg.append(List<List<scalar> >(nAvgSector[i]));
        CdSecAvg.append(List<List<scalar> >(nAvgSector[i]));
        liftSecAvg.append(List<List<scalar> >(nAvgSector[i]));
        dragSecAvg.append(List<List<scalar> >(nAvgSector[i]));
        axialForceSecAvg.append(List<List<scalar> >(nAvgSector[i]));
        tangentialForceSecAvg.append(List<List<scalar> >(nAvgSector[i]));
        for(int m = 0; m < nAvgSector[i]; m++)
        {
            windVectorsSecAvg[i][m].append(List<vector>(nRadial[i],vector::zero));
            alphaSecAvg[i][m].append(List<scalar>(nRadial[i],0.0));
            VmagSecAvg[i][m].append(List<scalar>(nRadial[i],0.0));
            ClSecAvg[i][m].append(List<scalar>(nRadial[i],0.0));
            CdSecAvg[i][m].append(List<scalar>(nRadial[i],0.0));
            liftSecAvg[i][m].append(List<scalar>(nRadial[i],0.0));
            dragSecAvg[i][m].append(List<scalar>(nRadial[i],0.0));
            axialForceSecAvg[i][m].append(List<scalar>(nRadial[i],0.0));
            tangentialForceSecAvg[i][m].append(List<scalar>(nRadial[i],0.0));
        }


        // Now calculate the actuator section center points for each blade
        // of each turbine in the array.  All blades points will be calculated
        // at zero azimuth (blade pointing up), and then rotated to its correct
        // position before doing a global rotation to the initial azimuth of
        // the rotor.  Also calculate the radius of each point (not including coning).
        vector root = rotorApex[i];
        scalar beta = PreCone[j][0] - ShftTilt[j];
        root.x() = root.x() + HubRad[j]*Foam::sin(beta);
        root.z() = root.z() + HubRad[j]*Foam::cos(beta);
        dist = 0.0;
        for(int m = 0; m < nRadial[i]; m ++)
        {
            dist = dist + 0.5*dr[i][m];
            scalar dAzimuth = 2.0*Foam::constant::mathematical::pi/nAzimuth[i][m];
            for(int k = 0; k < nAzimuth[i][m]; k++)
            {
               totDiskPointsArray++;
               totDiskPoints[i]++;
               elementAzimuth[i][m][k] = k*dAzimuth;
               bladePoints[i][m][k].x() = root.x() + dist*Foam::sin(beta);
               bladePoints[i][m][k].y() = root.y();
               bladePoints[i][m][k].z() = root.z() + dist*Foam::cos(beta);

               scalar elementAzimuthInt = elementAzimuth[i][m][k] + 0.5 * (2.0*Foam::constant::mathematical::pi) / scalar(nAvgSector[i]);
               scalar sectorWidth = (2.0*Foam::constant::mathematical::pi) / scalar(nAvgSector[i]);
               if (elementAzimuthInt >= 2.0*Foam::constant::mathematical::pi)
               {
                   elementAzimuthInt -= 2.0*Foam::constant::mathematical::pi;
               }
               sectorIndices[i][m][k] = elementAzimuthInt / sectorWidth;

               if (k > 0)
               {
                   bladePoints[i][m][k] = rotatePoint(bladePoints[i][m][k], rotorApex[i], uvShaft[i], elementAzimuth[i][m][k]);
               }
            }
            dist = dist + 0.5*dr[i][m];
        }




        // Generate random numbers for the blade point perturbation during control
        // processor identification.  This does not affect the actual location--it is
        // just there to break ties and make sure > 1 processors don't account for a
        // single blade point.
        if(Pstream::myProcNo() == 0)
        {
            for(int m = 0; m < nRadial[i]; m++)
            {
                for(int k = 0; k < nAzimuth[i][m]; k++)
                {
                    bladePointsPerturbVector[i][m][k] = perturb*(2.0*rndGen.vector01()-vector::one); 
                }
            }
        }
        
    }
    Pstream::scatter(bladePointsPerturbVector);


    // Yaw the nacelle to initial position.
    deltaNacYaw = nacYaw;
    yawNacelle();

    // Rotate the rotor to initial azimuth angle.
    deltaAzimuth =  azimuth;
    rotateBlades();  

    // Find out which processors control each actuator line point.
    findControlProcNo();

    // Compute the wind vectors at this initial time step.
    computeWindVectors();

    // Compute the blade forces due to this wind at the initial time step.
    computeBladeForce();

    // Compute the resultant body force at this initial time step.
    computeBodyForce();

    // Open the turbine data output files and print initial information.
    openOutputFiles();
    computeSectorAverage(); 
    printOutputFiles();
}

// * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * * * //

void horizontalAxisWindTurbinesADM::rotateBlades()
{  
    // Perform rotation turbine by turbine.
    forAll(uvShaft, i)
    {	
        // Check the rotation direction first and set the local delta azimuth
	// variable accordingly.
      //scalar deltaAzimuthI = 0.0;
      //if (rotationDir[i] == "cw")
      //{
      //    deltaAzimuthI =  deltaAzimuth[i];
      //}
      //if (rotationDir[i] == "ccw")
      //{
      //    deltaAzimuthI = -deltaAzimuth[i];
      //}

	// Rotate turbine blades, blade by blade, point by point.
      //forAll(bladePoints[i], j)
      //{
      //    forAll(bladePoints[i][j], k)
      //    {
      //        bladePoints[i][j][k] = rotatePoint(bladePoints[i][j][k], rotorApex[i], uvShaft[i], deltaAzimuthI);
      //    }
      //}   

	// Calculate the new azimuth angle and make sure it isn't
        // bigger than 2*pi.
        if (pastFirstTimeStep)
        {
	    azimuth[i] = azimuth[i] + deltaAzimuth[i];
            if (azimuth[i] >= 2.0 * Foam::constant::mathematical::pi)
            {
                azimuth[i] -= 2.0 * Foam::constant::mathematical::pi;
            }
        }
    }
}
        

void horizontalAxisWindTurbinesADM::yawNacelle()
{
    // Perform rotation turbine by turbine.
    forAll(uvTower, i)
    {
	// Rotate the rotor apex first.
        rotorApex[i] = rotatePoint(rotorApex[i], towerShaftIntersect[i], uvTower[i], deltaNacYaw[i]);

	// Recompute the shaft unit vector since the shaft has rotated.
	uvShaft[i] = rotorApex[i] - towerShaftIntersect[i];
	uvShaft[i] = (uvShaft[i]/mag(uvShaft[i])) * uvShaftDir[i];
	
	// Rotate turbine blades, blade by blade, point by point.
	forAll(bladePoints[i], j)
        {
            forAll(bladePoints[i][j], k)
            {
                bladePoints[i][j][k] = rotatePoint(bladePoints[i][j][k], towerShaftIntersect[i], uvTower[i], deltaNacYaw[i]);
            }
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
        }
    }
}


void horizontalAxisWindTurbinesADM::computeRotSpeed()
{
    // Proceed turbine by turbine.
    forAll(rotSpeed, i)
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
            rotSpeed[i] += (dt/DriveTrainIner[j])*(GBEfficiency[j]*torqueRotor[i]*fluidDensity[i] - GBRatio[j]*torqueGen[i]);
        }


        // Limit the rotor speed to be positive and such that the generator does not turn
        // faster than rated.
        if (RotSpeedLimiter[j])
        {
            # include "limiters/rotSpeedLimiter.H"
        }
 
        // Compute the change in blade azimuth angle based on the time step and current rotor speed.
        deltaAzimuth[i] = rotSpeed[i] * dt;

    }
}


void horizontalAxisWindTurbinesADM::filterRotSpeed()
{
    // Proceed turbine by turbine.
    forAll(rotSpeedF, i)
    {
        // Get the turbine type index.
        int j = turbineTypeID[i];

        // Compute the filtering coefficient based on the corner frequency and time step.
        scalar alpha = exp(-dt * SpeedFilterCornerFrequency[j]);

        // Apply a simple recursive, single-pole, low-pass filter.
        rotSpeedF[i] = (1.0 - alpha)*rotSpeed[i] + alpha*rotSpeedF[i];
    }
}


void horizontalAxisWindTurbinesADM::controlGenTorque()
{
    // Proceed turbine by turbine.
    forAll(torqueGen, i)
    {
        // Get the turbine type index.
        int j = turbineTypeID[i];

        // Get the current filtered generator speed.
        scalar genSpeedF = (rotSpeedF[i]/rpmRadSec)*GBRatio[j];


        // Initialize the commanded generator torque variable;
        scalar torqueGenCommanded = torqueGen[i];



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

        // Update the pitch array.
        torqueGen[i] = torqueGenCommanded;
    }
}
        

void horizontalAxisWindTurbinesADM::controlNacYaw()
{
    // Proceed turbine by turbine.
    forAll(deltaNacYaw, i)
    {
        // Get the turbine type index.
        int j = turbineTypeID[i];


        
        // Apply a controller to update the nacelle yaw position.
        if (NacYawControllerType[j] == "none")
        {
            // Do nothing.
	    deltaNacYaw[i] = 0.0;
        }

        else if (NacYawControllerType[j] == "simple")
        {
            // Placeholder for when this is implemented.
        }
        
        else if (NacYawControllerType[j] == "timeYawTable")
        {
        }


        
        // Limit the change in nacelle yaw angle.
        if (NacYawRateLimiter[j])
        {
        }

    }
}
        

void horizontalAxisWindTurbinesADM::controlBladePitch()
{
    // Proceed turbine by turbine.
    forAll(pitch, i)
    {

        // Get the turbine type index.
        int j = turbineTypeID[i];
        
        // Initialize the gain scheduling variable.
        scalar GK = 0.0;

        // Initialize the commanded pitch variable.
        scalar pitchCommanded = pitch[i]*degRad;


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
        pitch[i] = pitchCommanded/degRad;
    }
}


void horizontalAxisWindTurbinesADM::findControlProcNo()
{
    // Create a local and global list of minimum distance cells to actuator line 
    // points of turbines that this processor controls.  Initialize the values to huge.
    List<scalar> minDisLocal(totDiskPointsArray,1.0E30);
    List<scalar> minDisGlobal(totDiskPointsArray,1.0E30);

    forAll(turbinesControlled, p)
    {
        int i = turbinesControlled[p];
        int iter = 0;
        if(i > 0)
        {
            for(int n = 0; n < i; n++)
            {
                iter += totDiskPoints[n];
            }
        }
        
        forAll(bladePoints[i], j)
        {
            forAll(bladePoints[i][j], k)
            {
                // Find the cell that the actuator point lies within and the distance
                // from the actuator line point to that cell center.
                label cellID = sphereCells[i][0];
                scalar minDis = mag(mesh_.C()[cellID] - (bladePoints[i][j][k] + bladePointsPerturbVector[i][j][k]));

                forAll(sphereCells[i], m)
                {
                    scalar dis = mag(mesh_.C()[sphereCells[i][m]] - (bladePoints[i][j][k] + bladePointsPerturbVector[i][j][k]));
                    if(dis <= minDis)
                    {
                        cellID = sphereCells[i][m];
                    }
                    minDis = mag(mesh_.C()[cellID] - (bladePoints[i][j][k] + bladePointsPerturbVector[i][j][k]));
                }
                minDisLocal[iter] = minDis;
                minDisGlobal[iter] = minDis;
                minDisCellID[i][j][k] = cellID;
                iter++;
            }
        }
    }

    // Parallel gather/scatter the global minimum distance list and reduce it by keeping 
    // only the minimum values.
    Pstream::gather(minDisGlobal,minOp<List<scalar> >());
    Pstream::scatter(minDisGlobal);

    // Compare the global to local lists.  Where the lists agree, this processor controls
    // the actuator line point.
    forAll(turbinesControlled, p)
    {
        int i = turbinesControlled[p];
        int iter = 0;
        if(i > 0)
        {
            for(int n = 0; n < i; n++)
            {
                iter += totDiskPoints[n];
            }
        }
        
        forAll(bladePoints[i], j)
        {
            forAll(bladePoints[i][j], k)
            {
                if(minDisGlobal[iter] != minDisLocal[iter])
                {
                    minDisCellID[i][j][k] = -1;
                }
                iter++;
            }
        }
    }
}	


void horizontalAxisWindTurbinesADM::computeWindVectors()
{
    // Create a list of wind velocity in x, y, z coordinates for each blade point.
    List<vector> windVectorsLocal(totDiskPointsArray,vector::zero);

    // If linear interpolation of the velocity from the CFD mesh to the actuator
    // points is used, we need velocity gradient information.
    gradU = fvc::grad(U_);

    forAll(turbinesControlled, p)
    {
        int i = turbinesControlled[p];
        int iter = 0;
        if(i > 0)
        {
            for(int n = 0; n < i; n++)
            {
                iter += totDiskPoints[n];
            }
        }
        
        forAll(bladePoints[i], j)
        {
            forAll(bladePoints[i][j], k)
            {
                if(minDisCellID[i][j][k] != -1)
                {
                    // If the velocity interpolation is "cellCenter", then just use 
                    // the velocity at the center of the cell within which this
                    // actuator point lies
                    windVectorsLocal[iter] = U_[minDisCellID[i][j][k]];

                    // But if linear interpolation is used, add a correction based
                    // on the local velocity gradient.
                    if (pointInterpType[i] == "linear")
                    {
                        vector dx = bladePoints[i][j][k] - mesh_.C()[minDisCellID[i][j][k]];
                        vector dU = dx & gradU[minDisCellID[i][j][k]];
                        windVectorsLocal[iter] += dU;
                    }
                }
                iter++;
            }
        }
    }

    // Perform a parallel gather of this local list to the master processor and
    // and then parallel scatter the list back out to all the processors.
    Pstream::gather(windVectorsLocal,sumOp<List<vector> >());
    Pstream::scatter(windVectorsLocal);

    // Put the gathered/scattered wind vectors into the windVector variable.
    // Proceed turbine by turbine.
    int iter = 0;
    forAll(windVectors, i)
    {
        // Proceed blade by blade.
        forAll(windVectors[i], j)
        { 
            // Proceed point by point.
            forAll(windVectors[i][j], k)
            {
                // Zero the wind vector.
                windVectors[i][j][k] = vector::zero;

                // Now put the velocity in that cell into blade-oriented coordinates.
                windVectors[i][j][k] = windVectorsLocal[iter];

                // Scale the inflow velocity by the specified scaling factor.
                windVectors[i][j][k] *= inflowVelocityScalar[i];

                iter++;
            }
        }
    }
}


void horizontalAxisWindTurbinesADM::computeBladeForce()
{
    // Take the x,y,z wind vectors and project them into the blade coordinate system.
    // Proceed turbine by turbine.
    forAll(windVectors, i)
    {
        int n = turbineTypeID[i];

        // Proceed blade by blade.
        forAll(windVectors[i], j)
        {
            forAll(windVectors[i][j], k)
            {

                // If clockwise rotating, this vector points along the blade toward the tip.
                // If counter-clockwise rotating, this vector points along the blade toward the root.
                if (rotationDir[i] == "cw")
                {
                    bladeAlignedVectors[i][j][k][2] =   bladePoints[i][j][k] - rotorApex[i];
                    bladeAlignedVectors[i][j][k][2] =   bladeAlignedVectors[i][j][k][2]/mag(bladeAlignedVectors[i][j][k][2]);
                }
                else if (rotationDir[i] == "ccw")
                {
                    bladeAlignedVectors[i][j][k][2] = -(bladePoints[i][j][k] - rotorApex[i]);
                    bladeAlignedVectors[i][j][k][2] =   bladeAlignedVectors[i][j][k][2]/mag(bladeAlignedVectors[i][j][k][2]);
                }

                // This vector points in the tangential direction opposite the turbines rotation type.  It is
                // set up this way because it will point in the direction of oncoming flow that the blade sees
                // due to rotation.
                bladeAlignedVectors[i][j][k][1] = bladeAlignedVectors[i][j][k][2]^uvShaft[i];
                bladeAlignedVectors[i][j][k][1] = bladeAlignedVectors[i][j][k][1]/mag(bladeAlignedVectors[i][j][k][1]);

                // This vector points normal to the other two and toward downwind (not exactly downwind if
                // the blade is coned).  It points in the direction of the oncoming flow due to wind that the
                // blade sees.
                bladeAlignedVectors[i][j][k][0] = bladeAlignedVectors[i][j][k][1]^bladeAlignedVectors[i][j][k][2];
                bladeAlignedVectors[i][j][k][0] = bladeAlignedVectors[i][j][k][0]/mag(bladeAlignedVectors[i][j][k][0]);


                vector windVectorsInt = windVectors[i][j][k];

                // Zero the wind vector.
                windVectors[i][j][k] = vector::zero;

                // Now put the velocity in that cell into blade-oriented coordinates and add on the
                // velocity due to blade rotation.
                windVectors[i][j][k].x() = (bladeAlignedVectors[i][j][k][0] & windVectorsInt);
                windVectors[i][j][k].y() = (bladeAlignedVectors[i][j][k][1] & windVectorsInt) + (rotSpeed[i] * bladeRadius[i][j] * cos(PreCone[n][0]));
                windVectors[i][j][k].z() = (bladeAlignedVectors[i][j][k][2] & windVectorsInt);
            }
        }
    }



    // Proceed turbine by turbine.
    forAll(windVectors, i)
    {
        int m = turbineTypeID[i];

        // Set the total thrust of the turbine to zero.  Thrust will be summed on a blade-element-
        // wise basis.
        thrust[i] = 0.0;

        // Set the total aerodynamic torque of the turbine to zero.  Thrust will be summed on a blade-element-
        // wise basis.
        torqueRotor[i] = 0.0;

        // Proceed blade by blade.
        forAll(windVectors[i], j)
        {

            // Proceed point by point.
            forAll(windVectors[i][j], k)
            {
                // Interpolate the local twist angle.
                scalar twistAng = interpolate(bladeRadius[i][j], BladeStation[m], BladeTwist[m]);

                // Interpolate the local chord.
                scalar chord = interpolate(bladeRadius[i][j], BladeStation[m], BladeChord[m]);

                // Find the local airfoil type.
                label airfoil = interpolate(bladeRadius[i][j], BladeStation[m], BladeAirfoilTypeID[m]);
                label maxIndex = BladeAirfoilTypeID[m].size() - 1;
                airfoil = min(max(0,airfoil),maxIndex);

                // Find the local velocity magnitude compose of only the axial and tangential flow (do
                // not include the radial (along blade span) flow).
                Vmag[i][j][k] = Foam::pow((Foam::pow(windVectors[i][j][k].x(),2) + Foam::pow(windVectors[i][j][k].y(),2)),0.5);

                // Get the angle of the wind with respect to rotor plane tangent direction.
                scalar windAng = Foam::atan2(windVectors[i][j][k].x(),windVectors[i][j][k].y())/degRad; 

                // Angle of attack is local angle of wind with respect to rotor plane tangent minus local twist.
                alpha[i][j][k] = windAng - twistAng - pitch[i];

                // Use airfoil look-up tables to get coefficient of lift and drag.
                Cl[i][j][k] = interpolate(alpha[i][j][k], airfoilAlpha[airfoil], airfoilCl[airfoil]);
                Cd[i][j][k] = interpolate(alpha[i][j][k], airfoilAlpha[airfoil], airfoilCd[airfoil]);

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

                    scalar ftip  = (TipRad[m] - bladeRadius[i][j])/(bladeRadius[i][j] * sin(mag(windAng)*degRad));
                    scalar Ftip  = (2.0/(Foam::constant::mathematical::pi)) * acos(min(1.0, exp(-g * (NumBl[m] / 2.0) * ftip)));

                    scalar froot = (bladeRadius[i][j] - HubRad[m])/(bladeRadius[i][j] * sin(mag(windAng)*degRad));
                    scalar Froot = (2.0/(Foam::constant::mathematical::pi)) * acos(min(1.0, exp(-g * (NumBl[m] / 2.0) * froot)));

                    F = Ftip * Froot;
                }

                // Using Cl, Cd, wind velocity, chord, and actuator element width, calculate the
                // lift and drag per density.
                //lift[i][j][k] = 0.5 * F * Cl[i][j][k] * Vmag[i][j][k] * Vmag[i][j][k] * chord * dr[i][k];
                //drag[i][j][k] = 0.5 * F * Cd[i][j][k] * Vmag[i][j][k] * Vmag[i][j][k] * chord * dr[i][k];
                Cl[i][j][k] *= F * forceScalar[i];
                Cd[i][j][k] *= F * forceScalar[i];
                lift[i][j][k] = 0.5 * Cl[i][j][k] * Vmag[i][j][k] * Vmag[i][j][k] * chord * dr[i][j];
                drag[i][j][k] = 0.5 * Cd[i][j][k] * Vmag[i][j][k] * Vmag[i][j][k] * chord * dr[i][j];

                // Make the scalar lift and drag quantities vectors in the Cartesian coordinate system.
                vector dragVector = bladeAlignedVectors[i][j][k][0]*windVectors[i][j][k].x() + bladeAlignedVectors[i][j][k][1]*windVectors[i][j][k].y();
                dragVector = dragVector/mag(dragVector);

                vector liftVector = dragVector^bladeAlignedVectors[i][j][k][2];
                liftVector = liftVector/mag(liftVector);

                liftVector = -lift[i][j][k] * liftVector;
                dragVector = -drag[i][j][k] * dragVector;

                // Add up lift and drag to get the resultant force/density applied to this blade element.
                bladeForce[i][j][k] = (liftVector + dragVector);

                // Find the component of the blade element force/density in the axial (along the shaft)
                // direction.
                axialForce[i][j][k] = -bladeForce[i][j][k] & uvShaft[i];

                // Find the component of the blade element force/density in the tangential (torque-creating)
                // direction.
                tangentialForce[i][j][k] = bladeForce[i][j][k] & bladeAlignedVectors[i][j][k][1];

                // Add this blade element's contribution to thrust to the total turbine thrust.
                thrust[i] += axialForce[i][j][k] * solidity[i][j];

                // Add this blade element's contribution to aerodynamic torque to the total turbine aerodynamic torque.
                torqueRotor[i] += tangentialForce[i][j][k] * solidity[i][j] * bladeRadius[i][j] * cos(PreCone[m][0]);
            }
        }

        // Compute rotor power based on aerodynamic torque and rotation speed.
        powerRotor[i] = torqueRotor[i] * rotSpeed[i];

        // Compute the generator power.
        powerGenerator[i] = torqueGen[i] * (rotSpeed[i] * GBRatio[m]) * GenEfficiency[m];
    }
}


void horizontalAxisWindTurbinesADM::computeBodyForce()
{  
    // Zero out the body force to begin with.
    bodyForce *= 0.0;

    // Proceed turbine by turbine.
    scalar thrustSum = 0.0;
    scalar torqueSum = 0.0;
    scalar thrustBodyForceSum = 0.0;
    scalar torqueBodyForceSum = 0.0;

    forAll(bladeForce, i)
    {
        
        int n = turbineTypeID[i];

        // Proceed to compute body forces for turbine i only if there are sphere cells on this processor for this turbine.
        if (sphereCells[i].size() > 0)
        {
            // For each blade.
            forAll(bladeForce[i], j)
            {
                // For each blade point.
                forAll(bladeForce[i][j], k)
                {
                    // For each sphere cell.
                    forAll(sphereCells[i], m)
                    {
                        scalar dis = mag(mesh_.C()[sphereCells[i][m]] - bladePoints[i][j][k]);
                        if (dis <= projectionRadius[i])
                        {
                            bodyForce[sphereCells[i][m]] += bladeForce[i][j][k] * solidity[i][j] * (Foam::exp(-Foam::sqr(dis/epsilon[i]))/(Foam::pow(epsilon[i],3)*Foam::pow(Foam::constant::mathematical::pi,1.5)));
                            thrustBodyForceSum += (-bladeForce[i][j][k] * solidity[i][j] * (Foam::exp(-Foam::sqr(dis/epsilon[i]))/(Foam::pow(epsilon[i],3)*Foam::pow(Foam::constant::mathematical::pi,1.5))) *
                                                    mesh_.V()[sphereCells[i][m]]) & uvShaft[i];
                            torqueBodyForceSum += ( bladeForce[i][j][k] * solidity[i][j] * (Foam::exp(-Foam::sqr(dis/epsilon[i]))/(Foam::pow(epsilon[i],3)*Foam::pow(Foam::constant::mathematical::pi,1.5))) * 
                                                    bladeRadius[i][j] * cos(PreCone[n][0]) * mesh_.V()[sphereCells[i][m]]) & bladeAlignedVectors[i][j][k][1];
                        }
                    }
                }  
            }
        }
        thrustSum += thrust[i];
        torqueSum += torqueRotor[i];
    }
    reduce(thrustBodyForceSum,sumOp<scalar>());
    reduce(torqueBodyForceSum,sumOp<scalar>());

    // Print information comparing the actual thrust and torque to the integrated body force.
    Info << "Thrust from Body Force = " << thrustBodyForceSum << tab << "Thrust from Act. Disk = " << thrustSum << tab << "Ratio = " << thrustBodyForceSum/thrustSum << endl;
    Info << "Torque from Body Force = " << torqueBodyForceSum << tab << "Torque from Act. Disk = " << torqueSum << tab << "Ratio = " << torqueBodyForceSum/torqueSum << endl;
}


void horizontalAxisWindTurbinesADM::computeSectorAverage()
{
    forAll(alphaSecAvg, i)
    {
        forAll(alphaSecAvg[i], j)
        {
            forAll(alphaSecAvg[i][j], k)
            {
                windVectorsSecAvg[i][j][k] = vector::zero;
                alphaSecAvg[i][j][k] = 0.0;
                VmagSecAvg[i][j][k] = 0.0;
                ClSecAvg[i][j][k] = 0.0;
                CdSecAvg[i][j][k] = 0.0;
                liftSecAvg[i][j][k] = 0.0;
                dragSecAvg[i][j][k] = 0.0;
                axialForceSecAvg[i][j][k] = 0.0;
                tangentialForceSecAvg[i][j][k] = 0.0;

                forAll(sectorIndices[i][k], m)
                {
                    if (sectorIndices[i][k][m] == j)
                    {
                        windVectorsSecAvg[i][j][k] += windVectors[i][k][m] / (scalar(nAzimuth[i][k])/scalar(nAvgSector[i]));
                        alphaSecAvg[i][j][k] += alpha[i][k][m] / (scalar(nAzimuth[i][k])/scalar(nAvgSector[i]));
                        VmagSecAvg[i][j][k] += Vmag[i][k][m] / (scalar(nAzimuth[i][k])/scalar(nAvgSector[i]));
                        ClSecAvg[i][j][k] += Cl[i][k][m] / (scalar(nAzimuth[i][k])/scalar(nAvgSector[i]));
                        CdSecAvg[i][j][k] += Cd[i][k][m] / (scalar(nAzimuth[i][k])/scalar(nAvgSector[i]));
                        liftSecAvg[i][j][k] += lift[i][k][m] / (scalar(nAzimuth[i][k])/scalar(nAvgSector[i]));
                        dragSecAvg[i][j][k] += drag[i][k][m] / (scalar(nAzimuth[i][k])/scalar(nAvgSector[i]));
                        axialForceSecAvg[i][j][k] += axialForce[i][k][m] / (scalar(nAzimuth[i][k])/scalar(nAvgSector[i]));
                        tangentialForceSecAvg[i][j][k] += tangentialForce[i][k][m] / (scalar(nAzimuth[i][k])/scalar(nAvgSector[i]));
                    }
                }
            }
        }
    }
}


vector horizontalAxisWindTurbinesADM::rotatePoint(vector point, vector rotationPoint, vector axis, scalar angle)
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
    point = point - rotationPoint;

    // Perform the rotation.
    point = RM & point;

    // Return the rotated point to its new location relative to the rotation point.
    point = point + rotationPoint;

    return point;
}


scalar horizontalAxisWindTurbinesADM::interpolate(scalar xNew, DynamicList<scalar>& xOld, DynamicList<scalar>& yOld)
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


label horizontalAxisWindTurbinesADM::interpolate(scalar xNew, DynamicList<scalar>& xOld, DynamicList<label>& yOld)
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

scalar horizontalAxisWindTurbinesADM::compassToStandard(scalar dir)
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

scalar horizontalAxisWindTurbinesADM::standardToCompass(scalar dir)
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
    
void horizontalAxisWindTurbinesADM::update()
{
    // Update the time step size.
    dt = runTime_.deltaT().value();

    // Update the current simulation time.
    time = runTime_.timeName();
    t = runTime_.value();

    if(bladeUpdateType[0] == "oldPosition")
    {
        // Find out which processor controls which actuator point,
        // and with that informatio sample the wind at the actuator
        // points.
      //findControlProcNo();
        computeWindVectors();

        // Update the rotor state.
        filterRotSpeed();
        controlGenTorque();
        controlBladePitch();
        controlNacYaw();
        computeRotSpeed();
        rotateBlades();
        yawNacelle();
    }
    else if(bladeUpdateType[0] == "newPosition")
    {
        // Update the rotor state.
        filterRotSpeed();
        controlGenTorque();
        controlBladePitch();
        controlNacYaw();
        computeRotSpeed();
        rotateBlades();
        yawNacelle();

        // Find out which processor controls which actuator point,
        // and with that information sample the wind at the actuator
        // points.
      //findControlProcNo();
        computeWindVectors();
    }

    // Compute the blade forces.
    computeBladeForce();

    // Project the blade forces as body forces.
    computeBodyForce();

    // Print turbine output to file.
        outputIndex++;

        if (outputControl == "timeStep")
        {
            if (outputIndex >= outputInterval)
    	    {
	        outputIndex = 0;
                computeSectorAverage();
	        printOutputFiles();
	    }
        }
        else if (outputControl == "runTime")
        {
            if ((runTime_.value() - lastOutputTime) >= outputInterval)
            {
    	        lastOutputTime += outputInterval;
                computeSectorAverage();
	        printOutputFiles();
            }
        }
        else
        {
            computeSectorAverage();
            printOutputFiles();
        }

    // Now that at least the first time step is finished, set pastFirstTimeStep
    // to true.
    pastFirstTimeStep = true;
}


void horizontalAxisWindTurbinesADM::openOutputFiles()
{
    if (Pstream::master())
    {
        // Create the name of the root of where turbine files get ouput.
        fileName rootDir;

        if (Pstream::parRun())
        {
            rootDir = runTime_.path()/"../turbineOutput";
        }
        else
        {
            rootDir = runTime_.path()/"turbineOutput";
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



        // Create a total aerodynamic torque file.
        //torqueRotorFile(rootDir/time/"torqueRotor");
        torqueRotorFile_ = new OFstream(rootDir/time/"torqueRotor");
        *torqueRotorFile_ << "#Turbine    Time(s)    dt(s)    rotor torque (N-m)" << endl;

        // Create a generator torque file.
        torqueGenFile_ = new OFstream(rootDir/time/"torqueGen");
        *torqueGenFile_ << "#Turbine    Time(s)    dt(s)    generator torque (N-m)" << endl;

        // Create a total thrust file.
        thrustFile_ = new OFstream(rootDir/time/"thrust");
        *thrustFile_ << "#Turbine    Time(s)    dt(s)    thrust (N)" << endl;

        // Create an aerodynamic power file.
        powerRotorFile_ = new OFstream(rootDir/time/"powerRotor");
        *powerRotorFile_ << "#Turbine    Time(s)    dt(s)    rotor power (W)" << endl;

        // Create a total power file.
        powerGeneratorFile_ = new OFstream(rootDir/time/"powerGenerator");
        *powerGeneratorFile_ << "#Turbine    Time(s)    dt(s)    rotor generator (W)" << endl;

        // Create a rotation rate file.
        rotSpeedFile_ = new OFstream(rootDir/time/"rotSpeed");
        *rotSpeedFile_ << "#Turbine    Time(s)    dt(s)    rotor rotation rate(rpm)" << endl;
        
        // Create a filtered rotation rate file.
        rotSpeedFFile_ = new OFstream(rootDir/time/"rotSpeedFiltered");
        *rotSpeedFFile_ << "#Turbine    Time(s)    dt(s)    filtered rotor rotation rate(rpm)" << endl;

        // Create a blade 1 azimuth angle file.
        azimuthFile_ = new OFstream(rootDir/time/"azimuth");
        *azimuthFile_ << "#Turbine    Time(s)    dt(s)    blade 1 azimuth angle (degrees)" << endl;

        // Create a blade pitch angle file.
        pitchFile_ = new OFstream(rootDir/time/"pitch");
        *pitchFile_ << "#Turbine    Time(s)    dt(s)    blade pitch angle (degrees)" << endl;

        // Create a nacelle yaw direction file.
        nacYawFile_ = new OFstream(rootDir/time/"nacYaw");
        *nacYawFile_ << "#Turbine    Time(s)    dt(s)    nacelle yaw angle (degrees)" << endl;

        // Create an angle of attack file.
        alphaFile_ = new OFstream(rootDir/time/"alpha");
        *alphaFile_ << "#Turbine    Sector    Time(s)    dt(s)    angle-of-attack(degrees)" << endl;

        // Create a wind speed magnitude file.
        VmagFile_ = new OFstream(rootDir/time/"Vmag");
        *VmagFile_ << "#Turbine    Sector    Time(s)    dt(s)    Vmag(m/s)" << endl;
    
        // Create an axial wind speed file.
        VaxialFile_ = new OFstream(rootDir/time/"Vaxial");
        *VaxialFile_ << "#Turbine    Sector    Time(s)    dt(s)    Vaxial(m/s)" << endl;

        // Create a tangential wind speed file.
        VtangentialFile_ = new OFstream(rootDir/time/"Vtangential");
        *VtangentialFile_ << "#Turbine    Sector    Time(s)    dt(s)    Vtangential(m/s)" << endl;

        // Create a radial wind speed file.
        VradialFile_ = new OFstream(rootDir/time/"Vradial");
        *VradialFile_ << "#Turbine    Sector    Time(s)    dt(s)    Vradial(m/s)" << endl;

        // Create a coefficient of lift file.
        ClFile_ = new OFstream(rootDir/time/"Cl");
        *ClFile_ << "#Turbine    Sector    Time(s)    dt(s)    Cl" << endl;

        // Create a coefficient of drag file.
        CdFile_ = new OFstream(rootDir/time/"Cd");
        *CdFile_ << "#Turbine    Sector    Time(s)    dt(s)    Cd" << endl;

        // Create a lift file.
        liftFile_ = new OFstream(rootDir/time/"lift");
        *liftFile_ << "#Turbine    Sector    Time(s)    dt(s)    lift (N)" << endl;

        // Create a drag file.
        dragFile_ = new OFstream(rootDir/time/"drag");
        *dragFile_ << "#Turbine    Sector    Time(s)    dt(s)    drag (N)" << endl;

        // Create a axial force file.
        axialForceFile_ = new OFstream(rootDir/time/"axialForce");
        *axialForceFile_ << "#Turbine    Sector    Time(s)    dt(s)    axial force (N)" << endl;

        // Create a tangential force file.
        tangentialForceFile_ = new OFstream(rootDir/time/"tangentialForce");
        *tangentialForceFile_ << "#Turbine    Sector    Time(s)    dt(s)    tangential force (N)" << endl;

    }
}


void horizontalAxisWindTurbinesADM::printOutputFiles()
{
    if (Pstream::master())
    {
        forAll(alphaSecAvg,i)
        {
            // Write out time and delta t.
            *torqueRotorFile_ << i << " " << time << " " << dt << " ";
            *torqueGenFile_ << i << " " << time << " " << dt << " ";
            *thrustFile_ << i << " " << time << " " << dt << " ";
            *powerRotorFile_ << i << " " << time << " " << dt << " ";
            *powerGeneratorFile_ << i << " " << time << " " << dt << " ";
            *rotSpeedFile_ << i << " " << time << " " << dt << " ";
            *rotSpeedFFile_ << i << " " << time << " " << dt << " ";
            *azimuthFile_ << i << " " << time << " " << dt << " ";
            *pitchFile_ << i << " " << time << " " << dt << " ";
            *nacYawFile_ << i << " " << time << " " << dt << " ";

            // Write out information for each turbine.
            *torqueRotorFile_ << torqueRotor[i]*fluidDensity[i] << endl;
            *torqueGenFile_ << torqueGen[i] << endl;
            *thrustFile_ << thrust[i]*fluidDensity[i] << endl;
            *powerRotorFile_ << powerRotor[i]*fluidDensity[i] << endl;
            *powerGeneratorFile_ << powerGenerator[i]*fluidDensity[i] << endl;
            *rotSpeedFile_ << rotSpeed[i]/rpmRadSec << endl;
            *rotSpeedFFile_ << rotSpeedF[i]/rpmRadSec << endl;
            *azimuthFile_ << azimuth[i]/degRad << endl;
            *pitchFile_ << pitch[i] << endl;
            *nacYawFile_ << standardToCompass(nacYaw[i]/degRad) << endl;

            // Proceed sector by sector.
            forAll(alphaSecAvg[i], j)
            {
                // Write out time and delta t.
                *alphaFile_ << i << " " << j << " " << time << " " << dt << " ";
                *VmagFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *VaxialFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *VtangentialFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *VradialFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *ClFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *CdFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *liftFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *dragFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *axialForceFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *tangentialForceFile_ << i << " " << j << " " <<  time << " " << dt << " ";

                forAll(alphaSecAvg[i][j], k)
                {   
                    *alphaFile_ << alphaSecAvg[i][j][k] << " ";
                    *VmagFile_ << VmagSecAvg[i][j][k] << " ";
                    *VaxialFile_ << windVectorsSecAvg[i][j][k].x() << " ";
                    *VtangentialFile_ << windVectorsSecAvg[i][j][k].y() << " ";
                    *VradialFile_ << windVectorsSecAvg[i][j][k].z() << " ";
                    *ClFile_ << ClSecAvg[i][j][k] << " ";
                    *CdFile_ << CdSecAvg[i][j][k] << " ";
                    *liftFile_ << liftSecAvg[i][j][k]*fluidDensity[i] << " ";
                    *dragFile_ << dragSecAvg[i][j][k]*fluidDensity[i] << " ";
                    *axialForceFile_ << axialForceSecAvg[i][j][k]*fluidDensity[i] << " ";
                    *tangentialForceFile_ << tangentialForceSecAvg[i][j][k]*fluidDensity[i] << " ";
                }
                *alphaFile_ << endl;
                *VmagFile_ << endl;
                *VaxialFile_ << endl;
                *VtangentialFile_ << endl;
                *VradialFile_ << endl;
                *ClFile_ << endl;
                *CdFile_ << endl;
                *liftFile_ << endl;
                *dragFile_ << endl;
                *axialForceFile_ << endl;
                *tangentialForceFile_ << endl;
            }
        }
          
        *torqueRotorFile_ << endl;
        *torqueGenFile_ << endl;
        *thrustFile_ << endl;
        *powerRotorFile_ << endl;
        *powerGeneratorFile_ << endl;
        *rotSpeedFile_ << endl;
        *rotSpeedFFile_ << endl;
        *azimuthFile_ << endl;
        *pitchFile_ << endl;
        *nacYawFile_ << endl;

        *alphaFile_ << endl;
        *VmagFile_ << endl;
        *VaxialFile_ << endl;
        *VtangentialFile_ << endl;
        *VradialFile_ << endl;
        *ClFile_ << endl;
        *CdFile_ << endl;
        *liftFile_ << endl;
        *dragFile_ << endl;
        *axialForceFile_ << endl;
        *tangentialForceFile_ << endl;
    }
}
   
     
void horizontalAxisWindTurbinesADM::printDebug()
{
    Info << "Print Debugging Information" << endl;
    Info << "turbineType = " << turbineType << endl;
    Info << "baseLocation = " << baseLocation << endl;
    Info << "nRadial = " << nRadial << endl;
    Info << "pointDistType = " << pointDistType << endl;
    Info << "epsilon = " << epsilon << endl;
    Info << "projectionRadius = " << projectionRadius << endl;
    Info << "azimuth = " << azimuth << endl;
    Info << "rotSpeed = " << rotSpeed << endl;
    Info << "pitch = " << pitch << endl;
    Info << "nacYaw = " << nacYaw << endl << endl << endl;
    
    Info << "numTurbinesDistinct = " << numTurbinesDistinct << endl;
    Info << "turbineTypeDistinct = " << turbineTypeDistinct << endl;
    Info << "turbineTypeID = " << turbineTypeID << endl << endl << endl;;

    Info << "NumBl = " << NumBl << endl;
    Info << "TipRad = " << TipRad << endl;
    Info << "HubRad = " << HubRad << endl;
    Info << "UndSling = " << UndSling << endl;
    Info << "OverHang = " << OverHang << endl;
    Info << "TowerHt = " << TowerHt << endl;
    Info << "Twr2Shft = " << Twr2Shft << endl;
    Info << "ShftTilt = " << ShftTilt << endl;
    Info << "PreCone = " << PreCone << endl;
    Info << "GBRatio = " << GBRatio << endl;
    Info << "GBEfficiency = " << GBEfficiency << endl;
    Info << "GenEfficiency = " << GenEfficiency << endl;
    Info << "HubIner = " << HubIner << endl;
    Info << "GenIner = " << GenIner << endl;
    Info << "BladeIner = " << BladeIner << endl;
    Info << "GenTorqueControllerType = " << GenTorqueControllerType << endl;
    Info << "NacYawControllerType = " << NacYawControllerType << endl;
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

    Info << "sphereCells = " << sphereCells << endl << endl << endl;

    Info << "dr = " << dr << endl;
    Info << "bladePoints = " << bladePoints << endl;
    Info << "bladeRadius = " << bladeRadius << endl;
    Info << "towerShaftIntersect = " << towerShaftIntersect << endl;
    Info << "rotorApex = " << rotorApex << endl;
    Info << "uvShaft = " << uvShaft << endl;
    Info << "uvShaftDir = " << uvShaftDir << endl;
    Info << "uvTower = " << uvTower << endl;
    Info << "deltaNacYaw = " << deltaNacYaw << endl;
    Info << "deltaAzimuth = " << deltaAzimuth << endl;

    Info << "bladeForce = " << bladeForce << endl;
    Info << "windVectors = " << windVectors << endl;
    Info << "bladeAlignedVectors = " << bladeAlignedVectors << endl;
}


volVectorField& horizontalAxisWindTurbinesADM::force()
{
    // Return the body force field to the solver
    return bodyForce;
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace turbineModels
} // End namespace Foam

// ************************************************************************* //

