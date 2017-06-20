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

#include "horizontalAxisWindTurbinesALMOpenFAST.H"
#include "interpolateXY.H"

namespace Foam
{
namespace turbineModels
{

// * * * * * * * * * * * * * *  Constructor  * * * * * * * * * * * * * * * * //

horizontalAxisWindTurbinesALMOpenFAST::horizontalAxisWindTurbinesALMOpenFAST
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

    // Set the processor number
    p(Pstream::myProcNo()),

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
    ),

    FAST(new fast::OpenFAST)


{
    // Initialize the turbine array model.
    initialize();   
}

void horizontalAxisWindTurbinesALMOpenFAST::end()
{
    FAST->end();
}
// * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * * * //

void horizontalAxisWindTurbinesALMOpenFAST::initialize()
{
    // Read input files.
    readInput();

    // Send inputs to FAST.
    sendInput();

    // Initialize FAST.
    FAST->init();

    // Initialize arrays that hold blade, tower, nacelle information.
    initializeArrays();

    // Get all the blade, tower, nacelle positions from FAST.
    getPositions();

    // Get the chord lengths from FAST.
    getChordLengths();
    
    // Define the sets of search cells when sampling velocity and projecting
    // the body force.
    forAll(turbineName,i)
    {
        updateRotorSearchCells(i);
      //if (includeNacelle[i])
      //{
            updateNacelleSearchCells(i);
      //}
      //if (includeTower[i])
      //{
            updateTowerSearchCells(i);
      //}
    }

    // If there are search cells for a particular turbine, then this processor
    // must do calculations for this turbine, so check for that.
    updateTurbinesControlled();

    // Compute the blade aligned vectors.
    computeBladeAlignedVectors();

    // Compute the blade point radii away from main shaft axis.
    computeBladePointRadius();

    // Compute the radius from the main shaft axis of the CFD mesh cells.
    forAll(turbineName,i)
    {
        updateRadius(i);
    }

    // Find out which processors control each actuator line point.
    updateBladePointControlProcNo();
    updateNacellePointControlProcNo();
    updateTowerPointControlProcNo();

    // Sample the wind vectors at this initial time step.
    sampleBladePointWindVectors();
    sampleNacellePointWindVectors();
    sampleTowerPointWindVectors();

    // Get the wind vector in geometry aligned coordinates.
    computeBladeAlignedVelocity();

    // Send the sampled velocities out to FAST.
    sendVelocities();

    // Set individual turbine inputs to and initialize FAST.
    if ((p < numTurbines) && (! FAST->isDryRun()))
    {
        if (FAST->isTimeZero())
        {
            FAST->solution0();
        }
    }

    // Get the positions out after solution0 and step are called.
    getPositions();

    // Set the rotorApex and mainShaftOrientation that was before
    // the latest search.
    rotorApexBeforeSearch = 1.0*rotorApex;
    mainShaftOrientationBeforeSearch = 1.0*mainShaftOrientation;

    // Open the turbine data output files and print initial information.
    openOutputFiles();
    printOutputFiles();
}

void horizontalAxisWindTurbinesALMOpenFAST::readInput()
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
        nFASTSubSteps.append(int(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("nFASTSubSteps"))));

        includeNacelle.append(readBool(turbineArrayProperties.subDict(turbineName[i]).lookup("includeNacelle")));
        includeTower.append(readBool(turbineArrayProperties.subDict(turbineName[i]).lookup("includeTower")));

        baseLocation.append(vector(turbineArrayProperties.subDict(turbineName[i]).lookup("baseLocation")));

        numBladePoints.append(int(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("numBladePoints"))));
        numNacellePoints.append(int(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("numNacellePoints"))));
        numTowerPoints.append(int(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("numTowerPoints"))));

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
        nacelleEquivalentRadius.append(Foam::sqrt(nacelleFrontalArea[i]/Foam::constant::mathematical::pi));

        tipRootLossCorrType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("tipRootLossCorrType")));

        velocityDragCorrType.append(word(turbineArrayProperties.subDict(turbineName[i]).lookup("velocityDragCorrType")));

        fluidDensity.append(scalar(readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("fluidDensity"))));

        if(includeNacelle[i])   //  if any of the nacelles are active, set this global boolean to true.
        {
            includeNacelleSomeTrue = true;
        }
      //else   // set the number of nacelle points to 1, if the nacelle is not active.
      //{
      //    numNacellePoints[i] = 1;
      //}


        if(includeTower[i])   //  if any of the nacelles are active, set this global boolean to true.
        {
            includeTowerSomeTrue = true;
        }
      //else   // set the number of tower points to 1, if the tower is not active.
      //{
      //    numTowerPoints[i] = 1;
      //}

        if((nacelleForceProjectionType[i] == "advanced1") || (nacelleForceProjectionType[i] == "advanced2")) // for the advanced nacelle force projection, only 1 nacelle point can be handled
        {
            numNacellePoints[i] = 1;
        }
    }
}

 
void horizontalAxisWindTurbinesALMOpenFAST::sendInput()
{
  
  fi.comm = MPI_COMM_WORLD;
  fi.nTurbinesGlob = numTurbines;
  fi.dryRun = dryRun ;
  fi.debug = true;
  fi.tStart = tStart - timeSimulationStart;
  // True restart capability is not an option
  if (fi.tStart > 0) {
    fi.simStart = fast::restartDriverInitFAST;
  } else {
    fi.simStart = fast::init;
  }
  Pout << "nFASTSubSteps has to be the same for all turbines" << endl ;
  fi.dtFAST = dt/nFASTSubSteps[0]; 
  fi.nEveryCheckPoint = checkPointInterval ;
  fi.tMax = timeSimulationEnd-timeSimulationStart ; 
  fi.numScInputs = numSuperControllerOutputs ;
  fi.numScOutputs = numSuperControllerInputs ;
  
  fi.globTurbineData.resize(fi.nTurbinesGlob);
  for (int iTurb=0; iTurb < fi.nTurbinesGlob; iTurb++) {
    std::vector<double> baseLoc(3);
    baseLoc[0] = baseLocation[iTurb].x();
    baseLoc[1] = baseLocation[iTurb].y();
    baseLoc[2] = baseLocation[iTurb].z();

    std::vector<double> hubLoc(3);
    hubLoc[0] = baseLoc[0];
    hubLoc[1] = baseLoc[1];
    hubLoc[2] = baseLoc[2] + 90.0;
    
    fi.globTurbineData[iTurb].TurbID = iTurb;
    fi.globTurbineData[iTurb].FASTInputFileName = FASTInputFileName[iTurb];
    fi.globTurbineData[iTurb].FASTRestartFileName = FASTRestartFileName[iTurb];
    fi.globTurbineData[iTurb].TurbineBasePos = baseLoc ;
    fi.globTurbineData[iTurb].TurbineHubPos = hubLoc ;
    fi.globTurbineData[iTurb].numForcePtsBlade = numBladePoints[iTurb];
    fi.globTurbineData[iTurb].numForcePtsTwr = numTowerPoints[iTurb];    
  }

  FAST->setInputs(fi);

  FAST->allocateTurbinesToProcsSimple();
}


void horizontalAxisWindTurbinesALMOpenFAST::initializeArrays()
{
    // Get the number of blades, blade points, tower points, and shaft vectors all in order and onto all cores. 
    forAll(turbineName,i)
    {
       numBl.append(0);
       numBladeSamplePoints.append(0);
       numTowerSamplePoints.append(0);
       mainShaftOrientation.append(vector::zero);
       mainShaftOrientationBeforeSearch.append(vector::zero);
       rotorApex.append(vector::zero);
       rotorApexBeforeSearch.append(vector::zero);
    }
 
    getNumBlades();
    getNumBladePoints();
    getNumTowerPoints();



    // Create the actuator line points (not yet rotated for initial nacelle
    // yaw or initial rotor azimuth), the actuator tower points, and the
    // actuator nacelle points. i-index is at array level, j-index is
    // for the type of turbine, k-index is for each blade, and m-index is
    // for each actuator point.  Also create other important vectors, and
    // initialize the forces, blade aligned coordinate system, and
    // wind vectors to zero.
    totBladePoints = 0;
    totBladeSamplePoints = 0;
    totNacellePoints = 0;
    totTowerPoints = 0;
    totTowerSamplePoints = 0;
    
    Random rndGen(123456);

    for(int i = 0; i < numTurbines; i++)
    {
        // Blade points.
        bladeDs.append(DynamicList<scalar>(0));
        for(int m = 0; m < numBladePoints[i]; m++)
        {
            bladeDs[i].append(1.0/numBladePoints[i]);
        }
        totBladePoints += numBladePoints[i];
        totBladeSamplePoints += numBladeSamplePoints[i]*numBl[i];
        bladePoints.append(List<List<vector> >(numBl[i], List<vector>(numBladePoints[i],vector::zero)));
        bladeSamplePoints.append(List<List<vector> >(numBl[i], List<vector>(numBladeSamplePoints[i],vector::zero)));
        bladePointRadius.append(List<List<scalar> >(numBl[i], List<scalar>(numBladePoints[i],0.0)));
        bladeSamplePointRadius.append(List<List<scalar> >(numBl[i], List<scalar>(numBladeSamplePoints[i],0.0)));
        bladePointOrientation.append(List<List<tensor> >(numBl[i], List<tensor>(numBladePoints[i],tensor::zero)));
        bladePointChord.append(List<List<scalar> >(numBl[i], List<scalar>(numBladePoints[i],0.0)));




        // Tower points.
        towerDs.append(DynamicList<scalar>(0));
        for(int m = 0; m < numTowerPoints[i]; m++)
        {
            towerDs[i].append(1.0/numTowerPoints[i]);
        }
        totTowerPoints += numTowerPoints[i];
        totTowerSamplePoints += numTowerSamplePoints[i];
        towerPoints.append(List<vector>(numTowerPoints[i],vector::zero));
        towerSamplePoints.append(List<vector>(numTowerSamplePoints[i],vector::zero));
        towerPointHeight.append(List<scalar>(numTowerPoints[i],0.0));
        towerPointOrientation.append(List<tensor>(numTowerPoints[i],tensor::zero));
        towerPointChord.append(List<scalar>(numTowerPoints[i],0.0));




        // Nacelle points.
        nacelleDs.append(DynamicList<scalar>(0));
        for(int m = 0; m < numNacellePoints[i]; m++)
        {
            nacelleDs[i].append(1.0/numNacellePoints[i]);
        }
        totNacellePoints += numNacellePoints[i];
        nacellePoints.append(List<vector>(numNacellePoints[i],vector::zero));
        nacelleSamplePoint.append(vector::zero);






        // Generate random numbers for the point perturbation during control
        // processor identification.  This does not affect the actual location--it is
        // just there to break ties and make sure > 1 processors don't account for a
        // single actuator point.
        bladePointsPerturbVector.append(List<List<vector> >(numBl[i], List<vector>(numBladePoints[i],vector::zero)));
        if (p == 0)
        {
            for (int k =  0; k < numBl[i]; k++)
            {
                for (int m = 0; m < numBladePoints[i]; m++)
                {
                    bladePointsPerturbVector[i][k][m] = perturb*(2.0*rndGen.vector01()-vector::one); 
                }
            }
        }

        towerPointsPerturbVector.append(List<vector>(numTowerPoints[i],vector::zero));
        if (p == 0)
        {
            for (int m = 0; m < numTowerPoints[i]; m++)
            {
                towerPointsPerturbVector[i][m] = perturb*(2.0*rndGen.vector01()-vector::one); 
            }
        }

        nacellePointPerturbVector.append(vector::zero);
        if (p == 0)
        {
            nacellePointPerturbVector[i] = perturb*(2.0*rndGen.vector01()-vector::one);
        }

        
        // Define the size of the blade-aligned vectors and set to zero.
        bladeAlignedVectors.append(List<List<List<vector> > >(numBl[i], List<List<vector> >(numBladePoints[i],List<vector>(3,vector::zero))));
        bladeAlignedVectorsSample.append(List<List<List<vector> > >(numBl[i], List<List<vector> >(numBladeSamplePoints[i],List<vector>(3,vector::zero))));

        // Define the size of the blade, nacelle, and tower force arrays and set to zero.
        bladePointForce.append(List<List<vector> >(numBl[i], List<vector>(numBladePoints[i],vector::zero)));
        towerPointForce.append(List<vector>(numTowerPoints[i],vector::zero));
        nacellePointForce.append(List<vector>(numNacellePoints[i],vector::zero));
  
        // Define the actuator element wind vector arrays and set them to zero.
        bladeWindVectorsCartesian.append(List<List<vector> >(numBl[i],List<vector>(numBladeSamplePoints[i],vector::zero)));
        bladeWindVectors.append(List<List<vector> >(numBl[i],List<vector>(numBladeSamplePoints[i],vector::zero)));
        towerWindVectors.append(List<vector>(numTowerSamplePoints[i],vector::zero));
        nacelleWindVector.append(vector::zero);

        // Define the size of the deltaNacYaw, deltaAzimuth, and deltaPitch lists and set to zero.
        deltaNacYaw.append(0.0);
        deltaAzimuth.append(0.0);

        // Define the size of the angle of attack lists and set to zero.
        bladePointAlpha.append(List<List<scalar> >(numBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointAlpha.append(List<scalar>(numTowerPoints[i],0.0));

        // Define the size of the wind speed magnitude lists and set to zero.
        bladePointVmag.append(List<List<scalar> >(numBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointVmag.append(List<scalar>(numTowerPoints[i],0.0));
        nacellePointVmag.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the coefficient of bladePointLift lists and set to zero.
        bladePointCl.append(List<List<scalar> >(numBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointCl.append(List<scalar>(numTowerPoints[i],0.0));

        // Define the size of the coefficient of drag lists and set to zero.
        bladePointCd.append(List<List<scalar> >(numBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointCd.append(List<scalar>(numTowerPoints[i],0.0));
        nacellePointCd.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the bladePointLift lists and set to zero.
        bladePointLift.append(List<List<scalar> >(numBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointLift.append(List<scalar>(numTowerPoints[i],0.0));

        // Define the size of the drag lists and set to zero.
        bladePointDrag.append(List<List<scalar> >(numBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointDrag.append(List<scalar>(numTowerPoints[i],0.0));
        nacellePointDrag.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the axial force lists and set to zero.
        bladePointAxialForce.append(List<List<scalar> >(numBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointAxialForce.append(List<scalar>(numTowerPoints[i],0.0));
        nacellePointAxialForce.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the horizontal force lists and set to zero.
        bladePointHorizontalForce.append(List<List<scalar> >(numBl[i], List<scalar>(numBladePoints[i],0.0)));
        towerPointHorizontalForce.append(List<scalar>(numTowerPoints[i],0.0));
        nacellePointHorizontalForce.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the horizontal force lists and set to zero.
        bladePointVerticalForce.append(List<List<scalar> >(numBl[i], List<scalar>(numBladePoints[i],0.0)));
        nacellePointVerticalForce.append(List<scalar>(numNacellePoints[i],0.0));

        // Define the size of the torque lists and set to zero.
        bladePointTorque.append(List<List<scalar> >(numBl[i], List<scalar>(numBladePoints[i],0.0)));

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
        bladeMinDisCellID.append(List<List<label> >(numBl[i], List<label>(numBladePoints[i],-1)));
        nacelleMinDisCellID.append(-1);
        towerMinDisCellID.append(List<label>(numTowerPoints[i],-1));

        DynamicList<label> influenceCellsI;
        bladeInfluenceCells.append(influenceCellsI);
        nacelleInfluenceCells.append(influenceCellsI);
        towerInfluenceCells.append(influenceCellsI);

        bladeProjectionRadius.append(0.0);
        nacelleProjectionRadius.append(0.0);
        towerProjectionRadius.append(0.0);

        rotorSpeed.append(0.0);
    }

    Pstream::scatter(bladePointsPerturbVector);
    Pstream::scatter(nacellePointPerturbVector);
    Pstream::scatter(towerPointsPerturbVector);
}


void horizontalAxisWindTurbinesALMOpenFAST::updateRotorSearchCells(int turbineNumber)
{
    // Define the cells that can possibly be influenced by the force
    // exerted each turbine by the rotor.  In otherwords, define a set 
    // of cell IDs around each turbine that will be saved into memory 
    // so that the entire domain need not be passed through when  
    // applying the force field.  (The i-index is at the turbine array  
    // level for each turbine and the j-index is at the individual blade level.)
    int i = turbineNumber;


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
    forAll (bladePointChord[i],j)
    {
        forAll (bladePointChord[i][j],k)
        {
           if(bladePointChord[i][j][k] > bladeChordMax)
           {
               bladeChordMax = bladePointChord[i][j][k];
           }
        }
    }

/*
    scalar bladeUserDefMax = -1.0E6;
    forAll (BladeUserDef[m],j)
    {
        if(BladeUserDef[m][j] > bladeUserDefMax)
        {
            bladeUserDefMax = BladeUserDef[m][j];
        }
    }
*/

    if ((bladeForceProjectionType[i] == "uniformGaussian") ||
        (bladeForceProjectionType[i] == "generalizedGaussian") ||
        (bladeForceProjectionType[i] == "generalizedGaussian2D"))
    {
        bladeProjectionRadius[i] = bladeEpsilonMax * Foam::sqrt(Foam::log(1.0/0.001));
    }

/*
    else if (bladeForceProjectionType[i] == "variableUniformGaussianUserDef")
    {
        bladeProjectionRadius[i] = bladeUserDefMax * bladeEpsilonMax * Foam::sqrt(Foam::log(1.0/0.001));
    }
*/
    else if ((bladeForceProjectionType[i] == "variableUniformGaussianChord") ||
             (bladeForceProjectionType[i] == "chordThicknessGaussian") ||
             (bladeForceProjectionType[i] == "chordThicknessGaussian2D"))
    {
        bladeProjectionRadius[i] = bladeChordMax * bladeEpsilonMax * Foam::sqrt(Foam::log(1.0/0.001));
    }

    // Get a measure of blade precone angle, blade tip radius, and distance from tower top to blade tip.
    List<scalar> preCone(numBl[i],0.0);
    List<scalar> tipRadius(numBl[i],0.0);
    List<scalar> towerTopToTip(numBl[i],0.0);
    scalar preConeMax = 0.0;
    scalar tipRadiusMax = 0.0;
    scalar towerTopToTipMax = 0.0;


    forAll(preCone,j)
    {
        vector r = bladePoints[i][j][1] - bladePoints[i][j][0];
        r /= mag(r);
 
        vector shaft = mainShaftOrientation[i];

        scalar rDotShaft = r & shaft;

        scalar rMag = Foam::mag(r);
        scalar shaftMag = Foam::mag(shaft);

        scalar angleCos = rDotShaft / (rMag * shaftMag);

        preCone[j] = 0.5*Foam::constant::mathematical::pi - acos(angleCos);

        vector bladeLength = bladePoints[i][j][numBladePoints[i]-1] - rotorApex[i];

        vector towerTopToBladeTip = bladePoints[i][j][numBladePoints[i]-1] - towerPoints[i][numTowerPoints[i]-1];

        tipRadius[j] = Foam::mag(bladeLength);
        
        towerTopToTip[j] = Foam::mag(towerTopToBladeTip);

        if (mag(preCone[j]) > mag(preConeMax))
        {
           preConeMax = preCone[j];
        }

        if (tipRadius[j] > tipRadiusMax)
        {
           tipRadiusMax = tipRadius[j];
        }

        if (towerTopToTip[j] > towerTopToTipMax)
        {
           towerTopToTipMax = towerTopToTip[j];
        }

        /*
        Info << "r = " << r << endl;
        Info << "shaft = " << shaft << endl;
        Info << "preCone[" << j << "] = " << preCone[j] / degRad << endl;
        Info << "mag(preCone) = " << mag(preCone[j]) << endl;
        Info << "rotorApex = " << rotorApex[i] << endl;
        Info << "bladePoints[" << i << "][" << j << "][" << numBladePoints[i] << "] = " << bladePoints[i][j][numBladePoints[i]-1] << endl;
        Info << "preConeMax = " << preConeMax / degRad << endl;
        Info << "tipRadiusMax = " << tipRadiusMax << endl;
        Info << "towerTopToTipMax = " << towerTopToTipMax << endl;
        */
    }


        
    // Defines the search cell set a all cells within a sphere that occupies
    // the volume that the rotor lies in for any yaw angle.  This is a fairly
    // large volume, so possibly inefficient for fine meshes.  See the "disk"
    // option below.
    DynamicList<label> influenceCellsI;
    if (bladeSearchCellMethod[i] == "sphere")
    {
        scalar sphereRadius = towerTopToTipMax + bladeProjectionRadius[i];

        // Find the cells within the region of influence.
        forAll(U_.mesh().cells(),cellI)
        {
            if (mag(U_.mesh().C()[cellI] - towerPoints[i][numTowerPoints[i]-1]) <= sphereRadius)
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
            scalar xBladeMin = 0.0;
            scalar xBladeMax = 0.0;
            v = U_.mesh().C()[cellI] - rotorApex[i];
            zP.z() = 1.0;
            xP = mainShaftOrientation[i];
            xP /= mag(xP);
            yP = zP ^ xP;
            yP /= mag(yP);
            zP = xP ^ yP;
            zP /= mag(zP);
            vP = transformVectorCartToLocal(v,xP,yP,zP);
            r = Foam::sqrt(Foam::sqr(vP.y()) + Foam::sqr(vP.z()));
            xBladeMin = -r * Foam::sin(preConeMax);
            xBladeMax =  r * Foam::sin(preConeMax);
            if (((vP.x() >= xBladeMin - bladeProjectionRadius[i]) && (vP.x() <= xBladeMax + bladeProjectionRadius[i])) &&
                (r <= tipRadiusMax + bladeProjectionRadius[i]))
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


void horizontalAxisWindTurbinesALMOpenFAST::updateNacelleSearchCells(int turbineNumber)
{
    // Define the cells that can possibly be influenced by the force
    // exerted each turbine by the nacelle.  In otherwords, define a set 
    // of cell IDs around each turbine that will be saved into memory 
    // so that the entire domain need not be passed through when  
    // applying the force field.  (The i-index is at the turbine array  
    // level for each turbine and the j-index is at the individual blade level.)
    int i = turbineNumber;


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
    nacelleProjectionRadius[i] = nacelleEpsilonMax * Foam::sqrt(Foam::log(1.0/0.001)) + nacelleEquivalentRadius[i];


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
            xP = mainShaftOrientation[i];
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
            else if (vP.x() > nacelleLength[i])
            {
                r1 = Foam::sqrt(Foam::sqr(vP.x() - nacelleLength[i]) + Foam::sqr(vP.y()) + Foam::sqr(vP.z()));
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



void horizontalAxisWindTurbinesALMOpenFAST::updateTowerSearchCells(int turbineNumber)
{
    // Define the cells that can possibly be influenced by the force
    // exerted each turbine by the tower.  In otherwords, define a set 
    // of cell IDs around each turbine that will be saved into memory 
    // so that the entire domain need not be passed through when  
    // applying the force field.  (The i-index is at the turbine array  
    // level for each turbine  and the j-index is at the individual blade level.)
    int i = turbineNumber;

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
    forAll (towerPointChord[i],j)
    {
        if (towerPointChord[i][j] > towerChordMax)
        {
            towerChordMax = towerPointChord[i][j];
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
            if (U_.mesh().C()[cellI].z() <= towerPoints[i][numTowerPoints[i]-1].z())
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


void horizontalAxisWindTurbinesALMOpenFAST::updateTurbinesControlled()
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

    /*
    Pout << "bladesControlled: " << bladesControlled << endl;
    Pout << "nacellesControlled: " << nacellesControlled << endl;
    Pout << "towersControlled: " << towersControlled << endl;
    */
}


void horizontalAxisWindTurbinesALMOpenFAST::updateBladePointControlProcNo()
{
    // Create a local and global list of minimum distance cells to velocity sampling 
    // points of turbines that this processor controls.  Initialize the values to huge.
    List<scalar> minDisLocalBlade(totBladeSamplePoints,1.0E30);
    List<scalar> minDisGlobalBlade(totBladeSamplePoints,1.0E30);


    forAll(bladesControlled, p)
    {
        int i = bladesControlled[p];
        int iterBlade = 0;
        if(i > 0)
        {
            for(int n = 0; n < i; n++)
            {
                iterBlade += numBladeSamplePoints[n] * numBl[n];
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
                iterBlade += numBladeSamplePoints[n] * numBl[n];
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


void horizontalAxisWindTurbinesALMOpenFAST::updateNacellePointControlProcNo()
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



void horizontalAxisWindTurbinesALMOpenFAST::updateTowerPointControlProcNo()
{
    // Create a local and global list of minimum distance cells to velocity sampling 
    // points of turbines that this processor controls.  Initialize the values to huge.
    List<scalar> minDisLocalTower(totTowerSamplePoints,1.0E30);
    List<scalar> minDisGlobalTower(totTowerSamplePoints,1.0E30);


    forAll(towersControlled, p)
    {
        int i = towersControlled[p];
        int iterTower = 0;
        if(i > 0)
        {
            for(int n = 0; n < i; n++)
            {
                iterTower += numTowerSamplePoints[n];
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
                iterTower += numTowerSamplePoints[n];
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




void horizontalAxisWindTurbinesALMOpenFAST::updateRadius(int turbineNumber)
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
        xP = mainShaftOrientation[i];
        xP /= mag(xP);
        yP = zP ^ xP;
        yP /= mag(yP);
        zP = xP ^ yP;
        zP /= mag(zP);
        vP = transformVectorCartToLocal(v,xP,yP,zP);
        rFromShaft[cellI] = Foam::sqrt(Foam::sqr(vP.y()) + Foam::sqr(vP.z()));
    }
}


void horizontalAxisWindTurbinesALMOpenFAST::getPositions()
{
   // Local point location vector of doubles for communication with FAST.
   std::vector<double> pointLocation(3,0.0);

   // Local point orientation vector of doubles for communication with FAST.
   std::vector<double> pointOrientation(9,0.0);

   // Local main shaft unit vector vector of doubles for communication with FAST.
   std::vector<double> shaftOrientation(3,0.0);
 
   // Get the total number of velocity sampling points for this processor's turbine.
   int localNumSamplePoints = 0;
   if (p < numTurbines)
   {
       localNumSamplePoints = (numBl[p] * numBladeSamplePoints[p]) + numTowerSamplePoints[p] + 1;
   }
  
   // Get the total number of force points for this processor's turbine.
   int localNumPoints = 0;
   if (p < numTurbines)
   {
       localNumPoints = (numBl[p] * numBladePoints[p]) + numTowerPoints[p] + 1;
   }

   // Get the total number of velocity sampling points for all turbines.
   int totalNumSamplePoints = 0;
   forAll(numBl,i)
   {
       totalNumSamplePoints += (numBl[i] * numBladeSamplePoints[i]) + numTowerSamplePoints[i] + 1;
   }

   // Get the total number of force points for all turbines.
   int totalNumPoints = 0;
   forAll(numBl,i)
   {
       totalNumPoints += (numBl[i] * numBladePoints[i]) + numTowerPoints[i] + 1;
   }
 
   // Create a local velocity sampling point list that is initially zero..
   List<vector> samplePoints_(totalNumSamplePoints,vector::zero);
   
   // Create a local force point list that is initially zero..
   List<vector> points_(totalNumPoints,vector::zero);

   // Create a local orientation list that is initially zero..
   List<tensor> orientation_(totalNumPoints,tensor::zero);

   int startIndex = 0;





   // Get the main shaft axis updated.
   for(int i = 0; i < numTurbines; i++)
   {
      // Zero the orientation vector unless i corresponds to the
      // turbine that this processor controls.
      mainShaftOrientation[i] = vector::zero;
      if (p == i) 
      {
	 FAST->getHubShftDir(shaftOrientation, i);
         mainShaftOrientation[i].x() = shaftOrientation[0];
         mainShaftOrientation[i].y() = shaftOrientation[1];
         mainShaftOrientation[i].z() = shaftOrientation[2];
      }
   }

   // Parallel sum the list and send back out to all cores.
   Pstream::gather(mainShaftOrientation,sumOp<List<vector> >());
   Pstream::scatter(mainShaftOrientation);





   // Get the velocity sampling points all updated and ordered nicely.
   // Find the start index in that list that belongs to this processor.
   startIndex = 0;
   if (p < numTurbines)
   {
      for(int i = 0; i < p; i++)
      {
         startIndex += (numBl[i] * numBladeSamplePoints[i]) + numTowerSamplePoints[i] + 1;
      } 
      
      // Call FAST to populate this processor's part of the FAST point list.
      for (int i = 0; i < localNumSamplePoints; i++)
      {
	 FAST->getVelNodeCoordinates(pointLocation,i,p);
         samplePoints_[startIndex + i].x() = pointLocation[0];
         samplePoints_[startIndex + i].y() = pointLocation[1];
         samplePoints_[startIndex + i].z() = pointLocation[2];     
      }
   }

   // Parallel sum the list and send back out to all cores.
   Pstream::gather(samplePoints_,sumOp<List<vector> >());
   Pstream::scatter(samplePoints_);

   // Put the local points vector entries into the nice ordered
   // list of points.
   startIndex = 0;
   forAll(bladeSamplePoints,i)
   {
       int m = 0;

       // Get rotor apex.
       rotorApex[i] = samplePoints_[startIndex];
       m++;

       // Get the nacelle sample point that is some distance upstream
       // of the rotor apex.
       nacelleSamplePoint[i] = rotorApex[i] - (nacelleSampleDistance[i] * mainShaftOrientation[i]);

       // Compute the nacelle force points.
       forAll(nacellePoints[i],j)
       {
           if (j == 0)
           {
              nacellePoints[i][j] = rotorApex[i];
           }
           else
           {
              nacellePoints[i][j] = nacellePoints[i][j-1] + (nacelleDs[i][j] * mainShaftOrientation[i]);
           }
       }

       // Get blade velocity sampling points.
       forAll(bladeSamplePoints[i],j)
       {
           forAll(bladeSamplePoints[i][j],k)
           {
                bladeSamplePoints[i][j][k] = samplePoints_[startIndex + m];
                m++;
           }
       }

       // Get tower velocity sampling points (and add on the sampling distance to sample upstream
       // of the tower.
       forAll(towerSamplePoints[i],j)
       {
           towerSamplePoints[i][j] = samplePoints_[startIndex + m] - (towerSampleDistance[i] * mainShaftOrientation[i]);
           m++;
       }

       startIndex += (numBl[i] * numBladeSamplePoints[i]) + numTowerSamplePoints[i] + 1;
   }






   // Get the force points all updated and ordered nicely.
   // Find the start index in that list that belongs to this processor.
   startIndex = 0;
   if (p < numTurbines)
   {
      for(int i = 0; i < p; i++)
      {
         startIndex += (numBl[i] * numBladePoints[i]) + numTowerPoints[i] + 1;
      }

      // Call FAST to populate this processor's part of the FAST point list.
      for (int i = 0; i < localNumPoints; i++)
      {
	  FAST->getForceNodeCoordinates(pointLocation,i,p);
          points_[startIndex + i].x() = pointLocation[0];
          points_[startIndex + i].y() = pointLocation[1];
          points_[startIndex + i].z() = pointLocation[2];
       }

       // Call FAST to populate this processor's part of the orientation list.
       for (int i = 0; i < localNumPoints; i++)
       {
       	  FAST->getForceNodeOrientation(pointOrientation,i,p);
          orientation_[startIndex + i].xx() = pointOrientation[0];
          orientation_[startIndex + i].xy() = pointOrientation[1];
          orientation_[startIndex + i].xz() = pointOrientation[2];
          orientation_[startIndex + i].yx() = pointOrientation[3];
          orientation_[startIndex + i].yy() = pointOrientation[4];
          orientation_[startIndex + i].yz() = pointOrientation[5];
          orientation_[startIndex + i].zx() = pointOrientation[6];
          orientation_[startIndex + i].zy() = pointOrientation[7];
          orientation_[startIndex + i].zz() = pointOrientation[8];
       }
   }

   // Parallel sum the list and send back out to all cores.
   Pstream::gather(points_,sumOp<List<vector> >());
   Pstream::scatter(points_);
   Pstream::gather(orientation_,sumOp<List<tensor> >());
   Pstream::scatter(orientation_);

   // Put the local points vector entries into the nice ordered
   // list of points.
   startIndex = 0;
   forAll(bladePoints,i)
   {
       int m = 1;

       // To derive rotor speed, grab the blade 1 first point.
       vector bladePoint1Old = bladePoints[i][1][1] - rotorApex[i];
       bladePoint1Old /= mag(bladePoint1Old);
      
       // Get blade force points.
       forAll(bladePoints[i],j)
       {
           forAll(bladePoints[i][j],k)
           {
                bladePoints[i][j][k] = points_[startIndex + m];
                bladePointOrientation[i][j][k] = orientation_[startIndex + m];
                m++;
           }
       }

       // Get tower force points.
       forAll(towerPoints[i],j)
       {
           towerPoints[i][j] = points_[startIndex + m];
           towerPointOrientation[i][j] = orientation_[startIndex + m];
           m++;
       }

       startIndex += (numBl[i] * numBladePoints[i]) + numTowerPoints[i] + 1;

       // Derive the rotor speed
       vector bladePoint1 = bladePoints[i][1][1] - rotorApex[i];
       bladePoint1 /= mag(bladePoint1);

     //Info << "bladePoint1 = " << bladePoint1 << endl;
     //Info << "bladePoint1Old = " << bladePoint1Old << endl;

       scalar deltaAzimuth = Foam::acos((bladePoint1 & bladePoint1Old) / (mag(bladePoint1) * mag(bladePoint1Old)));

     //Info << "deltaAzimuth = " << deltaAzimuth / degRad << endl;

     //Info << "dt = " << dt << endl;

       rotorSpeed[i] = deltaAzimuth / dt;

     //Info << "rotorSpeed = " << rotorSpeed << tab << rotorSpeed / rpmRadSec << endl;
   }

   /*
   Info << "mainShaftOrientation: " << mainShaftOrientation << endl;
   Info << "rotorApex = " << rotorApex << endl;
   Info << "nacelleSamplePoint = " << nacelleSamplePoint << endl;
   Info << "nacellePoints = " << nacellePoints << endl;
   Info << "bladeSamplePoints = " << bladeSamplePoints << endl;
   Info << "bladePoints = " << bladePoints << endl;
   Info << "towerSamplePoints = " << towerSamplePoints << endl;
   Info << "towerPoints = " << towerPoints << endl;
   Info << "bladePointOrientation = " << bladePointOrientation << endl;
   */
}


void horizontalAxisWindTurbinesALMOpenFAST::sendVelocities()
{
   // Local point vector vector of doubles for communication with FAST.
   std::vector< double> pointVelocity(3);

   // Only send velocities from the turbine number that matches the 
   // processor number.
   int i = p;

   if (p < numTurbines)
   {
        // Send the velocity information over to FAST starting with the nacelle, cycling through
        // the blade points, and then to the tower points.
        // - nacelle
        int m = 0;
        pointVelocity[0] = nacelleWindVector[i].x();
        pointVelocity[1] = nacelleWindVector[i].y();
        pointVelocity[2] = nacelleWindVector[i].z();
 
        FAST->setVelocity(pointVelocity, m, i);

        m++;

        // - blades
        forAll(bladeWindVectorsCartesian[i], j)
        {
            forAll(bladeWindVectorsCartesian[i][j],k)
            {
                 pointVelocity[0] = bladeWindVectorsCartesian[i][j][k].x();
                 pointVelocity[1] = bladeWindVectorsCartesian[i][j][k].y();
                 pointVelocity[2] = bladeWindVectorsCartesian[i][j][k].z();

                 FAST->setVelocity(pointVelocity, m, i);

                 m++;
            }
        }

        // - tower
        forAll(towerWindVectors[i],j)
        {
            pointVelocity[0] = towerWindVectors[i][j].x();
            pointVelocity[1] = towerWindVectors[i][j].y();
            pointVelocity[2] = towerWindVectors[i][j].z();

            FAST->setVelocity(pointVelocity, m, i);

            m++;
        }
    }
}



void horizontalAxisWindTurbinesALMOpenFAST::getForces()
{
   // Local point force vector of doubles for communication with FAST.
   std::vector<double> pointForce(3);

   // Get the total number of force points for this processor's turbine.
   int localNumPoints = 0;
   if (p < numTurbines)
   {
       localNumPoints = (numBl[p] * numBladePoints[p]) + numTowerPoints[p] + 1;
   }
   

   // Get the total number of force points for all turbines.
   int totalNumPoints = 0;
   forAll(numBl,i)
   {
       totalNumPoints += (numBl[i] * numBladePoints[i]) + numTowerPoints[i] + 1;
   }
 
   // Create a local point force list that is initially zero..
   List<vector> forces_(totalNumPoints,vector::zero);

   int startIndex = 0;




   // Get the point forcess all updated and ordered nicely.
   // Find the start index in that list that belongs to this processor.
   startIndex = 0;
   if (p < numTurbines)
   {
       for(int i = 0; i < p; i++)
       {
           startIndex += (numBl[i] * numBladePoints[i]) + numTowerPoints[i] + 1;
       }

       // Call FAST to populate this processor's part of the FAST point list.
       for (int i = 0; i < localNumPoints; i++)
       {
	 FAST->getForce(pointForce, i, i);
           forces_[startIndex + i].x() = pointForce[0];
           forces_[startIndex + i].y() = pointForce[1];
           forces_[startIndex + i].z() = pointForce[2];
       }
     //Pout << forces_ << endl;
   }

   // Parallel sum the list and send back out to all cores.
   Pstream::gather(forces_,sumOp<List<vector> >());
   Pstream::scatter(forces_);

   // Put the local force vector entries into the nice ordered
   // list of forces.
   startIndex = 0;
   forAll(bladePointForce,i)
   {
       int m = 0;

       scalar nacelleVmag = Foam::mag(nacelleWindVector[i]);
     //Info << "nacelleVmag = " << nacelleVmag << endl;
       vector nacelleForceTotal = 0.5 * nacelleVmag * nacelleFrontalArea[i] * nacelleCd[i] * nacelleWindVector[i];
     //Info << "nacelleForceTotal = " << nacelleForceTotal << endl;

       forAll(nacellePointForce[i],j)
       {
          nacellePointForce[i][j] = -nacelleForceTotal / numNacellePoints[i];
       }

       m++;

       // Get blade force points.
       forAll(bladePointForce[i],j)
       {
           forAll(bladePointForce[i][j],k)
           {
               bladePointForce[i][j][k] = forces_[startIndex + m];
             //vector a = vector::zero;
             //a.x() = 1.0;
             //bladePointForce[i][j][k] = a;
               m++;
           }
       }

       // Get tower force points.
       forAll(towerPointForce[i],j)
       {
           towerPointForce[i][j] = forces_[startIndex + m];
           m++;
       }

       startIndex += (numBl[i] * numBladePoints[i]) + numTowerPoints[i] + 1;
   }

   /*
   Info << "nacellePointForce " << nacellePointForce << endl;
   Info << "bladePointForce = " << bladePointForce << endl;
   Info << "towerPointForce = " << towerPointForce << endl;
   */

   // Get the axial and torque component of forces.
   forAll(bladePointForce,i)
   {
       vector axialVector = mainShaftOrientation[i];
       axialVector.z() = 0.0;
       axialVector = axialVector / mag(axialVector);

       rotorAxialForce[i] = 0.0;
       rotorTorque[i] = 0.0;
       forAll(bladePointForce[i],j)
       {
           forAll(bladePointForce[i][j],k)
           {
        //     rotorAxialForce[i] += -bladePointForce[i][j][k] & bladeAlignedVectors[i][j][k][0];
               rotorAxialForce[i] += -bladePointForce[i][j][k] & axialVector;
               rotorTorque[i] += (bladePointForce[i][j][k] * bladePointRadius[i][j][k]) & bladeAlignedVectors[i][j][k][1];
           }
       }

       rotorPower[i] = rotorSpeed[i] * rotorTorque[i];

       towerAxialForce[i] = 0.0;
       forAll(towerPointForce[i],j)
       {
           towerAxialForce[i] += -towerPointForce[i][j] & axialVector ;    
       }

       nacelleAxialForce[i] = 0.0;
       forAll(nacellePointForce[i],j)
       {
           nacelleAxialForce[i] += -nacellePointForce[i][j] & axialVector;
       }
   }
 //Info << "nacellePointForce = " << nacellePointForce << endl;
}



void horizontalAxisWindTurbinesALMOpenFAST::getNumBlades()
{
   // Zero the list.
   forAll(numBl,i)
   {
      numBl[i] = 0;
   }
   
   // Get the number of blades of the turbine that this processor's instance
   // of FAST controls.
   if (p < numTurbines)
   {
      numBl[p] = FAST->get_numBlades(0);
   }

   // Parallel sum and scatter out the list so that all processors know.
   Pstream::gather(numBl,sumOp<List<int> >());
   Pstream::scatter(numBl);
}


void horizontalAxisWindTurbinesALMOpenFAST::getNumBladePoints()
{
   // Zero the list.
   forAll(numBladeSamplePoints,i)
   {
      numBladeSamplePoints[i] = 0;
      numBladePoints[i] = 0;
   }

   // Get the number of FAST points/blade of the turbine that this processor's
   // instance of FAST controls.
   if (p < numTurbines)
   {
      numBladeSamplePoints[p] = FAST->get_numVelPtsBlade(0);
      numBladePoints[p] = FAST->get_numForcePtsBlade(0);
   }

   // Parallel sum and scatter out the list so that all processors know.
   Pstream::gather(numBladeSamplePoints,sumOp<List<int> >());
   Pstream::scatter(numBladeSamplePoints);
   Pstream::gather(numBladePoints,sumOp<List<int> >());
   Pstream::scatter(numBladePoints);
}


void horizontalAxisWindTurbinesALMOpenFAST::getNumTowerPoints()
{
   // Zero the list.
   forAll(numTowerSamplePoints,i)
   {
      numTowerSamplePoints[i] = 0;
      numTowerPoints[i] = 0;
   }

   // Get the number of FAST points/tower of the turbine that this processor's
   // instance of FAST controls.
   if (p < numTurbines)
   {
      numTowerSamplePoints[p] = FAST->get_numVelPtsTwr(0);
      numTowerPoints[p] = FAST->get_numForcePtsTwr(0);
   }

   // Parallel sum and scatter out the list so that all processors know.
   Pstream::gather(numTowerSamplePoints,sumOp<List<int> >());
   Pstream::scatter(numTowerSamplePoints);
   Pstream::gather(numTowerPoints,sumOp<List<int> >());
   Pstream::scatter(numTowerPoints);
}


void horizontalAxisWindTurbinesALMOpenFAST::getChordLengths()
{
   // Get the total number of force points for this processor's turbine.
   int localNumPoints = 0;
   if (p < numTurbines)
   {
       localNumPoints = (numBl[p] * numBladePoints[p]) + numTowerPoints[p] + 1;
   }

   // Get the total number of force points for all turbines.
   int totalNumPoints = 0;
   forAll(numBl,i)
   {
       totalNumPoints += (numBl[i] * numBladePoints[i]) + numTowerPoints[i] + 1;
   }

   // Create a local force point list that is initially zero..
   List<scalar> chord_(totalNumPoints,0.0);


   // Get the force  points all updated and ordered nicely.
   // Find the start index in that list that belongs to this processor.
   int startIndex = 0;
   if (p < numTurbines)
   {
       for(int i = 0; i < p; i++)
       {
           startIndex += (numBl[i] * numBladePoints[i]) + numTowerPoints[i] + 1;
       }

       // Call FAST to populate this processor's part of the FAST chord list.
       for (int i = 0; i < localNumPoints; i++)
       {
	 chord_[startIndex + i] = FAST->getChord(i, p);
       }
   }

   // Parallel sum the list and send back out to all cores.
   Pstream::gather(chord_,sumOp<List<scalar> >());
   Pstream::scatter(chord_);


   // Put the local chord vector entries into the nice ordered
   // list of chords.
   startIndex = 0;
   forAll(bladePointChord,i)
   {
       int m = 1;

       // Get blade force point chords.
       forAll(bladePointChord[i],j)
       {
           forAll(bladePointChord[i][j],k)
           {
                bladePointChord[i][j][k] = chord_[startIndex + m];
                m++;
           }
       }

       // Get tower force points chrods.
       forAll(towerPointChord[i],j)
       {
           towerPointChord[i][j] = chord_[startIndex + m];
           m++;
       }

       startIndex += (numBl[i] * numBladePoints[i]) + numTowerPoints[i] + 1;
   }

   /*
   Info << "bladePointChord = " << bladePointChord << endl;
   Info << "towerPointChord = " << towerPointChord << endl;
   */
}


void horizontalAxisWindTurbinesALMOpenFAST::computeBladeAlignedVectors()
{
    forAll(bladePoints,i)
    {
        forAll(bladePoints[i],j)
        {
            forAll(bladePoints[i][j],k)
            {
                // This vector points along the actuator line element.
                if (k == 0)
                {
                    bladeAlignedVectors[i][j][k][2] = bladePoints[i][j][0] - rotorApex[i];
                }
                else
                {
                    bladeAlignedVectors[i][j][k][2] = bladePoints[i][j][k] - bladePoints[i][j][k-1];
                }
                bladeAlignedVectors[i][j][k][2] = bladeAlignedVectors[i][j][k][2]/mag(bladeAlignedVectors[i][j][k][2]);
      
                // This vector points in the tangential direction opposite the turbines rotation type.  It is
                // set up this way because it will point in the direction of oncoming flow that the blade sees
                // due to rotation.
                bladeAlignedVectors[i][j][k][1] = bladeAlignedVectors[i][j][k][2]^mainShaftOrientation[i];
                bladeAlignedVectors[i][j][k][1] = bladeAlignedVectors[i][j][k][1]/mag(bladeAlignedVectors[i][j][k][1]);

                // This vector points normal to the other two and toward downwind (not exactly downwind if
                // the blade is coned).  It points in the direction of the oncoming flow due to wind that the
                // blade sees.
                bladeAlignedVectors[i][j][k][0] = bladeAlignedVectors[i][j][k][1]^bladeAlignedVectors[i][j][k][2];
                bladeAlignedVectors[i][j][k][0] = bladeAlignedVectors[i][j][k][0]/mag(bladeAlignedVectors[i][j][k][0]);
            }
        }
    }
  //Info << "bladeAlignedVector = " <<  bladeAlignedVectors << endl;

    forAll(bladeSamplePoints,i)
    {
        forAll(bladeSamplePoints[i],j)
        {
            forAll(bladeSamplePoints[i][j],k)
            {
                // This vector points along the actuator line element.
                if (k == 0)
                {
                    bladeAlignedVectorsSample[i][j][k][2] = bladeSamplePoints[i][j][0] - rotorApex[i];
                }
                else
                {
                    bladeAlignedVectorsSample[i][j][k][2] = bladeSamplePoints[i][j][k] - bladeSamplePoints[i][j][k-1];
                }
                bladeAlignedVectorsSample[i][j][k][2] = bladeAlignedVectorsSample[i][j][k][2]/mag(bladeAlignedVectorsSample[i][j][k][2]);

                // This vector points in the tangential direction opposite the turbines rotation type.  It is
                // set up this way because it will point in the direction of oncoming flow that the blade sees
                // due to rotation.
                bladeAlignedVectorsSample[i][j][k][1] = bladeAlignedVectorsSample[i][j][k][2]^mainShaftOrientation[i];
                bladeAlignedVectorsSample[i][j][k][1] = bladeAlignedVectorsSample[i][j][k][1]/mag(bladeAlignedVectors[i][j][k][1]);

                // This vector points normal to the other two and toward downwind (not exactly downwind if
                // the blade is coned).  It points in the direction of the oncoming flow due to wind that the
                // blade sees.
                bladeAlignedVectorsSample[i][j][k][0] = bladeAlignedVectorsSample[i][j][k][1]^bladeAlignedVectorsSample[i][j][k][2];
                bladeAlignedVectorsSample[i][j][k][0] = bladeAlignedVectorsSample[i][j][k][0]/mag(bladeAlignedVectorsSample[i][j][k][0]);
            }
        }
    }
  //Info << "bladeAlignedVectorSample = " <<  bladeAlignedVectorsSample << endl;
}


void horizontalAxisWindTurbinesALMOpenFAST::sampleBladePointWindVectors()
{
    // Create a list of wind velocity in x, y, z coordinates for each blade sample point.
    List<vector> bladeWindVectorsLocal(totBladeSamplePoints,vector::zero);
    
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
                iterBlade += numBladeSamplePoints[n] * numBl[n];
            }
        }

        forAll(bladeSamplePoints[i], j)
        {
            forAll(bladeSamplePoints[i][j], k)
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
    forAll(bladeWindVectorsCartesian, i)
    {
        // Proceed blade by blade.
        forAll(bladeWindVectorsCartesian[i], j)
        { 
            // Proceed point by point.
            forAll(bladeWindVectorsCartesian[i][j], k)
            {
                // Zero the wind vector and put in the correct velocity.
                bladeWindVectorsCartesian[i][j][k] = vector::zero;
                bladeWindVectorsCartesian[i][j][k] = bladeWindVectorsLocal[iterBlade];

                iterBlade++;
            }
        }
    }

    /*
    Info << "bladeWindVectorsCartesian = " << bladeWindVectorsCartesian << endl;
    */
}


void horizontalAxisWindTurbinesALMOpenFAST::sampleTowerPointWindVectors()
{
    // Create a list of wind velocity in x, y, z coordinates for each tower sample point.
    List<vector> towerWindVectorsLocal(totTowerSamplePoints,vector::zero);
    
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
                iterTower += numTowerSamplePoints[n];
            }
        }  

        forAll(towerSamplePoints[i], j)
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

    /*
    Info << "towerWindVectors = " << towerWindVectors << endl;
    */
}



void horizontalAxisWindTurbinesALMOpenFAST::sampleNacellePointWindVectors()
{
    // Create a list of wind velocity in x, y, z coordinates for each nacelle sample point.
    List<vector> nacelleWindVectorLocal(numTurbines,vector::zero);
    
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

    /*
    Info << "nacelleWindVector = " <<  nacelleWindVector << endl;
    */
}



void horizontalAxisWindTurbinesALMOpenFAST::computeBladePointRadius()
{
    forAll(bladeSamplePoints, i)
    {
        // Proceed blade by blade.
        forAll(bladeSamplePoints[i], j)
        {
            forAll(bladeSamplePoints[i][j], k)
            {
                vector d = (bladeSamplePoints[i][j][k] - rotorApex[i]) - (((bladeSamplePoints[i][j][k] - rotorApex[i]) & (mainShaftOrientation[i])) * mainShaftOrientation[i]);
                bladeSamplePointRadius[i][j][k] = mag(d);
            }
        }
    }

    forAll(bladePoints, i)
    {
        // Proceed blade by blade.
        forAll(bladePoints[i], j)
        {
            forAll(bladePoints[i][j], k)
            {
                vector d = (bladePoints[i][j][k] - rotorApex[i]) - (((bladePoints[i][j][k] - rotorApex[i]) & (mainShaftOrientation[i])) * mainShaftOrientation[i]);
                bladePointRadius[i][j][k] = mag(d);
            }
        }
    }

  //Info << "bladePointRadius = " << bladePointRadius << endl;
  //Info << "bladeSamplePointRadius = " << bladeSamplePointRadius << endl;
}




void horizontalAxisWindTurbinesALMOpenFAST::computeBladeAlignedVelocity()
{
    // Take the x,y,z wind vectors and project them into the blade coordinate system.
    // Proceed turbine by turbine.
    forAll(bladeWindVectors, i)
    {
        // Proceed blade by blade.
        forAll(bladeWindVectors[i], j)
        {
            forAll(bladeWindVectors[i][j], k)
            {
                // Zero the wind vector.
                bladeWindVectors[i][j][k] = vector::zero;

                // Now put the velocity in that cell into blade-oriented coordinates and add on the
                // velocity due to blade rotation.
                bladeWindVectors[i][j][k].x() = (bladeAlignedVectors[i][j][k][0] & bladeWindVectorsCartesian[i][j][k]);
                bladeWindVectors[i][j][k].y() = (bladeAlignedVectors[i][j][k][1] & bladeWindVectorsCartesian[i][i][k]) + (rotorSpeed[i] * bladeSamplePointRadius[i][j][k]);
                bladeWindVectors[i][j][k].z() = (bladeAlignedVectors[i][j][k][2] & bladeWindVectorsCartesian[i][j][k]);
            }
        }
    }
}





void horizontalAxisWindTurbinesALMOpenFAST::updateBladeBodyForce(int turbineNumber)
{
    int i = turbineNumber;

    
    // Initialize variables that are integrated forces.
    scalar rotorAxialForceBodySum = 0.0;
    scalar rotorTorqueBodySum = 0.0;



    // Compute body force due to blades.
  //gBlade *= 0.0;
  //forAll(bladePointForce, i)
  //{
        // Proceed to compute body forces for turbine i only if there are influence cells on this processor for this turbine.
        if (bladeInfluenceCells[i].size() > 0)
        {

            // Get necessary axes.
            
            vector axialVector = mainShaftOrientation[i];
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
                            localVelocity += rFromShaft[bladeInfluenceCells[i][m]] * rotorSpeed[i] * bladeAlignedVectors[i][j][k][1];
                            Urel[bladeInfluenceCells[i][m]] = localVelocity;

                            // Compute the body force contribution.
                            if ((bladeForceProjectionDirection[i] == "localVelocityAligned") ||
                                (bladeForceProjectionDirection[i] == "localVelocityAlignedCorrected"))
                            {
                                /*                               
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
                                */
                            }
                            else if (bladeForceProjectionDirection[i] == "sampledVelocityAligned")
                            {
                                bodyForce[bladeInfluenceCells[i][m]] += bladePointForce[i][j][k] * spreading;
                            }
                            else
                            {
                                bodyForce[bladeInfluenceCells[i][m]] += bladePointForce[i][j][k] * spreading;
                            }
/*
                        }
                    }
                }
            } 

            // For each blade.
            forAll(bladePointForce[i], j)
            {
                // For each blade point.
                forAll(bladePointForce[i][j], k)
                {

                    forAll(bladeInfluenceCells[i], m)
                    {
                        vector disVector = (mesh_.C()[bladeInfluenceCells[i][m]] - bladePoints[i][j][k]);
                        scalar dis = mag(disVector);
                        if (dis <= bladeProjectionRadius[i])
                        {
*/      
                            // Compute global body-force-derived forces/moments for all force projection directions except
                            // the local-velocity-aligned method that is corrected.  We will do this after correction.
                            if (bladeForceProjectionDirection[i] != "localVelocityAlignedCorrected")
                            {
                             //rotorAxialForceBodySum += (-bodyForce[bladeInfluenceCells[i][m]] * mesh_.V()[bladeInfluenceCells[i][m]]) & axialVector;
                             //rotorTorqueBodySum += (bodyForce[bladeInfluenceCells[i][m]] * bladePointRadius[i][j][k] * mesh_.V()[bladeInfluenceCells[i][m]]) & bladeAlignedVectors[i][j][k][1];

                               rotorAxialForceBodySum += (-bladePointForce[i][j][k] * spreading * mesh_.V()[bladeInfluenceCells[i][m]]) & axialVector;
                               rotorTorqueBodySum += (bladePointForce[i][j][k] * spreading * bladePointRadius[i][j][k] * mesh_.V()[bladeInfluenceCells[i][m]])  & bladeAlignedVectors[i][j][k][1];
                            }
                        }
                    }

                    /*
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
                    */


                }  
            }
        }
        // Compute global actuator-element-force-derived forces/moments
      //rotorTorqueSum += rotorTorque[i];
  //}
    reduce(rotorAxialForceBodySum,sumOp<scalar>());
    reduce(rotorTorqueBodySum,sumOp<scalar>());


    // Print information comparing the actual rotor thrust and torque to the integrated body force.
    Info << "Turbine " << i << tab << "Rotor Axial Force from Body Force = " << rotorAxialForceBodySum << tab << "Rotor Axial Force from Actuator = " << rotorAxialForce[i] << tab  
         << "Ratio = " << rotorAxialForceBodySum/rotorAxialForce[i] << endl;
    Info << "Turbine " << i << tab << "Rotor Torque from Body Force = " << rotorTorqueBodySum << tab << "Rotor Torque from Actuator = " << rotorTorque[i] << tab 
         << "Ratio = " << rotorTorqueBodySum/max(rotorTorque[i],1.0E-5) << endl;
}



void horizontalAxisWindTurbinesALMOpenFAST::updateTowerBodyForce(int turbineNumber)
{
    int i = turbineNumber;

    // Initialize variables that are integrated forces.
    scalar towerAxialForceSum = 0.0;
    scalar towerAxialForceBodySum = 0.0;
    
    
    // Compute body force due to tower.
  //forAll(towerPointForce, i)
  //{
        if (includeTower[i])
        {
            forAll(towerPointForce[i], j)
            {

                // Get necessary axes.
                vector axialVector = mainShaftOrientation[i];
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
                          //scalar r = 0.5 * interpolate(towerPointHeight[i][j], TowerStation[n], TowerChord[n]);
                            scalar r = 0.5 * towerPointChord[i][j];
                            spreading = diskGaussian(towerEpsilon[i][0], towerEpsilon[i][1], verticalVector, r, d);
                        }
                        else if (towerForceProjectionType[i] == "ringGaussian" || towerForceProjectionType[i] == "advanced" )
                        {
                          //scalar r = 0.5 * interpolate(towerPointHeight[i][j], TowerStation[n], TowerChord[n]);
                            scalar r = 0.5 * towerPointChord[i][j];
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
      //towerAxialForceSum += towerAxialForce[i];
  //}
    reduce(towerAxialForceBodySum,sumOp<scalar>());

    // Print information comparing the actual tower thrust to the integrated body force.
    Info << "Turbine " << i << tab << "Tower Axial Force from BodyForce = " << towerAxialForceBodySum << tab << "Tower Axial Force from Actuator = " << towerAxialForce[i] << tab
         << "Ratio = " << towerAxialForceBodySum/max(towerAxialForce[i],1.0E-5) << endl;
}



void horizontalAxisWindTurbinesALMOpenFAST::updateNacelleBodyForce(int turbineNumber)
{
    int i = turbineNumber;

    // Initialize variables that are integrated forces.
    scalar nacelleAxialForceSum = 0.0;
    scalar nacelleAxialForceBodySum = 0.0;

    // Compute body force due to nacelle.
  //forAll(nacellePointForce, i)
  //{
        
        if (includeNacelle[i])
        {
            forAll(nacellePointForce[i], j)
            {

                // Get necessary axes.
                vector axialVector = mainShaftOrientation[i];
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
                            spreading = diskGaussian(nacelleEpsilon[i][0], nacelleEpsilon[i][1], axialVector, nacelleEquivalentRadius[i], d);
                        }
                        else if ((nacelleForceProjectionType[i] == "advanced1") || (nacelleForceProjectionType[i] == "advanced2"))
                        {
                            vector v = mesh_.C()[nacelleInfluenceCells[i][m]] - rotorApex[i];

                            zP = vector::zero;
                            zP.z() = 1.0;

                            xP = mainShaftOrientation[i];
                            xP /= mag(xP);

                            yP = zP ^ xP;
                            yP /= mag(yP);

                            zP = xP ^ yP;
                            zP /= mag(zP);

                            vP = transformVectorCartToLocal(v, xP, yP, zP);

                            if ((vP.x() > 0.0) && (vP.x() < nacelleLength[i]))
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
                            else if (vP.x() >= nacelleLength[i])
                            {
                                r = sqrt(sqr(vP.x() - nacelleLength[i]) + sqr(vP.y()) + sqr(vP.z()));
                                scalar h = sqrt(sqr(vP.y()) + sqr(vP.z()));
                                theta = atan2(h,-(vP.x() - nacelleLength[i]));
                            }

                            scalar L = nacelleLength[i];
                            scalar epsilonR = nacelleEpsilon[i][0];
                            scalar r0 = nacelleEquivalentRadius[i];

                            scalar coeff = 1.0 / ( L * pow(pi,1.5) * r0 * epsilonR + 
                                                   L * pi * sqr(epsilonR) * exp(-sqr(r0 / epsilonR)) + 
                                                   L * pow(pi,1.5) * r0 * epsilonR * erf(r0 / epsilonR) + 
                                                   2.0 * pow(pi,1.5) * epsilonR * (2.0*sqr(r0) + sqr(epsilonR)) * erf(r0 / epsilonR) +
                                                   2.0 * pi * sqr(epsilonR) * r0 * exp(-sqr(r0 / epsilonR)) );

                            spreading = gaussian1D(r, nacelleEquivalentRadius[i], nacelleEpsilon[i][0], coeff);
                        }
                        else
                        {
                            spreading = uniformGaussian3D(nacelleEpsilon[i][0], dis);
                        }



                        // This is the advanced nacelle body force projection that projects into a shell resembling a nacelle.
                        vector bodyForceContrib = vector::zero;
                        scalar forceBase = 0.0;

                        scalar x1 = (1.0/9.0) * nacelleLength[i];
                        scalar x2 = (8.0/9.0) * nacelleLength[i];
                        scalar theta1 = 125.0 * pi / 180.0;
                        scalar CpSide =  0.25;
                        scalar CpBack =  0.0;
                        scalar a1 = 16.0 * x1;
                        scalar a2 = 16.0 * (nacelleLength[i] - x2);
                        scalar a3 = 5.0;
                        scalar c = 1.5643;

                        if ((nacelleForceProjectionType[i] == "advanced1") || (nacelleForceProjectionType[i] == "advanced2"))
                        {
                            vector nacelleNormal = vector::zero;
                            if ((vP.x() > 0.0) && (vP.x() < nacelleLength[i]))
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
                                    forceBase = 0.5 * (CpSide - 1.25) - 0.5 * (1.25 + CpSide) * erf(a2 * (vP.x() - 0.5*(nacelleLength[i] + x2)));
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
                            else if (vP.x() >= nacelleLength[i])
                            {
                                nacelleNormal = vP;
                                nacelleNormal.x() -= nacelleLength[i];
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
   //     nacelleAxialForceSum += nacelleAxialForce[i];
  //}
    reduce(nacelleAxialForceBodySum,sumOp<scalar>());

    // Print information comparing the actual tower thrust to the integrated body force.
    Info << "Turbine " << i << tab << "Nacelle Axial Force from BodyForce = " << nacelleAxialForceBodySum << tab << "Nacelle Axial Force from Actuator = " << nacelleAxialForce[i] << tab
         << "Ratio = " << nacelleAxialForceBodySum/max(nacelleAxialForce[i],1E-5) << endl;
}



scalar horizontalAxisWindTurbinesALMOpenFAST::computeBladeProjectionFunction(vector disVector, int turbineNumber, int bladeNumber, int elementNumber)
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
    /*
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
    */
    else
    {
        spreading = uniformGaussian3D(bladeEpsilon[i][0], dis);
    }

    return spreading;
}


scalar horizontalAxisWindTurbinesALMOpenFAST::uniformGaussian3D(scalar epsilon, scalar d)
{
    // Compute the 3-dimensional Gaussian.
    scalar f = (1.0 / (Foam::pow(epsilon,3)*Foam::pow(Foam::constant::mathematical::pi,1.5))) * Foam::exp(-Foam::sqr(d/epsilon));
    return f;
}


scalar horizontalAxisWindTurbinesALMOpenFAST::generalizedGaussian3D(vector epsilon, vector d, vector dir0, vector dir1, vector dir2)
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

scalar horizontalAxisWindTurbinesALMOpenFAST::generalizedGaussian2D(vector epsilon, vector d, vector dir0, vector dir1)
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

scalar horizontalAxisWindTurbinesALMOpenFAST::diskGaussian(scalar rEpsilon, scalar xEpsilon, vector u, scalar r0, vector d)
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

scalar horizontalAxisWindTurbinesALMOpenFAST::ringGaussian(scalar rEpsilon, scalar xEpsilon, vector u, scalar r0, vector d)
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

scalar horizontalAxisWindTurbinesALMOpenFAST::gaussian1D(scalar x, scalar x0, scalar epsilon, scalar coeff)
{
    // Compute a 1D Gaussian function centered about x0 with width epsilon and scaled by coeff.
    scalar f = coeff * Foam::exp(-Foam::sqr((x - x0)/epsilon));

    return f;
}



vector horizontalAxisWindTurbinesALMOpenFAST::rotateVector(vector v, vector translation, vector axis, scalar angle)
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



vector horizontalAxisWindTurbinesALMOpenFAST::transformVectorCartToLocal(vector v, vector xP, vector yP, vector zP)
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




vector horizontalAxisWindTurbinesALMOpenFAST::transformVectorLocalToCart(vector vP, vector xP, vector yP, vector zP)
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




scalar horizontalAxisWindTurbinesALMOpenFAST::interpolate(scalar xNew, DynamicList<scalar>& xOld, DynamicList<scalar>& yOld)
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


label horizontalAxisWindTurbinesALMOpenFAST::interpolate(scalar xNew, DynamicList<scalar>& xOld, DynamicList<label>& yOld)
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

scalar horizontalAxisWindTurbinesALMOpenFAST::compassToStandard(scalar dir)
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

scalar horizontalAxisWindTurbinesALMOpenFAST::standardToCompass(scalar dir)
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
    
void horizontalAxisWindTurbinesALMOpenFAST::update()
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
        updateBladePointControlProcNo();
        updateNacellePointControlProcNo();
        updateTowerPointControlProcNo();

        sampleBladePointWindVectors();
        sampleNacellePointWindVectors();
        sampleTowerPointWindVectors();

        // Send the sampled velocities out to FAST.
        sendVelocities();

        // Compute the geometry aligned velocity.
        computeBladeAlignedVelocity();       

        // Update the rotor state.
      //filterRotSpeed();
      //controlGenTorque();
      //controlBladePitch();
      //controlNacYaw();
      //computeRotSpeed();
      //rotateBlades();
      //yawNacelle();

        // Update the turbine state.
        if (p < numTurbines)
        {
	  Pout <<  "nFASTSubSteps[" << p << "] = " << nFASTSubSteps[p] << endl ;
            for (int n = 0; n < nFASTSubSteps[p]; n++)
            {
                FAST->step();
            }
        }

        List<scalar> mainShaftOrientationChange;
        List<scalar> rotorApexChange;

        getPositions();

        // Find search cells.
        forAll(turbineName,i)
        {
            mainShaftOrientationChange.append(Foam::mag(mainShaftOrientation[i] - mainShaftOrientationBeforeSearch[i]));
            rotorApexChange.append(Foam::mag(rotorApex[i] - rotorApexBeforeSearch[i]));
         
            if ((mainShaftOrientationChange[i] > 1E-4) || (rotorApexChange[i] > 1E-1))
            {
               Info << "Performing Coarse Search on Turbine " << i << endl;
               Info << "Turbine " << i << tab << "mainShaftOrientationChange = " << mainShaftOrientationChange[i] << endl;
               Info << "Turbine " << i << tab << "rotorApexChange = " << rotorApexChange[i] << endl;
             
                updateRotorSearchCells(i);
              //if (includeNacelle[i])
              //{
                    updateNacelleSearchCells(i);
              //}
              //if (includeTower[i])
              //{
                    updateTowerSearchCells(i);
              //}
               mainShaftOrientationBeforeSearch[i] = 1.0* mainShaftOrientation[i];
               rotorApexBeforeSearch[i] = 1.0 * rotorApex[i];
            }
        }
        updateTurbinesControlled();

        // Recompute the blade-aligned coordinate system.
        computeBladeAlignedVectors();

      //Info << "bladeWindVectorsCartesian = " << bladeWindVectorsCartesian << endl;

        computeBladePointRadius();

        // Recompute radius from main shaft axis if body force
        // is projected normal to the streamlines
        forAll(turbineName,i)
        {
            if ((mainShaftOrientationChange[i] > 1E-5) || (rotorApexChange[i] > 1E0))
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

        // Update the turbine state.
        if (p < numTurbines)
        {
            for (int n = 0; n < nFASTSubSteps[p]; n++)
            {
                FAST->step();
            }
        }

        List<scalar> mainShaftOrientationChange;
        List<scalar> rotorApexChange;

        getPositions();

        // Find search cells.
        forAll(turbineName,i)
        {
            mainShaftOrientationChange.append(Foam::mag(mainShaftOrientation[i] - mainShaftOrientationBeforeSearch[i]));
            rotorApexChange.append(Foam::mag(rotorApex[i] - rotorApexBeforeSearch[i]));

            if ((mainShaftOrientationChange[i] > 1E-5) || (rotorApexChange[i] > 1E0))
            {
                Info << "Performing Coarse Search on Turbine " << i << endl;
                Info << "Turbine " << i << tab << "mainShaftOrientationChange = " << mainShaftOrientationChange[i] << endl;
                Info << "Turbine " << i << tab << "rotorApexChange = " << rotorApexChange[i] << endl;
                
                updateRotorSearchCells(i);
              //if (includeNacelle[i])
              //{
                    updateNacelleSearchCells(i);
              //}
              //if (includeTower[i])
              //{
                    updateTowerSearchCells(i);
              //}
            }

            mainShaftOrientationBeforeSearch[i] = 1.0 * mainShaftOrientation[i];
            rotorApexBeforeSearch[i] = 1.0 * rotorApex[i];
        }

        updateTurbinesControlled();

        // Recompute the blade-aligned coordinate system.
        computeBladeAlignedVectors();

        computeBladePointRadius();

        // Recompute radius from main shaft axis if body force
        // is projected normal to the streamlines
        forAll(turbineName,i)
        {
            if ((mainShaftOrientationChange[i] > 1E-5) || (rotorApexChange[i] > 1E0))
            {
                updateRadius(i);
            }
        }

        // Find out which processor controls which actuator point,
        // and with that information sample the wind at the actuator
        // points.
        updateBladePointControlProcNo();
        updateNacellePointControlProcNo();
        updateTowerPointControlProcNo();

        sampleBladePointWindVectors();
        sampleNacellePointWindVectors();
        sampleTowerPointWindVectors();

        // Send the sampled velocities out to FAST.
        sendVelocities();

        // Compute the geometry aligned velocity.
        computeBladeAlignedVelocity();       
    }

    // Compute the actuator point forces.
    getForces();

    // Zero out the body forces and spreading function.
    bodyForce *= 0.0;
    gBlade *= 0.0;

    // Project the actuator forces as body forces.
    forAll(turbineName,i)
    {
        updateBladeBodyForce(i);
     
        if (includeNacelle[i])
        {
            updateNacelleBodyForce(i);
        }
     
        if (includeTower[i])
        {
            updateTowerBodyForce(i);
        }
    }

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

    Info << "mainShaftOrientation = " << mainShaftOrientation << endl;
    Info << "mainShaftOrientationBeforeSearch = " << mainShaftOrientationBeforeSearch << endl;

    Info << "rotorApex = " << rotorApex << endl;
    Info << "rotorApexBeforeSearch = " << rotorApexBeforeSearch << endl;
}


void horizontalAxisWindTurbinesALMOpenFAST::openOutputFiles()
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
    //  bladePointAlphaFile_ = new OFstream(rootDir/time/"bladePointAlpha");
    // *bladePointAlphaFile_ << "#Turbine    Blade    Time(s)    dt(s)    angle-of-attack (degrees)" << endl;

        bladePointVmagFile_ = new OFstream(rootDir/time/"bladePointVmag");
       *bladePointVmagFile_ << "#Turbine    Blade    Time(s)    dt(s)    Vmag (m/s)" << endl;
    
        bladePointVaxialFile_ = new OFstream(rootDir/time/"bladePointVaxial");
       *bladePointVaxialFile_ << "#Turbine    Blade    Time(s)    dt(s)    Vaxial (m/s)" << endl;

        bladePointVtangentialFile_ = new OFstream(rootDir/time/"bladePointVtangential");
       *bladePointVtangentialFile_ << "#Turbine    Blade    Time(s)    dt(s)    Vtangential (m/s)" << endl;

        bladePointVradialFile_ = new OFstream(rootDir/time/"bladePointVradial");
       *bladePointVradialFile_ << "#Turbine    Blade    Time(s)    dt(s)    Vradial (m/s)" << endl;

    //  bladePointClFile_ = new OFstream(rootDir/time/"bladePointCl");
    // *bladePointClFile_ << "#Turbine    Blade    Time(s)    dt(s)    Cl" << endl;

    //  bladePointCdFile_ = new OFstream(rootDir/time/"bladePointCd");
    // *bladePointCdFile_ << "#Turbine    Blade    Time(s)    dt(s)    Cd" << endl;

    //  bladePointLiftFile_ = new OFstream(rootDir/time/"bladePointLift");
    // *bladePointLiftFile_ << "#Turbine    Blade    Time(s)    dt(s)    lift (N)" << endl;

    //  bladePointDragFile_ = new OFstream(rootDir/time/"bladePointDrag");
    // *bladePointDragFile_ << "#Turbine    Blade    Time(s)    dt(s)    drag (N)" << endl;

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
       
    //  towerPointClFile_ = new OFstream(rootDir/time/"towerPointCl");
    // *towerPointClFile_ << "#Turbine    Time(s)    dt(s)    Cl" << endl;

    //  towerPointCdFile_ = new OFstream(rootDir/time/"towerPointCd");
    // *towerPointCdFile_ << "#Turbine    Time(s)    dt(s)    Cd" << endl;

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

   //   generatorPowerFile_ = new OFstream(rootDir/time/"generatorPower");
   //  *generatorPowerFile_ << "#Turbine    Time(s)    dt(s)    generator power (W)" << endl;

        rotorSpeedFile_ = new OFstream(rootDir/time/"rotorSpeed");
   //  *rotorSpeedFile_ << "#Turbine    Time(s)    dt(s)    rotor rotation rate(rpm)" << endl;

   //   rotorSpeedFFile_ = new OFstream(rootDir/time/"rotorSpeedFiltered");
   //  *rotorSpeedFFile_ << "#Turbine    Time(s)    dt(s)    filtered rotor rotation rate(rpm)" << endl;

   //   rotorAzimuthFile_ = new OFstream(rootDir/time/"rotorAzimuth");
   //  *rotorAzimuthFile_ << "#Turbine    Time(s)    dt(s)    blade 1 azimuth angle (degrees)" << endl;



    // NACELLE-RELATED FILES 
        nacelleAxialForceFile_ = new OFstream(rootDir/time/"nacelleAxialForce");
       *nacelleAxialForceFile_ << "#Turbine    Time(s)    dt(s)    nacelle axial force (N)" << endl;

        nacelleHorizontalForceFile_ = new OFstream(rootDir/time/"nacelleHorizontalForce");
       *nacelleHorizontalForceFile_ << "#Turbine    Time(s)    dt(s)    nacelle horizontal force (N)" << endl;

        nacelleVerticalForceFile_ = new OFstream(rootDir/time/"nacelleVerticalForce");
       *nacelleVerticalForceFile_ << "#Turbine    Time(s)    dt(s)    nacelle vertical force (N)" << endl;

    //  nacelleYawFile_ = new OFstream(rootDir/time/"nacelleYaw");
    // *nacelleYawFile_ << "#Turbine    Time(s)    dt(s)    nacelle yaw angle (degrees)" << endl;



    // TOWER-RELATED FILES 
        towerAxialForceFile_ = new OFstream(rootDir/time/"towerAxialForce");
       *towerAxialForceFile_ << "#Turbine    Time(s)    dt(s)    tower axial force (N)" << endl;

        towerHorizontalForceFile_ = new OFstream(rootDir/time/"towerHorizontalForce");
       *towerHorizontalForceFile_ << "#Turbine    Time(s)    dt(s)    tower horizontal force (N)" << endl;


    
    // BLADE-RELATED FILES
   //   bladePitchFile_ = new OFstream(rootDir/time/"bladePitch");
   //  *bladePitchFile_ << "#Turbine    Time(s)    dt(s)    blade pitch angle (degrees)" << endl;

       

    // GENERATOR-RELATED FILES
   //   generatorTorqueFile_ = new OFstream(rootDir/time/"generatorTorque");
   //  *generatorTorqueFile_ << "#Turbine    Time(s)    dt(s)    generator torque (N-m)" << endl;

    }
}


void horizontalAxisWindTurbinesALMOpenFAST::printOutputFiles()
{
    if (Pstream::master())
    {
        forAll(rotorPower,i)
        {
            // Get necessary axes.
            vector axialVector = mainShaftOrientation[i];
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
   //       *towerPointAlphaFile_ << i << " " << time << " " << dt << " ";
            *towerPointVmagFile_ << i << " " << time << " " << dt << " ";
            *towerPointVaxialFile_ << i << " " << time << " " << dt << " ";
            *towerPointVhorizontalFile_ << i << " " << time << " " << dt << " ";
            *towerPointVverticalFile_ << i << " " << time << " " << dt << " ";
   //       *towerPointClFile_ << i << " " << time << " " << dt << " ";
   //       *towerPointCdFile_ << i << " " << time << " " << dt << " ";
  //        *towerPointLiftFile_ << i << " " << time << " " << dt << " ";
   //       *towerPointDragFile_ << i << " " << time << " " << dt << " ";
            *towerPointAxialForceFile_ << i << " " << time << " " << dt << " ";
            *towerPointHorizontalForceFile_ << i << " " << time << " " << dt << " ";
            *towerPointVerticalForceFile_ << i << " " << time << " " << dt << " ";
            *rotorTorqueFile_ << i << " " << time << " " << dt << " ";
            *rotorAxialForceFile_ << i << " " << time << " " << dt << " ";
            *rotorHorizontalForceFile_ << i << " " << time << " " << dt << " ";
            *rotorVerticalForceFile_ << i << " " << time << " " << dt << " ";
            *rotorPowerFile_ << i << " " << time << " " << dt << " ";
   //       *generatorPowerFile_ << i << " " << time << " " << dt << " ";
            *rotorSpeedFile_ << i << " " << time << " " << dt << " ";
   //       *rotorSpeedFFile_ << i << " " << time << " " << dt << " ";
   //       *rotorAzimuthFile_ << i << " " << time << " " << dt << " ";
            *nacelleAxialForceFile_ << i << " " << time << " " << dt << " ";
            *nacelleHorizontalForceFile_ << i << " " << time << " " << dt << " ";
            *nacelleVerticalForceFile_ << i << " " << time << " " << dt << " ";
   //       *nacelleYawFile_ << i << " " << time << " " << dt << " ";
            *towerAxialForceFile_ << i << " " << time << " " << dt << " ";
            *towerHorizontalForceFile_ << i << " " << time << " " << dt << " ";
   //       *generatorTorqueFile_ << i << " " << time << " " << dt << " ";
   //       *bladePitchFile_ << i << " " << time << " " << dt << " ";
         


            // Write out bulk information for each turbine (i.e., quantities not distributed
            // along points).
            *rotorTorqueFile_ << rotorTorque[i]*fluidDensity[i] << endl;
            *rotorAxialForceFile_ << rotorAxialForce[i]*fluidDensity[i] << endl;
            *rotorHorizontalForceFile_ << rotorHorizontalForce[i]*fluidDensity[i] << endl;
            *rotorVerticalForceFile_ << rotorVerticalForce[i]*fluidDensity[i] << endl;
            *rotorPowerFile_ << rotorPower[i]*fluidDensity[i] << endl;
   //       *generatorPowerFile_ << generatorPower[i]*fluidDensity[i] << endl;
            *rotorSpeedFile_ << rotorSpeed[i]/rpmRadSec << endl;
   //       *rotorSpeedFFile_ << rotorSpeedF[i]/rpmRadSec << endl;
   //       *rotorAzimuthFile_ << rotorAzimuth[i]/degRad << endl;
            *nacelleAxialForceFile_ << nacelleAxialForce[i]*fluidDensity[i] << endl;
            *nacelleHorizontalForceFile_ << nacelleHorizontalForce[i]*fluidDensity[i] << endl;
            *nacelleVerticalForceFile_ << nacelleVerticalForce[i]*fluidDensity[i] << endl;
   //       *nacelleYawFile_ <<  standardToCompass(nacYaw[i]/degRad) << endl;
            *towerAxialForceFile_ << towerAxialForce[i]*fluidDensity[i] << endl;
            *towerHorizontalForceFile_ << towerHorizontalForce[i]*fluidDensity[i] << endl;
   //       *generatorTorqueFile_ << generatorTorque[i] << endl;
   //       *bladePitchFile_ << bladePitch[i] << endl;


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
         //     *towerPointAlphaFile_ << towerPointAlpha[i][k] << " "; 
         //     *towerPointClFile_ << towerPointCl[i][k] << " "; 
         //     *towerPointCdFile_ << towerPointCd[i][k] << " "; 
         //     *towerPointLiftFile_ << towerPointLift[i][k]*fluidDensity[i] << " "; 
         //     *towerPointDragFile_ << towerPointDrag[i][k]*fluidDensity[i] << " "; 
                *towerPointAxialForceFile_ << towerPointAxialForce[i][k]*fluidDensity[i] << " "; 
                *towerPointHorizontalForceFile_ << towerPointHorizontalForce[i][k]*fluidDensity[i] << " "; 
            }
            forAll(towerSamplePoints[i], k)
            {
                *towerPointVmagFile_ << towerPointVmag[i][k] << " "; 
                *towerPointVaxialFile_ << (towerWindVectors[i][k] & axialVector) << " "; 
                *towerPointVhorizontalFile_ << (towerWindVectors[i][k] & horizontalVector) << " "; 
                *towerPointVverticalFile_ << (towerWindVectors[i][k] & verticalVector) << " "; 
            }
       //   *towerPointAlphaFile_ << endl;
            *towerPointVmagFile_ << endl;
            *towerPointVaxialFile_ << endl;
            *towerPointVhorizontalFile_ << endl;
            *towerPointVverticalFile_ << endl;
       //   *towerPointClFile_ << endl;
       //   *towerPointCdFile_ << endl;
            *towerPointLiftFile_ << endl;
            *towerPointDragFile_ << endl;
            *towerPointAxialForceFile_ << endl;
            *towerPointHorizontalForceFile_ << endl;
            *towerPointVerticalForceFile_ << endl;


            // Write out blade point quantitities.  Go blade by blade.
            forAll(bladePoints[i], j)
            {
                // Write out time and delta t.
       //       *bladePointAlphaFile_ << i << " " << j << " " << time << " " << dt << " ";
                *bladePointVmagFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointVaxialFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointVtangentialFile_ << i << " " << j << " " <<  time << " " << dt << " ";
                *bladePointVradialFile_ << i << " " << j << " " <<  time << " " << dt << " ";
       //       *bladePointClFile_ << i << " " << j << " " <<  time << " " << dt << " ";
       //       *bladePointCdFile_ << i << " " << j << " " <<  time << " " << dt << " ";
       //       *bladePointLiftFile_ << i << " " << j << " " <<  time << " " << dt << " ";
       //       *bladePointDragFile_ << i << " " << j << " " <<  time << " " << dt << " ";
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
       //           *bladePointAlphaFile_ << bladePointAlpha[i][j][k] << " ";
       //           *bladePointClFile_ << bladePointCl[i][j][k] << " ";
       //           *bladePointCdFile_ << bladePointCd[i][j][k] << " ";
       //           *bladePointLiftFile_ << bladePointLift[i][j][k]*fluidDensity[i] << " ";
       //           *bladePointDragFile_ << bladePointDrag[i][j][k]*fluidDensity[i] << " ";
                    *bladePointAxialForceFile_ << bladePointAxialForce[i][j][k]*fluidDensity[i] << " ";
                    *bladePointHorizontalForceFile_ << bladePointHorizontalForce[i][j][k]*fluidDensity[i] << " ";
                    *bladePointVerticalForceFile_ << bladePointVerticalForce[i][j][k]*fluidDensity[i] << " ";
                    *bladePointTorqueFile_ << bladePointTorque[i][j][k]*fluidDensity[i] << " ";
                    *bladePointXFile_ << bladePoints[i][j][k].x() << " ";
                    *bladePointYFile_ << bladePoints[i][j][k].y() << " ";
                    *bladePointZFile_ << bladePoints[i][j][k].z() << " ";
                }
                forAll(bladeSamplePoints[i][j], k)
                {
                    *bladePointVmagFile_ << bladePointVmag[i][j][k] << " ";
                    *bladePointVaxialFile_ << bladeWindVectors[i][j][k].x() << " ";
                    *bladePointVtangentialFile_ << bladeWindVectors[i][j][k].y() << " ";
                    *bladePointVradialFile_ << bladeWindVectors[i][j][k].z() << " ";
                }

                // End the line so we can go on to the next blade.
       //       *bladePointAlphaFile_ << endl;
                *bladePointVmagFile_ << endl;
                *bladePointVaxialFile_ << endl;
                *bladePointVtangentialFile_ << endl;
                *bladePointVradialFile_ << endl;
       //       *bladePointClFile_ << endl;
       //       *bladePointCdFile_ << endl;
       //       *bladePointLiftFile_ << endl;
       //       *bladePointDragFile_ << endl;
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
    //  *generatorPowerFile_ << endl;
        *rotorSpeedFile_ << endl;
    //  *rotorSpeedFFile_ << endl;
    //  *rotorAzimuthFile_ << endl;
        *nacelleAxialForceFile_ << endl;
        *nacelleHorizontalForceFile_ << endl;
        *nacelleVerticalForceFile_ << endl;
    //  *nacelleYawFile_ << endl;
        *towerAxialForceFile_ << endl;
        *towerHorizontalForceFile_ << endl;
    //  *generatorTorqueFile_ << endl;
    //  *bladePitchFile_ << endl;

    //  *bladePointAlphaFile_ << endl;
        *bladePointVmagFile_ << endl;
        *bladePointVaxialFile_ << endl;
        *bladePointVtangentialFile_ << endl;
        *bladePointVradialFile_ << endl;
    //  *bladePointClFile_ << endl;
    //  *bladePointCdFile_ << endl;
    //  *bladePointLiftFile_ << endl;
    //  *bladePointDragFile_ << endl;
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

    //  *towerPointAlphaFile_ << endl;
        *towerPointVmagFile_ << endl;
        *towerPointVaxialFile_ << endl;
        *towerPointVhorizontalFile_ << endl;
        *towerPointVverticalFile_ << endl;
    //  *towerPointClFile_ << endl;
    //  *towerPointCdFile_ << endl;
    //  *towerPointLiftFile_ << endl;
    //  *towerPointDragFile_ << endl;
        *towerPointAxialForceFile_ << endl;
        *towerPointHorizontalForceFile_ << endl;
        *towerPointVerticalForceFile_ << endl;
    }
}
   
     
void horizontalAxisWindTurbinesALMOpenFAST::printDebug()
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
    
    Info << "numBl = " << numBl << endl;
    Info << "nacelleLength = " << nacelleLength << endl;
    Info << "nacelleFrontalArea = " << nacelleFrontalArea << endl;
    Info << "nacelleEquivalentRadius = " << nacelleEquivalentRadius << endl;
    Info << "nacelleCd = " << nacelleCd << endl;

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
    Info << "mainShaftOrientation = " << mainShaftOrientation << endl;
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


volVectorField& horizontalAxisWindTurbinesALMOpenFAST::force()
{
    // Return the body force field to the solver
    return bodyForce;
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace turbineModels
} // End namespace Foam

// ************************************************************************* //

