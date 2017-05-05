/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2012-2014 OpenFOAM Foundation
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

#include "spinnerLidar.H"
#include "surfaceFields.H"
#include "dictionary.H"
#include "interpolateXY.H"
#include "clockTime.H"

// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

namespace Foam
{
defineTypeNameAndDebug(spinnerLidar, 0);
}


// * * * * * * * * * * * * * Private Member Functions  * * * * * * * * * * * //



// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

Foam::spinnerLidar::spinnerLidar
(
    const word& name,
    const objectRegistry& obr,
    const dictionary& dict,
    const bool loadFromFiles
)
:
    name_(name),
    mesh_(refCast<const fvMesh>(obr)),
    runTime_(mesh_.time()),
    active_(true),
    degRad((Foam::constant::mathematical::pi)/180.0),
    UName_("U"),
    U_(mesh_.lookupObject<volVectorField>(UName_)),
    dtSolver(runTime_.deltaT().value()),
    time(runTime_.timeName()),
    tSolver(runTime_.value()),
    tLidar(runTime_.value()),
    tCycle(runTime_.value()),
    tElapsed(0.0),
    tCarryOver(0.0),
    rndGen(123456),
    outputFile(NULL),
    meshPoints(mesh_.points())
{
    // Read the dictionary.
    read(dict);

    // Calculate the period for a full scan.
    scanPeriod = oneSecScanMotorRPM/motorRPM;
    Info << "scanPeriod " << scanPeriod << endl;

    // Calculate the period to take one sample.  A series of samples
    // then make up one spectrum.  Then a series of spectra are used
    // to get an average sample.
    samplePeriod = 1.0/sampleRate;
    Info << "samplePeriod " << samplePeriod << endl;

    // Calculate the rate at which average samples (based on a collection
    // of spectra) are gathered.
    averageSamplePeriod = samplePeriod * samplesPerSpectrum * spectraPerAverageSample;
    averageSampleRate = 1.0/averageSamplePeriod;
    Info << "averageSamplePeriod " << averageSamplePeriod << endl;

    // Because the actual lidar's average sample is made up of multiple spectra
    // collected as the lidar beam sweeps along its path, and sampling
    // the hundreds of samples that go into an average sample is not
    // feasible, but we still want to explore the averaging caused 
    // by the sweeping, we specify "virtualSamplesPerAverageSample" or the
    // number of samples this tool will take along the path of an average
    // sample.
    virtualSamplePeriod = averageSamplePeriod / virtualSamplesPerAverageSample;

    // Number of samples and subsamples.
    nAverageSamples = ceil(scanPeriod/averageSamplePeriod);
    Info << "nAverageSamples " << nAverageSamples << tab << scanPeriod/averageSamplePeriod << endl;
    scanPeriod = nAverageSamples * averageSamplePeriod;
    Info << "scanPeriod " << scanPeriod << endl;
    nVirtualSamples = nAverageSamples * virtualSamplesPerAverageSample;
    
    // Number of points along a beam to sample.
    nBeamPoints = beamDistribution.size();

    // Create the beam points.
    for(int i = 0; i < nVirtualSamples; i++)
    {
        samplePoints.append(List<vector>(nBeamPoints,vector::zero));
    }

    // Create the perturbation vectors that will be used to break ties
    // when deciding which processor a beam point lies upon.
    if (Pstream::myProcNo() == 0)
    {
        for(int i = 0; i < nVirtualSamples; i++)
        {
            perturbVectors.append(List<vector>(nBeamPoints,vector::zero));
            for(int j = 0; j < nBeamPoints; j++)
            {
                perturbVectors[i][j] = perturb*(2.0*rndGen.vector01()-vector::one);
            }
        }
    }
    Pstream::scatter(perturbVectors);

    // Create the sampled wind vector list.
    sampledWindVectors = List<vector>(nVirtualSamples*nBeamPoints,vector::zero);

    // Build the controlCellID list of lists.
    for(int i = 0; i < nVirtualSamples; i++)
    {
        controlCellID.append(List<label>(nBeamPoints,-1));
    }

    // Compute the prism rotation rates.
    rr1 = ((motorRPM/60.0)*2.0*Foam::constant::mathematical::pi) * gearRatioMotorToPrism1;
    rr2 = gearRatioPrism1ToPrism2 * rr1;

    // Create the initial prism rotation axis vectors.
    resetAxes();

    // Get the current lidar angles.
    rotationCurrent = 0.0;
    elevationCurrent = 0.0;

    // Rotate the prism rotation axes if necessary.
    rotateLidar();

    // Create the beams.
    createScanPattern();

    // Write out variables for debugging.
  //writeVariables();
}


// * * * * * * * * * * * * * * * * Destructor  * * * * * * * * * * * * * * * //

Foam::spinnerLidar::~spinnerLidar()
{}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

void Foam::spinnerLidar::read(const dictionary& dict)
{
    if (active_)
    {
        Info << type() << ": Reading dictionary..." << endl;

        UName_ = dict.lookupOrDefault<word>("UName", "U");
        
        beamScanPatternI = 0;

        scanRepeats = dict.lookupOrDefault<bool>("scanRepeats", true);

        timeBetweenScans = dict.lookupOrDefault<scalar>("timeBetweenScans", 0.0);

        motorRPM = dict.lookupOrDefault<scalar>("motorRPM", 2016.0);
        oneSecScanMotorRPM = dict.lookupOrDefault<scalar>("oneSecScanMotorRPM", 2016.0);
        gearRatioMotorToPrism1 = dict.lookupOrDefault<scalar>("gearRatioMotorToPrism1", 7.0/(2016.0/60.0));
        gearRatioPrism1ToPrism2 = dict.lookupOrDefault<scalar>("gearRatioPrism1ToPrism2", 6.0/7.0);
        prism1Angle = dict.lookupOrDefault<scalar>("prism1Angle", 15.0);
        prism2Angle = dict.lookupOrDefault<scalar>("prism2Angle", 15.0);
        sampleRate = dict.lookupOrDefault<scalar>("sampleRate", 100.0E6);
        samplesPerSpectrum = dict.lookupOrDefault<label>("samplesPerSpectrum", 512);
        spectraPerAverageSample = dict.lookupOrDefault<label>("spectraPerAverageSample", 625);
        virtualSamplesPerAverageSample = dict.lookupOrDefault<label>("virtualSamplesPerAverageSample", 3);

        beamOrigin = dict.lookupOrDefault<vector>("beamOrigin", vector::zero);
   
        beamMaxDistance =  dict.lookupOrDefault<scalar>("beamMaxDistance", 1000.0);

        beamDistribution = dict.lookup("beamDistribution");
        scalar beamDistMax = max(beamDistribution);
        forAll(beamDistribution,i)
        {
           beamDistribution[i] /= beamDistMax;
        }

        List<List<scalar> > beamAngle(dict.lookup("beamAngle"));
        
        forAll(beamAngle,i)
        {
           beamAngleTime.append(beamAngle[i][0]);
           beamAngleRotation.append(beamAngle[i][1]);
           beamAngleElevation.append(beamAngle[i][2]);
        }

        beamRotationAxis = dict.lookup("beamRotationAxis");
        beamRotationAxis /= mag(beamRotationAxis);

        beamElevationAxis = dict.lookup("beamElevationAxis");
        beamElevationAxis /= mag(beamElevationAxis);

        beamElevationAxisOriginal = beamElevationAxis;

        perturb = dict.lookupOrDefault<scalar>("perturb", 1E-4);
    }
}


void Foam::spinnerLidar::resetAxes()
{
    // Reset the beam position and create the initial prism rotation axis vectors.
    vector up = vector::zero;
    up.z() = 1.0;
    lastPrism1Axis = vector::zero;
    lastPrism2Axis = vector::zero;
    lastBeamAxis = vector::zero;
    lastPrism1Axis.x() = 1.0;
    lastPrism2Axis.x() = 1.0;
    lastBeamAxis.x() = 1.0;
    lastPrism1Axis = rotateVector(lastPrism1Axis,vector::zero,up,(0.0)*degRad);
    lastPrism2Axis = rotateVector(lastPrism2Axis,vector::zero,up,-(prism1Angle)*degRad);
    lastBeamAxis = rotateVector(lastBeamAxis,vector::zero,up,-(prism1Angle + prism2Angle)*degRad);
    lastBeamAxis = rotateVector(lastBeamAxis,vector::zero,lastPrism2Axis,-rr2*(0.5*virtualSamplePeriod));
    lastBeamAxis = rotateVector(lastBeamAxis,vector::zero,lastPrism1Axis,-rr1*(0.5*virtualSamplePeriod));
    lastPrism2Axis = rotateVector(lastPrism2Axis,vector::zero,lastPrism1Axis,-rr1*(0.5*virtualSamplePeriod));
    lastBeamAxis /= mag(lastBeamAxis);
    lastPrism1Axis /= mag(lastPrism1Axis);
    lastPrism2Axis /= mag(lastPrism2Axis);

    beamElevationAxis = beamElevationAxisOriginal;
}


void Foam::spinnerLidar::createScanPattern()
{
    Info << type() << ": Computing scan pattern..." << endl;

    // Initialize the sample time and set the axes to whereever
    // they last were.
    beamScanPatternTime.clear();
    beamScanPatternVector.clear();
    scalar sampleTime = 0.0;
    vector prism1Axis = lastPrism1Axis;
    vector prism2Axis = lastPrism2Axis;
    vector beamAxis = lastBeamAxis;

    for(int i = 0; i < nVirtualSamples; i++)
    {
        // We start at the last sample time, so add on the sample period,
        // also add a beam in the list for this sample time.
        sampleTime += virtualSamplePeriod;
        beamScanPatternTime.append(sampleTime);

        // Rotate beams and axes.  This is done keeping in mind that sampling
        // is done while the beam is moving and tracing out a path.  In this virtual
        // sampling, we can't sample over a path, but only at discrete points, so set
        // the point in the center of the path, or where the beam is at t - 1/2*samplePeriod.

        // Rotate the beam about prism 2 axis at rotation rate 2.
        beamAxis = rotateVector(beamAxis,vector::zero,prism2Axis,rr2*virtualSamplePeriod);
      
        // Rotate the beam about prism 1 axis at rotation rate 1.
        beamAxis = rotateVector(beamAxis,vector::zero,prism1Axis,rr1*virtualSamplePeriod);
        beamAxis /= mag(beamAxis);

        // Rotate the prism 2 axis about prism 1 axis at rotation rate 1.
        prism2Axis = rotateVector(prism2Axis,vector::zero,prism1Axis,rr1*virtualSamplePeriod);
        prism2Axis /= mag(prism2Axis);

        // Add this beam axis to the list of scan pattern vectors.
        beamScanPatternVector.append(beamAxis);
    }

    // Now that all the beamPoints are formed, set the last axes to whereever they last
    // are.
    lastBeamAxis = beamAxis/mag(beamAxis);
    lastPrism1Axis = prism1Axis/mag(prism1Axis);
    lastPrism2Axis = prism2Axis/mag(prism2Axis);
    

    // Set the beam points.
    forAll(samplePoints,i)
    {
        forAll(samplePoints[i],j)
        {
            samplePoints[i][j] = beamOrigin + beamMaxDistance*beamDistribution[j]*beamScanPatternVector[i];
        }
    }

    // Identify the control cell IDs.
    findControlProcAndCell();
}


void Foam::spinnerLidar::findControlProcAndCell()
{
    Info << type() << ": Searching for interpolation cells in ";

    clockTime timer;
    scalar elapsedTime = timer.timeIncrement();

    boundBox meshBb(meshPoints,false);
    vector minBb = meshBb.min();
    vector maxBb = meshBb.max();

    label nBeams = beamScanPatternTime.size();
    label nBeamPoints = beamDistribution.size();
    label totalSamplePoints = nBeams*nBeamPoints;
    label iter = 0;

    List<scalar> minDisLocal(totalSamplePoints,1.0E30);
    List<scalar> minDisGlobal(totalSamplePoints,1.0E30);

    for(int i = 0; i < nBeams; i++)
    {
        for(int j = 0; j < nBeamPoints; j++)
        {
            scalar minDis = 1.0E30;
            label cellID = -1;

            // Bounding box check.
            if ((samplePoints[i][j].x() >= minBb.x() && samplePoints[i][j].x() <= maxBb.x()) &&
                (samplePoints[i][j].y() >= minBb.y() && samplePoints[i][j].y() <= maxBb.y()) &&
                (samplePoints[i][j].z() >= minBb.z() && samplePoints[i][j].z() <= maxBb.z()))
            {
                forAll(mesh_.C(),k)
                {
                    scalar dis = mag(mesh_.C()[k] - (samplePoints[i][j] + perturbVectors[i][j]));
                    if (dis <= minDis)
                    {
                        cellID = k;
                        minDis = dis;
                    }
                }
            }
            minDisLocal[iter] = minDis;
            minDisGlobal[iter] = minDis;
            controlCellID[i][j] = cellID;
            iter++;
        }
    }

    Pstream::gather(minDisGlobal,minOp<List<scalar> >());
    Pstream::scatter(minDisGlobal);

    iter = 0;
    for(int i = 0; i < nBeams; i++)
    {
        for(int j = 0; j < nBeamPoints; j++)
        {
            if(minDisGlobal[iter] != minDisLocal[iter])
            {
                controlCellID[i][j] = -1;
            }
            iter++;
        }
    }

    elapsedTime = timer.timeIncrement();
    Info << elapsedTime << "s..." << endl;
}


void Foam::spinnerLidar::sampleWinds(label i, volTensorField& gradU)
{
    label nBeamPoints = beamDistribution.size();
    label iter = i * nBeamPoints;
    forAll(samplePoints[i],j)
    {
        // If this processor's piece of the domain contains this sampling point, then do
        // trilinear interpolation using U(x+dx) = U(x) + dU(x)/dx*(dx).
        if(controlCellID[i][j] != -1)
        {
            vector dx = samplePoints[i][j] - mesh_.C()[controlCellID[i][j]];
            vector dU = dx & gradU[controlCellID[i][j]];
            sampledWindVectors[iter] = U_[controlCellID[i][j]] + dU;
        }
        // Otherwise, if this sampling point is not handled by this processor, set the 
        // velocity to zero.
        else
        {
            sampledWindVectors[iter] = vector::zero;
        }
        iter++;
    }
    // Note, that this does not do the parallel gather of the results of each processor's
    // interpolation.  After all the beams are sampled, then the execute() function will
    // do the parallel gather.
}


vector Foam::spinnerLidar::rotateVector(vector v, vector rotationPoint, vector axis, scalar angle)
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
    v = v - rotationPoint;

    // Perform the rotation.
    v = RM & v;

    // Return the rotated point to its new location relative to the rotation point.
    v = v + rotationPoint;

    return v;
}



void Foam::spinnerLidar::rotateLidar()
{
    // Store the old beam angles.
    scalar rotationOld = rotationCurrent;
    scalar elevationOld = elevationCurrent;

    // Get the current beam angles.
    rotationCurrent = interpolateXY(tLidar,beamAngleTime,beamAngleRotation);
    elevationCurrent = interpolateXY(tLidar,beamAngleTime,beamAngleElevation);

    // Find the change in beam angle.
    scalar deltaRotation = (rotationCurrent - rotationOld) * degRad;
    scalar deltaElevation = (elevationCurrent - elevationOld) * degRad;

    // If the change in angle is finite, then perform the rotation.
    if ((mag(deltaRotation) > 0.0) || (mag(deltaElevation) > 0.0))
    {
        if (scanRepeats)
        {
            // If the scan pattern repeats, reset the axes and rotate them, rotate the existing beams
            // and recalculate interpolation cells because they are not recalculated in the execute
            // function.
            resetAxes();

            beamElevationAxis = rotateVector(beamElevationAxis,vector::zero,beamRotationAxis,rotationCurrent*degRad);
            lastPrism1Axis = rotateVector(lastPrism1Axis,vector::zero,beamRotationAxis,rotationCurrent*degRad);
            lastPrism1Axis = rotateVector(lastPrism1Axis,vector::zero,beamElevationAxis,elevationCurrent*degRad);
            lastPrism2Axis = rotateVector(lastPrism2Axis,vector::zero,beamRotationAxis,rotationCurrent*degRad);
            lastPrism2Axis = rotateVector(lastPrism2Axis,vector::zero,beamElevationAxis,elevationCurrent*degRad);
            lastBeamAxis = rotateVector(lastBeamAxis,vector::zero,beamRotationAxis,rotationCurrent*degRad);
            lastBeamAxis = rotateVector(lastBeamAxis,vector::zero,beamElevationAxis,elevationCurrent*degRad);

            // Rotate existing beams
            forAll(samplePoints,i)
            {
                forAll(samplePoints[i],j)
                {
                    samplePoints[i][j] = rotateVector(samplePoints[i][j],beamOrigin,beamRotationAxis,deltaRotation);
                    samplePoints[i][j] = rotateVector(samplePoints[i][j],beamOrigin,beamElevationAxis,deltaElevation);
                }
            }

            // Locate the cells within which the beam points lie.
            findControlProcAndCell();
        }
        else
        {
            // Otherwise, rotate the existing axes only because the beams will be recomputed in the 
            // execute function.
            beamElevationAxis = rotateVector(beamElevationAxis,vector::zero,beamRotationAxis,deltaRotation);
            lastPrism1Axis = rotateVector(lastPrism1Axis,vector::zero,beamRotationAxis,deltaRotation);
            lastPrism1Axis = rotateVector(lastPrism1Axis,vector::zero,beamElevationAxis,deltaElevation);
            lastPrism2Axis = rotateVector(lastPrism2Axis,vector::zero,beamRotationAxis,deltaRotation);
            lastPrism2Axis = rotateVector(lastPrism2Axis,vector::zero,beamElevationAxis,deltaElevation);
            lastBeamAxis = rotateVector(lastBeamAxis,vector::zero,beamRotationAxis,deltaRotation);
            lastBeamAxis = rotateVector(lastBeamAxis,vector::zero,beamElevationAxis,deltaElevation);
        }
    }
}


void Foam::spinnerLidar::execute()
{
    if (active_)
    {
        // Update the time information.
        time = runTime_.timeName();
        tSolver = runTime_.value();
        dtSolver = runTime_.deltaT().value();

        // Take the gradient of the field for use when doing
        // trilinear interpolation to the lidar sample points.
        volTensorField gradU = fvc::grad(U_);

        // Set up the logic to do the beam sampling.  This part is a bit 
        // messy and likely be streamlined.  The difficulty is that the 
        // lidar samples at a certain rate and also has to stop to refocus
        // when changing ranges, but the CFD solver has its own time step
        // that could even be variable.  This part also handles any rotations
        // of the entire lidar unit in case it has some sort of specified motion
        // (i.e., moving with the nacelle or vibration).
        scalar tCarryOverSubtractor = 0.0;
        tElapsed = tCarryOver;
        do
        {
            if (tCarryOver < dtSolver)
            {
                // this part is for sampling along beams.
                if (beamScanPatternI <= beamScanPatternTime.size()-1)
                {
                    scalar dtSampling = 0.0;
                    if (beamScanPatternI == 0)
                    {
                        dtSampling = beamScanPatternTime[0];
                    }
                    else
                    {
                        dtSampling = beamScanPatternTime[beamScanPatternI] -  beamScanPatternTime[beamScanPatternI-1];
                    }
                    tElapsed += dtSampling; 

                    if (tElapsed <= dtSolver)
                    {
                        // update the lidar time
                        tLidar = tSolver - dtSolver + tElapsed;

                      //Info << "sampling beam number " << beamScanPatternI << tab;
                      //Info << "t = " << tLidar << tab;
                      //Info << "tElapsed = " << tElapsed << endl;

                        // sample beam I.
                        sampleWinds(beamScanPatternI,gradU);

                        // advance to next beam.
                        beamScanPatternI += 1;
                    }
                    else
                    {
                        // in case advancing to the next beam places us in the next CFD time
                        // step, this variable should be set to a non-zero value so that we
                        // end up in the correct place on the next time step.
                        tCarryOverSubtractor = dtSampling;
                    }
                }

                // this part is for waiting for the beam refocus.
                if ((beamScanPatternI > beamScanPatternTime.size()-1) && ((tElapsed - dtSolver) < -1.0E-12))
                {
                    // parallel gather all the sampled data.
                    Pstream::gather(sampledWindVectors,sumOp<List<vector> >());

                    // update the lidar time
                    tLidar = tSolver - dtSolver + tElapsed;
  
                    // dump the data
                    writeBeamDataFormatted();

                    // reset the beam index.
                    beamScanPatternI = 0;

                    // rotate the lidar unit if necessary.
                    rotateLidar();

                    // recompute the beams for next scan.
                    if (scanRepeats == false)
                    {
                        createScanPattern();
                    }

                    // check to see where the time between scans puts us.  Does it spill over
                    // into the next time step or not?
                    tElapsed += timeBetweenScans;

                  //Info << "Between scan " << tab;
                  //Info << "t = " << tLidar << tab;
                  //Info << "tElapsed = " << tElapsed << endl;
                }
            }
            else
            {
                tCarryOver -= dtSolver;
            }
        } 
        while ((tElapsed - dtSolver) < -1.0E-12);

        tCarryOver = tElapsed - dtSolver - tCarryOverSubtractor;
    }
}


void Foam::spinnerLidar::end()
{
    if (active_)
    {
        execute();
    }
}


void Foam::spinnerLidar::timeSet()
{
    // Do nothing
}


void Foam::spinnerLidar::write()
{
    // Do nothing
}


void Foam::spinnerLidar::writeBeamDataFormatted()
{
    Info << type() << ": Writing the sampled data in ";

    clockTime timer;
    scalar elapsedTime = timer.timeIncrement();

//    scalar startTime = runTime_.elapsedClockTime();

    if (Pstream::master())
    {
        // Create the name of the root directory to dump data.
        fileName rootDir;

        if (Pstream::parRun())
        {
            rootDir = runTime_.path()/"../postProcessing"/name_;
        }
        else
        {
            rootDir = runTime_.path()/"postProcessing"/name_;
        }

        // Create the time directory.  This is the lidar time, which 
        // does not have to match up with the CFD time steps.       
        word tLidarName = Foam::name(tLidar);

        if (!isDir(rootDir/tLidarName))
        {
            mkDir(rootDir/tLidarName);
        }

        // Write the data files.
        // 1.) Lidar orientations during scan.
        label nBeams = beamScanPatternTime.size();
        label nBeamPoints = beamDistribution.size();
        label iter = 0;
        outputFile.reset(new OFstream(rootDir/tLidarName/"beamOrientation"));
        outputFile() << "#t (s)" << tab << "beam number" << tab << "beam orientation unit vector" << endl;
        forAll(beamScanPatternTime,i)
        {
            outputFile() << tLidar - beamScanPatternTime[nBeams-1] + beamScanPatternTime[i] << " " << i+1 << " " << beamScanPatternVector[i] << endl;
        }

        // 2.) u-velocity.
        outputFile.reset(new OFstream(rootDir/tLidarName/"uVel"));
        outputFile() << "#t (s)" << tab << "beam number" << tab << "u-velocity component (m/s)" << endl;
        iter = 0;
        forAll(beamScanPatternTime,i)
        {
            outputFile() << tLidar - beamScanPatternTime[nBeams-1] + beamScanPatternTime[i] << " " << i+1 << " "; 
            for(int j = 0; j < nBeamPoints; j++)
            {
                outputFile() << sampledWindVectors[iter].x() << " ";
                iter ++;
            }
            outputFile() << endl;
        }

        // 2.) v-velocity.
        outputFile.reset(new OFstream(rootDir/tLidarName/"vVel"));
        outputFile() << "#t (s)" << tab << "beam number" << tab << "v-velocity component (m/s)" << endl;
        iter = 0;
        forAll(beamScanPatternTime,i)
        {
            outputFile() << tLidar - beamScanPatternTime[nBeams-1] + beamScanPatternTime[i] << " " << i+1 << " ";
            for(int j = 0; j < nBeamPoints; j++)
            {
                outputFile() << sampledWindVectors[iter].y() << " ";
                iter ++;
            }
            outputFile() << endl;
        }

        // 3.) w-velocity.
        outputFile.reset(new OFstream(rootDir/tLidarName/"wVel"));
        outputFile() << "#t (s)" << tab << "beam number" << tab << "w-velocity component (m/s)" << endl;
        iter = 0;
        forAll(beamScanPatternTime,i)
        {
            outputFile() << tLidar - beamScanPatternTime[nBeams-1] + beamScanPatternTime[i] << " " << i+1 << " ";
            for(int j = 0; j < nBeamPoints; j++)
            {
                outputFile() << sampledWindVectors[iter].z() << " ";
                iter ++;
            }
            outputFile() << endl;
        }

        // 4.) line-of-sight velocity.
        outputFile.reset(new OFstream(rootDir/tLidarName/"losVel"));
        outputFile() << "#t (s)" << tab << "beam number" << tab << "los-velocity component (m/s)" << endl;
        iter = 0;
        forAll(beamScanPatternTime,i)
        {
            outputFile() << tLidar - beamScanPatternTime[nBeams-1] + beamScanPatternTime[i] << " " << i+1 << " ";
            for(int j = 0; j < nBeamPoints; j++)
            {
                scalar los = sampledWindVectors[iter] & beamScanPatternVector[i];
                outputFile() << los << " ";
                iter ++;
            }
            outputFile() << endl;
        }
    }

    elapsedTime = timer.timeIncrement();
    Info << elapsedTime << "s..." << endl;
}


void Foam::spinnerLidar::writeVariables()
{
    Info << "name_: " << name_ << endl;
    Info << "active_: " << active_ << endl;
    Info << "UName_: " << UName_ << endl;
    Info << "dtSolver: " << dtSolver << endl;
    Info << "tSolver: " << tSolver << endl;
    Info << "tLidar: " << tLidar << endl;
    Info << "tElapsed: " << tElapsed << endl;
    Info << "tCycle: " << tCycle << endl;
    Info << "beamScanPatternTime: " << beamScanPatternTime << endl;
    Info << "beamScanPatternVector: " << beamScanPatternVector << endl;
    Info << "beamScanPatternI: " << beamScanPatternI << endl;
    Info << "timeBetweenScans: " << timeBetweenScans << endl;
    Info << "motorRPM: " << motorRPM << endl;
    Info << "oneSecScanMotorRPM: " << oneSecScanMotorRPM << endl;
    Info << "gearRatioMotorToPrism1: " << gearRatioMotorToPrism1 << endl;
    Info << "gearRatioPrism1ToPrism2: " << gearRatioPrism1ToPrism2 << endl;
    Info << "rr1: " << rr1 << endl;
    Info << "rr2: " << rr2 << endl;
    Info << "prism1Angle: " << prism1Angle << endl;
    Info << "prism2Angle: " << prism2Angle << endl;
    Info << "sampleRate: " << sampleRate << endl;
    Info << "samplePeriod: " << samplePeriod << endl;
    Info << "samplesPerSpectrum: " << samplesPerSpectrum << endl;
    Info << "spectraPerAverageSample: " << spectraPerAverageSample << endl;
    Info << "averageSampleRate: " << averageSampleRate << endl;
    Info << "averageSamplePeriod: " << averageSamplePeriod << endl;
    Info << "virtualSamplesPerAverageSample: " << virtualSamplesPerAverageSample << endl;
    Info << "virtualSamplePeriod: " << virtualSamplePeriod << endl;
    Info << "scanPeriod: " << scanPeriod << endl;
    Info << "nAverageSamples: " << nAverageSamples << endl;
    Info << "nVirtualSamples: " << nVirtualSamples << endl;
    Info << "nBeamPoints: " << nBeamPoints << endl;
    Info << "beamOrigin: " << beamOrigin << endl;
    Info << "lastPrism1Axis: " << lastPrism1Axis << endl;
    Info << "lastPrism2Axis: " << lastPrism2Axis << endl;
    Info << "lastBeamAxis: " << lastBeamAxis << endl;
    Info << "beamMaxDistance: " << beamMaxDistance << endl;
    Info << "beamDistribution: " << beamDistribution << endl;
    Info << "beamAngleTime: " << beamAngleTime << endl;
    Info << "beamAngleRotation: " << beamAngleRotation << endl;
    Info << "beamAngleElevation: " << beamAngleElevation << endl;
    Info << "beamRotationAxis: " << beamRotationAxis << endl;
    Info << "beamElevationAxis: " << beamElevationAxis << endl;
    Info << "perturb: " << perturb << endl;
    Info << "perturbVectors: " << perturbVectors << endl;
    Info << "samplePoints: " << samplePoints << endl;
    Info << "sampledWindVectors: " << sampledWindVectors << endl;
    Pout << "controlCellID: " << controlCellID << endl;
    Info << "rotationCurrent: " << rotationCurrent << endl;
    Info << "elevationCurrent: " << elevationCurrent << endl;
}


// ************************************************************************* //
