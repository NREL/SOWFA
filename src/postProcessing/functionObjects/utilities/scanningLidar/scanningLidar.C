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

#include "scanningLidar.H"
#include "surfaceFields.H"
#include "dictionary.H"
#include "interpolateXY.H"

// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

namespace Foam
{
defineTypeNameAndDebug(scanningLidar, 0);
}


// * * * * * * * * * * * * * Private Member Functions  * * * * * * * * * * * //



// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

Foam::scanningLidar::scanningLidar
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
    outputFile(NULL)
{
    // Read the dictionary.
    read(dict);

    // Create the sample lines.
    createBeams();

    // Get the current beam angles.
    rotationCurrent = 0.0;
    elevationCurrent = 0.0;

    // Rotate the beams if necessary.
    rotateLidar();

    writeVariables();
}


// * * * * * * * * * * * * * * * * Destructor  * * * * * * * * * * * * * * * //

Foam::scanningLidar::~scanningLidar()
{}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

void Foam::scanningLidar::read(const dictionary& dict)
{
    if (active_)
    {
        Info<< type() << ":" << nl;

        UName_ = dict.lookupOrDefault<word>("UName", "U");
        
        List<List<scalar> > beamScanPattern(dict.lookup("beamScanPattern"));
        forAll(beamScanPattern,i)
        {
           beamScanPatternTime.append(beamScanPattern[i][0]);
           beamScanPatternVector.append(vector::zero);
           beamScanPatternVector[i].x() = beamScanPattern[i][1];
           beamScanPatternVector[i].y() = beamScanPattern[i][2];
           beamScanPatternVector[i].z() = beamScanPattern[i][3];
           beamScanPatternVector[i] /= mag(beamScanPatternVector[i]);
        }
        beamScanPatternI = 0;

        timeBetweenScans = dict.lookupOrDefault<scalar>("timeBetweenScans", 0.0);

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

        perturb = dict.lookupOrDefault<scalar>("perturb", 1E-4);
    }
}


void Foam::scanningLidar::createBeams()
{
    label nBeams = beamScanPatternTime.size();
    label nSamplePoints = beamDistribution.size();

    // Create the beam points.
    for(int i = 0; i < nBeams; i++)
    {
        samplePoints.append(List<vector>(nSamplePoints,vector::zero));
        for(int j = 0; j < nSamplePoints; j++)
        {
            samplePoints[i][j] = beamOrigin + beamMaxDistance*beamDistribution[j]*beamScanPatternVector[i];
        }
    }

    // Create the sampled wind vector list.
    sampledWindVectors = List<vector>(nBeams*nSamplePoints,vector::zero);    

    // Create the perturbation vectors that will be used to break ties
    // when deciding which processor a beam point lies upon.
    if (Pstream::myProcNo() == 0)
    {
        for(int i = 0; i < nBeams; i++)
        {
            perturbVectors.append(List<vector>(nSamplePoints,vector::zero));
            for(int j = 0; j < nSamplePoints; j++)
            {
                perturbVectors[i][j] = perturb*(2.0*rndGen.vector01()-vector::one);
            }
        }
    }
    Pstream::scatter(perturbVectors);

    // Build the controlCellID list of lists.
    for(int i = 0; i < nBeams; i++)
    {
        controlCellID.append(List<label>(nSamplePoints,-1));
    }
  
    // Identify the control cell IDs.
    findControlProcAndCell();
}


void Foam::scanningLidar::findControlProcAndCell()
{
    label nBeams = beamScanPatternTime.size();
    label nSamplePoints = beamDistribution.size();
    label totalSamplePoints = nBeams*nSamplePoints;
    label iter = 0;

    List<scalar> minDisLocal(totalSamplePoints,1.0E30);
    List<scalar> minDisGlobal(totalSamplePoints,1.0E30);

    for(int i = 0; i < nBeams; i++)
    {
        for(int j = 0; j < nSamplePoints; j++)
        {
            label cellID = 0;
            scalar minDis = 1.0E6;
            forAll(mesh_.C(),k)
            {
                scalar dis = mag(mesh_.C()[k] - (samplePoints[i][j] + perturbVectors[i][j]));
                if (dis <= minDis)
                {
                    cellID = k;
                    minDis = dis;
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
        for(int j = 0; j < nSamplePoints; j++)
        {
            if(minDisGlobal[iter] != minDisLocal[iter])
            {
                controlCellID[i][j] = -1;
            }
            iter++;
        }
    }
}


void Foam::scanningLidar::sampleWinds(label i, volTensorField& gradU)
{
    label nSamplePoints = beamDistribution.size();
    label iter = i * nSamplePoints;
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


vector Foam::scanningLidar::rotateVector(vector v, vector rotationPoint, vector axis, scalar angle)
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



void Foam::scanningLidar::rotateLidar()
{
    // Store the old beam angles.
    scalar rotationOld = rotationCurrent;
    scalar elevationOld = elevationCurrent;

    // Get the current beam angles.
    rotationCurrent = interpolateXY(tLidar,beamAngleTime,beamAngleRotation);
    elevationCurrent = interpolateXY(tLidar,beamAngleTime,beamAngleElevation);

    // Find the change in beam angle.
    scalar deltaRotation = (rotationCurrent - rotationOld)*degRad;
    scalar deltaElevation = (elevationCurrent - elevationOld)*degRad;

    // If the change in angle is finite, then perform the rotation.
    if ((mag(deltaRotation) > 0.0) || (mag(deltaElevation) > 0.0))
    {
        Info << "Rotating the lidar unit..." << endl;
        label nBeams = beamScanPatternTime.size();
        label nSamplePoints = beamDistribution.size();
        for(int i = 0; i < nBeams; i++)
        {
            beamScanPatternVector[i] = rotateVector(beamScanPatternVector[i],vector::zero,beamRotationAxis,deltaRotation);
            beamScanPatternVector[i] = rotateVector(beamScanPatternVector[i],vector::zero,beamElevationAxis,deltaElevation);
            for(int j = 0; j < nSamplePoints; j++)
            {
                samplePoints[i][j] = rotateVector(samplePoints[i][j],beamOrigin,beamRotationAxis,deltaRotation);
                samplePoints[i][j] = rotateVector(samplePoints[i][j],beamOrigin,beamElevationAxis,deltaElevation);
            }
        }
        beamElevationAxis = rotateVector(beamElevationAxis,vector::zero,beamRotationAxis,deltaRotation);

        // Because points moved, the owner cell, etc. must be recomputed.
        findControlProcAndCell();    
    }
}


void Foam::scanningLidar::execute()
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

                        Info << "sampling beam number " << beamScanPatternI << tab;
                        Info << "t = " << tLidar << tab;
                        Info << "tElapsed = " << tElapsed << endl;

                        // rotate the lidar.
                        rotateLidar();
 
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
                    writeBeamData();

                    // reset the beam index.
                    beamScanPatternI = 0;

                    // check to see where the time between scans puts us.  Does it spill over
                    // into the next time step or not?
                    tElapsed += timeBetweenScans;

                    Info << "Between scan " << tab;
                    Info << "t = " << tLidar << tab;
                    Info << "tElapsed = " << tElapsed << endl;
                }
            }
            else
            {
                tCarryOver -= dtSolver;
            }
        } 
        while ((tElapsed - dtSolver) < -1.0E-12);

        tCarryOver = tElapsed - dtSolver - tCarryOverSubtractor;

        Info << endl << endl;
    }
}


void Foam::scanningLidar::end()
{
    if (active_)
    {
        execute();
    }
}


void Foam::scanningLidar::timeSet()
{
    // Do nothing
}


void Foam::scanningLidar::write()
{
    // Do nothing
}


void Foam::scanningLidar::writeBeamData()
{
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
        label nSamplePoints = beamDistribution.size();
        label iter = 0;
        outputFile.reset(new OFstream(rootDir/tLidarName/"beamOrientation"));
        outputFile() << "#t (s)" << tab << "beam number" << tab << "beam orientation unit vector" << endl;
        forAll(beamScanPatternTime,i)
        {
            outputFile() << tLidar - beamScanPatternTime[nBeams-1] + beamScanPatternTime[i] << " " << i << " " << beamScanPatternVector[i] << endl;
        }

        // 2.) u-velocity.
        outputFile.reset(new OFstream(rootDir/tLidarName/"uVel"));
        outputFile() << "#t (s)" << tab << "beam number" << tab << "u-velocity component (m/s)" << endl;
        iter = 0;
        forAll(beamScanPatternTime,i)
        {
            outputFile() << tLidar - beamScanPatternTime[nBeams-1] + beamScanPatternTime[i] << " " << i << " "; 
            for(int j = 0; j < nSamplePoints; j++)
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
            outputFile() << tLidar - beamScanPatternTime[nBeams-1] + beamScanPatternTime[i] << " " << i << " ";
            for(int j = 0; j < nSamplePoints; j++)
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
            outputFile() << tLidar - beamScanPatternTime[nBeams-1] + beamScanPatternTime[i] << " " << i << " ";
            for(int j = 0; j < nSamplePoints; j++)
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
            outputFile() << tLidar - beamScanPatternTime[nBeams-1] + beamScanPatternTime[i] << " " << i << " ";
            for(int j = 0; j < nSamplePoints; j++)
            {
                scalar los = sampledWindVectors[iter] & beamScanPatternVector[i];
                outputFile() << los << " ";
                iter ++;
            }
            outputFile() << endl;
        }
    }
}


void Foam::scanningLidar::writeVariables()
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
    Info << "beamOrigin: " << beamOrigin << endl;
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
