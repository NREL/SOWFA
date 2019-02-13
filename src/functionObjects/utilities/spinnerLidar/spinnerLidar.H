/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2012-2013 OpenFOAM Foundation
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

Class
    Foam::spinnerLidar

Group
    grpUtilitiesFunctionObjects

Description
    This function models the scan pattern of DTU's spinner lidar.  It
    samples the flow field and writes the information to file at a pre-
    specified frequency.

SourceFiles
    spinnerLidar.C
    IOspinnerLidar.H

\*---------------------------------------------------------------------------*/

#ifndef spinnerLidar_H
#define spinnerLidar_H

#include "fvCFD.H"
#include "Random.H"


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// Forward declaration of classes
class objectRegistry;
class dictionary;
class mapPolyMesh;

/*---------------------------------------------------------------------------*\
                       Class spinnerLidar Declaration
\*---------------------------------------------------------------------------*/

class spinnerLidar
{
    // Private data

        //- Name of this set of spinnerLidar objects
        word name_;

        //- Reference to the mesh database
        const fvMesh& mesh_;

        //- Runtime pointer.
        const Time& runTime_;

        //- On/off switch
        bool active_;

        //- Degrees to radians conversion factor.
        const scalar degRad;

        //- Name of velocity field (optional)
        word UName_;

        //- Sampled vector field pointer.
        const volVectorField& U_;

        //- Current solver time step size.
        scalar dtSolver;

        //- Current simulation time.
        word time;
        scalar tSolver;
        scalar tLidar;
        scalar tCycle;
        scalar tElapsed;
        scalar tCarryOver;

        //- Random number generator.
        Random rndGen;

        //- Output file pointer.
        autoPtr<OFstream> outputFile;

        //- Local domain bounding box.
        const pointField& meshPoints;

        //- A list that defines the scan pattern.  The first column is time
        //  from zero to the completion of the pattern.  The remaining columns
        //  give the vector that the beam points along at that given time.
        List<scalar> beamScanPatternTime;
        List<vector> beamScanPatternVector;
        label beamScanPatternI;

        //- Does the scan pattern repeat.  Depending on the motor speed, sampling
        //  rates, etc., the scan segments may or may not repeat.  If they do not
        //  repeat, then for each time through the scan pattern, the interpolation
        //  cell locations need to be relocated, which is time consuming.  If
        //  scanRepeats is set to true, the scan segments are forced to repeat 
        //  location (even if that means the beam has to backtrack slightly to
        //  begin a new scan), and the relocating of interpolation cells is not
        //  required.
        bool scanRepeats;

        //- Time between scans.
        scalar timeBetweenScans;

        //- DTU Spinner Lidar specific input parameters
        scalar motorRPM;
        scalar oneSecScanMotorRPM;
        scalar gearRatioMotorToPrism1;
        scalar gearRatioPrism1ToPrism2;
        scalar prism1Angle;
        scalar prism2Angle;
        scalar sampleRate;
        scalar samplePeriod;
        label samplesPerSpectrum;
        label spectraPerAverageSample;
        scalar averageSampleRate;
        scalar averageSamplePeriod;
        label virtualSamplesPerAverageSample;
        scalar virtualSamplePeriod;
        scalar scanPeriod;
        label nBeamPoints;
        label nAverageSamples;
        label nVirtualSamples;

        //- Origin of lidar beam.
        vector beamOrigin;

        //- Lidar beam maximum sample distance.
        scalar beamMaxDistance;

        //- A list that defines the beam sampling point distribution.
        List<scalar> beamDistribution;

        //- A list the defines the beam rotation and elevation angles vs. time.
        scalarField beamAngleTime;
        scalarField beamAngleRotation;
        scalarField beamAngleElevation;

        //- The axis about which beam rotation and elevation changes are commanded.  
        //  Use the right-hand rule.
        vector beamRotationAxis;
        vector beamElevationAxis;
        vector beamElevationAxisOriginal;

        //- Value to perturb lidar sampling locations by to break ties when 
        //  searching for control processor.
        scalar perturb;

        //- Last position of the prisms and beam.  At start time, these are pointed horizontally
        //  to the right as viewed from the lidar looking downstream.  For prism 1, this is
        //  downsteam; for prism 2, this is the prism 1 angle; for the beam, this is the angle of the
        //  the combined prisms.
        vector lastPrism1Axis;
        vector lastPrism2Axis;
        vector lastBeamAxis;

        //- Sampling points.
        List<List<vector> > samplePoints;

        //- Sampled winds.
        List<vector> sampledWindVectors;

        //- Prism rotation rates.
        scalar rr1;
        scalar rr2;

        //- Perturbation vector for breaking processor ties.
        List<List<vector> > perturbVectors;

        //- ID of cell that this point lies within.  If the value is -1, then 
        //  this sample point does not lie on this processor.
        List<List<label> > controlCellID;

        //- The lidar current rotation angle and beam elevation.  Here we use the
        //  right-hand rule convention to give the rotation angle relative to
        //  the original position of the given beamScanPatternVector about the
        //  rotation axis.  Same with the elevation angle.
        scalar rotationCurrent;
        scalar elevationCurrent;

        //- Output files.
        OFstream* outFile_; 


    // Private Member Functions

        //- Disallow default bitwise copy construct
        spinnerLidar(const spinnerLidar&);

        //- Disallow default bitwise assignment
        void operator=(const spinnerLidar&);


public:

    //- Runtime type information
    TypeName("spinnerLidar");


    // Constructors

        //- Construct for given objectRegistry and dictionary.
        //  Allow the possibility to load fields from files
        spinnerLidar
        (
            const word& name,
            const objectRegistry&,
            const dictionary&,
            const bool loadFromFiles = false
        );


    //- Destructor
    virtual ~spinnerLidar();


    // Member Functions

        //- Return name of the set of spinnerLidar
        virtual const word& name() const
        {
            return name_;
        }

        //- Read the spinnerLidar data
        virtual void read(const dictionary&);

        //- Reset the beam position
        virtual void resetAxes();

        //- Create the lidar sampling beams
        virtual void createScanPattern();

        //- Find the processor and cell that contains the sampling point.
        virtual void findControlProcAndCell();

        //- Sample the winds.
        virtual void sampleWinds(label i, volTensorField& gradU);

        //- General rotation function.
        virtual vector rotateVector(vector v, vector rotationPoint, vector axis, scalar angle);

        //- Rotate the lidar.
        virtual void rotateLidar();

        //- Execute, currently does nothing
        virtual void execute();

        //- Execute at the final time-loop, currently does nothing
        virtual void end();

        //- Called when time was set at the end of the Time::operator++
        virtual void timeSet();

        //- Standard write function.
        virtual void write();

        //- Custom write function only called after a full scan.
        virtual void writeBeamDataFormatted();

        //- Write out variables for debugging
        virtual void writeVariables();

        //- Update for changes of mesh
        virtual void updateMesh(const mapPolyMesh&)
        {}

        //- Update for changes of mesh
        virtual void movePoints(const polyMesh&)
        {}
};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
