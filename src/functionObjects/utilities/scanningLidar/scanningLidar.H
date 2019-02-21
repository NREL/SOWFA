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
    Foam::scanningLidar

Group
    grpUtilitiesFunctionObjects

Description
    This function models the scan pattern for a generic scanning lidar.
    The scan pattern and scan times are user inputs.  The code repeats the
    scans and samples the velocity field and writes the data to file as
    the solver runs.

SourceFiles
    scanningLidar.C
    IOscanningLidar.H

\*---------------------------------------------------------------------------*/

#ifndef scanningLidar_H
#define scanningLidar_H

#include "functionObject.H"
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
                       Class scanningLidar Declaration
\*---------------------------------------------------------------------------*/

class scanningLidar
:
    public functionObject
{
    // Private data

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

        //- A list that defines the scan pattern.  The first column is time
        //  from zero to the completion of the pattern.  The remaining columns
        //  give the vector that the beam points along at that given time.
        List<scalar> beamScanPatternTime;
        List<vector> beamScanPatternVector;
        label beamScanPatternI;

        //- Time between scans.
        scalar timeBetweenScans;

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

        //- Value to perturb lidar sampling locations by to break ties when 
        //  searching for control processor.
        scalar perturb;

        //- Sampling points.
        List<List<vector> > samplePoints;

        //- Sampled winds.
        List<vector> sampledWindVectors;

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
        scanningLidar(const scanningLidar&);

        //- Disallow default bitwise assignment
        void operator=(const scanningLidar&);


public:

    //- Runtime type information
    TypeName("scanningLidar");


    // Constructors

        //- Construct for given objectRegistry and dictionary.
        //  Allow the possibility to load fields from files
        scanningLidar
        (
            const word& name,
            const objectRegistry&,
            const dictionary&,
            const bool loadFromFiles = false
        );


    //- Destructor
    virtual ~scanningLidar();


    // Member Functions

        //- Read the scanningLidar data
        virtual bool read(const dictionary&);

        //- Create the lidar sampling beams
        virtual void createBeams();

        //- Find the processor and cell that contains the sampling point.
        virtual void findControlProcAndCell();

        //- Sample the winds.
        virtual void sampleWinds(label i, volTensorField& gradU);

        //- General rotation function.
        virtual vector rotateVector(vector v, vector rotationPoint, vector axis, scalar angle);

        //- Rotate the lidar.
        virtual void rotateLidar();

        //- Execute, currently does nothing
        virtual bool execute();

        //- Execute at the final time-loop, currently does nothing
        virtual bool end();

        //- Standard write function.
        virtual bool write();

        //- Custom write function only called after a full scan.
        virtual void writeBeamData();

        //- Write out variables for debugging
        virtual void writeVariables();
};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
