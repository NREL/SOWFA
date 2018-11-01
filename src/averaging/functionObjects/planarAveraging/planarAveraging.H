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
    Foam::planarAveraging

Group
    grpUtilitiesFunctionObjects

Description
    This function computes planar averaged values and writes the data
    to file as the solver runs.

SourceFiles
    planarAveraging.C
    IOplanarAveraging.H

\*---------------------------------------------------------------------------*/

#ifndef planarAveraging_H
#define planarAveraging_H

#include "fvCFD.H"
#include "IOobjectList.H"
#include "meshPlanes.H"


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// Forward declaration of classes
class objectRegistry;
class dictionary;
class mapPolyMesh;

/*---------------------------------------------------------------------------*\
                       Class planarAveraging Declaration
\*---------------------------------------------------------------------------*/

class planarAveraging
{
    // Private data
    
        //- Name of this set of planarAveraging objects
        word name_;

        //- Reference to the mesh database
        const fvMesh& mesh_;

        //- Mesh planes
        meshPlanes zPlanes_;

        //- Load fields from files (not from objectRegistry)
        const bool loadFromFiles_;

        //- Output path
        fileName outputPath_;

        //- Names of fields to sample
        wordReList fieldSelection_;

        //- Current open files
        HashPtrTable<OFstream> filePtrs_;

        //- Process additional fields
        bool includeUU_;
        bool includewUU_;
        bool includeTU_;


    // Private Member Functions
        
        // Create and initialize geometric fields with
        // planar deviations for all requested fields
        // of type GeoField
        template<class GeoField>
        void createPlanarDevFields();

        // Update planar deviation fields
        // of type GeometricField<Type, fvPatchField, volMesh>
        template<class Type>
        void updatePlanarDevFields();

        // Create and initialize turbulence statistics fields
        void createTurbulenceStatFields();
        
        // Update turbulence statistics fields
        void updateTurbulenceStatFields();
        
        // Open file streams
        void openFiles();

        // Update all fields
        void updateFields();

        // Prepare object upon creation
        void prepare();

        //- Sample and write a particular volume field
        template<class Type>
        void sampleAndWrite
        (
            const GeometricField<Type, fvPatchField, volMesh>& vField
        );

        //- Sample and write all sampled fields
        template<class Type> void sampleAndWrite(const IOobjectList& objects);

        //- Disallow default bitwise copy construct
        planarAveraging(const planarAveraging&);

        //- Disallow default bitwise assignment
        void operator=(const planarAveraging&);


public:

    //- Runtime type information
    TypeName("planarAveraging");


    // Constructors

        //- Construct for given objectRegistry and dictionary.
        //  Allow the possibility to load fields from files
        planarAveraging
        (
            const word& name,
            const objectRegistry&,
            const dictionary&,
            const bool loadFromFiles = false
        );


    //- Destructor
    virtual ~planarAveraging();


    // Member Functions

        //- Return name of the set of planarAveraging
        virtual const word& name() const
        {
            return name_;
        }

        //- Read the planarAveraging data
        virtual void read(const dictionary&);

        //- Execute, currently does nothing
        virtual void execute();

        //- Execute at the final time-loop
        virtual void end();

        //- Called when time was set at the end of the Time::operator++
        virtual void timeSet();

        //- Standard write function.
        virtual void write();

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

#ifdef NoRepository
#   include "planarAveragingTemplates.C"
#endif

#endif

// ************************************************************************* //
