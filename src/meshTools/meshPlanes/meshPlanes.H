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

Class
    Foam::meshPlanes

Description
    Data structure containing a set of planes in a given direction

SourceFiles
    meshPlanes.C

\*---------------------------------------------------------------------------*/

#ifndef meshPlanes_H
#define meshPlanes_H

#include "fvCFD.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// Forward declaration of classes

/*---------------------------------------------------------------------------*\
                           Class meshPlanes Declaration
\*---------------------------------------------------------------------------*/

class meshPlanes
{
    // Private data

        //- Index corresponding to the normal to the planes
        label normalIndex_;

        //- Reference to mesh
        const fvMesh& mesh_;

        //- Total number of planes
        label numberOfPlanes_;

        //- Total volume per plane (global value)
        List<scalar> totVolPerPlane_;

        //- List of lists of cell ID labels corresponding to a specific plane on a processor
        List<List<label> > planesCellList_;

        //- Number of cells per plane on a processor
        labelList numCellPerPlane_;

        //- List of plane locations
        List<scalar> planeLocationValues_;


    // Private Member Functions

        //- Find cells per plane 
        void findPlanes();


public:

    //- Declare name of the class and its debug switch
    ClassName("meshPlanes");


    // Static member data

        //- tolerance on plane locations
        static scalar tol_;


    // Constructors

        //- Construct from components
        meshPlanes
        (
            const fvMesh& mesh,
            const word& normal
        );

        //- Construct default (z-planes)
        meshPlanes
        (
            const fvMesh& mesh
        );


    // Destructor
    virtual ~meshPlanes();


    // Member functions

        //- Compute planar average
        template<class Type>
        List<Type> average
        (
            const GeometricField<Type, fvPatchField, volMesh>& vField
        );

        //- Compute planar average at particular plane
        template<class Type>
        Type average
        (
            const GeometricField<Type, fvPatchField, volMesh>& vField,
            label planeI
        );

        //- Access
        
            label numberOfPlanes() const
            {
                return numberOfPlanes_;
            }
    
            List<scalar>& totVolPerPlane()
            {
                return totVolPerPlane_;
            }
    
            List<List<label> >& planesCellList()
            {
                return planesCellList_;
            }
    
            labelList& numCellPerPlane()
            {
                return numCellPerPlane_;
            }
    
            List<scalar>& planeLocationValues()
            {
                return planeLocationValues_;
            }


};

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

//- Loop across all planes in \a meshPlanes
// \par Usage
// \code
// forAllPlanes(meshPlanes, i)
// {
//      statements;
// }
// \endcode
// \sa forAllPlanes
#define forAllPlanes(meshPlanes, i) \
    for (Foam::label i=0; i<(meshPlanes).numberOfPlanes(); i++)

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
