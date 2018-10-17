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
    Foam::CoriolisForce

Description
    Coriolis force object

SourceFiles
    CoriolisForce.C

\*---------------------------------------------------------------------------*/

#ifndef CoriolisForce_H
#define CoriolisForce_H

#include "fvCFD.H"
#include "IOdictionary.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// Forward declaration of classes

/*---------------------------------------------------------------------------*\
                           Class CoriolisForce Declaration
\*---------------------------------------------------------------------------*/

class CoriolisForce
{
    // Private data

        //- Constants
            //- Runtime pointer
            const Time& runTime_;;
    
            //- Mesh pointer
            const fvMesh& mesh_;
    
            //- Velocity field pointer
            const volVectorField& U_;

            //- upIndex
            const label upIndex_;

        //- Planetary rotation vector field
        uniformDimensionedVectorField Omega_;

        //- Coriolis force
        volVectorField bodyForce_;


public:

    //- Declare name of the class and its debug switch
    ClassName("CoriolisForce");


    // Constructors
    CoriolisForce
    (
        const volVectorField& U,
        const label upIndex
    );


    // Destructor
    virtual ~CoriolisForce();


    // Public Member functions

        //- Update Coriolis force
        void update();

        //- Return force
        volVectorField& force()
        {
            return bodyForce_;
        }


};

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
