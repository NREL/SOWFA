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
    Foam::spongeLayer

Description
    Sponge layer for damping out vertically propagating gravity waves
    Possible sponge layer types are "Rayleigh", "viscous" or "none".
    - The "Rayleigh" type means that the damping term is computed as nu*(u_ref-u)
      The viscosity coefficient nu has dimensions of 1/s
    - The "viscous" type means that the damping term is computed as nu * Lapl(u)
      The viscosity coefficient nu has dimensions of m**2/s
    - The "none" type means no damping is added

SourceFiles
    spongeLayer.C

\*---------------------------------------------------------------------------*/

#ifndef spongeLayer_H
#define spongeLayer_H

#include "fvCFD.H"
#include "IOdictionary.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// Forward declaration of classes

/*---------------------------------------------------------------------------*\
                           Class spongeLayer Declaration
\*---------------------------------------------------------------------------*/

class spongeLayer
{
    // Private data

        //- Constants
            //- Runtime pointer
            const Time& runTime_;;
    
            //- Mesh pointer
            const fvMesh& mesh_;
    
            //- Velocity field pointer
            const volVectorField& U_;

        //- Type of sponge layer
        word type_;

        //- Reference velocity field
        uniformDimensionedVectorField Uref_;

        //- Viscosity field
        volScalarField viscosity_;

        //- Body force field of the sponge layer
        volVectorField bodyForce_;


public:

    //- Declare name of the class and its debug switch
    ClassName("spongeLayer");


    // Constructors
    spongeLayer
    (
        const volVectorField& U
    );


    // Destructor
    virtual ~spongeLayer();


    // Public Member functions

        //- Update sponge layer fore
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
