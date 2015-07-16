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
    Foam::incompressible::LESModels::NonLinEddyViscABLABL

Description
    General base class for all incompressible models that can be implemented
    as a non linear eddy viscosity, i.e. algebraic and one-equation models. Modified 
    from original version to better handle the atmospheric SGS models.

    Contains fields for k (SGS turbulent kinetic energy), gamma
    (modelled viscosity) and epsilon (SGS dissipation).

SourceFiles
    NonLinEddyViscABL.C

\*---------------------------------------------------------------------------*/

#ifndef NonLinEddyViscABL_H
#define NonLinEddyViscABL_H

#include "LESModel.H"
#include "GenEddyViscABL.H"
#include "uniformDimensionedFields.H"
#include "IOdictionary.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace incompressible
{
namespace LESModels
{

/*---------------------------------------------------------------------------*\
                           Class NonLinEddyViscABL Declaration
\*---------------------------------------------------------------------------*/

class NonLinEddyViscABL
:
    virtual public LESModel
    virtual public GenEddyViscABL
{
    // Private Member Functions

        // Disallow default bitwise copy construct and assignment
        NonLinEddyViscABL(const NonLinEddyViscABL&);
        NonLinEddyViscABL& operator=(const NonLinEddyViscABL&);


protected:

    // Fields

        volSymmTensorField nonlinearStress_;


public:

    //- Partial Runtime type information
    static const word typeName;

    // Constructors

        //- Construct from components
        NonLinEddyViscABL
        (
            const volVectorField& U,
            const surfaceScalarField& phi,
            transportModel& transport,
            const word& turbulenceModelName = turbulenceModel::typeName,
            const word& modelName = typeName
        );


    //- Destructor
    virtual ~NonLinEddyViscABL()
    {}


    // Member Functions

        //- Return the sub-grid stress tensor.
        virtual tmp<volSymmTensorField> B() const;

        //- Return the effective sub-grid turbulence stress tensor
        //  including the laminar stress
        virtual tmp<volSymmTensorField> devBeff() const;

        //- Return the deviatoric part of the effective sub-grid
        //  turbulence stress tensor including the laminar stress
        virtual tmp<fvVectorMatrix> divDevBeff(volVectorField& U) const;

        //- Correct the nonlinear terms
        virtual void correctNonlinearStress(const volTensorField& gradU) = 0;
};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace LESModels
} // End namespace incompressible
} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
