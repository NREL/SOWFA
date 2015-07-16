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

#include "NonLinEddyViscABL.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace incompressible
{
namespace LESModels
{

// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

defineTypeNameWithName(NonLinEddyViscABL, "NonLinEddyViscABL");


// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

NonLinEddyViscABL::NonLinEddyViscABL
(
    const volVectorField& U,
    const surfaceScalarField& phi,
    transportModel& transport,
    const word& turbulenceModelName,
    const word& modelName
)
:

    GenEddyViscModel(U, phi, transport, turbulenceModelName, modelName),
    
    nonlinearStress_
    (
        IOobject
        (
            "nonlinearStress",
            runTime_.timeName(),
            mesh_
        ),
        mesh_,
        dimensionedSymmTensor
        (
            "nonlinearStress",
            sqr(dimVelocity),
            symmTensor::zero
        )
    )

{
//    printCoeffs();
}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

tmp<volSymmTensorField> NonLinEddyViscABL::B() const
{
   tmp<volSymmTensorField> tB
   (
      GenEddyViscModel::B()
   );
   tB() += nonlinearStress_;
   return tB;
}


tmp<volSymmTensorField> NonLinEddyViscABL::devBeff() const
{
    return -nuEff()*dev(twoSymm(fvc::grad(U())));

    tmp<volSymmTensorField> tdevBeff
    (
        GenEddyViscModel::devBeff()
    );
    tdevBeff() += nonlinearStress_;
    return tdevBeff;
}


tmp<fvVectorMatrix> NonLinEddyViscABL::divDevBeff(volVectorField& U) const
{

  //volSymmTensorField S = symm(fvc::grad(U));
    volTensorField S = 0.5*(fvc::grad(U) + T(fvc::grad(U)));
    volTensorField Omega = skew(fvc::grad(U));

    volTensorField SS = S & S;
    volScalarField SMag = magSqr(S);

    volTensorField SOmega = S & Omega;
    volTensorField OmegaS = Omega & S;

    return
    (
        GenEddyViscModel::divDevBeff(U)
      + fvc::div(nonlinearStress_)
    );


}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace LESModels
} // End namespace incompressible
} // End namespace Foam

// ************************************************************************* //
