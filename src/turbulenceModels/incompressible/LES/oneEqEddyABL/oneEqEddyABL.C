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

#include "oneEqEddyABL.H"
#include "addToRunTimeSelectionTable.H"
#include "wallFvPatch.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace incompressible
{
namespace LESModels
{

// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

defineTypeNameAndDebug(oneEqEddyABL, 0);
addToRunTimeSelectionTable(LESModel, oneEqEddyABL, dictionary);


// * * * * * * * * * * * * * Private Member Functions  * * * * * * * * * * * //

void oneEqEddyABL::updateSubGridScaleFields()
{
    // Compute the eddy-viscosity field using the SGS-energy and stability-
    // dependent lengthscale.
    nuSgs_ = ck_*sqrt(k_)*l_;

    // Update boundary conditions on eddy-viscosity
    nuSgs_.correctBoundaryConditions();
}


// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

oneEqEddyABL::oneEqEddyABL
(
    const volVectorField& U,
    const surfaceScalarField& phi,
    transportModel& transport,
    const word& turbulenceModelName,
    const word& modelName
)
:
    // Inherit properties from the LESModel and GenEddyViscABL classes.
    LESModel(modelName, U, phi, transport, turbulenceModelName),
    GenEddyViscABL(U, phi, transport),

    // Create the SGS-energy field.
    k_
    (
        IOobject
        (
            "k",
            runTime_.timeName(),
            mesh_,
            IOobject::MUST_READ,
            IOobject::AUTO_WRITE
        ),
        mesh_
    ),

    // Read the Ck input parameter.
    ck_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "ck",
            coeffDict_,
            0.1
        )
    ),

    // Create the Ce input parameter field.
    ceField_
    (
        IOobject
        (
            "ceField",
            runTime_.timeName(),
            mesh_,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
       ),
        mesh_,
        dimensionedScalar("ceField",dimensionSet(0,0,0,0,0,0,0),0.93)
    )

{
    // Bound SGS energy from below so that it isn't negative.
    bound(k_, kMin_);

    // Update the SGS viscosity.
    updateSubGridScaleFields();

    printCoeffs();
}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

void oneEqEddyABL::correct(const tmp<volTensorField>& gradU)
{
    // Update the molecular viscosity, and the grid-dependent length scale.
    GenEddyViscABL::correct(gradU);


    // Update the stability-dependent length scale.
    GenEddyViscABL::computeLengthScale();


    // Use the stability-dependent and grid-dependent length scales to form the 
    // turbulent Prandtl number.
    volScalarField Prt = 1.0/(1.0 + (2.0*l_/delta()));


    // Ce is stability dependent, so set it here.  In Moeng's 1984 paper, she says
    // ce = 0.19 + (0.51*l_/delta()), but later in Moeng and Wyngaard's 1988 paper,
    // they say that ce = 0.93 is in better agreement with theory and yields better
    // results.  Therefore, this should be revised to ce = 0.19 + (0.74*l_/delta()).
    // Here we keep the original variable ce, but allow the user to specify
    // the base value, i.e, the value when l = delta.
    ceField_ = (ce_/0.93) * (0.19 + (0.74*l_/delta()));


    // Ce is also to be set to 3.9 at the lowest level.
    const fvPatchList& patches = mesh_.boundary();
    forAll(patches, patchi)
    {
        if (isA<wallFvPatch>(patches[patchi]))
        {
            forAll(patches[patchi], faceI)
            {
                label cellI = patches[patchi].faceCells()[faceI];
                ceField_[cellI] = 3.9;
            }
        }
    }


    // Form the SGS-energy production terms, using old values of velocity and temperature.
    tmp<volScalarField> P_shear = 2.0*nuSgs_*magSqr(symm(gradU));
    tmp<volScalarField> P_buoyant = (1.0/TRef_)*g_&((nuSgs_/Prt)*fvc::grad(T_));


    // Build the SGS-energy equation matrix system.
    tmp<fvScalarMatrix> kEqn
    (
       fvm::ddt(k_)
     + fvm::div(phi(), k_)
     - fvm::laplacian(2.0*DkEff(), k_)
    ==
       P_shear
     + P_buoyant
     - fvm::Sp(ceField_*sqrt(k_)/l_, k_)
    );


    // Solve the SGS-energy equation system.
    kEqn().relax();
    kEqn().solve();


    // Bound the SGS-energy to have a minimum value set by kMin_.
    bound(k_, kMin_);

   
    // Call the function that computes eddy viscosity.
    updateSubGridScaleFields();


    // Update the SGS thermal diffusivity.
    volScalarField& kappat_ = const_cast<volScalarField&>(U().db().lookupObject<volScalarField>(kappatName_));
    kappat_ = nuSgs_/Prt;
//  kappat_.correctBoundaryConditions();
}


bool oneEqEddyABL::read()
{
    if (GenEddyViscABL::read())
    {
        ck_.readIfPresent(coeffDict());

        return true;
    }
    else
    {
        return false;
    }
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace LESModels
} // End namespace incompressible
} // End namespace Foam

// ************************************************************************* //
