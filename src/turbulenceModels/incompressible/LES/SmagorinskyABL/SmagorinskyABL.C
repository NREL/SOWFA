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

#include "SmagorinskyABL.H"
#include "addToRunTimeSelectionTable.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace incompressible
{
namespace LESModels
{

// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

defineTypeNameAndDebug(SmagorinskyABL, 0);
addToRunTimeSelectionTable(LESModel, SmagorinskyABL, dictionary);

// * * * * * * * * * * * * * Private Member Functions  * * * * * * * * * * * //

void SmagorinskyABL::updateSubGridScaleFields(const volTensorField& gradU)
{
    nuSgs_ = ck_*delta()*sqrt(k(gradU));
    nuSgs_.correctBoundaryConditions();
}


// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

SmagorinskyABL::SmagorinskyABL
(
    const volVectorField& U,
    const surfaceScalarField& phi,
    transportModel& transport,
    const word& turbulenceModelName,
    const word& modelName
)
:
    LESModel(modelName, U, phi, transport, turbulenceModelName),
    GenEddyViscABL(U, phi, transport),

    ck_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "ck",
            coeffDict_,
            0.094
        )
    )
{
    updateSubGridScaleFields(fvc::grad(U));

    printCoeffs();
}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

void SmagorinskyABL::correct(const tmp<volTensorField>& gradU)
{
    // Update the molecular viscosity, and the grid-dependent length scale.
    GenEddyViscABL::correct(gradU);

    // Update the stability-dependent length scale.
    GenEddyViscABL::computeLengthScale();

    // Use the stability-dependent and grid-dependent length scales to form the
    // turbulent Prandtl number.
    volScalarField Prt = 1.0/(1.0 + (2.0*l_/delta()));

    // Call the function that computes eddy viscosity.
    updateSubGridScaleFields(gradU());

    // Update the SGS thermal conductivity.
    volScalarField& kappat_ = const_cast<volScalarField&>(U().db().lookupObject<volScalarField>(kappatName_));
    kappat_ = nuSgs_/Prt;
}


bool SmagorinskyABL::read()
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
