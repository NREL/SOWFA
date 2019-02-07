/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2011 OpenFOAM Foundation
     \\/     M anipulation  |
-------------------------------------------------------------------------------
License
    This file is part of OpenFOAM.

    OpenFOAM is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the
    Free Software Foundation; either version 3 of the License, or (at your
    option) any later version.

    OpenFOAM is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License
    along with OpenFOAM; if not, write to the Free Software Foundation,
    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

\*---------------------------------------------------------------------------*/

#include "dynLagrangianCsBound.H"
#include "addToRunTimeSelectionTable.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace incompressible
{
namespace LESModels
{

// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

defineTypeNameAndDebug(dynLagrangianCsBound, 0);
addToRunTimeSelectionTable(LESModel, dynLagrangianCsBound, dictionary);

// * * * * * * * * * * * * * Private Member Functions  * * * * * * * * * * * //

void dynLagrangianCsBound::updateSubGridScaleFields
(
    const tmp<volTensorField>& gradU
)
{
    Cs_ = Foam::sqrt(flm_/fmm_);
    // Bound Cs
    Cs_ = Foam::max(Cs_,CsMin);
    Cs_ = Foam::min(Cs_,CsMax);
  //nuSgs_ = Foam::sqr(Cs_)*delta()*sqrt(k(gradU));
    nuSgs_ = Foam::sqr(Cs_)*sqr(delta())*sqrt(2.0*magSqr(dev(symm(gradU))));
    nuSgs_.correctBoundaryConditions();
}


// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

dynLagrangianCsBound::dynLagrangianCsBound
(
    const volVectorField& U,
    const surfaceScalarField& phi,
    transportModel& transport,
    const word& turbulenceModelName,
    const word& modelName
)
:
    LESModel(modelName, U, phi, transport, turbulenceModelName),
    GenEddyVisc(U, phi, transport),
    CsMin(0.07),
    CsMax(0.14),
    flm_
    (
        IOobject
        (
            "flm",
            runTime_.timeName(),
            mesh_,
            IOobject::MUST_READ,
            IOobject::AUTO_WRITE
        ),
        mesh_
    ),
    fmm_
    (
        IOobject
        (
            "fmm",
            runTime_.timeName(),
            mesh_,
            IOobject::MUST_READ,
            IOobject::AUTO_WRITE
        ),
        mesh_
    ),
    Cs_
    (
        IOobject
        (
            "Cs",
            runTime_.timeName(),
            mesh_,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        Foam::sqrt(flm_/fmm_)
    ),
    theta_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "theta",
            coeffDict_,
            1.5
        )
    ),
    simpleFilter_(U.mesh()),
    filterPtr_(LESfilter::New(U.mesh(), coeffDict())),
    filter_(filterPtr_()),
    flm0_("flm0", flm_.dimensions(), 0.0),
    fmm0_("fmm0", fmm_.dimensions(), VSMALL)
{
    updateSubGridScaleFields(fvc::grad(U));

    printCoeffs();
}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

void dynLagrangianCsBound::correct(const tmp<volTensorField>& gradU)
{
    LESModel::correct(gradU);

    volSymmTensorField S(dev(symm(gradU())));

    volScalarField magS(mag(S));

    volVectorField Uf(filter_(U()));

    volSymmTensorField Sf(dev(symm(fvc::grad(Uf))));

    volScalarField magSf(mag(Sf));

    volSymmTensorField L(dev(filter_(sqr(U())) - (sqr(filter_(U())))));

    volSymmTensorField M(2.0*sqr(delta())*(filter_(magS*S) - 4.0*magSf*Sf));

    volScalarField invT
    (
        (1.0/(theta_.value()*delta()))*pow(flm_*fmm_, 1.0/8.0)
    );

    volScalarField LM(L && M);

    fvScalarMatrix flmEqn
    (
        fvm::ddt(flm_)
      + fvm::div(phi(), flm_)
     ==
        invT*LM
      - fvm::Sp(invT, flm_)
    );

    flmEqn.relax();
    flmEqn.solve();

    bound(flm_, flm0_);

    volScalarField MM(M && M);

    fvScalarMatrix fmmEqn
    (
        fvm::ddt(fmm_)
      + fvm::div(phi(), fmm_)
     ==
        invT*MM
      - fvm::Sp(invT, fmm_)
    );

    fmmEqn.relax();
    fmmEqn.solve();

    bound(fmm_, fmm0_);

    updateSubGridScaleFields(gradU);
}


bool dynLagrangianCsBound::read()
{
    if (GenEddyVisc::read())
    {
        filter_.read(coeffDict());
        theta_.readIfPresent(coeffDict());

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
