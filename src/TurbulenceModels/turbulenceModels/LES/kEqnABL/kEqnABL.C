/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2011-2017 OpenFOAM Foundation
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

#include "kEqnABL.H"
#include "fvOptions.H"
#include "bound.H"
#include "wallFvPatch.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace LESModels
{

// * * * * * * * * * * * * Protected Member Functions  * * * * * * * * * * * //

template<class BasicTurbulenceModel>
void kEqnABL<BasicTurbulenceModel>::correctNut()
{
    // Compute the eddy-viscosity field using the SGS-energy and stability-
    // dependent lengthscale and apply boundary conditions.
    this->nut_ = Ck_*sqrt(k_)*this->l_;
    this->nut_.correctBoundaryConditions();
    fv::options::New(this->mesh_).correct(this->nut_);

    BasicTurbulenceModel::correctNut();
}


template<class BasicTurbulenceModel>
tmp<fvScalarMatrix> kEqnABL<BasicTurbulenceModel>::kSource() const
{
    return tmp<fvScalarMatrix>
    (
        new fvScalarMatrix
        (
            k_,
            dimVolume*this->rho_.dimensions()*k_.dimensions()
            /dimTime
        )
    );
}


// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

template<class BasicTurbulenceModel>
kEqnABL<BasicTurbulenceModel>::kEqnABL
(
    const alphaField& alpha,
    const rhoField& rho,
    const volVectorField& U,
    const surfaceScalarField& alphaRhoPhi,
    const surfaceScalarField& phi,
    const transportModel& transport,
    const word& propertiesName,
    const word& type
)
:
    // Inherit properties from the atmospheric-boundary-layer-specific
    // LES eddy viscosity class.
    LESeddyViscosityABL<BasicTurbulenceModel>
    (
        type,
        alpha,
        rho,
        U,
        alphaRhoPhi,
        phi,
        transport,
        propertiesName
    ),

    // Create the subgrid-scale turbulent kinetic energy field.
    k_
    (
        IOobject
        (
            IOobject::groupName("k", this->alphaRhoPhi_.group()),
            this->runTime_.timeName(),
            this->mesh_,
            IOobject::MUST_READ,
            IOobject::AUTO_WRITE
        ),
        this->mesh_
    ),

    // Read the Ck model constant.
    Ck_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "Ck",
            this->coeffDict_,
            0.1
        )
    ),

    // Create the Ce model parameter field.
    CeField_
    (
       IOobject
       (
           "CeField",
           this->runTime_.timeName(),
           this->mesh_,
           IOobject::READ_IF_PRESENT,
           IOobject::AUTO_WRITE
       ),
       this->mesh_,
       dimensionedScalar("CeField",dimless,0.93)
    )
{
    // Bound the SGS energy from below so that it is not negative.
    bound(k_, this->kMin_);

    if (type == typeName)
    {
        this->printCoeffs(type);
    }
}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

template<class BasicTurbulenceModel>
bool kEqnABL<BasicTurbulenceModel>::read()
{
    if (LESeddyViscosityABL<BasicTurbulenceModel>::read())
    {
        Ck_.readIfPresent(this->coeffDict());

        return true;
    }
    else
    {
        return false;
    }
}


template<class BasicTurbulenceModel>
tmp<volScalarField> kEqnABL<BasicTurbulenceModel>::epsilon() const
{
    return tmp<volScalarField>
    (
        new volScalarField
        (
            IOobject
            (
                IOobject::groupName("epsilon", this->alphaRhoPhi_.group()),
                this->runTime_.timeName(),
                this->mesh_,
                IOobject::NO_READ,
                IOobject::NO_WRITE
            ),
            this->Ce_*k()*sqrt(k())/this->delta()
        )
    );
}


template<class BasicTurbulenceModel>
void kEqnABL<BasicTurbulenceModel>::correct()
{
    if (!this->turbulence_)
    {
        return;
    }

    // Local references
    const alphaField& alpha = this->alpha_;
    const rhoField& rho = this->rho_;
    const surfaceScalarField& alphaRhoPhi = this->alphaRhoPhi_;
    const volVectorField& U = this->U_;
    volScalarField& nut = this->nut_;
    fv::options& fvOptions(fv::options::New(this->mesh_));

    // Update the molecular viscosity and the grid-based length scale.
    LESeddyViscosityABL<BasicTurbulenceModel>::correct();

    // Update the stability-dependent length scale.
    LESeddyViscosityABL<BasicTurbulenceModel>::computeLengthScale();

    // Use the stability-dependent and grid-dependent length scales to form the
    // turbulent Prandtl number.
    volScalarField Prt = 1.0/(1.0 + (2.0*this->l_/this->delta()));

    // Ce is stability dependent, so set it here.  In Moeng's paper (1984), she says
    // Ce = 0.19 + (0.51*l_/delta()), but later in Moeng and Wyngaard's paper (1988),
    // they say that Ce = 0.93 is in better agreement with theory and yields better
    // results.  Therefore, this should be revised to Ce = 0.19 + (0.74*l_/delta()).
    // Here we keep the original variable Ce, but allow the user to specify
    // the base value, i.e, the value when l = delta.
    CeField_ = (this->Ce_/0.93) * (0.19 + (0.74*this->l_/this->delta()));

    // Ce is also to be set to 3.9 at the lowest level following Moeng (1984).
    const fvPatchList& patches = this->mesh_.boundary();
    forAll(patches, patchi)
    {
        if (isA<wallFvPatch>(patches[patchi]))
        {
            forAll(patches[patchi], faceI)
            {
                label cellI = patches[patchi].faceCells()[faceI];
                CeField_[cellI] = 3.9;
            }
        }
    }

    volScalarField divU(fvc::div(fvc::absolute(this->phi(), U)));

    // Compute the velocity gradient tensor.
    tmp<volTensorField> tgradU(fvc::grad(U));

    // Compute the shear-production term.
    volScalarField G(this->GName(), nut*(tgradU() && dev(twoSymm(tgradU()))));
    tgradU.clear();

    // Compute the buoyancy-production term.
    volScalarField B("B", (1.0/this->TRef_) * this->g_ & (nut/Prt) * fvc::grad(this->T_));

    // Assemble the matrix system to be solved to solve for k.
    tmp<fvScalarMatrix> kEqnABL
    (
        fvm::ddt(alpha, rho, k_)                       // time-derivative
      + fvm::div(alphaRhoPhi, k_)                      // advection
      - fvm::laplacian(2.0*alpha*rho*DkEff(), k_)      // diffusion
     ==
        alpha*rho*G                                    // shear production
      - fvm::SuSp((2.0/3.0)*alpha*rho*divU, k_)        // shear production
      + alpha*rho*B                                    // buoyancy production
      - fvm::Sp(CeField_*alpha*rho*sqrt(k_)/this->l_, k_)    // dissipation
      + kSource()                                      // extra source
      + fvOptions(alpha, rho, k_)                      // optional extra term
    );

    // Solve the matrix system for k
    kEqnABL.ref().relax();
    fvOptions.constrain(kEqnABL.ref());
    solve(kEqnABL);

    fvOptions.correct(k_);
 
    // Bound the subgrid-scale turbulent kinetic energy to have a minimum
    bound(k_, this->kMin_);

    // Compute the eddy viscosity.
    correctNut();

    // Update the subgrid-scale thermal diffusivity
    volScalarField& kappat_ = const_cast<volScalarField&>(U().db().lookupObject<volScalarField>(this->kappatName_));
    kappat_ = nut/Prt;
    kappat_.correctBoundaryConditions();
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace LESModels
} // End namespace Foam

// ************************************************************************* //
