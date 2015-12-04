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

#include "KosovicOneEqNBA.H"
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

defineTypeNameAndDebug(KosovicOneEqNBA, 0);
addToRunTimeSelectionTable(LESModel, KosovicOneEqNBA, dictionary);

// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

KosovicOneEqNBA::KosovicOneEqNBA
(
    const volVectorField& U,
    const surfaceScalarField& phi,
    transportModel& transport,
    const word& turbulenceModelName,
    const word& modelName
)
:
    // Inherit the properties of the LESModel class.
    LESModel(modelName, U, phi, transport, turbulenceModelName),


    // Define the model constants.
    cb_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
             "cb",
             coeffDict_,
             0.36
        )
    ),

    cs_
    (
        dimensioned<scalar>
        (
            "cs",
            Foam::sqrt((8.0*(1.0 + cb_))/(27.0*Foam::sqr(Foam::constant::mathematical::pi)))
        )
    ),

    ce_
    (
        dimensioned<scalar>
        (
            "ce",
            Foam::pow(8.0*Foam::constant::mathematical::pi/27.0,(1.0/3.0))*Foam::pow(cs_,(4.0/3.0))
        )
    ),

    ceps_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "ceps",
            coeffDict_,
            0.93
        )
    ),

    Ske_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "Ske",
            coeffDict_,
            0.5
        )
    ),

    c1_
    (
        dimensioned<scalar>
        (
            "c1",
            (Foam::sqrt(960.0)*cb_)/(7.0*(1.0+cb_)*0.5)
        )
    ),

    c2_
    (
        dimensioned<scalar>
        (
            "c2",
           -c1_
        )
    ),

    // Initialize the SGS energy variable.
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

    // Initialize the SGS viscosity variable.
    nuSgs_
    (
        IOobject
        (
            "nuSgs",
            runTime_.timeName(),
            mesh_,
            IOobject::MUST_READ,
            IOobject::AUTO_WRITE
        ),
        mesh_
    ),

    // Initialize the nonlinear part of the stress to something
    // that has the correct units.
    nonlinearStress_
    (
        "nonlinearStress",
        symm(delta()*delta()*(fvc::grad(U) & fvc::grad(U)))
    ),

    // Initialize the length scales (the grid length scale is already
    // there from the LESModel class).
    leps_
    (
        IOobject
        (
            "leps",
            runTime_.timeName(),
            mesh_,
            IOobject::READ_IF_PRESENT,
            IOobject::NO_WRITE
        ),
        delta()
    ),

    ln_
    (
        IOobject
        (
            "ln",
            runTime_.timeName(),
            mesh_,
            IOobject::READ_IF_PRESENT,
            IOobject::NO_WRITE
        ),
        delta()
    ),

    ls_
    (
        IOobject
        (
            "ls",
            runTime_.timeName(),
            mesh_,
            IOobject::READ_IF_PRESENT,
            IOobject::NO_WRITE
        ),
        delta()
    ),

    // Look up the name of the potential temperature variable.
    TName_
    (
        coeffDict_.lookupOrDefault<word>("TName","T")
    ),

    // Look up the name of the eddy diffusivity for the temperature
    // equation.
    kappatName_
    (
        coeffDict_.lookupOrDefault<word>("kappatName","kappat")
    ),

    // Get access to the potential temperature variable.
    T_(U.db().lookupObject<volScalarField>(TName_)),

    // Get access to the gravity vector.
    g_(U.db().lookupObject<uniformDimensionedVectorField>("g")),

    // Define the dictionary file to look for the reference
    // potential temperature.
    transportDict_
    (
        IOobject
        (
            "transportProperties",
            U.time().constant(),
            U.db(),
            IOobject::MUST_READ_IF_MODIFIED,
            IOobject::NO_WRITE
        )
    ),

    // Get the reference potential temperature.
    TRef_(transportDict_.lookup("TRef"))

{
    // Bound SGS energy from below so that it doesn't become
    // negative.
    bound(k_, kMin_);

    Info << "KosovicOneEqNBA Model Coefficients" << endl;
    Info << "    cb " << tab << tab << tab << cb_.value() << endl;
    Info << "    cs " << tab << tab << tab << cs_.value() << endl;
    Info << "    ce " << tab << tab << tab << ce_.value() << endl;
    Info << "    ceps " << tab << tab << ceps_.value() << endl;
    Info << "    S(ke) " << tab << tab << Ske_.value() << endl;
    Info << "    c1 " << tab << tab << tab << c1_.value() << endl;
    Info << "    c2 " << tab << tab << tab << c2_.value() << endl;
    Info << "    TName " << tab << tab << TName_ << endl;
    Info << "    kappatName " << tab << tab << kappatName_ << endl;
}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

// Returns the SGS stress tensor.
tmp<volSymmTensorField> KosovicOneEqNBA::B() const
{
    return ((2.0/3.0)*I)*k_ - nuSgs_*twoSymm(fvc::grad(U_)) + nonlinearStress_;
}

// Returns the combined SGS + viscous stress tensor (deviatoric part)
tmp<volSymmTensorField> KosovicOneEqNBA::devReff() const
{
    return -nuEff()*dev(twoSymm(fvc::grad(U()))) + nonlinearStress_;
}

// Returns the divergence of the deviatoric part of devBeff.  It treats
// as much of this implicitly as possible.
tmp<fvVectorMatrix> KosovicOneEqNBA::divDevReff(volVectorField& U) const
{
    return
    (
        fvc::div(nonlinearStress_)
      - fvm::laplacian(nuEff(), U)
      - fvc::div(nuEff()*dev(T(fvc::grad(U))))
    );
}

tmp<fvVectorMatrix> KosovicOneEqNBA::divDevRhoReff
(
    const volScalarField& rho,
    volVectorField& U
) const
{
    volScalarField muEff("muEff", rho*nuEff());

    return
    (
      - fvm::laplacian(muEff, U)
      - fvc::div(muEff*dev(T(fvc::grad(U))))
    );
}

// Compute the relevant length scales
void KosovicOneEqNBA::computeLengthScales()
{

    // Get the temperature gradient dotted with gravity (so, the vertical
    // component of the temperature gradient times gravity).  Also, get
    // the vertical component of the velocity gradient using gravity to find
    // vertical (rather than just assuming z is up).
    volScalarField gradTdotg = fvc::grad(T_) & g_;
    volVectorField gradUdotz = T(fvc::grad(U())) & (g_/mag(g_));
   
    forAll(gradTdotg,i)
    {
        // Compute buoyancy length scale.
        ln_[i] = 0.76*sqrt(k_[i])*sqrt(TRef_.value()/max(1.0E-6,mag(gradTdotg[i])));

        // Compute the shear length scale.
        ls_[i] = 2.76*sqrt(k_[i])/max(1.0E-6,sqrt(Foam::sqr(gradUdotz[i].x()) + Foam::sqr(gradUdotz[i].y())));

        // Compute the dissipation length scale.
        leps_[i] = 1.0/Foam::sqrt((1.0/(Foam::sqr(delta()[i])))+(1.0/(Foam::sqr(ln_[i])))+(1.0/(Foam::sqr(ls_[i]))));
    }
    gradTdotg.clear();
    gradUdotz.clear();
}

// Read the two model constants that must be set.
bool KosovicOneEqNBA::read()
{
    if (LESModel::read())
    {
        cb_.readIfPresent(coeffDict());
        ceps_.readIfPresent(coeffDict());
        Ske_.readIfPresent(coeffDict());
        return true;
    }
    else
    {
        return false;
    }
}

// Do the actual computation of the SGS model.
void KosovicOneEqNBA::correct(const tmp<volTensorField>& gradU)
{
    // Update the molecular viscosity, and the grid-dependent length scale.
    LESModel::correct(gradU);


    // Update the stability-dependent length scale.
    KosovicOneEqNBA::computeLengthScales();


    // Use the stability-dependent and grid-dependent length scales to form the
    // turbulent Prandtl number.
    volScalarField Prt = 1.0/(1.0 + (2.0*leps_/delta()));


    // Form the SGS-energy production terms, using old values of velocity and temperature.
    volSymmTensorField B = KosovicOneEqNBA::B();
    volScalarField P_shear = -(B && T(gradU));
    volScalarField P_buoyant = (1.0/TRef_)*g_&((nuSgs_/Prt)*fvc::grad(T_));


    // Build the SGS-energy equation matrix system.
    tmp<fvScalarMatrix> kEqn
    (
       fvm::ddt(k_)
     + fvm::div(phi(), k_)
     - fvm::laplacian(2.0*DkEff(), k_)
    ==
       P_shear
     + P_buoyant
     - fvm::Sp(ceps_*sqrt(k_)/leps_, k_)
    );


    // Solve the SGS-energy equation system.
    kEqn().relax();
    kEqn().solve();


    // Bound the SGS-energy to have a minimum value set by kMin_.
    bound(k_, kMin_);


    // Computes eddy viscosity and update the boundary conditions. There
    // are a couple of options on how to compute eddy viscosity with a 
    // nonlinear model.  It can be computed in the standard way as a
    // constant times a length scale times a velocity scale or it could
    // be computed as the least squares fit of the strain rate tensor to
    // the stress tensor.  We use the standard way following what Kosovic
    // shows for the diffusivity in the k-equation, in the linear part of
    // the stress-strain relation, and in the thermal eddy diffusivity.
    nuSgs_ = ce_*delta()*sqrt(k_);
    nuSgs_.correctBoundaryConditions();



    // Update the SGS thermal conductivity.
    volScalarField& kappat_ = const_cast<volScalarField&>(U().db().lookupObject<volScalarField>(kappatName_));
    kappat_ = nuSgs_/Prt;
//  kappat_.correctBoundaryConditions();   


    // Compute the nonlinear term.  First form the strain-rate tensor
    // S, and the rotation-rate tensor, W.  Note that W is not just the
    // skew-symmetric part of gradU, but has to be transposed because the
    // way OpenFOAM orders the gradient of a vector is transposed from
    // how we normally think about it.
    volSymmTensorField S = symm(fvc::grad(U()));
    volTensorField W = T(skew(fvc::grad(U())));
    nonlinearStress_ = -ce_ * delta() * delta() * Foam::pow(cs_,(2.0/3.0)) * Foam::pow((27.0/(8.0*Foam::constant::mathematical::pi)),(1.0/3.0)) *
    symm(
         c1_ * ((S & S) - ((1.0/3.0) * I * (S && S)))
       + c2_ * (twoSymm(S & W))
    );

}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace LESModels
} // End namespace incompressible
} // End namespace Foam

// ************************************************************************* //
