/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2011-2013 OpenFOAM Foundation
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

#include "kEpsilonABL.H"
#include "addToRunTimeSelectionTable.H"
#include "wallFvPatch.H"
#include "backwardsCompatibilityWallFunctions.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace incompressible
{
namespace RASModels
{

// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

defineTypeNameAndDebug(kEpsilonABL, 0);
addToRunTimeSelectionTable(RASModel, kEpsilonABL, dictionary);

// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

kEpsilonABL::kEpsilonABL
(
    const volVectorField& U,
    const surfaceScalarField& phi,
    transportModel& transport,
    const word& turbulenceModelName,
    const word& modelName
)
:
    RASModel(modelName, U, phi, transport, turbulenceModelName),

    Cmu_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "Cmu",
            coeffDict_,
            0.03
        )
    ),

    Clambda_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "Clamda",
            coeffDict_,
            0.075
        )
    ),

    Ceps1_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "Ceps1",
            coeffDict_,
            1.52
        )
    ),

    Ceps2_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "Ceps2",
            coeffDict_,
            1.833
        )
    ),

    sigmak_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "sigmak",
            coeffDict_,
            2.95
        )
    ),

    sigmaEps_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "sigmaEps",
            coeffDict_,
            2.95
        )
    ),

    kappa_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "kappa",
            coeffDict_,
            0.41
        )
    ),

    lambda_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "lambda",
            coeffDict_,
            0.00037
        )
    ),

    lMax_
    (
        "lMax",
        dimLength,
        1.0
    ),

    L_
    (
        "L",
        dimLength,
        1.0
    ),

    Ceps1Star_
    (
        IOobject
        (
            "Ceps1Star",
            runTime_.timeName(),
            mesh_,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
       ),
        mesh_,
        dimensionedScalar("Ceps1Star",dimless,1.52)
    ),

    Ceps3_
    (
        IOobject
        (
            "Ceps3",
            runTime_.timeName(),
            mesh_,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
       ),
        mesh_,
        dimensionedScalar("Ceps3",dimless,1.0)
    ),

    lm_
    (
        IOobject
        (
            "lm",
            runTime_.timeName(),
            mesh_,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
       ),
        mesh_,
        dimensionedScalar("lm",dimLength,1.0)
    ),

    k_
    (
        IOobject
        (
            "k",
            runTime_.timeName(),
            mesh_,
            IOobject::NO_READ,
            IOobject::AUTO_WRITE
        ),
        autoCreateK("k", mesh_)
    ),

    epsilon_
    (
        IOobject
        (
            "epsilon",
            runTime_.timeName(),
            mesh_,
            IOobject::NO_READ,
            IOobject::AUTO_WRITE
        ),
        autoCreateEpsilon("epsilon", mesh_)
    ),

    nut_
    (
        IOobject
        (
            "nut",
            runTime_.timeName(),
            mesh_,
            IOobject::NO_READ,
            IOobject::AUTO_WRITE
        ),
        autoCreateNut("nut", mesh_)
    ),

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

    TName_
    (
        coeffDict_.lookupOrDefault<word>("TName","T")
    ),

    RwallName_
    (
        coeffDict_.lookupOrDefault<word>("RwallName","Rwall")
    ),

    qwallName_
    (
        coeffDict_.lookupOrDefault<word>("qwallName","qwall")
    ),

    T_(U.db().lookupObject<volScalarField>(TName_)),

    g_(U.db().lookupObject<uniformDimensionedVectorField>("g")),

    TRef_(transportDict_.lookup("TRef")),

    Prt_(transportDict_.lookup("Prt"))
{
    bound(k_, kMin_);
    bound(epsilon_, epsilonMin_);

    nut_ = Cmu_*sqr(k_)/epsilon_;
    nut_.correctBoundaryConditions();

    printCoeffs();
}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //
void kEpsilonABL::computeLengthScale()
{
    // Compute friction velocity and wall heat flux.
    vectorField faceAreaNormal;
    scalarField faceArea;
    vectorField qwall;
    vectorField RwallNormal;
    const fvPatchList& patches = mesh_.boundary();

    forAll(patches, patchi)
    {
        if (isA<wallFvPatch>(patches[patchi]))
        {
            faceAreaNormal = patches[patchi].Sf();
            faceArea = patches[patchi].magSf();
            vectorField qwall = patches[patchi].lookupPatchField<volVectorField,vector>(qwallName_);
            symmTensorField Rwall = patches[patchi].lookupPatchField<volSymmTensorField,symmTensor>(RwallName_);
            RwallNormal = Rwall & faceAreaNormal;
        }
    } 

    scalar faceAreaSum = gSum(faceArea);
    vector qwallAvg = gSum(qwall)/faceAreaSum;
    vector RwallAvg = gSum(RwallNormal)/faceAreaSum;

    scalar tauWall = sqrt(pow(RwallAvg.x(),2) + pow(RwallAvg.y(),2));
    scalar uStar = sqrt(tauWall);

    // Compute Obukhov length.
    L_ = -pow(uStar,3)*TRef_.value()/(kappa_.value()*(g_.value() & qwallAvg));

    // Compute lm.
    scalar gMag = mag(g_);
    forAll(lm_, i)
    {
        scalar z = -(mesh_.C() & g_)/gMag;
        scalar zeta = z/L_;
        scalar phiM;
        if (zeta < 0.0)
        {
           phiM = pow((1.0-5.0*zeta),-0.25);
        }
        else
        {
           phiM = 1.0 + 5.0*zeta;
        }
        lm_[i] = (kappa_*z)/(phiM + (kappa_*z/lambda_))
    }

    // Compute lmax.
    scalar numerator = 0.0;
    scalar denominator = 0.0;
    forAll(k_, i)
    {
    }
    
}

tmp<volSymmTensorField> kEpsilonABL::R() const
{
    return tmp<volSymmTensorField>
    (
        new volSymmTensorField
        (
            IOobject
            (
                "R",
                runTime_.timeName(),
                mesh_,
                IOobject::NO_READ,
                IOobject::NO_WRITE
            ),
            ((2.0/3.0)*I)*k_ - nut_*twoSymm(fvc::grad(U_)),
            k_.boundaryField().types()
        )
    );
}


tmp<volSymmTensorField> kEpsilonABL::devReff() const
{
    return tmp<volSymmTensorField>
    (
        new volSymmTensorField
        (
            IOobject
            (
                "devRhoReff",
                runTime_.timeName(),
                mesh_,
                IOobject::NO_READ,
                IOobject::NO_WRITE
            ),
           -nuEff()*dev(twoSymm(fvc::grad(U_)))
        )
    );
}


tmp<fvVectorMatrix> kEpsilonABL::divDevReff(volVectorField& U) const
{
    return
    (
      - fvm::laplacian(nuEff(), U)
      - fvc::div(nuEff()*dev(T(fvc::grad(U))))
    );
}


tmp<fvVectorMatrix> kEpsilonABL::divDevRhoReff
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


bool kEpsilonABL::read()
{
    if (RASModel::read())
    {
        Cmu_.readIfPresent(coeffDict());
        Ceps1_.readIfPresent(coeffDict());
        Ceps2_.readIfPresent(coeffDict());
        sigmaEps_.readIfPresent(coeffDict());

        return true;
    }
    else
    {
        return false;
    }
}


void kEpsilonABL::correct()
{
    RASModel::correct();

    if (!turbulence_)
    {
        return;
    }

    // Compute the shear production term.
    volScalarField G("kEpsilonABL:G", 2.0*nut_*magSqr(symm(fvc::grad(U_))));

    // Compute the buoyancy production term.
    volScalarField B("kEpsilonABL:B",(1.0/TRef_)*g_&((nut_/Prt_)*fvc::grad(T_)));

    // Update epsilon and G at the wall
    epsilon_.boundaryField().updateCoeffs();

    // Dissipation equation
    tmp<fvScalarMatrix> epsEqn
    (
        fvm::ddt(epsilon_)
      + fvm::div(phi_, epsilon_)
      - fvm::laplacian(DepsilonEff(), epsilon_)
     ==
        Ceps1_*G*epsilon_/k_
      + Ceps3_*B*epsilon_/k_
      - fvm::Sp(Ceps2_*epsilon_/k_, epsilon_)
    );

    epsEqn().relax();

    epsEqn().boundaryManipulate(epsilon_.boundaryField());

    solve(epsEqn);
    bound(epsilon_, epsilonMin_);


    // Turbulent kinetic energy equation
    tmp<fvScalarMatrix> kEqn
    (
        fvm::ddt(k_)
      + fvm::div(phi_, k_)
      - fvm::laplacian(DkEff(), k_)
     ==
        G
      + B
      - fvm::Sp(epsilon_/k_, k_)
    );

    kEqn().relax();
    solve(kEqn);
    bound(k_, kMin_);


    // Re-calculate viscosity
    nut_ = Cmu_*sqr(k_)/epsilon_;
    nut_.correctBoundaryConditions();
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace RASModels
} // End namespace incompressible
} // End namespace Foam

// ************************************************************************* //
