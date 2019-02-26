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

    lmax_
    (
        "lmax",
        dimLength,
        1.0
    ),

    L_
    (
        "L",
        dimLength,
        1.0
    ),

    alphaB_
    (
        IOobject
        (
            "alphaB",
            runTime_.timeName(),
            mesh_,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
       ),
        mesh_,
        dimensionedScalar("alphaB",dimless,1.00)
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

    Prt_(transportDict_.lookup("Prt")),

    upVec_(vector::zero)
{
    bound(k_, kMin_);
    bound(epsilon_, epsilonMin_);

    nut_ = Cmu_*sqr(k_)/epsilon_;
    nut_.correctBoundaryConditions();

    upVec_ = -g_.value()/mag(g_.value());

    printCoeffs();
}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //
void kEpsilonABL::computeLengthScale()
{
    // Compute friction velocity and wall heat flux.
    vectorField faceAreaNormal;
    scalarField faceArea;
    vectorField qwallLocal;
    vectorField RwallNormal;
    const fvPatchList& patches = mesh_.boundary();

    forAll(patches, patchi)
    {
        if (isA<wallFvPatch>(patches[patchi]))
        {
            faceAreaNormal = patches[patchi].Sf();
            faceArea = patches[patchi].magSf();
            vectorField qwallLocal = patches[patchi].lookupPatchField<volVectorField,vector>(qwallName_);
            symmTensorField Rwall = patches[patchi].lookupPatchField<volSymmTensorField,symmTensor>(RwallName_);
            RwallNormal = Rwall & faceAreaNormal;
        }
    } 

    scalar faceAreaSum = gSum(faceArea);

    vector RwallAvg = gSum(RwallNormal)/faceAreaSum;
    scalar tauWall = sqrt(pow(RwallAvg.x(),2) + pow(RwallAvg.y(),2));
    dimensionedScalar uStar("uStar",dimVelocity,max(sqrt(tauWall),1.0E-10));

    vector qwallAvg = gSum(qwallLocal)/faceAreaSum;
    if (mag(qwallAvg) == 0.0)
    {
       vector tiny = -1.0E-10*upVec_;
       qwallAvg += tiny;
    }
    dimensionedVector qwall("qwall",dimTemperature*dimVelocity,qwallAvg);

  //Info << "uStar = " << uStar << endl;
  //Info << "qwall = " << qwall << endl;
  //Info << "TRef = " << TRef_ << endl;
  //Info << "g = " << g_ << endl;

    // Compute Obukhov length.
    L_ = -pow(uStar,3)*TRef_/(kappa_*(-g_ & qwall));
  //Info << "L = " << L_ << endl;
  //Info << "upVec = " << upVec_ << endl;
 
    // Compute lm.
    forAll(lm_, i)
    {
        scalar z = mesh_.C()[i] & upVec_;
        scalar zeta = z/L_.value();
        scalar phiM = 1.0;
        if (zeta < 0.0)
        {
           phiM = pow((1.0 - 5.0*zeta),-0.25);
        }
        else
        {
           phiM = 1.0 + 5.0*zeta;
        }
        lm_[i] = ((kappa_*z)/(phiM + (kappa_*z/lambda_))).value();
    }

    // Compute lmax.
    scalar numerator = 0.0;
    scalar denominator = 0.0;
    forAll(k_, i)
    {
        numerator += sqrt(k_[i]) * (mesh_.C()[i] & upVec_) * mesh_.V()[i];
        denominator += sqrt(k_[i]) * mesh_.V()[i];
    }
    
    reduce(numerator,sumOp<scalar>());
    reduce(denominator,sumOp<scalar>());

    dimensionedScalar n("n",dimVelocity*dimLength,numerator);
    dimensionedScalar d("d",dimVelocity,min(denominator,1.0E-10));

    lmax_ = Clambda_ * (n/d);
  //Info << "lmax = " << lmax_ << endl;
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

    // Update all the length scales.
    computeLengthScale();

    // Compute the shear production term.
    volScalarField G("kEpsilonABL:G", 2.0*nut_*magSqr(symm(fvc::grad(U_))));
  //dimensionedScalar Gmin("Gmin",dimVelocity*dimVelocity/dimTime,1.0E-10);
  //bound(G, 1.0E-10);

    forAll(G,i)
    {
        if (G[i] == 0.0)
        {
            G[i] = 1.0E-10;
        }
    }
    forAll(G.boundaryField(),b)
    {
        forAll(G.boundaryField()[b],i)
        {
            if (G.boundaryField()[b][i] == 0.0)
            {
                G.boundaryField()[b][i] = 1.0E-10;
            }
        }
    }


    // Compute the buoyancy production term.
    volScalarField B("kEpsilonABL:B",(1.0/TRef_)*g_&((nut_/Prt_)*fvc::grad(T_)));

    // Compute the local gradient Richardson number.
    volScalarField Ri = B/G;

    // Compute alphaB.
    forAll(alphaB_,i)
    {
        if (Ri[i] > 0.0)
        {
            alphaB_[i] = 1.0 - lm_[i]/lmax_.value();
        }
        else
        {
            alphaB_[i] = 1.0 - (1.0 - (Ceps2_.value() - 1.0) / (Ceps2_.value() - Ceps1_.value())) * lm_[i]/lmax_.value();
        }
    }

    // Compute Ceps1Star.
    Ceps1Star_ = Ceps1_ + (Ceps2_ - Ceps1_)*(lm_/lmax_);
  //Info << "Ceps1Star = " << Ceps1Star_ << endl;

    // Compute Ceps3
    Ceps3_ = (Ceps1_ - Ceps2_)*alphaB_ + 1.0;
  //Info << "Ceps3 = " << Ceps3_ << endl;
    
    // Update epsilon and G at the wall
    epsilon_.boundaryField().updateCoeffs();

    // Dissipation equation
    tmp<fvScalarMatrix> epsEqn
    (
        fvm::ddt(epsilon_)
      + fvm::div(phi_, epsilon_)
      - fvm::laplacian(DepsilonEff(), epsilon_)
     ==
        Ceps1Star_*G*epsilon_/k_
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
