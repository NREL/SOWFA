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

#include "GenEddyViscABL.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace incompressible
{
namespace LESModels
{

// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

defineTypeNameWithName(GenEddyViscABL, "GenEddyViscABL");


// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

GenEddyViscABL::GenEddyViscABL
(
    const volVectorField& U,
    const surfaceScalarField& phi,
    transportModel& transport,
    const word& turbulenceModelName,
    const word& modelName
)
:
    LESModel(modelName, U, phi, transport, turbulenceModelName),

    ce_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "ce",
            coeffDict_,
            0.93
        )
    ),
    
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

    l_
    (
        IOobject
        (
            "l",
            runTime_.timeName(),
            mesh_,
            IOobject::READ_IF_PRESENT,
            IOobject::NO_WRITE
        ),
        delta()
    ),

    TName_
    (
        coeffDict_.lookupOrDefault<word>("TName","T")
    ),

    kappatName_
    (
        coeffDict_.lookupOrDefault<word>("kappatName","kappat")
    ),

    T_(U.db().lookupObject<volScalarField>(TName_)),

    g_(U.db().lookupObject<uniformDimensionedVectorField>("g")),

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

    TRef_(transportDict_.lookup("TRef")) 


{
//    printCoeffs();
}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

tmp<volSymmTensorField> GenEddyViscABL::B() const
{
    return ((2.0/3.0)*I)*k() - nuSgs_*twoSymm(fvc::grad(U()));
}


tmp<volSymmTensorField> GenEddyViscABL::devBeff() const
{
    return -nuEff()*dev(twoSymm(fvc::grad(U())));
}


tmp<fvVectorMatrix> GenEddyViscABL::divDevBeff(volVectorField& U) const
{
    return
    (
      - fvm::laplacian(nuEff(), U) - fvc::div(nuEff()*dev(T(fvc::grad(U))))
    );
}

void GenEddyViscABL::computeLengthScale()
{
    volScalarField gradTdotg = fvc::grad(T_) & g_;
    volScalarField k_ = 1.0*k();
    forAll(gradTdotg,i)
    {
        // neutral/unstable
        if (gradTdotg[i] >= 0.0)
        {
            l_[i] = delta()[i];
        }
        // stable
        else
        {
            l_[i] = min(delta()[i], 0.76*sqrt(k_[i])*sqrt(TRef_.value()/mag(gradTdotg[i])));
        }
    }
    gradTdotg.clear();
    k_.clear();
}

void GenEddyViscABL::correct(const tmp<volTensorField>& gradU)
{
    LESModel::correct(gradU);
}


bool GenEddyViscABL::read()
{
    if (LESModel::read())
    {
        ce_.readIfPresent(coeffDict());

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
