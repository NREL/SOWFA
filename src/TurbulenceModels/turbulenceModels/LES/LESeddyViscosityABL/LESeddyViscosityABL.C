/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2013-2017 OpenFOAM Foundation
     \\/     M anipulation  | Copyright (C) 2016 OpenCFD Ltd.
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

#include "LESeddyViscosityABL.H"
#include "zeroGradientFvPatchField.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace LESModels
{

// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

template<class BasicTurbulenceModel>
LESeddyViscosityABL<BasicTurbulenceModel>::LESeddyViscosityABL
(
    const word& type,
    const alphaField& alpha,
    const rhoField& rho,
    const volVectorField& U,
    const surfaceScalarField& alphaRhoPhi,
    const surfaceScalarField& phi,
    const transportModel& transport,
    const word& propertiesName
)
:
    eddyViscosity<LESModel<BasicTurbulenceModel>>
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

    Ce_
    (
        dimensioned<scalar>::lookupOrAddToDict
        (
            "Ce",
            this->coeffDict_,
            0.93
        )
    ),

    l_
    (
        IOobject
        (
            "l",
            this->runTime_.timeName(),
            this->mesh_,
            IOobject::NO_READ,
            IOobject::NO_WRITE
        ),
        this->delta()
    ),

    TName_("T"),

    kappatName_("kappat"),

    T_(U.db().lookupObject<volScalarField>(TName_)),

    g_(U.db().lookupObject<uniformDimensionedVectorField>("g")),

    transportProperties_
    (
        IOobject
        (
            "transportProperties",
            this->runTime_.constant(),
            this->runTime_,
            IOobject::MUST_READ,
            IOobject::NO_WRITE,
            false
        )
    ),

    TRef_
    (
        "TRef",
        dimTemperature,
        transportProperties_
    )
{
    this->coeffDict_.readIfPresent("TName", TName_);
    this->coeffDict_.readIfPresent("kappatName", kappatName_);
}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

template<class BasicTurbulenceModel>
bool LESeddyViscosityABL<BasicTurbulenceModel>::read()
{
    if (eddyViscosity<LESModel<BasicTurbulenceModel>>::read())
    {
        Ce_.readIfPresent(this->coeffDict());

        return true;
    }
    else
    {
        return false;
    }
}


template<class BasicTurbulenceModel>
tmp<volScalarField> LESeddyViscosityABL<BasicTurbulenceModel>::epsilon() const
{
    tmp<volScalarField> tk(this->k());

    tmp<volScalarField> tepsilon
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
            Ce_*tk()*sqrt(tk())/this->delta(),
            zeroGradientFvPatchField<scalar>::typeName
        )
    );
    volScalarField& epsilon = tepsilon.ref();
    epsilon.correctBoundaryConditions();

    return tepsilon;
}

template<class BasicTurbulenceModel>
void LESeddyViscosityABL<BasicTurbulenceModel>::computeLengthScale()
{
    volScalarField gradTdotg = fvc::grad(T_) & g_;
    volScalarField k_(this->k());
    forAll(gradTdotg,i)
    {
        // neutral/unstable
        if (gradTdotg[i] >= 0.0)
        {
            l_[i] = this->delta()[i];
        }
        // stable
        else
        {
            l_[i] = min(this->delta()[i], 0.76*sqrt(k_[i])*sqrt(TRef_.value()/mag(gradTdotg[i])));
        }
    }
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace LESModels
} // End namespace Foam

// ************************************************************************* //
