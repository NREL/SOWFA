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

#include "buoyancyModel.H"


// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

namespace Foam
{
    defineTypeNameAndDebug(buoyancyModel, 0);
}


// * * * * * * * * * * * * * Private Member Functions  * * * * * * * * * * * //

void Foam::buoyancyModel::updateBuoyancyTerm()
{
    // Compute the buoyancy term, depending on the definition of
    // the background pressure

    if (backgroundPressureType_ == "noSplit")
    {
        buoyancyTerm_ = ((g_ & mesh_.Sf())/mesh_.magSf()) * fvc::interpolate(rhok_);
    }
    else if (backgroundPressureType_ == "rho0Split")
    {
        buoyancyTerm_ = ((g_ & mesh_.Sf())/mesh_.magSf()) * fvc::interpolate(rhok_ - 1.0);
    }
    else if (backgroundPressureType_ == "rhokSplit")
    {
        buoyancyTerm_ = -ghf_*fvc::snGrad(rhok_);
    }
}

void Foam::buoyancyModel::updateBackgroundPressure()
{
    if (backgroundPressureType_ == "noSplit")
    {
        pBackground_ = 0.0;
    }
    else if (backgroundPressureType_ == "rho0Split")
    {
        pBackground_ = gh_;
    }
    else if (backgroundPressureType_ == "rhokSplit")
    {
        pBackground_ = rhok_ * gh_;
    }
}

void Foam::buoyancyModel::updateDensityField()
{
    rhok_ = 1.0 - ( (T_ - TRef_)/TRef_ );
}

// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

Foam::buoyancyModel::buoyancyModel
(
    const volScalarField& T,
    const dimensionedScalar TRef,
    const dimensionedVector hRef
)
:
    // Set the pointer to runTime
    runTime_(T.time()),

    // Set the pointer to the mesh
    mesh_(T.mesh()),

    // Set the pointer to the temperature field
    T_(T),

    // Set the reference temperature
    TRef_(TRef),

    // Initialize the gravitational acceleration field
    g_(T.db().lookupObject<uniformDimensionedVectorField>("g")),

    // Initialize the Boussinesq density field
    rhok_
    (
        IOobject
        (
            "rhok",
            runTime_.timeName(),
            mesh_
        ),
        1.0 - ( (T - TRef)/TRef )
    ),

    // Initialize the gravity potential field
    gh_("gh", g_ & (mesh_.C() - hRef)),
    ghf_("ghf", g_ & (mesh_.Cf() - hRef)),

    // Initialize background pressure
    pBackground_("pBackground", rhok_*gh_),

    // Initialize the buoyancy term
    buoyancyTerm_("buoyancyTerm", -ghf_ * fvc::snGrad(rhok_))


{
    // Define dictionary with input data
    IOdictionary ABLProperties
    (
        IOobject
        (
            "ABLProperties",
            runTime_.time().constant(),
            runTime_,
            IOobject::MUST_READ,
            IOobject::NO_WRITE
        )
    );

    // PROPERTIES CONCERNING THE WAY IN WHICH THE BACKGROUND PRESSURE IS DEFINED

    // Options for defining the background pressure:
    // - noSplit:   do not split out hydrostatic part; pressure is then perturbation pressure.
    // - rho0Split: split out the hydrostatic part; define hydrostatic as rho_0 * g * z.
    // - rhokSplit: split out the hydrostatic part; define hydrostatic as rho_k * g * z.
    backgroundPressureType_ = ABLProperties.lookupOrDefault<word>("perturbationPressureType","rhokSplit");
    word backgroundOutput;
    if (backgroundPressureType_ == "noSplit")
    {
        backgroundOutput = "nothing";
    }
    else if (backgroundPressureType_ == "rho0Split")
    {
        backgroundOutput = "rho_0 * g * z";
    }
    else if (backgroundPressureType_ == "rhokSplit")
    {
        backgroundOutput = "rho_k * g * z";
    }
    Info << "Defining background hydrostatic pressure to be " << backgroundOutput << endl;


}


// * * * * * * * * * * * * * * * * Destructor  * * * * * * * * * * * * * * * //

Foam::buoyancyModel::~buoyancyModel()
{}


// ************************************************************************* //
