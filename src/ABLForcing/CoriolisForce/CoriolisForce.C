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

#include "CoriolisForce.H"


// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

namespace Foam
{
    defineTypeNameAndDebug(CoriolisForce, 0);
}


// * * * * * * * * * * * * * Private Member Functions  * * * * * * * * * * * //

void Foam::CoriolisForce::update()
{
    // Compute the Coriolis force (neglect the component in the vertical direction).
    bodyForce_ = -2.0*(Omega_^U_);
    if (upIndex_ == 0)
    {
        forAll(bodyForce_,cellI)
        {
            bodyForce_[cellI].x() = 0.0;
        }
    }
    else if (upIndex_ == 1)
    {
        forAll(bodyForce_,cellI)
        {
            bodyForce_[cellI].y() = 0.0;
        }
    }
    else if (upIndex_ == 2)
    {
        forAll(bodyForce_,cellI)
        {
            bodyForce_[cellI].z() = 0.0;
        }
    }
}

// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

Foam::CoriolisForce::CoriolisForce
(
    const volVectorField& U,
    const label upIndex
)
:
    // Set the pointer to runTime
    runTime_(U.time()),

    // Set the pointer to the mesh
    mesh_(U.mesh()),

    // Set the pointer to the velocity field
    U_(U),

    // Set upIndex
    upIndex_(upIndex),

    // Initialize the reference velocity field
    Omega_
    (
        IOobject
        (
            "Omega_",
            runTime_.constant(),
            mesh_,
            IOobject::NO_READ,
            IOobject::NO_WRITE
        ),
        dimensionedVector("Omega_", dimensionSet(0, 0, -1, 0, 0, 0, 0), vector::zero)
    ),

    // Initialize the body force field
    bodyForce_
    (
        IOobject
        (
            "bodyForce_",
            runTime_.timeName(),
            mesh_,
            IOobject::NO_READ,
            IOobject::NO_WRITE
        ),
        mesh_,
        dimensionedVector("bodyForce",dimensionSet(0, 1, -2, 0, 0, 0, 0),vector::zero)
    )


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

    // Planetary rotation period (hours)
    scalar planetaryRotationPeriod(readScalar(ABLProperties.lookup("planetaryRotationPeriod")));

    // Latitude on the planetary body (degrees)
    scalar latitude(readScalar(ABLProperties.lookup("latitude")));
    
    // Compute the planetar rotation vector
    vector Omega;
    Omega.x() = 0.0;
    Omega.y() =
        (
            ( 2.0 * Foam::constant::mathematical::pi ) /
            ( max(1.0E-5,planetaryRotationPeriod)*3600.0)
        ) *
        Foam::cos( latitude*Foam::constant::mathematical::pi/180.0 );
    Omega.z() =
        (
            ( 2.0 * Foam::constant::mathematical::pi ) /
            ( max(1.0E-5,planetaryRotationPeriod)*3600.0)
        ) *
        Foam::sin( latitude*Foam::constant::mathematical::pi/180.0 );

    Omega_ = dimensionedVector("Omega", dimensionSet(0, 0, -1, 0, 0, 0, 0), Omega);

    Info << "Creating Coriolis force object" << endl;
    Info << Omega_ << endl;


}


// * * * * * * * * * * * * * * * * Destructor  * * * * * * * * * * * * * * * //

Foam::CoriolisForce::~CoriolisForce()
{}


// ************************************************************************* //
