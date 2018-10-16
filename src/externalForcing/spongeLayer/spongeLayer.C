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

#include "spongeLayer.H"


// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

namespace Foam
{
    defineTypeNameAndDebug(spongeLayer, 0);
}


// * * * * * * * * * * * * * Private Member Functions  * * * * * * * * * * * //

void Foam::spongeLayer::update()
{
    // Compute the sponge layer damping force
    if (type_ == "Rayleigh")
    {
        bodyForce_ = viscosity_ * (Uref_ - U_);
    }
    else if (type_ == "viscous")
    {
        bodyForce_ = fvc::laplacian(viscosity_,U_);
    }
}

// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

Foam::spongeLayer::spongeLayer
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

    // Initialize the reference velocity field
    Uref_
    (
        IOobject
        (
            "Uref_",
            runTime_.constant(),
            mesh_,
            IOobject::NO_READ,
            IOobject::NO_WRITE
        ),
        dimensionedVector("Uref_", dimensionSet(0, 1, -1, 0, 0, 0, 0), vector::zero)
    ),

    // Initialize the viscosity field
    viscosity_
    (
        IOobject
        (
            "viscosity_",
            runTime_.timeName(),
            mesh_,
            IOobject::NO_READ,
            IOobject::AUTO_WRITE
        ),
        mesh_,
        dimensionedScalar("viscosity_", dimensionSet(0, 0, -1, 0, 0, 0, 0), 0.0)
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

    type_ = ABLProperties.lookupOrDefault<word>("spongeLayerType","none");

    // Sponge layer base height
    scalar baseHeight   = ABLProperties.lookupOrDefault<scalar>("spongeLayerBaseHeight",0.0);

    // Sponge layer top height
    scalar topHeight    = ABLProperties.lookupOrDefault<scalar>("spongeLayerTopHeight",10000.0);

    // Sponge layer viscosity at the top boundary
    scalar viscosityTop = ABLProperties.lookupOrDefault<scalar>("spongeLayerViscosityTop",0.0);

    // Create sponge layer reference velocity
    scalar Ux = ABLProperties.lookupOrDefault<scalar>("spongeLayerUx",0.0);
    scalar Uy = ABLProperties.lookupOrDefault<scalar>("spongeLayerUy",0.0);
    vector Uref;
    Uref.x() = Ux;
    Uref.y() = Uy;
    Uref.z() = 0.0;

    Uref_ = dimensionedVector("Uref", dimensionSet(0, 1, -1, 0, 0, 0, 0), Uref);
    
    if (type_ == "Rayleigh" || type_ == "viscous")
    {
        Info << "Adding " << type_ << " damping layer between " << baseHeight << " and " << topHeight;
        Info << " with lambdaMax " << viscosityTop << endl;
    }

    // For a viscous type sponge layer, change the dimensions of the viscosity field from 1/s to m^2/s
    if (type_ == "viscous")
    {
        viscosity_.dimensions().reset(dimensionSet(0, 2, -1, 0, 0, 0, 0));
    }
    
    // Set viscosity to cosine profile between baseHeight and topHeight
    // and zero below baseHeight
    forAll(mesh_.cells(),cellI)
    {
        scalar z = mesh_.C()[cellI][upIndex];
        viscosity_[cellI] = viscosityTop * 0.5 *
            (
                1.0 - Foam::cos
                    (
                        Foam::constant::mathematical::pi *
                        max( (z - baseHeight)/(topHeight - baseHeight) , 0.0 )
                    )

            );
    }

    forAll(viscosity_.boundaryField(),i)
    {
        if ( !mesh_.boundary()[i].coupled() )
        {
            forAll(viscosity_.boundaryField()[i],j)
            {
                scalar z = mesh_.boundary()[i].Cf()[j].z();
                viscosity_.boundaryField()[i][j] = viscosityTop * 0.5 *
                    (
                        1.0 - Foam::cos
                            (
                                Foam::constant::mathematical::pi *
                                max( (z - baseHeight)/(topHeight - baseHeight) , 0.0 )
                            )

                    );
            }
        }
    }


}


// * * * * * * * * * * * * * * * * Destructor  * * * * * * * * * * * * * * * //

Foam::spongeLayer::~spongeLayer()
{}


// ************************************************************************* //
