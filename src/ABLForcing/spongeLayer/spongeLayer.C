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
    const word& name,
    const volVectorField& U
)
:
    // Set name
    name_(name),

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
            name_ & "Uref",
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
            name_ & "viscosity",
            runTime_.timeName(),
            mesh_,
            IOobject::NO_READ,
            IOobject::NO_WRITE
        ),
        mesh_,
        dimensionedScalar("viscosity_", dimensionSet(0, 0, -1, 0, 0, 0, 0), 0.0)
    ),

    // Initialize the body force field
    bodyForce_
    (
        IOobject
        (
            name_ & "force",
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
    
    const dictionary& spongeDict(ABLProperties.subOrEmptyDict(name_));

    type_ = spongeDict.lookupOrDefault<word>("type","none");

    // Sponge layer start location
    scalar startLocation = spongeDict.lookupOrDefault<scalar>("startLocation",0.0);

    // Sponge layer width
    scalar width = spongeDict.lookupOrDefault<scalar>("width",10000.0);

    // Maximum viscosity
    scalar viscosityMax = spongeDict.lookupOrDefault<scalar>("viscosityMax",0.0); 
    
    // Coordinate index
    label coordIndex = spongeDict.lookupOrDefault<label>("coordIndex",2);

    // Step up or step down
    word direction = spongeDict.lookupOrDefault<word>("direction","stepUp");

    // Create sponge layer reference velocity
    scalar Ux = spongeDict.lookupOrDefault<scalar>("Ux",0.0);
    scalar Uy = spongeDict.lookupOrDefault<scalar>("Uy",0.0);
    vector Uref;
    Uref.x() = Ux;
    Uref.y() = Uy;
    Uref.z() = 0.0;

    Uref_ = dimensionedVector("Uref", dimensionSet(0, 1, -1, 0, 0, 0, 0), Uref);
    
    if (type_ == "Rayleigh" || type_ == "viscous")
    {
        Info << "Adding " << name << " layer (" << type_ << " damping) in coordinate direction " << coordIndex;
        Info << " between " << startLocation << " and " << startLocation+width << " (" << direction;
        Info << ") with lambdaMax " << viscosityMax << endl;
    }

    // For a viscous type sponge layer, change the dimensions of the viscosity field from 1/s to m^2/s
    if (type_ == "viscous")
    {
        viscosity_.dimensions().reset(dimensionSet(0, 2, -1, 0, 0, 0, 0));
    }
    
    // Set viscosity to cosine profile between startLocation and startLocation+width,
    // For step up:   zero below startLocation and one  above startLocation+width
    // For step down: one  below startLocation and zero above startLocation+width
    scalar fact = 1.0; //stepUp
    if (direction == "stepDown")
    {
        fact = -1.0;
    }

    forAll(mesh_.cells(),cellI)
    {
        scalar loc = mesh_.C()[cellI][coordIndex];
        viscosity_[cellI]  = (loc<=startLocation) * (1.0 - fact);
        viscosity_[cellI] += ((loc>startLocation) && (loc<startLocation+width)) *
            (
                1.0 - fact * Foam::cos
                    (
                        Foam::constant::mathematical::pi * (loc - startLocation)/width
                    )

            );
        viscosity_[cellI] += (loc>=startLocation+width) * (1.0 + fact);
        viscosity_[cellI] *= 0.5 * viscosityMax;

    }

    forAll(viscosity_.boundaryField(),i)
    {
        if ( !mesh_.boundary()[i].coupled() )
        {
            forAll(viscosity_.boundaryField()[i],j)
            {
                scalar loc = mesh_.boundary()[i].Cf()[j][coordIndex];
                viscosity_.boundaryField()[i][j]  = (loc<=startLocation) * (1.0 - fact);
                viscosity_.boundaryField()[i][j] += ((loc>startLocation) && (loc<startLocation+width)) *
                    (
                        1.0 - fact * Foam::cos
                            (
                                Foam::constant::mathematical::pi * (loc - startLocation)/width
                            )

                    );
                viscosity_.boundaryField()[i][j] += (loc>=startLocation+width) * (1.0 + fact);
                viscosity_.boundaryField()[i][j] *= 0.5 * viscosityMax;
            }
        }
    }


}


// * * * * * * * * * * * * * * * * Destructor  * * * * * * * * * * * * * * * //

Foam::spongeLayer::~spongeLayer()
{}


// ************************************************************************* //
