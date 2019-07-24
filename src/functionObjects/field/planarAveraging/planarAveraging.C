/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2012-2014 OpenFOAM Foundation
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

#include "planarAveraging.H"
#include "dictionary.H"
#include "addToRunTimeSelectionTable.H"

// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

namespace Foam
{
    defineTypeNameAndDebug(planarAveraging, 0);

    addToRunTimeSelectionTable
    (
        functionObject,
        planarAveraging,
        dictionary
    );
}


// * * * * * * * * * * * * * Private Member Functions  * * * * * * * * * * * //

void Foam::planarAveraging::createTurbulenceStatFields()
{
    // Create and initialize Reynolds stress tensor UU
    if (includeUU_)
    {
        const volVectorField& Uprime = mesh_.thisDb().lookupObject<volVectorField>("UPlanarDev");

        new volSymmTensorField
        (
            IOobject
            (
                "UU",
                mesh_.time().timeName(),
                mesh_,
                IOobject::NO_READ,
                IOobject::NO_WRITE
            ),
            symm(Uprime*Uprime)
        );
    }

    // Create and initialize wUU tensor
    if (includewUU_)
    {
        const volVectorField& Uprime = mesh_.thisDb().lookupObject<volVectorField>("UPlanarDev");

        new volSymmTensorField
        (
            IOobject
            (
                "wUU",
                mesh_.time().timeName(),
                mesh_,
                IOobject::NO_READ,
                IOobject::NO_WRITE
            ),
            symm(Uprime*Uprime)
        );
    }

    // Create and initialize turbulent heat flux vector TU
    if (includeTU_)
    {
        const volVectorField& Uprime = mesh_.thisDb().lookupObject<volVectorField>("UPlanarDev");
        const volScalarField& Tprime = mesh_.thisDb().lookupObject<volScalarField>("TPlanarDev");

        new volVectorField
        (
            IOobject
            (
                "TU",
                mesh_.time().timeName(),
                mesh_,
                IOobject::NO_READ,
                IOobject::NO_WRITE
            ),
            Tprime*Uprime
        );
    }
}


void Foam::planarAveraging::updateTurbulenceStatFields()
{
    // Update Reynolds stress tensor UU
    if (includeUU_)
    {
        const volVectorField& Uprime = mesh_.thisDb().lookupObject<volVectorField>("UPlanarDev");

        symmTensorField& UU = 
            mesh_.thisDb().lookupObjectRef<symmTensorField>("UU");

        UU = symm(Uprime*Uprime);
    }

    // Update wUU tensor
    if (includewUU_)
    {
        const volVectorField& Uprime = mesh_.thisDb().lookupObject<volVectorField>("UPlanarDev");
        
        symmTensorField& wUU = 
            mesh_.thisDb().lookupObjectRef<symmTensorField>("wUU");

        wUU = symm(Uprime*Uprime);
        forAll(mesh_.cells(),cellI)
        {
            wUU[cellI] *= Uprime[cellI].z();
        }
    }

    // Update turbulent heat flux vector TU
    if (includeTU_)
    {
        const volVectorField& Uprime = mesh_.thisDb().lookupObject<volVectorField>("UPlanarDev");
        const volScalarField& Tprime = mesh_.thisDb().lookupObject<volScalarField>("TPlanarDev");

        vectorField& TU = 
            mesh_.thisDb().lookupObjectRef<vectorField>("TU");

        TU = Tprime*Uprime;
    }
}


void Foam::planarAveraging::openFiles()
{
    // Specify output path
    if (Pstream::parRun())
    {
        outputPath_ = mesh_.time().path()/".."/"postProcessing"/name()/mesh_.time().timeName();
    }
    else
    {
        outputPath_ = mesh_.time().path()/"postProcessing"/name()/mesh_.time().timeName();
    }

    if (Pstream::master)
    {
        // Create directory if it does not exist
        mkDir(outputPath_);

        // Open the statistics files
        forAll(fieldSelection_,fieldI)
        {
            const word& fieldName = fieldSelection_[fieldI];

            OFstream* fPtr = new OFstream(outputPath_/fieldName);

            OFstream& fout = *fPtr;

            filePtrs_.insert(fieldName, fPtr);

            fout << "Heights (m) ";
            forAllPlanes(zPlanes_,planeI)
            {
                fout << zPlanes_.planeLocationValues()[planeI] << " ";
            }
            fout << endl;

            fout << "Time(s)" << " " << "dt (s)" << " " << fieldName << endl;

        }
    }
}


void Foam::planarAveraging::updateFields()
{
    // Update planar deviation fields
    updatePlanarDevFields<scalar>();
    updatePlanarDevFields<vector>();
    updatePlanarDevFields<symmTensor>();

    // Update turbulence statistics fields
    updateTurbulenceStatFields();
}


void Foam::planarAveraging::prepare()
{
    // Create planar deviation fields
    createPlanarDevFields<volScalarField>();
    createPlanarDevFields<volVectorField>();
    createPlanarDevFields<volSymmTensorField>();

    // Create turbulence statistics fields
    createTurbulenceStatFields();

    // Open file streams
    openFiles();

    write();

}


// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

Foam::planarAveraging::planarAveraging
(
    const word& name,
    const Time& t,
    const dictionary& dict
)
:
    functionObject(name),
    mesh_
    (
        refCast<const fvMesh>
        (
            t.lookupObject<objectRegistry>
            (
                dict.lookupOrDefault("region", polyMesh::defaultRegion)
            )
        )
    ),
    zPlanes_(mesh_),
    loadFromFiles_(false),
    outputPath_(fileName::null),
    fieldSelection_(),
    includeUU_(false),
    includewUU_(false),
    includeTU_(false)
{
    // Initialize mesh planes
    zPlanes_.initialize();

    // Read the dictionary.
    read(dict);

    prepare();

}


Foam::planarAveraging::planarAveraging
(
    const word& name,
    const objectRegistry& obr,
    const dictionary& dict,
    const bool loadFromFiles
)
:
    functionObject(name),
    mesh_(refCast<const fvMesh>(obr)),
    zPlanes_(mesh_),
    loadFromFiles_(loadFromFiles),
    outputPath_(fileName::null),
    fieldSelection_(),
    includeUU_(false),
    includewUU_(false),
    includeTU_(false)
{
    // Initialize mesh planes
    zPlanes_.initialize();

    // Read the dictionary.
    read(dict);

    prepare();

}


// * * * * * * * * * * * * * * * * Destructor  * * * * * * * * * * * * * * * //

Foam::planarAveraging::~planarAveraging()
{}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

bool Foam::planarAveraging::read(const dictionary& dict)
{
    dict.lookup("fields") >> fieldSelection_;

    // Check whether resolved turbulent fields are requested
    forAll(fieldSelection_,fieldI)
    {
        if (fieldSelection_[fieldI]=="UU")
        {
            includeUU_ = true;
        }
        if (fieldSelection_[fieldI]=="wUU")
        {
            includewUU_ = true;
        }
        if (fieldSelection_[fieldI]=="TU")
        {
            includeTU_ = true;
        }
    }

    return true;
}

bool Foam::planarAveraging::execute()
{
    return true;
}


bool Foam::planarAveraging::write()
{
    //Update fields
    updateFields();

    const IOobjectList objects(mesh_, mesh_.time().timeName());

    sampleAndWrite<volScalarField>(objects);
    sampleAndWrite<volVectorField>(objects);
    sampleAndWrite<volSymmTensorField>(objects);

    return true;
}


// ************************************************************************* //
