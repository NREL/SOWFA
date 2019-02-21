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

#include "planarAveraging.H"
#include "volFields.H"

// * * * * * * * * * * * * * Private Member Functions  * * * * * * * * * * * //

template<class GeoField>
void Foam::planarAveraging::createPlanarDevFields()
{
    //Get names of fields of type GeoField which are requested and are defined as IO object
    wordList names = mesh_.thisDb().names<GeoField>();
    labelList nameIDs = findStrings(fieldSelection_, names);
    wordHashSet fieldNames = wordList(names, nameIDs);

    forAllConstIter(wordHashSet, fieldNames, iter)
    {
        const word& fieldName = iter.key();

        const GeoField& baseField = mesh_.thisDb().lookupObject<GeoField>(fieldName);
        
        // Store on registry
        mesh_.thisDb().store
        (
            new GeoField
            (
                IOobject
                (
                    fieldName+"PlanarDev",
                    mesh_.time().timeName(),
                    mesh_,
                    IOobject::NO_READ,
                    IOobject::NO_WRITE
                ),
                1*baseField
            )
        );
    }
}


template<class Type>
void Foam::planarAveraging::updatePlanarDevFields()
{

    typedef GeometricField<Type, fvPatchField, volMesh> volFieldType;

    //Get names of fields of type GeoField which are requested and are defined as IO object
    wordList names = mesh_.thisDb().names<volFieldType>();
    labelList nameIDs = findStrings(fieldSelection_, names);
    wordHashSet fieldNames = wordList(names, nameIDs);

    forAllConstIter(wordHashSet, fieldNames, iter)
    {
        const word& fieldName = iter.key();

        if (mesh_.thisDb().foundObject<volFieldType>(fieldName+"PlanarDev"))
        {
            const volFieldType& fld = mesh_.thisDb().lookupObject<volFieldType>(fieldName);

            volFieldType& fldprime = 
                mesh_.thisDb().lookupObjectRef<volFieldType>(fieldName+"PlanarDev");

            List<Type> fldmean = zPlanes_.average<Type>(fld);

            forAllPlanes(zPlanes_,planeI)
            {
                for(label i = 0; i < zPlanes_.numCellPerPlane()[planeI]; i++)
                {
                    label cellI = zPlanes_.planesCellList()[planeI][i];
                    fldprime[cellI] = fld[cellI] - fldmean[planeI];
                }
            }
        }
    }
}


template<class Type>
void Foam::planarAveraging::sampleAndWrite
(
    const GeometricField<Type, fvPatchField, volMesh>& vField
)
{
    List<Type> vFieldMean = zPlanes_.average<Type>(vField);

    if (Pstream::master())
    {
        OFstream& os = *filePtrs_[vField.name()];

        os << mesh_.time().timeName() << " " << mesh_.time().deltaT().value();

        forAllPlanes(zPlanes_,planeI)
        {
            os << " " << vFieldMean[planeI];
        }
        os << endl;
    }
}


template<class GeoField>
void Foam::planarAveraging::sampleAndWrite(const IOobjectList& objects)
{
    wordList names;
    if (loadFromFiles_)
    {
        IOobjectList fieldObjects(objects.lookupClass(GeoField::typeName));
        names = fieldObjects.names();
    }
    else
    {
        names = mesh_.thisDb().names<GeoField>();
    }

    labelList nameIDs(findStrings(fieldSelection_, names));

    wordHashSet fieldNames(wordList(names, nameIDs));

    forAllConstIter(wordHashSet, fieldNames, iter)
    {
        const word& fieldName = iter.key();

        if (loadFromFiles_)
        {
            const GeoField fld
            (
                IOobject
                (
                    fieldName,
                    mesh_.time().timeName(),
                    mesh_,
                    IOobject::MUST_READ
                ),
                mesh_
            );

            sampleAndWrite(fld);
        }
        else
        {
            sampleAndWrite
            (
                mesh_.thisDb().lookupObject<GeoField>(fieldName)
            );
        }
    }
}


// ************************************************************************* //
