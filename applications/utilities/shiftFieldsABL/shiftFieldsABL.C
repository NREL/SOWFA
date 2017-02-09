/*---------------------------------------------------------------------------*\
This file was modified or created at the National Renewable Energy
Laboratory (NREL) on January 6, 2012 in creating the SOWFA (Simulator for
Offshore Wind Farm Applications) package of wind plant modeling tools that
are based on the OpenFOAM software. Access to and use of SOWFA imposes
obligations on the user, as set forth in the NWTC Design Codes DATA USE
DISCLAIMER AGREEMENT that can be found at
<http://wind.nrel.gov/designcodes/disclaimer.html>.
\*---------------------------------------------------------------------------*/

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

Application
    shiftFieldsABL

Description
    Initializes the flow field for turbulent atmospheric boundary layer LES.
    EWQ (4/20/16) : Created from setFieldsABL

\*---------------------------------------------------------------------------*/

#include "fvCFD.H"
#include "singlePhaseTransportModel.H"
#include "interpolateXY.H"
#include "interpolateSplineXY.H"
#include "Random.H"
#include "wallDist.H"

#include "argList.H"        // EWQ
#include "timeSelector.H"   // EWQ

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

int main(int argc, char *argv[])
{
timeSelector::addOptions(false, true); // EWQ: no -constant option, exclude 0 from time range
argList::addBoolOption
(
    "backwardScheme",
    "also shift *_0 fields for backward differencing scheme"
);
#   include "setRootCase.H"
#   include "createTime.H"
#   include "createMesh.H"

const bool backward = args.optionFound("backwardScheme");

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

//EWQ - begin loop over selected times
Foam::instantList timeDirs = Foam::timeSelector::select0(runTime, args);
forAll(timeDirs, timeI)
{
    runTime.setTime(timeDirs[timeI], timeI);
    Info<< "Time = " << runTime.timeName() << endl;

// Read in the existing solution files.   

/*
 * SKIPPED
 *
Info << "Reading field U" << endl;
volVectorField U
(
    IOobject
    (
        "U",
        runTime.timeName(),
        mesh,
        IOobject::MUST_READ,
        IOobject::NO_WRITE
    ),
    mesh
);
*/

Info << "Reading field T" << endl;
volScalarField T
(
    IOobject
    (
        "T",
        runTime.timeName(),
        mesh,
        IOobject::MUST_READ,
        IOobject::NO_WRITE
    ),
    mesh
);

if( backward ) Info << "Reading field T_0" << endl;
volScalarField T_0
(
    IOobject
    (
        "T_0",
        runTime.timeName(),
        mesh,
        IOobject::READ_IF_PRESENT,
        IOobject::NO_WRITE
    ),
    mesh
);

/*
 * SKIPPED
 *
Info << "Reading field p_rgh" << endl;
volScalarField p_rgh
(
    IOobject
    (
        "p_rgh",
        runTime.timeName(),
        mesh,
        IOobject::MUST_READ,
        IOobject::NO_WRITE
    ),
    mesh
);
*/

// Get access to the input dictionary.
IOdictionary shiftFieldsABLDict
(
    IOobject
    (
        "shiftFieldsABLDict",
        runTime.time().system(),
        runTime,
        IOobject::MUST_READ,
        IOobject::NO_WRITE
    )
);

// Read in the shiftFieldsABLDict entries.
bool updateInternalFields(shiftFieldsABLDict.lookupOrDefault<bool>("updateInternalFields",true));
bool updateBoundaryFields(shiftFieldsABLDict.lookupOrDefault<bool>("updateBoundaryFields",true));
dimensionedScalar deltaT
(
    "deltaT",
    dimensionSet(0,0,0,1,0,0,0),
    shiftFieldsABLDict.lookupOrDefault<scalar>("deltaT",0.0)
);


//
// Update the interior fields.
//
if (updateInternalFields)
{
    // Velocity.
    /*
     * SKIP 
     *
    Info << "Updating internal U field..." << endl;
    forAll(U,cellI)
    {
        U[cellI] += UPrime;
    }
    */

    // Potential temperature.
    Info << "Updating internal T field..." << endl;
    Info << "  shift by " << deltaT << endl;
    T += deltaT;
    if( backward ) 
    {
        Info<< "  shifting T_0 for backward ddt scheme" << endl;
        T_0 += deltaT;
    }

    // Modified pressure.
    /*
     * SKIP
     *
    Info << "Updating internal p_rgh field..." << endl;
    forAll(p_rgh,cellI)
    {
        p_rgh[cellI] = 0.0;
    }
    */

} // end if updateInternalFields


//
// Update the boundary field.
//
if (updateBoundaryFields)
{
    Info << "Updating boundaries..." << endl;
    //U.correctBoundaryConditions();
    T.correctBoundaryConditions();
    if( backward ) T_0.correctBoundaryConditions();
    //p_rgh.correctBoundaryConditions();
}


//
// Write out the updated fields.
//

//Info<< "Writing field U" << endl;
//U.write();

Info<< "Writing field T" << endl;
T.write(); 
if ( backward )
{
    Info<< "Writing field T_0" << endl;
    T_0.write(); 
}

//Info<< "Writing field p_rgh" << endl;
//p_rgh.write();

} //EWQ - end loop over selected times

return 0;
}


// ************************************************************************* //

