/*---------------------------------------------------------------------------* \
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
    vorticityAvg

Description
    Calculates and writes the vorticity of mean velocity field UAvg.

    The -noWrite option just outputs the max/min values without writing
    the field.

\*---------------------------------------------------------------------------*/

#include "calc.H"
#include "fvc.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

void Foam::calc(const argList& args, const Time& runTime, const fvMesh& mesh)
{
    bool writeResults = !args.optionFound("noWrite");

    IOobject Uheader
    (
        "UAvg",
        runTime.timeName(),
        mesh,
        IOobject::MUST_READ
    );

    if (Uheader.headerOk())
    {
        Info<< "    Reading UAvg" << endl;
        volVectorField UAvg(Uheader, mesh);

        Info<< "    Calculating vorticityAvg" << endl;
        volVectorField vorticityAvg
        (
            IOobject
            (
                "vorticityAvg",
                runTime.timeName(),
                mesh,
                IOobject::NO_READ
            ),
            fvc::curl(UAvg)
        );

        volScalarField magVorticityAvg
        (
            IOobject
            (
                "magVorticityAvg",
                runTime.timeName(),
                mesh,
                IOobject::NO_READ
            ),
            mag(vorticityAvg)
        );

        Info<< "mean vorticity max/min : "
            << max(magVorticityAvg).value() << " "
            << min(magVorticityAvg).value() << endl;

        if (writeResults)
        {
            vorticityAvg.write();
            magVorticityAvg.write();
        }
    }
    else
    {
        Info<< "    No UAvg" << endl;
    }

    Info<< "\nEnd\n" << endl;
}


// ************************************************************************* //
