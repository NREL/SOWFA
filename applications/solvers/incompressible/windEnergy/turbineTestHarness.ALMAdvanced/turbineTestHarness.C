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
    testHarness

Description
    Not really a solver, but just applies a unform velocity in a specified
    way for testing things like the actuator line turbine model.

\*---------------------------------------------------------------------------*/

#include "fvCFD.H"
#include "horizontalAxisWindTurbinesALMAdvanced.H"
#include "interpolateXY.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

int main(int argc, char *argv[])
{
    #include "setRootCase.H"
    #include "createTime.H"
    #include "createMesh.H"
    #include "createFields.H"
    #include "readProperties.H"

    // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

    // Enter the time loop
    Info << "\nStarting time loop\n" << endl;

    while (runTime.loop())
    {
        Info << "Time = " << runTime.timeName() << nl << endl;
        scalar t = runTime.value();

        scalar U_ = interpolateXY(t,tProfile,UProfile);
        scalar V_ = interpolateXY(t,tProfile,VProfile);
        scalar W_ = interpolateXY(t,tProfile,WProfile);

        forAll(U, i)
        {
            U[i].x() = U_;
            U[i].y() = V_;
            U[i].z() = W_;
        }

        U.correctBoundaryConditions(); 

        #include "computeDivergence.H"

        // Update the turbine.
        turbines.update();

        // Update the solution field if necessary.
        runTime.write();

        Info<< "ExecutionTime = " << runTime.elapsedCpuTime() << " s"
            << "  ClockTime = " << runTime.elapsedClockTime() << " s"
            << nl << endl;
    }

    Info<< "End\n" << endl;

    return 0;
}


// ************************************************************************* //
