/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     | Website:  https://openfoam.org
    \\  /    A nd           | Copyright (C) 2011-2018 OpenFOAM Foundation
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
    superDeliciousVanilla (the code name for the new unified solver)

Description
    Transient solver for wind-energy and atmospheric boundary layer flows.
    The incompressible equations are solved along with a Boussinesq buoyancy
    term. There is flexibility in turbulence modeling. Although the solver
    is primarily meant for and tested for large-eddy simulations, it could
    also be used with a RANS turbulence model.

    The code includes planetary Coriolis forces, pressure gradient forces
    driving the wind, ability to use mesoscale forcings, coupling with
    atmospheric-style lower rough-wall boundary conditions, and coupling
    with actuator turbine models.

\*---------------------------------------------------------------------------*/

#include "fvCFD.H"
#include "singlePhaseTransportModel.H"
#include "turbulentTransportModel.H"
#include "wallDist.H"
#include "fixedFluxPressureFvPatchScalarField.H"
#include "interpolateXY.H"
#include "fvOptions.H"
#include "pimpleControl.H"
#include "ABL.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

int main(int argc, char *argv[])
{
    #include "postProcess.H"

    #include "setRootCaseLists.H"
    #include "createTime.H"
    #include "createMesh.H"
    #include "createControl.H"
    #include "createFields.H"
    #include "createTimeControls.H"
    #include "CourantNo.H"
    #include "setInitialDeltaT.H"
    #include "computeDivergence.H"
    #include "createDivSchemeBlendingField.H"

    turbulence->validate();

    // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

    Info << endl << "Starting time loop" << endl;

    // Update boundary conditions before starting in case anything needs
    // updating, for example after using mapFields to interpolate initial
    // field.
    U.correctBoundaryConditions();
    phi = linearInterpolate(U) & mesh.Sf();
    #include "turbulenceCorrect.H"
    T.correctBoundaryConditions();

    // Time stepping loop.
    while (runTime.run())
    {
        #include "readTimeControls.H"
        #include "CourantNo.H"
        #include "setDeltaT.H"
        #include "updateDivSchemeBlendingField.H"

        runTime++;

        Info << "Time = " << runTime.timeName() << tab;
        Info << "Time Step = " << runTime.timeIndex() << endl;

        // Outer-iteration loop.
        while (pimple.loop())
        {
            // Update the source terms.
            momentumSourceTerm.update(pimple.finalPimpleIter());
            temperatureSourceTerm.update(pimple.finalPimpleIter());

            // Predictor step.
            Info << "   Predictor" << endl;

            #include "UEqn.H"
            #include "turbulenceCorrect.H"
            #include "TEqn.H"

            // Corrector steps.
            int corrIter = 1;
            while (pimple.correct())
            {
                Info << "   Corrector Step " << corrIter << endl;

                #include "pEqn.H"
                #include "turbulenceCorrect.H"
                #include "TEqn.H"

                corrIter++;
            }

            // Compute the continuity errors.
            #include "computeDivergence.H"
        }

        // Write the solution if at write time.
        runTime.write();

        // Report timing.
        Info << "ExecutionTime = " << runTime.elapsedCpuTime() << " s"
             << "  ClockTime = " << runTime.elapsedClockTime() << " s"
             << nl << endl;
    }

    Info << "Ending the simulation" << endl;

    return 0;
}


// ************************************************************************* //
