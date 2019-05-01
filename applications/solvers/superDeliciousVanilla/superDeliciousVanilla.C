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
    buoyantBoussinesqPimpleFoam

Description
    Transient solver for buoyant, turbulent flow of incompressible fluids.

    Uses the Boussinesq approximation:
    \f[
        rho_{k} = 1 - beta(T - T_{ref})
    \f]

    where:
        \f$ rho_{k} \f$ = the effective (driving) kinematic density
        beta = thermal expansion coefficient [1/K]
        T = temperature [K]
        \f$ T_{ref} \f$ = reference temperature [K]

    Valid when:
    \f[
        \frac{beta(T - T_{ref})}{rho_{ref}} << 1
    \f]

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

    while (runTime.run())
    {
        #include "readTimeControls.H"
        #include "CourantNo.H"
        #include "setDeltaT.H"
        #include "updateDivSchemeBlendingField.H"

        runTime++;

        Info << "Time = " << runTime.timeName() << tab;
        Info << "Time Step = " << runTime.timeIndex() << endl;

        // --- Pressure-velocity PIMPLE corrector loop
        while (pimple.loop())
        {
            Info << "   Predictor..." << endl;
            #include "UEqn.H"
            #include "turbulenceCorrect.H"
            #include "TEqn.H"

            // --- Pressure corrector loop
            int pressureCorr = 0;
            while (pimple.correct())
            {
                Info << "   Corrector Step " << pressureCorr << "..." << endl;
                #include "pEqn.H"
                #include "turbulenceCorrect.H"
                #include "TEqn.H"
                pressureCorr++;
            }

            // --- Update the source terms
            momentumSourceTerm.update();
            temperatureSourceTerm.update();

            // --- Compute the continuity errors.
            #include "computeDivergence.H"
        }

        runTime.write();

        Info<< "ExecutionTime = " << runTime.elapsedCpuTime() << " s"
            << "  ClockTime = " << runTime.elapsedClockTime() << " s"
            << nl << endl;
    }

    Info << "End" << endl;

    return 0;
}


// ************************************************************************* //
