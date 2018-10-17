{
    // The inverse of the momentum equation "A" operator (the matrix diagonal) and
    // its projection from cell centers to cell faces.  Because it is a diagonal matrix,
    // The inverse is just the diagonal matrix of the reciprocals, hence the "r" in the
    // name "rAU".
    volScalarField rAU("rAU", 1.0/UEqn.A());
    surfaceScalarField rAUf("(1|A(U))", fvc::interpolate(rAU));

    // The momentum equation "H" operator is the off-diagonal times the last known U.
    // In the equations it is always multiplied with inv(A).  This is HbyA.
    volVectorField HbyA("HbyA", U);
    HbyA = rAU*UEqn.H();

    // This is the Boussinesq buoyancy term multipled by the inverse of the A operator.
    Boussinesq.updateBuoyancyTerm();
    surfaceScalarField phig(rAUf * Boussinesq.buoyancyTerm() * mesh.magSf());

    // Project HbyA to cell faces and apply a correction for time stepping scheme.
    surfaceScalarField phiHbyA
    (
        "phiHbyA",
        (fvc::interpolate(HbyA) & mesh.Sf())
      + rAUf*fvc::ddtCorr(U, phi)
    );

    // This part balances global mass flux.
    surfaceScalarField phiFixedFlux = phiHbyA;

    adjustPhi(phiFixedFlux, U, p_rgh);

    phiHbyA += phig;

    // Update the fixedFluxPressure BCs to ensure flux consistency
    setSnGrad<fixedFluxPressureFvPatchScalarField>
    (
        p_rgh.boundaryField(),
        (
            phiHbyA.boundaryField()
          - phiFixedFlux.boundaryField()
        )/(mesh.magSf().boundaryField()*rAUf.boundaryField())
    );


    // Non-orthogonal corrector loop.
    while (pimple.correctNonOrthogonal())
    {
        fvScalarMatrix p_rghEqn
        (
            fvm::laplacian(rAUf, p_rgh) == fvc::div(phiHbyA)
        );

        if (activatePressureRefCell)
        {
            p_rghEqn.setReference(pRefCell, getRefCellValue(p_rgh, pRefCell));
        }

        p_rghEqn.solve(mesh.solver(p_rgh.select(pimple.finalInnerIter())));

        if (pimple.finalNonOrthogonalIter())
        {
            // Calculate the conservative fluxes
            phi = phiHbyA - p_rghEqn.flux();

            // Explicitly relax pressure for momentum corrector
            p_rgh.relax();

            // Correct the momentum source with the pressure gradient flux
            // calculated from the relaxed pressure
            U = HbyA + rAU*fvc::reconstruct((phig - p_rghEqn.flux())/rAUf);
            U.correctBoundaryConditions();
        }
    }

    // Compute the full pressure and adjust the pressure level.
    #include "adjustPressureLevel.H"
}
