    // Solve the momentum equation

    // Update Coriolis forcing
    Coriolis.update();

    // Update Buoyancy term
    Boussinesq.updateBuoyancyTerm();

    // Update sponge layer forcing
    sponge.update();

    fvVectorMatrix UEqn
    (
        fvm::ddt(U)                             // time derivative
      + fvm::div(phi, U)                        // convection
      + turbulence->divDevReff(U)               // momentum flux
      + fvc::div(Rwall)
      - Coriolis.force()                        // Coriolis force
      - sponge.force()                          // Sponge layer damping
      - momentumSourceTerm.force()              // mesoscale source terms
    );

    UEqn.relax();

    if (pimple.momentumPredictor())
    {
        solve
        (
            UEqn
            ==
            fvc::reconstruct
            (
                (
                  - fvc::snGrad(p_rgh)          // modified pressure gradient
                  + Boussinesq.buoyancyTerm()   // buoyancy force
                ) * mesh.magSf()
            )
        );
    }
