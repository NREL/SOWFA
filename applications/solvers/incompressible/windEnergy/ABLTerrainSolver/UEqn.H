    // Solve the momentum equation

    // Update Coriolis force
    Coriolis.update();

    // Update buoyancy term
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
      + gradP                                   // driving pressure gradient
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
                  + Boussinesq.buoyancyTerm ()  // buoyancy force
                ) * mesh.magSf()
            )
        );
    }
