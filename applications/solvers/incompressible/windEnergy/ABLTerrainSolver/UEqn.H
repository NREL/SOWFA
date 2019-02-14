    // Solve the momentum equation
    

    #include "computeCoriolisForce.H"

    #include "computeBuoyancyTerm.H"

    // Update sponge layer forcing
    sponge.update();

    fvVectorMatrix UEqn
    (
        fvm::ddt(U)                             // time derivative
      + fvm::div(phi, U)                        // convection
      + turbulence->divDevReff(U)               // momentum flux
      + fvc::div(Rwall)
      - fCoriolis                               // Coriolis force
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
                  + buoyancyTerm                // buoyancy force
                ) * mesh.magSf()
            )
        );
    }
