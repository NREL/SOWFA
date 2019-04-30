    // Solve the momentum equation
    
    // Update Coriolis forcing
    Coriolis.update();

    // Update buoyancy term
    Boussinesq.updateBuoyancyTerm();

    MRF.correctBoundaryVelocity(U);

    fvVectorMatrix UEqn
    (
        fvm::ddt(U) + fvm::div(phi, U)
      + MRF.DDt(U)
      + turbulence->divDevReff(U)
      + fvc::div(Rwall)
      - momentumSourceTerm.force()
      - Coriolis.force()
     ==
        fvOptions(U)
    );

    UEqn.relax();

    fvOptions.constrain(UEqn);

    if (pimple.momentumPredictor())
    {
        solve
        (
            UEqn
         ==
            fvc::reconstruct
            (
                (
                  - fvc::snGrad(p_rgh)
                  + Boussinesq.buoyancyTerm()
                )*mesh.magSf()
            )
        );

        fvOptions.correct(U);
    }
