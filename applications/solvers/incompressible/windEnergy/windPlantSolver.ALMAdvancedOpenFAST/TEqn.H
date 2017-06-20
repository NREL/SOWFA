{
    kappat = turbulence->nut()/Prt;
    kappat.correctBoundaryConditions();

    volScalarField kappaEff("kappaEff", turbulence->nu()/Pr + kappat);

    fvScalarMatrix TEqn
    (
        fvm::ddt(T)
      + fvm::div(phi, T)
      - fvm::laplacian(kappaEff, T)
      - fvc::div(qwall)
      - SourceT
    );

    TEqn.relax();
    TEqn.solve();

    rhok = 1.0 - ( (T - TRef)/TRef );
}
