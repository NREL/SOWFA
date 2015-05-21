{
    // update modeled turbulent thermal conductivy now in case the the turbulence
    // model does not update it (the OpenFOAM standard ones do not, but the SOWFA
    // custom ones mostly do).
    kappat = turbulence->nut()/Prt;

    // update the turbulence model.  If it updates modeled turbulent thermal 
    // conductivity, the last step is overwritten.
    turbulence->correct();

    // update the boundary conditions on modeled turbulent thermal conductivity.
    kappat.correctBoundaryConditions();

    // update the boundary conditions on surface heat flux.
    qwall.correctBoundaryConditions();

    // update the boundary conditions on surface shear stress.
    Rwall.correctBoundaryConditions();

}
