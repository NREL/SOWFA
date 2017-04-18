    // Create the Q field
    Info << "Creating second invariant of velocity-gradient tensor field..." << endl;
    volScalarField Q
    (
        IOobject
        (
            "Q",
            runTime.timeName(),
            mesh,
            IOobject::NO_READ,
            IOobject::AUTO_WRITE
        ),
        0.5*(sqr(tr(fvc::grad(U))) - tr(((fvc::grad(U)) & (fvc::grad(U)))))
    );

    // Create the vorticity field
    Info << "Creating vorticity field..." << endl;
    volVectorField omega
    (
        IOobject
        (
            "omega",
            runTime.timeName(),
            mesh,
            IOobject::NO_READ,
            IOobject::AUTO_WRITE
        ),
        fvc::curl(U)
    );
