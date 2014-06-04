    // Read the gravitational acceleration
    Info << "Reading the gravitational acceleration, g..." << endl;
    uniformDimensionedVectorField g
    (
        IOobject
        (
            "g",
            runTime.constant(),
            mesh,
            IOobject::MUST_READ,
            IOobject::NO_WRITE
        )
    );

    // Read the Coriolis force associated parameters
    Info << "Reading planetary rotation rate..." << endl;
    

    Info << "Reading the latitude..." << endl;
