    // Create and read the velocity field
    Info << "Creating and reading velocity field, U..." << endl;
    volVectorField U
    (
        IOobject
        (
            "U",
            runTime.timeName(),
            mesh,
            IOobject::MUST_READ,
            IOobject::AUTO_WRITE
        ),
        mesh
    );

    // Create and calculate the velocity flux
    Info << "Creating and calculating velocity flux field, phi..." << endl;
    #include "createPhi.H"

    // Read the atmospheric boundary layer specific properties
    Info << "Reading the atmospheric boundary layer properties..." << endl;
    #include "readABLProperties.H"

