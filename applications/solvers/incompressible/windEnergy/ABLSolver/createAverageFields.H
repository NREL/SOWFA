    // Create initial average temperature field
    Info<< "Creating mean temperature field, Tmean..." << endl;
    volScalarField Tmean
    (
        IOobject
        (
            "Tmean",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::NO_WRITE
        ),
        T
    );

    // Create initial fluctuating temperature field
    Info<< "Creating fluctuating temperature field, Tprime..." << endl;
    volScalarField Tprime
    (
        IOobject
        (
            "Tprime",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        T
    );

    // Create initial average velocity field
    Info<< "Creating mean velocity field, Umean..." << endl;
    volVectorField Umean
    (
        IOobject
        (
            "Umean",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::NO_WRITE
        ),
        U
    );

    // Create initial fluctuating velocity field
    Info<< "Creating fluctuating velocity field, Uprime..." << endl;
    volVectorField Uprime
    (
        IOobject
        (
            "Uprime",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        U
    );

    // Create mean SGS stress field
    Info<< "Creating mean SGS stress field, Rmean..." << endl;
    volSymmTensorField Rmean
    (
        IOobject
        (
            "Rmean",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedSymmTensor("Rmean",dimensionSet(0, 2, -2, 0, 0, 0, 0),symmTensor::zero)
    );

    // Create mean SGS temperature flux field
    Info<< "Creating mean SGS temperature flux field, qmean..." << endl;
    volVectorField qmean
    (
        IOobject
        (
            "qmean",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedVector("qmean",dimensionSet(0, 1, -1, 1, 0, 0, 0),vector::zero)
    );

    // Create mean SGS viscosityfield
    Info<< "Creating mean SGS viscosity field, nuSGSmean..." << endl;
    volScalarField nuSGSmean
    (
        IOobject
        (
            "nuSGSmean",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedScalar("nuSGSmean",dimensionSet(0,2,-1,0,0,0,0),0.0)
    );
