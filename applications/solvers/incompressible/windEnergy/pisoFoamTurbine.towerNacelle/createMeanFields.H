    // Create and read in initial time-averaged velocity field.
    Info << "Creating time-average velocity field, UMean..." << endl;
    volVectorField UMean
    (
        IOobject
        (
            "UMean",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedVector("UMean",dimensionSet(0, 1, -1, 0, 0, 0, 0),vector::zero)
    );

    // Create and read in initial time-averaged pressure field.
    Info << "Creating time-averaged pressure field, pMean..." << endl;
    volScalarField pMean
    (
        IOobject
        (
            "pMean",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedScalar("pMean",dimensionSet(0, 2, -2, 0, 0, 0, 0),0.0)
    );

    // Create the time-averaged velocity correlation field.
    Info<< "Creating the time-averaged velocity correlation field (<u_i'u_j'>), uuMean..." << endl;
    volSymmTensorField uuMean
    (
        IOobject
        (
            "uuMean",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedSymmTensor("uuMean",dimensionSet(0, 2, -2, 0, 0, 0, 0),symmTensor::zero)
    );

    // Create the time-averaged pressure correlation field.
    Info<< "Creating the time-averaged pressure correlation field (<p'p'>), ppMean..." << endl;
    volScalarField ppMean
    (
        IOobject
        (
            "ppMean",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedScalar("ppMean",dimensionSet(0, 4, -4, 0, 0, 0, 0),0.0)
    );

    // Create the time-averaged velocity-pressure correlation field.
    Info<< "Creating the time-averaged velocity-pressure correlation field (<u_i'p'>), upMean..." << endl;
    volVectorField upMean
    (
        IOobject
        (
            "upMean",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedVector("upMean",dimensionSet(0, 3, -3, 0, 0, 0, 0),vector::zero)
    );

    // Create the time-averaged modeled turbulent stress field
    Info<< "Creating the time-averaged modeled turbulent stress field (<R_ij>), RMean..." << endl;
    volSymmTensorField RMean
    (
        IOobject
        (
            "RMean",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedSymmTensor("RMean",dimensionSet(0, 2, -2, 0, 0, 0, 0),symmTensor::zero)
    );

    // Create the time-averaged modeled turbulent stress field
    Info<< "Creating the time-averaged modeled turbulent kinetic energy field (<k>), kMean..." << endl;
    volScalarField kMean
    (
        IOobject
        (
            "kMean",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedScalar("kMean",dimensionSet(0, 2, -2, 0, 0, 0, 0),0.0)
    );

    // Create the time-averaged total resolved plus modeled turbulent stress field
    Info<< "Creating the time-averaged modeled turbulent stress field (<u_i'u_j'> + <R_ij>), uuRTotal..." << endl;
    volSymmTensorField uuRTotal
    (
        IOobject
        (
            "uuRTotal",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedSymmTensor("uuRTotal",dimensionSet(0, 2, -2, 0, 0, 0, 0),symmTensor::zero)
    );




    // Declare averaging start and total times for average fields and correlation fields.
    scalar startTimeMean = readScalar(runTime.controlDict().lookup("startTimeMean"));
    scalar startTimeCorr = readScalar(runTime.controlDict().lookup("startTimeCorr"));
    scalar sumTimeMean = 0.0;
    scalar sumTimeCorr = 0.0;
