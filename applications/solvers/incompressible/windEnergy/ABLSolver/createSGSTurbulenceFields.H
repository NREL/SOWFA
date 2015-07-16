    // Create SGS temperature temperature flux field
    Info << "Creating SGS temperature flux field, qSgs..." << endl;
    volVectorField qSgs
    (
        IOobject
        (
            "qSgs",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedVector("qSgs",dimensionSet(0, 1, -1, 1, 0, 0, 0),vector::zero)
    );

   
    // Create the deviatoric SGS stress field
    Info << "Creating SGS stress field, RdevSgs..." << endl;
    volSymmTensorField RdevSgs
    (
        IOobject
        (
            "RdevSgs",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedSymmTensor("RdevSgs",dimensionSet(0, 2, -2, 0, 0, 0, 0),symmTensor::zero)
    ); 

  
    // Create the SGS turbulent kinetic energy field
    Info << "Creating SGS turbulent kinetic field, kSgs..." << endl;
    volScalarField kSgs
    (
        IOobject
        (
            "kSgs",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedScalar("kSgs",dimensionSet(0, 2, -2, 0, 0, 0, 0),0.0)
    );
