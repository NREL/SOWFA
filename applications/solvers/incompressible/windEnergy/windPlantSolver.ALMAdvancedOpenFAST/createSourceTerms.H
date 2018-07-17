    // Create the source term, which lumps together the driving pressure gradient and mesoscale source terms
    // for advection of momentum and potential temperature.

    // Create the momentum sourc term.
    Info << "Creating the momentum source term, SourceU..." << endl;
    volVectorField SourceU
    (
        IOobject
        (
            "SourceU",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedVector("SourceU",dimVelocity/dimTime,vector::zero)
    );

    Info << "Creating the potential temperature source term, SourceT..." << endl;
    volScalarField SourceT
    (
        IOobject
        (
            "SourceT",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        dimensionedScalar("SourceT",dimTemperature/dimTime,0.0)
    );


    // These are the indices in the interpolation table of the surrounding values
    // that the interpolation comes from.
    label indTemperatureTime = 0;
    label indMomentumXTime = 0;
    label indMomentumYTime = 0;
    label indMomentumZTime = 0;
    label momentumHeightListLength = 1;
    label temperatureHeightListLength = 1;

    if (nSourceMomentumHeights == 1)
    {
        momentumHeightListLength = 1;
    }
    else 
    {
        momentumHeightListLength = SourceU.size();
    }

    if (nSourceTemperatureHeights == 1)
    {
        temperatureHeightListLength = 1;
    }
    else
    {
        temperatureHeightListLength = SourceT.size();
    }
    List<label> indMomentumHeight(momentumHeightListLength,0);
    List<label> indTemperatureHeight(temperatureHeightListLength,0);
