    dimensionedVector gradP
    (
        "gradP",
        dimensionSet(0, 1, -2, 0, 0),
        vector::zero
    );

    IFstream gradPFile
    (
        runTime.path()/runTime.timeName()/"uniform"/"gradP.raw"
    );

    if(gradPFile.good())
    {
        gradPFile >> gradP;
        Info<< "Reading average pressure gradient file..." << endl << "Initializing as gradP = " << gradP << endl << endl;
    }
    else
    {
        Info<< "Initializing with zero pressure gradient..." << endl << endl;
    };
