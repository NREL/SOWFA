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


    List<scalar> sourceUXColumn(hLevelsTotal,0.0);
    List<scalar> sourceUYColumn(hLevelsTotal,0.0);
    List<scalar> sourceUZColumn(hLevelsTotal,0.0);
    List<scalar> sourceTColumn(hLevelsTotal,0.0);



    // Create the momentum source term time history directory
    if (Pstream::master() && !isDir(postProcessingDir/"SourceHistory"))
    {
        mkDir(postProcessingDir/"SourceHistory");
    }
    if (Pstream::master() && !isDir(postProcessingDir/"SourceHistory"/runTime.timeName()))
    {
        mkDir(postProcessingDir/"SourceHistory"/runTime.timeName());
    }



    // Create the momentum source term time history files.
    autoPtr<OFstream> sourceUXHistoryFile(NULL);
    if (Pstream::master())
    {
        sourceUXHistoryFile.reset(new OFstream(postProcessingDir/"SourceHistory"/runTime.timeName()/"SourceUXHistory"));

        if (nSourceMomentumHeights > 1)
        {
            sourceUXHistoryFile() << "Heights (m) ";
            forAll(hLevelsValues,i)
            {
                sourceUXHistoryFile() << hLevelsValues[i] << " ";
            }
            sourceUXHistoryFile() << endl;
        }

        sourceUXHistoryFile() << "Time (s)" << " " << "dt (s)" << " " << "source term (m/s^2)" << endl;
    }

    autoPtr<OFstream> sourceUYHistoryFile(NULL);
    if (Pstream::master())
    {
        sourceUYHistoryFile.reset(new OFstream(postProcessingDir/"SourceHistory"/runTime.timeName()/"SourceUYHistory"));

        if (nSourceMomentumHeights > 1)
        {
            sourceUYHistoryFile() << "Heights (m) ";
            forAll(hLevelsValues,i)
            {
                sourceUYHistoryFile() << hLevelsValues[i] << " ";
            }
            sourceUYHistoryFile() << endl;
        }

        sourceUYHistoryFile() << "Time (s)" << " " << "dt (s)" << " " << "source term (m/s^2)" << endl;
    }

    autoPtr<OFstream> sourceUZHistoryFile(NULL);
    if (Pstream::master())
    {
        sourceUZHistoryFile.reset(new OFstream(postProcessingDir/"SourceHistory"/runTime.timeName()/"SourceUZHistory"));

        if (nSourceMomentumHeights > 1)
        {
            sourceUZHistoryFile() << "Heights (m) ";
            forAll(hLevelsValues,i)
            {
                sourceUZHistoryFile() << hLevelsValues[i] << " ";
            }
            sourceUZHistoryFile() << endl;
        }

        sourceUZHistoryFile() << "Time (s)" << " " << "dt (s)" << " " << "source term (m/s^2)" << endl;
    }



    // Create the potential temperature source term time history files.
    autoPtr<OFstream> sourceTHistoryFile(NULL);
    if (Pstream::master())
    {
        sourceTHistoryFile.reset(new OFstream(postProcessingDir/"SourceHistory"/runTime.timeName()/"SourceTHistory"));

        if (nSourceMomentumHeights > 1)
        {
            sourceTHistoryFile() << "Heights (m) ";
            forAll(hLevelsValues,i)
            {
                sourceTHistoryFile() << hLevelsValues[i] << " ";
            }
            sourceTHistoryFile() << endl;
        }

        sourceTHistoryFile() << "Time (s)" << " " << "dt (s)" << " " << "source term (K/s)" << endl;
    }
