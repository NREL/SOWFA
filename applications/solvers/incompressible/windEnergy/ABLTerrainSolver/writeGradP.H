    if (runTime.outputTime())
    {
        OFstream gradPFile
        (
            runTime.path()/runTime.timeName()/"uniform"/"gradP.raw"
        );

        if(gradPFile.good())
        {
            gradPFile << gradP << endl;
        }
        else
        {
            FatalErrorIn(args.executable())
                << "Cannot open file "
                << runTime.path()/runTime.timeName()/"uniform"/"gradP.raw"
                << exit(FatalError);
        };
    };
