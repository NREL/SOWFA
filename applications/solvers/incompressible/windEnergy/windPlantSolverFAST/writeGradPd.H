    if (runTime.outputTime())
    {
        OFstream gradPdFile
        (
            runTime.path()/runTime.timeName()/"uniform"/"gradPd.raw"
        );

        if(gradPdFile.good())
        {
            gradPdFile << gradPd << endl;
        }
        else
        {
            FatalErrorIn(args.executable())
                << "Cannot open file "
                << runTime.path()/runTime.timeName()/"uniform"/"gradPd.raw"
                << exit(FatalError);
        };
    };
