    // Create the driving pressure gradient field.    
    dimensionedVector gradPd
    (
        "gradPd",
        dimensionSet(0, 1, -2, 0, 0),
        vector::zero
    );


    // Set the primary driving pressure gradient file name.
    IFstream gradPdFile
    (
        runTime.path()/runTime.timeName()/"uniform"/"gradPd.raw"
    );


    // Check to see if the file exists.  If so, open it and set the driving pressure gradient
    // to its value.  If not, set the driving pressure gradient to zero.
    if(gradPdFile.good())
    {
        gradPdFile >> gradPd;
        Info << "Reading average pressure gradient file..." << endl << "Initializing as gradP = " << gradPd << endl << endl;
    }
    else
    {
        Info << "Initializing with zero pressure gradient..." << endl << endl;
    };


    // Create the driving pressure gradient time history file.
    if (Pstream::master() && !isDir(postProcessingDir/"gradPdHistory"))
    {
        mkDir(postProcessingDir/"gradPdHistory");
    }
    if (Pstream::master() && !isDir(postProcessingDir/"gradPdHistory"/runTime.timeName()))
    {
        mkDir(postProcessingDir/"gradPdHistory"/runTime.timeName());
    }
    OFstream gradPdHistoryFile
    (
        postProcessingDir/"gradPdHistory"/runTime.timeName()/"gradPdHistory"
    );
    gradPdHistoryFile << "Time (s)" << " " << "pressure gradient (m/s^2)" << " "
                                           << "Desired Wind Speed (m/s)" << " " 
                                           << "Desired Wind Direction (deg)" << " "
                                           << "Desired Wind Vector (m/s)" << " "
                                           << "Uncorrected Wind Vector (m/s)" << endl;
