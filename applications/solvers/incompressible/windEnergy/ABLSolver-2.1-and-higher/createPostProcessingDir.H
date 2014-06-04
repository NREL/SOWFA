    // Create a post processing directory if it does not already exist. 
    fileName postProcessingDir;
    if (Pstream::parRun())
    {
        postProcessingDir = runTime.path()/"../postProcessing";
    }
    else
    {
        postProcessingDir = runTime.path()/"/postProcessing";
    }
    if (Pstream::master() && !isDir(postProcessingDir))
    {
        mkDir(postProcessingDir);
    }
