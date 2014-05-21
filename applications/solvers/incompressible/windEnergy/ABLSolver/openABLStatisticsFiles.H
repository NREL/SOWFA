    // Open the ABL statistics files.
    OFstream uStarFile
       (
            rootDir/runTime.timeName()/"uStar"
       );
    OFstream scriptRFile
       (
            rootDir/runTime.timeName()/"scriptR"
       );
    OFstream ReLESFile
       (
            rootDir/runTime.timeName()/"ReLES"
       );
    OFstream ziFile
       (
            rootDir/runTime.timeName()/"zi"
       );
    OFstream phiMFile
       (
            rootDir/runTime.timeName()/"phiM"
       );
