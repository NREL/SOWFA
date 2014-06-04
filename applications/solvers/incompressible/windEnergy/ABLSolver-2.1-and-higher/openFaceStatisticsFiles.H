    // Define a set of mean variable lists
    List<symmTensor> sgsVelFluxLevelsList(hLevelsFaceTotal,symmTensor::zero);
    List<vector>     sgsTempFluxLevelsList(hLevelsFaceTotal,vector::zero);



    // Open and write out the vertical levels file
    OFstream hLevelsFaceFile
        (
           rootDir/runTime.timeName()/"hLevelsFace"
        );
    forAll(hLevelsFaceValues,hLevelsI)
    {
        hLevelsFaceFile << hLevelsFaceValues[hLevelsI] << " ";
    }
    hLevelsFaceFile << endl;



    // Open the statistics files.
    OFstream R11meanFile
        (
            rootDir/runTime.timeName()/"R11_mean"
        );
    OFstream R12meanFile
        (
            rootDir/runTime.timeName()/"R12_mean"
        );
    OFstream R13meanFile
        (
            rootDir/runTime.timeName()/"R13_mean"
        );
    OFstream R22meanFile
        (
            rootDir/runTime.timeName()/"R22_mean"
        );
    OFstream R23meanFile
        (
            rootDir/runTime.timeName()/"R23_mean"
        );
    OFstream R33meanFile
        (
            rootDir/runTime.timeName()/"R33_mean"
        );


    OFstream q1meanFile
        (
            rootDir/runTime.timeName()/"q1_mean"
        );
    OFstream q2meanFile
        (
            rootDir/runTime.timeName()/"q2_mean"
        );
    OFstream q3meanFile
        (
            rootDir/runTime.timeName()/"q3_mean"
        );
