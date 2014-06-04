    // Define a set of mean variable lists
    List<scalar>     TmeanLevelsList(hLevelsTotal,0.0);
    List<vector>     UmeanLevelsList(hLevelsTotal,vector::zero);
    List<symmTensor> velFluxLevelsList(hLevelsTotal,symmTensor::zero);
    List<symmTensor> velFluxFluxLevelsList(hLevelsTotal,symmTensor::zero);
    List<vector>     tempFluxLevelsList(hLevelsTotal,vector::zero);
    List<symmTensor> RmeanLevelsList(hLevelsTotal,symmTensor::zero);
    List<vector>     qmeanLevelsList(hLevelsTotal,vector::zero);
    List<scalar>     nuSGSmeanLevelsList(hLevelsTotal,0.0);



    // If the averaging directory doesn't exist, make it.
    if (Pstream::master() && !isDir(postProcessingDir/"averaging"))
    {
        mkDir(postProcessingDir/"averaging");
    }

    // Check to see if the start time directory exists within the averaging directory; if not, create it.
    if (Pstream::master() && !isDir(postProcessingDir/"averaging"/runTime.timeName()))
    {
        mkDir(postProcessingDir/"averaging"/runTime.timeName());
    }



    // Open and write out the cell center vertical levels file
    OFstream hLevelsFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"hLevelsCell"
    );
    forAll(hLevelsValues,hLevelsI)
    {
        hLevelsFile << hLevelsValues[hLevelsI] << " ";
    }
    hLevelsFile << endl;



    // Open the statistics files.
    OFstream TmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"T_mean"
    );



    OFstream umeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"U_mean"
    );
    OFstream vmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"V_mean"
    );
    OFstream wmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"W_mean"
    );



    OFstream uuPmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"uu_mean"
    );
    OFstream uvPmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"uv_mean"
    );
    OFstream uwPmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"uw_mean"
    );
    OFstream vvPmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"vv_mean"
    );
    OFstream vwPmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"vw_mean"
    );
    OFstream wwPmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"ww_mean"
    );



    OFstream wuuPmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"wuu_mean"
    );
    OFstream wuvPmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"wuv_mean"
    );
    OFstream wuwPmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"wuw_mean"
    );
    OFstream wvvPmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"wvv_mean"
    );
    OFstream wvwPmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"wvw_mean"
    );
    OFstream wwwPmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"www_mean"
    );



    OFstream TuPmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"Tu_mean"
    );
    OFstream TvPmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"Tv_mean"
    );
    OFstream TwPmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"Tw_mean"
    );

    


    OFstream R11meanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"R11_mean"
    );
    OFstream R12meanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"R12_mean"
    );
    OFstream R13meanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"R13_mean"
    );
    OFstream R22meanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"R22_mean"
    );
    OFstream R23meanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"R23_mean"
    );
    OFstream R33meanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"R33_mean"
    );


    OFstream q1meanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"q1_mean"
    );
    OFstream q2meanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"q2_mean"
    );
    OFstream q3meanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"q3_mean"
    );


    OFstream nuSGSmeanFile
    (
        postProcessingDir/"averaging"/runTime.timeName()/"nu_SGS_mean"
    );
