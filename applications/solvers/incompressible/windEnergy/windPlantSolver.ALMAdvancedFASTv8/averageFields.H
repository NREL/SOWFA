    // Get horizontally averaged velocity profile
    forAll(hLevelsValues,hLevelsI)
    {
        scalar TmeanVol = 0.0;
        vector UmeanVol = vector::zero;
        for (label i = 0; i < numCellPerLevel[hLevelsI]; i++)
        {
	    label cellI = hLevelsCellList[hLevelsI][i];
	    TmeanVol += T[cellI] * mesh.V()[cellI];
	    UmeanVol += U[cellI] * mesh.V()[cellI];
        }
        reduce(TmeanVol,sumOp<scalar>());
        reduce(UmeanVol,sumOp<vector>());
        TmeanLevelsList[hLevelsI] = TmeanVol/totVolPerLevel[hLevelsI];
        UmeanLevelsList[hLevelsI] = UmeanVol/totVolPerLevel[hLevelsI];


        for(label i = 0; i < numCellPerLevel[hLevelsI]; i++)
	{
	    label cellI = hLevelsCellList[hLevelsI][i];
	    Tmean[cellI] = TmeanLevelsList[hLevelsI];
	    Umean[cellI] = UmeanLevelsList[hLevelsI];
	}
    }

    // Then get fluctuating part
    Uprime = U - Umean;
    Tprime = T - Tmean;
