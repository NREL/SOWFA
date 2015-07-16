// Get horizontally averaged quantities
{
volSymmTensorField R = 1.0*turbulence->R();
volVectorField q = -kappat * fvc::grad(T);
volScalarField nuSGS = 1.0*turbulence->nut();

    forAll(hLevelsValues,hLevelsI)
    {
        scalar TmeanVol = 0.0;
        vector UmeanVol = vector::zero;
        symmTensor RmeanVol = symmTensor::zero;
        vector qmeanVol = vector::zero;
        scalar nuSGSmeanVol = 0.0;

        for (label i = 0; i < numCellPerLevel[hLevelsI]; i++)
        {
	    label cellI = hLevelsCellList[hLevelsI][i];
	    TmeanVol += T[cellI] * mesh.V()[cellI];
	    UmeanVol += U[cellI] * mesh.V()[cellI];
            RmeanVol += R[cellI] * mesh.V()[cellI];
            qmeanVol += q[cellI] * mesh.V()[cellI];
            nuSGSmeanVol += nuSGS[cellI] * mesh.V()[cellI];
        }

        reduce(TmeanVol,sumOp<scalar>());
        reduce(UmeanVol,sumOp<vector>());
        reduce(RmeanVol,sumOp<symmTensor>());
        reduce(qmeanVol,sumOp<vector>());
        reduce(nuSGSmeanVol,sumOp<scalar>());

        TmeanLevelsList[hLevelsI] = TmeanVol/totVolPerLevel[hLevelsI];
        UmeanLevelsList[hLevelsI] = UmeanVol/totVolPerLevel[hLevelsI];
        RmeanLevelsList[hLevelsI] = RmeanVol/totVolPerLevel[hLevelsI];
        qmeanLevelsList[hLevelsI] = qmeanVol/totVolPerLevel[hLevelsI];
        nuSGSmeanLevelsList[hLevelsI] = nuSGSmeanVol/totVolPerLevel[hLevelsI];

        for(label i = 0; i < numCellPerLevel[hLevelsI]; i++)
	{
	    label cellI = hLevelsCellList[hLevelsI][i];
	    Tmean[cellI] = TmeanLevelsList[hLevelsI];
	    Umean[cellI] = UmeanLevelsList[hLevelsI];
            Rmean[cellI] = RmeanLevelsList[hLevelsI];
            qmean[cellI] = qmeanLevelsList[hLevelsI];
            nuSGSmean[cellI] = nuSGSmeanLevelsList[hLevelsI];
	}
    }

    // Then get fluctuating part
    Uprime = U - Umean;
    Tprime = T - Tmean;
}
