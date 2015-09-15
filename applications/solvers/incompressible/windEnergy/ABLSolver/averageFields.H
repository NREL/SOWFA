// Get horizontally averaged quantities
{
    volSymmTensorField R = 1.0*turbulence->R();
    volVectorField q = -kappat * fvc::grad(T);
    volScalarField nuSGS = 1.0*turbulence->nut();

    List<scalar> TmeanVol(hLevelsTotal,0.0);
    List<vector> UmeanVol(hLevelsTotal,vector::zero);
    List<symmTensor> RmeanVol(hLevelsTotal,symmTensor::zero);
    List<vector> qmeanVol(hLevelsTotal,vector::zero);
    List<scalar> nuSGSmeanVol(hLevelsTotal,0.0);

    forAll(hLevelsValues,hLevelsI)
    {
//      scalar TmeanVol = 0.0;
//      vector UmeanVol = vector::zero;
//      symmTensor RmeanVol = symmTensor::zero;
//      vector qmeanVol = vector::zero;
//      scalar nuSGSmeanVol = 0.0;

        for (label i = 0; i < numCellPerLevel[hLevelsI]; i++)
        {
	    label cellI = hLevelsCellList[hLevelsI][i];
	    TmeanVol[hLevelsI] += T[cellI] * mesh.V()[cellI];
	    UmeanVol[hLevelsI] += U[cellI] * mesh.V()[cellI];
            RmeanVol[hLevelsI] += R[cellI] * mesh.V()[cellI];
            qmeanVol[hLevelsI] += q[cellI] * mesh.V()[cellI];
            nuSGSmeanVol[hLevelsI] += nuSGS[cellI] * mesh.V()[cellI];
        }
    }

    reduce(TmeanVol,sumOp<List<scalar> >());
    reduce(UmeanVol,sumOp<List<vector> >());
    reduce(RmeanVol,sumOp<List<symmTensor> >());
    reduce(qmeanVol,sumOp<List<vector> >());
    reduce(nuSGSmeanVol,sumOp<List<scalar> >());
//      reduce(TmeanVol,sumOp<scalar>());
//      reduce(UmeanVol,sumOp<vector>());
//      reduce(RmeanVol,sumOp<symmTensor>());
//      reduce(qmeanVol,sumOp<vector>());
//      reduce(nuSGSmeanVol,sumOp<scalar>());
    
    forAll(hLevelsValues,hLevelsI)
    {
        TmeanLevelsList[hLevelsI] = TmeanVol[hLevelsI]/totVolPerLevel[hLevelsI];
        UmeanLevelsList[hLevelsI] = UmeanVol[hLevelsI]/totVolPerLevel[hLevelsI];
        RmeanLevelsList[hLevelsI] = RmeanVol[hLevelsI]/totVolPerLevel[hLevelsI];
        qmeanLevelsList[hLevelsI] = qmeanVol[hLevelsI]/totVolPerLevel[hLevelsI];
        nuSGSmeanLevelsList[hLevelsI] = nuSGSmeanVol[hLevelsI]/totVolPerLevel[hLevelsI];

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
