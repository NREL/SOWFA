   if (statisticsOn)
   {
        if (runTime.timeIndex() % statisticsFreq == 0)
	{
	     // Average the field variables
             #include "averageFields.H"

	     // Then get the statistics at each vertical level
             List<symmTensor> velFluxMeanVol(hLevelsTotal,symmTensor::zero);
             List<symmTensor> velFluxFluxMeanVol(hLevelsTotal,symmTensor::zero);
             List<vector> tempFluxMeanVol(hLevelsTotal,vector::zero);
	     forAll(hLevelsValues,hLevelsI)
	     {
//                symmTensor velFluxMeanVol = symmTensor::zero;
//                symmTensor velFluxFluxMeanVol = symmTensor::zero;
//                vector tempFluxMeanVol = vector::zero;

	  	  // sum variable times cell volume over the cells at a certain level
		  for (label i = 0; i < numCellPerLevel[hLevelsI]; i++)
		  {
		       label cellI = hLevelsCellList[hLevelsI][i];
	               velFluxMeanVol[hLevelsI].xx() += Uprime[cellI].x() * Uprime[cellI].x() * mesh.V()[cellI];
	               velFluxMeanVol[hLevelsI].xy() += Uprime[cellI].x() * Uprime[cellI].y() * mesh.V()[cellI];
	               velFluxMeanVol[hLevelsI].xz() += Uprime[cellI].x() * Uprime[cellI].z() * mesh.V()[cellI];
	               velFluxMeanVol[hLevelsI].yy() += Uprime[cellI].y() * Uprime[cellI].y() * mesh.V()[cellI];
	               velFluxMeanVol[hLevelsI].yz() += Uprime[cellI].y() * Uprime[cellI].z() * mesh.V()[cellI];
	               velFluxMeanVol[hLevelsI].zz() += Uprime[cellI].z() * Uprime[cellI].z() * mesh.V()[cellI];

	               velFluxFluxMeanVol[hLevelsI].xx() += Uprime[cellI].z() * Uprime[cellI].x() * Uprime[cellI].x() * mesh.V()[cellI];
	               velFluxFluxMeanVol[hLevelsI].xy() += Uprime[cellI].z() * Uprime[cellI].x() * Uprime[cellI].y() * mesh.V()[cellI];
	               velFluxFluxMeanVol[hLevelsI].xz() += Uprime[cellI].z() * Uprime[cellI].x() * Uprime[cellI].z() * mesh.V()[cellI];
	               velFluxFluxMeanVol[hLevelsI].yy() += Uprime[cellI].z() * Uprime[cellI].y() * Uprime[cellI].y() * mesh.V()[cellI];
	               velFluxFluxMeanVol[hLevelsI].yz() += Uprime[cellI].z() * Uprime[cellI].y() * Uprime[cellI].z() * mesh.V()[cellI];
	               velFluxFluxMeanVol[hLevelsI].zz() += Uprime[cellI].z() * Uprime[cellI].z() * Uprime[cellI].z() * mesh.V()[cellI];

		       tempFluxMeanVol[hLevelsI].x() += Tprime[cellI]     * Uprime[cellI].x() * mesh.V()[cellI];
		       tempFluxMeanVol[hLevelsI].y() += Tprime[cellI]     * Uprime[cellI].y() * mesh.V()[cellI];
		       tempFluxMeanVol[hLevelsI].z() += Tprime[cellI]     * Uprime[cellI].z() * mesh.V()[cellI];
		  }
              }

              // parallel gather the sums from each processor.  No need to scatter back out to processors because only 
              // master writes statistics to file.
              Pstream::gather(velFluxMeanVol,sumOp<List<symmTensor> >());
              Pstream::gather(velFluxFluxMeanVol,sumOp<List<symmTensor> >());
              Pstream::gather(tempFluxMeanVol,sumOp<List<vector> >());
//		  reduce(velFluxMeanVol,sumOp<symmTensor>());
//		  reduce(velFluxFluxMeanVol,sumOp<symmTensor>());
//		  reduce(tempFluxMeanVol,sumOp<vector>());

             // divide the volume-weighted sums by total volume at a certain level
             forAll(hLevelsValues,hLevelsI)
             {
                  velFluxLevelsList[hLevelsI] = velFluxMeanVol[hLevelsI]/totVolPerLevel[hLevelsI];
                  velFluxFluxLevelsList[hLevelsI] = velFluxFluxMeanVol[hLevelsI]/totVolPerLevel[hLevelsI];
                  tempFluxLevelsList[hLevelsI] = tempFluxMeanVol[hLevelsI]/totVolPerLevel[hLevelsI];
             }

             // Write the statistics to files
	     if (Pstream::master())
             {
	          TmeanFile << runTime.timeName() << " " << runTime.deltaT().value();

	          umeanFile << runTime.timeName() << " " << runTime.deltaT().value();
	          vmeanFile << runTime.timeName() << " " << runTime.deltaT().value();
	          wmeanFile << runTime.timeName() << " " << runTime.deltaT().value();

	          uuPmeanFile << runTime.timeName() << " " << runTime.deltaT().value();
	          uvPmeanFile << runTime.timeName() << " " << runTime.deltaT().value();
	          uwPmeanFile << runTime.timeName() << " " << runTime.deltaT().value();
	          vvPmeanFile << runTime.timeName() << " " << runTime.deltaT().value();
	          vwPmeanFile << runTime.timeName() << " " << runTime.deltaT().value();
	          wwPmeanFile << runTime.timeName() << " " << runTime.deltaT().value();

	          wuuPmeanFile << runTime.timeName() << " " << runTime.deltaT().value();
	          wuvPmeanFile << runTime.timeName() << " " << runTime.deltaT().value();
	          wuwPmeanFile << runTime.timeName() << " " << runTime.deltaT().value();
	          wvvPmeanFile << runTime.timeName() << " " << runTime.deltaT().value();
	          wvwPmeanFile << runTime.timeName() << " " << runTime.deltaT().value();
	          wwwPmeanFile << runTime.timeName() << " " << runTime.deltaT().value();

	          TuPmeanFile << runTime.timeName() << " " << runTime.deltaT().value();
	          TvPmeanFile << runTime.timeName() << " " << runTime.deltaT().value();
	          TwPmeanFile << runTime.timeName() << " " << runTime.deltaT().value();

                  R11meanFile << runTime.timeName() << " " << runTime.deltaT().value();
                  R12meanFile << runTime.timeName() << " " << runTime.deltaT().value();
                  R13meanFile << runTime.timeName() << " " << runTime.deltaT().value();
                  R22meanFile << runTime.timeName() << " " << runTime.deltaT().value();
                  R23meanFile << runTime.timeName() << " " << runTime.deltaT().value();
                  R33meanFile << runTime.timeName() << " " << runTime.deltaT().value();

                  q1meanFile << runTime.timeName() << " " << runTime.deltaT().value();
                  q2meanFile << runTime.timeName() << " " << runTime.deltaT().value();
                  q3meanFile << runTime.timeName() << " " << runTime.deltaT().value();

                  nuSGSmeanFile << runTime.timeName() << " " << runTime.deltaT().value();

	          forAll(hLevelsValues,hLevelsI)
	          {
	               TmeanFile << " " << TmeanLevelsList[hLevelsI];

	               umeanFile << " " << UmeanLevelsList[hLevelsI].x();
	               vmeanFile << " " << UmeanLevelsList[hLevelsI].y();
	               wmeanFile << " " << UmeanLevelsList[hLevelsI].z();

	               uuPmeanFile << " " << velFluxLevelsList[hLevelsI].xx();
	               uvPmeanFile << " " << velFluxLevelsList[hLevelsI].xy();
	               uwPmeanFile << " " << velFluxLevelsList[hLevelsI].xz();
	               vvPmeanFile << " " << velFluxLevelsList[hLevelsI].yy();
	               vwPmeanFile << " " << velFluxLevelsList[hLevelsI].yz();
	               wwPmeanFile << " " << velFluxLevelsList[hLevelsI].zz();

	               wuuPmeanFile << " " << velFluxFluxLevelsList[hLevelsI].xx();
	               wuvPmeanFile << " " << velFluxFluxLevelsList[hLevelsI].xy();
	               wuwPmeanFile << " " << velFluxFluxLevelsList[hLevelsI].xz();
	               wvvPmeanFile << " " << velFluxFluxLevelsList[hLevelsI].yy();
	               wvwPmeanFile << " " << velFluxFluxLevelsList[hLevelsI].yz();
	               wwwPmeanFile << " " << velFluxFluxLevelsList[hLevelsI].zz();

	               TuPmeanFile << " " << tempFluxLevelsList[hLevelsI].x();
	               TvPmeanFile << " " << tempFluxLevelsList[hLevelsI].y();
	               TwPmeanFile << " " << tempFluxLevelsList[hLevelsI].z();

                       R11meanFile << " " << RmeanLevelsList[hLevelsI].xx();
                       R12meanFile << " " << RmeanLevelsList[hLevelsI].xy();
                       R13meanFile << " " << RmeanLevelsList[hLevelsI].xz();
                       R22meanFile << " " << RmeanLevelsList[hLevelsI].yy();
                       R23meanFile << " " << RmeanLevelsList[hLevelsI].yz();
                       R33meanFile << " " << RmeanLevelsList[hLevelsI].zz();

                       q1meanFile << " " << qmeanLevelsList[hLevelsI].x();
                       q2meanFile << " " << qmeanLevelsList[hLevelsI].y();
                       q3meanFile << " " << qmeanLevelsList[hLevelsI].z();

                       nuSGSmeanFile << " " << nuSGSmeanLevelsList[hLevelsI];

	          }

	          TmeanFile << endl;

	          umeanFile << endl;
	          vmeanFile << endl;
	          wmeanFile << endl;

	          uuPmeanFile << endl;
	          uvPmeanFile << endl;
	          uwPmeanFile << endl;
	          vvPmeanFile << endl;
	          vwPmeanFile << endl;
	          wwPmeanFile << endl;

	          wuuPmeanFile << endl;
	          wuvPmeanFile << endl;
	          wuwPmeanFile << endl;
	          wvvPmeanFile << endl;
	          wvwPmeanFile << endl;
	          wwwPmeanFile << endl;

	          TuPmeanFile << endl;
	          TvPmeanFile << endl;
	          TwPmeanFile << endl;

                  R11meanFile << endl;
                  R12meanFile << endl;
                  R13meanFile << endl;
                  R22meanFile << endl;
                  R23meanFile << endl;
                  R33meanFile << endl;

                  q1meanFile << endl;
                  q2meanFile << endl;
                  q3meanFile << endl;

                  nuSGSmeanFile << endl;
	     }
        }
   }
