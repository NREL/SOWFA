   if (statisticsOn)
   {
        if (runTime.timeIndex() % statisticsFreq == 0)
	{
	     // Then get the statistics at each vertical face level
	     forAll(hLevelsFaceValues,hLevelsI)
	     {
	          symmTensor sgsVelFluxMeanVol = symmTensor::zero;
		  vector sgsTempFluxMeanVol = vector::zero;

	  	  // sum variable times face area over the faces at a certain level
                  //   sum over interior faces first
		  for (label i = 0; i < numInteriorFacePerLevel[hLevelsI]; i++)
		  {
		       label faceI = hLevelsInteriorFaceList[hLevelsI][i];

                       sgsVelFluxMeanVol.xx() += (devR[faceI].xx() + (2.0/3.0)*kLES[faceI]) * mesh.magSf()[faceI];
                       sgsVelFluxMeanVol.xy() += (devR[faceI].xy()                 ) * mesh.magSf()[faceI];
                       sgsVelFluxMeanVol.xz() += (devR[faceI].xz()                 ) * mesh.magSf()[faceI];
                       sgsVelFluxMeanVol.yy() += (devR[faceI].yy() + (2.0/3.0)*kLES[faceI]) * mesh.magSf()[faceI];
                       sgsVelFluxMeanVol.yz() += (devR[faceI].yz()                 ) * mesh.magSf()[faceI];
                       sgsVelFluxMeanVol.zz() += (devR[faceI].zz() + (2.0/3.0)*kLES[faceI]) * mesh.magSf()[faceI];

		       sgsTempFluxMeanVol.x() += q[faceI].x() * mesh.magSf()[faceI];
		       sgsTempFluxMeanVol.y() += q[faceI].y() * mesh.magSf()[faceI];
		       sgsTempFluxMeanVol.z() += q[faceI].z() * mesh.magSf()[faceI];
		  }
                  
                  //   sum over boundary faces
                  forAll(mesh.boundaryMesh(),patchID)
                  {
                      const fvPatch& cPatch = mesh.boundary()[patchID];
                      for (label i = 0; i < numBoundaryFacePerLevel[hLevelsI][patchID]; i++)
                      {
                          label faceI = hLevelsBoundaryFaceList[hLevelsI][patchID][i];

                          // if we are summing up processor boundary faces, they will get summed twice since a processor face lies on two processors.  Therefore, only sum half of
                          // it each time.
                          if (cPatch.coupled())
                          {
                              sgsVelFluxMeanVol.xx() += (devR.boundaryField()[patchID][faceI].xx() + (2.0/3.0)*kLES.boundaryField()[patchID][faceI]) * 0.5 * cPatch.magSf()[faceI];
                              sgsVelFluxMeanVol.xy() += (devR.boundaryField()[patchID][faceI].xy()                 ) * 0.5 * cPatch.magSf()[faceI];
                              sgsVelFluxMeanVol.xz() += (devR.boundaryField()[patchID][faceI].xz()                 ) * 0.5 * cPatch.magSf()[faceI];
                              sgsVelFluxMeanVol.yy() += (devR.boundaryField()[patchID][faceI].yy() + (2.0/3.0)*kLES.boundaryField()[patchID][faceI]) * 0.5 * cPatch.magSf()[faceI];
                              sgsVelFluxMeanVol.yz() += (devR.boundaryField()[patchID][faceI].yz()                 ) * 0.5 * cPatch.magSf()[faceI];
                              sgsVelFluxMeanVol.zz() += (devR.boundaryField()[patchID][faceI].zz() + (2.0/3.0)*kLES.boundaryField()[patchID][faceI]) * 0.5 * cPatch.magSf()[faceI];
  
                              sgsTempFluxMeanVol.x() += q.boundaryField()[patchID][faceI].x() * 0.5 * cPatch.magSf()[faceI];
                              sgsTempFluxMeanVol.y() += q.boundaryField()[patchID][faceI].y() * 0.5 * cPatch.magSf()[faceI];
                              sgsTempFluxMeanVol.z() += q.boundaryField()[patchID][faceI].z() * 0.5 * cPatch.magSf()[faceI];
                          }
                          else
                          {
                              sgsVelFluxMeanVol.xx() += (devR.boundaryField()[patchID][faceI].xx() + (2.0/3.0)*kLES.boundaryField()[patchID][faceI]) * cPatch.magSf()[faceI];
                              sgsVelFluxMeanVol.xy() += (devR.boundaryField()[patchID][faceI].xy()                 ) * cPatch.magSf()[faceI];
                              sgsVelFluxMeanVol.xz() += (devR.boundaryField()[patchID][faceI].xz()                 ) * cPatch.magSf()[faceI];
                              sgsVelFluxMeanVol.yy() += (devR.boundaryField()[patchID][faceI].yy() + (2.0/3.0)*kLES.boundaryField()[patchID][faceI]) * cPatch.magSf()[faceI];
                              sgsVelFluxMeanVol.yz() += (devR.boundaryField()[patchID][faceI].yz()                 ) * cPatch.magSf()[faceI];
                              sgsVelFluxMeanVol.zz() += (devR.boundaryField()[patchID][faceI].zz() + (2.0/3.0)*kLES.boundaryField()[patchID][faceI]) * cPatch.magSf()[faceI];

                              sgsTempFluxMeanVol.x() += q.boundaryField()[patchID][faceI].x() * cPatch.magSf()[faceI];
                              sgsTempFluxMeanVol.y() += q.boundaryField()[patchID][faceI].y() * cPatch.magSf()[faceI];
                              sgsTempFluxMeanVol.z() += q.boundaryField()[patchID][faceI].z() * cPatch.magSf()[faceI];
                          }

                      }
                  }

		  // parallel gather the sums from each processor and scatter back out to processors
		  reduce(sgsVelFluxMeanVol,sumOp<symmTensor>());
		  reduce(sgsTempFluxMeanVol,sumOp<vector>());

		  // divide the volume-weighted sums by total volume at a certain level
                  sgsVelFluxLevelsList[hLevelsI] = sgsVelFluxMeanVol/totAreaPerLevel[hLevelsI];
                  sgsTempFluxLevelsList[hLevelsI] = sgsTempFluxMeanVol/totAreaPerLevel[hLevelsI];
	     }

             // Write the statistics to files
	     if (Pstream::master())
             {
                  R11meanFile << runTime.timeName() << " " << runTime.deltaT().value();
                  R12meanFile << runTime.timeName() << " " << runTime.deltaT().value();
                  R13meanFile << runTime.timeName() << " " << runTime.deltaT().value();
                  R22meanFile << runTime.timeName() << " " << runTime.deltaT().value();
                  R23meanFile << runTime.timeName() << " " << runTime.deltaT().value();
                  R33meanFile << runTime.timeName() << " " << runTime.deltaT().value();

                  q1meanFile << runTime.timeName() << " " << runTime.deltaT().value();
                  q2meanFile << runTime.timeName() << " " << runTime.deltaT().value();
                  q3meanFile << runTime.timeName() << " " << runTime.deltaT().value();

	          forAll(hLevelsFaceValues,hLevelsI)
	          {

	               R11meanFile << " " << sgsVelFluxLevelsList[hLevelsI].xx();
	               R12meanFile << " " << sgsVelFluxLevelsList[hLevelsI].xy();
	               R13meanFile << " " << sgsVelFluxLevelsList[hLevelsI].xz();
	               R22meanFile << " " << sgsVelFluxLevelsList[hLevelsI].yy();
	               R23meanFile << " " << sgsVelFluxLevelsList[hLevelsI].yz();
	               R33meanFile << " " << sgsVelFluxLevelsList[hLevelsI].zz();

	               q1meanFile << " " << sgsTempFluxLevelsList[hLevelsI].x();
	               q2meanFile << " " << sgsTempFluxLevelsList[hLevelsI].y();
	               q3meanFile << " " << sgsTempFluxLevelsList[hLevelsI].z();
	          }

                  R11meanFile << endl;
                  R12meanFile << endl;
                  R13meanFile << endl;
                  R22meanFile << endl;
                  R23meanFile << endl;
                  R33meanFile << endl;

                  q1meanFile << endl;
                  q2meanFile << endl;
                  q3meanFile << endl;
	     }
        }
   }
