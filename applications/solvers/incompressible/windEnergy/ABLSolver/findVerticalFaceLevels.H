    // On each processor, do a search through all faces with a normal
    // aligned with up direction and make a list of all distinct heights
    DynamicList<scalar> hLevelsFaceValuesP(0);
    label hLevelsFaceTotal = 0;

    surfaceVectorField surfNorm = mesh.Sf()/mesh.magSf();

    //   internal faces.
    forAll(mesh.owner(), faceI)
    {
         if(mag(surfNorm[faceI] & nUp) > hLevelsTol)
         {
              const vector& faceCenter = mesh.Cf()[faceI];
              label hLevelsFlag = 0;
              if(hLevelsFaceValuesP.size() == 0)
              {
                   hLevelsFaceValuesP.append(faceCenter[upIndex]);
                   hLevelsFaceTotal++;
              }
              else
              {
                   forAll(hLevelsFaceValuesP,hLevelsI)
                   {
                        const scalar h = hLevelsFaceValuesP[hLevelsI];
                        if(mag(faceCenter[upIndex] - h) < hLevelsTol)
                        {
                             hLevelsFlag = 1;
                        }
                   }
                   if(hLevelsFlag == 0)
                   {
                        hLevelsFaceValuesP.append(faceCenter[upIndex]);
                        hLevelsFaceTotal++;
                   }
              }
         }
    }

    //   boundary faces.
    forAll(mesh.boundaryMesh(), patchID)
    {
         const fvPatch& cPatch = mesh.boundary()[patchID];
         forAll(cPatch, faceI)
         {
              if(mag(surfNorm.boundaryField()[patchID][faceI] & nUp) > hLevelsTol)
              {
                   const vector& faceCenter = cPatch.Cf()[faceI];
                   label hLevelsFlag = 0;
                   forAll(hLevelsFaceValuesP,hLevelsI)
                   {
                        const scalar h = hLevelsFaceValuesP[hLevelsI];
                        if(mag(faceCenter[upIndex] - h) < hLevelsTol)
                        {
                             hLevelsFlag = 1;
                        }
                   }
                   if(hLevelsFlag == 0)
                   {
                        hLevelsFaceValuesP.append(faceCenter[upIndex]);
                        hLevelsFaceTotal++;
                   }
              }
         }
    }
    reduce(hLevelsFaceTotal,sumOp<label>());



    // Now combine the cell height lists from each processor on the master
    List<List<scalar> > hLevelsFaceValuesM(Pstream::nProcs());
    hLevelsFaceValuesM[Pstream::myProcNo()] = hLevelsFaceValuesP;
    Pstream::gatherList(hLevelsFaceValuesM);
    Pstream::scatterList(hLevelsFaceValuesM);

    

    // Remove duplicate values in the list of heights
    DynamicList<scalar> hLevelsFaceValuesUnsrt;
    {
         label I = 0;
         hLevelsFaceTotal = 0;
         forAll(hLevelsFaceValuesM,ListI)
         {
              forAll(hLevelsFaceValuesM[ListI],ListJ)
              {
                   label hLevelsFlag = 0;
                   if(I == 0)
                   {
                        hLevelsFaceValuesUnsrt.append(hLevelsFaceValuesM[ListI][ListJ]);
                        hLevelsFaceTotal++;
                   }
                   else
                   {
                        for(label J = 0; J < hLevelsFaceTotal; J++)
                        {
                             const scalar h = hLevelsFaceValuesUnsrt[J];
                             if(mag(hLevelsFaceValuesM[ListI][ListJ] - h) < hLevelsTol)
                             {
                                  hLevelsFlag = 1;
                             }
                        }
                        if(hLevelsFlag == 0)
                        {
                             hLevelsFaceValuesUnsrt.append(hLevelsFaceValuesM[ListI][ListJ]);
                             hLevelsFaceTotal++;
                        }
                   }
                   I++;
              }
         }
    }
    List<scalar> hLevelsFaceValues = hLevelsFaceValuesUnsrt;
    Info << "Total number of cell face height levels: " << hLevelsFaceTotal << endl << endl;



    // Sort the height list to be in ascending order
    {
	 scalar hLevelsValuesMin = min(hLevelsFaceValuesUnsrt);
         forAll(hLevelsFaceValuesUnsrt,hLevelsI)
         {
	      if(hLevelsI == 0)
	      {
	           hLevelsFaceValues[hLevelsI] = hLevelsValuesMin;
	      }
	      else
	      {
                   hLevelsValuesMin = 1.0E20;
                   forAll(hLevelsFaceValuesUnsrt,hLevelsJ)
	           {
		        if((hLevelsFaceValuesUnsrt[hLevelsJ] > hLevelsFaceValues[hLevelsI - 1]) && (hLevelsFaceValuesUnsrt[hLevelsJ] < hLevelsValuesMin))
			{
			     hLevelsValuesMin = hLevelsFaceValuesUnsrt[hLevelsJ];
			}
	           }
		   hLevelsFaceValues[hLevelsI] = hLevelsValuesMin;
              }
	 }
    }



    // Make a list of lists of face ID labels.  Each list within the list contains
    // the face ID labels corresponding to a specific height on a processor.  The overall list
    // should contain as many face ID labels lists as there are distinct heights.
    // Also sum up the area of faces at each level on each processor.
    List<scalar> totAreaPerLevelP(hLevelsFaceTotal);

    //   interior.
    labelList numInteriorFacePerLevel(hLevelsFaceTotal);
    forAll(hLevelsFaceValues,hLevelsI)
    {
        numInteriorFacePerLevel[hLevelsI] = 0;
        totAreaPerLevelP[hLevelsI] = 0.0;
        forAll(mesh.owner(),faceI)
        {
            const vector& faceCenter = mesh.Cf()[faceI];
            if(mag(faceCenter[upIndex] - hLevelsFaceValues[hLevelsI]) < hLevelsTol)
            {
                numInteriorFacePerLevel[hLevelsI]++;
                totAreaPerLevelP[hLevelsI] += mesh.magSf()[faceI];
            }
        }
    }
    List<List<label> > hLevelsInteriorFaceList(hLevelsFaceTotal,List<label>(max(numInteriorFacePerLevel)));
    forAll(hLevelsFaceValues,hLevelsI)
    {
        int i = 0;
        forAll(mesh.owner(),faceI)
        {
            const vector& faceCenter = mesh.Cf()[faceI];
            if(mag(faceCenter[upIndex] - hLevelsFaceValues[hLevelsI]) < hLevelsTol)
            {
                hLevelsInteriorFaceList[hLevelsI][i] = faceI;
                i++;
            }
        }
    }

    //    boundary.
    List<List<label> > numBoundaryFacePerLevel(hLevelsFaceTotal,List<label>(mesh.boundaryMesh().size()));
    label maxNumBoundaryFacePerLevel = 0;
    forAll(hLevelsFaceValues,hLevelsI)
    {
         forAll(mesh.boundaryMesh(), patchID)
         {
              const fvPatch& cPatch = mesh.boundary()[patchID];

	      numBoundaryFacePerLevel[hLevelsI][patchID] = 0;
              forAll(cPatch, faceI)
              {
	           const vector& faceCenter = cPatch.Cf()[faceI];
                   if(mag(faceCenter[upIndex] - hLevelsFaceValues[hLevelsI]) < hLevelsTol)
		   {
		        numBoundaryFacePerLevel[hLevelsI][patchID]++;
                        
                        // Since coupled faces would otherwise be counted twice, only use half their area.
                        if (cPatch.coupled())
                        {
			    totAreaPerLevelP[hLevelsI] += 0.5*cPatch.magSf()[faceI];
                        }
                        else
                        {
			    totAreaPerLevelP[hLevelsI] += cPatch.magSf()[faceI];
                        }
		   }
	      }
	      if(numBoundaryFacePerLevel[hLevelsI][patchID] > maxNumBoundaryFacePerLevel)
	      {
		   maxNumBoundaryFacePerLevel = numBoundaryFacePerLevel[hLevelsI][patchID];
	      }
	 }
    }
    List<List<List<label> > > hLevelsBoundaryFaceList(hLevelsFaceTotal,List<List<label> >(mesh.boundaryMesh().size(),List<label>(maxNumBoundaryFacePerLevel)));
    forAll(hLevelsFaceValues,hLevelsI)
    {
	 forAll(mesh.boundaryMesh(),patchID)
         {
	      const fvPatch& cPatch = mesh.boundary()[patchID];
	      int i = 0;
	      forAll(cPatch, faceI)
	      {
	           const vector& faceCenter = cPatch.Cf()[faceI];
		   if(mag(faceCenter[upIndex] - hLevelsFaceValues[hLevelsI]) < hLevelsTol)
		   {
		        hLevelsBoundaryFaceList[hLevelsI][patchID][i] = faceI;
			i++;
		   }
	      }
         }
    }



    // Gather the areas per level to get a global value
    List<List<scalar> > totAreaPerLevelM(Pstream::nProcs());
    totAreaPerLevelM[Pstream::myProcNo()] = totAreaPerLevelP;
    Pstream::gatherList(totAreaPerLevelM);
    Pstream::scatterList(totAreaPerLevelM);

    List<scalar> totAreaPerLevel(hLevelsFaceTotal);
    forAll(hLevelsFaceValues,hLevelsI)
    {
        totAreaPerLevel[hLevelsI] = 0.0;
        for (int n = 0; n < Pstream::nProcs(); n++)
        {
            totAreaPerLevel[hLevelsI] += totAreaPerLevelM[n][hLevelsI];
        }
    }


    forAll(numInteriorFacePerLevel,i)
    {
        label numInteriorFacePerLevelGlobal = numInteriorFacePerLevel[i];
        reduce(numInteriorFacePerLevelGlobal,sumOp<label>());

        label numBoundaryFacePerLevelGlobal = 0;
        forAll(numBoundaryFacePerLevel[i],j)
        {
            numBoundaryFacePerLevelGlobal += numBoundaryFacePerLevel[i][j];
        }
        reduce(numBoundaryFacePerLevelGlobal,sumOp<label>());

        Info << numInteriorFacePerLevelGlobal << tab << numBoundaryFacePerLevelGlobal << tab << numInteriorFacePerLevelGlobal + numBoundaryFacePerLevelGlobal << tab << totAreaPerLevel[i] << endl;
    }
