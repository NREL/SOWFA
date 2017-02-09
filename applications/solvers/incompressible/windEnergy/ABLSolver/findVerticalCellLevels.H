    // On each processor, do a search through all cells and make a list of 
    // all distinct cell heights

    // Up index
    label upIndex = 2;
    vector nUp(vector::zero);
    nUp.z() = 1.0;

    scalar hLevelsTol = 1.0E-8;
    HashSet<scalar> hLevelsValuesP;
    label hLevelsTotal = 0;
    forAll(mesh.cells(),cellI)
    {
	const vector& cellCenter = mesh.C()[cellI];
	int hLevelsFlag = 0;
        if(cellI == 0)
	{
	    hLevelsValuesP.insert(cellCenter[upIndex]);
	    hLevelsTotal++;
	}
	else
	{
	    forAll(hLevelsValuesP,hLevelsI)
	    {
	        const scalar h = hLevelsValuesP.toc()[hLevelsI];;
		if(mag(cellCenter[upIndex] - h) < hLevelsTol)
		{
                    hLevelsFlag = 1;
		}
	    }
	    if(hLevelsFlag == 0)
	    {
	        hLevelsValuesP.insert(cellCenter[upIndex]);
		hLevelsTotal++;
	    }
	}
    }
    reduce(hLevelsTotal,sumOp<label>());



    // Now combine the cell height lists from each processor on the master
    List<List<scalar> > hLevelsValuesM(Pstream::nProcs());
    hLevelsValuesM[Pstream::myProcNo()] = hLevelsValuesP.toc();
    Pstream::gatherList(hLevelsValuesM);
    Pstream::scatterList(hLevelsValuesM);



    // Remove duplicate values in the list of heights
    HashSet<scalar> hLevelsValuesHash;
    {
         label I = 0;
         hLevelsTotal = 0;
         forAll(hLevelsValuesM,ListI)
         {
              forAll(hLevelsValuesM[ListI],ListJ)
              {
	           label hLevelsFlag = 0;
	           if(I == 0)
	           {
	                hLevelsValuesHash.insert(hLevelsValuesM[ListI][ListJ]);
		        hLevelsTotal++;
	           }
	           else
	           {
	                for(label J = 0; J < hLevelsTotal; J++)
	                {
	                     const scalar h = hLevelsValuesHash.toc()[J];
	                     if(mag(hLevelsValuesM[ListI][ListJ] - h) < hLevelsTol)
	  	             {
		                  hLevelsFlag = 1;
		             }
	                }
	                if(hLevelsFlag == 0)
	                {
	                     hLevelsValuesHash.insert(hLevelsValuesM[ListI][ListJ]);
		             hLevelsTotal++;
	                }
	           }
                   I++;
	      }
         }
    }
    List<scalar> hLevelsValues = hLevelsValuesHash.toc();
    Info << endl << "Total number of cell center height levels: " << hLevelsTotal << endl;


    // Sort the height list to be in ascending order
    forAll(hLevelsValues,hLevelsI)
    {
	scalar hLevelsValuesMin = 1.0E20;
	scalar hValue = 0.0;
	label hLabelJ = 0;
	for(int hLevelsJ = hLevelsI; hLevelsJ < hLevelsTotal; hLevelsJ++)
	{
            if(hLevelsValues[hLevelsJ] < hLevelsValuesMin)
	    {
	        hValue = hLevelsValues[hLevelsJ];
		hLabelJ = hLevelsJ;
		hLevelsValuesMin = hValue;
	    }
	}
	hLevelsValues[hLabelJ] = hLevelsValues[hLevelsI];
	hLevelsValues[hLevelsI] = hValue;
    }


    // Make a list of lists of cell ID labels.  Each list within the list contains
    // the cell ID labels corresponding to a specific height on a processor.  The overall list
    // should contain as many cell ID labels lists as there are distinct heights.
    // Also sum up the volume of cells at each level on each processor.
    labelList numCellPerLevel(hLevelsTotal);
    List<scalar> totVolPerLevelP(hLevelsTotal);
   
    forAll(hLevelsValues,hLevelsI)
    {
	numCellPerLevel[hLevelsI] = 0;
	totVolPerLevelP[hLevelsI] = 0.0;
        forAll(mesh.cells(),cellI)
        {
            const vector& cellCenter = mesh.C()[cellI];
	    if(mag(cellCenter[upIndex] - hLevelsValues[hLevelsI]) < hLevelsTol)
	    {
	        numCellPerLevel[hLevelsI]++;
		totVolPerLevelP[hLevelsI] += mesh.V()[cellI];
	    }
	}
    }
    List<List<label> > hLevelsCellList(hLevelsTotal,List<label>(max(numCellPerLevel)));
    forAll(hLevelsValues,hLevelsI)
    {
	int i = 0;
	forAll(mesh.cells(),cellI)
	{
	    const vector& cellCenter = mesh.C()[cellI];
	    if(mag(cellCenter[upIndex] - hLevelsValues[hLevelsI]) < hLevelsTol)
	    {
		hLevelsCellList[hLevelsI][i] = cellI;
		i++;
	    }
	}
    }

    // Gather the volumes per level to get a global value
    List<List<scalar> > totVolPerLevelM(Pstream::nProcs());
    totVolPerLevelM[Pstream::myProcNo()] = totVolPerLevelP;
    Pstream::gatherList(totVolPerLevelM);
    Pstream::scatterList(totVolPerLevelM);
    
    List<scalar> totVolPerLevel(hLevelsTotal);
    forAll(hLevelsValues,hLevelsI)
    {
	totVolPerLevel[hLevelsI] = 0.0;
	for (int n = 0; n < Pstream::nProcs(); n++)
	{
            totVolPerLevel[hLevelsI] += totVolPerLevelM[n][hLevelsI];
        }
    }




    forAll(numCellPerLevel,i)
    {
        label numCellPerLevelGlobal = numCellPerLevel[i];
        reduce(numCellPerLevelGlobal,sumOp<label>());
    }
