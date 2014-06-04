  label hLevelsWind1I = 0; 
  label hLevelsWind2I = 0;
  scalar hLevelsWind1Diff = 0;
  scalar hLevelsWind2Diff = 0;
  scalar hLevelsWind1 = 0;
  scalar hLevelsWind2 = 0;

  //if (driveWindOn)
  {
       // Find the grid levels closest to the specified wind height
       List<scalar> hLevelsDiff(hLevelsTotal);
       forAll(hLevelsValues,hLevelsI)
       {
            hLevelsDiff[hLevelsI] = (hLevelsValues[hLevelsI] - hWind.value());
       }

       // Find the two levels closest to the specified wind height
       //    Find the closest level
       forAll(hLevelsValues,hLevelsI)
       {
            if(hLevelsI == 0)
            {
	         hLevelsWind1I = hLevelsI;
	         hLevelsWind1Diff = hLevelsDiff[hLevelsI];
	         hLevelsWind1  = hLevelsValues[hLevelsI];
            }
	    else
            {
	         if(mag(hLevelsDiff[hLevelsI]) < mag(hLevelsWind1Diff))
	         {
	              hLevelsWind1I = hLevelsI;
	              hLevelsWind1Diff = hLevelsDiff[hLevelsI];
	              hLevelsWind1  = hLevelsValues[hLevelsI];
	         }
            }
       }

       //    Find the next closest level
       int j = 0;
       forAll(hLevelsValues,hLevelsI)
       {
            if(hLevelsI != hLevelsWind1I)
            {
	         if(j == 0)
                 {
	              hLevelsWind2I = hLevelsI;
	              hLevelsWind2Diff = hLevelsDiff[hLevelsI];
	              hLevelsWind2  = hLevelsValues[hLevelsI];
                 }
  	         else
                 {
	              if(mag(hLevelsDiff[hLevelsI]) < mag(hLevelsWind2Diff))
	              {
	                   hLevelsWind2I = hLevelsI;
	                   hLevelsWind2Diff = hLevelsDiff[hLevelsI];
	                   hLevelsWind2  = hLevelsValues[hLevelsI];
	              }
                 }
	         j++;
	    }
       }

  }
