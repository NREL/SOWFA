  scalar hLevelsTemp1Diff = 0;
  scalar hLevelsTemp2Diff = 0;

  {
       // Find the grid levels closest to the specified wind height
       List<scalar> hLevelsDiff(hLevelsTotal);
       forAll(hLevelsValues,hLevelsI)
       {
            hLevelsDiff[hLevelsI] = (hLevelsValues[hLevelsI] - sourceHeightsTemperatureSpecified[0]);
       }

       // Find the two levels closest to the specified wind height
       //    Find the closest level
       forAll(hLevelsValues,hLevelsI)
       {
            if(hLevelsI == 0)
            {
	         hLevelsTemp1I = hLevelsI;
	         hLevelsTemp1Diff = hLevelsDiff[hLevelsI];
	         hLevelsTemp1  = hLevelsValues[hLevelsI];
            }
	    else
            {
	         if(mag(hLevelsDiff[hLevelsI]) < mag(hLevelsTemp1Diff))
	         {
	              hLevelsTemp1I = hLevelsI;
	              hLevelsTemp1Diff = hLevelsDiff[hLevelsI];
	              hLevelsTemp1  = hLevelsValues[hLevelsI];
	         }
            }
       }

       //    Find the next closest level
       int j = 0;
       forAll(hLevelsValues,hLevelsI)
       {
            if(hLevelsI != hLevelsTemp1I)
            {
	         if (j == 0)
                 {
	              hLevelsTemp2I = hLevelsI;
	              hLevelsTemp2Diff = hLevelsDiff[hLevelsI];
	              hLevelsTemp2  = hLevelsValues[hLevelsI];
                 }
  	         else
                 {
	              if(mag(hLevelsDiff[hLevelsI]) < mag(hLevelsTemp2Diff))
	              {
	                   hLevelsTemp2I = hLevelsI;
	                   hLevelsTemp2Diff = hLevelsDiff[hLevelsI];
	                   hLevelsTemp2  = hLevelsValues[hLevelsI];
	              }
                 }
	         j++;
	    }
       }
  }
