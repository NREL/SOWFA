   if (statisticsOn)
   {
        if (runTime.timeIndex() % statisticsFreq == 0)
	{
             // Calculate ABL Parameters
             //   uStar is already calculated.

             //   script R
             scalar uw = 0.5*(velFluxLevelsList[0].xz() + velFluxLevelsList[1].xz());
             scalar vw = 0.5*(velFluxLevelsList[0].yz() + velFluxLevelsList[1].yz());
             scalar R13 = sgsVelFluxLevelsList[1].xz();
             scalar R23 = sgsVelFluxLevelsList[1].yz();
             scalar TR = Foam::sqrt( Foam::pow(uw,2.0)  + Foam::pow(vw,2.0)  );
             scalar TS = Foam::sqrt( Foam::pow(R13,2.0) + Foam::pow(R23,2.0) );
             scalar scriptR = TR/TS;


             //   zi (height of minimum of z-component of temperature flux vector
             scalar zi = 0.0;
             scalar minTempFlux = 1.0E20;
             forAll(hLevelsFaceValues,hLevelI)
             {
                  scalar tempFlux = 0.0;
                  if ((hLevelI == 0) || (hLevelI == hLevelsFaceValues.size()))
                  {
                      tempFlux = sgsTempFluxLevelsList[hLevelI].z();
                  }
                  else
                  {
                      tempFlux = sgsTempFluxLevelsList[hLevelI].z() + 0.5*(tempFluxLevelsList[hLevelI].z() + tempFluxLevelsList[hLevelI-1].z());
                  }
                  if(tempFlux < minTempFlux)
                  {
                       minTempFlux = tempFlux;
                       zi = hLevelsFaceValues[hLevelI];
                  }
             }

             //   Re_LES
	     scalar dUdz1 = ( (UmeanLevelsList[1].x()-UmeanLevelsList[0].x()) / (hLevelsValues[1]-hLevelsValues[0]) );
	     scalar dVdz1 = ( (UmeanLevelsList[1].y()-UmeanLevelsList[0].y()) / (hLevelsValues[1]-hLevelsValues[0]) );
             scalar nuLESBar = Foam::sqrt( (Foam::sqr(R13) + Foam::sqr(R23)) / (Foam::sqr(dUdz1) + Foam::sqr(dVdz1)) );
             scalar ReLES = (zi*uStar.value())/nuLESBar;

             //   phi_M
             List<scalar> phiM(hLevelsFaceValues.size());
             scalar dz = hLevelsValues[1]-hLevelsValues[0];
             forAll(phiM,hLevelsI)
             {
                  if (hLevelsI == 0)
                  {
                       phiM[hLevelsI] = 0.0;
                  }
                  else if (hLevelsI == phiM.size())
                  {
                       phiM[hLevelsI] = 0.0;
                  }
                  else
                  {
                       scalar Umean1 = Foam::sqrt(sqr(UmeanLevelsList[hLevelsI].x()) + sqr(UmeanLevelsList[hLevelsI].y()));
                       scalar Umean0 = Foam::sqrt(sqr(UmeanLevelsList[hLevelsI - 1].x()) + sqr(UmeanLevelsList[hLevelsI - 1].y()));
                       phiM[hLevelsI] = (kappa*hLevelsFaceValues[hLevelsI]/uStar.value())*((Umean1-Umean0)/dz);
                  }
             }

             // Write the statistics to files
	     if (Pstream::myProcNo() == 0)
             {
                  uStarFile   << runTime.timeName() << " " << runTime.deltaT().value() << " " << uStar.value() << endl;
                  scriptRFile << runTime.timeName() << " " << runTime.deltaT().value() << " " << scriptR << endl;
                  ReLESFile   << runTime.timeName() << " " << runTime.deltaT().value() << " " << ReLES << endl;
                  ziFile      << runTime.timeName() << " " << runTime.deltaT().value() << " " << zi << endl;
                  phiMFile    << runTime.timeName() << " " << runTime.deltaT().value();

                  forAll(hLevelsFaceValues,hLevelsI)
                  {
                       phiMFile << " " << phiM[hLevelsI];
                  }

                  phiMFile << endl;
	     }
        }
   }
