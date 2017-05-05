{
   // Get the current time and time step size.
   scalar t = runTime.value();
   scalar dt = runTime.deltaT().value();

   // This part is if the momentum source terms are applied directly as given
   if (momentumSourceType == "given")
   {
       // This is if the source is only given at one height.  In that case,
       // assume the source is set uniformly throughout the domain to the
       // given value as a function of time only.
       if (nSourceMomentumHeights == 1)
       {
           scalar sourceUX = interpolate2D(t,
                                           sourceHeightsMomentumSpecified[0],
                                           sourceMomentumXTimesSpecified,
                                           sourceHeightsMomentumSpecified,
                                           sourceMomentumXSpecified);

           scalar sourceUY = interpolate2D(t,
                                           sourceHeightsMomentumSpecified[0],
                                           sourceMomentumYTimesSpecified,
                                           sourceHeightsMomentumSpecified,
                                           sourceMomentumYSpecified);

           scalar sourceUZ = interpolate2D(t,
                                           sourceHeightsMomentumSpecified[0],
                                           sourceMomentumZTimesSpecified,
                                           sourceHeightsMomentumSpecified,
                                           sourceMomentumZSpecified);
         
           vector s(vector::zero);
           s.x() = sourceUX;
           s.y() = sourceUY;
           s.z() = sourceUZ;

           forAll(SourceU,i)
           {
               SourceU[i] = s;
           }


           // Write the source information.
           if (Pstream::master())
           {
               if (statisticsOn)
               {
                   if (runTime.timeIndex() % statisticsFreq == 0)
                   {
                       sourceUXHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value() << " " << sourceUX << endl;
                       sourceUYHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value() << " " << sourceUY << endl;
                       sourceUZHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value() << " " << sourceUZ << endl;
                   }
               }
           }
       }
       //  Otherwise, set the source as a function of height and time.
       else
       {
          // Interpolate the source values in time and height.
           sourceUXColumn = interpolate2D(t,
                                          hLevelsValues,
                                          sourceMomentumXTimesSpecified,
                                          sourceHeightsMomentumSpecified,
                                          sourceMomentumXSpecified);
 
           sourceUYColumn = interpolate2D(t,
                                          hLevelsValues,
                                          sourceMomentumYTimesSpecified,
                                          sourceHeightsMomentumSpecified,
                                          sourceMomentumYSpecified);

           sourceUZColumn = interpolate2D(t, 
                                          hLevelsValues,
                                          sourceMomentumZTimesSpecified,
                                          sourceHeightsMomentumSpecified,
                                          sourceMomentumZSpecified);

           // Now go by cell levels and apply the source term.
           forAll(hLevelsCellList,level)
           {
               for (label i = 0; i < numCellPerLevel[level]; i++)
               {
                   vector s(vector::zero);
                   s.x() = sourceUXColumn[level];
                   s.y() = sourceUYColumn[level];
                   s.z() = sourceUZColumn[level];

                   SourceU[hLevelsCellList[level][i]] = s;
               }
           }                               


           // Write the column of source information.
           if (Pstream::master())
           {
               if (statisticsOn)
               {
                   if (runTime.timeIndex() % statisticsFreq == 0)
                   {
                       sourceUXHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value();
                       sourceUYHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value();
                       sourceUZHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value();

                       forAll(hLevelsCellList,level)
                       {
                          sourceUXHistoryFile() << " " << sourceUXColumn[level];
                          sourceUYHistoryFile() << " " << sourceUYColumn[level];
                          sourceUZHistoryFile() << " " << sourceUZColumn[level];
                       }

                       sourceUXHistoryFile() << endl;
                       sourceUYHistoryFile() << endl;
                       sourceUZHistoryFile() << endl;
                   }
               }
           }
       }
   }
   
   // This part is if the momentum source terms have to be computed.
   else if (momentumSourceType == "computed")
   {
       // This is if the velocity is only specified at one height.  In that case,
       // a source term will be computed and applied uniformly in all three
       // dimensions, the same as in the original ABLSolver.
       if (nSourceMomentumHeights == 1)
       {

           // Get 1/A at cell centers and faces from the U equation matrix.
           volScalarField rAU("rAU", 1.0/UEqn.A());
           surfaceScalarField rAUf("(1|A(U))", fvc::interpolate(rAU));

           // Create a volVectorField for the source term change.
           volVectorField dsVol("dsVol", SourceU);

           // Calculate the average wind velocity at the closest level to specified.
           vector UMean1 = vector::zero;
           for (label i = 0; i < numCellPerLevel[hLevelsWind1I]; i++)
           {
               label cellI = hLevelsCellList[hLevelsWind1I][i];
               UMean1 += U[cellI] * mesh.V()[cellI];
           }
           reduce(UMean1,sumOp<vector>());
           UMean1 = UMean1/totVolPerLevel[hLevelsWind1I];

           // Calculate the average wind velocity at the next closest level to specified.
           vector UMean2 = vector::zero;
           for (label i = 0; i < numCellPerLevel[hLevelsWind2I]; i++)
           {
               label cellI = hLevelsCellList[hLevelsWind2I][i];
               UMean2 += U[cellI] * mesh.V()[cellI];
           }
           reduce(UMean2,sumOp<vector>());
           UMean2 = UMean2/totVolPerLevel[hLevelsWind2I];

           // Linearly interpolate to get the average wind velocity at the desired height.
           vector Umean = UMean1 + (((UMean2 - UMean1)/(hLevelsWind2 - hLevelsWind1)) * (sourceHeightsMomentumSpecified[0] - hLevelsWind1));


           // Interpolate the desired velocity values in time.
           scalar desiredUX = interpolate2D(t,
                                            sourceHeightsMomentumSpecified[0],
                                            sourceMomentumXTimesSpecified,
                                            sourceHeightsMomentumSpecified,
                                            sourceMomentumXSpecified);

           scalar desiredUY = interpolate2D(t,
                                            sourceHeightsMomentumSpecified[0],
                                            sourceMomentumYTimesSpecified,
                                            sourceHeightsMomentumSpecified,
                                            sourceMomentumYSpecified);

           scalar desiredUZ = interpolate2D(t,
                                            sourceHeightsMomentumSpecified[0],
                                            sourceMomentumZTimesSpecified,
                                            sourceHeightsMomentumSpecified,
                                            sourceMomentumZSpecified);

           vector UmeanDesired(vector::zero);
           if (velocityInputType == "component")
           {
              UmeanDesired.x() = desiredUX;
              UmeanDesired.y() = desiredUY;
              UmeanDesired.z() = desiredUZ;
           }
           else if (velocityInputType == "speedAndDirection")
           {
              UmeanDesired = windRoseToCartesian(desiredUX,desiredUY);
              UmeanDesired.z() = desiredUZ;
           }

           Info << "<U> at " << sourceHeightsMomentumSpecified[0] << ": " << Umean << " m/s" << endl;
           Info << "<U>_desired: " << UmeanDesired << " m/s" << endl;
           scalar uxError = Umean.x() - UmeanDesired.x();
           scalar uyError = Umean.y() - UmeanDesired.y();
           scalar uzError = Umean.z() - UmeanDesired.z();
           Info << "<U>_error: (" << uxError << " " << uyError << " " << uzError << ") m/s" << endl;

           // Compute the correction to the source term.
           scalar rAUmean = (rAU.weightedAverage(mesh.V())).value();
           vector ds = (UmeanDesired - Umean) / rAUmean;

           // Subtract off any vertical part.
           ds -= (ds & nUp) * nUp;

           // Apply relaxation.
           ds *= alphaMomentum;

           // Update the source term.
           forAll(SourceU,i)
           {
               dsVol[i] = ds;
               SourceU[i] += ds;
           }

           dsVol.correctBoundaryConditions();
           SourceU.correctBoundaryConditions();

           // Explicitly correct velocity and flux.
           U += rAU * dsVol;
           U.correctBoundaryConditions();
           phi += rAUf * (fvc::interpolate(dsVol) & mesh.Sf());

           // Write the source information.
           if (Pstream::master())
           {
               if (statisticsOn)
               {
                   if (runTime.timeIndex() % statisticsFreq == 0)
                   {
                       sourceUXHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value() << " " << SourceU[0].x() << endl;
                       sourceUYHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value() << " " << SourceU[0].y() << endl;
                       sourceUZHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value() << " " << SourceU[0].z() << endl;
                   }
               }
           }
       }


       // Otherwise, set the source as a function of height and time.
       else
       {
           // Get the inverse of the A operator on the U equation matrix.
           // The A operator gives the diagonal elements of the matrix.
           // Also, interpolate it to the cell faces.
           volScalarField rAU("rAU", 1.0/UEqn.A());
           surfaceScalarField rAUf("(1|A(U))", fvc::interpolate(rAU));

       
           // Create a volVectorField for the source term change.
           volVectorField dsVol("dsVol", SourceU);


           // Interpolate the desired velocity values in time and height.
           List<scalar> desiredUXColumn = interpolate2D(t,
                                                        hLevelsValues,
                                                        sourceMomentumXTimesSpecified,
                                                        sourceHeightsMomentumSpecified,
                                                        sourceMomentumXSpecified);

           List<scalar> desiredUYColumn = interpolate2D(t,
                                                        hLevelsValues,
                                                        sourceMomentumYTimesSpecified,
                                                        sourceHeightsMomentumSpecified,
                                                        sourceMomentumYSpecified);

           List<scalar> desiredUZColumn = interpolate2D(t,
                                                        hLevelsValues,
                                                        sourceMomentumZTimesSpecified,
                                                        sourceHeightsMomentumSpecified,
                                                        sourceMomentumZSpecified);

            
           // Compute the planar-averaged actual velocity and A operator at each cell level.
           List<vector> Umean(hLevelsTotal,vector::zero);
           List<vector> UmeanDesired(hLevelsTotal,vector::zero);
           List<scalar> rAUmean(hLevelsTotal,0.0);
   

           forAll(hLevelsCellList,level)
           {
               for (label i = 0; i < numCellPerLevel[level]; i++)
               {
                   Umean[level] += U[hLevelsCellList[level][i]] * mesh.V()[hLevelsCellList[level][i]];
                   rAUmean[level] += rAU[hLevelsCellList[level][i]] * mesh.V()[hLevelsCellList[level][i]];
               }
           }

           reduce(Umean,sumOp<List<vector> >());
           reduce(rAUmean,sumOp<List<scalar> >());

           forAll(hLevelsCellList,level)
           {
               Umean[level] /= totVolPerLevel[level];
               rAUmean[level] /= totVolPerLevel[level];

               vector v(vector::zero);
               v.x() = desiredUXColumn[level];
               v.y() = desiredUYColumn[level];
               v.z() = desiredUZColumn[level];
               UmeanDesired[level] = v;
           }

           //Info << "UmeanDesired = " << UmeanDesired << endl;
           //Info << "UmeanActual = " << Umean << endl;


           // Compute the correction to the source term.
           forAll(hLevelsCellList,level)
           {
               // this is the correction at this level.
               vector ds = (UmeanDesired[level] - Umean[level]) / rAUmean[level];

               // subtract off any vertical part.
               ds -= (ds & nUp) * nUp;

               // apply relaxation.
               ds *= alphaMomentum;

               // add the correction to the source column vector.
               sourceUXColumn[level] += ds.x();
               sourceUYColumn[level] += ds.y();
               sourceUZColumn[level] += ds.z();

               // add the correction on to the source field.
               for (label i = 0; i < numCellPerLevel[level]; i++)
               {
                   dsVol[hLevelsCellList[level][i]] = ds;
                   SourceU[hLevelsCellList[level][i]] += ds;
               }
           }
           dsVol.correctBoundaryConditions();
           SourceU.correctBoundaryConditions();

           // Explicitly correct velocity and flux.
           U += rAU * dsVol;
           U.correctBoundaryConditions();
           phi += rAUf * (fvc::interpolate(dsVol) & mesh.Sf());

           // Write the column of source information.
           if (Pstream::master())
           {
               if (statisticsOn)
               {
                   if (runTime.timeIndex() % statisticsFreq == 0)
                   {
                       sourceUXHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value();
                       sourceUYHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value();
                       sourceUZHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value();

                       forAll(hLevelsCellList,level)
                       {
                          sourceUXHistoryFile() << " " << sourceUXColumn[level];
                          sourceUYHistoryFile() << " " << sourceUYColumn[level];
                          sourceUZHistoryFile() << " " << sourceUZColumn[level];
                       }

                       sourceUXHistoryFile() << endl;
                       sourceUYHistoryFile() << endl;
                       sourceUZHistoryFile() << endl;
                   }
               }
           }
       }
   }


   // This part is if the temperature source terms are applied directly as given
   if (temperatureSourceType == "given")
   {
       // This is if the source is only given at one height.  In that case,
       // assume the source is set uniformly throughout the domain to the
       // given value as a function of time only.
       if (nSourceTemperatureHeights == 1)
       {
           scalar sourceT = interpolate2D(t,
                                          sourceHeightsTemperatureSpecified[0],
                                          sourceTemperatureTimesSpecified,
                                          sourceHeightsTemperatureSpecified,
                                          sourceTemperatureSpecified);
         
           forAll(SourceT,i)
           {           
               SourceT[i] = sourceT;
           }

           // Write the source information.
           if (Pstream::master())
           {
               if (statisticsOn)
               {
                   if (runTime.timeIndex() % statisticsFreq == 0)
                   {
                       sourceTHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value() << " " << sourceT << endl;
                   }
               }
           }
       }
       // Otherwise, set the source as a function of height and time.
       else
       {
           // Interpolate the source values in time and height.
           sourceTColumn = interpolate2D(t,
                                         hLevelsValues,
                                         sourceTemperatureTimesSpecified,
                                         sourceHeightsTemperatureSpecified,
                                         sourceTemperatureSpecified);

           // Now go by cell levels and apply the source term.
           forAll(hLevelsCellList,level)
           {
               for (label i = 0; i < numCellPerLevel[level]; i++)
               {
                   SourceT[hLevelsCellList[level][i]] = sourceTColumn[level];
               }
           }                               

           if (Pstream::master())
           {
               if (statisticsOn)
               {
                   if (runTime.timeIndex() % statisticsFreq == 0)
                   {
                       sourceTHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value();

                       forAll(hLevelsCellList,level)
                       {
                          sourceTHistoryFile() << " " << sourceTColumn[level];
                       }

                       sourceTHistoryFile() << endl;
                   }
               }
           }
       }
   }

   // This part is if the temperature source terms have to be computed.
   else if (temperatureSourceType == "computed")
   {
       // This is if the temperature is only specified at one height.  In that case,
       // a source term will be computed and applied uniformly in all three
       // dimensions.
       if (nSourceTemperatureHeights == 1)
       {
           // Create a volScalarField for the source term change.
           volScalarField dsVol("dsVol", SourceT);

           // Calculate the average temperature at the closest level to specified.
           scalar TMean1 = 0.0;
           for (label i = 0; i < numCellPerLevel[hLevelsWind1I]; i++)
           {
               label cellI = hLevelsCellList[hLevelsWind1I][i];
               TMean1 += T[cellI] * mesh.V()[cellI];
           }
           reduce(TMean1,sumOp<scalar>());
           TMean1 = TMean1/totVolPerLevel[hLevelsWind1I];

           // Calculate the average temperature at the next closest level to specified.
           scalar TMean2 = 0.0;
           for (label i = 0; i < numCellPerLevel[hLevelsWind2I]; i++)
           {
               label cellI = hLevelsCellList[hLevelsWind2I][i];
               TMean2 += T[cellI] * mesh.V()[cellI];
           }
           reduce(TMean2,sumOp<scalar>());
           TMean2 = TMean2/totVolPerLevel[hLevelsWind2I];

           // Linearly interpolate to get the average wind velocity at the desired height.
           scalar Tmean = TMean1 + (((TMean2 - TMean1)/(hLevelsTemp2 - hLevelsTemp1)) * (sourceHeightsTemperatureSpecified[0] - hLevelsTemp1));

           // Interpolate the desired temperature in time.
           scalar TmeanDesired = interpolate2D(t,
                                               sourceHeightsTemperatureSpecified[0],
                                               sourceTemperatureTimesSpecified,
                                               sourceHeightsTemperatureSpecified,
                                               sourceTemperatureSpecified);

           Info << "<T> at " << sourceHeightsTemperatureSpecified[0] << ": " << Tmean << " K" << endl;
           Info << "<T>_desired: " << TmeanDesired << " K" << endl;
           Info << "<T>_error: " << Tmean - TmeanDesired << " K" << endl;
       
           // Compute the correction to the source term.
           scalar ds = (TmeanDesired - Tmean) / dt;

           // Apply the relaxation.
           ds *= alphaTemperature;

           // Update the source term.
           forAll(SourceT,i)
           {
               dsVol[i] = ds;
               SourceT[i] += ds;
           }
  
           dsVol.correctBoundaryConditions();
           SourceT.correctBoundaryConditions();

           // Explicitly correct the temperature.
           T += runTime.deltaT() * dsVol;
           T.correctBoundaryConditions();

           // Write the source information.
           if (Pstream::master())
           {
               if (statisticsOn)
               {
                   if (runTime.timeIndex() % statisticsFreq == 0)
                   {
                       sourceTHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value() << " " << SourceT[0] << endl;
                   }
               }
           }
       }


       // Otherwise, set the source as a function of height and time.
       else
       {
           // Create a volScalarField for the source term change.
           volScalarField dsVol("dsVol", SourceT);


           // Interpolate the desired velocity values in time and height.
           List<scalar> desiredTColumn = interpolate2D(t,
                                                       hLevelsValues,
                                                       sourceTemperatureTimesSpecified,
                                                       sourceHeightsTemperatureSpecified,
                                                       sourceTemperatureSpecified);

            
           // Compute the planar-averaged actual temperature at each cell level.
           List<scalar> Tmean(hLevelsTotal,0.0);
           List<scalar> TmeanDesired(hLevelsTotal,0.0);
   

           forAll(hLevelsCellList,level)
           {
               for (label i = 0; i < numCellPerLevel[level]; i++)
               {
                   Tmean[level] += T[hLevelsCellList[level][i]] * mesh.V()[hLevelsCellList[level][i]];
               }
           }

           reduce(Tmean,sumOp<List<scalar> >());

           forAll(hLevelsCellList,level)
           {
               Tmean[level] /= totVolPerLevel[level];

               TmeanDesired[level] = desiredTColumn[level];
           }

           //Info << "TmeanDesired = " << TmeanDesired << endl;
           //Info << "TmeanActual = " << Tmean << endl;

        
           // Compute the correction to the source term.
           forAll(hLevelsCellList,level)
           {
               // this is the correction a this level.
               scalar ds = (TmeanDesired[level] - Tmean[level]) / dt;

               // apply relaxation.
               ds *= alphaTemperature;

               // add the correction to the source column vector.
               sourceTColumn[level] += ds;

               // add the correction on to the source field.
               for (label i = 0; i < numCellPerLevel[level]; i++)
               {
                   dsVol[hLevelsCellList[level][i]] = ds;
                   SourceT[hLevelsCellList[level][i]] += ds;
               }
           }
           dsVol.correctBoundaryConditions();
           SourceT.correctBoundaryConditions();

           // Explicitly correct velocity and flux.
           T += runTime.deltaT() * dsVol;
           T.correctBoundaryConditions();

           // Write the column of source information.
           if (Pstream::master())
           {
               if (statisticsOn)
               {
                   if (runTime.timeIndex() % statisticsFreq == 0)
                   {
                       sourceTHistoryFile() << runTime.timeName() << " " << runTime.deltaT().value();

                       forAll(hLevelsCellList,level)
                       {
                          sourceTHistoryFile() << " " << sourceTColumn[level];
                       }

                       sourceTHistoryFile() << endl;
                   }
               }
           }
       }
   }
}
