{
   // Get the current time.
   scalar t = runTime.value();

   {
       // This is if the source is only given at one height.  In that case,
       // assume the source is set uniformly throughout the domain to the
       // given value as a function of time only.
       if (nSourceMomentumHeights == 1)
       {
           vector s;

           s.x() = interpolate2D(t,
                                 sourceHeightsMomentumSpecified[0],
                                 indMomentumXTime,
                                 indMomentumHeight[0],
                                 sourceMomentumXTimesSpecified,
                                 sourceHeightsMomentumSpecified,
                                 sourceMomentumXSpecified);

           s.y() = interpolate2D(t,
                                 sourceHeightsMomentumSpecified[0],
                                 indMomentumYTime,
                                 indMomentumHeight[0],
                                 sourceMomentumYTimesSpecified,
                                 sourceHeightsMomentumSpecified,
                                 sourceMomentumYSpecified);

           s.z() = interpolate2D(t,
                                 sourceHeightsMomentumSpecified[0],
                                 indMomentumZTime,
                                 indMomentumHeight[0],
                                 sourceMomentumZTimesSpecified,
                                 sourceHeightsMomentumSpecified,
                                 sourceMomentumZSpecified);

           forAll(SourceU,i)
           {
               SourceU[i] = s;
           }

           Info << "Momentum Driving Source Term: (" << s.x() << " " << s.y() << " " << s.z() << ") m/s^2" << endl; 

       }
       //  Otherwise, set the source as a function of height and time.
       else
       {
           forAll(SourceU,i)
           {
               vector c = mesh.C()[i];

               SourceU[i].x() = interpolate2D(t,
                                              c.z(),
                                              indMomentumXTime,
                                              indMomentumHeight[i],
                                              sourceMomentumXTimesSpecified,
                                              sourceHeightsMomentumSpecified,
                                              sourceMomentumXSpecified);

               SourceU[i].y() = interpolate2D(t,
                                              c.z(),
                                              indMomentumYTime,
                                              indMomentumHeight[i],
                                              sourceMomentumYTimesSpecified,
                                              sourceHeightsMomentumSpecified,
                                              sourceMomentumYSpecified);

               SourceU[i].z() = interpolate2D(t, 
                                              c.z(),
                                              indMomentumZTime,
                                              indMomentumHeight[i],
                                              sourceMomentumZTimesSpecified,
                                              sourceHeightsMomentumSpecified,
                                              sourceMomentumZSpecified);
               
           }
       }
   }
   
   {
       // This is if the source is only given at one height.  In that case,
       // assume the source is set uniformly throughout the domain to the
       // given value as a function of time only.
       if (nSourceTemperatureHeights == 1)
       {
           scalar sourceT = interpolate2D(t,
                                          sourceHeightsTemperatureSpecified[0],
                                          indTemperatureTime,
                                          indTemperatureHeight[0],
                                          sourceTemperatureTimesSpecified,
                                          sourceHeightsTemperatureSpecified,
                                          sourceTemperatureSpecified);
         
           forAll(SourceT,i)
           {           
               SourceT[i] = sourceT;
           }

           Info << "Temperature Driving Source Term: " << sourceT << " K/s" << endl; 
       }
       // Otherwise, set the source as a function of height and time.
       else
       {
           forAll(SourceT,i)
           {
               vector c = mesh.C()[i];

               SourceT[i] = interpolate2D(t,
                                          c.z(),
                                          indTemperatureTime,
                                          indTemperatureHeight[i],
                                          sourceTemperatureTimesSpecified,
                                          sourceHeightsTemperatureSpecified,
                                          sourceTemperatureSpecified);
           }
       }
   }
}
