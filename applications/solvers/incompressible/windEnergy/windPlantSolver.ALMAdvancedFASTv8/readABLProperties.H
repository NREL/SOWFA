    IOdictionary ABLProperties
    (
        IOobject
        (
            "ABLProperties",
            runTime.time().constant(),
            runTime,
            IOobject::MUST_READ,
            IOobject::NO_WRITE
        )
    );

    // PROPERTIES CONCERNING THE SOURCE TERMS.

       // Read in the heights at which the sources are given.
       List<scalar> sourceHeightsMomentumSpecified(ABLProperties.lookup("sourceHeightsMomentum"));
       List<scalar> sourceHeightsTemperatureSpecified(ABLProperties.lookup("sourceHeightsTemperature"));

       // Read in the table of momentum source vs. time and height.
       List<List<scalar> > sourceTableMomentumX(ABLProperties.lookup("sourceTableMomentumX"));
       List<List<scalar> > sourceTableMomentumY(ABLProperties.lookup("sourceTableMomentumY"));
       List<List<scalar> > sourceTableMomentumZ(ABLProperties.lookup("sourceTableMomentumZ"));

       // Read in the table of temperature source vs. time and height.
       List<List<scalar> > sourceTableTemperature(ABLProperties.lookup("sourceTableTemperature"));


       // Check sizes of source tables.
       label nSourceMomentumHeights = sourceHeightsMomentumSpecified.size();
       label nSourceTemperatureHeights = sourceHeightsTemperatureSpecified.size();
       label nSourceMomentumXTimes = sourceTableMomentumX.size();
       label nSourceMomentumYTimes = sourceTableMomentumY.size();
       label nSourceMomentumZTimes = sourceTableMomentumZ.size();
       label nSourceTemperatureTimes = sourceTableTemperature.size();
       forAll(sourceTableMomentumX,i)
       {
           if (sourceTableMomentumX[i].size()-1 != nSourceMomentumHeights)
           {
               FatalErrorIn
               (
                   "Number of sourceTableMomentumX heights does not match sourceHeightsMomentum"
               )   << abort(FatalError);
           }
       }
       forAll(sourceTableMomentumY,i)
       {
           if (sourceTableMomentumY[i].size()-1 != nSourceMomentumHeights)
           {
               FatalErrorIn
               (
                   "Number of sourceTableMomentumY heights does not match sourceHeightsMomentum"
               )   << abort(FatalError);
           }
       }
       forAll(sourceTableMomentumZ,i)
       {
           if (sourceTableMomentumZ[i].size()-1 != nSourceMomentumHeights)
           {
               FatalErrorIn
               (
                   "Number of sourceTableMomentumZ heights does not match sourceHeightsMomentum"
               )   << abort(FatalError);
           }
       }
       forAll(sourceTableTemperature,i)
       {
           if (sourceTableTemperature[i].size()-1 != nSourceTemperatureHeights)
           {
               FatalErrorIn
               (
                   "Number of sourceTableTemperature heights does not match sourceHeightsTemperature"
               )   << abort(FatalError);
           }
       }


       // Break the source tables into interpolation tables.
       List<scalar> sourceMomentumXTimesSpecified(nSourceMomentumXTimes,0.0);
       List<scalar> sourceMomentumYTimesSpecified(nSourceMomentumYTimes,0.0);
       List<scalar> sourceMomentumZTimesSpecified(nSourceMomentumZTimes,0.0);
       List<scalar> sourceTemperatureTimesSpecified(nSourceTemperatureTimes,0.0);

       List<List<scalar> > sourceMomentumXSpecified(nSourceMomentumXTimes,List<scalar>(nSourceMomentumHeights,0.0));
       List<List<scalar> > sourceMomentumYSpecified(nSourceMomentumYTimes,List<scalar>(nSourceMomentumHeights,0.0));
       List<List<scalar> > sourceMomentumZSpecified(nSourceMomentumZTimes,List<scalar>(nSourceMomentumHeights,0.0));
       List<List<scalar> > sourceTemperatureSpecified(nSourceTemperatureTimes,List<scalar>(nSourceTemperatureHeights,0.0));

       for(int i = 0; i < nSourceMomentumXTimes; i++)
       {
           sourceMomentumXTimesSpecified[i] = sourceTableMomentumX[i][0];
           for(int j = 0; j < nSourceMomentumHeights; j++)
           {
               sourceMomentumXSpecified[i][j] = sourceTableMomentumX[i][j+1];
           }
       }

       for(int i = 0; i < nSourceMomentumYTimes; i++)
       {
           sourceMomentumYTimesSpecified[i] = sourceTableMomentumY[i][0];
           for(int j = 0; j < nSourceMomentumHeights; j++)
           {
               sourceMomentumYSpecified[i][j] = sourceTableMomentumY[i][j+1];
           }
       }

       for(int i = 0; i < nSourceMomentumZTimes; i++)
       {
           sourceMomentumZTimesSpecified[i] = sourceTableMomentumZ[i][0];
           for(int j = 0; j < nSourceMomentumHeights; j++)
           {
               sourceMomentumZSpecified[i][j] = sourceTableMomentumZ[i][j+1];
           }
       }

       for(int i = 0; i < nSourceTemperatureTimes; i++)
       {
           sourceTemperatureTimesSpecified[i] = sourceTableTemperature[i][0];
           for(int j = 0; j < nSourceTemperatureHeights; j++)
           {
               sourceTemperatureSpecified[i][j] = sourceTableTemperature[i][j+1];
           }
       }





    // PROPERTIES CONCERNING CORIOLIS FORCES

       // Planetary rotation period (hours)
       scalar planetaryRotationPeriod(readScalar(ABLProperties.lookup("planetaryRotationPeriod")));

       // Latitude on the planetary body (degrees)
       scalar latitude(readScalar(ABLProperties.lookup("latitude")));

       // Compute the planetary rotation vector
       label upIndex = 2;
       vector Omega_;
       Omega_.x() = 0.0;
       Omega_.y() = ((2.0 * Foam::constant::mathematical::pi) / (max(1.0E-5,planetaryRotationPeriod)*3600.0)) * Foam::cos(latitude*Foam::constant::mathematical::pi/180.0);
       Omega_.z() = ((2.0 * Foam::constant::mathematical::pi) / (max(1.0E-5,planetaryRotationPeriod)*3600.0)) * Foam::sin(latitude*Foam::constant::mathematical::pi/180.0);
       uniformDimensionedVectorField Omega
       (
           IOobject
           (
               "Omega",
               runTime.constant(),
               mesh,
               IOobject::NO_READ,
               IOobject::NO_WRITE
           ),
           dimensionedVector("Omega",dimensionSet(0, 0, -1, 0, 0, 0, 0),Omega_)
       );

       Info << Omega << endl;       




    // PROPERTIES CONCERNING GATHERING STATISTICS

       // Specify when to start building up the time average and correlations
       scalar avgStartTime(ABLProperties.lookupOrDefault<scalar>("avgStartTime", 0));
       scalar corrStartTime(ABLProperties.lookupOrDefault<scalar>("corrStartTime", 0));
