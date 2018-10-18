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

    // PROPERTIES CONCERNING DRIVING THE WIND TO A SPECIFIED MEAN VELOCITY
    // AT A SPECIFIED HEIGHT

       // Drive wind to specified speed a specified height using time
       // varying driving pressure gradient?
       bool driveWindOn(readBool(ABLProperties.lookup("driveWindOn")));

       // Desired horizontally-averaged wind speed at a height
       dimensionedScalar UWindSpeed(ABLProperties.lookup("UWindSpeed"));

       // Desired horizontally-averaged wind direction at a height (degrees)
       scalar UWindDir(readScalar(ABLProperties.lookup("UWindDir")));

       // Height at which horizontally-averaged wind vector is specified
       dimensionedScalar hWind(ABLProperties.lookup("hWind"));
       dimensionedScalar hWindWidth(ABLProperties.lookup("hWindWidth"));

       // Convert the cardinal wind direction (like on a compass) to the normal
       // convention of 0 radians on the + x axis with positive degrees in the
       // counter-clockwise direction
       Info << "     Specified wind at " << hWind.value() << " m is from " << UWindDir << " degrees at " << UWindSpeed.value() << " m/s" << endl;
       if (UWindDir > 180.0)
       {
           UWindDir = UWindDir - 180.0;
       }
       else
       {
           UWindDir = UWindDir + 180.0;
       }
       UWindDir = 90.0 - UWindDir;
       if (UWindDir < 0.0)
       {
           UWindDir = UWindDir + 360.0;
       }
       UWindDir = UWindDir * ((Foam::constant::mathematical::pi)/180.0);

       // Calculate the wind vector
       vector UWindSpeedToVector;
       UWindSpeedToVector.x() = Foam::cos(UWindDir);
       UWindSpeedToVector.y() = Foam::sin(UWindDir);
       UWindSpeedToVector.z() = 0.0;
       dimensionedVector UWind = UWindSpeed * UWindSpeedToVector;
       Info << "     This is a wind vector of " << UWind.value() << " m/s, where +x is east and +y is north" << endl;
       Info << "                               N" << endl;
       Info << "                               0" << endl;
       Info << "                               |" << endl << endl;
       Info << "                    W 270 --       --  90 E" << endl << endl;
       Info << "                               |" << endl;
       Info << "                              180" << endl;
       Info << "                               S"  << endl;


       // Relaxation factor on the pressure gradient control
       scalar alpha(ABLProperties.lookupOrDefault<scalar>("alpha",1.0));


        // Up index
        label upIndex = 2;
        vector nUp(vector::zero);
        nUp.z() = 1.0;

    // PROPERTIES CONCERNING THE PRESSURE
       // This switch dictates whether or not the pressure reference cell is set explicitly
       // in the p_rgh solve.  If true, pressure is constrained at the pressure reference
       // cell by manipulating the matrix row for that cell to remove the null space.  This
       // assures that the pressure level is constrained, but it also means the continuity
       // equation is no longer solved at that cell, so the divergence error can be significant
       // enough there to cause localized oscillations.  Iterative solvers can still converge
       // even with a null space, but more iterations may be needed, so this switch can be
       // set to false.
       bool activatePressureRefCell(ABLProperties.lookupOrDefault<bool>("activatePressureRefCell", true));
       if (activatePressureRefCell)
       {
            Info << "Pressure reference cell matrix row modification enabled" << endl;
       }
       else
       {
            Info << "Pressure reference cell matrix row modification not enabled" << endl;
       }



    // PROPERTIES CONCERNING GATHERING STATISTICS

       scalar avgStartTime(ABLProperties.lookupOrDefault<scalar>("avgStartTime", 0));
       scalar corrStartTime(ABLProperties.lookupOrDefault<scalar>("corrStartTime", 0));
