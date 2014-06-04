  {
        // Get 1/A at cell centers and faces from the U equation matrix.
        volScalarField rAU("rAU", 1.0/UEqn.A());
        surfaceScalarField rAUf("(1|A(U))", fvc::interpolate(rAU));


        // Calculate the average wind velocity at the closest level to specified.
        vector UWind1 = vector::zero;
        for (label i = 0; i < numCellPerLevel[hLevelsWind1I]; i++)
        {
	     label cellI = hLevelsCellList[hLevelsWind1I][i];
	     UWind1 += U[cellI] * mesh.V()[cellI];
        }
        reduce(UWind1,sumOp<vector>());
	UWind1 = UWind1/totVolPerLevel[hLevelsWind1I];

        // Calculate the average wind velocity at the next closest level to specified.
	vector UWind2 = vector::zero;
        for (label i = 0; i < numCellPerLevel[hLevelsWind2I]; i++)
        {
	     label cellI = hLevelsCellList[hLevelsWind2I][i];
	     UWind2 += U[cellI] * mesh.V()[cellI];
        }
        reduce(UWind2,sumOp<vector>());
	UWind2 = UWind2/totVolPerLevel[hLevelsWind2I];

        // Linearly interpolate to get the average wind velocity at the desired height.
        dimensionedVector UWindStar
	(
	    "UWindStar",
	    dimensionSet(0, 1, -1, 0, 0, 0, 0),
	    UWind1 + (((UWind2 - UWind1)/(hLevelsWind2 - hLevelsWind1)) * (hWind.value() - hLevelsWind1))
	);
        dimensionedVector UWindStarParallel = UWindStar - ((UWindStar & nUp) * nUp);

       
        // Use the look-up table of desired wind speed to get the current wind vector.
        vector UWind_(vector::zero);
        scalar UWindSpeed_(0.0);
        scalar UWindDir_(0.0);
        if (windInterpType == "linear")
        {
            UWind_      = interpolateXY(runTime.time().value(),desiredWindTime,desiredWindVector);
            UWindSpeed_ = interpolateXY(runTime.time().value(),desiredWindTime,desiredWindSpeed);
            UWindDir_   = interpolateXY(runTime.time().value(),desiredWindTime,desiredWindDirectionDeg);
        }
        else if (windInterpType == "cubic")
        {
            UWind_      = interpolateSplineXY(runTime.time().value(),desiredWindTime,desiredWindVector);
            UWindSpeed_ = interpolateSplineXY(runTime.time().value(),desiredWindTime,desiredWindSpeed);
            UWindDir_   = interpolateSplineXY(runTime.time().value(),desiredWindTime,desiredWindDirectionDeg);
        }
        dimensionedVector UWind
        (
            "UWind",
            dimensionSet(0, 1, -1, 0, 0, 0, 0),
            UWind_
        );

        if (driveWindOn)
        {
            // Correct the driving pressure gradient, velocity field, and flux field.
            dimensionedVector pdDelta = alpha*(UWind - UWindStarParallel)/rAU.weightedAverage(mesh.V());

            U += rAU*pdDelta;
            phi += rAUf*(pdDelta & mesh.Sf());

            gradPd -=  ((gradPd & nUp) * nUp) + pdDelta;
        }

        // Write driving pressure gradient time history file.
        Info << "Desired Mean Wind Speed - Direction = " << tab << UWindSpeed_ << " m/s - " << UWindDir_ << " degrees" << endl;
        Info << "Desired Mean Wind Vector = "     << tab << UWind.value() << " m/s" << endl;
        Info << "Uncorrected Mean Wind Vector = " << tab << UWindStar.value() << " m/s" << endl;
        Info << "Corrected Pressure Gradient = "  << tab << gradPd.value()   << " m/s^2" << endl;
        gradPdHistoryFile << runTime.timeName() << " " << gradPd.value() << " " << UWindSpeed_ << " " << UWindDir_ << " " << UWind.value() << " " << UWindStar.value() << endl;
  }
