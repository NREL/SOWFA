  {
        volScalarField rAU("rAU", 1.0/UEqn.A());
        surfaceScalarField rAUf("(1|A(U))", fvc::interpolate(rAU));

        // Calculate the average wind velocity in the height range
        vector UWindI = vector::zero;
        forAll (windHeightCell, celli)
        {
            UWindI += U[windHeightCell[celli]] * mesh.V()[windHeightCell[celli]];
        }
        reduce(UWindI,sumOp<vector>());
        UWindI = UWindI / windHeightCellVolumeTotal;

        dimensionedVector UWindStar
	(
	    "UWindStar",
	    dimensionSet(0, 1, -1, 0, 0, 0, 0),
	    UWindI
	);
       
        // Correct the driving pressure gradient and wind 
        if (driveWindOn)
        {
             dimensionedVector UWindStarParallel = UWindStar - ((UWindStar & nUp) * nUp);

	     dimensionedVector pDelta = alpha*(UWind - UWindStarParallel)/rAU.weightedAverage(mesh.V());

 	     U += rAU*pDelta;
	     phi += rAUf*(pDelta & mesh.Sf());

	     gradP -=  ((gradP & nUp) * nUp) + pDelta;
        }

	Info << "Uncorrected UWind = "  << UWindStar.value() << tab
             << "Pressure Gradient = " << gradP.value()   << endl;
  }
