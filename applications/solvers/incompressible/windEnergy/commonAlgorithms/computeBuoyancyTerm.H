    // Compute the buoyancy term, depending on the definition of
    // perturbation pressure.

    surfaceScalarField buoyancyTerm = -ghf*fvc::snGrad(rhok);
    if (perturbationPressureType == "noSplit")
    {
        buoyancyTerm = ((g & mesh.Sf())/mesh.magSf()) * fvc::interpolate(rhok);
    }
    else if (perturbationPressureType == "rho0Split")
    {
        buoyancyTerm = ((g & mesh.Sf())/mesh.magSf()) * fvc::interpolate(rhok - 1.0);
    }
    else if (perturbationPressureType == "rhokSplit")
    {
        buoyancyTerm = -ghf*fvc::snGrad(rhok);
    }
