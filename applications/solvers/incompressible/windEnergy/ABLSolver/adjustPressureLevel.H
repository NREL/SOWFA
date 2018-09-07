    // Compute the full pressure and adjust the pressure level.
    if (perturbationPressureType == "noSplit")
    {
        p = p_rgh;
    }
    else if (perturbationPressureType == "rho0Split")
    {
        p = p_rgh + gh;
    }
    else if (perturbationPressureType == "rhokSplit")
    {
        p = p_rgh + rhok*gh;
    }

    if (p_rgh.needReference())
    {
        p += dimensionedScalar
        (
            "p",
            p.dimensions(),
            pRefValue - getRefCellValue(p, pRefCell)
        );

        if (perturbationPressureType == "noSplit")
        {
           p_rgh = p;
        }
        else if (perturbationPressureType == "rho0Split")
        {
           p_rgh = p - gh;
        }
        else if (perturbationPressureType == "rhokSplit")
        {
           p_rgh = p - rhok*gh;
        }
    }
