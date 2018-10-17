    // Compute the full pressure and adjust the pressure level.
    Boussinesq.updateBackgroundPressure();
    p = p_rgh + Boussinesq.backgroundPressure();

    if (p_rgh.needReference())
    {
        p += dimensionedScalar
        (
            "p",
            p.dimensions(),
            pRefValue - getRefCellValue(p, pRefCell)
        );

        p_rgh = p - Boussinesq.backgroundPressure();
    }
