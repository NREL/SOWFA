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

    // ALL OTHER PROPERTIES ARE READ IN INDIVIDUAL CLASS OBJECTS
    label upIndex = 2;
