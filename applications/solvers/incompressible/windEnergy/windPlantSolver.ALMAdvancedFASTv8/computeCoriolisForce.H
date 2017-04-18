        // Compute the Coriolis force (neglect the component in the vertical direction).
        fCoriolis = -2.0*(Omega^U);
        if      (upIndex == 0)
        {
             forAll(fCoriolis,cellI)
             {
                  fCoriolis[cellI].x() = 0.0;
             }
        }
        else if (upIndex == 1)
        {
             forAll(fCoriolis,cellI)
             {
                  fCoriolis[cellI].y() = 0.0;
             }
        }
        else if (upIndex == 2)
        {
             forAll(fCoriolis,cellI)
             {
                  fCoriolis[cellI].z() = 0.0;
             }
        }
