    // Read blending information
    xBlending1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("xBlending1"));
    xBlending2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("xBlending2"));
    xBlending3 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("xBlending3"));
    xBlending4 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("xBlending4"));

    blendingFactor1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactor1"));
    blendingFactor2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactor2"));

    // Check to see if the blending factors or face sizes have changed.  If so, recalculate the U and T
    // blending factor field.
    

    if ((xBlending1 != xBlending1Old) ||
        (xBlending2 != xBlending2Old) ||
        (xBlending3 != xBlending3Old) ||
        (xBlending4 != xBlending4Old) ||
        (blendingFactor1 != blendingFactor1Old) ||
        (blendingFactor2 != blendingFactor2Old))
    {


        // Write a message to the log file.
        Info << "Updating scheme blending factor field..." << endl;

        // Update the old value of these quantities for checking for updated files.
        xBlending1Old = xBlending1;
        xBlending2Old = xBlending2;
        xBlending3Old = xBlending3;
        xBlending4Old = xBlending4;

        blendingFactor1Old = blendingFactor1;
        blendingFactor2Old = blendingFactor2;




    // Create blending factor field
    // internal field
    forAll(UBlendingFactor, faceI)
    {
        scalar x = mesh.Cf()[faceI].x();
 
        if     ( x < xBlending1 )
        {
            UBlendingFactor[faceI] = blendingFactor1;
        }
        else if ((x > xBlending1) && (x < xBlending2))
        {
            UBlendingFactor[faceI] = blendingFactor2 + 
                                     0.5 * (blendingFactor1 - blendingFactor2) * 
                                    (1.0 + Foam::cos(((x - xBlending1)/(xBlending2 - xBlending1))*Foam::constant::mathematical::pi));
        }
        else if ((x > xBlending2) && (x < xBlending3))
        {
            UBlendingFactor[faceI] = blendingFactor2;
        }
        else if ((x > xBlending3) && (x < xBlending4))
        {
            UBlendingFactor[faceI] = blendingFactor1 + 
                                     0.5 * (blendingFactor2 - blendingFactor1) * 
                                    (1.0 + Foam::cos(((x - xBlending3)/(xBlending4 - xBlending3))*Foam::constant::mathematical::pi));
        }
        else if ( x > xBlending4 )
        {
            UBlendingFactor[faceI] = blendingFactor1;
        }
    }

    // boundary field
    forAll(UBlendingFactor.boundaryField(), patchI)
    {
        forAll(UBlendingFactor.boundaryField()[patchI], faceI)
        {
            scalar x = mesh.boundary()[patchI].Cf()[faceI].x();

            if     ( x < xBlending1 )
            {
                UBlendingFactor.boundaryField()[patchI][faceI] = blendingFactor1;
            }
            else if ((x > xBlending1) && (x < xBlending2))
            {
                UBlendingFactor.boundaryField()[patchI][faceI] = blendingFactor2 +
                                         0.5 * (blendingFactor1 - blendingFactor2) *
                                        (1.0 + Foam::cos(((x - xBlending1)/(xBlending2 - xBlending1))*Foam::constant::mathematical::pi));
            }
            else if ((x > xBlending2) && (x < xBlending3))
            {
                UBlendingFactor.boundaryField()[patchI][faceI] = blendingFactor2;
            }
            else if ((x > xBlending3) && (x < xBlending4))
            {
                UBlendingFactor.boundaryField()[patchI][faceI] = blendingFactor1 +
                                         0.5 * (blendingFactor2 - blendingFactor1) *
                                        (1.0 + Foam::cos(((x - xBlending3)/(xBlending4 - xBlending3))*Foam::constant::mathematical::pi));
            }
            else if ( x > xBlending4 )
            {
                UBlendingFactor.boundaryField()[patchI][faceI] = blendingFactor1;
            }
        }
    }
    }
