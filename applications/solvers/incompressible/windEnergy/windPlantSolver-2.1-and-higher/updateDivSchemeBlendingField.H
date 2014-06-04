    // Re-read blending information
    faceSize1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("faceSize1"));
    faceSize2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("faceSize2"));
    faceSize3 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("faceSize3"));
    faceSize4 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("faceSize4"));

    blendingFactorU1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorU1"));
    blendingFactorU2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorU2"));
    blendingFactorU3 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorU3"));
    blendingFactorU4 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorU4"));

    blendingFactorT1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorT1"));
    blendingFactorT2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorT2"));
    blendingFactorT3 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorT3"));
    blendingFactorT4 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorT4"));


    // Check to see if the blending factors or face sizes have changed.  If so, recalculate the U and T
    // blending factor field.
    if ((faceSize1 != faceSize1Old) ||
        (faceSize2 != faceSize2Old) ||
        (faceSize3 != faceSize3Old) ||
        (faceSize4 != faceSize4Old) ||
        (blendingFactorU1 != blendingFactorU1Old) ||
        (blendingFactorU2 != blendingFactorU2Old) ||
        (blendingFactorU3 != blendingFactorU3Old) ||
        (blendingFactorU4 != blendingFactorU4Old) ||
        (blendingFactorT1 != blendingFactorT1Old) ||
        (blendingFactorT2 != blendingFactorT2Old) ||
        (blendingFactorT3 != blendingFactorT3Old) ||
        (blendingFactorT4 != blendingFactorT4Old))
    {


        // Write a message to the log file.
        Info << "Updating scheme blending factor field..." << endl;


        // Update the old value of these quantities for checking for updated files.
        faceSize1Old = faceSize1;
        faceSize2Old = faceSize2;
        faceSize3Old = faceSize3;
        faceSize4Old = faceSize4;

        blendingFactorU1Old = blendingFactorU1; 
        blendingFactorU2Old = blendingFactorU2; 
        blendingFactorU3Old = blendingFactorU3; 
        blendingFactorU4Old = blendingFactorU4; 

        blendingFactorT1Old = blendingFactorT1; 
        blendingFactorT2Old = blendingFactorT2; 
        blendingFactorT3Old = blendingFactorT3; 
        blendingFactorT4Old = blendingFactorT4; 


        // Update the blending factor fields.
        // internal field
        forAll(UBlendingFactor, faceI)
        {
            scalar area = mesh.magSf()[faceI];

            UBlendingFactor[faceI] = blendingFactorU1;
            TBlendingFactor[faceI] = blendingFactorT1;
 
            if ( area <= faceSize1 )
            {
                UBlendingFactor[faceI] = blendingFactorU1;
                TBlendingFactor[faceI] = blendingFactorT1;
            }
            if ( area <= faceSize2 )
            {
                UBlendingFactor[faceI] = blendingFactorU2;
                TBlendingFactor[faceI] = blendingFactorT2;
            }
            if ( area <= faceSize3 )
            {
                UBlendingFactor[faceI] = blendingFactorU3;
                TBlendingFactor[faceI] = blendingFactorT3;
            }
            if ( area <= faceSize4 )
            {
                UBlendingFactor[faceI] = blendingFactorU4;
                TBlendingFactor[faceI] = blendingFactorT4;
            }
        }

        // boundary field
        forAll(UBlendingFactor.boundaryField(), patchI)
        {
            forAll(UBlendingFactor.boundaryField()[patchI], faceI)
            {
                scalar area = mesh.boundary()[patchI].magSf()[faceI];

                UBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorU1;
                TBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorT1;

                if ( area < faceSize1 )
                {
                    UBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorU1;
                    TBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorT1;
                }
                if ( area < faceSize2 )
                {
                    UBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorU2;
                    TBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorT2;
                }
                if ( area < faceSize3 )
                {
                    UBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorU3;
                    TBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorT3;
                }
                if ( area < faceSize4 )
                {
                    UBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorU4;
                    TBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorT4;
                }
            }
        }
    }
