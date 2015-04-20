    // Read blending information
    zBlending1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("zBlending1"));
    zBlending2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("zBlending2"));

    blendingFactorU1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorT1"));
    blendingFactorU2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorU2"));
    blendingFactorT1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorT1"));
    blendingFactorT2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorT2"));

    // Check to see if the blending factors or face sizes have changed.  If so, recalculate the U and T
    // blending factor field.
    

    if ((zBlending1 != zBlending1Old) ||
        (zBlending2 != zBlending2Old) ||
        (blendingFactorU1 != blendingFactorU1Old) ||
        (blendingFactorU2 != blendingFactorU2Old) ||
        (blendingFactorT1 != blendingFactorT1Old) ||
        (blendingFactorT2 != blendingFactorT2Old))
    {


        // Write a message to the log file.
        Info << "Updating scheme blending factor field..." << endl;

        // Update the old value of these quantities for checking for updated files.
        zBlending1Old = zBlending1;
        zBlending2Old = zBlending2;

        blendingFactorU1Old = blendingFactorU1;
        blendingFactorU2Old = blendingFactorU2;
        blendingFactorT1Old = blendingFactorT1;
        blendingFactorT2Old = blendingFactorT2;


    // Set the blending factor fields.
    // internal field
    forAll(UBlendingFactor, faceI)
    {
        scalar z = mesh.Cf()[faceI].z();

        if     ( z < zBlending1 )
        {
            UBlendingFactor[faceI] = blendingFactorU1;
            TBlendingFactor[faceI] = blendingFactorT1;
        }
        else if ((z > zBlending1) && (z < zBlending2))
        {
            UBlendingFactor[faceI] = blendingFactorU2 +
                                     0.5 * (blendingFactorU1 - blendingFactorU2) *
                                    (1.0 + Foam::cos(((z - zBlending1)/(zBlending2 - zBlending1))*Foam::constant::mathematical::pi));
            TBlendingFactor[faceI] = blendingFactorT2 +
                                     0.5 * (blendingFactorT1 - blendingFactorT2) *
                                    (1.0 + Foam::cos(((z - zBlending1)/(zBlending2 - zBlending1))*Foam::constant::mathematical::pi));
        }
        else if ( z > zBlending2 )
        {
            UBlendingFactor[faceI] = blendingFactorU2;
            TBlendingFactor[faceI] = blendingFactorT2;
        }
    }

    // boundary field
    forAll(UBlendingFactor.boundaryField(), patchI)
    {
        forAll(UBlendingFactor.boundaryField()[patchI], faceI)
        {
            scalar z = mesh.boundary()[patchI].Cf()[faceI].z();

            if     ( z < zBlending1 )
            {
                UBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorU1;
                TBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorT1;
            }
            else if ((z > zBlending1) && (z < zBlending2))
            {
                UBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorU2 +
                                         0.5 * (blendingFactorU1 - blendingFactorU2) *
                                        (1.0 + Foam::cos(((z - zBlending1)/(zBlending2 - zBlending1))*Foam::constant::mathematical::pi));
                TBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorT2 +
                                         0.5 * (blendingFactorT1 - blendingFactorT2) *
                                        (1.0 + Foam::cos(((z - zBlending1)/(zBlending2 - zBlending1))*Foam::constant::mathematical::pi));
            }
            else if ( z > zBlending2 )
            {
                UBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorU2;
                TBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorT2;
            }
        }
    }
    }
