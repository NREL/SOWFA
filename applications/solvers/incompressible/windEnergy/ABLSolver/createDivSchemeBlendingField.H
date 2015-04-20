    // Create the blending factor fields.
    Info << "Creating divergence scheme blending factor field, UBlendingFactor..." << endl;
    surfaceScalarField UBlendingFactor
    (
        IOobject
        (
            "UBlendingFactor",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        1.0
    );

    Info << "Creating divergence scheme blending factor field, TBlendingFactor..." << endl;
    surfaceScalarField TBlendingFactor
    (
        IOobject
        (
            "TBlendingFactor",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        mesh,
        1.0
    );


    // Read blending information
    scalar zBlending1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("zBlending1"));
    scalar zBlending2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("zBlending2"));

    scalar blendingFactorU1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorU1"));
    scalar blendingFactorU2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorU2"));
    scalar blendingFactorT1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorT1"));
    scalar blendingFactorT2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorT2"));

    scalar zBlending1Old = zBlending1;
    scalar zBlending2Old = zBlending2;

    scalar blendingFactorU1Old = blendingFactorU1;
    scalar blendingFactorU2Old = blendingFactorU2;
    scalar blendingFactorT1Old = blendingFactorT1;
    scalar blendingFactorT2Old = blendingFactorT2;



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
