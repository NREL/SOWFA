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

    // Read blending information
    scalar xBlending1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("xBlending1"));
    scalar xBlending2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("xBlending2"));
    scalar xBlending3 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("xBlending3"));
    scalar xBlending4 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("xBlending4"));

    scalar blendingFactor1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactor1"));
    scalar blendingFactor2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactor2"));

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
