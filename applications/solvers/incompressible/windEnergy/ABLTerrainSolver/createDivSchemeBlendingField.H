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


    // Read blending information.
    scalar faceSize1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("faceSize1"));
    scalar faceSize2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("faceSize2"));
    scalar faceSize3 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("faceSize3"));
    scalar faceSize4 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("faceSize4"));

    scalar blendingFactorU1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorU1"));
    scalar blendingFactorU2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorU2"));
    scalar blendingFactorU3 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorU3"));
    scalar blendingFactorU4 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorU4"));

    scalar blendingFactorT1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorT1"));
    scalar blendingFactorT2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorT2"));
    scalar blendingFactorT3 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorT3"));
    scalar blendingFactorT4 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("blendingFactorT4"));


    // Create the old value of these quantities for checking for updated files.
    scalar faceSize1Old = faceSize1;
    scalar faceSize2Old = faceSize2;
    scalar faceSize3Old = faceSize3;
    scalar faceSize4Old = faceSize4;

    scalar blendingFactorU1Old = blendingFactorU1; 
    scalar blendingFactorU2Old = blendingFactorU2; 
    scalar blendingFactorU3Old = blendingFactorU3; 
    scalar blendingFactorU4Old = blendingFactorU4; 

    scalar blendingFactorT1Old = blendingFactorT1; 
    scalar blendingFactorT2Old = blendingFactorT2; 
    scalar blendingFactorT3Old = blendingFactorT3; 
    scalar blendingFactorT4Old = blendingFactorT4; 


    // Set the blending factor fields.
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
