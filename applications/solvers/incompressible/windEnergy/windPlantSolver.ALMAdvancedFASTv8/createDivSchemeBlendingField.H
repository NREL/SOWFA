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
    //
    List<List<scalar> > profileTable(mesh.schemesDict().subDict("schemeBlending").lookup("faceSizeBlendingTable"));
    scalarField faceAreaList(profileTable.size(),0.0);
    scalarField UBlendingList(profileTable.size(),0.0);
    scalarField TBlendingList(profileTable.size(),0.0);
    forAll(faceAreaList,i)
    {
        faceAreaList[i] = profileTable[i][0];
        UBlendingList[i] = profileTable[i][1];
        TBlendingList[i] = profileTable[i][2];
    }

    scalar zBlending1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("heightBlending_z1"));
    scalar zBlending2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("heightBlending_z2"));

    scalar blendingFactorUz1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("heightBlendingFactorU_z1"));
    scalar blendingFactorTz1 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("heightBlendingFactorT_z1"));
    scalar blendingFactorUz2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("heightBlendingFactorU_z2"));
    scalar blendingFactorTz2 = readScalar(mesh.schemesDict().subDict("schemeBlending").lookup("heightBlendingFactorT_z2"));

    bool useWallDistZ(mesh.schemesDict().subDict("schemeBlending").lookupOrDefault<bool>("useWallDistZ",false));




    // Create the old value of these quantities for checking for updated files.
    List<List<scalar> > profileTableOld = profileTable;

    scalar zBlending1Old = zBlending1;
    scalar zBlending2Old = zBlending2;

    scalar blendingFactorUz1Old = blendingFactorUz1;
    scalar blendingFactorTz1Old = blendingFactorTz1;
    scalar blendingFactorUz2Old = blendingFactorUz2;
    scalar blendingFactorTz2Old = blendingFactorTz2;

    bool useWallDistZOld = useWallDistZ;



    // Get distance from the wall
    vector up = vector::zero;
    up.z() = 1.0;
    surfaceScalarField dFace = mesh.Cf() & up;
    if (useWallDistZ)
    {
        Info << "Calculating wall distance..." << endl;
        wallDist d(mesh);
        dFace = fvc::interpolate(d);
    }



    // Set the blending factor fields.
    // internal field
    forAll(UBlendingFactor, faceI)
    {
        scalar area = mesh.magSf()[faceI];


        UBlendingFactor[faceI] = max(min(interpolateSplineXY(area,faceAreaList,UBlendingList),1.0),0.0);
        TBlendingFactor[faceI] = max(min(interpolateSplineXY(area,faceAreaList,TBlendingList),1.0),0.0);
 
   
        scalar z = 0.0;
        if (useWallDistZ)
        {
            z = dFace[faceI];
        }
        else
        {    
            z = mesh.Cf()[faceI].z();
        }

        if ((z > zBlending1) && (z < zBlending2))
        {
            UBlendingFactor[faceI] = blendingFactorUz2 +
                                     0.5 * (blendingFactorUz1 - blendingFactorUz2) *
                                    (1.0 + Foam::cos(((z - zBlending1)/(zBlending2 - zBlending1))*Foam::constant::mathematical::pi));
            TBlendingFactor[faceI] = blendingFactorTz2 +
                                     0.5 * (blendingFactorTz1 - blendingFactorTz2) *
                                    (1.0 + Foam::cos(((z - zBlending1)/(zBlending2 - zBlending1))*Foam::constant::mathematical::pi));
        }
        else if ( z > zBlending2 )
        {
            UBlendingFactor[faceI] = blendingFactorUz2;
            TBlendingFactor[faceI] = blendingFactorTz2;
        }

    }

    // boundary field
    forAll(UBlendingFactor.boundaryField(), patchI)
    {
        forAll(UBlendingFactor.boundaryField()[patchI], faceI)
        {
            scalar area = mesh.boundary()[patchI].magSf()[faceI];

            UBlendingFactor.boundaryField()[patchI][faceI] = max(min(interpolateSplineXY(area,faceAreaList,UBlendingList),1.0),0.0);
            TBlendingFactor.boundaryField()[patchI][faceI] = max(min(interpolateSplineXY(area,faceAreaList,TBlendingList),1.0),0.0);


            scalar z = 0.0;
            if (useWallDistZ)
            {
                z = dFace.boundaryField()[patchI][faceI];
            }
            else
            {
                z = mesh.boundary()[patchI].Cf()[faceI].z();
            }


            if ((z > zBlending1) && (z < zBlending2))
            {
                UBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorUz2 +
                                         0.5 * (blendingFactorUz1 - blendingFactorUz2) *
                                        (1.0 + Foam::cos(((z - zBlending1)/(zBlending2 - zBlending1))*Foam::constant::mathematical::pi));
                TBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorTz2 +
                                         0.5 * (blendingFactorTz1 - blendingFactorTz2) *
                                        (1.0 + Foam::cos(((z - zBlending1)/(zBlending2 - zBlending1))*Foam::constant::mathematical::pi));
            }
            else if ( z > zBlending2 )
            {
                UBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorUz2;
                TBlendingFactor.boundaryField()[patchI][faceI] = blendingFactorTz2;
            }


        }
    }
