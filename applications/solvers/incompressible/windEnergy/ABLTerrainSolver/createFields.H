    // Create and read the potential temperature field
    Info << "Creating and reading potential temperature field, T..." << endl;
    volScalarField T
    (
        IOobject
        (
            "T",
            runTime.timeName(),
            mesh,
            IOobject::MUST_READ,
            IOobject::AUTO_WRITE
        ),
        mesh
    );

    // Create initial average temperature field
    Info<< "Creating mean temperature field, Tmean..." << endl;
    volScalarField Tmean
    (
        IOobject
        (
            "Tmean",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::NO_WRITE
        ),
        T
    );

    // Create initial fluctuating temperature field
    Info<< "Creating fluctuating temperature field, Tprime..." << endl;
    volScalarField Tprime
    (
        IOobject
        (
            "Tprime",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        T
    );

    // Create and read the modified pressure field
    Info << "Creating and reading modified pressure field, p_rgh..." << endl;
    volScalarField p_rgh
    (
        IOobject
        (
            "p_rgh",
            runTime.timeName(),
            mesh,
            IOobject::MUST_READ,
            IOobject::AUTO_WRITE
        ),
        mesh
    );

    // Create and read the velocity field
    Info << "Creating and reading velocity field, U..." << endl;
    volVectorField U
    (
        IOobject
        (
            "U",
            runTime.timeName(),
            mesh,
            IOobject::MUST_READ,
            IOobject::AUTO_WRITE
        ),
        mesh
    );


    // Create initial average velocity field
    Info<< "Creating mean velocity field, Umean..." << endl;
    volVectorField Umean
    (
        IOobject
        (
            "Umean",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::NO_WRITE
        ),
        U
    );

    // Create initial fluctuating velocity field
    Info<< "Creating fluctuating velocity field, Uprime..." << endl;
    volVectorField Uprime
    (
        IOobject
        (
            "Uprime",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::AUTO_WRITE
        ),
        U
    );

    // Create and calculate the velocity flux
    Info << "Creating and calculating velocity flux field, phi..." << endl;
    #include "createPhi.H"

    // Read the transport properties and set up the laminar (molecular) transport model
    Info << "Reading transport properties..." << endl;
    #include "readTransportProperties.H"

    // Read the atmospheric boundary layer specific properties
    Info << "Reading the atmospheric boundary layer properties..." << endl;
    #include "readABLProperties.H"

    // Create the turbulence model (RANS, LES, or none)
    Info << "Creating turbulence model..." << endl;
    autoPtr<incompressible::turbulenceModel> turbulence
    (
        incompressible::turbulenceModel::New(U, phi, laminarTransport)
    );

    // Create Coriolis force vector
    Info << "Creating the Coriolis force vector, fCoriolis..." << endl;
    volVectorField fCoriolis
    (
        IOobject
        (
            "fCoriolis",
            runTime.timeName(),
            mesh,
            IOobject::READ_IF_PRESENT,
            IOobject::NO_WRITE
        ),
        mesh,
        dimensionedVector("fCoriolis",dimensionSet(0, 1, -2, 0, 0, 0, 0),vector::zero)
    );

    // Create and calculate the Boussinesq density field for computing buoyancy forces
    Info << "Creating kinematic (Boussinesq) density field, rhok..." << endl;
    volScalarField rhok
    (
        IOobject
        (
            "rhok",
            runTime.timeName(),
            mesh
        ),
        1.0 - ( (T - TRef)/TRef )
    );   

    // Create and read the turbulent thermal conductivity field
    Info << "Creating the kinematic thermal conductivity field, kappat..." << endl;
    volScalarField kappat
    (
        IOobject
        (
            "kappat",
            runTime.timeName(),
            mesh,
            IOobject::MUST_READ,
            IOobject::AUTO_WRITE
        ),
        mesh
    );

    // Create the wall shear stress field
    Info << "Reading and creating the wall shear stress field, Rwall..." << endl;
    volSymmTensorField Rwall
    (
        IOobject
        (
            "Rwall",
            runTime.timeName(),
            mesh,
            IOobject::MUST_READ,
            IOobject::AUTO_WRITE
        ),
        mesh
    );
    
    // Create the wall temperature flux field
    Info << "Reading and creating the wall temperature flux field, qwall..." << endl;
    volVectorField qwall
    (
        IOobject
        (
            "qwall",
            runTime.timeName(),
            mesh,
            IOobject::MUST_READ,
            IOobject::AUTO_WRITE
        ),
        mesh
    );

    // Create and calculate the gravity potential field
    Info << "Creating and calculating the gravity potential field, gh and ghf..." << endl;
    volScalarField gh("gh", g & mesh.C());
    surfaceScalarField ghf("ghf", g & mesh.Cf());

    // Create and calculate the static pressure field
    Info << "Creating and calculating the static pressure field, p..." << endl;
    volScalarField p
    (
        IOobject
        (
            "p",
            runTime.timeName(),
            mesh,
            IOobject::NO_READ,
            IOobject::AUTO_WRITE
        ),
        p_rgh + rhok*gh
    );

    // Set up the pressure reference cell information
    Info << "Setting up the pressure reference cell information..." << endl;
    label pRefCell = 0;
    scalar pRefValue = 0.0;
    setRefCell
    (
        p,
        p_rgh,
        mesh.solutionDict().subDict("PIMPLE"),
        pRefCell,
        pRefValue
    );

    if (p_rgh.needReference())
    {
        p += dimensionedScalar
        (
            "p",
            p.dimensions(),
            pRefValue - getRefCellValue(p, pRefCell)
        );
    }

