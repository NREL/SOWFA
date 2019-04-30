Info<< "Reading thermophysical properties\n" << endl;

Info<< "Reading field T\n" << endl;
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

Info<< "Reading field p_rgh\n" << endl;
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

Info<< "Reading field U\n" << endl;
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

#include "createPhi.H"

#include "readTransportProperties.H"

Info<< "Creating turbulence model\n" << endl;
autoPtr<incompressible::turbulenceModel> turbulence
(
    incompressible::turbulenceModel::New(U, phi, laminarTransport)
);

// Create Coriolis force object
CoriolisForce Coriolis(U);

// Create external source term objects
DrivingForce<scalar> temperatureSourceTerm("temperature",T);
DrivingForce<vector> momentumSourceTerm("momentum",U);

// kinematic turbulent thermal thermal conductivity m2/s
Info<< "Reading field kappat\n" << endl;
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


#include "readGravitationalAcceleration.H"


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
    p_rgh
);

label pRefCell = 0;
scalar pRefValue = 0.0;
setRefCell
(
    p,
    p_rgh,
    pimple.dict(),
    pRefCell,
    pRefValue
);

// Create buoyancy model
vector hRef_ = vector::zero;
if (pRefCell != -1)
{
    hRef_ = mesh.C()[pRefCell];
}
reduce(hRef_,sumOp<vector>());
dimensionedVector hRef("hRef",dimLength,hRef_);
    
buoyancyModel Boussinesq(T, TRef, hRef);

#include "adjustPressureLevel.H"

mesh.setFluxRequired(p_rgh.name());

#include "createMRF.H"
#include "createIncompressibleRadiationModel.H"
#include "createFvOptions.H"
