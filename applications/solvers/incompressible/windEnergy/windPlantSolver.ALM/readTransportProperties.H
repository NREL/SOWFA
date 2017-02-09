    // Set up the model for laminar (molecular) transport
    // (i.e. the model for molecular viscosity)
    singlePhaseTransportModel laminarTransport(U, phi);

    // Reference temperature [K]
    dimensionedScalar TRef(laminarTransport.lookup("TRef"));
    
    // Laminar (molecular) Prandtl number
    dimensionedScalar Pr(laminarTransport.lookup("Pr"));

    // Turbulent Prandtl number
    dimensionedScalar Prt(laminarTransport.lookup("Prt"));
