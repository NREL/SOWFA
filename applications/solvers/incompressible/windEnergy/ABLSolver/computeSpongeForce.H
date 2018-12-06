        // Compute the sponge layer damping force
        if (spongeLayerType == "Rayleigh")
        {
            fSponge = spongeLayerViscosity * (spongeLayerReferenceVelocity - U);
        }
        else if (spongeLayerType == "viscous")
        {
            fSponge = fvc::laplacian(spongeLayerViscosity,U);
        }
