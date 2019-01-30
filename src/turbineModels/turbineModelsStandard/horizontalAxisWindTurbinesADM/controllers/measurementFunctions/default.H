// Write measurements for the superController. In our example case, we have nInputsToSSC = 3

// Proceed turbine by turbine.
    forAll(deltaNacYaw, i)
    {
        superInfoToSSC[i*nInputsToSSC+0] = powerGenerator[i]; // Generator power
        superInfoToSSC[i*nInputsToSSC+1] = torqueRotor[i];    // Turbine rotor torque
        superInfoToSSC[i*nInputsToSSC+2] = thrust[i];         // Turbine thrust force
    }