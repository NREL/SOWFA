// Write measurements for the superController. In our example case, we have nInputsToSSC = 3

// Proceed turbine by turbine.
    forAll(deltaNacYaw, i)
    {
        superInfoToSSC[i*nInputsToSSC+0] = generatorPower[i];
        superInfoToSSC[i*nInputsToSSC+1] = rotorSpeed[i];
        superInfoToSSC[i*nInputsToSSC+2] = rotorAxialForce[i]; 
    }