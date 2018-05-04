// _SSC_
//For each turbine, yaw 1 deg/s in direction of error, 
// if error is less than this increment do nothing

//Compute the yaw error
yawError = superInfoFromSSC[i * nOutputsFromSSC] - nacYaw[i];

//Wrap the yaw to be +/- 180
if (yawError < -1.0 * Foam::constant::mathematical::pi )
	yawError = yawError + 2.0 * Foam::constant::mathematical::pi;
if (yawError >  1.0 * Foam::constant::mathematical::pi )
	yawError = yawError - 2.0 * Foam::constant::mathematical::pi;

// Grab the absolute value
// Note, not using abs, because it rounds to int
if (yawError >= 0.0)
	yawErrorAbs = yawError;
else
	yawErrorAbs = -1 * yawError;

// Decide if yawing, and set yaw delta at delta angle giving 1 Deg/s
if ( yawErrorAbs < Foam::constant::mathematical::pi / 180.0 * runTime_.deltaT().value() -.001) // small error
	deltaNacYaw[i] = 0.0; // Do nothing
else if  (yawError < 0)
	deltaNacYaw[i] = -1 * Foam::constant::mathematical::pi / 180.0 * runTime_.deltaT().value();  // Yaw negative
else
	deltaNacYaw[i] = Foam::constant::mathematical::pi / 180.0 * runTime_.deltaT().value();  // Yaw positive

// Write measurements for the superController
for(int i = 0; i < numTurbines; i++){
	// In our example case, we have nInputsToSSC = 3
	superInfoToSSC[i*nInputsToSSC+0] = powerGenerator[i]; // Generator power
	superInfoToSSC[i*nInputsToSSC+1] = torqueRotor[i];    // Turbine rotor torque
	superInfoToSSC[i*nInputsToSSC+2] = thrust[i];         // Turbine thrust force
}