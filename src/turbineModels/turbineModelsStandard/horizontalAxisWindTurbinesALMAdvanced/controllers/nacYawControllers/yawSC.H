// _SSC_

// Yaw controller: set commanded yaw angle in radians
float setpointYaw;
setpointYaw = superInfoFromSSC[i*nOutputsFromSSC];
setpointYaw = compassToStandard(setpointYaw); // Rotate
nacYawCommanded = setpointYaw * degRad; // Degrees to radians
