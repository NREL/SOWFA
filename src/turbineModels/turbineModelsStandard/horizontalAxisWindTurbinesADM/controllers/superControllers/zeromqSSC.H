// _SSC_ zeromqSSC.C
//
// This superController code serves as a zeroMQ client, which will communicate
// with a zeroMQ server. The zeroMQ server will be the remote controller, such as
// a MATLAB script, a Python script, or any other piece of software using the
// zeroMQ functionality.
//
// Authors: Bart Doekemeijer & Paul Fleming 
//
static std::vector<float> zeromqInfoToSC;
static std::vector<float> zeromqInfoFromSC;
static int isFirstCall = 1;
int sizeInfoFromSSC = nOutputsFromSSC*numTurbines;

if (isFirstCall == 1){
	// Initialize std::vectors with appropriate dimensions
	for (int i=0; i < numTurbines*nInputsToSSC; i++){
		zeromqInfoToSC.push_back(0.0); // Initialize with zero values
		//
	}
	for (int i=0; i < sizeInfoFromSSC; i++){
		zeromqInfoFromSC.push_back(0.0); // Initialize with zero values
	}
	isFirstCall = -1; // disable for future calls
}
	

// Pass the SSC inputs to the local std::vector format
for(int i = 0; i < nInputsToSSC*numTurbines; i++){
	zeromqInfoToSC[i] = superInfoToSSC[i];
}
	
// Call the SC from external library
SC_zeromq(runTime_.value(), zeromqInfoToSC, zeromqInfoFromSC, sizeInfoFromSSC );

// Print the applied control settings
for(int i=0; i < numTurbines; i++){
	for(int j=0; j<nOutputsFromSSC;j++){
		printf("SSC: Turbine[%d], Updating control variables. Setpoint[%d]: %f.\n",i,j,zeromqInfoFromSC[i*numTurbines+j]);
	}
}

// Format the SSC outputs to the appropriate dimensions and units
for(int i = 0; i < numTurbines; i++)
{
	// In this example, the first entries are the yaw angles
	superInfoFromSSC[i*nOutputsFromSSC] = zeromqInfoFromSC[i*nOutputsFromSSC]; // Extract from the SSC output
    superInfoFromSSC[i*nOutputsFromSSC] = compassToStandard(superInfoFromSSC[i*nOutputsFromSSC]); // Rotate
    superInfoFromSSC[i*nOutputsFromSSC] = superInfoFromSSC[i*nOutputsFromSSC] * degRad; // Degrees to radians
	
	// In this example, the second entries are the min. blade pitch angles
    superInfoFromSSC[i*nOutputsFromSSC+1] = zeromqInfoFromSC[i*nOutputsFromSSC+1]; // Extract from the SSC output
    superInfoFromSSC[i*nOutputsFromSSC+1] = superInfoFromSSC[i*nOutputsFromSSC+1] * degRad; // Degrees to radians
}