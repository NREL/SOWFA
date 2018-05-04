//_SSC_
static std::vector<float> zeromqInfoToSC;
static std::vector<float> zeromqInfoFromSC;
static int isFirstCall = 1;
int sizeInfoFromSSC = nInputsToSSC*numTurbines;

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
	

// Pass the inputs to the std::vector format
for(int i = 0; i < nInputsToSSC*numTurbines; i++){
	zeromqInfoToSC[i] = superInfoToSSC[i];
}
	
// Call the SC from external library
SC_zeromq(runTime_.value(), zeromqInfoToSC, zeromqInfoFromSC, sizeInfoFromSSC );
//SC_zeromq(timeStep, zeromqInfoToSC, zeromqInfoFromSC, sizeInfoFromSSC );

// Print the applied control settings
for(int i=0; i < numTurbines; i++){
	for(int j=0; j<nOutputsFromSC;j++){
		printf("SSC: Turbine[%d], Updating control variables. Setpoint[%d]: %f.\n",i,j,zeromqInfoFromSC[i*numTurbines+j]);
	}
}

// Format the SSC outputs to the appropriate dimensions and units
for(int i = 0; i < numTurbines; i++)
{
	// In this example, the first entries are the yaw angles
	superInfoFromSSC[i*nOutputsFromSC] = zeromqInfoFromSC[i*nOutputsFromSC]; // Extract from the SSC output
    superInfoFromSSC[i*nOutputsFromSC] = compassToStandard(superInfoFromSSC[i*nOutputsFromSC]); // Rotate
    superInfoFromSSC[i*nOutputsFromSC] = superInfoFromSSC[i*nOutputsFromSC] * degRad; // Degrees to radians
	
	// In this example, the second entries are the min. blade pitch angles
    superInfoFromSSC[i*nOutputsFromSC+1] = zeromqInfoFromSC[i*nOutputsFromSC+1]; // Extract from the SSC output
    superInfoFromSSC[i*nOutputsFromSC+1] = superInfoFromSSC[i*nOutputsFromSC+1] * degRad; // Degrees to radians
}