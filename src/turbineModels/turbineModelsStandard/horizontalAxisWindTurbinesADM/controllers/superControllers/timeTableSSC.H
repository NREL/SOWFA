//_SSC_

// Define variables: Matrix with current control actions for each turbine
std::vector< std::vector<float> > nextControlAction(numTurbines,std::vector<float>(nOutputsFromSSC)); 

// Call the SC from external library
SC_timeTable(numTurbines,nOutputsFromSSC,nextControlAction,runTime_.value());

// Print the applied control settings
for(int i=0; i < numTurbines; i++){
	for(int j=0; j<nOutputsFromSSC;j++){
		printf("SSC: Turbine[%d], Updating control variables. Setpoint[%d]: %f.\n",i,j,nextControlAction[i][j]);
	}
}
	
// Format the SSC outputs to the appropriate dimensions and units
for(int i = 0; i < numTurbines; i++)
{
	// In this example, the first entries are the yaw angles
	superInfoFromSSC[i*nOutputsFromSSC] = nextControlAction[i][0]; // Extract from the SSC output
    superInfoFromSSC[i*nOutputsFromSSC] = compassToStandard(superInfoFromSSC[i*nOutputsFromSSC]); // Rotate
    superInfoFromSSC[i*nOutputsFromSSC] = superInfoFromSSC[i*nOutputsFromSSC] * degRad; // Degrees to radians
	
	// In this example, the second entries are the min. blade pitch angles
    superInfoFromSSC[i*nOutputsFromSSC+1] = nextControlAction[i][1]; // Extract from the SSC output
    superInfoFromSSC[i*nOutputsFromSSC+1] = superInfoFromSSC[i*nOutputsFromSSC+1] * degRad; // Degrees to radians
}
