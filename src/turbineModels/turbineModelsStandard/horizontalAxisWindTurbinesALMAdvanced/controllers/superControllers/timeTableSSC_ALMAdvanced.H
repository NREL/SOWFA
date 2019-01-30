//_SSC_

// Define variables: Matrix with current control actions for each turbine
std::vector< std::vector<float> > nextControlAction(numTurbines,std::vector<float>(nOutputsFromSSC)); 

// Call the SC from external library
SC_timeTable_ALMAdvanced(numTurbines,nOutputsFromSSC,nextControlAction,runTime_.value());

// Copy entries to global variable and print the to-be-applied control settings
for(int i=0; i < numTurbines; i++){
    for(int j=0; j<nOutputsFromSSC;j++){
        superInfoFromSSC[i*nOutputsFromSSC+j] = nextControlAction[i][j]; // Extract from the SSC output
        printf("SSC: Turbine[%d], Received control variables. Setpoint[%d]: %f.\n",i,j,nextControlAction[i][j]);
    }
}