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
SC_zeromq(zmqAddress, runTime_.value(), zeromqInfoToSC, zeromqInfoFromSC, sizeInfoFromSSC );

// Copy entries to global variable and print the to-be-applied control settings
for(int i=0; i < numTurbines; i++){
    for(int j=0; j<nOutputsFromSSC;j++){
        superInfoFromSSC[i*nOutputsFromSSC+j] = zeromqInfoFromSC[i*nOutputsFromSSC+j]; // Extract from the SSC output
        printf("SSC: Turbine[%d], Received control variables. Setpoint[%d]: %f.\n",i,j,zeromqInfoFromSC[i*nOutputsFromSSC+j]);
    }
}