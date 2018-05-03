//_SSC_
const int MAX_ARRAY = 1000;
float inputArray[MAX_ARRAY];
float outputArray[MAX_ARRAY];

SCSimpleADM(inputArray, outputArray, runTime_.value(), numTurbines );


// Fix the units from compass deg to standard rad and place into the 
// from SC dynamiclist, do this for each turbine
for(int i = 0; i < numTurbines; i++)
{
    // First collect the yaw target and correct
    superInfoFromSSC[i*nOutputsFromSC] = outputArray[i*nOutputsFromSC];
    superInfoFromSSC[i*nOutputsFromSC] = compassToStandard(superInfoFromSSC[i*nOutputsFromSC]);
    superInfoFromSSC[i*nOutputsFromSC] = superInfoFromSSC[i*nOutputsFromSC] * degRad;

    // Next collect the minimum pitch angle
    superInfoFromSSC[i*nOutputsFromSC+1] = outputArray[i*nOutputsFromSC+1];
    superInfoFromSSC[i*nOutputsFromSC+1] = superInfoFromSSC[i*nOutputsFromSC+1] * degRad;
}
