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
    superInfoFromSSC[i*nOutputsFromSSC] = outputArray[i*nOutputsFromSSC];
    superInfoFromSSC[i*nOutputsFromSSC] = compassToStandard(superInfoFromSSC[i*nOutputsFromSSC]);
    superInfoFromSSC[i*nOutputsFromSSC] = superInfoFromSSC[i*nOutputsFromSSC] * degRad;

    // Next collect the minimum pitch angle
    superInfoFromSSC[i*nOutputsFromSSC+1] = outputArray[i*nOutputsFromSSC+1];
    superInfoFromSSC[i*nOutputsFromSSC+1] = superInfoFromSSC[i*nOutputsFromSSC+1] * degRad;
}
