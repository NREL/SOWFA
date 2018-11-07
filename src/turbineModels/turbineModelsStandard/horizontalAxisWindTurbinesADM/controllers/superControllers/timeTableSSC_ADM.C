// _SSC_ 
//  file: timeTableSSC.C
//  by Paul Fleming & Bart Doekemeijer
//  Date: 05/03/2018
//
//  Info: A superController code that loads a table and applies the control settings
//        at the relevant timesteps.
//
//        this function will read an ascii text file defined by "char *inputFile",
//        and apply the specified control settings at the specified time steps. This
//        controller code is compatible with the "yawSC.H" and "PIDSC.H" turbine
//        controller files, but can easily be extended or modified to one's needs.
//

#include <iostream>
#include <vector>
#include <stdio.h>
#include <stdlib.h>

// Read text file and format appropriately
void readInputSSC_ADM(char *inputFile, int nTurbs, int nControlVars,
				  std::vector<int> &nTurbInputs, std::vector< std::vector<float> >&timeArray, 
				  std::vector< std::vector <std::vector<float> > > &controlArray)
{
	float controlTime; // Time at which the control action is applied
	FILE *fp; 		   // Input file handle
	char buffer[500];  // Define variables for handling SC_Struct
	int turbID; 	   // Temporary turbine number
	float temp;        // Temporary variable for scanning over floats in the input file
	
	// Define temporary (multi-dimensional) vectors
	std::vector<float> tmpFloatVector; 					// Temporary float vector
	std::vector< std::vector<float> > tmpFloatMatrix; 	// Temporary float matrix
	std::vector<float> tmpControlVector(nControlVars, 0); // Temporary float vector 
	
	//Open file for reading
	if((fp=fopen(inputFile, "r")) == NULL) {
		fprintf(stderr,"%s: line %d: Cannot find SSC Input File with path '%s'.\n",__FILE__,__LINE__,inputFile);
		exit(1);
	}
	else {
		fprintf(stderr,"Successfully opened SSC Input File '%s'.\n",inputFile);
	}
	
	// Populate timeArray and controlArray for each turbine
	for (int i = 0; i < nTurbs; i++){
		timeArray.push_back(tmpFloatVector);    // Populate with nTurbs vectors
		controlArray.push_back(tmpFloatMatrix); // Populate with nTurbs matrices
	}
	
	//Read the tables until the file end is found 
	printf("SSC: Reading the input file line by line...\n");
	fgets(buffer,500,fp); //Advance past the first comment line
	for (int i = 0; 1 > 0; i++)
	{
		if(fscanf (fp, "%f", &temp) < 1)
			break;
		controlTime = temp; // First entry should be the simulation time (s)
		
		fscanf (fp, "%f", &temp);
		turbID = temp; // Second entry should be the turbine number
		
		// Check if turbID exceeds number of turbines defined
		if (turbID > nTurbs-1) {
			printf("SSC: ERROR. You have defined control inputs for undefined turbines (turbine[%d]). Ignoring this row of inputs.\n",turbID);
			for (int cVar = 0; cVar < nControlVars; cVar++) {
				fscanf (fp, "%f", &temp);
			}			
		} 
		else {
			// Process control settings for this row of text
			timeArray[turbID].push_back(controlTime); 		// Append with controlTime
			printf("  Turbine[%d], time: %f \n", turbID,timeArray[turbID][nTurbInputs[turbID]]);
			for (int cVar = 0; cVar < nControlVars; cVar++) {
				fscanf (fp, "%f", &temp);
				tmpControlVector[cVar] = temp;
				printf("    Control setting [%d]: %f \n", cVar,tmpControlVector[cVar]);
			}
			controlArray[turbID].push_back(tmpControlVector); // Append control vector to controlArray
			nTurbInputs[turbID] = nTurbInputs[turbID]+1; // Add one to the nTurbInputs
		}

	}
	printf("Closing file...\n\n");
	fclose(fp);	 //Close the file
}

// Function to determine next control setting
void lookupControlAction_ADM(float simTime, int nTurbs, int nControlVars, std::vector< std::vector<float> >&timeArray, 
							std::vector< std::vector <std::vector<float> > > &controlArray, std::vector<int> &nTurbInputs, 
							std::vector< std::vector<float> >&nextControlAction)
{
	int controlIndex=0;
	int controlSettingAvailable=0;
	
	// Determine next control setting for each turbine
	for (int i = 0; i < nTurbs; i++){
		controlSettingAvailable=0;
		
		// Find most recent timestamp of control settings for turbine i
		for (int j = 0; j < nTurbInputs[i]; j++){
			if(simTime >= timeArray[i][j]){
				controlIndex = j; 
				controlSettingAvailable=1;
			}			
		}
		
		// Write control settings of most recent time instant to outputs for turbine i
		if (controlSettingAvailable == 1){
			for (int cVar = 0; cVar < nControlVars; cVar++){
				nextControlAction[i][cVar] = controlArray[i][controlIndex][cVar];
			}
		} else {
			printf("SSC: WARNING: NO INITIAL CONDITIONS DEFINED FOR TURBINE %d. ASSUMING ZEROS.\n", i);
			for (int cVar = 0; cVar < nControlVars; cVar++){
				nextControlAction[i][cVar] = 0.0;
			}			
		}
	}
}

void SC_timeTable_ADM(int nTurbs,int nControlVars, std::vector< std::vector<float> >&nextControlAction, float simTime)
{
	char *inputFile = "SC_INPUT.txt"; // Filename of input file/path address relative to main case folder
		
	static int isFirstCall = 1;
	static std::vector<int> nTurbInputs(nTurbs, 0); 	 // Vector with number of input timestaps for each turbine
	static std::vector< std::vector<float> > timeArray;  // Matrix with timestamps for each turbine
	static std::vector< std::vector< std::vector<float> > > controlArray; // Tensor (3D matrix) with control settings for each turbine at each timestamp
			
	// Read input SSC file if first time this function is called
	if(isFirstCall == 1){
		readInputSSC_ADM(inputFile, nTurbs, nControlVars, nTurbInputs, timeArray, controlArray);
		isFirstCall = 0; // disable for future calls
	}
	
	// Retrieve control settings for time instant 'simTime'
	lookupControlAction_ADM(simTime, nTurbs, nControlVars, timeArray,controlArray, nTurbInputs,nextControlAction);
	
	/*
	// Print calculated control settings
	for(int i=0; i < nTurbs; i++){
		for(int j=0; j<nControlVars;j++){
			printf("SSC: Turbine[%d], Updating control variables. Setpoint[%d]: %f.\n",i,j,nextControlAction[i][j]);
		}
	}*/
}


/*
// Example function for stand-alone development & debugging
int main()
{
	// Define integers and arrays
	float simTime;
	int nTurbs = 5;
	int nOutputsFromSSC = 2;
	int nControlVars = nOutputsFromSSC;
	std::vector<float> tmpFloatVector(nControlVars, 0); 	 // Vector with number of input timestaps for each turbine
	std::vector< std::vector<float> > nextControlAction(nTurbs,std::vector<float>(nOutputsFromSSC)); // Matrix with current control actions for each turbine
	
	printf("TIME = 5.0 SECONDS.\n");
	simTime = 5.0;
	SC_timeTable_ADM(nTurbs,nOutputsFromSSC,nextControlAction,simTime);

	printf("TIME = 11 SECONDS.\n");
	simTime = 11.0;
	SC_timeTable_ADM(nTurbs,nOutputsFromSSC,nextControlAction,simTime);

	printf("TIME = 15 SECONDS.\n");
	simTime = 15.0;	
	SC_timeTable_ADM(nTurbs,nOutputsFromSSC,nextControlAction,simTime);

	printf("TIME = 21 SECONDS.\n");
	simTime = 21.0;	
	SC_timeTable_ADM(nTurbs,nOutputsFromSSC,nextControlAction,simTime);
	
    return 0;
}*/