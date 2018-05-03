//_SSC_
// OVERALL IDEA: SIMPLE CONTROLLER DOES NOTHING WITH THE INPUTS EXCEPT EXTRACT THE TIME AND DETERMINE SETPOINTS FOR YAW AND PITCH

// INPUT/OUTPUT
// inputArray, not used for now
// outputArray, for each turbine, yaw angle (compass deg) and minimum pitch (deg)
// simTime, needed to know what time it is
// numTurbines, number of turbines



#include <stdio.h>
#include <stdlib.h> // for malloc

// The main function of the super controller
void SCSimpleADM(float * inputArray, float * outputArray, float simTime,  int numTurbines)
{

	//Define variables for handling SC_Struct
	char buffer[500];

	//Define firstTimecheck and file variables
	static int isFirstCall = 1;
	FILE *fp; //File ID
	static int numRows = 0;

	//Declare reference vectors
	static float timeVec[1000];
	static int turbineVec[1000];
	static float yawVec[1000];
	static float PitchMinVec[1000];
	float temp;

	//Initialize row and declare memory
	if (isFirstCall == 1)
	{
		fprintf(stderr,"%s %d FIRST CALL \n",__FILE__,__LINE__);

		//Open file for writing
		if((fp=fopen("SC_INPUT.txt", "r")) == NULL) {
			fprintf(stderr,"Cannot open SC Input File.\n");
		}

		fgets(buffer,500,fp);

		//Now read the tables until END is found 
		//for (int i = 0; i < numRows; i++)
		for (int i = 0; 1 > 0; i++)
		{
			if(fscanf (fp, "%f", &temp) < 1)
				break;
			timeVec[i] = temp;
			fscanf (fp, "%f", &temp);
			turbineVec[i] = temp;
			fscanf (fp, "%f", &temp);
			yawVec[i] = temp;
			fscanf (fp, "%f", &temp);
			PitchMinVec[i] = temp;
			fprintf(stderr,"%s %d INPUTS %f %d %f %f \n",__FILE__,__LINE__,timeVec[i],turbineVec[i],yawVec[i],PitchMinVec[i]);
			numRows++;	
	}

		//Close the file
		fclose(fp);

		isFirstCall = 0; //Don't repeat
	}

	//Now loop through the possible conditions and apply references
	for (int i = 0; i < numRows; i++)
	{
		for (int turbineID = 0; turbineID < numTurbines; turbineID ++)
		{
		//fprintf(stderr,"%s %d tID %d turbVec %d localTime %f timeVec %f \n",__FILE__,__LINE__,turbineID,turbineVec[i],simTime,timeVec[i]);

		if (turbineID == turbineVec[i] && simTime >= timeVec[i]) // This condition applies to this turbine at this time
			{
				//fprintf(stderr,"%s %d UPDATING OUTPUT...........................................\n",__FILE__,__LINE__);

				// Set the yaw setpoint
				outputArray[turbineID * 2] = yawVec[i];
				outputArray[turbineID * 2 + 1] = PitchMinVec[i];

			}
		}
	}
}

