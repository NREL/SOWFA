// _SSC_ zeromqSSC.C
//
// This superController code serves as a zeroMQ client, which will communicate
// with a zeroMQ server. The zeroMQ server will be the remote controller, such as
// a MATLAB script, a Python script, or any other piece of software using the
// zeroMQ functionality.
//
// Authors: Bart Doekemeijer & Paul Fleming 
//
#include <zmq.h> // zeroMQ interface
#include <vector>
#include <stdio.h>
#include <unistd.h>
#include <iostream>
#include <string>
#include <sstream>

void SC_zeromq(float timeStep, std::vector<float> infoToSC, std::vector<float> &infoFromSC, int sizeInfoFromSSC )
{
    static int isFirstCall = 1;
    static void *context = zmq_ctx_new ();
    static void *requester = zmq_socket (context, ZMQ_REQ);
    std::string strToSSC; // String that gets sent to SSC
    char charFromSSC [9900];
    
    // Establish connection of this is the first call to the SC
    if (isFirstCall){
        printf ("0mq client: Connecting to 0mq server...\n");
        zmq_connect (requester, "tcp://localhost:5553");
        isFirstCall = 0;  // disable for future calls
    }

    // Format the to be transmitted data from std::vector to char
    std::stringstream ssToSC; // stringStream used to construct strToSSC
    ssToSC << timeStep; // First entry is always timestep
    for(size_t i = 0; i < infoToSC.size(); ++i) {
      ssToSC << " " << infoToSC[i];
    }
    strToSSC = ssToSC.str();
    std::cout << "infoToSC: [" << strToSSC << "] \n";
    
    // Send and receive from SSC through zeroMQ
    zmq_send (requester, strToSSC.c_str(), 9900, 0);
    zmq_recv (requester, charFromSSC, 9900, 0);
    
    // Format received char/string to std::vector
    std::cout << "infoFromSC: [" << charFromSSC << "] \n";
    std::stringstream ss(charFromSSC);
    for(int i=0;i<sizeInfoFromSSC;i++){
      ss >> infoFromSC[i];
      //printf("infoFromSC[%d] = %f \n",i,infoFromSC[i]);
    }
}

/*
int main()
{
    float timeStep = 10.0;
    static std::vector<float> infoToSC;
    static std::vector<float> infoFromSC;

    const int numTurbines = 2;
    const int nInputsToSSC = 2;
    const int nOutputsFromSSC = 2;
    
    for(int i = 0; i < 3; i++){
        printf("Call %d.\n",i);
        #include "zeromqSSC.H"
    }
    
    return 0;
}
*/