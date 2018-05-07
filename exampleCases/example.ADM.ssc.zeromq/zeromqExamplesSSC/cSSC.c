// COPIED AND MODIFIED FROM: http://zguide.zeromq.org/c:hwserver

//  Hello World server

#include <zmq.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>

int main (void)
{
    printf("Setting up connection.\n");

    
    //  Socket to talk to clients
    void *context = zmq_ctx_new ();
    void *responder = zmq_socket (context, ZMQ_REP);
    int rc = zmq_bind (responder, "tcp://*:5552");
    
    assert (rc == 0);
    printf("Set up connection. Attempting to receive signals.\n");
   	
    while (1) {
        char buffer [9900];
		
		// Receive info from SOWFA
        zmq_recv (responder, buffer, 9900, 0);
		printf("Received signal [%s].\n", buffer);
		
		// Do your control stuff
		// ...
		// ... for this example case, just give constant output
		char returnSignal [11] = "263.33 2.0"; // Make sure the dimensions are appropriate
		// ...
		//
		
		// Send back to SOWFA
		printf ("Sending signal back to SOWFA [%s].\n",returnSignal);
		zmq_send (responder, returnSignal, 9900, 0);
		printf("Signal sent\n");
		
	}
    return 0;
}
