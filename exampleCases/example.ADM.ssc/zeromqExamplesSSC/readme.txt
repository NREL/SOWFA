//
// _SSC_ readme.txt  
// Author: Bart Doekemeijer
// Date: May 4, 2018
//

__Description:
This document is a short readme for using the zeromqSSC option for wind farm control. 
Read it carefully and study the examples before applying your own controller to SOWFA.


__Guidelines:
The 'zeromqSSC' interface works using the zeroMQ tool (http://zeromq.org/). This is an
interfacing tool that allows the exchange of information between different programs and
even between different computers using a network communication protocol (e.g., tcp).
This communication typically is between two programs: a 'client' and a 'server'. 

In SOWFA, the 'server' is your controller implemented in the language of choice, and the
'client' is actually (the 'zeromqSSC' function included in) SOWFA. Note that information
is exchanged through a string of an array of numbers. The formatting from array to string,
and then the formatting from string back to array, should be done on both sides: the client
and the server. So, in your case, you will have to account for this in your supercontroller.


__ Usage:
1. Enable the SSC in the turbineArrayProperties, and set 'sscFunction' to 'zeromqSSC'.
2. Set the individual turbine yaw and blade pitch controller to "yawSC" and "PIDSC".
3. Start up your zeroMQ server (i.e., your wind farm controller code using zeroMQ).
4. Start up your SOWFA simulation.


__ Example cases:
Some examples cases are given in this folder for a wind farm controller in C, a wind farm
controller is Python, and a wind farm controller in MATLAB. These are just bare-bone 
functions/placeholders for you to program your wind farm control solution in.

Note: make sure to compile the C-example with -lzmq. E.g., for NRELs Peregrine HPC, use:
gcc -Wall -o cSSC -L /nopt/nrel/apps/zeromq/gcc/4.2.2/lib -lzmq \
    -I /nopt/nrel/apps/zeromq/gcc/4.2.2/include cSSC.c


__Debugging:
If there are any issues: check if this is an issue with SOWFA/zeromqSSC, or if this is a
problem with your controller. If the issue is on the SOWFA side, please submit an 'issue'
on the Github page (https://github.com/NREL/SOWFA). For zeromqSSC-related issues, contact
Bart Doekemeijer (TU Delft) or Paul Fleming (NREL).
