# COPIED AND MODIFIED FROM: http://zguide.zeromq.org/py:hwserver
#
#   Hello World server in Python
#   Binds REP socket to tcp://*:5555
#   Expects b"Hello" from client, replies with b"World"
#
#
# For peregrine: use 'module load python/2.7.6'. 
# Otherwise: make sure you have pyzmq, through 'pip install pyzmq'
#
import time
import zmq

context = zmq.Context()
socket = context.socket(zmq.REP)
socket.bind("tcp://*:5553")

while True:
    #  Wait for next request from client
    message = socket.recv()
    print("Received request: %s" % message)

    #  Do some 'work'
    time.sleep(1)

    #  Send reply back to client
    socket.send(b"263.33 2.0")
