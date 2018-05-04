clear all; close all; clc;
%
% MATLAB can use zeroMQ, but it is not necessarily so straight-forward. The easiest solution found was
% using "jeroMQ", which can be downloaded from https://github.com/zeromq/jeromq. After installation,
% Update the path below and you should be all set.
%
% For more information, check out:
% https://mathworks.com/matlabcentral/answers/269061-how-do-i-integrate-zeromq-library-with-matlab-i-want-my-matlab-program-to-be-a-subscriber-of-zeromq
% 
% Note: to install jeroMQ, you need to have 'maven' installed. When using Maven to install jeroMQ,
% you may run into an error about the unit testing. If so, disable them and run again using
% 'mvn install -DskipTests'
%
% Recommended Java JDK version: 1.8.0_171 (tested by excluding unit tests)
%
%
javaaddpath('/home/bartdoekemeijer/OpenFOAM_Develop/jeroMQ/jeromq/target/jeromq-0.4.4-SNAPSHOT.jar','-dpath');

import org.zeromq.*


disp(['Setting up the server...']);
context = zmq.Ctx();
socket = context.createSocket(ZMQ.REP);
socket.bind('tcp://*:5553');

noMessagesReceived = 0;
disp(['Entering loop for collecting and sending data...']);
while 1

	%% Receive information from SOWFA
    % message = socket.recv(0); % Wait indefinitely until found a transmitter
    message = socket.recv(1); % Do not wait for transmitter   
    if length(message) <= 0
        %disp([datestr(rem(now,1)) '__ no message received.']);
        pause(0.05); % wait 50 ms
    else
        json_data = native2unicode(message.data)'; % Received message
		disp([datestr(rem(now,1)) '__ message received: [' json_data '].']);
        
		messageRecv = message;
		
		% Do your control input stuff here
		% ...
		pause(1); % Do work: find control action
		str_send = '263.35 2.0';
        % ...
		% ...
		
        % Send a message (control action) back to SOWFA
        noMessagesReceived = noMessagesReceived + 1;
        message = zmq.Msg(length(str_send));
        message.put(unicode2native(str_send));
        socket.send(message, 0);
    end
end
% Close connection
socket.close()