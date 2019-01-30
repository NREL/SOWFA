% clear all; close all; clc;
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

% Setup zeroMQ server
zmqJar = '/home/bmdoekemeijer/OpenFOAM/zeroMQ/jeromq-0.4.4-SNAPSHOT.jar';
zmqPort = 5553; % Port to connect to (must match turbineArrayProperies)
zmqTimeout = 3600; % [s]
zmqVerbose = true; % Output text
zmqServer = zeromqObj(zmqJar,zmqPort,zmqTimeout,zmqVerbose);

disp(['Entering wind farm controller loop...']);
measurementVector = [];
while 1
    % Receive information from SOWFA
    dataReceived = zmqServer.receive();
    currentTime  = dataReceived(1,1);
    measurementVector = [measurementVector;dataReceived(1,2:end)];
    
    if currentTime < 10
        yawAngleArrayOut   = [270.0 270.0];
        pitchAngleArrayOut = [  0.0   0.0];
    elseif currentTime < 20
        yawAngleArrayOut   = [260.0 270.0];
        pitchAngleArrayOut = [  0.0   0.0];
    elseif currentTime < 30
        yawAngleArrayOut   = [260.0 280.0];
        pitchAngleArrayOut = [  0.0   0.0];
    elseif currentTime < 50
        yawAngleArrayOut   = [263.0 285.0];
        pitchAngleArrayOut = [  0.0   0.0];
    else
        yawAngleArrayOut   = [240.0 270.0];
        pitchAngleArrayOut = [  0.0   0.0];
    end
    disp([datestr(rem(now,1)) '__    Synthesizing message string.']);
    dataSend = setupZmqSignal(yawAngleArrayOut,pitchAngleArrayOut);
        
    % Send a message (control action) back to SOWFA
    zmqServer.send(dataSend);
end
% Close connection
zmqServer.disconnect()

function [dataOut] = setupZmqSignal(yawAngles,pitchAngles)
dataOut = [];
for i = 1:length(yawAngles)
    dataOut = [dataOut yawAngles(i) pitchAngles(i)];
end
end