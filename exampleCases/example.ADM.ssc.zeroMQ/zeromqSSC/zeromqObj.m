classdef zeromqObj < handle
    
    properties (Access = public)
        port
        status
        timeoutBuffer
        verbose
        lastMessageSent_string
        lastMessageSent_data
        lastMessageRecvd_string
        lastMessageRecvd_data
    end
    
    properties (Access = private)
        context
        socket
    end
    methods
        function obj = zeromqObj(jeromqPath,port,timeoutBuffer,verbose)
            % Import path and set-up zeroMQ commands
            javaaddpath(jeromqPath,'-dpath');
            
            % Setup connection details
            obj.port = port;
            obj.timeoutBuffer = timeoutBuffer;
            obj.verbose = verbose;
            
            % Initialize empty strings and vectors
            obj.lastMessageSent_string  = '';
            obj.lastMessageSent_data    = [];
            obj.lastMessageRecvd_string = '';
            obj.lastMessageRecvd_data   = [];
            obj.status = 'initialized';            
            
            % Setup connection
            obj.connect();
        end
        
        function [] = connect(obj)           
            import org.zeromq.*
            
            % Setup connection
            if obj.verbose
                disp([datestr(rem(now,1)) '__ Connecting to tcp://*:' num2str(obj.port) '.']);
            end
            
            obj.context = zmq.Ctx();
            obj.socket = obj.context.createSocket(ZMQ.REP);
            obj.socket.bind(['tcp://*:' num2str(obj.port)]);
            obj.status = 'connected';
        end

        function [] = disconnect(obj)    
            if obj.verbose
                disp([datestr(rem(now,1)) '__ Disconnecting from tcp://*:' num2str(obj.port) '.']);
            end            
            obj.socket.close();
            obj.status = 'disconnected';
        end
        
        
        function [dataReceived] = receive(obj)           
            message = '';
            timeoutTimer = tic;
            while length(message) <= 0
                message = obj.socket.recv(1); % Receive information
                if toc(timeoutTimer) > obj.timeoutBuffer % Time-out
                    obj.socket.close()
                    error([datestr(rem(now,1)) '__No message received within timeoutBuffer: ' ...
                            num2str(obj.timeoutBuffer) ' s. Exiting...']);
                end
                pause(0.010); % wait 10 ms
            end
            
            % Once message received: convert to floats
            json_data = native2unicode(message.data)'; % Received message
            
            % cut down json_data to remove all non-used bytes
            sortedFloats = textscan( json_data, '%f', 'Delimiter',' ' );
            dataReceived  = sortedFloats{1}'; % row vector with data
                        
            obj.lastMessageRecvd_string = json_data;
            obj.lastMessageRecvd_data   = dataReceived;
            
            if obj.verbose
                tmp_recvData_string = strjoin(arrayfun(@(x) num2str(x),dataReceived,'UniformOutput',false),' ');
                disp([datestr(rem(now,1)) '__ Received data: [' num2str(tmp_recvData_string) ']']);
            end
        end     
        
        function [] = send(obj,data)  
            if (size(data,1)>1 && size(data,2)>1)
                error('Currently, we only support sending data in vector format.');
            end
            
            % Create string from data vector
            str_send = num2str(data(1));
            for i = 2:length(data)
                str_send = [str_send ' ' num2str(data(i))];
            end
            
            % Send a message
            message = zmq.Msg(length(str_send));
            message.put(unicode2native(str_send));
            obj.socket.send(message, 0);
            
            obj.lastMessageSent_string = str_send;
            obj.lastMessageSent_data   = data;
            
            if obj.verbose
                disp([datestr(rem(now,1)) '__ Sent message: [' str_send ']']);
            end
        end            
        
    end
    
    methods (Hidden)
        %
    end
end

