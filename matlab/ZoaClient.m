classdef ZoaClient < handle
% ZoaClient  Connect to a Zoa optical-design server over ZeroMQ (JeroMQ).
%
% Starts buildHB/zoa_server on a free TCP port, then opens a JeroMQ REQ
% socket to it.  All communication is synchronous (request/reply).
%
% Requirements:
%   - Drop a jeromq-<ver>.jar into the same directory as this file, OR
%     call javaaddpath('/path/to/jeromq-<ver>.jar') before constructing.
%   - The zoa_server binary must exist at ../buildHB/zoa_server relative
%     to this file's location.
%
% Usage:
%   zoa = ZoaClient();
%   zoa.cmd("RES osdtriplet");
%   r   = zoa.analyze("SEI");
%   r.tables.seidel.data        % 9-row × 7-col double array
%   zoa.close();

    properties (Access = private)
        sock     % JeroMQ ZMQ_REQ socket
        ctx      % JeroMQ ZContext
        proc     % java.lang.Process (the server subprocess)
        port     % integer TCP port
    end

    methods

        function obj = ZoaClient()
            % Constructor: locate jar, find free port, start server, connect.

            % --- locate and add JeroMQ jar if not already on path ----------
            classDir = fileparts(mfilename('fullpath'));
            jars = dir(fullfile(classDir, 'jeromq-*.jar'));
            if ~isempty(jars)
                jarPath = fullfile(classDir, jars(1).name);
                if ~any(strcmp(javaclasspath('-dynamic'), jarPath))
                    javaaddpath(jarPath);
                end
            end

            % --- find a free TCP port -------------------------------------
            import java.net.ServerSocket
            ss = ServerSocket(0);
            obj.port = ss.getLocalPort();
            ss.close();

            % --- locate the server executable -----------------------------
            serverExe = fullfile(classDir, '..', 'buildHB', 'zoa_server');
            serverExe = char(java.io.File(serverExe).getCanonicalPath());
            if ~isfile(serverExe)
                error('ZoaClient:notFound', ...
                    'zoa_server not found at: %s', serverExe);
            end

            endpoint = sprintf('tcp://*:%d', obj.port);

            % --- start the server process ---------------------------------
            pb = java.lang.ProcessBuilder({serverExe, endpoint});
            pb.redirectErrorStream(true);
            obj.proc = pb.start();

            % --- create JeroMQ context and REQ socket ---------------------
            obj.ctx  = org.zeromq.ZContext();
            obj.sock = obj.ctx.createSocket(org.zeromq.SocketType.REQ);
            obj.sock.setReceiveTimeOut(30000);   % 30-second recv timeout
            obj.sock.connect(sprintf('tcp://localhost:%d', obj.port));

            % --- wait for server to be ready (PING/PONG handshake) --------
            pause(0.5);
            for attempt = 1:20
                try
                    obj.sock.send(uint8('__PING__'));
                    data = obj.sock.recv();
                    if ~isempty(data) && strcmp(char(data), 'PONG')
                        return;
                    end
                catch
                    % ignore and retry
                end
                pause(0.25);
            end
            error('ZoaClient:timeout', 'Server did not respond to PING after 5 s');
        end

        function reply = cmd(obj, command)
            % Send a plain text command; return the text reply.
            obj.sock.send(uint8(char(command)));
            data = obj.sock.recv();
            if isempty(data)
                reply = '';
            else
                reply = char(data);
            end
        end

        function r = analyze(obj, command)
            % Send __JSON__ <command>; decode the JSON result via zoaDecode.
            jsonCmd = ['__JSON__ ' char(command)];
            obj.sock.send(uint8(jsonCmd));
            data = obj.sock.recv();
            if isempty(data)
                error('ZoaClient:noReply', 'No reply received for: %s', command);
            end
            jsonStr = char(data);
            r = zoaDecode(jsonStr);
        end

        function close(obj)
            % Send __QUIT__, close socket and context, destroy server process.
            try
                obj.sock.send(uint8('__QUIT__'));
                obj.sock.recv();   % wait for BYE
            catch
                % ignore send/recv errors during shutdown
            end
            try
                obj.ctx.destroy();
            catch
            end
            try
                obj.proc.destroy();
            catch
            end
        end

    end % methods

end % classdef
