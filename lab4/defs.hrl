% This record defines the structure of the 
% client process. 
% 
% It contains the following fields: 
%
% nick: it stores the nickname of the client
% gui: it stores the name (or Pid) of the GUI process.
% connected_server: the name (or PID) of the server the 
% client is connected to
% connected_channels: the name (or PID) of the channels
% the client is connected to
%
-record(cl_st, {nick, gui, connected_server, connected_channels}).
    
% This record defines the structure of the 
% server process. 
% 
% It contains the following fields: 
%
% clients: the name (or Pid) of the client processes connected
% to the server
% channels: the name (or PID) of the channels the server is 
% connected to
% nicknames: the nicknames of all the clients connected to the
% server
%
-record(server_st, {clients, channels, nicknames}).

% This record defines the structure of the 
% channel process. 
% 
% It contains the following fields: 
%
% clients: the name (or Pid) of the client processes connected
% to the channel
% name: the name of the channel
%
-record(channel_st, {clients, name}).

