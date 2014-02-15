% This record defines the structure of the 
% client process. 
% 
% It contains the following fields: 
%
% gui: it stores the name (or Pid) of the GUI process.
% connected_server: the name (or PID) of the server the 
% client is connected to
%
-record(cl_st, {nick, gui, connected_server, connected_chatrooms}).
    
% This record defines the structure of the 
% server process. 
% 
-record(server_st, {clients, chatrooms}).

% TODO Use shorter names for the different stuff in here