-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, {connect, Pid}) ->
    NewClients = St#server_st.clients ++ [Pid],
    io:fwrite("connected: ~w ", [NewClients]),
    NewState = St#server_st{clients=NewClients},
    {ok, NewState};
    
loop(St, {disconnect, Pid}) ->
    NewClients = lists:delete(Pid, St#server_st.clients),
    io:fwrite("disconnected: ~w ", [NewClients]),
    NewState = St#server_st{clients=NewClients},
    % TODO remove connection from chatrooms, if any left
    {ok, NewState}.


initial_state(_Server) ->
    #server_st{clients=[], chatrooms=[]}.
    

