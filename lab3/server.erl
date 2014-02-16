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
    {ok, NewState};


loop(St, {join, _Channel, Pid}) ->
	NewChannels = St#server_st.channels ++ [_Channel],
	NewState = St#server_st{channels=NewChannels},
	newChannel(_Channel),
	genserver:request(list_to_atom(_Channel), {connect, Pid}),
	{ok, NewState}.

newChannel(_Channel) ->
    %catch(unregister(list_to_atom(_Channel))),
    genserver:start(list_to_atom(_Channel), channel:initial_state(_Channel), 
                    fun channel:loop/2).

initial_state(_Server) ->
    #server_st{clients=[], channels=[]}.
    

