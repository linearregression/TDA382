-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, {connect, Pid, Nick}) ->
    NickTaken = lists:member(Nick, St#server_st.nicknames),
    if
        NickTaken == false ->
            NewClients = St#server_st.clients ++ [Pid],    
            NewNicknames = St#server_st.nicknames ++ [Nick],
            NewState = St#server_st{clients=NewClients, nicknames=NewNicknames},
            io:fwrite("connected: ~w ", [NewClients]),
            {ok, NewState};
        true -> 
            {'EXIT', {error, nick_taken, "Nick already taken on server"}}
    end;
    
    
loop(St, {disconnect, Pid, Nick}) ->
    NewClients = lists:delete(Pid, St#server_st.clients),
    NewNicknames = lists:delete(Nick, St#server_st.nicknames),
    io:fwrite("disconnected: ~w ", [NewClients]),
    NewState = St#server_st{clients=NewClients, nicknames=NewNicknames},
    % TODO remove connection from chatrooms, if any left
    {ok, NewState};

loop(St, {join, _Channel, Pid}) ->
	KeyFound = lists:member(_Channel, St#server_st.channels),
	if	
		false == KeyFound ->	
			NewChannels = St#server_st.channels ++ [_Channel],
			NewState = St#server_st{channels=NewChannels},
			newChannel(_Channel),
			genserver:request(list_to_atom(_Channel), {connect, Pid}),
			{ok, NewState};
		true ->			
			genserver:request(list_to_atom(_Channel), {connect, Pid}),
			{ok, St}	
	end.

newChannel(_Channel) ->
    %catch(unregister(list_to_atom(_Channel))),
    genserver:start(list_to_atom(_Channel), channel:initial_state(_Channel), 
                    fun channel:loop/2).

initial_state(_Server) ->
    #server_st{clients=[], channels=[], nicknames=[]}.
    

