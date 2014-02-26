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
            {ok, NewState};
        true -> 
            {'EXIT', {error, nick_taken, "Nick already taken on server"}}
    end;
    
    
loop(St, {disconnect, Pid, Nick}) ->
    NewClientList = lists:delete(Pid, St#server_st.clients),
    NewNicknameList = lists:delete(Nick, St#server_st.nicknames),
    NewState = St#server_st{clients=NewClientList, nicknames=NewNicknameList},
    {ok, NewState};

loop(St, {join, _Channel, Pid}) ->
	KeyFound = lists:member(_Channel, St#server_st.channels),
	if	
		KeyFound == false ->	
			NewChannelList = St#server_st.channels ++ [_Channel],
			NewState = St#server_st{channels=NewChannelList},
			start_channel_process(_Channel),
			genserver:request(list_to_atom(_Channel), {connect, Pid}),
			{ok, NewState};
		true ->			
			genserver:request(list_to_atom(_Channel), {connect, Pid}),
			{ok, St}	
	end.

start_channel_process(_Channel) ->
    genserver:start(list_to_atom(_Channel), channel:initial_state(_Channel), 
                    fun channel:loop/2).

initial_state(_Server) ->
    #server_st{clients=[], channels=[], nicknames=[]}.
    

