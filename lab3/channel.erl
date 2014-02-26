-module(channel).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, Pid}) ->
    NewClients = St#channel_st.clients ++ [Pid],
    %io:fwrite("connected to channel: ~w ", [NewClients]),
    NewState = St#channel_st{clients=NewClients},
    {ok, NewState};

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%    
loop(St, {disconnect, Pid}) ->
    NewClients = lists:delete(Pid, St#channel_st.clients),
    %io:fwrite("disconnected to channel: ~w ", [NewClients]),
    NewState = St#channel_st{clients=NewClients},
    {ok, NewState};

%%%%%%%%%%%%%%%
%%%% Send message
%%%%%%%%%%%%%%%
loop(St, {msg_from_client, Pid, Name, _Msg}) ->
	spawn(fun() -> new_process(St, Pid, Name, _Msg) end),
	{ok, St}.

new_process(St, Pid, Name, _Msg) ->
	SendToList = lists:delete(Pid, St#channel_st.clients),
		send_msg_to_client(SendToList, St#channel_st.name, Name, _Msg).

send_msg_to_client([SendTo|ClientList], Channel, Name, _Msg) ->
	spawn(fun() -> genserver:request(SendTo, {Channel, Name, _Msg})end),
	send_msg_to_client(ClientList, Channel, Name, _Msg);

send_msg_to_client([], Channel, Name, _Msg) ->
	donothing.

initial_state(_Channel) ->
    #channel_st{clients=[], name=_Channel}.
    

