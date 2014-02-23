-module(channel).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, {connect, Pid}) ->
    NewClients = St#channel_st.clients ++ [Pid],
    io:fwrite("connected to channel: ~w ", [NewClients]),
    NewState = St#channel_st{clients=NewClients},
    {ok, NewState};
    
loop(St, {disconnect, Pid}) ->
    NewClients = lists:delete(Pid, St#channel_st.clients),
    io:fwrite("disconnected to channel: ~w ", [NewClients]),
    NewState = St#channel_st{clients=NewClients},
    {ok, NewState};

loop(St, {msg_from_client, Pid, Name, _Msg}) ->
	lists:foreach(fun(Client) ->
		spawn(fun() -> send_msg_to_client(St, Client, Name, _Msg) end) end,
		lists:delete(Pid, St#channel_st.clients)),
	{ok, St}.

send_msg_to_client(St, Pid, Name, _Msg) ->
	io:fwrite("Message sent: ~w ", [_Msg]),
	genserver:request(Pid, {St#channel_st.name, Name, _Msg}).

initial_state(_Channel) ->
    #channel_st{clients=[], name=_Channel}.
    

