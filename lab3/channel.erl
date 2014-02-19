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
    % TODO remove connection from chatrooms, if any left
    {ok, NewState};

loop(St, {msg_from_client, Pid, _Msg}) ->
	lists:foreach(fun(Client) ->
		send_msg_to_client(Client, _Msg) end,
		lists:delete(Pid, St#channel_st.clients));
	{ok, St}.

send_msg_to_client(Pid, _Msg) ->
	io:fwrite("Message sent: ~w ", [_Msg]),
	request(Pid, {_Msg}).

initial_state(_Channel) ->
    #channel_st{clients=[]}.
    

