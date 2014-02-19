-module(client).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Server}) ->
    R = request(list_to_atom(_Server), {connect, self()}),
    NewState = St#cl_st{connected_server=_Server},
    {ok, NewState} ;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%


loop(St, disconnect) ->
    % TODO Only allow the user to disconnect if he has left all chat rooms
    % if that is not the case, what should we return? exit? error?
    if
        St#cl_st.connected_server /= "-1" -> % tries to disconnect from a server that he is not connected to
            {{error, user_not_connected, "Dummy text"}, St};
      %  len(St#cl_st.connected_channels) /= 0 -> % has not left all chatrooms
       %     {{error, leave_channels_first, "Dummy text 2"}, St};
        true -> 
            case catch(request(list_to_atom(St#cl_st.connected_server), {disconnect, self()})) of
                {'EXIT', Reason} -> % if the server process cannot be reached
                    {{error, server_not_reached, "Dummy text 3"}, St};
                Result -> 
                    NewState = St#cl_st{connected_server=-1}, % TODO what to put here?
                    {ok, NewState}
            end
    end;
    

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join,_Channel}) ->
	R = request(list_to_atom(St#cl_st.connected_server), {join, _Channel, self()}),
	NewChannels = St#cl_st.connected_channels ++ [_Channel],
    NewState = St#cl_st{connected_channels=NewChannels},
    {ok, NewState} ;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel}) ->
	KeyFound = lists:keyfind(_Channel, 1, St#cl_st.connected_channels), 	
	if
		false == KeyFound ->
			{{error, user_not_joined, "Dummy text"}, St};
        true -> 
            %leave channel
			NewChannels = lists:delete(_Channel, St#cl_st.connected_channels),
			NewState = St#cl_st{connected_channels = NewChannels},
			{ok, NewState}
    end;

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
    IsConnectedToChannel = lists:keyfind(_Channel, 1, St#cl_st.connected_channels),
    if
		IsConnectedToChannel == false ->
			{{error, user_not_joined, "Tried to write to channel not part of."}, St};
        true -> 
            % write message
            list_to_atom(_Channel) ! {message, _Msg},
			{ok, St}
    end;

%%%%%%%%%%%%%%
%%% WhoIam
%%%%%%%%%%%%%%
loop(St, whoiam) ->
    {St#cl_st.nick, St} ;

%%%%%%%%%%
%%% Nick
%%%%%%%%%%
loop(St,{nick,_Nick}) ->
    %unregister(list_to_atom(St#cl_st.nick)), % unregister last nick
    NewState = St#cl_st{nick = _Nick}, % save nick to new state
    %register(list_to_atom(_Nick), self()),  % register the new nick
    {ok, NewState} ; 

%%%%%%%%%%%%%
%%% Debug
%%%%%%%%%%%%%
loop(St, debug) ->
    {St, St} ;

%%%%%%%%%%%%%%%%%%%%%
%%%% Incoming message
%%%%%%%%%%%%%%%%%%%%%
loop(St = #cl_st { gui = GUIName }, _MsgFromClient) ->
    {Channel, Name, Msg} = decompose_msg(_MsgFromClient),
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.


% This function will take a message from the client and
% decomposed in the parts needed to tell the GUI to display
% it in the right chat room.
decompose_msg(_MsgFromClient) ->
    {"", "", ""}.


request(_Server, Msg) ->
    genserver:request(_Server, Msg).

initial_state(Nick, GUIName) ->
    #cl_st { nick = Nick, gui = GUIName, connected_channels = [] }.
