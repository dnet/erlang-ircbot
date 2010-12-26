-module(ircbot).
-author("Jim Menard, jimm@io.com").
-author("András Veres-Szentkirályi, vsza@vsza.hu").
-include("ircbot.hrl").
-export([start/0, start/1, start/2, start/3, start/4]).
% -compile(export_all). % DEBUG

%% A simple IRC bot. No hot code swapping or anything like that---yet.
%%
%% TODO handle multiple receivers

-define(DEFAULT_SERVER, "irc.freenode.net").
-define(DEFAULT_PORT, 6667).
-include("nick.hrl").
-define(FULL_NAME, "Jim's Erlang Bot").

start() ->
    start("#jimm-test").

start(Channel) ->
    start(Channel, ?DEFAULT_SERVER, ?DEFAULT_PORT).

start(Channel, Host) ->
    start(Channel, Host, ?DEFAULT_PORT).

start(Channel, Host, Port) ->
	start(Channel, Host, Port, []).

start(Channel, Host, Port, Modules) ->
	Socket = connect(Channel, Host, Port),
	Master = self(),
	ModPids = lists:map(
		fun({M, P}) -> apply(M, ircmain, [Master | P]) end, Modules),
	master(#ms{channel = Channel, modpids = ModPids,
		socket = Socket, host = Host, port = Port}).

connect(Channel, Host, Port) ->
	% active true means receiving data in messages
	{ok, Socket} = gen_tcp:connect(Host, Port,
		[binary, {active, true}]),
	send_init(Socket, Channel),
	Socket.

reconnect_master(S) ->
	Socket = connect(S#ms.channel, S#ms.host, S#ms.port),
	master(S#ms{socket = Socket}).

master(State = #ms{socket = Socket}) ->
	receive
		% Loop cases
		{tcp_error, Socket, Reason} ->
			io:format("Socket error [~w]: ~s~n", [Socket, Reason]),
			master(State);
		{announce, Text} ->
			send(Socket, "PRIVMSG " ++ State#ms.channel ++ " :" ++ Text),
			master(State);
		{topic, Text} ->
			send(Socket, "TOPIC " ++ State#ms.channel ++ " :" ++ Text),
			master(State);
		{raw, Text} ->
			send(Socket, Text),
			master(State);
		{subscribe, Pid} ->
			master(State#ms{rawsubscribers = [Pid | State#ms.rawsubscribers]});
		{getmods, Pid} ->
			Pid ! {mods, State#ms.modpids},
			master(State);
		{tcp, Socket, Data} ->
			lists:foreach(fun(P) -> P ! {incoming, Data} end, State#ms.rawsubscribers),
			master(State);
		{tcp_closed, Socket} ->
			io:format("Socket ~w closed [~w]~n", [Socket, self()]),
			reconnect_master(State);
		{reconnect, QuitCommand} ->
			quit(Socket, QuitCommand),
			reconnect_master(State);
		% Quit cases
		{quit, QuitCommand} ->
			lists:foreach(fun(P) -> P ! quit end, State#ms.modpids),
            quit(Socket, QuitCommand),
            ok
	end.

send(Socket, Text) ->
    io:format("~p~n", [Text]),
    gen_tcp:send(Socket, list_to_binary(Text ++ "\r\n")).

send_init(Socket, Channel) ->
    send(Socket, "USER " ++ ?NICK ++ " dummy-host dummy-server :" ++ ?FULL_NAME),
    send(Socket, "NICK " ++ ?NICK ++ ""),
    send(Socket, "JOIN " ++ Channel).

quit(Socket, QuitCommand) ->
    send(Socket, QuitCommand),
    gen_tcp:close(Socket),
    io:format("Socket ~w closed [~w]~n", [Socket, self()]).
