-module(ircbot).
-author("Jim Menard, jimm@io.com").
-author("András Veres-Szentkirályi, vsza@vsza.hu").
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
	% active true means receiving data in messages
	{ok, Socket} = gen_tcp:connect(Host, Port,
		[binary, {active, true}]),
	send_init(Socket, Channel),
	Master = self(),
	ModPids = lists:map(
		fun({M, P}) -> apply(M, ircmain, [Master | P]) end, Modules),
	master({Channel, ModPids, Socket, []}).

master({Channel, ModPids, Socket, RawSubscribers} = State) ->
	receive
		% Loop cases
		{tcp_error, Socket, Reason} ->
			io:format("Socket error [~w]: ~s~n", [Socket, Reason]),
			master(State);
		{announce, Text} ->
			send(Socket, "PRIVMSG " ++ Channel ++ " :" ++ Text),
			master(State);
		{topic, Text} ->
			send(Socket, "TOPIC " ++ Channel ++ " :" ++ Text),
			master(State);
		{raw, Text} ->
			send(Socket, Text),
			master(State);
		{subscribe, Pid} ->
			master({Channel, ModPids, Socket, [Pid | RawSubscribers]});
		{tcp, Socket, Data} ->
			lists:foreach(fun(P) -> P ! {incoming, Data} end, RawSubscribers),
			master(State);
		% Quit cases
		{quit, QuitCommand} ->
			lists:foreach(fun(P) -> P ! quit end, ModPids),
            quit(Socket, QuitCommand),
            ok;
        {tcp_closed, Socket} ->
            io:format("Socket ~w closed [~w]~n", [Socket, self()]),
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
