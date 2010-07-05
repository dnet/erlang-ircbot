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
-define(NICK, "jimm-erlang-bot").
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
	% active false means explicit recv needed
	{ok, Socket} = gen_tcp:connect(Host, Port,
		[binary, {packet, 0}, {active, false}]),
	send_init(Socket, Channel),
	Master = self(),
	spawn(fun() -> receive_loop(Socket, Master) end),
	ModPids = lists:map(
		fun({M, P}) -> apply(M, ircmain, [Master | P]) end, Modules),
	master(Channel, ModPids, Socket).

master(Channel, ModPids, Socket) ->
	receive
		quit -> lists:foreach(fun(P) -> P ! quit end, ModPids)
	end.

send(Socket, Text) ->
    io:format("~p~n", [Text]),
    gen_tcp:send(Socket, list_to_binary(Text ++ "\r\n")).

send_init(Socket, Channel) ->
    send(Socket, "USER " ++ ?NICK ++ " dummy-host dummy-server :" ++ ?FULL_NAME),
    send(Socket, "NICK " ++ ?NICK ++ ""),
    send(Socket, "JOIN " ++ Channel).

receive_loop(Socket, Callback) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            case process(Data) of
                {ok, Answer} ->
                    send(Socket, Answer),
                    receive_loop(Socket, Callback);
                noreply ->
                    receive_loop(Socket, Callback);
                {quit, QuitCommand} ->
                    Callback ! quit,
                    quit(Socket, QuitCommand),
                    ok
            end;
        {error, closed} ->
            io:format("Socket ~w closed [~w]~n", [Socket, self()]),
            ok
    end.

%% Incoming message processing

process(<<"PING :", Data/binary>>) ->
    {ok, "PONG :" ++ strip_crlf(binary_to_list(Data))};
process(Data) ->
    Str = strip_crlf(binary_to_list(Data)),
    io:format("~p~n", [Str]),
    {Prefix, Command, Params} = parse_message(Str),
    process(Prefix, Command, Params).

process(Prefix, "PRIVMSG", Params) ->
    [To, Message] = Params,
    ReplyTo = privmsg_reply_to(Prefix, To),
    process_privmsg(Message, ReplyTo, Prefix);
process(_, _, _) ->
    noreply.

process_privmsg(_, noreply, _) ->
    noreply;
process_privmsg(Message, ReplyTo, Prefix) ->
    [H|T] = string:tokens(Message, " "),
    process_privmsg(H, T, ReplyTo, Prefix).

admincmp(Prefix, Line) ->
	{ok, MP} = re:compile(Line),
	case re:run(Prefix, MP) of
		{match, _} -> true;
		nomatch -> false
	end.

adminchk(Prefix, Dev) ->
	case io:get_line(Dev, "") of
		eof ->
			file:close(Dev), false;
		Line ->
			case admincmp(Prefix,
				lists:filter(fun (X) -> X =/= $\n andalso X =/= $\r end, Line)) of
				true ->
					file:close(Dev), true;
				false ->
					adminchk(Prefix, Dev)
			end
	end.

admin(Prefix) ->
	{ok, Device} = file:open("admins", [read]),
	adminchk(Prefix, Device).

%% Handle particular messages
process_privmsg("quit", _Remainder, _ReplyTo, Prefix) ->
	case admin(Prefix) of
		true ->
			{quit, "QUIT :Goodbye from " ++ ?NICK};
		false ->
			noreply
	end;
process_privmsg(_, _, _, _) ->
    noreply.

quit(Socket, QuitCommand) ->
    send(Socket, QuitCommand),
    gen_tcp:close(Socket),
    io:format("Socket ~w closed [~w]~n", [Socket, self()]).

strip_crlf(Str) ->
    string:strip(string:strip(Str, right, $\n), right, $\r).

% Return {Prefix, Command, Params} or {undefined, Command, Params}.
parse_message([$:|Msg]) ->
    Tokens = string:tokens(Msg, " "),
    [Prefix|T] = Tokens,
    [Command|T2] = T,
    {Prefix, Command, params(T2)};
parse_message(Msg) ->
    Tokens = string:tokens(Msg, " "),
    [Command|T] = Tokens,
    {undefined, Command, params(T)}.

% Turn list of params into new list where ["foo" ":rest" "of" "line"] becomes
% ["foo" "rest of line"].
params(L) ->
    params(L, []).

params([], P) ->
    lists:reverse(P);
params([[$:|HT]|T], P) ->
    params([], [string:join([HT|T], " ")|P]);
params([H|T], P) ->
    params(T, [H|P]).

% Return the nick or channel to which a reply should be sent.
privmsg_reply_to(Prefix, ?NICK) ->
    sender_of(Prefix);
privmsg_reply_to(_Prefix, [$#|_]=OrigTo) ->
    OrigTo;
privmsg_reply_to(_, _) ->
    noreply.

% Extract and return sender from a message prefix.
sender_of(Prefix) ->
    [Nick|_] = string:tokens(Prefix, "!"),
    Nick.
