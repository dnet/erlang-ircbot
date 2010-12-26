-module(admin).
-export([ircmain/1, ircproc/1, lsmod/2]).
-include("nick.hrl").

ircmain(Contact) ->
	Pid = spawn(?MODULE, ircproc, [Contact]),
	Contact ! {subscribe, Pid},
	Pid.

ircproc(Contact) ->
	receive
		quit -> quit;
		{incoming, Data} ->
			case process(Data, Contact) of
				{ok, Answer} ->
					Contact ! {raw, Answer},
					ircproc(Contact);
				noreply ->
					ircproc(Contact);
				{quit, _QuitCommand} = Q ->
					Contact ! Q
			end;
		_ -> ircproc(Contact)
	end.

%% Incoming message processing

process(Data, Contact) ->
    Str = strip_crlf(binary_to_list(Data)),
    io:format("~p~n", [Str]),
    {Prefix, Command, Params} = parse_message(Str),
    process(Prefix, Command, Params, Contact).

process(Prefix, "PRIVMSG", Params, Contact) ->
    [To, Message] = Params,
    ReplyTo = privmsg_reply_to(Prefix, To),
    process_privmsg(Message, ReplyTo, Prefix, Contact);
process(_, _, _, _) ->
    noreply.

process_privmsg(_, noreply, _, _) ->
    noreply;
process_privmsg(Message, ReplyTo, Prefix, Contact) ->
    [H|T] = string:tokens(Message, " "),
    process_privmsg(H, T, ReplyTo, Prefix, Contact).

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
			case admincmp(Prefix, strip_crlf(Line)) of
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
process_privmsg("quit", _Remainder, _ReplyTo, Prefix, _Contact) ->
	case admin(Prefix) of
		true ->
			{quit, "QUIT :Goodbye from " ++ ?NICK};
		false ->
			noreply
	end;
process_privmsg("lsmod", _Remainder, ReplyTo, Prefix, Contact) ->
	case admin(Prefix) of
		true ->
			spawn(?MODULE, lsmod, [ReplyTo, Contact]), noreply;
		false ->
			noreply
	end;
process_privmsg(_, _, _, _, _) ->
    noreply.

lsmod(To, Contact) ->
	Contact ! {getmods, self()},
	receive
		{mods, L} -> Contact ! {raw, lists:foldl(
			fun(Pid, Msg) ->
				Msg ++ "\r\nPRIVMSG " ++ To ++ " :" ++ pid_to_list(Pid)
			end, "PRIVMSG " ++ To ++ " :Loaded modules:", L)}
	after 1500 ->
		Contact ! {raw, "PRIVMSG " ++ To ++ " :(timeout)"}
	end.

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
