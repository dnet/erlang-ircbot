-module(admin).
-export([ircmain/1, ircproc/1, lsmod/2, rmmod/3, insmod/3, reload/3, reload/2]).
-include("nick.hrl").

ircmain(Contact) ->
	Pid = spawn(?MODULE, ircproc, [Contact]),
	Contact ! {subscribe, Pid},
	Pid.

reload(Contact, Pid) ->
	Pid ! reloaded,
	ircproc(Contact).

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
		{ident, Pid} ->
			Pid ! {ident, "admin"},
			ircproc(Contact);
		{reload, Pid} ->
			?MODULE:reload(Contact, Pid);
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
	case string:tokens(Message, " ") of
		[] -> noreply;
		[H|T] ->
			process_privmsg(H, T, ReplyTo, Prefix, Contact)
	end.

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
process_privmsg("-quit", _Remainder, _ReplyTo, Prefix, _Contact) ->
	case admin(Prefix) of
		true ->
			{quit, ["QUIT :Goodbye from ", ?NICK]};
		false ->
			noreply
	end;
process_privmsg("-lsmod", _Remainder, ReplyTo, Prefix, Contact) ->
	case admin(Prefix) of
		true ->
			spawn(?MODULE, lsmod, [ReplyTo, Contact]), noreply;
		false ->
			noreply
	end;
process_privmsg("-rmmod", Remainder, ReplyTo, Prefix, Contact) ->
	case admin(Prefix) of
		true ->
			spawn(?MODULE, rmmod, [ReplyTo, Remainder, Contact]), noreply;
		false ->
			noreply
	end;
process_privmsg("-insmod", Remainder, ReplyTo, Prefix, Contact) ->
	case admin(Prefix) of
		true ->
			spawn(?MODULE, insmod, [ReplyTo, Remainder, Contact]), noreply;
		false ->
			noreply
	end;
process_privmsg("-reload", Remainder, ReplyTo, Prefix, Contact) ->
	case admin(Prefix) of
		true ->
			spawn(?MODULE, reload, [ReplyTo, Remainder, Contact]), noreply;
		false ->
			noreply
	end;
process_privmsg("-load", [Module | _], ReplyTo, Prefix, _Contact) ->
	case admin(Prefix) of
		true ->
			Atom = list_to_atom(Module),
			{ok, ["PRIVMSG ", ReplyTo, " :",
				case code:load_file(Atom) of
					{module, Atom} -> "module loaded ok";
					{error, not_purged} ->
						case code:soft_purge(Atom) of
							true -> "module loaded ok (old version got purged)";
							false -> "old version is still in use, use rmmod"
						end;
					{error, What} -> ["could not load module: ", atom_to_list(What)]
				end]
			};
		false ->
			noreply
	end;
process_privmsg("-help", _Remainder, ReplyTo, Prefix, _Contact) ->
	case admin(Prefix) of
		true ->
			{ok, ["PRIVMSG ", ReplyTo,
				" :Available commands are: quit, lsmod, rmmod, insmod, load, reload, help"]};
		false ->
			noreply
	end;
process_privmsg(_, _, _, _, _) ->
    noreply.

lsmod(To, Contact) ->
	Contact ! {getmods, self()},
	receive
		{mods, L} -> {_, T} = lists:foldl(
			fun(Pid, {N, Msg}) ->
				Pid ! {ident, self()},
				Ident = receive {ident, I} -> I after 200 -> "(timeout)" end,
				{N + 1, io_lib:format(
						  "~s\r\nPRIVMSG ~s :~w. ~w ~s",
						  [Msg, To, N, Pid, Ident])}
			end, {1, ["PRIVMSG ", To, " :Loaded modules:"]}, L),
			Contact ! {raw, T}
	after 1500 ->
		Contact ! {raw, ["PRIVMSG ", To, " :(timeout)"]}
	end.

rmmod(To, [FirstParam | _], Contact) ->
	N = list_to_integer(FirstParam),
	Contact ! {getmods, self()},
	receive
		{mods, L} ->
			ModPid = lists:nth(N, L),
			Contact ! {killmod, ModPid, self()},
			Resp = receive {killmod, Msg} -> Msg after 500 -> "(timeout)" end,
			Contact ! {raw, ["PRIVMSG ", To, " :", Resp]}
	after 1500 ->
		Contact ! {raw, ["PRIVMSG ", To, " :(timeout)"]}
	end.

insmod(To, [ModName | Params], Contact) ->
	Contact ! {insmod, ModName, Params, self()},
	receive
		{insmod, Resp} ->
			Contact ! {raw, ["PRIVMSG ", To, " :", Resp]}
	after 1500 ->
		Contact ! {raw, ["PRIVMSG ", To, " :(timeout)"]}
	end.

reload(To, [FirstParam | _], Contact) ->
	N = list_to_integer(FirstParam),
	Contact ! {getmods, self()},
	receive
		{mods, L} ->
			lists:nth(N, L) ! {reload, self()},
			Resp = receive reloaded -> "module reloaded successfully" after 2500 -> "(timeout)" end,
			Contact ! {raw, ["PRIVMSG ", To, " :", Resp]}
	after 1500 ->
		Contact ! {raw, ["PRIVMSG ", To, " :(timeout)"]}
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
