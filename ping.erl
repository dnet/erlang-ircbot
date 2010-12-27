-module(ping).
-export([ircmain/1, ircproc/1]).

ircmain(Contact) ->
	Pid = spawn(?MODULE, ircproc, [Contact]),
	Contact ! {subscribe, Pid},
	Pid.

ircproc(Contact) ->
	receive
		quit -> quit;
		{incoming, Data} ->
			S = binary_to_list(Data),
			[ServerName | _] = string:tokens(S, " "),
			inner_ircproc(Contact, ServerName);
		{ident, Pid} ->
			Pid ! {ident, "ping"},
			ircproc(Contact);
		_ -> ircproc(Contact)
	end.

inner_ircproc(Contact, ServerName) ->
	inner_ircproc(Contact, ServerName, false).

inner_ircproc(Contact, ServerName, Reconnect) ->
	receive
		quit -> quit;
		{ident, Pid} ->
			Pid ! {ident, "ping"},
			inner_ircproc(Contact, ServerName, Reconnect);
		_ -> inner_ircproc(Contact, ServerName)
		after 120000 ->
			case Reconnect of
				true ->
					Contact ! {reconnect, "QUIT :Reconnecting"},
					inner_ircproc(Contact, ServerName);
				_ ->
					Contact ! {raw, "PING " ++ ServerName},
					inner_ircproc(Contact, ServerName, true)
			end
	end.
