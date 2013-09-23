-module(pong).
-export([ircmain/1, ircproc/1, reload/2]).

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
		{incoming, <<"PING :", Data/binary>>} ->
			Contact ! {raw, ["PONG :", Data]},
			ircproc(Contact);
		{ident, Pid} ->
			Pid ! {ident, "pong"},
			ircproc(Contact);
		{reload, Pid} ->
			?MODULE:reload(Contact, Pid);
		_ -> ircproc(Contact)
	end.
