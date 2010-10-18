-module(pong).
-export([ircmain/1, ircproc/1]).

ircmain(Contact) ->
	Pid = spawn(?MODULE, ircproc, [Contact]),
	Contact ! {subscribe, Pid},
	Pid.

ircproc(Contact) ->
	receive
		quit -> quit;
		{incoming, <<"PING :", Data/binary>>} ->
			Contact ! {raw, "PONG :" ++ binary_to_list(Data)},
			ircproc(Contact);
		_ -> ircproc(Contact)
	end.
