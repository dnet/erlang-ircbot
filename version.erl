-module(version).
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
		{incoming, <<":", Data/binary>>} ->
			case string:tokens(binary_to_list(Data), " ") of
				[From, "PRIVMSG", _, [58, 1, 86, 69, 82, 83, 73, 79, 78, 1 | _]] ->
					[Nick | _] = string:tokens(From, "!"),
					Contact ! {raw, lists:flatten(io_lib:format(
						"NOTICE ~s :\x01VERSION jimm-erlang-bot running " ++
						"on Erlang emulator ~s OTP release ~s\x01",
						[Nick, erlang:system_info(version),
							erlang:system_info(otp_release)]))};
				_ -> nop
			end,
			ircproc(Contact);
		{ident, Pid} ->
			Pid ! {ident, "version"},
			ircproc(Contact);
		{reload, Pid} ->
			?MODULE:reload(Contact, Pid);
		_ -> ircproc(Contact)
	end.
