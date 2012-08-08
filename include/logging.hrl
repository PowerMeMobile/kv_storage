-ifndef(logging_hrl).
-define(logging_hrl, included).

-compile([{parse_transform, lager_transform}]).

-define(log_common(Lvl, Fmt, Args),
		lager:Lvl(
			case erlang:get(p_name) of
				undefined ->
					"";
				_ ->
					erlang:get(p_name) ++ ": "
				end ++
			Fmt ++  " [~s:~p]", Args ++ [?FILE, ?LINE]
		)).

-define(log_debug(Fmt, Args), ?log_common(debug, Fmt, Args)).
-define(log_info(Fmt, Args), ?log_common(info, Fmt, Args)).
-define(log_notice(Fmt, Args), ?log_common(notice, Fmt, Args)).
-define(log_warn(Fmt, Args), ?log_common(warning, Fmt, Args)).
-define(log_error(Fmt, Args), ?log_common(error, Fmt, Args)).
-define(log_crit(Fmt, Args), ?log_common(critical, Fmt, Args)).
-define(log_alert(Fmt, Args), ?log_common(alert, Fmt, Args)).
-define(log_fatal(Fmt, Args), ?log_common(emergency, Fmt, Args)).

-define(set_pname(PName), erlang:put(p_name, PName)).
-define(set_pname_fmt(Fmt, Params), erlang:put(p_name, lists:flatten(io_lib:format(Fmt, [Params])))).
-define(set_pname(Fmt, OID), erlang:put(p_name, lists:flatten(io_lib:format(Fmt, [uuid:to_string(OID)])))).
-define(set_pname(Fmt, OID, Params), erlang:put(p_name, lists:flatten(io_lib:format(Fmt, [uuid:to_string(OID) | Params])))).

-endif.
