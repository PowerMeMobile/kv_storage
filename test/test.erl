-module(test).
-export([benchmark/1, benchmark/2, for/3]).
-export([benchmark_storages/2]).

benchmark(Fun) ->
    statistics(runtime),
    statistics(wall_clock),
	Fun(),
    {_, Runtime} = statistics(runtime),
    {_, Wallclock} = statistics(wall_clock),
	RuntimeSec = Runtime / 1000.0,
    WallclockSec = Wallclock / 1000.0,
    io:format("Erlang Elapsed ~p (runtime) ~p (wall clock) seconds~n", [RuntimeSec, WallclockSec]).

for(I, Max, _) when I > Max ->
    ok;
for(I, Max, F) ->
    F(),
    for(I+1, Max, F).

benchmark(File, Fun) ->
    statistics(wall_clock),
	Fun(),
    {_, Wallclock} = statistics(wall_clock),
    WallclockSec = Wallclock / 1000.0,
    io:format(File, "~p", [WallclockSec]).

benchmark_storages(TableName, RecordsCount) ->
	Plugins = [
		{"Kyte", kyte_kv_storage_plugin, [
			{name_suffix, ".kch"},
			{parts, 2},
			{key_codec, etf},
			{val_codec, etf}
		]},
		{"MongoDB", mongodb_kv_storage_plugin, []},
		{"Riak", riak_kv_storage_plugin, []},
		{"PostgreSQL", postgresql_kv_storage_plugin, []},
		{"MySQL(MyISAM)", mysql_kv_storage_plugin, [
			{engine, "MyISAM"}
		]},
		{"MySQL(InnoDB)", mysql_kv_storage_plugin, [
			{engine, "InnoDB"}
		]},
		{"Cassandra", cassandra_kv_storage_plugin, []}
	],
	{ok, F} = file:open(io_lib:format("~s_~p.dat", [TableName, RecordsCount]), [write]),
	lists:foreach(
		fun({{Name, Plugin, Opts}, Index}) ->
			io:format(F, "~s\t", [Name]),
			{ok, T} = Plugin:open(lists:flatten(io_lib:format("~s_~p", [TableName, Index])), Opts),
			test:benchmark(F,
				fun() ->
					lists:foreach(
						fun(N) ->
							Plugin:write(T,
							{"1c391c64-a038-11e1-b812-00269e42f7a5", N},
							"1c391c64-a038-11e1-b812-00269e42f7a5")
						end,
						lists:seq(1, RecordsCount))
				end),
			io:format(F, "\t", []),
			test:benchmark(F,
				fun() ->
					lists:foreach(
						fun(N) ->
							Plugin:write(T,
							{"1c391c64-a038-11e1-b812-00269e42f7a5", N},
							"1c391c64-a038-11e1-b812-00269e42f7a5")
						end,
						lists:seq(1, RecordsCount))
				end),
			io:format(F, "\t", []),
			test:benchmark(F,
				fun() ->
					lists:foreach(
						fun(N) ->
							Plugin:read(T,
							{"1c391c64-a038-11e1-b812-00269e42f7a5", N})
						end,
						lists:seq(1, RecordsCount))
				end),
			io:format(F, "\t", []),
			test:benchmark(F,
				fun() ->
					lists:foreach(
						fun(N) ->
							Plugin:delete(T,
							{"1c391c64-a038-11e1-b812-00269e42f7a5", N})
						end,
						lists:seq(1, RecordsCount))
				end),
		   	Plugin:close(T),
			io:format(F, "~n", [])
		end,
		lists:zip(Plugins, lists:seq(1, length(Plugins)))),
	file:close(F).
