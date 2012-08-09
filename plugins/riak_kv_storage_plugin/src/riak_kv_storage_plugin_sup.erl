-module(riak_kv_storage_plugin_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0
]).

%% supervisor callbacks
-export([init/1]).

-include("application.hrl").
-include_lib("kv_storage/include/logging.hrl").
-include_lib("kv_storage/include/supervisor_spec.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
	{ok, {
		{one_for_one, 5, 10}, [
			{riak_kv_storage_plugin,
				{riak_kv_storage_plugin, start_link, []},
				permanent, 1000000, worker, [riak_kv_storage_plugin]}
		]}
	}.

%% ===================================================================
%% Internal
%% ===================================================================
