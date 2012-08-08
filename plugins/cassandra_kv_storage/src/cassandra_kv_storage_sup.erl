-module(cassandra_kv_storage_sup).

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
			{cassandra_kv_storage,
				{cassandra_kv_storage, start_link, []}, permanent, 1000000, worker, [cassandra_kv_storage]}
		]}
	}.

%% ===================================================================
%% Internal
%% ===================================================================
