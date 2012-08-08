-module(kv_storage_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0
]).

%% supervisor callbacks
-export([init/1]).

-include("application.hrl").
-include("logging.hrl").
-include("supervisor_spec.hrl").

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
		{rest_for_one, 5, 10}, [
			{engines_sup,
				{kv_storage_engines_sup, start_link, []},
				permanent, infinity, supervisor, [kv_storage_engines_sup]},
			{engines_starter,
				{kv_storage_engines_starter, start_link, []},
				transient, 5000, worker, [kv_storage_engines_starter]}
		]}
	}.

%% ===================================================================
%% Internal
%% ===================================================================
