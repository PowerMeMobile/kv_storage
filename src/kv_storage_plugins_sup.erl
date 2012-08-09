-module(kv_storage_plugins_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0,
	start_plugin/1
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

-spec start_plugin(Plugin::atom()) -> {ok, pid()}.
start_plugin(Plugin) ->
	%% this assumes the Plugin's supervisor's name suffix is `_sup'.
	PluginSup = list_to_atom(atom_to_list(Plugin) ++ "_sup"),
	ChildSpec =
		{Plugin, {PluginSup, start_link, []}, permanent, infinity, supervisor, [PluginSup]},
	supervisor:start_child(?MODULE, ChildSpec).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
	{ok, {{one_for_one, 5, 10}, []}}.

%% ===================================================================
%% Internal
%% ===================================================================
