-module(kv_storage_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

-include("application.hrl").
-include("logging.hrl").
-include("application_spec.hrl").

%% ===================================================================
%% application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	register(?MODULE, self()),
	kv_storage_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal
%% ===================================================================
