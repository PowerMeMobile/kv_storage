-module(mysql_kv_storage_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

-include("application.hrl").
-include_lib("kv_storage/include/application_spec.hrl").
-include_lib("kv_storage/include/logging.hrl").

%% ===================================================================
%% application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	register(?MODULE, self()),
	mysql_kv_storage_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal
%% ===================================================================
