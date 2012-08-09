-module(kv_storage_plugins_starter).

-behaviour(gen_server).

%% API
-export([
	start_link/0
]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-include("application.hrl").
-include("logging.hrl").
-include("gen_server_spec.hrl").

-record(state, {}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),

	%% !!! This is a synchronous operation and intended design decision !!!
	%% !!! `start_link' doesn't finish until all plugins are loaded !!!
	%% !!! This allows to initialize all plugins before all their clients !!!
	{ok, Plugins} = application:get_env(?APP, plugins),
	lists:foreach(
		fun(Plugin) ->
			?log_debug("starting ~p...", [Plugin]),
			%% this assumes the Plugin's name and its application name are the same.
			ok = ensure_app_started(Plugin),
			{ok, _} = kv_storage_plugins_sup:start_plugin(Plugin),
			?log_debug("~p started.", [Plugin])
		end,
		Plugins),
	gen_server:cast(self(), terminate),

	{ok, #state{}}.

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(terminate, State = #state{}) ->
	{stop, normal, State};

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State = #state{}) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec ensure_app_started(Application::atom()) -> ok | {error, term()}.
ensure_app_started(Application) ->
	case application:start(Application) of
		ok ->
			ok;
		{error, {not_started, Dependency}} ->
			case ensure_app_started(Dependency) of
				ok ->
					ensure_app_started(Application);
				Error ->
					Error
			end;
		{error, {already_started, Application}} ->
			ok;
		Error ->
			Error
	end.
