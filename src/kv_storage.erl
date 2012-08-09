-module(kv_storage).

%% API
-export([
	open/1,
	close/1,
	read/1,
	read/2,
	write/3,
	delete/2
]).

%% Behaviour's required callbacks.
-export([behaviour_info/1]).

-spec behaviour_info(callbacks | any()) -> [{atom(), arity()}] | undefined.
behaviour_info(callbacks) ->
    [{start_link, 0}, {open, 2}, {close, 1}, {read, 1}, {read, 2}, {write, 3}, {delete, 2}];
behaviour_info(_Other) ->
    undefined.

-include("application.hrl").
-include("logging.hrl").

-type handle() :: {Plugin::atom(), Collection::term()}.

%% ===================================================================
%% API
%% ===================================================================

-spec open(CollectionName::term()) -> {ok, Handle::handle()} | {error, Reason::term()}.
open(CollectionName) ->
	{ok, Application} = application:get_application(),
	{ok, {Plugin, Options}} = storage_info(Application),
	case Plugin:open(CollectionName, Options) of
		{ok, Collection} ->
			{ok, {Plugin, Collection}};
		Error ->
			Error
		 end.

-spec close(Handle::handle()) -> ok | {error, Reason::term()}.
close({Plugin, Collection}) ->
	Plugin:close(Collection).

-spec read(Handle::handle()) -> {ok, [{Key::term(), Value::term()}]} | {error, Reason::term()}.
read({Plugin, Collection}) ->
	Plugin:read(Collection).

-spec read(Handle::handle(), Key::term()) -> {ok, Value::term()} | {error, no_entry} | {error, Reason::term()}.
read({Plugin, Collection}, Key) ->
	Plugin:read(Collection, Key).

-spec write(Handle::handle(), Key::term(), Value::term()) -> ok | {error, Reason::term()}.
write({Plugin, Collection},  Key, Value) ->
	Plugin:write(Collection, Key, Value).

-spec delete(Handle::handle(), Key::term()) -> ok | {error, no_entry} | {error, Reason::term()}.
delete({Plugin, Collection}, Key) ->
	Plugin:delete(Collection, Key).

%% ===================================================================
%% Internal
%% ===================================================================

-spec storage_info(atom()) -> {ok, {atom(), [tuple()]}} | {error, no_entry}.
storage_info(Application) ->
	case application:get_env(Application, kv_storage) of
		{ok, Args} ->
			case proplists:get_value(plugin, Args) of
				undefined ->
					{error, no_entry};
				Plugin ->
					case proplists:get_value(options, Args) of
						undefined ->
							{error, no_entry};
						Options ->
							{ok, {Plugin, Options}}
					end
			end;
		undefined ->
			{error, no_entry}
	end.
