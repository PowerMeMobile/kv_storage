-module(kv_storage_common).

-behaviour(gen_server).

%% API
-export([
	start_link/1,
	%% non-versioned
	read/1,
	read/2,
	write/3,
	delete/2,
	%% versioned
	read_version/2,
	read_version/3,
	write_version/4,
	delete_version/3
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

-include("logging.hrl").
-include("gen_server_spec.hrl").

-record(state, {
	db :: kv_storage:handle()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link(CollectionName::atom()) -> {ok, pid()}.
start_link(CollectionName) ->
	gen_server:start_link({local, CollectionName}, ?MODULE, [CollectionName], []).

%% Non-versioned

-spec read(CollectionName::atom()) -> {ok, [{Key::term(), Value::term()}]} | {error, Reason::term()}.
read(CollectionName) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	kv_storage:read(Db).

-spec read(CollectionName::atom(), Key::term()) -> {ok, Value::term()} | {error, Reason::term()}.
read(CollectionName, Key) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	kv_storage:read(Db, Key).

-spec write(CollectionName::atom(), Key::term(), Value::term()) -> ok | {error, Reason::term()}.
write(CollectionName, Key, Value) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	kv_storage:write(Db, Key, Value).

-spec delete(CollectionName::atom(), Key::term()) -> ok | {error, Reason::term()}.
delete(CollectionName, Key) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	kv_storage:delete(Db, Key).

%% Versioned

-spec read_version(CollectionName::atom(), Version::integer()) -> {ok, [{Key::term(), Value::term()}]} | {error, Reason::term()}.
read_version(CollectionName, Version) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	case kv_storage:read(Db) of
		{ok, Entries} ->
			{ok, lists:map(
				fun({Key, VersionedValue}) ->
					case VersionedValue of
						{Version, Value} ->
							{Key, Value};
						{OldVersion, OldValue} ->
							{Key, update(OldVersion, OldValue, Version)}
					end
				end,
				Entries)};
		Error ->
			Error
	 end.

-spec read_version(CollectionName::atom(), Version::integer(), Key::term()) -> {ok, Value::term()} | {error, Reason::term()}.
read_version(CollectionName, Version, Key) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	case kv_storage:read(Db, Key) of
		{ok, {Version, Value}} ->
			{ok, Value};
		{ok, {OldVersion, OldValue}} when is_integer(OldVersion) ->
			update(OldVersion, OldValue, Version);
		Error ->
			Error
	 end.

-spec write_version(CollectionName::atom(), Version::integer(), Key::term(), Value::term()) -> ok | {error, Reason::term()}.
write_version(CollectionName, Version, Key, Value) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	VValue = {Version, Value},
	kv_storage:write(Db, Key, VValue).

-spec delete_version(CollectionName::atom(), Version::integer(), Key::term()) -> ok | {error, Reason::term()}.
delete_version(CollectionName, _Version, Key) ->
	{ok, Db} = gen_server:call(CollectionName, get_db, infinity),
	kv_storage:delete(Db, Key).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([CollectionName]) ->
	{ok, Application} = application:get_application(),
	{ok, {StorageName, PluginInfo}} = get_storage_info(Application, CollectionName),
	{ok, Db} = kv_storage:open(StorageName, PluginInfo),

	{ok, #state{
		db = Db
	}}.

handle_call(get_db, _From, State = #state{db = Db}) ->
	{reply, {ok, Db}, State};

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State = #state{
	db = Db
}) ->
	kv_storage:close(Db).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec update(OldVersion::integer(), OldValue::term(), NewVersion::integer()) -> {ok, NewValue::term()} | {error, Reason::term()}.
update(_OldVersion, OldValue, _NewVersion) ->
	{ok, OldValue}.

-spec get_storage_info(Application::atom(), CollectionName::atom()) ->
	{ok, {CollectionName::term(), PluginInfo::[tuple()]}} | {error, no_entry}.
get_storage_info(Application, CollectionName) ->
	{ok, StorageInfo} = application:get_env(Application, CollectionName),
	parse_storage_info(Application, StorageInfo).

-spec parse_storage_info(Application::atom(), StorageInfo::[tuple()]) ->
	{ok, {CollectionName::term(), PluginInfo::[tuple()]}} | {error, no_entry}.
parse_storage_info(Application, StorageInfo) ->
	case parse_collection_specific_storage_info(StorageInfo) of
		{error, no_entry} ->
			parse_application_specific_storage_info(Application, StorageInfo);
		{ok, ParsedInfo} ->
			{ok, ParsedInfo}
	end.

-spec parse_collection_specific_storage_info(StorageInfo::[tuple()]) ->
	{ok, {CollectionName::term(), PluginInfo::[tuple()]}} | {error, no_entry}.
parse_collection_specific_storage_info(StorageInfo) ->
	case proplists:get_value(collection_name, StorageInfo) of
		undefined ->
			{error, no_entry};
		CollectionName ->
			case proplists:get_value(kv_storage, StorageInfo) of
				undefined ->
					{error, no_entry};
				PluginInfo ->
					{ok, {CollectionName, PluginInfo}}
			end
	end.

-spec parse_application_specific_storage_info(Application::atom(), StorageInfo::[tuple()]) ->
	{ok, {CollectionName::term(), PluginInfo::[tuple()]}} | {error, no_entry}.
parse_application_specific_storage_info(Application, StorageInfo) ->
	CollectionName = StorageInfo,
	case application:get_env(Application, kv_storage) of
		undefined ->
			{error, no_entry};
		{ok, PluginInfo} ->
			{ok, {CollectionName, PluginInfo}}
	end.
