%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager_cache).
-typing([eqwalizer]).

-export([
    ensure_tables/0,
    delete_tables/0,
    stop_shards/1,
    sync_shard_table/1,
    put_shard_pid/2,
    clear_shard_pid/1,
    lookup_cached_guild_pid/1,
    maybe_cache_guild_pid/3,
    cleanup_guild_from_cache/1
]).

-type guild_id() :: integer().
-type shard_map() :: #{pid := pid(), ref := reference()}.
-type state() :: #{shards := #{non_neg_integer() => shard_map()}, shard_count := pos_integer()}.

-export_type([guild_id/0, state/0]).

-define(GUILD_PID_CACHE, guild_pid_cache).
-define(SHARD_TABLE, guild_manager_shard_table).

-type ets_option() ::
    ets:table_type()
    | ets:table_access()
    | named_table
    | {keypos, pos_integer()}
    | {heir, pid(), term()}
    | {heir, none}
    | {write_concurrency, boolean() | auto}
    | {read_concurrency, boolean()}
    | {decentralized_counters, boolean()}
    | compressed.

-spec ensure_tables() -> ok.
ensure_tables() ->
    ensure_shard_table(),
    ensure_guild_pid_cache().

-spec ensure_shard_table() -> ok.
ensure_shard_table() ->
    ensure_named_table(?SHARD_TABLE, [named_table, public, set, {read_concurrency, true}]).

-spec ensure_guild_pid_cache() -> ok.
ensure_guild_pid_cache() ->
    ensure_named_table(
        ?GUILD_PID_CACHE,
        [named_table, public, set, {read_concurrency, true}, {write_concurrency, true}]
    ).

-spec delete_tables() -> ok.
delete_tables() ->
    safe_delete_table(?SHARD_TABLE),
    safe_delete_table(?GUILD_PID_CACHE).

-spec stop_shards(state()) -> ok.
stop_shards(State) ->
    Shards = maps:get(shards, State),
    lists:foreach(fun stop_shard/1, maps:values(Shards)).

-spec sync_shard_table(state()) -> ok.
sync_shard_table(State) ->
    ensure_shard_table(),
    _ = ets:delete_all_objects(?SHARD_TABLE),
    ShardCount = maps:get(shard_count, State),
    ets:insert(?SHARD_TABLE, {shard_count, ShardCount}),
    Shards = maps:get(shards, State),
    maps:foreach(fun(Index, Pid) -> sync_shard_pid({Index, Pid}) end, Shards).

-spec put_shard_pid(non_neg_integer(), pid()) -> ok.
put_shard_pid(Index, Pid) ->
    ensure_shard_table(),
    ets:insert(?SHARD_TABLE, {{shard_pid, Index}, Pid}),
    ok.

-spec clear_shard_pid(non_neg_integer()) -> ok.
clear_shard_pid(Index) ->
    try ets:delete(?SHARD_TABLE, {shard_pid, Index}) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec lookup_cached_guild_pid(guild_id()) -> {ok, pid()} | not_found.
lookup_cached_guild_pid(GuildId) ->
    case safe_lookup(?GUILD_PID_CACHE, GuildId) of
        [{GuildId, GuildPid}] when is_pid(GuildPid) ->
            validate_cached_guild_pid(GuildId, GuildPid);
        _ ->
            not_found
    end.

-spec maybe_cache_guild_pid(guild_id(), term(), term()) -> term().
maybe_cache_guild_pid(GuildId, {start_or_lookup, GuildId}, {ok, GuildPid} = Reply) when
    is_pid(GuildPid)
->
    maybe_insert_cached_guild_pid(GuildId, GuildPid),
    Reply;
maybe_cache_guild_pid(GuildId, {lookup, GuildId}, {ok, GuildPid} = Reply) when
    is_pid(GuildPid)
->
    maybe_insert_cached_guild_pid(GuildId, GuildPid),
    Reply;
maybe_cache_guild_pid(
    GuildId, {start_transferred, GuildId, _TransferState}, {ok, GuildPid} = Reply
) when
    is_pid(GuildPid)
->
    maybe_insert_cached_guild_pid(GuildId, GuildPid),
    Reply;
maybe_cache_guild_pid(_GuildId, _Request, Reply) ->
    Reply.

-spec cleanup_guild_from_cache(pid()) -> ok.
cleanup_guild_from_cache(Pid) ->
    Matches = safe_match_object(?GUILD_PID_CACHE, {'$1', Pid}),
    lists:foreach(fun delete_cached_match/1, Matches),
    ok.

-spec delete_cached_match(term()) -> ok.
delete_cached_match({GuildId, _Pid}) ->
    ets:delete(?GUILD_PID_CACHE, GuildId),
    ok;
delete_cached_match(_Other) ->
    ok.

-spec ensure_named_table(atom(), [ets_option()]) -> ok.
ensure_named_table(Name, Options) ->
    case ets:whereis(Name) of
        undefined -> try_create_named_table(Name, Options);
        _Tid -> ok
    end.

-spec try_create_named_table(atom(), [ets_option()]) -> ok.
try_create_named_table(Name, Options) ->
    try ets:new(Name, Options) of
        _Tid -> ok
    catch
        error:badarg -> ok
    end.

-spec safe_delete_table(atom()) -> ok.
safe_delete_table(Name) ->
    try ets:delete(Name) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec stop_shard(shard_map()) -> ok.
stop_shard(ShardMap) ->
    Pid = maps:get(pid, ShardMap),
    try gen_server:stop(Pid, shutdown, 5000) of
        _ -> ok
    catch
        exit:_Reason -> ok
    end.

-spec sync_shard_pid({non_neg_integer(), shard_map()}) -> ok.
sync_shard_pid({Index, #{pid := Pid}}) ->
    put_shard_pid(Index, Pid).

-spec safe_lookup(atom(), term()) -> [term()].
safe_lookup(Table, Key) ->
    try ets:lookup(Table, Key) of
        Objects -> Objects
    catch
        error:badarg -> []
    end.

-spec safe_match_object(atom(), tuple()) -> [term()].
safe_match_object(Table, Pattern) ->
    try ets:match_object(Table, Pattern) of
        Objects -> Objects
    catch
        error:badarg -> []
    end.

-spec validate_cached_guild_pid(guild_id(), pid()) -> {ok, pid()} | not_found.
validate_cached_guild_pid(GuildId, GuildPid) ->
    case
        is_cached_guild_pid_alive(GuildPid) andalso is_registered_guild_pid(GuildId, GuildPid)
    of
        true ->
            {ok, GuildPid};
        false ->
            ets:delete(?GUILD_PID_CACHE, GuildId),
            not_found
    end.

-spec is_registered_guild_pid(guild_id(), pid()) -> boolean().
is_registered_guild_pid(GuildId, GuildPid) ->
    GuildKey = process_registry:build_process_key(guild, GuildId),
    case process_registry:registry_whereis(GuildKey) of
        GuildPid -> true;
        _ -> false
    end.

-spec maybe_insert_cached_guild_pid(guild_id(), pid()) -> ok.
maybe_insert_cached_guild_pid(GuildId, GuildPid) ->
    case node(GuildPid) of
        LocalNode when LocalNode =:= node() ->
            ensure_guild_pid_cache(),
            ets:insert(?GUILD_PID_CACHE, {GuildId, GuildPid}),
            ok;
        _ ->
            ok
    end.

-spec is_cached_guild_pid_alive(pid()) -> boolean().
is_cached_guild_pid_alive(GuildPid) ->
    case node(GuildPid) of
        LocalNode when LocalNode =:= node() ->
            process_liveness:is_alive(GuildPid);
        _ ->
            false
    end.
