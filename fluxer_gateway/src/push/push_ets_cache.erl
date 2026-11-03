%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_ets_cache).
-typing([eqwalizer]).

-export([
    init/0,
    get_user_guild_settings/2,
    put_user_guild_settings/3, put_user_guild_settings/4,
    reserve_user_guild_settings/2,
    get_blocked_ids/1,
    put_blocked_ids_fetched/3,
    reserve_blocked_ids/1,
    release/1,
    rebalance/0,
    cache_stats/0,
    evict_tables/1
]).

-export_type([fill/0]).

-define(USER_GUILD_SETTINGS, push_user_guild_settings).
-define(BLOCKED_IDS, push_blocked_ids).

-define(MAX_TABLE_ENTRIES, 500000).
-define(EVICT_BATCH, 4096).
-define(MAX_EVICT_RESEEKS, 8).
-define(RESERVATION_TTL_MS, 120000).
-define(DEFAULT_BLOCKED_IDS_TTL, 300).
-define(MIN_BLOCKED_IDS_TTL, 30).
-define(MAX_BLOCKED_IDS_TTL, 3600).

-define(ETS_OPTS, [
    named_table, public, set, {read_concurrency, true}, {write_concurrency, true}
]).

-type fill() :: {atom(), pos_integer(), [term()]}.

-spec init() -> ok.
init() ->
    ensure_table(?USER_GUILD_SETTINGS),
    ensure_table(?BLOCKED_IDS),
    ok.

-spec get_user_guild_settings(integer(), integer()) -> map() | undefined.
get_user_guild_settings(UserId, GuildId) ->
    try ets:lookup(?USER_GUILD_SETTINGS, {UserId, GuildId}) of
        [{{UserId, GuildId}, Settings}] when is_map(Settings) -> Settings;
        _ -> undefined
    catch
        error:badarg -> undefined
    end.

-spec put_user_guild_settings(integer(), integer(), map()) -> ok.
put_user_guild_settings(UserId, GuildId, Settings) ->
    write(?USER_GUILD_SETTINGS, {{UserId, GuildId}, Settings}).

-spec put_user_guild_settings(integer(), integer(), map(), fill()) -> ok.
put_user_guild_settings(UserId, GuildId, Settings, {?USER_GUILD_SETTINGS, _, _} = Fill) ->
    fill(Fill, {{UserId, GuildId}, Settings}).

-spec reserve_user_guild_settings([integer()], integer()) -> fill().
reserve_user_guild_settings(UserIds, GuildId) ->
    reserve(?USER_GUILD_SETTINGS, [{UserId, GuildId} || UserId <- UserIds]).

-spec get_blocked_ids(integer()) -> [integer()] | undefined.
get_blocked_ids(UserId) ->
    try ets:lookup(?BLOCKED_IDS, UserId) of
        [{UserId, BlockedIds}] when is_list(BlockedIds) ->
            BlockedIds;
        [{UserId, BlockedIds, infinity}] when is_list(BlockedIds) ->
            BlockedIds;
        [{UserId, BlockedIds, ExpiresAt}] when is_list(BlockedIds), is_integer(ExpiresAt) ->
            live_fetched_blocked_ids(BlockedIds, ExpiresAt);
        _ ->
            undefined
    catch
        error:badarg -> undefined
    end.

-spec live_fetched_blocked_ids([integer()], integer()) -> [integer()] | undefined.
live_fetched_blocked_ids(BlockedIds, ExpiresAt) ->
    case erlang:system_time(second) < ExpiresAt of
        true -> BlockedIds;
        false -> undefined
    end.

-spec put_blocked_ids_fetched(integer(), [integer()], fill()) -> ok.
put_blocked_ids_fetched(UserId, BlockedIds, {?BLOCKED_IDS, _, _} = Fill) ->
    ExpiresAt = erlang:system_time(second) + blocked_ids_ttl_seconds(),
    fill(Fill, {UserId, BlockedIds, ExpiresAt}).

-spec reserve_blocked_ids([integer()]) -> fill().
reserve_blocked_ids(UserIds) ->
    reserve(?BLOCKED_IDS, UserIds).

-spec blocked_ids_ttl_seconds() -> pos_integer().
blocked_ids_ttl_seconds() ->
    Value = push_utils:app_pos_integer(
        push_blocked_ids_cache_ttl_seconds, ?DEFAULT_BLOCKED_IDS_TTL
    ),
    min(max(Value, ?MIN_BLOCKED_IDS_TTL), ?MAX_BLOCKED_IDS_TTL).

-spec write(atom(), tuple()) -> ok.
write(Table, Row) ->
    guard_table_size(Table, ?MAX_TABLE_ENTRIES),
    try ets:insert(Table, Row) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec reserve(atom(), [term()]) -> fill().
reserve(Table, Keys) ->
    guard_table_size(Table, ?MAX_TABLE_ENTRIES),
    Token = erlang:unique_integer([positive]),
    ReservedAt = erlang:monotonic_time(millisecond),
    lists:foreach(fun(Key) -> reserve_key(Table, Key, Token, ReservedAt) end, Keys),
    {Table, Token, Keys}.

-spec reserve_key(atom(), term(), pos_integer(), integer()) -> ok.
reserve_key(Table, Key, Token, ReservedAt) ->
    try
        _ = ets:select_delete(Table, stale_rows(Table, Key)),
        _ = ets:insert_new(Table, {Key, pending, Token, ReservedAt}),
        ok
    catch
        error:badarg -> ok
    end.

-spec stale_rows(atom(), term()) -> ets:match_spec().
stale_rows(?BLOCKED_IDS, Key) ->
    Now = erlang:system_time(second),
    [{{Key, '_', '$1'}, [{is_integer, '$1'}, {'=<', '$1', Now}], [true]}];
stale_rows(_Table, _Key) ->
    [].

-spec fill(fill(), tuple()) -> ok.
fill({Table, Token, _Keys}, Row) ->
    Key = element(1, Row),
    try ets:select_replace(Table, [{{Key, pending, Token, '_'}, [], [{const, Row}]}]) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec release(fill()) -> ok.
release({Table, Token, Keys}) ->
    lists:foreach(
        fun(Key) -> select_delete(Table, [{{Key, pending, Token, '_'}, [], [true]}]) end,
        Keys
    ).

-spec rebalance() -> ok.
rebalance() ->
    init(),
    _ = rebalance_table(?USER_GUILD_SETTINGS, fun user_id_from_user_guild_key/1),
    _ = rebalance_table(?BLOCKED_IDS, fun user_id_from_key/1),
    ok.

-spec cache_stats() -> map().
cache_stats() ->
    #{
        user_guild_settings_size => table_size(?USER_GUILD_SETTINGS),
        blocked_ids_size => table_size(?BLOCKED_IDS)
    }.

-spec evict_tables(map()) -> ok.
evict_tables(MaxEntries) ->
    Now = erlang:system_time(second),
    select_delete(?BLOCKED_IDS, expired_rows(Now)),
    lists:foreach(fun expire_reservations/1, [?USER_GUILD_SETTINGS, ?BLOCKED_IDS]),
    evict_table(?USER_GUILD_SETTINGS, maps:get(user_guild_settings, MaxEntries, undefined)),
    evict_table(?BLOCKED_IDS, maps:get(blocked_ids, MaxEntries, undefined)),
    ok.

-spec expired_rows(integer()) -> ets:match_spec().
expired_rows(Now) ->
    [{{'_', '_', '$1'}, [{is_integer, '$1'}, {'=<', '$1', Now}], [true]}].

-spec expire_reservations(atom()) -> ok.
expire_reservations(Table) ->
    Cutoff = erlang:monotonic_time(millisecond) - ?RESERVATION_TTL_MS,
    select_delete(Table, [{{'_', pending, '_', '$1'}, [{'<', '$1', Cutoff}], [true]}]).

-spec select_delete(atom(), ets:match_spec()) -> ok.
select_delete(Table, MatchSpec) ->
    try ets:select_delete(Table, MatchSpec) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec guard_table_size(atom(), non_neg_integer()) -> ok.
guard_table_size(Table, MaxEntries) ->
    case table_size(Table) >= MaxEntries of
        true -> evict_table(Table, max(0, MaxEntries - ?EVICT_BATCH));
        false -> ok
    end.

-spec ensure_table(atom()) -> ok.
ensure_table(Name) ->
    case ets:whereis(Name) of
        undefined -> create_table(Name);
        _ -> ok
    end.

-spec create_table(atom()) -> ok.
create_table(Name) ->
    try
        _ = ets:new(Name, ?ETS_OPTS),
        ok
    catch
        error:badarg -> ok
    end.

-spec table_size(atom()) -> non_neg_integer().
table_size(Table) ->
    try ets:info(Table, size) of
        Size when is_integer(Size) -> Size;
        _ -> 0
    catch
        error:badarg -> 0
    end.

-spec evict_table(atom(), non_neg_integer() | undefined) -> ok.
evict_table(_Table, undefined) ->
    ok;
evict_table(Table, MaxEntries) ->
    Size = table_size(Table),
    case Size > MaxEntries of
        true ->
            ToDelete = Size - MaxEntries,
            evict_scan(Table, ToDelete, ?MAX_EVICT_RESEEKS);
        false ->
            ok
    end.

-spec evict_scan(atom(), non_neg_integer(), non_neg_integer()) -> ok.
evict_scan(_Table, 0, _Reseeks) ->
    ok;
evict_scan(_Table, _N, 0) ->
    ok;
evict_scan(Table, N, Reseeks) ->
    case evict_n(Table, first_key(Table), N) of
        {done, _Remaining} -> ok;
        {interrupted, Remaining} -> evict_scan(Table, Remaining, Reseeks - 1)
    end.

-spec evict_n(atom(), term(), non_neg_integer()) ->
    {done, non_neg_integer()} | {interrupted, non_neg_integer()}.
evict_n(_Table, '$end_of_table', N) ->
    {done, N};
evict_n(_Table, _Key, 0) ->
    {done, 0};
evict_n(Table, Key, N) ->
    case next_key(Table, Key) of
        {ok, Next} ->
            safe_delete(Table, Key),
            evict_n(Table, Next, N - 1);
        {error, badarg} ->
            {interrupted, N}
    end.

-spec first_key(atom()) -> term().
first_key(Table) ->
    try ets:first(Table) of
        Key -> Key
    catch
        error:badarg -> '$end_of_table'
    end.

-spec next_key(atom(), term()) -> {ok, term()} | {error, badarg}.
next_key(Table, Key) ->
    try ets:next(Table, Key) of
        Next -> {ok, Next}
    catch
        error:badarg -> {error, badarg}
    end.

-spec rebalance_table(atom(), fun((term()) -> integer() | undefined)) -> non_neg_integer().
rebalance_table(Table, UserIdFun) ->
    KeysToDelete = collect_non_local_keys(Table, UserIdFun),
    lists:foreach(fun(Key) -> safe_delete(Table, Key) end, KeysToDelete),
    length(KeysToDelete).

-spec collect_non_local_keys(atom(), fun((term()) -> integer() | undefined)) -> [term()].
collect_non_local_keys(Table, UserIdFun) ->
    try
        ets:foldl(
            fun(Record, Acc) ->
                Key = element(1, Record),
                accumulate_if_remote(Key, UserIdFun, Acc)
            end,
            [],
            Table
        )
    catch
        error:badarg -> []
    end.

-spec accumulate_if_remote(term(), fun((term()) -> integer() | undefined), [term()]) ->
    [term()].
accumulate_if_remote(Key, UserIdFun, Acc) ->
    case UserIdFun(Key) of
        UserId when is_integer(UserId) -> accumulate_owner_key(Key, UserId, Acc);
        undefined -> Acc
    end.

-spec accumulate_owner_key(term(), integer(), [term()]) -> [term()].
accumulate_owner_key(Key, UserId, Acc) ->
    case gateway_node_router:owner_node_result(UserId, push) of
        {ok, OwnerNode} when OwnerNode =:= node() -> Acc;
        {ok, OwnerNode} when is_atom(OwnerNode) -> [Key | Acc];
        {error, _Reason} -> Acc
    end.

-spec safe_delete(atom(), term()) -> ok.
safe_delete(Table, Key) ->
    try ets:delete(Table, Key) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec user_id_from_user_guild_key(term()) -> integer() | undefined.
user_id_from_user_guild_key({UserId, _GuildId}) when is_integer(UserId) ->
    UserId;
user_id_from_user_guild_key(_) ->
    undefined.

-spec user_id_from_key(term()) -> integer() | undefined.
user_id_from_key(UserId) when is_integer(UserId) ->
    UserId;
user_id_from_key(_) ->
    undefined.
