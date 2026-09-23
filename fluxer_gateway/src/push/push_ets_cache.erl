%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_ets_cache).
-typing([eqwalizer]).

-export([
    init/0,
    get_user_guild_settings/2,
    put_user_guild_settings/3,
    put_user_guild_settings/4,
    delete_user_guild_settings/2,
    reserve_user_guild_settings/2,
    get_subscriptions/1,
    get_subscriptions_many/1,
    put_subscriptions/3,
    delete_subscriptions/1,
    reserve_subscriptions/1,
    get_blocked_ids/1,
    put_blocked_ids/2,
    put_blocked_ids_fetched/3,
    reserve_blocked_ids/1,
    get_badge_count/1,
    put_badge_count/4,
    delete_badge_count/1,
    reserve_badge_counts/1,
    get_bearer_token/1,
    put_bearer_token/3,
    release/1,
    rebalance/0,
    rebalance_async/0,
    cache_stats/0,
    evict_tables/1,
    table_size/1
]).

-export_type([fill/0]).

-define(USER_GUILD_SETTINGS, push_user_guild_settings).
-define(SUBSCRIPTIONS, push_subscriptions).
-define(BLOCKED_IDS, push_blocked_ids).
-define(BADGE_COUNTS, push_badge_counts).
-define(BEARER_TOKENS, push_bearer_tokens).

-define(MAX_TABLE_ENTRIES, 500000).
-define(MAX_BEARER_TOKENS, 10000).
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
    ensure_table(?SUBSCRIPTIONS),
    ensure_table(?BLOCKED_IDS),
    ensure_table(?BADGE_COUNTS),
    ensure_table(?BEARER_TOKENS),
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

-spec delete_user_guild_settings(integer(), integer()) -> ok.
delete_user_guild_settings(UserId, GuildId) ->
    safe_delete(?USER_GUILD_SETTINGS, {UserId, GuildId}).

-spec reserve_user_guild_settings([integer()], integer()) -> fill().
reserve_user_guild_settings(UserIds, GuildId) ->
    reserve(?USER_GUILD_SETTINGS, [{UserId, GuildId} || UserId <- UserIds]).

-spec get_subscriptions(integer()) -> list() | undefined.
get_subscriptions(UserId) ->
    try ets:lookup(?SUBSCRIPTIONS, UserId) of
        [{UserId, Subs}] when is_list(Subs) -> Subs;
        _ -> undefined
    catch
        error:badarg -> undefined
    end.

-spec get_subscriptions_many([integer()]) -> {#{integer() => list()}, [integer()]}.
get_subscriptions_many(UserIds) ->
    lists:foldl(
        fun add_cached_subscriptions/2,
        {#{}, []},
        UserIds
    ).

-spec add_cached_subscriptions(integer(), {#{integer() => list()}, [integer()]}) ->
    {#{integer() => list()}, [integer()]}.
add_cached_subscriptions(UserId, {CachedAcc, MissingAcc}) ->
    case get_subscriptions(UserId) of
        Subscriptions when is_list(Subscriptions) ->
            {CachedAcc#{UserId => Subscriptions}, MissingAcc};
        undefined ->
            {CachedAcc, [UserId | MissingAcc]}
    end.

-spec put_subscriptions(integer(), list(), fill()) -> ok.
put_subscriptions(UserId, Subscriptions, {?SUBSCRIPTIONS, _, _} = Fill) ->
    fill(Fill, {UserId, Subscriptions}).

-spec delete_subscriptions(integer()) -> ok.
delete_subscriptions(UserId) ->
    safe_delete(?SUBSCRIPTIONS, UserId).

-spec reserve_subscriptions([integer()]) -> fill().
reserve_subscriptions(UserIds) ->
    reserve(?SUBSCRIPTIONS, UserIds).

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

-spec put_blocked_ids(integer(), [integer()]) -> ok.
put_blocked_ids(UserId, BlockedIds) ->
    write(?BLOCKED_IDS, {UserId, BlockedIds, infinity}).

-spec put_blocked_ids_fetched(integer(), [integer()], fill()) -> ok.
put_blocked_ids_fetched(UserId, BlockedIds, {?BLOCKED_IDS, _, _} = Fill) ->
    ExpiresAt = erlang:system_time(second) + blocked_ids_ttl_seconds(),
    fill(Fill, {UserId, BlockedIds, ExpiresAt}).

-spec reserve_blocked_ids([integer()]) -> fill().
reserve_blocked_ids(UserIds) ->
    reserve(?BLOCKED_IDS, UserIds).

-spec blocked_ids_ttl_seconds() -> pos_integer().
blocked_ids_ttl_seconds() ->
    Value = app_pos_integer(push_blocked_ids_cache_ttl_seconds, ?DEFAULT_BLOCKED_IDS_TTL),
    min(max(Value, ?MIN_BLOCKED_IDS_TTL), ?MAX_BLOCKED_IDS_TTL).

-spec app_pos_integer(atom(), pos_integer()) -> pos_integer().
app_pos_integer(Key, Default) ->
    case application:get_env(fluxer_gateway, Key, undefined) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> Default
    end.

-spec get_badge_count(integer()) -> {non_neg_integer(), integer()} | undefined.
get_badge_count(UserId) ->
    try ets:lookup(?BADGE_COUNTS, UserId) of
        [{UserId, Count, CachedAt}] when is_integer(Count), Count >= 0, is_integer(CachedAt) ->
            {Count, CachedAt};
        _ ->
            undefined
    catch
        error:badarg -> undefined
    end.

-spec put_badge_count(integer(), non_neg_integer(), integer(), fill()) -> ok.
put_badge_count(UserId, Count, CachedAt, {?BADGE_COUNTS, _, _} = Fill) ->
    fill(Fill, {UserId, Count, CachedAt}).

-spec delete_badge_count(integer()) -> ok.
delete_badge_count(UserId) ->
    safe_delete(?BADGE_COUNTS, UserId).

-spec reserve_badge_counts([integer()]) -> fill().
reserve_badge_counts(UserIds) ->
    reserve(?BADGE_COUNTS, UserIds).

-spec get_bearer_token(term()) -> {ok, binary(), integer()} | undefined.
get_bearer_token(Key) ->
    try ets:lookup(?BEARER_TOKENS, Key) of
        [{_, Token, ExpiresAt}] when is_binary(Token), is_integer(ExpiresAt) ->
            {ok, Token, ExpiresAt};
        _ ->
            undefined
    catch
        error:badarg -> undefined
    end.

-spec put_bearer_token(term(), binary(), integer()) -> ok.
put_bearer_token(Key, Token, ExpiresAt) when is_binary(Token), is_integer(ExpiresAt) ->
    guard_table_size(?BEARER_TOKENS, ?MAX_BEARER_TOKENS),
    try ets:insert(?BEARER_TOKENS, {Key, Token, ExpiresAt}) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

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
stale_rows(?BADGE_COUNTS, Key) ->
    [{{Key, '_', '_'}, [], [true]}];
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

-spec rebalance_async() -> ok.
rebalance_async() ->
    _ = spawn(fun rebalance/0),
    ok.

-spec rebalance() -> ok.
rebalance() ->
    init(),
    _ = rebalance_table(?USER_GUILD_SETTINGS, fun user_id_from_user_guild_key/1),
    _ = rebalance_table(?SUBSCRIPTIONS, fun user_id_from_key/1),
    _ = rebalance_table(?BLOCKED_IDS, fun user_id_from_key/1),
    _ = rebalance_table(?BADGE_COUNTS, fun user_id_from_key/1),
    ok.

-spec cache_stats() -> map().
cache_stats() ->
    #{
        user_guild_settings_size => table_size(?USER_GUILD_SETTINGS),
        push_subscriptions_size => table_size(?SUBSCRIPTIONS),
        blocked_ids_size => table_size(?BLOCKED_IDS),
        badge_counts_size => table_size(?BADGE_COUNTS),
        bearer_tokens_size => table_size(?BEARER_TOKENS)
    }.

-spec evict_tables(map()) -> ok.
evict_tables(MaxEntries) ->
    Now = erlang:system_time(second),
    select_delete(?BLOCKED_IDS, expired_rows(Now)),
    select_delete(?BEARER_TOKENS, expired_rows(Now)),
    lists:foreach(
        fun expire_reservations/1,
        [?USER_GUILD_SETTINGS, ?SUBSCRIPTIONS, ?BLOCKED_IDS, ?BADGE_COUNTS]
    ),
    evict_table(?USER_GUILD_SETTINGS, maps:get(user_guild_settings, MaxEntries, undefined)),
    evict_table(?SUBSCRIPTIONS, maps:get(subscriptions, MaxEntries, undefined)),
    evict_table(?BLOCKED_IDS, maps:get(blocked_ids, MaxEntries, undefined)),
    evict_table(?BADGE_COUNTS, maps:get(badge_counts, MaxEntries, undefined)),
    evict_table(?BEARER_TOKENS, ?MAX_BEARER_TOKENS),
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

synced_blocked_ids_are_stored_without_an_expiry_test() ->
    init(),
    try
        ok = put_blocked_ids(4001, [4002]),
        ?assertEqual([{4001, [4002], infinity}], ets:lookup(?BLOCKED_IDS, 4001))
    after
        safe_delete(?BLOCKED_IDS, 4001)
    end.

synced_blocked_ids_never_expire_test() ->
    init(),
    try
        ok = put_blocked_ids(4002, [4003]),
        ?assertEqual([4003], get_blocked_ids(4002)),
        ok = evict_tables(#{}),
        ?assertEqual([4003], get_blocked_ids(4002))
    after
        safe_delete(?BLOCKED_IDS, 4002)
    end.

fetched_blocked_ids_expire_and_are_reclaimed_test() ->
    init(),
    try
        ok = put_blocked_ids_fetched(4004, [4005], reserve_blocked_ids([4004])),
        ?assertEqual([4005], get_blocked_ids(4004)),
        Stale = erlang:system_time(second) - 1,
        true = ets:insert(?BLOCKED_IDS, {4004, [4005], Stale}),
        ?assertEqual(undefined, get_blocked_ids(4004)),
        ok = evict_tables(#{}),
        ?assertEqual([], ets:lookup(?BLOCKED_IDS, 4004))
    after
        safe_delete(?BLOCKED_IDS, 4004)
    end.

untimed_blocked_ids_are_still_honoured_test() ->
    init(),
    try
        true = ets:insert(?BLOCKED_IDS, {4008, [4009]}),
        ?assertEqual([4009], get_blocked_ids(4008)),
        ok = evict_tables(#{}),
        ?assertEqual([4009], get_blocked_ids(4008))
    after
        safe_delete(?BLOCKED_IDS, 4008)
    end.

blocked_ids_ttl_is_clamped_test() ->
    ?assertEqual(?DEFAULT_BLOCKED_IDS_TTL, blocked_ids_ttl_seconds()),
    ?assertEqual(?MIN_BLOCKED_IDS_TTL, with_ttl_env(1, fun blocked_ids_ttl_seconds/0)),
    ?assertEqual(?MAX_BLOCKED_IDS_TTL, with_ttl_env(999999, fun blocked_ids_ttl_seconds/0)),
    ?assertEqual(?DEFAULT_BLOCKED_IDS_TTL, with_ttl_env(0, fun blocked_ids_ttl_seconds/0)).

with_ttl_env(Value, Fun) ->
    application:set_env(fluxer_gateway, push_blocked_ids_cache_ttl_seconds, Value),
    try
        Fun()
    after
        application:unset_env(fluxer_gateway, push_blocked_ids_cache_ttl_seconds)
    end.

-endif.
