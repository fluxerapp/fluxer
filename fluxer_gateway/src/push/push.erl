%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    handle_message_create/1,
    sync_user_guild_settings/3,
    sync_user_guild_settings_local/3,
    sync_user_blocked_ids/2,
    sync_user_blocked_ids_local/2,
    invalidate_user_subscriptions_local/1,
    invalidate_user_badge_count_local/1,
    invalidate_user_badge_counts_local/1,
    clear_channel_notifications/3
]).
-export([get_cache_stats/0]).
-export([push_owner_key/1]).

-define(EVICT_INTERVAL_MS, 60000).
-define(DEFAULT_MAX_ENTRIES, 500000).

-define(PUSH_COUNTER_TABLE, push_worker_counter).
-define(CNT_WORKER_POOL, push_loss_worker_pool).
-define(CNT_FETCH_ATTEMPTS, push_blocked_ids_fetch_attempts).
-define(CNT_FETCH_FAILURES, push_blocked_ids_fetch_failures).
-define(CNT_SUPPRESSED, push_blocked_ids_suppressed).
-define(CNT_BUDGET_EXHAUSTED, push_blocked_ids_budget_exhausted).
-define(MAX_FETCH_RPCS, 8).
-define(DEFAULT_FETCH_USERS, 2000).
-define(MAX_FETCH_USERS, 5000).
-define(DEFAULT_FETCH_CHUNK, 500).
-define(MIN_FETCH_CHUNK, 50).
-define(MAX_FETCH_CHUNK, 1000).

-type state() :: #{
    max_entries := non_neg_integer()
}.

-spec start_link() -> {ok, pid()} | {error, term()} | ignore.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:process_flag(fullsweep_after, 10),
    push_ets_cache:init(),
    init_worker_counter(),
    maybe_schedule_eviction(env_boolean(push_enabled)),
    {ok, #{max_entries => ?DEFAULT_MAX_ENTRIES}}.

-spec maybe_schedule_eviction(boolean()) -> ok.
maybe_schedule_eviction(true) ->
    _ = schedule_eviction(),
    ok;
maybe_schedule_eviction(false) ->
    ok.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call(get_cache_stats, _From, State) ->
    {reply, {ok, cache_stats_with_counters()}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({handle_message_create, Params}, State) when is_map(Params) ->
    handle_message_create_cast(Params, State);
handle_cast({sync_user_guild_settings, UserId, GuildId, UserGuildSettings}, State) when
    is_integer(UserId), is_integer(GuildId), is_map(UserGuildSettings)
->
    push_ets_cache:put_user_guild_settings(UserId, GuildId, UserGuildSettings),
    {noreply, State};
handle_cast({sync_user_blocked_ids, UserId, BlockedIds}, State) when is_integer(UserId) ->
    handle_sync_user_blocked_ids(UserId, BlockedIds, State);
handle_cast({cache_user_guild_settings, UserId, GuildId, Settings}, State) when
    is_integer(UserId), is_integer(GuildId), is_map(Settings)
->
    push_ets_cache:put_user_guild_settings(UserId, GuildId, Settings),
    {noreply, State};
handle_cast({clear_channel_notifications, UserId, ChannelId, MessageId}, State) when
    is_integer(UserId), is_integer(ChannelId), is_integer(MessageId)
->
    handle_clear_channel_notifications(UserId, ChannelId, MessageId, State);
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(evict_caches, State) ->
    MaxEntries = maps:get(max_entries, State),
    push_ets_cache:evict_tables(#{
        user_guild_settings => MaxEntries,
        blocked_ids => MaxEntries
    }),
    schedule_eviction(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec handle_message_create(map()) -> ok.
handle_message_create(Params) ->
    case is_push_active() of
        true -> cast_to_push_owner(push_owner_key(Params), {handle_message_create, Params});
        false -> ok
    end.

-spec sync_user_guild_settings(integer(), integer(), map()) -> ok.
sync_user_guild_settings(UserId, GuildId, Settings) ->
    maybe_cast(UserId, {sync_user_guild_settings, UserId, GuildId, Settings}).

-spec sync_user_guild_settings_local(integer(), integer(), map()) -> ok.
sync_user_guild_settings_local(UserId, GuildId, Settings) ->
    local_cache_mutation(fun() ->
        push_ets_cache:put_user_guild_settings(UserId, GuildId, Settings)
    end).

-spec sync_user_blocked_ids(integer(), [integer()]) -> ok.
sync_user_blocked_ids(UserId, BlockedIds) ->
    maybe_cast(UserId, {sync_user_blocked_ids, UserId, BlockedIds}).

-spec sync_user_blocked_ids_local(integer(), term()) -> ok.
sync_user_blocked_ids_local(UserId, BlockedIds) ->
    case push_normalize:integer_list(BlockedIds) of
        {ok, TypedBlockedIds} ->
            put_blocked_ids_local(UserId, TypedBlockedIds);
        error ->
            ok
    end.

-spec invalidate_user_subscriptions_local(integer()) -> ok.
invalidate_user_subscriptions_local(_UserId) ->
    ok.

-spec invalidate_user_badge_count_local(integer()) -> ok.
invalidate_user_badge_count_local(_UserId) ->
    ok.

-spec invalidate_user_badge_counts_local([integer()]) -> ok.
invalidate_user_badge_counts_local(_UserIds) ->
    ok.

-spec put_blocked_ids_local(integer(), [integer()]) -> ok.
put_blocked_ids_local(UserId, TypedBlockedIds) ->
    local_cache_mutation(fun() ->
        push_ets_cache:put_blocked_ids(UserId, TypedBlockedIds)
    end).

-spec maybe_cast(term(), term()) -> ok.
maybe_cast(Key, Msg) ->
    case is_push_noop() of
        true -> ok;
        false -> cast_to_push_owner(Key, Msg)
    end.

-spec is_push_active() -> boolean().
is_push_active() ->
    not is_push_noop() andalso env_boolean(push_enabled).

-spec is_push_noop() -> boolean().
is_push_noop() ->
    persistent_term:get(push_noop, false).

-spec clear_channel_notifications(integer(), integer(), integer()) -> ok.
clear_channel_notifications(UserId, ChannelId, MessageId) ->
    case is_push_active() of
        true ->
            ok = push_outbox:truncate_read(UserId, ChannelId, MessageId),
            cast_clear_if_enabled(
                clear_notifications_enabled(), UserId, ChannelId, MessageId
            );
        false ->
            ok
    end.

-spec cast_clear_if_enabled(boolean(), integer(), integer(), integer()) -> ok.
cast_clear_if_enabled(true, UserId, ChannelId, MessageId) ->
    cast_to_push_owner(UserId, {clear_channel_notifications, UserId, ChannelId, MessageId});
cast_clear_if_enabled(false, _UserId, _ChannelId, _MessageId) ->
    ok.

-spec clear_notifications_enabled() -> boolean().
clear_notifications_enabled() ->
    case persistent_term:get(push_enrolled_clear_notifications_enabled, undefined) of
        OperatorChoice when is_boolean(OperatorChoice) -> OperatorChoice;
        _ -> env_boolean(push_enrolled_clear_notifications_enabled, true)
    end.

-spec get_cache_stats() -> {ok, map()}.
get_cache_stats() ->
    gen_server:call(?MODULE, get_cache_stats, 5000).

-spec push_owner_key(map()) -> term().
push_owner_key(Params) ->
    push_message_params:owner_key(Params).

-spec cast_to_push_owner(term(), term()) -> ok.
cast_to_push_owner(Key, Msg) ->
    case resolve_push_owner(Key) of
        {ok, TargetNode} ->
            Target = push_target(TargetNode),
            safe_cast(Target, Msg);
        unavailable ->
            ok
    end.

-spec safe_cast(gen_server:server_ref(), term()) -> ok.
safe_cast(Target, Msg) ->
    try gen_server:cast(Target, Msg) of
        _ -> ok
    catch
        throw:_Reason -> ok;
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec push_target(node()) -> atom() | {atom(), node()}.
push_target(TargetNode) ->
    case TargetNode =:= node() of
        true -> ?MODULE;
        false -> {?MODULE, TargetNode}
    end.

-spec resolve_push_owner(term()) -> {ok, node()} | unavailable.
resolve_push_owner(undefined) ->
    {ok, node()};
resolve_push_owner(Key) ->
    try gateway_node_router:owner_node_result(Key, push) of
        {ok, OwnerNode} when is_atom(OwnerNode) -> {ok, OwnerNode};
        {error, _Reason} -> unavailable
    catch
        throw:_Reason -> unavailable;
        error:_Reason -> unavailable;
        exit:_Reason -> unavailable
    end.

-spec do_handle_message_create(map()) -> ok.
do_handle_message_create(Params) ->
    case push_message_params:context(Params) of
        {ok, Context} ->
            do_handle_message_create_context(Context);
        {error, Reason} ->
            logger:debug("Push: skipping malformed message create", #{reason => Reason}),
            ok
    end.

-spec do_handle_message_create_context(push_message_params:context()) -> ok.
do_handle_message_create_context(Context) ->
    #{
        message_data := MessageData,
        user_ids := UserIds,
        guild_id := GuildId,
        author_id := AuthorId,
        user_roles := UserRolesMap,
        connected_users := ConnectedUsers,
        channel_id := ChannelId,
        message_id := MessageId,
        guild_default_notifications := GuildDefaultNotifications,
        guild_name := GuildName,
        channel_name := ChannelName,
        markdown_context := MarkdownContext
    } = Context,
    logger:debug("Push: evaluating eligibility", #{
        message_id => MessageId,
        channel_id => ChannelId,
        guild_id => GuildId,
        author_id => AuthorId,
        candidate_count => length(UserIds)
    }),
    EligibleUsers = filter_eligible_users(
        UserIds,
        AuthorId,
        GuildId,
        ChannelId,
        MessageData,
        GuildDefaultNotifications,
        UserRolesMap,
        ConnectedUsers,
        maps:get(large_guild_metadata, Context, undefined)
    ),
    logger:debug("Push: eligibility result", #{
        message_id => MessageId,
        channel_id => ChannelId,
        eligible_count => length(EligibleUsers)
    }),
    publish_eligible_users(
        EligibleUsers,
        MessageData,
        MarkdownContext,
        GuildId,
        ChannelId,
        MessageId,
        GuildName,
        ChannelName
    ).

-spec publish_eligible_users(
    [integer()],
    map(),
    map(),
    integer(),
    integer(),
    integer(),
    binary() | undefined,
    binary() | undefined
) -> ok.
publish_eligible_users(
    [],
    _MessageData,
    _MarkdownContext,
    _GuildId,
    _ChannelId,
    _MessageId,
    _GuildName,
    _ChannelName
) ->
    ok;
publish_eligible_users(
    EligibleUsers,
    MessageData,
    MarkdownContext,
    GuildId,
    ChannelId,
    MessageId,
    GuildName,
    ChannelName
) ->
    _ = push_job_publisher:publish_message(
        EligibleUsers,
        MessageData,
        MarkdownContext,
        GuildId,
        ChannelId,
        MessageId,
        GuildName,
        ChannelName
    ),
    ok.

-spec filter_eligible_users(
    [integer()],
    integer(),
    integer(),
    integer(),
    map(),
    integer(),
    map(),
    map(),
    map() | undefined
) -> [integer()].
filter_eligible_users(
    UserIds,
    AuthorId,
    GuildId,
    ChannelId,
    MessageData,
    GuildDefaultNotifications,
    UserRolesMap,
    ConnectedUsers,
    SuppliedMetadata
) ->
    LargeGuildMetadata = resolve_large_guild_metadata(GuildId, SuppliedMetadata),
    Candidates = drop_blocked_recipients(UserIds, AuthorId, #{}),
    push_eligibility:prefetch_user_guild_settings(Candidates, AuthorId, GuildId),
    EligibleUsers = lists:filter(
        fun(UserId) ->
            push_eligibility:is_eligible_for_push(
                UserId,
                AuthorId,
                GuildId,
                ChannelId,
                MessageData,
                GuildDefaultNotifications,
                UserRolesMap,
                ConnectedUsers,
                LargeGuildMetadata
            )
        end,
        Candidates
    ),
    drop_blocked_recipients(EligibleUsers, AuthorId, fetch_missing_blocked_ids(EligibleUsers)).

-spec resolve_large_guild_metadata(integer(), map() | undefined) -> map() | undefined.
resolve_large_guild_metadata(0, _SuppliedMetadata) ->
    undefined;
resolve_large_guild_metadata(GuildId, SuppliedMetadata) ->
    supplied_or_local_metadata(GuildId, SuppliedMetadata).

-spec supplied_or_local_metadata(integer(), map() | undefined) -> map() | undefined.
supplied_or_local_metadata(GuildId, undefined) ->
    large_guild_metadata_local(GuildId);
supplied_or_local_metadata(_GuildId, SuppliedMetadata) when is_map(SuppliedMetadata) ->
    SuppliedMetadata;
supplied_or_local_metadata(GuildId, _SuppliedMetadata) ->
    large_guild_metadata_local(GuildId).

-spec large_guild_metadata_local(integer()) -> map() | undefined.
large_guild_metadata_local(GuildId) ->
    push_eligibility_checks:get_guild_large_metadata(GuildId).

-spec drop_blocked_recipients([integer()], integer(), #{integer() => [integer()]}) ->
    [integer()].
drop_blocked_recipients(UserIds, AuthorId, Fetched) ->
    Kept = lists:filter(
        fun(UserId) -> not is_blocked_recipient(UserId, AuthorId, Fetched) end,
        UserIds
    ),
    count_suppressed(length(UserIds) - length(Kept)),
    Kept.

-spec is_blocked_recipient(integer(), integer(), #{integer() => [integer()]}) -> boolean().
is_blocked_recipient(UserId, AuthorId, Fetched) ->
    lists:member(AuthorId, maps:get(UserId, Fetched, [])) orelse
        push_eligibility:is_user_blocked(UserId, AuthorId).

-spec count_suppressed(non_neg_integer()) -> ok.
count_suppressed(0) ->
    ok;
count_suppressed(Suppressed) ->
    bump_counter(?CNT_SUPPRESSED, Suppressed).

-spec fetch_missing_blocked_ids([integer()]) -> #{integer() => [integer()]}.
fetch_missing_blocked_ids(UserIds) ->
    case missing_blocked_ids(UserIds) of
        [] -> #{};
        Missing -> fetch_blocked_ids_within_budget(Missing)
    end.

-spec missing_blocked_ids([integer()]) -> [integer()].
missing_blocked_ids(UserIds) ->
    lists:usort(lists:filter(fun is_blocked_ids_cache_miss/1, UserIds)).

-spec is_blocked_ids_cache_miss(integer()) -> boolean().
is_blocked_ids_cache_miss(UserId) ->
    push_ets_cache:get_blocked_ids(UserId) =:= undefined.

-spec fetch_blocked_ids_within_budget([integer()]) -> #{integer() => [integer()]}.
fetch_blocked_ids_within_budget(Missing) ->
    {Budget, ChunkSize} = blocked_ids_fetch_budget(),
    Budgeted = lists:sublist(Missing, Budget),
    count_budget_exhausted(length(Missing) - length(Budgeted)),
    Fill = push_ets_cache:reserve_blocked_ids(Budgeted),
    try
        fetch_blocked_ids_chunks(chunk_user_ids(Budgeted, ChunkSize, []), Fill, #{})
    catch
        Class:Reason -> blocked_ids_fetch_crashed(Class, Reason)
    after
        push_ets_cache:release(Fill)
    end.

-spec blocked_ids_fetch_crashed(throw | error | exit, term()) -> #{}.
blocked_ids_fetch_crashed(Class, Reason) ->
    bump_counter(?CNT_FETCH_FAILURES),
    logger:debug("Push: blocked id fetch crashed", #{class => Class, reason => Reason}),
    #{}.

-spec blocked_ids_fetch_budget() -> {pos_integer(), pos_integer()}.
blocked_ids_fetch_budget() ->
    ChunkSize = blocked_ids_fetch_chunk(),
    MaxUsers = blocked_ids_fetch_max_users(),
    {min(MaxUsers, ChunkSize * ?MAX_FETCH_RPCS), ChunkSize}.

-spec count_budget_exhausted(non_neg_integer()) -> ok.
count_budget_exhausted(0) ->
    ok;
count_budget_exhausted(Skipped) ->
    bump_counter(?CNT_BUDGET_EXHAUSTED, Skipped).

-spec fetch_blocked_ids_chunks(
    [[integer()]], push_ets_cache:fill(), #{integer() => [integer()]}
) -> #{integer() => [integer()]}.
fetch_blocked_ids_chunks([], _Fill, Fetched) ->
    Fetched;
fetch_blocked_ids_chunks([Chunk | Rest], Fill, Fetched) ->
    case fetch_blocked_ids_chunk(Chunk, Fill) of
        {ok, ChunkFetched} ->
            fetch_blocked_ids_chunks(Rest, Fill, maps:merge(Fetched, ChunkFetched));
        error ->
            Fetched
    end.

-spec chunk_user_ids([integer()], pos_integer(), [[integer()]]) -> [[integer()]].
chunk_user_ids([], _ChunkSize, Acc) ->
    lists:reverse(Acc);
chunk_user_ids(UserIds, ChunkSize, Acc) ->
    Chunk = lists:sublist(UserIds, ChunkSize),
    chunk_user_ids(drop_prefix(UserIds, ChunkSize), ChunkSize, [Chunk | Acc]).

-spec drop_prefix([integer()], non_neg_integer()) -> [integer()].
drop_prefix(UserIds, 0) ->
    UserIds;
drop_prefix([], _N) ->
    [];
drop_prefix([_UserId | Rest], N) ->
    drop_prefix(Rest, N - 1).

-spec fetch_blocked_ids_chunk([integer()], push_ets_cache:fill()) ->
    {ok, #{integer() => [integer()]}} | error.
fetch_blocked_ids_chunk([], _Fill) ->
    {ok, #{}};
fetch_blocked_ids_chunk(UserIds, Fill) ->
    bump_counter(?CNT_FETCH_ATTEMPTS),
    Request = #{
        <<"type">> => <<"get_user_blocked_ids">>,
        <<"user_ids">> => [integer_to_binary(UserId) || UserId <- UserIds]
    },
    case rpc_client:call(Request) of
        {ok, Data} ->
            {ok, cache_blocked_ids_response(UserIds, Data, Fill)};
        {error, Reason} ->
            fetch_blocked_ids_failed(Reason, length(UserIds))
    end.

-spec fetch_blocked_ids_failed(term(), non_neg_integer()) -> error.
fetch_blocked_ids_failed(Reason, UserCount) ->
    bump_counter(?CNT_FETCH_FAILURES),
    logger:debug("Push: blocked id fetch failed", #{
        reason => Reason, user_count => UserCount
    }),
    error.

-spec cache_blocked_ids_response([integer()], map(), push_ets_cache:fill()) ->
    #{integer() => [integer()]}.
cache_blocked_ids_response(UserIds, Data, Fill) ->
    maps:from_list([{UserId, cache_blocked_ids_entry(UserId, Data, Fill)} || UserId <- UserIds]).

-spec cache_blocked_ids_entry(integer(), map(), push_ets_cache:fill()) -> [integer()].
cache_blocked_ids_entry(UserId, Data, Fill) ->
    BlockedIds = blocked_ids_from_response(maps:get(integer_to_binary(UserId), Data, [])),
    ok = push_ets_cache:put_blocked_ids_fetched(UserId, BlockedIds, Fill),
    BlockedIds.

-spec blocked_ids_from_response(term()) -> [integer()].
blocked_ids_from_response(Values) when is_list(Values) ->
    lists:filtermap(fun snowflake_id:filter/1, Values);
blocked_ids_from_response(_Values) ->
    [].

-spec blocked_ids_fetch_max_users() -> pos_integer().
blocked_ids_fetch_max_users() ->
    Value = app_pos_integer(push_blocked_ids_fetch_max_users, ?DEFAULT_FETCH_USERS),
    min(Value, ?MAX_FETCH_USERS).

-spec blocked_ids_fetch_chunk() -> pos_integer().
blocked_ids_fetch_chunk() ->
    Value = app_pos_integer(push_blocked_ids_fetch_chunk, ?DEFAULT_FETCH_CHUNK),
    min(max(Value, ?MIN_FETCH_CHUNK), ?MAX_FETCH_CHUNK).

-spec app_pos_integer(atom(), pos_integer()) -> pos_integer().
app_pos_integer(Key, Default) ->
    case application:get_env(fluxer_gateway, Key, undefined) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> Default
    end.

-spec blocked_ids_counters() -> map().
blocked_ids_counters() ->
    #{
        blocked_ids_fetch_attempts => read_counter(?CNT_FETCH_ATTEMPTS),
        blocked_ids_fetch_failures => read_counter(?CNT_FETCH_FAILURES),
        blocked_ids_suppressed => read_counter(?CNT_SUPPRESSED),
        blocked_ids_budget_exhausted => read_counter(?CNT_BUDGET_EXHAUSTED)
    }.

-spec handle_sync_user_blocked_ids(integer(), term(), state()) -> {noreply, state()}.
handle_sync_user_blocked_ids(UserId, BlockedIds, State) ->
    case push_normalize:integer_list(BlockedIds) of
        {ok, TypedBlockedIds} ->
            push_ets_cache:put_blocked_ids(UserId, TypedBlockedIds),
            {noreply, State};
        error ->
            {noreply, State}
    end.

-spec handle_message_create_cast(map(), state()) -> {noreply, state()}.
handle_message_create_cast(Params, State) ->
    SpawnResult = maybe_spawn_push_worker(fun() -> do_handle_message_create(Params) end),
    log_message_worker_drop(SpawnResult, Params),
    {noreply, State}.

-spec handle_clear_channel_notifications(integer(), integer(), integer(), state()) ->
    {noreply, state()}.
handle_clear_channel_notifications(UserId, ChannelId, MessageId, State) ->
    _ = push_job_publisher:publish_clear(UserId, ChannelId, MessageId),
    {noreply, State}.

-spec log_message_worker_drop(ok | dropped, map()) -> ok.
log_message_worker_drop(ok, _Params) ->
    ok;
log_message_worker_drop(dropped, Params) ->
    bump_counter(?CNT_WORKER_POOL),
    MessageData = maps:get(message_data, Params, #{}),
    log_worker_pool_drop(
        loss_logging_enabled(),
        maps:get(<<"id">>, MessageData, undefined),
        maps:get(<<"channel_id">>, MessageData, undefined)
    ).

-spec log_worker_pool_drop(boolean(), term(), term()) -> ok.
log_worker_pool_drop(true, MessageId, ChannelId) ->
    logger:error("Push: worker pool saturated, dropping message create", #{
        message_id => MessageId, channel_id => ChannelId
    });
log_worker_pool_drop(false, MessageId, ChannelId) ->
    logger:debug("Push: worker pool saturated, dropping message create", #{
        message_id => MessageId, channel_id => ChannelId
    }).

-spec cache_stats_with_counters() -> map().
cache_stats_with_counters() ->
    Base = maps:merge(push_ets_cache:cache_stats(), blocked_ids_counters()),
    maps:merge(Base, push_loss_counters()).

-spec push_loss_counters() -> map().
push_loss_counters() ->
    #{
        counters => counter_table_status(),
        worker_pool_dropped => read_counter(?CNT_WORKER_POOL)
    }.

-spec counter_table_status() -> live | unavailable.
counter_table_status() ->
    case ets:info(?PUSH_COUNTER_TABLE, size) of
        Size when is_integer(Size) -> live;
        _ -> unavailable
    end.

-spec loss_logging_enabled() -> boolean().
loss_logging_enabled() ->
    application:get_env(fluxer_gateway, push_loss_logging, false) =:= true.

-spec read_counter(atom()) -> non_neg_integer() | unavailable.
read_counter(Key) ->
    try ets:lookup(?PUSH_COUNTER_TABLE, Key) of
        [{Key, Value}] when is_integer(Value), Value >= 0 -> Value;
        _ -> unavailable
    catch
        error:badarg -> unavailable
    end.

-spec bump_counter(atom()) -> ok.
bump_counter(Key) ->
    bump_counter(Key, 1).

-spec bump_counter(atom(), non_neg_integer()) -> ok.
bump_counter(Key, Increment) ->
    try ets:update_counter(?PUSH_COUNTER_TABLE, Key, {2, Increment}) of
        _Value -> ok
    catch
        error:badarg -> insert_missing_counter(Key, Increment)
    end.

-spec insert_missing_counter(atom(), non_neg_integer()) -> ok.
insert_missing_counter(Key, Increment) ->
    try ets:insert_new(?PUSH_COUNTER_TABLE, {Key, Increment}) of
        true -> ok;
        false -> retry_bump_counter(Key, Increment)
    catch
        error:badarg -> ok
    end.

-spec retry_bump_counter(atom(), non_neg_integer()) -> ok.
retry_bump_counter(Key, Increment) ->
    try ets:update_counter(?PUSH_COUNTER_TABLE, Key, {2, Increment}) of
        _Value -> ok
    catch
        error:badarg -> ok
    end.

-spec local_cache_mutation(fun(() -> ok)) -> ok.
local_cache_mutation(Fun) ->
    case whereis(?MODULE) of
        undefined ->
            ok;
        _Pid ->
            safe_local_cache_mutation(Fun)
    end.

-spec safe_local_cache_mutation(fun(() -> ok)) -> ok.
safe_local_cache_mutation(Fun) ->
    try Fun() of
        ok -> ok
    catch
        error:badarg -> ok
    end.

-spec init_worker_counter() -> ok.
init_worker_counter() ->
    push_worker_pool:init_counter().

-spec maybe_spawn_push_worker(fun(() -> term())) -> ok | dropped.
maybe_spawn_push_worker(Fun) ->
    push_worker_pool:maybe_spawn(Fun).

-spec schedule_eviction() -> reference().
schedule_eviction() ->
    erlang:send_after(?EVICT_INTERVAL_MS, self(), evict_caches).

-spec env_boolean(atom()) -> boolean().
env_boolean(Key) ->
    case fluxer_gateway_env:get(Key) of
        true -> true;
        _ -> false
    end.

-spec env_boolean(atom(), boolean()) -> boolean().
env_boolean(Key, Default) ->
    case fluxer_gateway_env:get(Key) of
        Value when is_boolean(Value) -> Value;
        _ -> Default
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sync_user_guild_settings_local_updates_local_cache_test() ->
    push_ets_cache:init(),
    with_registered_push(fun() ->
        Settings = #{mobile_push => false, message_notifications => 1},
        ok = sync_user_guild_settings_local(10, 20, Settings),
        ?assertEqual(Settings, push_ets_cache:get_user_guild_settings(10, 20))
    end),
    push_ets_cache:delete_user_guild_settings(10, 20).

sync_user_blocked_ids_local_updates_local_cache_test() ->
    push_ets_cache:init(),
    with_registered_push(fun() ->
        ok = sync_user_blocked_ids_local(10, [20, 30]),
        ?assertEqual([20, 30], push_ets_cache:get_blocked_ids(10))
    end).

a_message_publishes_every_eligible_recipient_in_one_call_test() ->
    push_ets_cache:init(),
    lists:foreach(fun(UserId) -> ok = push_ets_cache:put_blocked_ids(UserId, []) end, [51, 52]),
    ok = push_ets_cache:put_blocked_ids(53, [7]),
    Self = self(),
    ok = meck:new(push_job_publisher, [passthrough, no_link]),
    try
        ok = meck:expect(
            push_job_publisher,
            publish_message,
            fun(UserIds, _Data, _Markdown, GuildId, ChannelId, MessageId, _GName, _CName) ->
                Self ! {published, UserIds, GuildId, ChannelId, MessageId},
                ok
            end
        ),
        ok = do_handle_message_create(#{
            message_data => #{
                <<"channel_id">> => <<"123">>,
                <<"id">> => <<"456">>,
                <<"channel_type">> => 1
            },
            user_ids => [51, 52, 53],
            guild_id => 0,
            author_id => 7
        }),
        receive
            {published, UserIds, GuildId, ChannelId, MessageId} ->
                ?assertEqual({[51, 52], 0, 123, 456}, {UserIds, GuildId, ChannelId, MessageId})
        after 2000 -> erlang:error(nothing_published)
        end
    after
        meck:unload(push_job_publisher)
    end.

a_message_without_eligible_recipients_publishes_nothing_test() ->
    push_ets_cache:init(),
    ok = push_ets_cache:put_blocked_ids(61, [7]),
    ok = meck:new(push_job_publisher, [passthrough, no_link]),
    try
        ok = do_handle_message_create(#{
            message_data => #{
                <<"channel_id">> => <<"123">>,
                <<"id">> => <<"456">>,
                <<"channel_type">> => 1
            },
            user_ids => [61],
            guild_id => 0,
            author_id => 7
        }),
        ?assertEqual(0, meck:num_calls(push_job_publisher, publish_message, '_'))
    after
        meck:unload(push_job_publisher)
    end.

filter_eligible_users_fetches_large_metadata_once_test() ->
    push_ets_cache:init(),
    lists:foreach(
        fun(UserId) -> push_ets_cache:put_user_guild_settings(UserId, 42, #{}) end,
        [1, 2, 3]
    ),
    lists:foreach(fun(UserId) -> push_ets_cache:put_blocked_ids(UserId, []) end, [1, 2, 3]),
    Self = self(),
    ok = meck:new(push_eligibility_checks, [passthrough, no_link]),
    try
        ok = meck:expect(push_eligibility_checks, get_guild_large_metadata, fun(42) ->
            Self ! metadata_lookup,
            #{member_count => 3000, features => []}
        end),
        MessageData = #{
            <<"channel_type">> => 0,
            <<"mentions">> => [#{<<"id">> => <<"1">>}]
        },
        ?assertEqual(
            [1],
            filter_eligible_users(
                [1, 2, 3], 999, 42, 10, MessageData, 0, #{}, #{}, undefined
            )
        ),
        ?assertEqual(1, drain_metadata_lookup_count(0))
    after
        meck:unload(push_eligibility_checks),
        lists:foreach(
            fun(UserId) -> push_ets_cache:delete_user_guild_settings(UserId, 42) end,
            [1, 2, 3]
        )
    end.

blocked_ids_fetch_defaults_are_bounded_test() ->
    ?assertEqual(?DEFAULT_FETCH_USERS, blocked_ids_fetch_max_users()),
    ?assertEqual(?DEFAULT_FETCH_CHUNK, blocked_ids_fetch_chunk()).

blocked_ids_from_response_skips_unparsable_entries_test() ->
    ?assertEqual([123, 456], blocked_ids_from_response([<<"123">>, <<"abc">>, 456, null])),
    ?assertEqual([], blocked_ids_from_response(<<"not_a_list">>)).

chunk_user_ids_splits_into_bounded_batches_test() ->
    ?assertEqual([], chunk_user_ids([], 2, [])),
    ?assertEqual([[1, 2], [3, 4], [5]], chunk_user_ids([1, 2, 3, 4, 5], 2, [])),
    ?assertEqual([[1, 2, 3]], chunk_user_ids([1, 2, 3], 500, [])).

blocked_ids_fetch_budget_is_capped_test() ->
    assert_budget_capped(),
    with_fetch_env(1, 1000000, fun assert_budget_capped/0),
    with_fetch_env(1000000, 1000000, fun assert_budget_capped/0),
    assert_budget_capped().

assert_budget_capped() ->
    {Budget, ChunkSize} = blocked_ids_fetch_budget(),
    ?assert(ChunkSize >= ?MIN_FETCH_CHUNK),
    ?assert(ChunkSize =< ?MAX_FETCH_CHUNK),
    ?assert(Budget =< ?MAX_FETCH_USERS),
    ?assert(length(chunk_user_ids(lists:seq(1, Budget), ChunkSize, [])) =< ?MAX_FETCH_RPCS).

synced_blocked_recipients_are_dropped_test() ->
    push_ets_cache:init(),
    ok = push_ets_cache:put_blocked_ids(5020, [999]),
    ?assertEqual([], filter_dm_recipients([5020], 999)).

fetched_blocked_recipients_are_dropped_test() ->
    push_ets_cache:init(),
    ok = seed_fetched_blocked_ids(5031, [999]),
    ?assertEqual([], filter_dm_recipients([5031], 999)).

block_suppression_drops_blocked_recipient_test() ->
    push_ets_cache:init(),
    ok = push_ets_cache:put_blocked_ids(5002, [999]),
    ok = push_ets_cache:put_blocked_ids(5003, []),
    ?assertEqual([5003], filter_dm_recipients([5002, 5003], 999)).

cached_recipients_need_no_blocked_ids_fetch_test() ->
    push_ets_cache:init(),
    ok = seed_fetched_blocked_ids(5004, []),
    ?assertEqual([], missing_blocked_ids([5004])).

blocked_ids_fetch_stops_after_a_failing_chunk_test() ->
    push_ets_cache:init(),
    Fill = push_ets_cache:reserve_blocked_ids([]),
    ?assertEqual(#{}, fetch_blocked_ids_chunks([], Fill, #{})),
    ?assertEqual(#{}, fetch_blocked_ids_chunks([[], []], Fill, #{})),
    ok = push_ets_cache:release(Fill).

blocked_ids_counters_are_unavailable_without_the_shared_table_test() ->
    delete_counter_table(),
    ?assertEqual(unavailable, read_counter(?CNT_SUPPRESSED)).

blocked_ids_counters_are_exposed_in_cache_stats_test() ->
    push_ets_cache:init(),
    with_counter_table(fun() ->
        ok = push_ets_cache:put_blocked_ids(5010, [999]),
        ?assertEqual([], filter_dm_recipients([5010], 999)),
        ?assertEqual(1, read_counter(?CNT_SUPPRESSED)),
        assert_cache_stats_expose_blocked_ids()
    end).

assert_cache_stats_expose_blocked_ids() ->
    Stats = maps:merge(push_ets_cache:cache_stats(), blocked_ids_counters()),
    lists:foreach(
        fun(Key) -> ?assertEqual(true, maps:is_key(Key, Stats)) end,
        [
            blocked_ids_size,
            blocked_ids_fetch_attempts,
            blocked_ids_fetch_failures,
            blocked_ids_suppressed,
            blocked_ids_budget_exhausted
        ]
    ).

push_loss_counters_expose_every_counter_push_writes_test() ->
    with_counter_table(fun() ->
        ok = log_message_worker_drop(dropped, #{}),
        Stats = push_loss_counters(),
        ?assertEqual(live, maps:get(counters, Stats)),
        ?assertEqual(1, maps:get(worker_pool_dropped, Stats))
    end).

push_loss_counters_are_unavailable_without_the_shared_table_test() ->
    delete_counter_table(),
    Stats = push_loss_counters(),
    ?assertEqual(unavailable, maps:get(counters, Stats)),
    ?assertEqual(unavailable, maps:get(worker_pool_dropped, Stats)).

push_loss_counters_keep_a_genuine_zero_distinct_from_absent_test() ->
    with_counter_table(fun() ->
        ?assertEqual(unavailable, maps:get(worker_pool_dropped, push_loss_counters())),
        ok = bump_counter(?CNT_WORKER_POOL, 0),
        ?assertEqual(0, maps:get(worker_pool_dropped, push_loss_counters()))
    end).

cache_stats_with_counters_carries_the_loss_surface_test() ->
    push_ets_cache:init(),
    with_counter_table(fun() ->
        Stats = cache_stats_with_counters(),
        lists:foreach(
            fun(Key) -> ?assertEqual(true, maps:is_key(Key, Stats)) end,
            [blocked_ids_size, blocked_ids_suppressed, counters, worker_pool_dropped]
        )
    end).

with_counter_table(Fun) ->
    delete_counter_table(),
    _ = ets:new(?PUSH_COUNTER_TABLE, [named_table, public, set, {write_concurrency, true}]),
    try
        Fun()
    after
        delete_counter_table()
    end.

delete_counter_table() ->
    try ets:delete(?PUSH_COUNTER_TABLE) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

seed_fetched_blocked_ids(UserId, BlockedIds) ->
    push_ets_cache:put_blocked_ids_fetched(
        UserId, BlockedIds, push_ets_cache:reserve_blocked_ids([UserId])
    ).

filter_dm_recipients(UserIds, AuthorId) ->
    MessageData = #{<<"channel_type">> => 1},
    filter_eligible_users(UserIds, AuthorId, 0, 10, MessageData, 0, #{}, #{}, undefined).

with_fetch_env(ChunkSize, MaxUsers, Fun) ->
    application:set_env(fluxer_gateway, push_blocked_ids_fetch_chunk, ChunkSize),
    application:set_env(fluxer_gateway, push_blocked_ids_fetch_max_users, MaxUsers),
    try
        Fun()
    after
        application:unset_env(fluxer_gateway, push_blocked_ids_fetch_chunk),
        application:unset_env(fluxer_gateway, push_blocked_ids_fetch_max_users)
    end.

filter_eligible_users_batches_missing_settings_lookups_test() ->
    push_ets_cache:init(),
    Self = self(),
    ok = meck:new(rpc_client, [passthrough, no_link]),
    try
        ok = meck:expect(rpc_client, call, fun(Request) ->
            Self ! {rpc_request, Request},
            {ok, #{<<"user_guild_settings">> => [#{}, #{}, #{}]}}
        end),
        ?assertEqual(
            [1, 2, 3],
            filter_eligible_users([1, 2, 3], 999, 43, 10, #{}, 0, #{}, #{}, undefined)
        ),
        ?assertEqual(1, drain_settings_request_count(0))
    after
        meck:unload(rpc_client),
        lists:foreach(
            fun(UserId) -> push_ets_cache:delete_user_guild_settings(UserId, 43) end,
            [1, 2, 3]
        )
    end.

drain_settings_request_count(Count) ->
    receive
        {rpc_request, #{<<"type">> := <<"get_user_guild_settings">>}} ->
            drain_settings_request_count(Count + 1)
    after 0 ->
        Count
    end.

drain_metadata_lookup_count(Count) ->
    receive
        metadata_lookup -> drain_metadata_lookup_count(Count + 1)
    after 0 ->
        Count
    end.

with_registered_push(Fun) ->
    case whereis(?MODULE) of
        undefined ->
            with_new_registered_push(Fun);
        _Pid ->
            Fun()
    end.

with_new_registered_push(Fun) ->
    register(?MODULE, self()),
    try
        Fun()
    after
        unregister(?MODULE)
    end.

-endif.
