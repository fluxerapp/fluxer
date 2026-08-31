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
    invalidate_user_subscriptions/1,
    invalidate_user_subscriptions_local/1,
    invalidate_user_badge_count/1,
    invalidate_user_badge_count_local/1,
    invalidate_user_badge_counts_local/1,
    clear_channel_notifications/3
]).
-export([get_cache_stats/0]).
-export([push_owner_key/1]).

-define(EVICT_INTERVAL_MS, 60000).
-define(DEFAULT_MAX_ENTRIES, 500000).

-type state() :: #{
    badge_counts_ttl_seconds := non_neg_integer(),
    max_entries := non_neg_integer()
}.
-type worker_state() :: #{
    badge_counts_ttl_seconds := non_neg_integer()
}.

-spec start_link() -> {ok, pid()} | {error, term()} | ignore.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:process_flag(fullsweep_after, 10),
    push_ets_cache:init(),
    init_worker_counter(),
    PushEnabled = env_boolean(push_enabled),
    maybe_warn_vapid_misconfigured(PushEnabled),
    case PushEnabled of
        true ->
            BcTtl = env_non_neg_integer(push_badge_counts_cache_ttl_seconds, 0),
            schedule_eviction(),
            {ok, #{
                badge_counts_ttl_seconds => BcTtl,
                max_entries => ?DEFAULT_MAX_ENTRIES
            }};
        false ->
            {ok, #{
                badge_counts_ttl_seconds => 0,
                max_entries => ?DEFAULT_MAX_ENTRIES
            }}
    end.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call(get_cache_stats, _From, State) ->
    Stats = push_ets_cache:cache_stats(),
    {reply, {ok, Stats}, State};
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
handle_cast({invalidate_user_subscriptions, UserId}, State) when is_integer(UserId) ->
    push_ets_cache:delete_subscriptions(UserId),
    {noreply, State};
handle_cast({cache_user_guild_settings, UserId, GuildId, Settings}, State) when
    is_integer(UserId), is_integer(GuildId), is_map(Settings)
->
    push_ets_cache:put_user_guild_settings(UserId, GuildId, Settings),
    {noreply, State};
handle_cast({invalidate_user_badge_count, UserId}, State) when is_integer(UserId) ->
    push_ets_cache:delete_badge_count(UserId),
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
        subscriptions => MaxEntries,
        blocked_ids => MaxEntries,
        badge_counts => MaxEntries
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

-spec put_blocked_ids_local(integer(), [integer()]) -> ok.
put_blocked_ids_local(UserId, TypedBlockedIds) ->
    local_cache_mutation(fun() ->
        push_ets_cache:put_blocked_ids(UserId, TypedBlockedIds)
    end).

-spec invalidate_user_subscriptions(integer()) -> ok.
invalidate_user_subscriptions(UserId) ->
    maybe_cast(UserId, {invalidate_user_subscriptions, UserId}).

-spec invalidate_user_subscriptions_local(integer()) -> ok.
invalidate_user_subscriptions_local(UserId) ->
    local_cache_mutation(fun() ->
        push_ets_cache:delete_subscriptions(UserId)
    end).

-spec invalidate_user_badge_count(integer()) -> ok.
invalidate_user_badge_count(UserId) ->
    maybe_cast(UserId, {invalidate_user_badge_count, UserId}).

-spec invalidate_user_badge_count_local(integer()) -> ok.
invalidate_user_badge_count_local(UserId) ->
    local_cache_mutation(fun() ->
        push_ets_cache:delete_badge_count(UserId)
    end).

-spec invalidate_user_badge_counts_local(term()) -> ok.
invalidate_user_badge_counts_local(UserIds) ->
    case push_normalize:integer_list(UserIds) of
        {ok, TypedUserIds} ->
            lists:foreach(fun invalidate_user_badge_count_local/1, TypedUserIds);
        error ->
            ok
    end.

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
    case
        is_push_active() andalso persistent_term:get(push_clear_notifications_enabled, false)
    of
        true ->
            cast_to_push_owner(
                UserId, {clear_channel_notifications, UserId, ChannelId, MessageId}
            );
        false ->
            ok
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

-spec do_handle_message_create(map(), worker_state()) -> ok.
do_handle_message_create(Params, State) ->
    case push_message_params:context(Params) of
        {ok, Context} ->
            do_handle_message_create_context(Context, State);
        {error, Reason} ->
            logger:debug("Push: skipping malformed message create", #{reason => Reason}),
            ok
    end.

-spec do_handle_message_create_context(push_message_params:context(), worker_state()) -> ok.
do_handle_message_create_context(Context, State) ->
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
        ConnectedUsers
    ),
    logger:debug("Push: eligibility result", #{
        message_id => MessageId,
        channel_id => ChannelId,
        eligible_count => length(EligibleUsers),
        eligible_user_ids => EligibleUsers
    }),
    dispatch_if_eligible(
        EligibleUsers,
        MessageData,
        MarkdownContext,
        GuildId,
        ChannelId,
        MessageId,
        GuildName,
        ChannelName,
        State
    ).

-spec filter_eligible_users(
    [integer()], integer(), integer(), integer(), map(), integer(), map(), map()
) -> [integer()].
filter_eligible_users(
    UserIds,
    AuthorId,
    GuildId,
    ChannelId,
    MessageData,
    GuildDefaultNotifications,
    UserRolesMap,
    ConnectedUsers
) ->
    LargeGuildMetadata = large_guild_metadata(GuildId),
    push_eligibility:prefetch_user_guild_settings(UserIds, AuthorId, GuildId),
    lists:filter(
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
        UserIds
    ).

-spec large_guild_metadata(integer()) -> map() | undefined.
large_guild_metadata(0) ->
    undefined;
large_guild_metadata(GuildId) ->
    push_eligibility_checks:get_guild_large_metadata(GuildId).

-spec dispatch_if_eligible(
    [integer()],
    map(),
    map(),
    integer(),
    integer(),
    integer(),
    binary() | undefined,
    binary() | undefined,
    worker_state()
) -> ok.
dispatch_if_eligible(
    [],
    _MessageData,
    _MarkdownContext,
    _GuildId,
    _ChannelId,
    _MessageId,
    _GuildName,
    _ChannelName,
    _State
) ->
    ok;
dispatch_if_eligible(
    EligibleUsers,
    MessageData,
    MarkdownContext,
    GuildId,
    ChannelId,
    MessageId,
    GuildName,
    ChannelName,
    State
) ->
    BadgeCountsTtl = maps:get(badge_counts_ttl_seconds, State),
    case
        push_dispatcher:enqueue_send_notifications(
            EligibleUsers,
            MessageData,
            MarkdownContext,
            GuildId,
            ChannelId,
            MessageId,
            GuildName,
            ChannelName,
            BadgeCountsTtl
        )
    of
        ok ->
            ok;
        dropped ->
            logger:debug("Push: dispatcher saturated, dropping notification job", #{
                message_id => MessageId,
                channel_id => ChannelId,
                guild_id => GuildId,
                eligible_count => length(EligibleUsers)
            }),
            ok
    end.

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
    BadgeCountsTtl = maps:get(badge_counts_ttl_seconds, State),
    WorkerState = #{badge_counts_ttl_seconds => BadgeCountsTtl},
    SpawnResult = maybe_spawn_push_worker(fun() ->
        do_handle_message_create(Params, WorkerState)
    end),
    log_message_worker_drop(SpawnResult, Params),
    {noreply, State}.

-spec handle_clear_channel_notifications(integer(), integer(), integer(), state()) ->
    {noreply, state()}.
handle_clear_channel_notifications(UserId, ChannelId, MessageId, State) ->
    BadgeCountsTtl = maps:get(badge_counts_ttl_seconds, State),
    case
        push_dispatcher:enqueue_clear_notifications(
            UserId, ChannelId, MessageId, BadgeCountsTtl
        )
    of
        ok ->
            ok;
        dropped ->
            logger:debug("Push: dispatcher saturated, dropping clear notification job", #{
                user_id => UserId, channel_id => ChannelId, message_id => MessageId
            })
    end,
    {noreply, State}.

-spec log_message_worker_drop(ok | dropped, map()) -> ok.
log_message_worker_drop(ok, _Params) ->
    ok;
log_message_worker_drop(dropped, Params) ->
    MessageData = maps:get(message_data, Params, #{}),
    logger:debug("Push: worker pool saturated, dropping message create", #{
        message_id => maps:get(<<"id">>, MessageData, undefined),
        channel_id => maps:get(<<"channel_id">>, MessageData, undefined)
    }).

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

-spec maybe_warn_vapid_misconfigured(boolean()) -> ok.
maybe_warn_vapid_misconfigured(true) ->
    Public = fluxer_gateway_env:get(vapid_public_key),
    Private = fluxer_gateway_env:get(vapid_private_key),
    case
        is_binary(Public) andalso is_binary(Private) andalso
            byte_size(Public) > 0 andalso byte_size(Private) > 0
    of
        true ->
            ok;
        false ->
            logger:error(
                "Push: push_enabled=true but VAPID keys are missing or empty; "
                "all web push notifications will be silently dropped"
            ),
            ok
    end;
maybe_warn_vapid_misconfigured(_) ->
    ok.

-spec env_boolean(atom()) -> boolean().
env_boolean(Key) ->
    case fluxer_gateway_env:get(Key) of
        true -> true;
        _ -> false
    end.

-spec env_non_neg_integer(atom(), non_neg_integer()) -> non_neg_integer().
env_non_neg_integer(Key, Default) ->
    case fluxer_gateway_env:get(Key) of
        Value when is_integer(Value), Value >= 0 -> Value;
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

invalidate_user_badge_counts_local_deletes_every_cached_entry_test() ->
    push_ets_cache:init(),
    push_ets_cache:put_badge_count(10, 5, 1000),
    push_ets_cache:put_badge_count(11, 7, 1000),
    with_registered_push(fun() ->
        ok = invalidate_user_badge_counts_local([10, 11])
    end),
    ?assertEqual(undefined, push_ets_cache:get_badge_count(10)),
    ?assertEqual(undefined, push_ets_cache:get_badge_count(11)).

invalidate_user_badge_counts_local_ignores_untyped_ids_test() ->
    push_ets_cache:init(),
    push_ets_cache:put_badge_count(12, 5, 1000),
    with_registered_push(fun() ->
        ok = invalidate_user_badge_counts_local([<<"12">>])
    end),
    ?assertEqual({5, 1000}, push_ets_cache:get_badge_count(12)),
    push_ets_cache:delete_badge_count(12).

invalidate_user_subscriptions_local_deletes_local_cache_test() ->
    push_ets_cache:init(),
    push_ets_cache:put_subscriptions(10, [#{<<"endpoint">> => <<"test">>}]),
    with_registered_push(fun() ->
        ok = invalidate_user_subscriptions_local(10),
        ?assertEqual(undefined, push_ets_cache:get_subscriptions(10))
    end).

filter_eligible_users_fetches_large_metadata_once_test() ->
    push_ets_cache:init(),
    lists:foreach(
        fun(UserId) -> push_ets_cache:put_user_guild_settings(UserId, 42, #{}) end,
        [1, 2, 3]
    ),
    Self = self(),
    ok = meck:new(push_eligibility_checks, [passthrough, no_link]),
    try
        ok = meck:expect(push_eligibility_checks, get_guild_large_metadata, fun(42) ->
            Self ! metadata_lookup,
            #{member_count => 300, features => []}
        end),
        MessageData = #{
            <<"channel_type">> => 0,
            <<"mentions">> => [#{<<"id">> => <<"1">>}]
        },
        ?assertEqual(
            [1],
            filter_eligible_users([1, 2, 3], 999, 42, 10, MessageData, 0, #{}, #{})
        ),
        ?assertEqual(1, drain_metadata_lookup_count(0))
    after
        meck:unload(push_eligibility_checks),
        lists:foreach(
            fun(UserId) -> push_ets_cache:delete_user_guild_settings(UserId, 42) end,
            [1, 2, 3]
        )
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
            filter_eligible_users([1, 2, 3], 999, 43, 10, #{}, 0, #{}, #{})
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
