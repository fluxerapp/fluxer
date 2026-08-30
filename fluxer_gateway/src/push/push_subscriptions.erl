%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_subscriptions).
-typing([eqwalizer]).

-export([
    fetch_and_send_subscriptions/8,
    fetch_and_send_subscriptions/9,
    fetch_and_send_clear_notification/4
]).
-export([fetch_and_cache_user_guild_settings/2]).
-export([delete_failed_subscriptions/1]).
-export([delivery_concurrency/0]).

-define(DEFAULT_DELIVERY_CONCURRENCY, 8).
-define(DELIVERY_WORKER_TIMEOUT_MS, 30000).
-define(SUBSCRIPTION_FETCH_CHUNK_SIZE, 256).

-spec fetch_and_send_subscriptions(
    [integer()],
    map(),
    integer(),
    integer(),
    integer(),
    binary() | undefined,
    binary() | undefined,
    map()
) -> ok.
fetch_and_send_subscriptions(
    UserIds,
    MessageData,
    GuildId,
    ChannelId,
    MessageId,
    GuildName,
    ChannelName,
    BadgeCounts
) ->
    fetch_and_send_subscriptions(
        UserIds,
        MessageData,
        GuildId,
        ChannelId,
        MessageId,
        GuildName,
        ChannelName,
        #{},
        BadgeCounts
    ).

-spec fetch_and_send_subscriptions(
    [integer()],
    map(),
    integer(),
    integer(),
    integer(),
    binary() | undefined,
    binary() | undefined,
    map(),
    map()
) -> ok.
fetch_and_send_subscriptions(
    UserIds,
    MessageData,
    GuildId,
    ChannelId,
    MessageId,
    GuildName,
    ChannelName,
    MarkdownContext,
    BadgeCounts
) ->
    Ctx = build_send_ctx(
        MessageData,
        GuildId,
        ChannelId,
        MessageId,
        GuildName,
        ChannelName,
        MarkdownContext,
        BadgeCounts
    ),
    {CachedSubscriptions, MissingUserIds} = cached_subscriptions(UserIds),
    send_cached_subscriptions(CachedSubscriptions, Ctx),
    send_missing_if_any(MissingUserIds, Ctx).

-spec send_missing_if_any([integer()], tuple()) -> ok.
send_missing_if_any([], _Ctx) ->
    ok;
send_missing_if_any(MissingUserIds, Ctx) ->
    fetch_and_send_missing_subscriptions(MissingUserIds, Ctx).

-spec fetch_and_send_missing_subscriptions([integer()], tuple()) -> ok.
fetch_and_send_missing_subscriptions(UserIds, Ctx) ->
    {FetchedUserIds, SubscriptionsData} = fetch_missing_subscriptions(UserIds),
    send_fetched_subscriptions(FetchedUserIds, SubscriptionsData, Ctx).

-spec fetch_missing_subscriptions([integer()]) -> {[integer()], map()}.
fetch_missing_subscriptions(UserIds) ->
    Chunks = chunk_user_ids(UserIds, ?SUBSCRIPTION_FETCH_CHUNK_SIZE, []),
    {FetchedChunks, SubscriptionsData} = lists:foldl(
        fun fetch_subscriptions_chunk/2,
        {[], #{}},
        Chunks
    ),
    {lists:append(lists:reverse(FetchedChunks)), SubscriptionsData}.

-spec chunk_user_ids([integer()], pos_integer(), [[integer()]]) -> [[integer()]].
chunk_user_ids([], _ChunkSize, Acc) ->
    lists:reverse(Acc);
chunk_user_ids(UserIds, ChunkSize, Acc) when length(UserIds) =< ChunkSize ->
    lists:reverse([UserIds | Acc]);
chunk_user_ids(UserIds, ChunkSize, Acc) ->
    {Chunk, Rest} = lists:split(ChunkSize, UserIds),
    chunk_user_ids(Rest, ChunkSize, [Chunk | Acc]).

-spec fetch_subscriptions_chunk([integer()], {[[integer()]], map()}) -> {[[integer()]], map()}.
fetch_subscriptions_chunk(UserIds, {FetchedAcc, DataAcc}) ->
    SubscriptionsReq = #{
        <<"type">> => <<"get_push_subscriptions">>,
        <<"user_ids">> => [integer_to_binary(UserId) || UserId <- UserIds]
    },
    logger:debug("Push: fetching subscriptions via RPC", #{user_count => length(UserIds)}),
    case rpc_client:call(SubscriptionsReq) of
        {ok, SubscriptionsData} ->
            logger:debug("Push: RPC returned subscriptions", #{
                user_count => length(UserIds),
                response_keys => maps:keys(SubscriptionsData)
            }),
            {[UserIds | FetchedAcc], maps:merge(DataAcc, SubscriptionsData)};
        {error, Reason} ->
            logger:debug(
                "Push: RPC failed to fetch subscriptions",
                #{user_count => length(UserIds), reason => Reason}
            ),
            {FetchedAcc, DataAcc}
    end.

-spec send_fetched_subscriptions([integer()], map(), tuple()) -> ok.
send_fetched_subscriptions(UserIds, SubscriptionsData, Ctx) ->
    Tasks = lists:foldl(
        fun(UserId, Acc) ->
            add_fetched_user_subscription_task(UserId, SubscriptionsData, Acc)
        end,
        [],
        UserIds
    ),
    send_subscription_tasks(lists:reverse(Tasks), Ctx).

-spec add_fetched_user_subscription_task(integer(), map(), [{integer(), list()}]) ->
    [{integer(), list()}].
add_fetched_user_subscription_task(UserId, SubscriptionsData, Acc) ->
    UserIdBin = integer_to_binary(UserId),
    case maps:get(UserIdBin, SubscriptionsData, []) of
        [] ->
            push_ets_cache:put_subscriptions(UserId, []),
            logger:debug("Push: no subscriptions for user", #{user_id => UserId}),
            Acc;
        Subscriptions ->
            push_ets_cache:put_subscriptions(UserId, Subscriptions),
            logger:debug(
                "Push: found subscriptions for user",
                #{user_id => UserId, count => length(Subscriptions)}
            ),
            [{UserId, Subscriptions} | Acc]
    end.

-spec fetch_and_send_clear_notification(integer(), integer(), integer(), non_neg_integer()) ->
    ok.
fetch_and_send_clear_notification(UserId, ChannelId, MessageId, BadgeCount) ->
    case push_ets_cache:get_subscriptions(UserId) of
        Subscriptions when is_list(Subscriptions) ->
            push_sender:send_clear_to_user_subscriptions(
                UserId,
                Subscriptions,
                ChannelId,
                MessageId,
                BadgeCount
            );
        undefined ->
            fetch_and_send_clear_notification_from_rpc(UserId, ChannelId, MessageId, BadgeCount)
    end.

-spec fetch_and_send_clear_notification_from_rpc(
    integer(), integer(), integer(), non_neg_integer()
) -> ok.
fetch_and_send_clear_notification_from_rpc(UserId, ChannelId, MessageId, BadgeCount) ->
    SubscriptionsReq = #{
        <<"type">> => <<"get_push_subscriptions">>,
        <<"user_ids">> => [integer_to_binary(UserId)]
    },
    logger:debug(
        "Push: fetching subscriptions for notification clear",
        #{user_id => UserId, channel_id => ChannelId, message_id => MessageId}
    ),
    Result = rpc_client:call(SubscriptionsReq),
    send_clear_rpc_result(UserId, ChannelId, MessageId, BadgeCount, Result).

-spec send_clear_rpc_result(
    integer(), integer(), integer(), non_neg_integer(), {ok, map()} | {error, term()}
) -> ok.
send_clear_rpc_result(UserId, ChannelId, MessageId, BadgeCount, {ok, SubscriptionsData}) ->
    UserIdBin = integer_to_binary(UserId),
    send_clear_fetched_subscriptions(
        UserId, ChannelId, MessageId, BadgeCount, maps:get(UserIdBin, SubscriptionsData, [])
    );
send_clear_rpc_result(UserId, _ChannelId, _MessageId, _BadgeCount, {error, Reason}) ->
    logger:debug(
        "Push: RPC failed to fetch subscriptions for notification clear",
        #{user_id => UserId, reason => Reason}
    ),
    ok.

-spec send_clear_fetched_subscriptions(
    integer(), integer(), integer(), non_neg_integer(), list()
) -> ok.
send_clear_fetched_subscriptions(UserId, _ChannelId, _MessageId, _BadgeCount, []) ->
    logger:debug("Push: no subscriptions for notification clear", #{user_id => UserId}),
    ok;
send_clear_fetched_subscriptions(UserId, ChannelId, MessageId, BadgeCount, Subscriptions) ->
    push_ets_cache:put_subscriptions(UserId, Subscriptions),
    push_sender:send_clear_to_user_subscriptions(
        UserId,
        Subscriptions,
        ChannelId,
        MessageId,
        BadgeCount
    ).

-spec cached_subscriptions([integer()]) -> {map(), [integer()]}.
cached_subscriptions(UserIds) ->
    push_ets_cache:get_subscriptions_many(UserIds).

-spec send_cached_subscriptions(map(), tuple()) -> ok.
send_cached_subscriptions(SubscriptionsByUser, Ctx) ->
    Tasks = maps:fold(
        fun add_cached_user_task/3,
        [],
        SubscriptionsByUser
    ),
    send_subscription_tasks(Tasks, Ctx).

-spec add_cached_user_task(integer(), list(), [{integer(), list()}]) ->
    [{integer(), list()}].
add_cached_user_task(UserId, Subscriptions, Acc) ->
    case Subscriptions of
        [] ->
            Acc;
        _ ->
            [{UserId, Subscriptions} | Acc]
    end.

-spec send_subscription_tasks([{integer(), list()}], tuple()) -> ok.
send_subscription_tasks(Tasks, Ctx) ->
    send_subscription_tasks(Tasks, Ctx, delivery_concurrency()).

-spec send_subscription_tasks([{integer(), list()}], tuple(), pos_integer()) -> ok.
send_subscription_tasks([], _Ctx, _MaxConcurrency) ->
    ok;
send_subscription_tasks(Tasks, Ctx, 1) ->
    send_subscription_task_chunk(Tasks, Ctx);
send_subscription_tasks(Tasks, Ctx, MaxConcurrency) ->
    TaskCount = length(Tasks),
    WorkerCount = min(TaskCount, MaxConcurrency),
    ChunkSize = max(1, (TaskCount + WorkerCount - 1) div WorkerCount),
    Chunks = chunk_subscription_tasks(Tasks, ChunkSize, []),
    wait_delivery_workers(start_delivery_workers(Chunks, Ctx, #{})).

-spec delivery_concurrency() -> pos_integer().
delivery_concurrency() ->
    case fluxer_gateway_env:get_optional(push_subscription_delivery_concurrency) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> ?DEFAULT_DELIVERY_CONCURRENCY
    end.

-spec chunk_subscription_tasks([{integer(), list()}], pos_integer(), [[{integer(), list()}]]) ->
    [[{integer(), list()}]].
chunk_subscription_tasks([], _ChunkSize, Acc) ->
    lists:reverse(Acc);
chunk_subscription_tasks(Tasks, ChunkSize, Acc) ->
    {Chunk, Rest} = take_subscription_chunk(Tasks, ChunkSize, []),
    chunk_subscription_tasks(Rest, ChunkSize, [Chunk | Acc]).

-spec take_subscription_chunk([{integer(), list()}], non_neg_integer(), [{integer(), list()}]) ->
    {[{integer(), list()}], [{integer(), list()}]}.
take_subscription_chunk(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
take_subscription_chunk([], _Remaining, Acc) ->
    {lists:reverse(Acc), []};
take_subscription_chunk([Task | Rest], Remaining, Acc) ->
    take_subscription_chunk(Rest, Remaining - 1, [Task | Acc]).

-spec start_delivery_workers([[{integer(), list()}]], tuple(), #{reference() => true}) ->
    #{reference() => true}.
start_delivery_workers([], _Ctx, Refs) ->
    Refs;
start_delivery_workers([Chunk | Rest], Ctx, Refs) ->
    {_Pid, Ref} = spawn_monitor(fun() ->
        send_subscription_task_chunk(Chunk, Ctx)
    end),
    start_delivery_workers(Rest, Ctx, Refs#{Ref => true}).

-spec wait_delivery_workers(#{reference() => true}) -> ok.
wait_delivery_workers(Refs) when map_size(Refs) =:= 0 ->
    ok;
wait_delivery_workers(Refs) ->
    receive
        {'DOWN', Ref, process, _Pid, _Reason} ->
            wait_delivery_workers(maps:remove(Ref, Refs))
    after ?DELIVERY_WORKER_TIMEOUT_MS ->
        logger:warning("Push: timed out waiting for subscription delivery workers", #{
            remaining_workers => map_size(Refs)
        }),
        ok
    end.

-spec send_subscription_task_chunk([{integer(), list()}], tuple()) -> ok.
send_subscription_task_chunk(Tasks, Ctx) ->
    lists:foreach(fun(Task) -> send_subscription_task(Task, Ctx) end, Tasks),
    ok.

-spec send_subscription_task({integer(), list()}, tuple()) -> ok.
send_subscription_task({UserId, Subscriptions}, Ctx) ->
    push_sender:send_to_user_subscriptions(
        UserId, Subscriptions, send_context(UserId, Ctx)
    ).

-spec fetch_and_cache_user_guild_settings(integer(), integer()) -> map() | null.
fetch_and_cache_user_guild_settings(UserId, GuildId) ->
    Req = #{
        <<"type">> => <<"get_user_guild_settings">>,
        <<"user_ids">> => [integer_to_binary(UserId)],
        <<"guild_id">> => integer_to_binary(GuildId)
    },
    logger:debug(
        "Push: fetching user guild settings via RPC",
        #{user_id => UserId, guild_id => GuildId}
    ),
    case rpc_client:call(Req) of
        {ok, Data} ->
            cache_user_guild_settings(UserId, GuildId, Data);
        {error, Reason} ->
            logger:debug(
                "Push: RPC failed to fetch user guild settings",
                #{user_id => UserId, guild_id => GuildId, reason => Reason}
            ),
            null
    end.

-spec cache_user_guild_settings(integer(), integer(), map()) -> map().
cache_user_guild_settings(UserId, GuildId, Data) ->
    SettingsData =
        case maps:get(<<"user_guild_settings">>, Data, [null]) of
            [First | _] -> First;
            _ -> null
        end,
    case SettingsData of
        null ->
            logger:debug(
                "Push: user guild settings returned null; caching empty sentinel",
                #{user_id => UserId, guild_id => GuildId}
            ),
            push_ets_cache:put_user_guild_settings(UserId, GuildId, #{}),
            #{};
        Settings ->
            logger:debug(
                "Push: user guild settings fetched and cached",
                #{
                    user_id => UserId,
                    guild_id => GuildId,
                    muted => maps:get(muted, Settings, undefined),
                    mobile_push => maps:get(mobile_push, Settings, undefined)
                }
            ),
            push_ets_cache:put_user_guild_settings(UserId, GuildId, Settings),
            Settings
    end.

-spec delete_failed_subscriptions([map()]) -> {ok, term()} | {error, term()}.
delete_failed_subscriptions(FailedSubscriptions) ->
    invalidate_failed_subscription_users(FailedSubscriptions),
    DeleteReq = #{
        <<"type">> => <<"delete_push_subscriptions">>,
        <<"subscriptions">> => FailedSubscriptions
    },
    rpc_client:call(DeleteReq).

-spec invalidate_failed_subscription_users([map()]) -> ok.
invalidate_failed_subscription_users(FailedSubscriptions) ->
    UserIds = lists:usort(
        lists:filtermap(fun failed_subscription_user_id/1, FailedSubscriptions)
    ),
    lists:foreach(fun push_ets_cache:delete_subscriptions/1, UserIds),
    ok.

-spec failed_subscription_user_id(map()) -> false | {true, integer()}.
failed_subscription_user_id(#{<<"user_id">> := UserIdBin}) when is_binary(UserIdBin) ->
    snowflake_id:filter(UserIdBin);
failed_subscription_user_id(#{user_id := UserId}) ->
    snowflake_id:filter(UserId);
failed_subscription_user_id(_Subscription) ->
    false.

-spec build_send_ctx(
    map(),
    integer(),
    integer(),
    integer(),
    binary() | undefined,
    binary() | undefined,
    map(),
    map()
) -> tuple().
build_send_ctx(
    MsgData,
    GuildId,
    ChannelId,
    MessageId,
    GuildName,
    ChannelName,
    MarkdownContext,
    BadgeCounts
) ->
    ResolvedMarkdownContext = resolve_markdown_context(MsgData, GuildId, MarkdownContext),
    ContentPreview = push_notification_format:build_content_preview(
        MsgData, ResolvedMarkdownContext
    ),
    {
        MsgData,
        GuildId,
        ChannelId,
        MessageId,
        GuildName,
        ChannelName,
        ResolvedMarkdownContext,
        ContentPreview,
        BadgeCounts
    }.

-spec resolve_markdown_context(map(), integer(), map()) -> map().
resolve_markdown_context(_MsgData, _GuildId, MarkdownContext) when
    is_map(MarkdownContext), map_size(MarkdownContext) > 0
->
    MarkdownContext;
resolve_markdown_context(MsgData, GuildId, _MarkdownContext) ->
    push_notification_format:build_markdown_context(MsgData, GuildId, #{}, #{}).

-spec send_context(integer(), tuple()) -> push_sender:send_context().
send_context(UserId, Ctx) ->
    {
        MsgData,
        GuildId,
        ChannelId,
        MessageId,
        GuildName,
        ChannelName,
        MarkdownContext,
        ContentPreview,
        BadgeCounts
    } = Ctx,
    #{
        message_data => MsgData,
        guild_id => GuildId,
        channel_id => ChannelId,
        message_id => MessageId,
        guild_name => GuildName,
        channel_name => ChannelName,
        markdown_context => MarkdownContext,
        content_preview => ContentPreview,
        badge_count => maps:get(UserId, BadgeCounts, 0)
    }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

send_subscription_tasks_sends_all_users_with_bounded_workers_test() ->
    Self = self(),
    ok = meck:new(push_sender, [passthrough, no_link]),
    try
        ok = meck:expect(push_sender, send_to_user_subscriptions, fun(
            UserId, Subscriptions, SendContext
        ) ->
            Self !
                {sent_subscription_task, UserId, Subscriptions,
                    maps:get(badge_count, SendContext)},
            ok
        end),
        Tasks = [{1, [sub1]}, {2, [sub2]}, {3, [sub3]}],
        ?assertEqual(ok, send_subscription_tasks(Tasks, test_send_ctx(), 2)),
        ?assertEqual(
            [{1, [sub1], 5}, {2, [sub2], 6}, {3, [sub3], 7}],
            lists:sort(collect_sent_subscription_tasks(3, []))
        ),
        ?assert(meck:validate(push_sender))
    after
        meck:unload(push_sender)
    end.

chunk_subscription_tasks_uses_bounded_chunk_size_test() ->
    Tasks = [{1, [a]}, {2, [b]}, {3, [c]}, {4, [d]}, {5, [e]}],
    ?assertEqual(
        [[{1, [a]}, {2, [b]}], [{3, [c]}, {4, [d]}], [{5, [e]}]],
        chunk_subscription_tasks(Tasks, 2, [])
    ).

chunk_user_ids_uses_bounded_chunk_size_test() ->
    ?assertEqual([], chunk_user_ids([], 2, [])),
    ?assertEqual([[1, 2]], chunk_user_ids([1, 2], 2, [])),
    ?assertEqual([[1, 2], [3, 4], [5]], chunk_user_ids([1, 2, 3, 4, 5], 2, [])).

add_fetched_user_subscription_task_caches_empty_result_test() ->
    push_ets_cache:init(),
    push_ets_cache:delete_subscriptions(4242),
    ?assertEqual([], add_fetched_user_subscription_task(4242, #{}, [])),
    ?assertEqual([], push_ets_cache:get_subscriptions(4242)),
    push_ets_cache:delete_subscriptions(4242).

fetch_missing_subscriptions_chunks_and_merges_test() ->
    ok = meck:new(rpc_client, [passthrough, no_link]),
    try
        ok = meck:expect(rpc_client, call, fun subscriptions_chunk_stub/1),
        UserIds = lists:seq(1, ?SUBSCRIPTION_FETCH_CHUNK_SIZE + 2),
        Expected = #{
            <<"1">> => [{chunk, ?SUBSCRIPTION_FETCH_CHUNK_SIZE}],
            integer_to_binary(?SUBSCRIPTION_FETCH_CHUNK_SIZE + 1) => [{chunk, 2}]
        },
        ?assertEqual({UserIds, Expected}, fetch_missing_subscriptions(UserIds)),
        ?assertEqual(2, meck:num_calls(rpc_client, call, '_'))
    after
        meck:unload(rpc_client)
    end.

fetch_missing_subscriptions_keeps_other_chunks_when_one_fails_test() ->
    ok = meck:new(rpc_client, [passthrough, no_link]),
    try
        ok = meck:expect(rpc_client, call, fun failing_first_chunk_stub/1),
        UserIds = lists:seq(1, ?SUBSCRIPTION_FETCH_CHUNK_SIZE + 2),
        TailUserIds = [?SUBSCRIPTION_FETCH_CHUNK_SIZE + 1, ?SUBSCRIPTION_FETCH_CHUNK_SIZE + 2],
        Expected = #{
            integer_to_binary(?SUBSCRIPTION_FETCH_CHUNK_SIZE + 1) => [{chunk, 2}]
        },
        ?assertEqual({TailUserIds, Expected}, fetch_missing_subscriptions(UserIds))
    after
        meck:unload(rpc_client)
    end.

subscriptions_chunk_stub(#{<<"user_ids">> := [FirstUserIdBin | _] = UserIdBins}) ->
    {ok, #{FirstUserIdBin => [{chunk, length(UserIdBins)}]}}.

failing_first_chunk_stub(#{<<"user_ids">> := [<<"1">> | _]}) ->
    {error, timeout};
failing_first_chunk_stub(Request) ->
    subscriptions_chunk_stub(Request).

collect_sent_subscription_tasks(0, Acc) ->
    Acc;
collect_sent_subscription_tasks(Count, Acc) ->
    receive
        {sent_subscription_task, UserId, Subscriptions, BadgeCount} ->
            collect_sent_subscription_tasks(
                Count - 1, [{UserId, Subscriptions, BadgeCount} | Acc]
            )
    after 1000 ->
        ?assert(false)
    end.

test_send_ctx() ->
    {
        #{<<"content">> => <<"hello">>},
        10,
        20,
        30,
        <<"guild">>,
        <<"channel">>,
        #{},
        <<"hello">>,
        #{1 => 5, 2 => 6, 3 => 7}
    }.

-endif.
