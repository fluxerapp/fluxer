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
-export([delivery_stats/0]).

-define(DEFAULT_DELIVERY_CONCURRENCY, 8).
-define(DEFAULT_DELIVERY_MAX_WORKERS, 64).
-define(DEFAULT_DELIVERY_TASKS_PER_WORKER, 128).
-define(DEFAULT_SUBSCRIPTION_FETCH_BATCH, 2000).
-define(SUBSCRIPTION_FETCH_MAX_CONSECUTIVE_FAILURES, 3).
-define(DEFAULT_SUBSCRIPTION_FETCH_BUDGET_MS, 120000).
-define(DELIVERY_IDLE_TIMEOUT_MS, 30000).
-define(PUSH_COUNTERS, push_worker_counter).
-define(CNT_LIVE_WORKERS, delivery_live_workers).
-define(CTX_BADGE_COUNTS, 9).

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
    Batches = chunk_user_ids(UserIds, subscription_fetch_batch_size(), []),
    logger:debug("Push: fetching subscriptions via batched RPC", #{
        user_count => length(UserIds), batch_count => length(Batches)
    }),
    {Tasks, FailedBatches, FailedUsers, _Consecutive} =
        fetch_subscription_batches(Batches, {[], 0, 0, 0}),
    report_subscription_fetch_failures(FailedBatches, FailedUsers, Ctx),
    deliver_subscription_tasks(lists:reverse(Tasks), Ctx).

-type subscription_batch_acc() ::
    {[{integer(), list()}], non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-spec fetch_subscription_batches([[integer()]], subscription_batch_acc()) ->
    subscription_batch_acc().
fetch_subscription_batches(Batches, Acc) ->
    Deadline = erlang:monotonic_time(millisecond) + subscription_fetch_budget_ms(),
    fetch_subscription_batches(Batches, Acc, Deadline).

-spec fetch_subscription_batches([[integer()]], subscription_batch_acc(), integer()) ->
    subscription_batch_acc().
fetch_subscription_batches([], Acc, _Deadline) ->
    Acc;
fetch_subscription_batches(Remaining, Acc, Deadline) ->
    case subscription_fetch_exhausted(Acc, Deadline) of
        true -> abandon_subscription_batches(Remaining, Acc);
        false -> fetch_next_subscription_batch(Remaining, Acc, Deadline)
    end.

-spec subscription_fetch_exhausted(subscription_batch_acc(), integer()) -> boolean().
subscription_fetch_exhausted({_Tasks, _FailedBatches, _FailedUsers, Consecutive}, Deadline) ->
    Consecutive >= ?SUBSCRIPTION_FETCH_MAX_CONSECUTIVE_FAILURES orelse
        erlang:monotonic_time(millisecond) >= Deadline.

-spec abandon_subscription_batches([[integer()]], subscription_batch_acc()) ->
    subscription_batch_acc().
abandon_subscription_batches(Remaining, {Tasks, FailedBatches, FailedUsers, Consecutive}) ->
    {Tasks, FailedBatches + length(Remaining), FailedUsers + batched_user_count(Remaining, 0),
        Consecutive}.

-spec fetch_next_subscription_batch([[integer()]], subscription_batch_acc(), integer()) ->
    subscription_batch_acc().
fetch_next_subscription_batch(
    [Batch | Rest], {Tasks, FailedBatches, FailedUsers, Consecutive}, Deadline
) ->
    Req = #{
        <<"type">> => <<"get_push_subscriptions">>,
        <<"user_ids">> => [integer_to_binary(UserId) || UserId <- Batch]
    },
    case rpc_client:call(Req) of
        {ok, BatchData} ->
            BatchTasks = lists:foldl(
                fun(UserId, Acc) ->
                    add_fetched_user_subscription_task(UserId, BatchData, Acc)
                end,
                Tasks,
                Batch
            ),
            fetch_subscription_batches(
                Rest, {BatchTasks, FailedBatches, FailedUsers, 0}, Deadline
            );
        {error, Reason} ->
            logger:debug(
                "Push: RPC failed to fetch subscriptions",
                #{user_count => length(Batch), reason => Reason}
            ),
            fetch_subscription_batches(
                Rest,
                {Tasks, FailedBatches + 1, FailedUsers + length(Batch), Consecutive + 1},
                Deadline
            )
    end.

-spec batched_user_count([[integer()]], non_neg_integer()) -> non_neg_integer().
batched_user_count([], Acc) ->
    Acc;
batched_user_count([Batch | Rest], Acc) ->
    batched_user_count(Rest, Acc + length(Batch)).

-spec report_subscription_fetch_failures(non_neg_integer(), non_neg_integer(), tuple()) -> ok.
report_subscription_fetch_failures(0, _FailedUsers, _Ctx) ->
    ok;
report_subscription_fetch_failures(FailedBatches, FailedUsers, Ctx) ->
    bump_counter(subscription_fetch_calls_failed, FailedBatches),
    bump_counter(subscription_fetch_users_dropped, FailedUsers),
    {GuildId, MessageId} = delivery_ids(Ctx),
    logger:warning("Push: subscription fetch batches failed; recipients not attempted", #{
        guild_id => GuildId,
        message_id => MessageId,
        failed_batches => FailedBatches,
        recipients_not_attempted => FailedUsers
    }).

-spec chunk_user_ids([integer()], pos_integer(), [[integer()]]) -> [[integer()]].
chunk_user_ids([], _BatchSize, Acc) ->
    lists:reverse(Acc);
chunk_user_ids(UserIds, BatchSize, Acc) ->
    {Batch, Rest} = take_user_id_batch(UserIds, BatchSize, []),
    chunk_user_ids(Rest, BatchSize, [Batch | Acc]).

-spec take_user_id_batch([integer()], non_neg_integer(), [integer()]) ->
    {[integer()], [integer()]}.
take_user_id_batch(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
take_user_id_batch([], _Remaining, Acc) ->
    {lists:reverse(Acc), []};
take_user_id_batch([UserId | Rest], Remaining, Acc) ->
    take_user_id_batch(Rest, Remaining - 1, [UserId | Acc]).

-spec count_subscription_fetch_loss(non_neg_integer()) -> ok.
count_subscription_fetch_loss(UserCount) ->
    bump_counter(subscription_fetch_calls_failed, 1),
    bump_counter(subscription_fetch_users_dropped, UserCount).

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
    count_subscription_fetch_loss(1),
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
    deliver_subscription_tasks(Tasks, Ctx).

-spec add_cached_user_task(integer(), list(), [{integer(), list()}]) ->
    [{integer(), list()}].
add_cached_user_task(UserId, Subscriptions, Acc) ->
    case Subscriptions of
        [] ->
            Acc;
        _ ->
            [{UserId, Subscriptions} | Acc]
    end.

-spec deliver_subscription_tasks([{integer(), list()}], tuple()) -> ok.
deliver_subscription_tasks(Tasks, Ctx) ->
    send_bounded_subscription_tasks(Tasks, Ctx).

-spec send_bounded_subscription_tasks([{integer(), list()}], tuple()) -> ok.
send_bounded_subscription_tasks([], _Ctx) ->
    ok;
send_bounded_subscription_tasks(Tasks, Ctx) ->
    TaskCount = length(Tasks),
    Desired = bounded_worker_count(TaskCount, delivery_concurrency(), delivery_max_workers()),
    Granted = claim_delivery_slots(Desired, gateway_http_client:push_max_concurrency()),
    try
        run_claimed_delivery(Tasks, TaskCount, Ctx, Granted)
    after
        release_delivery_slots(Granted)
    end.

-spec run_claimed_delivery([{integer(), list()}], pos_integer(), tuple(), non_neg_integer()) ->
    ok.
run_claimed_delivery(Tasks, _TaskCount, Ctx, 0) ->
    bump_counter(delivery_budget_exhausted, 1),
    run_inline_delivery(Tasks, Ctx);
run_claimed_delivery(Tasks, TaskCount, Ctx, WorkerCount) ->
    ChunkSize = max(1, (TaskCount + WorkerCount - 1) div WorkerCount),
    Chunks = chunk_subscription_tasks(Tasks, ChunkSize, []),
    Workers = start_progress_workers(
        Chunks, shared_ctx_payload(Ctx), ctx_badge_counts(Ctx), self(), #{}
    ),
    await_progress_workers(Workers, 0, TaskCount, delivery_ids(Ctx)).

-spec run_inline_delivery([{integer(), list()}], tuple()) -> ok.
run_inline_delivery([], _Ctx) ->
    ok;
run_inline_delivery([Task | Rest], Ctx) ->
    run_subscription_task(Task, Ctx),
    run_inline_delivery(Rest, Ctx).

-spec claim_delivery_slots(pos_integer(), pos_integer()) -> non_neg_integer().
claim_delivery_slots(Desired, Budget) ->
    try ets:update_counter(?PUSH_COUNTERS, ?CNT_LIVE_WORKERS, {2, Desired}) of
        Reserved -> settle_delivery_claim(Desired, Budget, Reserved)
    catch
        error:badarg -> claim_first_delivery_slots(Desired, Budget)
    end.

-spec claim_first_delivery_slots(pos_integer(), pos_integer()) -> non_neg_integer().
claim_first_delivery_slots(Desired, Budget) ->
    try ets:insert_new(?PUSH_COUNTERS, {?CNT_LIVE_WORKERS, 0}) of
        _Inserted -> retry_claim_delivery_slots(Desired, Budget)
    catch
        error:badarg -> 0
    end.

-spec retry_claim_delivery_slots(pos_integer(), pos_integer()) -> non_neg_integer().
retry_claim_delivery_slots(Desired, Budget) ->
    try ets:update_counter(?PUSH_COUNTERS, ?CNT_LIVE_WORKERS, {2, Desired}) of
        Reserved -> settle_delivery_claim(Desired, Budget, Reserved)
    catch
        error:badarg -> 0
    end.

-spec settle_delivery_claim(pos_integer(), pos_integer(), integer()) -> non_neg_integer().
settle_delivery_claim(Desired, Budget, Reserved) ->
    Granted = min(Desired, max(0, Budget - (Reserved - Desired))),
    release_delivery_slots(Desired - Granted),
    Granted.

-spec release_delivery_slots(non_neg_integer()) -> ok.
release_delivery_slots(0) ->
    ok;
release_delivery_slots(Count) ->
    try ets:update_counter(?PUSH_COUNTERS, ?CNT_LIVE_WORKERS, {2, -Count, 0, 0}) of
        _Value -> ok
    catch
        error:badarg -> ok
    end.

-spec bounded_worker_count(pos_integer(), pos_integer(), pos_integer()) -> pos_integer().
bounded_worker_count(TaskCount, Base, Bound) ->
    TasksPerWorker = delivery_tasks_per_worker(),
    Sized = (TaskCount + TasksPerWorker - 1) div TasksPerWorker,
    min(TaskCount, min(Bound, max(Base, Sized))).

-type delivery_workers() :: #{pid() => {reference(), non_neg_integer()}}.

-spec start_progress_workers(
    [[{integer(), list()}]], binary(), map(), pid(), delivery_workers()
) -> delivery_workers().
start_progress_workers([], _Shared, _BadgeCounts, _Coordinator, Workers) ->
    Workers;
start_progress_workers([Chunk | Rest], Shared, BadgeCounts, Coordinator, Workers) ->
    ChunkSize = length(Chunk),
    ChunkBadges = chunk_badge_counts(Chunk, BadgeCounts),
    bump_counter(delivery_workers_spawned, 1),
    {Pid, Ref} = spawn_monitor(fun() ->
        run_progress_chunk(Chunk, restore_chunk_ctx(Shared, ChunkBadges), Coordinator)
    end),
    start_progress_workers(
        Rest, Shared, BadgeCounts, Coordinator, Workers#{Pid => {Ref, ChunkSize}}
    ).

-spec shared_ctx_payload(tuple()) -> binary().
shared_ctx_payload(Ctx) ->
    term_to_binary(setelement(?CTX_BADGE_COUNTS, Ctx, #{})).

-spec restore_chunk_ctx(binary(), map()) -> tuple().
restore_chunk_ctx(Shared, BadgeCounts) ->
    case binary_to_term(Shared) of
        Ctx when is_tuple(Ctx) -> setelement(?CTX_BADGE_COUNTS, Ctx, BadgeCounts)
    end.

-spec chunk_badge_counts([{integer(), list()}], map()) -> map().
chunk_badge_counts(Chunk, BadgeCounts) ->
    maps:with([UserId || {UserId, _Subscriptions} <- Chunk], BadgeCounts).

-spec ctx_badge_counts(tuple()) -> map().
ctx_badge_counts(Ctx) ->
    {
        _MsgData,
        _GuildId,
        _ChannelId,
        _MessageId,
        _GuildName,
        _ChannelName,
        _MarkdownContext,
        _ContentPreview,
        BadgeCounts
    } = Ctx,
    BadgeCounts.

-spec run_progress_chunk([{integer(), list()}], tuple(), pid()) -> ok.
run_progress_chunk([], _Ctx, _Coordinator) ->
    ok;
run_progress_chunk([Task | Rest], Ctx, Coordinator) ->
    run_subscription_task(Task, Ctx),
    Coordinator ! {push_delivery_progress, self()},
    run_progress_chunk(Rest, Ctx, Coordinator).

-spec run_subscription_task({integer(), list()}, tuple()) -> ok.
run_subscription_task({UserId, _Subscriptions} = Task, Ctx) ->
    try
        send_subscription_task(Task, Ctx)
    catch
        Class:Reason ->
            bump_counter(delivery_task_failures, 1),
            logger:warning("Push: delivery task raised", #{
                user_id => UserId, class => Class, reason => Reason
            })
    end,
    ok.

-spec await_progress_workers(
    delivery_workers(), non_neg_integer(), non_neg_integer(), {integer(), integer()}
) -> ok.
await_progress_workers(Workers, Unattempted, TaskCount, Ids) when map_size(Workers) =:= 0 ->
    report_unattempted_recipients(Unattempted, TaskCount, Ids);
await_progress_workers(Workers, Unattempted, TaskCount, Ids) ->
    receive
        {push_delivery_progress, Pid} ->
            await_progress_workers(record_task_done(Pid, Workers), Unattempted, TaskCount, Ids);
        {'DOWN', _Ref, process, Pid, normal} ->
            await_progress_workers(maps:remove(Pid, Workers), Unattempted, TaskCount, Ids);
        {'DOWN', _Ref, process, Pid, _Reason} ->
            {Lost, Rest} = discard_crashed_worker(Pid, Workers),
            await_progress_workers(Rest, Unattempted + Lost, TaskCount, Ids)
    after ?DELIVERY_IDLE_TIMEOUT_MS ->
        report_stalled_delivery(Workers, Unattempted, TaskCount, Ids)
    end.

-spec record_task_done(pid(), delivery_workers()) -> delivery_workers().
record_task_done(Pid, Workers) ->
    case Workers of
        #{Pid := {Ref, Left}} -> Workers#{Pid := {Ref, max(0, Left - 1)}};
        _ -> Workers
    end.

-spec discard_crashed_worker(pid(), delivery_workers()) ->
    {non_neg_integer(), delivery_workers()}.
discard_crashed_worker(Pid, Workers) ->
    case maps:take(Pid, Workers) of
        {{_Ref, Left}, Rest} ->
            bump_counter(delivery_worker_crashes, 1),
            bump_counter(delivery_recipients_not_attempted, Left),
            {Left, Rest};
        error ->
            {0, Workers}
    end.

-spec report_unattempted_recipients(
    non_neg_integer(), non_neg_integer(), {integer(), integer()}
) -> ok.
report_unattempted_recipients(0, _TaskCount, _Ids) ->
    ok;
report_unattempted_recipients(Unattempted, TaskCount, {GuildId, MessageId}) ->
    logger:warning("Push: delivery worker died with recipients unattempted", #{
        guild_id => GuildId,
        message_id => MessageId,
        recipients_total => TaskCount,
        recipients_not_attempted => Unattempted
    }).

-spec report_stalled_delivery(
    delivery_workers(), non_neg_integer(), non_neg_integer(), {integer(), integer()}
) -> ok.
report_stalled_delivery(Workers, Unattempted, TaskCount, {GuildId, MessageId}) ->
    Abandoned = map_size(Workers),
    Stranded = abandon_delivery_workers(maps:to_list(Workers), 0),
    bump_counter(delivery_workers_abandoned, Abandoned),
    bump_counter(delivery_recipients_unconfirmed, Stranded),
    logger:error("Push: subscription delivery stalled with recipients unattempted", #{
        guild_id => GuildId,
        message_id => MessageId,
        recipients_total => TaskCount,
        recipients_not_attempted => Unattempted,
        recipients_unconfirmed => Stranded,
        workers_still_running => Abandoned,
        idle_timeout_ms => ?DELIVERY_IDLE_TIMEOUT_MS
    }).

-spec abandon_delivery_workers(
    [{pid(), {reference(), non_neg_integer()}}], non_neg_integer()
) -> non_neg_integer().
abandon_delivery_workers([], Stranded) ->
    Stranded;
abandon_delivery_workers([{_Pid, {Ref, Left}} | Rest], Stranded) ->
    _ = erlang:demonitor(Ref, [flush]),
    abandon_delivery_workers(Rest, trunc(Stranded + Left)).

-spec delivery_ids(tuple()) -> {integer(), integer()}.
delivery_ids(Ctx) ->
    {
        _MsgData,
        GuildId,
        _ChannelId,
        MessageId,
        _GuildName,
        _ChannelName,
        _MarkdownContext,
        _ContentPreview,
        _BadgeCounts
    } = Ctx,
    {GuildId, MessageId}.

-spec delivery_stats() -> #{atom() => term()}.
delivery_stats() ->
    Counters = maps:from_list([{Key, counter_value(Key)} || Key <- delivery_counter_keys()]),
    Counters#{delivery_last_crash => last_delivery_crash()}.

-spec delivery_counter_keys() -> [atom()].
delivery_counter_keys() ->
    [
        ?CNT_LIVE_WORKERS,
        delivery_workers_spawned,
        delivery_worker_crashes,
        delivery_workers_abandoned,
        delivery_recipients_not_attempted,
        delivery_recipients_unconfirmed,
        delivery_task_failures,
        delivery_budget_exhausted,
        subscription_fetch_calls_failed,
        subscription_fetch_users_dropped,
        badge_fetch_calls_failed,
        badge_fetch_users_defaulted
    ].

-spec counter_value(atom()) -> non_neg_integer().
counter_value(Key) ->
    try ets:lookup(?PUSH_COUNTERS, Key) of
        [{Key, Value}] when is_integer(Value), Value >= 0 -> Value;
        _ -> 0
    catch
        error:badarg -> 0
    end.

-spec last_delivery_crash() -> term().
last_delivery_crash() ->
    try ets:lookup(?PUSH_COUNTERS, delivery_last_crash) of
        [{delivery_last_crash, Summary}] -> Summary;
        _ -> none
    catch
        error:badarg -> none
    end.

-spec bump_counter(atom(), integer()) -> ok.
bump_counter(Key, Delta) ->
    try
        _ = ets:update_counter(?PUSH_COUNTERS, Key, {2, Delta}),
        ok
    catch
        error:badarg -> seed_and_bump_counter(Key, Delta)
    end.

-spec seed_and_bump_counter(atom(), integer()) -> ok.
seed_and_bump_counter(Key, Delta) ->
    try
        _ = ets:insert_new(?PUSH_COUNTERS, {Key, 0}),
        _ = ets:update_counter(?PUSH_COUNTERS, Key, {2, Delta}),
        ok
    catch
        error:badarg -> ok
    end.

-spec delivery_max_workers() -> pos_integer().
delivery_max_workers() ->
    case application:get_env(fluxer_gateway, push_delivery_max_workers, undefined) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> ?DEFAULT_DELIVERY_MAX_WORKERS
    end.

-spec delivery_tasks_per_worker() -> pos_integer().
delivery_tasks_per_worker() ->
    case application:get_env(fluxer_gateway, push_delivery_tasks_per_worker, undefined) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> ?DEFAULT_DELIVERY_TASKS_PER_WORKER
    end.

-spec subscription_fetch_batch_size() -> pos_integer().
subscription_fetch_batch_size() ->
    case application:get_env(fluxer_gateway, push_subscription_fetch_batch_size, undefined) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> ?DEFAULT_SUBSCRIPTION_FETCH_BATCH
    end.

-spec subscription_fetch_budget_ms() -> pos_integer().
subscription_fetch_budget_ms() ->
    case application:get_env(fluxer_gateway, push_subscription_fetch_budget_ms, undefined) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> ?DEFAULT_SUBSCRIPTION_FETCH_BUDGET_MS
    end.

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

deliver_subscription_tasks_sends_every_user_its_badge_count_test() ->
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
        ?assertEqual(ok, deliver_subscription_tasks(Tasks, test_send_ctx())),
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

bounded_worker_count_scales_with_recipients_and_stays_bounded_test() ->
    application:unset_env(fluxer_gateway, push_delivery_tasks_per_worker),
    ?assertEqual(3, bounded_worker_count(3, 8, 64)),
    ?assertEqual(8, bounded_worker_count(100, 8, 64)),
    ?assertEqual(20, bounded_worker_count(2500, 8, 64)),
    ?assertEqual(64, bounded_worker_count(10000, 8, 64)),
    ?assertEqual(8, bounded_worker_count(10000, 8, 8)).

bounded_worker_count_treats_the_bound_as_a_real_ceiling_test() ->
    application:unset_env(fluxer_gateway, push_delivery_tasks_per_worker),
    ?assertEqual(4, bounded_worker_count(10000, 8, 4)),
    ?assertEqual(1, bounded_worker_count(10000, 8, 1)),
    ?assertEqual(2, bounded_worker_count(2500, 8, 2)).

shared_ctx_payload_carries_no_badge_counts_test() ->
    Ctx = test_send_ctx(),
    Shared = shared_ctx_payload(Ctx),
    ?assert(is_binary(Shared)),
    ?assertEqual(#{}, element(?CTX_BADGE_COUNTS, binary_to_term(Shared))),
    ?assert(byte_size(Shared) < byte_size(term_to_binary(Ctx))).

restore_chunk_ctx_rebuilds_the_ctx_with_only_the_chunk_badges_test() ->
    Ctx = test_send_ctx(),
    Shared = shared_ctx_payload(Ctx),
    Chunk = [{2, [sub]}],
    Restored = restore_chunk_ctx(Shared, chunk_badge_counts(Chunk, ctx_badge_counts(Ctx))),
    ?assertEqual(9, tuple_size(Restored)),
    ?assertEqual(#{2 => 6}, element(?CTX_BADGE_COUNTS, Restored)),
    ?assertEqual(element(1, Ctx), element(1, Restored)),
    ?assertEqual(element(7, Ctx), element(7, Restored)),
    ?assertEqual(element(8, Ctx), element(8, Restored)),
    ?assertEqual(#{}, element(?CTX_BADGE_COUNTS, restore_chunk_ctx(Shared, #{}))).

bounded_fanout_isolates_a_raising_task_and_counts_it_test() ->
    Self = self(),
    ok = meck:new(push_sender, [passthrough, no_link]),
    ensure_test_counter_table(),
    Before = counter_value(delivery_task_failures),
    application:set_env(fluxer_gateway, push_delivery_tasks_per_worker, 1),
    try
        ok = meck:expect(push_sender, send_to_user_subscriptions, fun(UserId, _Subs, _Ctx) ->
            case UserId of
                3 -> error(deliberate_task_crash);
                _ -> Self ! {bounded_sent, UserId}
            end,
            ok
        end),
        Tasks = [{UserId, [sub]} || UserId <- lists:seq(1, 5)],
        ?assertEqual(ok, deliver_subscription_tasks(Tasks, test_send_ctx())),
        ?assertEqual([1, 2, 4, 5], lists:sort(collect_bounded_sent(4, []))),
        ?assertEqual(Before + 1, counter_value(delivery_task_failures))
    after
        application:unset_env(fluxer_gateway, push_delivery_tasks_per_worker),
        meck:unload(push_sender)
    end.

bounded_fanout_returns_reserved_slots_to_the_push_budget_test() ->
    ok = meck:new(push_sender, [passthrough, no_link]),
    ensure_test_counter_table(),
    ok = reset_test_counter(?CNT_LIVE_WORKERS),
    try
        ok = meck:expect(push_sender, send_to_user_subscriptions, fun(_UserId, _Subs, _Ctx) ->
            ok
        end),
        Tasks = [{UserId, [sub]} || UserId <- lists:seq(1, 40)],
        ?assertEqual(ok, send_bounded_subscription_tasks(Tasks, test_send_ctx())),
        ?assertEqual(0, counter_value(?CNT_LIVE_WORKERS))
    after
        meck:unload(push_sender)
    end.

bounded_fanout_releases_reserved_slots_when_workers_are_killed_test() ->
    ok = meck:new(push_sender, [passthrough, no_link]),
    ensure_test_counter_table(),
    ok = reset_test_counter(?CNT_LIVE_WORKERS),
    ok = reset_test_counter(delivery_recipients_not_attempted),
    application:set_env(fluxer_gateway, push_delivery_tasks_per_worker, 1),
    try
        ok = meck:expect(push_sender, send_to_user_subscriptions, fun(_UserId, _Subs, _Ctx) ->
            exit(self(), kill)
        end),
        Tasks = [{UserId, [sub]} || UserId <- lists:seq(1, 4)],
        ?assertEqual(ok, send_bounded_subscription_tasks(Tasks, test_send_ctx())),
        ?assertEqual(0, counter_value(?CNT_LIVE_WORKERS)),
        ?assertEqual(4, counter_value(delivery_recipients_not_attempted))
    after
        application:unset_env(fluxer_gateway, push_delivery_tasks_per_worker),
        meck:unload(push_sender)
    end.

claim_delivery_slots_reserves_before_the_next_job_reads_the_budget_test() ->
    ensure_test_counter_table(),
    ok = reset_test_counter(?CNT_LIVE_WORKERS),
    ?assertEqual(8, claim_delivery_slots(8, 10)),
    ?assertEqual(8, counter_value(?CNT_LIVE_WORKERS)),
    ?assertEqual(2, claim_delivery_slots(8, 10)),
    ?assertEqual(10, counter_value(?CNT_LIVE_WORKERS)),
    ?assertEqual(0, claim_delivery_slots(4, 10)),
    ?assertEqual(10, counter_value(?CNT_LIVE_WORKERS)),
    ok = release_delivery_slots(10),
    ?assertEqual(0, counter_value(?CNT_LIVE_WORKERS)),
    ok = release_delivery_slots(5),
    ?assertEqual(0, counter_value(?CNT_LIVE_WORKERS)).

bounded_fanout_delivers_inline_when_the_push_budget_is_exhausted_test() ->
    Self = self(),
    ok = meck:new(push_sender, [passthrough, no_link]),
    ensure_test_counter_table(),
    ok = reset_test_counter(delivery_budget_exhausted),
    Budget = gateway_http_client:push_max_concurrency(),
    ok = reset_test_counter(?CNT_LIVE_WORKERS),
    _ = claim_delivery_slots(Budget, Budget),
    try
        ok = meck:expect(push_sender, send_to_user_subscriptions, fun(UserId, _Subs, _Ctx) ->
            Self ! {bounded_sent, UserId},
            ok
        end),
        Tasks = [{UserId, [sub]} || UserId <- lists:seq(1, 6)],
        ?assertEqual(ok, send_bounded_subscription_tasks(Tasks, test_send_ctx())),
        ?assertEqual(lists:seq(1, 6), lists:sort(collect_bounded_sent(6, []))),
        ?assertEqual(1, counter_value(delivery_budget_exhausted)),
        ?assertEqual(Budget, counter_value(?CNT_LIVE_WORKERS))
    after
        ok = release_delivery_slots(Budget),
        meck:unload(push_sender)
    end.

delivery_stats_exposes_cumulative_loss_without_logs_test() ->
    ensure_test_counter_table(),
    Before = maps:get(delivery_recipients_not_attempted, delivery_stats()),
    ok = bump_counter(delivery_recipients_not_attempted, 3),
    Stats = delivery_stats(),
    ?assertEqual(Before + 3, maps:get(delivery_recipients_not_attempted, Stats)),
    ?assertEqual(
        lists:sort([delivery_last_crash | delivery_counter_keys()]),
        lists:sort(maps:keys(Stats))
    ).

bump_counter_never_creates_the_counter_table_test() ->
    case drop_test_counter_table() of
        ok ->
            ?assertEqual(ok, bump_counter(delivery_recipients_not_attempted, 1)),
            ?assertEqual(0, counter_value(delivery_recipients_not_attempted)),
            ?assertEqual(undefined, ets:info(?PUSH_COUNTERS, name));
        skip ->
            ok
    end.

ensure_test_counter_table() ->
    case ets:info(?PUSH_COUNTERS, name) of
        undefined ->
            _ = ets:new(?PUSH_COUNTERS, [
                named_table, public, set, {write_concurrency, true}
            ]),
            ok;
        _ ->
            ok
    end.

drop_test_counter_table() ->
    case ets:info(?PUSH_COUNTERS, name) of
        undefined ->
            ok;
        _ ->
            try ets:delete(?PUSH_COUNTERS) of
                _ -> ok
            catch
                error:badarg -> skip
            end
    end.

reset_test_counter(Key) ->
    try ets:insert(?PUSH_COUNTERS, {Key, 0}) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

bounded_fanout_delivers_every_recipient_test() ->
    Self = self(),
    ok = meck:new(push_sender, [passthrough, no_link]),
    try
        ok = meck:expect(push_sender, send_to_user_subscriptions, fun(UserId, _Subs, _Ctx) ->
            Self ! {bounded_sent, UserId},
            ok
        end),
        Tasks = [{UserId, [sub]} || UserId <- lists:seq(1, 200)],
        ?assertEqual(ok, deliver_subscription_tasks(Tasks, test_send_ctx())),
        ?assertEqual(lists:seq(1, 200), lists:sort(collect_bounded_sent(200, [])))
    after
        meck:unload(push_sender)
    end.

chunk_user_ids_uses_bounded_batches_test() ->
    ?assertEqual([[1, 2], [3, 4], [5]], chunk_user_ids([1, 2, 3, 4, 5], 2, [])),
    ?assertEqual([], chunk_user_ids([], 2, [])).

fetch_subscription_batches_stops_after_consecutive_failures_test() ->
    ok = meck:new(rpc_client, [passthrough, no_link]),
    try
        ok = meck:expect(rpc_client, call, fun(_Req) -> {error, unavailable} end),
        Batches = [[N] || N <- lists:seq(1, 10)],
        ?assertEqual(
            {[], 10, 10, ?SUBSCRIPTION_FETCH_MAX_CONSECUTIVE_FAILURES},
            fetch_subscription_batches(Batches, {[], 0, 0, 0})
        ),
        ?assertEqual(
            ?SUBSCRIPTION_FETCH_MAX_CONSECUTIVE_FAILURES,
            length(meck:history(rpc_client))
        )
    after
        meck:unload(rpc_client)
    end.

fetch_missing_in_batches_sends_only_successful_batches_test() ->
    Self = self(),
    ok = meck:new(push_sender, [passthrough, no_link]),
    ok = meck:new(rpc_client, [passthrough, no_link]),
    ok = meck:new(push_ets_cache, [passthrough, no_link]),
    application:set_env(fluxer_gateway, push_subscription_fetch_batch_size, 2),
    try
        ok = meck:expect(push_ets_cache, put_subscriptions, fun(_UserId, _Subs) -> ok end),
        ok = meck:expect(rpc_client, call, fun(#{<<"user_ids">> := Ids}) ->
            case Ids of
                [<<"1">>, <<"2">>] -> {ok, #{<<"1">> => [sub1], <<"2">> => [sub2]}};
                _ -> {error, unavailable}
            end
        end),
        ok = meck:expect(push_sender, send_to_user_subscriptions, fun(UserId, Subs, _Ctx) ->
            Self ! {bounded_sent, {UserId, Subs}},
            ok
        end),
        ?assertEqual(
            ok, fetch_and_send_missing_subscriptions([1, 2, 3, 4], test_send_ctx())
        ),
        ?assertEqual([{1, [sub1]}, {2, [sub2]}], lists:sort(collect_bounded_sent(2, [])))
    after
        application:unset_env(fluxer_gateway, push_subscription_fetch_batch_size),
        meck:unload(push_ets_cache),
        meck:unload(rpc_client),
        meck:unload(push_sender)
    end.

subscription_fetch_failure_counts_dropped_recipients_test() ->
    ok = meck:new(rpc_client, [passthrough, no_link]),
    ensure_test_counter_table(),
    ok = reset_test_counter(subscription_fetch_calls_failed),
    ok = reset_test_counter(subscription_fetch_users_dropped),
    try
        ok = meck:expect(rpc_client, call, fun(_Req) -> {error, unavailable} end),
        ?assertEqual(
            ok, fetch_and_send_missing_subscriptions([1, 2, 3], test_send_ctx())
        ),
        ?assertEqual(1, counter_value(subscription_fetch_calls_failed)),
        ?assertEqual(3, counter_value(subscription_fetch_users_dropped))
    after
        meck:unload(rpc_client)
    end.

clear_notification_fetch_failure_counts_the_dropped_recipient_test() ->
    ok = meck:new(rpc_client, [passthrough, no_link]),
    ensure_test_counter_table(),
    ok = reset_test_counter(subscription_fetch_calls_failed),
    ok = reset_test_counter(subscription_fetch_users_dropped),
    try
        ok = meck:expect(rpc_client, call, fun(_Req) -> {error, unavailable} end),
        ?assertEqual(
            ok, fetch_and_send_clear_notification_from_rpc(7, 8, 9, 0)
        ),
        ?assertEqual(1, counter_value(subscription_fetch_calls_failed)),
        ?assertEqual(1, counter_value(subscription_fetch_users_dropped))
    after
        meck:unload(rpc_client)
    end.

collect_bounded_sent(0, Acc) ->
    Acc;
collect_bounded_sent(Count, Acc) ->
    receive
        {bounded_sent, Sent} ->
            collect_bounded_sent(Count - 1, [Sent | Acc])
    after 5000 ->
        ?assert(false)
    end.

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
