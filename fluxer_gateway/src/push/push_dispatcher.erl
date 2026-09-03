%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_dispatcher).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([
    start_link/0,
    enqueue_send_notifications/8,
    enqueue_send_notifications/9,
    enqueue_clear_notifications/4,
    stats/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_MAX_INFLIGHT, 256).
-define(DEFAULT_MAX_QUEUE, 10000).
-define(ENQUEUE_TIMEOUT_MS, 1000).

-define(PUSH_COUNTER_TABLE, push_worker_counter).
-define(CNT_QUEUE_FULL, push_loss_queue_full).
-define(CNT_INVALID_JOB, push_loss_invalid_job).
-define(CNT_ENQUEUE_TIMEOUT, push_loss_enqueue_timeout).
-define(CNT_ENQUEUE_FAILED, push_loss_enqueue_failed).
-define(CNT_JOB_CRASHED, push_loss_job_crashed).
-define(CNT_WORKER_DIED, push_loss_worker_died).
-define(CNT_WORKER_POOL, push_loss_worker_pool).
-define(CNT_RESTARTS, push_dispatcher_restarts).
-define(CNT_RESTART_DISCARDED, push_dispatcher_restart_discarded).
-define(CNT_QUEUE_ENQUEUED, push_dispatcher_queue_enqueued).
-define(CNT_QUEUE_DEQUEUED, push_dispatcher_queue_dequeued).

-type push_job() ::
    #{
        type := message_create,
        user_ids := [integer()],
        message_data := map(),
        markdown_context := map(),
        guild_id := integer(),
        channel_id := integer(),
        message_id := integer(),
        guild_name := binary() | undefined,
        channel_name := binary() | undefined,
        badge_counts_ttl_seconds := non_neg_integer()
    }
    | #{
        type := clear_channel,
        user_id := integer(),
        channel_id := integer(),
        message_id := integer(),
        badge_counts_ttl_seconds := non_neg_integer()
    }.

-type state() :: #{
    queue := queue:queue(push_job()),
    queued := non_neg_integer(),
    inflight := non_neg_integer(),
    workers := #{reference() => true},
    max_inflight := pos_integer(),
    max_queue := pos_integer(),
    started_at => integer()
}.

-type counter_value() :: non_neg_integer() | unavailable.

-type stats() :: #{
    queued := non_neg_integer(),
    inflight := non_neg_integer(),
    counters := live | unavailable,
    dispatcher_uptime_seconds := non_neg_integer() | undefined,
    dispatcher_restarts := counter_value(),
    restart_discarded_jobs := counter_value(),
    queue_backlog_lost := counter_value(),
    queue_full_dropped := counter_value(),
    invalid_job_dropped := counter_value(),
    enqueue_timeout := counter_value(),
    enqueue_failed := counter_value(),
    job_crashed := counter_value(),
    worker_died := counter_value(),
    worker_pool_dropped := counter_value()
}.

-spec start_link() -> {ok, pid()} | {error, term()} | ignore.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec enqueue_send_notifications(
    [integer()],
    map(),
    integer(),
    integer(),
    integer(),
    binary() | undefined,
    binary() | undefined,
    non_neg_integer()
) -> ok | dropped.
enqueue_send_notifications(
    UserIds,
    MessageData,
    GuildId,
    ChannelId,
    MessageId,
    GuildName,
    ChannelName,
    BadgeCountsTtlSeconds
) ->
    enqueue_send_notifications(
        UserIds,
        MessageData,
        #{},
        GuildId,
        ChannelId,
        MessageId,
        GuildName,
        ChannelName,
        BadgeCountsTtlSeconds
    ).

-spec enqueue_send_notifications(
    [integer()],
    map(),
    map(),
    integer(),
    integer(),
    integer(),
    binary() | undefined,
    binary() | undefined,
    non_neg_integer()
) -> ok | dropped.
enqueue_send_notifications(
    UserIds,
    MessageData,
    MarkdownContext,
    GuildId,
    ChannelId,
    MessageId,
    GuildName,
    ChannelName,
    BadgeCountsTtlSeconds
) ->
    Job = send_notifications_job(
        UserIds,
        MessageData,
        MarkdownContext,
        GuildId,
        ChannelId,
        MessageId,
        GuildName,
        ChannelName,
        BadgeCountsTtlSeconds
    ),
    log_enqueue_send_notifications(UserIds, GuildId, ChannelId, MessageId),
    safe_enqueue(Job).

-spec enqueue_clear_notifications(integer(), integer(), integer(), non_neg_integer()) ->
    ok | dropped.
enqueue_clear_notifications(UserId, ChannelId, MessageId, BadgeCountsTtlSeconds) ->
    Job = #{
        type => clear_channel,
        user_id => UserId,
        channel_id => ChannelId,
        message_id => MessageId,
        badge_counts_ttl_seconds => BadgeCountsTtlSeconds
    },
    logger:debug(
        "Push: enqueuing clear notification job",
        #{user_id => UserId, channel_id => ChannelId, message_id => MessageId}
    ),
    safe_enqueue(Job).

-spec stats() -> stats() | #{}.
stats() ->
    Enqueued = read_counter(?CNT_QUEUE_ENQUEUED),
    try gen_server:call(?MODULE, stats, 1000) of
        #{queued := Queued, inflight := Inflight} = Reply ->
            StartedAt = maps:get(started_at, Reply, undefined),
            stats_with_loss(Queued, Inflight, StartedAt, Enqueued);
        _ ->
            #{}
    catch
        exit:_ -> #{};
        error:_ -> #{}
    end.

-spec stats_with_loss(non_neg_integer(), non_neg_integer(), term(), term()) -> stats().
stats_with_loss(Queued, Inflight, StartedAt, Enqueued) ->
    #{
        queued => Queued,
        inflight => Inflight,
        counters => counter_table_status(),
        dispatcher_uptime_seconds => uptime_seconds(StartedAt),
        dispatcher_restarts => read_counter(?CNT_RESTARTS),
        restart_discarded_jobs => read_counter(?CNT_RESTART_DISCARDED),
        queue_backlog_lost => queue_backlog_lost(Enqueued, Queued),
        queue_full_dropped => read_counter(?CNT_QUEUE_FULL),
        invalid_job_dropped => read_counter(?CNT_INVALID_JOB),
        enqueue_timeout => read_counter(?CNT_ENQUEUE_TIMEOUT),
        enqueue_failed => read_counter(?CNT_ENQUEUE_FAILED),
        job_crashed => read_counter(?CNT_JOB_CRASHED),
        worker_died => read_counter(?CNT_WORKER_DIED),
        worker_pool_dropped => read_counter(?CNT_WORKER_POOL)
    }.

-spec uptime_seconds(term()) -> non_neg_integer() | undefined.
uptime_seconds(StartedAt) when is_integer(StartedAt) ->
    max(0, erlang:monotonic_time(second) - StartedAt);
uptime_seconds(_StartedAt) ->
    undefined.

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:process_flag(fullsweep_after, 10),
    bump_counter(?CNT_RESTARTS),
    {ok, #{
        queue => queue:new(),
        queued => 0,
        inflight => 0,
        workers => #{},
        max_inflight => budget_aware_max_inflight(),
        max_queue => get_int_or_default(push_dispatcher_max_queue, ?DEFAULT_MAX_QUEUE),
        started_at => erlang:monotonic_time(second)
    }}.

-spec budget_aware_max_inflight() -> pos_integer().
budget_aware_max_inflight() ->
    Configured = get_int_or_default(push_dispatcher_max_inflight, ?DEFAULT_MAX_INFLIGHT),
    PushBudget = gateway_http_client:push_max_concurrency(),
    DeliveryConcurrency = max(1, push_subscriptions:delivery_concurrency()),
    BudgetCap = max(1, PushBudget div DeliveryConcurrency),
    min(Configured, BudgetCap).

-spec handle_call(term(), gen_server:from(), state()) ->
    {reply, term(), state()}.
handle_call(stats, _From, #{queued := Queued, inflight := Inflight} = State) ->
    Reply = #{
        queued => Queued,
        inflight => Inflight,
        started_at => maps:get(started_at, State, undefined)
    },
    {reply, Reply, State};
handle_call({enqueue, Job}, _From, State) ->
    {Result, State1} = handle_enqueue(Job, State),
    {reply, Result, State1};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({enqueue, Job}, State) ->
    {_Result, State1} = handle_enqueue(Job, State),
    {noreply, State1};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(
    {'DOWN', Ref, process, _Pid, Reason},
    #{workers := Workers, inflight := Inflight} = State
) ->
    case maps:is_key(Ref, Workers) of
        true ->
            count_worker_down(Reason),
            RemainingWorkers = maps:remove(Ref, Workers),
            DecrementedInflight = max(0, Inflight - 1),
            drain_queue(State#{
                workers := RemainingWorkers,
                inflight := DecrementedInflight
            });
        false ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    bump_counter(?CNT_RESTART_DISCARDED, maps:get(queued, State, 0)).

-spec count_worker_down(term()) -> ok.
count_worker_down(normal) ->
    ok;
count_worker_down(_Reason) ->
    bump_counter(?CNT_JOB_CRASHED),
    bump_counter(?CNT_WORKER_DIED).

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec maybe_enqueue_or_start(push_job(), state()) -> {ok | dropped, state()}.
maybe_enqueue_or_start(Job, #{inflight := Inflight, max_inflight := MaxInflight} = State) ->
    case Inflight < MaxInflight of
        true ->
            logger:debug(
                "Push: starting job immediately",
                #{
                    message_id => maps:get(message_id, Job, undefined),
                    inflight => Inflight,
                    max_inflight => MaxInflight
                }
            ),
            {ok, start_job(Job, State)};
        false ->
            logger:debug(
                "Push: at capacity, queueing job",
                #{
                    message_id => maps:get(message_id, Job, undefined),
                    inflight => Inflight,
                    max_inflight => MaxInflight,
                    queued => maps:get(queued, State)
                }
            ),
            maybe_enqueue(Job, State)
    end.

-spec maybe_enqueue(push_job(), state()) -> {ok | dropped, state()}.
maybe_enqueue(Job, #{queued := Queued, max_queue := MaxQueue, queue := Queue0} = State) ->
    case Queued < MaxQueue of
        true ->
            bump_counter(?CNT_QUEUE_ENQUEUED),
            Queue1 = queue:in(Job, Queue0),
            {ok, State#{queue := Queue1, queued := Queued + 1}};
        false ->
            bump_counter(?CNT_QUEUE_FULL),
            DropCount = bump_drop_count(),
            log_queue_full_drop(loss_logging_enabled(), Job, Queued, MaxQueue, DropCount),
            {dropped, State}
    end.

-spec log_queue_full_drop(
    boolean(), push_job(), non_neg_integer(), pos_integer(), non_neg_integer()
) -> ok.
log_queue_full_drop(true, Job, Queued, MaxQueue, _DropCount) ->
    logger:error(
        "Push: queue full, dropping job",
        #{
            message_id => maps:get(message_id, Job, undefined),
            queued => Queued,
            max_queue => MaxQueue,
            total_dropped => read_counter(?CNT_QUEUE_FULL)
        }
    );
log_queue_full_drop(false, Job, Queued, MaxQueue, DropCount) ->
    logger:warning(
        "Push: queue full, dropping job",
        #{
            message_id => maps:get(message_id, Job, undefined),
            queued => Queued,
            max_queue => MaxQueue,
            total_dropped => DropCount
        }
    ).

-spec bump_drop_count() -> non_neg_integer().
bump_drop_count() ->
    Current =
        case erlang:get(push_dispatcher_drop_count) of
            N when is_integer(N) -> N;
            _ -> 0
        end,
    Updated = Current + 1,
    erlang:put(push_dispatcher_drop_count, Updated),
    Updated.

-spec loss_logging_enabled() -> boolean().
loss_logging_enabled() ->
    application:get_env(fluxer_gateway, push_loss_logging, false) =:= true.

-spec counter_table_status() -> live | unavailable.
counter_table_status() ->
    case ets:info(?PUSH_COUNTER_TABLE, size) of
        Size when is_integer(Size) -> live;
        _ -> unavailable
    end.

-spec queue_backlog_lost(term(), non_neg_integer()) -> counter_value().
queue_backlog_lost(Enqueued, Queued) when is_integer(Enqueued) ->
    max(0, trunc(Enqueued - dequeued_total() - Queued));
queue_backlog_lost(_Enqueued, _Queued) ->
    unavailable.

-spec dequeued_total() -> non_neg_integer().
dequeued_total() ->
    case read_counter(?CNT_QUEUE_DEQUEUED) of
        Dequeued when is_integer(Dequeued) -> Dequeued;
        unavailable -> 0
    end.

-spec read_counter(atom()) -> counter_value().
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

-spec start_job(push_job(), state()) -> state().
start_job(Job, #{workers := Workers, inflight := Inflight} = State) ->
    {_Pid, Ref} =
        spawn_monitor(fun() ->
            run_job(Job)
        end),
    State#{
        workers := Workers#{Ref => true},
        inflight := Inflight + 1
    }.

-spec drain_queue(state()) -> {noreply, state()}.
drain_queue(
    #{inflight := Inflight, max_inflight := MaxInflight, queue := Queue0, queued := Queued} =
        State
) ->
    case Inflight < MaxInflight of
        true -> drain_available_queue(queue:out(Queue0), Queued, State);
        false -> {noreply, State}
    end.

-spec send_notifications_job(
    [integer()],
    map(),
    map(),
    integer(),
    integer(),
    integer(),
    binary() | undefined,
    binary() | undefined,
    non_neg_integer()
) -> push_job().
send_notifications_job(
    UserIds,
    MessageData,
    MarkdownContext,
    GuildId,
    ChannelId,
    MessageId,
    GuildName,
    ChannelName,
    BadgeCountsTtlSeconds
) ->
    #{
        type => message_create,
        user_ids => UserIds,
        message_data => MessageData,
        markdown_context => MarkdownContext,
        guild_id => GuildId,
        channel_id => ChannelId,
        message_id => MessageId,
        guild_name => GuildName,
        channel_name => ChannelName,
        badge_counts_ttl_seconds => BadgeCountsTtlSeconds
    }.

-spec log_enqueue_send_notifications([integer()], integer(), integer(), integer()) -> ok.
log_enqueue_send_notifications(UserIds, GuildId, ChannelId, MessageId) ->
    logger:debug(
        "Push: enqueuing dispatch job",
        #{
            message_id => MessageId,
            channel_id => ChannelId,
            guild_id => GuildId,
            user_count => length(UserIds)
        }
    ).

-spec drain_available_queue(
    {{value, push_job()}, queue:queue(push_job())} | {empty, queue:queue(push_job())},
    non_neg_integer(),
    state()
) -> {noreply, state()}.
drain_available_queue({{value, Job}, Queue1}, Queued, State) ->
    bump_counter(?CNT_QUEUE_DEQUEUED),
    State1 = State#{queue := Queue1, queued := max(0, Queued - 1)},
    State2 = start_job(Job, State1),
    drain_queue(State2);
drain_available_queue({empty, _}, _Queued, State) ->
    {noreply, State}.

-spec run_job(push_job()) -> ok.
run_job(#{message_id := MessageId} = Job) ->
    try
        run_typed_job(maps:get(type, Job, message_create), Job),
        logger:debug("Push: worker completed", #{message_id => MessageId}),
        ok
    catch
        Class:Reason:Stacktrace ->
            bump_counter(?CNT_JOB_CRASHED),
            log_job_crash(loss_logging_enabled(), MessageId, Class, Reason, Stacktrace),
            ok
    end.

-spec log_job_crash(boolean(), integer(), atom(), term(), list()) -> ok.
log_job_crash(true, MessageId, Class, Reason, Stacktrace) ->
    logger:error(
        "Push: worker crashed",
        #{
            message_id => MessageId,
            class => Class,
            reason => Reason,
            stacktrace => Stacktrace
        }
    );
log_job_crash(false, MessageId, Class, Reason, _Stacktrace) ->
    logger:debug(
        "Push: worker crashed",
        #{message_id => MessageId, class => Class, reason => Reason}
    ).

-spec run_typed_job(message_create | clear_channel, push_job()) -> ok.
run_typed_job(message_create, #{
    user_ids := UserIds,
    message_data := MessageData,
    markdown_context := MarkdownContext,
    guild_id := GuildId,
    channel_id := ChannelId,
    message_id := MessageId,
    guild_name := GuildName,
    channel_name := ChannelName,
    badge_counts_ttl_seconds := BadgeCountsTtlSeconds
}) ->
    logger:debug(
        "Push: worker starting send_push_notifications",
        #{message_id => MessageId, user_count => length(UserIds)}
    ),
    push_sender:send_push_notifications(#{
        user_ids => UserIds,
        message_data => MessageData,
        markdown_context => MarkdownContext,
        guild_id => GuildId,
        channel_id => ChannelId,
        message_id => MessageId,
        guild_name => GuildName,
        channel_name => ChannelName,
        badge_counts_ttl_seconds => BadgeCountsTtlSeconds
    });
run_typed_job(clear_channel, #{
    user_id := UserId,
    channel_id := ChannelId,
    message_id := MessageId,
    badge_counts_ttl_seconds := BadgeCountsTtlSeconds
}) ->
    logger:debug(
        "Push: worker starting clear_channel_notifications",
        #{user_id => UserId, channel_id => ChannelId, message_id => MessageId}
    ),
    push_sender:send_clear_channel_notifications(
        UserId, ChannelId, MessageId, BadgeCountsTtlSeconds
    ).

-spec get_int_or_default(atom(), integer()) -> integer().
get_int_or_default(Key, Default) ->
    case fluxer_gateway_env:get_optional(Key) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> Default
    end.

-spec safe_enqueue(push_job()) -> ok | dropped.
safe_enqueue(Job) ->
    try gen_server:call(?MODULE, {enqueue, Job}, ?ENQUEUE_TIMEOUT_MS) of
        ok ->
            ok;
        dropped ->
            dropped;
        _ ->
            bump_counter(?CNT_ENQUEUE_FAILED),
            dropped
    catch
        throw:_Reason ->
            bump_counter(?CNT_ENQUEUE_FAILED),
            dropped;
        error:_Reason ->
            bump_counter(?CNT_ENQUEUE_FAILED),
            dropped;
        exit:Reason ->
            count_enqueue_exit(Reason),
            dropped
    end.

-spec count_enqueue_exit(term()) -> ok.
count_enqueue_exit({timeout, _Call}) ->
    bump_counter(?CNT_ENQUEUE_TIMEOUT);
count_enqueue_exit(_Reason) ->
    bump_counter(?CNT_ENQUEUE_FAILED).

-spec handle_enqueue(term(), state()) -> {ok | dropped, state()}.
handle_enqueue(Job0, State) ->
    case push_job(Job0) of
        {ok, Job} ->
            maybe_enqueue_or_start(Job, State);
        error ->
            bump_counter(?CNT_INVALID_JOB),
            {dropped, State}
    end.

-spec push_job(term()) -> {ok, push_job()} | error.
push_job(#{type := message_create} = Job) ->
    push_message_create_job(Job);
push_job(#{type := clear_channel} = Job) ->
    push_clear_channel_job(Job);
push_job(_) ->
    error.

-spec push_message_create_job(map()) -> {ok, push_job()} | error.
push_message_create_job(
    #{
        user_ids := UserIds,
        message_data := MessageData,
        guild_id := GuildId,
        channel_id := ChannelId,
        message_id := MessageId,
        guild_name := GuildName,
        channel_name := ChannelName,
        badge_counts_ttl_seconds := BadgeCountsTtlSeconds
    } = Job
) when
    is_map(MessageData),
    is_integer(GuildId),
    is_integer(ChannelId),
    is_integer(MessageId),
    is_integer(BadgeCountsTtlSeconds),
    BadgeCountsTtlSeconds >= 0
->
    push_send_job(UserIds, MessageData, GuildId, ChannelId, MessageId, #{
        guild_name => GuildName,
        channel_name => ChannelName,
        badge_counts_ttl_seconds => BadgeCountsTtlSeconds,
        markdown_context => maps:get(markdown_context, Job, #{})
    });
push_message_create_job(_) ->
    error.

-spec push_clear_channel_job(map()) -> {ok, push_job()} | error.
push_clear_channel_job(#{
    user_id := UserId,
    channel_id := ChannelId,
    message_id := MessageId,
    badge_counts_ttl_seconds := BadgeCountsTtlSeconds
}) when
    is_integer(UserId),
    is_integer(ChannelId),
    is_integer(MessageId),
    is_integer(BadgeCountsTtlSeconds),
    BadgeCountsTtlSeconds >= 0
->
    {ok, clear_channel_job(UserId, ChannelId, MessageId, BadgeCountsTtlSeconds)};
push_clear_channel_job(_) ->
    error.

-spec clear_channel_job(integer(), integer(), integer(), non_neg_integer()) -> push_job().
clear_channel_job(UserId, ChannelId, MessageId, BadgeCountsTtlSeconds) ->
    #{
        type => clear_channel,
        user_id => UserId,
        channel_id => ChannelId,
        message_id => MessageId,
        badge_counts_ttl_seconds => BadgeCountsTtlSeconds
    }.

-spec push_send_job(
    term(), map(), integer(), integer(), integer(), #{
        guild_name := term(),
        channel_name := term(),
        badge_counts_ttl_seconds := non_neg_integer(),
        markdown_context => map()
    }
) -> {ok, push_job()} | error.
push_send_job(UserIds0, MessageData, GuildId, ChannelId, MessageId, Options) ->
    case send_job_options(UserIds0, Options) of
        {ok, UserIds, GuildName, ChannelName, BadgeCountsTtlSeconds, MarkdownContext} ->
            {ok,
                send_notifications_job(
                    UserIds,
                    MessageData,
                    MarkdownContext,
                    GuildId,
                    ChannelId,
                    MessageId,
                    GuildName,
                    ChannelName,
                    BadgeCountsTtlSeconds
                )};
        error ->
            error
    end.

-spec send_job_options(term(), map()) ->
    {ok, [integer()], binary() | undefined, binary() | undefined, non_neg_integer(), map()}
    | error.
send_job_options(UserIds0, Options) ->
    GuildName0 = maps:get(guild_name, Options),
    ChannelName0 = maps:get(channel_name, Options),
    BadgeCountsTtlSeconds = maps:get(badge_counts_ttl_seconds, Options),
    MarkdownContext = maps:get(markdown_context, Options, #{}),
    case
        {
            push_normalize:integer_list(UserIds0),
            optional_binary(GuildName0),
            optional_binary(ChannelName0)
        }
    of
        {{ok, UserIds}, {ok, GuildName}, {ok, ChannelName}} when is_map(MarkdownContext) ->
            {ok, UserIds, GuildName, ChannelName, BadgeCountsTtlSeconds, MarkdownContext};
        _ ->
            error
    end.

-spec optional_binary(term()) -> {ok, binary() | undefined} | error.
optional_binary(undefined) ->
    {ok, undefined};
optional_binary(Value) when is_binary(Value) ->
    {ok, Value};
optional_binary(_) ->
    error.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

absent_counter_table_reads_unavailable_test() ->
    delete_counter_table(),
    ?assertEqual(unavailable, counter_table_status()),
    ?assertEqual(unavailable, read_counter(?CNT_QUEUE_FULL)),
    ?assertEqual(ok, bump_counter(?CNT_QUEUE_FULL)),
    ?assertEqual(unavailable, read_counter(?CNT_QUEUE_FULL)).

absent_counter_key_is_distinct_from_zero_test() ->
    with_counter_table(fun() ->
        ?assertEqual(live, counter_table_status()),
        ?assertEqual(unavailable, read_counter(?CNT_QUEUE_FULL)),
        ok = bump_counter(?CNT_RESTART_DISCARDED, 0),
        ?assertEqual(0, read_counter(?CNT_RESTART_DISCARDED))
    end).

bump_counter_creates_then_increments_key_test() ->
    with_counter_table(fun() ->
        ok = bump_counter(?CNT_QUEUE_FULL),
        ?assertEqual(1, read_counter(?CNT_QUEUE_FULL)),
        ok = bump_counter(?CNT_QUEUE_FULL, 4),
        ?assertEqual(5, read_counter(?CNT_QUEUE_FULL))
    end).

enqueue_exit_separates_timeout_from_failure_test() ->
    with_counter_table(fun() ->
        ok = count_enqueue_exit({timeout, {gen_server, call, []}}),
        ok = count_enqueue_exit({noproc, {gen_server, call, []}}),
        ?assertEqual(1, read_counter(?CNT_ENQUEUE_TIMEOUT)),
        ?assertEqual(1, read_counter(?CNT_ENQUEUE_FAILED))
    end).

worker_down_counts_only_abnormal_exits_test() ->
    with_counter_table(fun() ->
        ok = count_worker_down(normal),
        ?assertEqual(unavailable, read_counter(?CNT_WORKER_DIED)),
        ok = count_worker_down(killed),
        ok = count_worker_down({shutdown, restarting}),
        ?assertEqual(2, read_counter(?CNT_WORKER_DIED))
    end).

terminate_counts_discarded_queue_depth_test() ->
    with_counter_table(fun() ->
        ?assertEqual(ok, terminate(shutdown, dispatcher_state(7, 10))),
        ?assertEqual(7, read_counter(?CNT_RESTART_DISCARDED))
    end).

invalid_job_enqueue_is_counted_test() ->
    with_counter_table(fun() ->
        State = dispatcher_state(0, 10),
        ?assertEqual({dropped, State}, handle_enqueue(#{type => invalid}, State)),
        ?assertEqual(1, read_counter(?CNT_INVALID_JOB))
    end).

uptime_seconds_reports_undefined_without_start_time_test() ->
    ?assertEqual(undefined, uptime_seconds(undefined)),
    ?assertEqual(0, uptime_seconds(erlang:monotonic_time(second) + 5)),
    ?assert(is_integer(uptime_seconds(erlang:monotonic_time(second) - 3))).

abnormal_worker_down_also_counts_a_crashed_job_test() ->
    with_counter_table(fun() ->
        ok = count_worker_down(normal),
        ?assertEqual(unavailable, read_counter(?CNT_JOB_CRASHED)),
        ok = count_worker_down(killed),
        ?assertEqual(1, read_counter(?CNT_JOB_CRASHED)),
        ?assertEqual(1, read_counter(?CNT_WORKER_DIED))
    end).

enqueue_records_queue_growth_outside_the_process_test() ->
    with_counter_table(fun() ->
        {ok, State1} = maybe_enqueue(clear_job(), dispatcher_state(0, 10)),
        ?assertEqual(1, maps:get(queued, State1)),
        ?assertEqual(1, read_counter(?CNT_QUEUE_ENQUEUED)),
        ?assertEqual(unavailable, read_counter(?CNT_QUEUE_DEQUEUED))
    end).

queue_full_drop_does_not_record_queue_growth_test() ->
    with_counter_table(fun() ->
        {dropped, _State1} = maybe_enqueue(clear_job(), dispatcher_state(3, 3)),
        ?assertEqual(1, read_counter(?CNT_QUEUE_FULL)),
        ?assertEqual(unavailable, read_counter(?CNT_QUEUE_ENQUEUED))
    end).

queue_backlog_lost_survives_an_untrappable_kill_test() ->
    with_counter_table(fun() ->
        ?assertEqual(unavailable, queue_backlog_lost(read_counter(?CNT_QUEUE_ENQUEUED), 0)),
        ok = bump_counter(?CNT_QUEUE_ENQUEUED, 7),
        ok = bump_counter(?CNT_QUEUE_DEQUEUED, 2),
        ?assertEqual(0, queue_backlog_lost(read_counter(?CNT_QUEUE_ENQUEUED), 5)),
        ?assertEqual(5, queue_backlog_lost(read_counter(?CNT_QUEUE_ENQUEUED), 0)),
        ?assertEqual(0, queue_backlog_lost(read_counter(?CNT_QUEUE_ENQUEUED), 900))
    end).

dispatcher_state(Queued, MaxQueue) ->
    #{
        queue => queue:new(),
        queued => Queued,
        inflight => 0,
        workers => #{},
        max_inflight => 1,
        max_queue => MaxQueue
    }.

clear_job() ->
    #{
        type => clear_channel,
        user_id => 1,
        channel_id => 2,
        message_id => 3,
        badge_counts_ttl_seconds => 0
    }.

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

-endif.
