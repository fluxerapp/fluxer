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
    max_queue := pos_integer()
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

-spec stats() -> #{queued := non_neg_integer(), inflight := non_neg_integer()} | #{}.
stats() ->
    try
        gen_server:call(?MODULE, stats, 1000)
    catch
        exit:_ -> #{};
        error:_ -> #{}
    end.

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:process_flag(fullsweep_after, 10),
    {ok, #{
        queue => queue:new(),
        queued => 0,
        inflight => 0,
        workers => #{},
        max_inflight => budget_aware_max_inflight(),
        max_queue => get_int_or_default(push_dispatcher_max_queue, ?DEFAULT_MAX_QUEUE)
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
    {reply, #{queued => Queued, inflight => Inflight}, State};
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
    {'DOWN', Ref, process, _Pid, _Reason},
    #{workers := Workers, inflight := Inflight} = State
) ->
    case maps:is_key(Ref, Workers) of
        true ->
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
terminate(_Reason, _State) ->
    ok.

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
            Queue1 = queue:in(Job, Queue0),
            {ok, State#{queue := Queue1, queued := Queued + 1}};
        false ->
            DropCount = bump_drop_count(),
            logger:warning(
                "Push: queue full, dropping job",
                #{
                    message_id => maps:get(message_id, Job, undefined),
                    queued => Queued,
                    max_queue => MaxQueue,
                    total_dropped => DropCount
                }
            ),
            {dropped, State}
    end.

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
        Class:Reason ->
            logger:debug(
                "Push: worker crashed",
                #{message_id => MessageId, class => Class, reason => Reason}
            ),
            ok
    end.

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
        ok -> ok;
        dropped -> dropped;
        _ -> dropped
    catch
        throw:_Reason -> dropped;
        error:_Reason -> dropped;
        exit:_Reason -> dropped
    end.

-spec handle_enqueue(term(), state()) -> {ok | dropped, state()}.
handle_enqueue(Job0, State) ->
    case push_job(Job0) of
        {ok, Job} -> maybe_enqueue_or_start(Job, State);
        error -> {dropped, State}
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
