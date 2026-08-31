%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_connection_guild).
-typing([eqwalizer]).

-export([
    handle_guild_connect/3,
    handle_guild_connect_result/4,
    handle_guild_connect_timeout/4,
    handle_guild_connect_worker_down/3,
    maybe_spawn_guild_connect/5,
    finalize_guild_connection/4,
    repair_stalled_guild_connects/1,
    do_guild_connect/1,
    handle_result_internal/4
]).

-export_type([session_state/0, guild_id/0, attempt/0, guild_connect_result/0, session_result/0]).

-define(GUILD_CONNECT_MAX_INFLIGHT, 32).
-define(GUILD_CONNECT_TIMEOUT_MS, 120000).
-define(GUILD_CONNECT_REPAIR_INTERVAL_MS, 30000).
-define(GUILD_CONNECT_REPAIR_LIMIT, 8).
-define(NOT_MEMBER_MAX_RETRIES, 3).

-type session_state() :: session:session_state().
-type guild_id() :: session:guild_id().
-type attempt() :: non_neg_integer().
-type guild_connect_result() ::
    {ok, pid(), map()}
    | {ok_unavailable, pid(), map()}
    | {ok_cached_unavailable, map()}
    | {error, term()}.

-type session_result() :: {noreply, session_state()} | {stop, normal, session_state()}.

-spec handle_guild_connect(guild_id(), attempt(), session_state()) -> session_result().
handle_guild_connect(GuildId, Attempt, State) ->
    Guilds = maps:get(guilds, State),
    SessionId = maps:get(id, State),
    UserId = maps:get(user_id, State),
    case maps:find(GuildId, Guilds) of
        {ok, {_Pid, _Ref}} ->
            {noreply, State};
        {ok, cached_unavailable} ->
            session_connection_unavailability:maybe_handle_cached_unavailability(
                GuildId, Attempt, SessionId, UserId, State
            );
        _ ->
            maybe_spawn_guild_connect(GuildId, Attempt, SessionId, UserId, State)
    end.

-spec maybe_spawn_guild_connect(guild_id(), attempt(), binary(), integer(), session_state()) ->
    {noreply, session_state()}.
maybe_spawn_guild_connect(GuildId, Attempt, SessionId, UserId, State) ->
    Inflight0 = guild_connect_inflight(State),
    case
        {maps:is_key(GuildId, Inflight0), map_size(Inflight0) >= ?GUILD_CONNECT_MAX_INFLIGHT}
    of
        {true, _} ->
            {noreply, State};
        {false, true} ->
            erlang:send_after(50, self(), {guild_connect, GuildId, Attempt}),
            {noreply, State};
        {false, false} ->
            try_acquire_and_spawn(GuildId, Attempt, SessionId, UserId, Inflight0, State)
    end.

-spec try_acquire_and_spawn(guild_id(), attempt(), binary(), integer(), map(), session_state()) ->
    {noreply, session_state()}.
try_acquire_and_spawn(GuildId, Attempt, SessionId, UserId, Inflight0, State) ->
    Bot = ensure_bool(maps:get(bot, State, false)),
    IsStaff = ensure_bool(maps:get(is_staff, State, false)),
    case gateway_concurrency:try_acquire_guild_start() of
        {error, at_capacity} ->
            Jitter = rand:uniform(200),
            erlang:send_after(50 + Jitter, self(), {guild_connect, GuildId, Attempt}),
            {noreply, State};
        ok ->
            spawn_guild_connect_worker(
                GuildId, Attempt, SessionId, UserId, Bot, IsStaff, Inflight0, State
            )
    end.

-spec spawn_guild_connect_worker(
    guild_id(),
    attempt(),
    binary(),
    integer(),
    boolean(),
    boolean(),
    map(),
    session_state()
) -> {noreply, session_state()}.
spawn_guild_connect_worker(GuildId, Attempt, SessionId, UserId, Bot, IsStaff, Inflight0, State) ->
    Inflight = Inflight0#{GuildId => Attempt},
    Ctx = #{
        session_pid => self(),
        guild_id => GuildId,
        attempt => Attempt,
        session_id => SessionId,
        user_id => UserId,
        bot => Bot,
        is_staff => IsStaff,
        initial_guild_id => maps:get(initial_guild_id, State, undefined),
        user_data => maps:get(user_data, State, #{})
    },
    {WorkerPid, WorkerRef} = spawn_monitor(fun() ->
        do_guild_connect_with_release(Ctx)
    end),
    Workers = guild_connect_workers(State),
    Timers = guild_connect_timers(State),
    Token = make_ref(),
    TimerRef = erlang:send_after(
        ?GUILD_CONNECT_TIMEOUT_MS, self(), {guild_connect_timeout, GuildId, Attempt, Token}
    ),
    State1 = State#{
        guild_connect_inflight => Inflight,
        guild_connect_workers => Workers#{WorkerRef => {GuildId, Attempt, WorkerPid}},
        guild_connect_timers => Timers#{GuildId => {Token, TimerRef}}
    },
    {noreply, State1}.

-spec do_guild_connect_with_release(map()) -> ok.
do_guild_connect_with_release(Ctx) ->
    try
        do_guild_connect(Ctx)
    after
        gateway_concurrency:release_guild_start()
    end.

-spec do_guild_connect(map()) -> ok.
do_guild_connect(#{session_pid := SessionPid, guild_id := GuildId, attempt := Attempt} = Ctx) ->
    ConnectStartedAt = gateway_timings:start(),
    Result =
        try
            session_connection_guild_resolve:do_local_guild_connect(Ctx)
        catch
            exit:{noproc, _} -> {error, {guild_died, noproc}};
            exit:{normal, _} -> {error, {guild_died, normal}};
            exit:Reason -> {error, {exception, Reason}};
            error:Reason -> {error, {exception, Reason}};
            throw:Reason -> {error, {exception, Reason}}
        end,
    WorkerTimings = gateway_timings:record_function(
        guild_connect_worker,
        <<"session_connection_guild:do_guild_connect/1">>,
        ConnectStartedAt,
        gateway_timings:new()
    ),
    SessionPid ! {gateway_timing_update, WorkerTimings},
    _ =
        case Result of
            pending -> ok;
            _ -> _ = SessionPid ! {guild_connect_result, GuildId, Attempt, Result}
        end,
    ok.

-spec handle_guild_connect_result(
    guild_id(), attempt(), guild_connect_result(), session_state()
) ->
    session_result().
handle_guild_connect_result(GuildId, Attempt, Result, State) ->
    Inflight = guild_connect_inflight(State),
    case maps:find(GuildId, Inflight) of
        {ok, Attempt} ->
            State1 = remove_pending_guild_connect(GuildId, Attempt, State),
            handle_result_internal(GuildId, Attempt, Result, State1);
        _ ->
            {noreply, State}
    end.

-spec handle_guild_connect_timeout(guild_id(), attempt(), reference(), session_state()) ->
    session_result().
handle_guild_connect_timeout(GuildId, Attempt, Token, State) ->
    case maps:get(GuildId, guild_connect_timers(State), undefined) of
        {Token, _TimerRef} -> expire_guild_connect(GuildId, Attempt, State);
        _ -> {noreply, State}
    end.

-spec expire_guild_connect(guild_id(), attempt(), session_state()) -> session_result().
expire_guild_connect(GuildId, Attempt, State) ->
    UserId = maps:get(user_id, State),
    logger:warning(
        "guild_connect_timeout: guild_id=~p"
        " user_id=~p attempt=~p",
        [GuildId, UserId, Attempt]
    ),
    State1 = remove_pending_guild_connect(GuildId, Attempt, State),
    session_connection_retry:retry_or_fail(
        GuildId,
        Attempt,
        State1,
        fun session_ready:mark_guild_unavailable/2
    ).

-spec handle_guild_connect_worker_down(reference(), term(), session_state()) ->
    {guild_connect_worker, session_result()} | not_guild_connect_worker.
handle_guild_connect_worker_down(WorkerRef, Reason, State) ->
    Workers = guild_connect_workers(State),
    case maps:take(WorkerRef, Workers) of
        {{_GuildId, _Attempt, _WorkerPid}, RemainingWorkers} when
            Reason =:= normal; Reason =:= shutdown
        ->
            {guild_connect_worker,
                {noreply, put_guild_connect_workers(RemainingWorkers, State)}};
        {{GuildId, Attempt, _WorkerPid}, RemainingWorkers} ->
            State1 = put_guild_connect_workers(RemainingWorkers, State),
            fail_dead_guild_connect_worker(GuildId, Attempt, Reason, State1);
        error ->
            not_guild_connect_worker
    end.

-spec fail_dead_guild_connect_worker(guild_id(), attempt(), term(), session_state()) ->
    {guild_connect_worker, session_result()}.
fail_dead_guild_connect_worker(GuildId, Attempt, Reason, State) ->
    Inflight = guild_connect_inflight(State),
    case maps:get(GuildId, Inflight, undefined) of
        Attempt ->
            UserId = maps:get(user_id, State),
            logger:warning(
                "guild_connect_worker_down: guild_id=~p user_id=~p attempt=~p reason=~p",
                [GuildId, UserId, Attempt, Reason]
            ),
            State1 = remove_guild_connect_inflight(GuildId, State),
            Result = session_connection_retry:retry_or_fail(
                GuildId,
                Attempt,
                State1,
                fun session_ready:mark_guild_unavailable/2
            ),
            {guild_connect_worker, Result};
        _Other ->
            {guild_connect_worker, {noreply, State}}
    end.

-spec cleanup_guild_connect_worker(guild_id(), attempt(), session_state()) -> session_state().
cleanup_guild_connect_worker(GuildId, Attempt, State) ->
    Workers = guild_connect_workers(State),
    RemainingWorkers = maps:fold(
        fun
            (WorkerRef, {WorkerGuildId, WorkerAttempt, _WorkerPid}, Acc) when
                WorkerGuildId =:= GuildId, WorkerAttempt =:= Attempt
            ->
                demonitor(WorkerRef, [flush]),
                Acc;
            (WorkerRef, WorkerInfo, Acc) ->
                Acc#{WorkerRef => WorkerInfo}
        end,
        #{},
        Workers
    ),
    put_guild_connect_workers(RemainingWorkers, State).

-spec guild_connect_inflight(session_state()) -> map().
guild_connect_inflight(State) ->
    maps:get(guild_connect_inflight, State, #{}).

-spec remove_pending_guild_connect(guild_id(), attempt(), session_state()) -> session_state().
remove_pending_guild_connect(GuildId, Attempt, State) ->
    cleanup_guild_connect_worker(
        GuildId, Attempt, remove_guild_connect_inflight(GuildId, State)
    ).

-spec remove_guild_connect_inflight(guild_id(), session_state()) -> session_state().
remove_guild_connect_inflight(GuildId, State) ->
    State1 = cancel_guild_connect_timer(GuildId, State),
    State1#{guild_connect_inflight => maps:remove(GuildId, guild_connect_inflight(State1))}.

-spec guild_connect_timers(session_state()) -> map().
guild_connect_timers(State) ->
    maps:get(guild_connect_timers, State, #{}).

-spec cancel_guild_connect_timer(guild_id(), session_state()) -> session_state().
cancel_guild_connect_timer(GuildId, State) ->
    case maps:take(GuildId, guild_connect_timers(State)) of
        {{_Token, TimerRef}, Remaining} ->
            _ = erlang:cancel_timer(TimerRef),
            State#{guild_connect_timers => Remaining};
        _ ->
            State
    end.

-spec guild_connect_workers(session_state()) -> map().
guild_connect_workers(State) ->
    maps:get(guild_connect_workers, State, #{}).

-spec put_guild_connect_workers(map(), session_state()) -> session_state().
put_guild_connect_workers(Workers, State) ->
    State#{guild_connect_workers => Workers}.

-spec handle_result_internal(
    guild_id(), attempt(), guild_connect_result(), session_state()
) -> session_result().
handle_result_internal(GuildId, _, {ok_unavailable, GuildPid, Resp}, State) ->
    ReadyFun = fun(St) ->
        session_ready:process_guild_state(Resp, St)
    end,
    finalize_guild_connection(GuildId, GuildPid, State, ReadyFun);
handle_result_internal(GuildId, Attempt, {ok_cached_unavailable, _}, State) ->
    session_connection_unavailability:mark_cached_guild_unavailable_and_retry(
        GuildId, Attempt, State
    );
handle_result_internal(GuildId, _, {ok, GuildPid, GuildState}, State) ->
    ReadyFun = fun(St) ->
        session_ready:process_guild_state(GuildState, St)
    end,
    finalize_guild_connection(GuildId, GuildPid, State, ReadyFun);
handle_result_internal(GuildId, Attempt, {error, Reason}, State) ->
    handle_result_error(GuildId, Attempt, Reason, State).

-spec handle_result_error(
    guild_id(), attempt(), term(), session_state()
) -> session_result().
handle_result_error(GuildId, Attempt, not_member, State) ->
    handle_not_member(GuildId, Attempt, State);
handle_result_error(GuildId, Attempt, {session_connect_failed, Reason}, State) ->
    log_and_retry(
        "guild_session_connect_failed", GuildId, Attempt, Reason, State
    );
handle_result_error(GuildId, Attempt, {guild_manager_failed, Reason}, State) ->
    handle_guild_manager_error(GuildId, Attempt, Reason, State);
handle_result_error(GuildId, Attempt, {exception, Reason}, State) ->
    handle_exception_error(GuildId, Attempt, Reason, State);
handle_result_error(GuildId, Attempt, Reason, State) ->
    log_and_retry(
        "guild_connect_failed", GuildId, Attempt, Reason, State
    ).

-spec handle_not_member(guild_id(), attempt(), session_state()) -> session_result().
handle_not_member(GuildId, Attempt, State) when Attempt < ?NOT_MEMBER_MAX_RETRIES ->
    Delay = backoff_utils:calculate_with_jitter(Attempt),
    erlang:send_after(Delay, self(), {guild_connect, GuildId, Attempt + 1}),
    {noreply, State};
handle_not_member(GuildId, Attempt, State) ->
    UserId = maps:get(user_id, State),
    logger:info(
        "guild_connect_not_member_removed: guild_id=~p user_id=~p attempt=~p",
        [GuildId, UserId, Attempt]
    ),
    remove_non_member_guild(GuildId, State).

-spec remove_non_member_guild(guild_id(), session_state()) -> session_result().
remove_non_member_guild(GuildId, State) ->
    Guilds0 = maps:get(guilds, State, #{}),
    demonitor_guild_ref(maps:get(GuildId, Guilds0, undefined)),
    DeleteData = #{<<"id">> => integer_to_binary(GuildId)},
    {noreply, State1} = session_dispatch:handle_dispatch(guild_delete, DeleteData, State),
    Guilds1 = maps:remove(GuildId, maps:get(guilds, State1, #{})),
    State2 = session_guilds:remove_guild_subscription_state(
        GuildId, State1#{guilds => Guilds1}
    ),
    session_ready:check_readiness(State2).

-spec demonitor_guild_ref(term()) -> ok.
demonitor_guild_ref({Pid, Ref}) when is_pid(Pid), is_reference(Ref) ->
    demonitor(Ref, [flush]),
    ok;
demonitor_guild_ref(_) ->
    ok.

-spec handle_guild_manager_error(
    guild_id(), attempt(), term(), session_state()
) -> session_result().
handle_guild_manager_error(GuildId, Attempt, {error, timeout}, State) ->
    session_connection_retry:retry_timeout_without_penalty(
        GuildId, Attempt, State
    );
handle_guild_manager_error(GuildId, Attempt, {error, loading}, State) ->
    session_connection_retry:retry_timeout_without_penalty(
        GuildId, Attempt, State
    );
handle_guild_manager_error(GuildId, _, {error, not_eligible}, State) ->
    session_ready:mark_guild_unavailable(GuildId, State);
handle_guild_manager_error(GuildId, Attempt, Reason, State) ->
    Nodedown = session_connection_retry:is_guild_manager_nodedown_failure(Reason),
    case Nodedown of
        true ->
            session_connection_retry:retry_timeout_without_penalty(
                GuildId, Attempt, State
            );
        false ->
            log_and_retry(
                "guild_connect_failed",
                GuildId,
                Attempt,
                {guild_manager_failed, Reason},
                State
            )
    end.

-spec handle_exception_error(
    guild_id(), attempt(), term(), session_state()
) -> session_result().
handle_exception_error(GuildId, Attempt, Reason, State) ->
    Transient = session_connection_retry:is_transient_connect_exception(Reason),
    case Transient of
        true ->
            session_connection_retry:retry_timeout_without_penalty(
                GuildId, Attempt, State
            );
        false ->
            log_and_retry(
                "guild_connect_failed",
                GuildId,
                Attempt,
                {exception, Reason},
                State
            )
    end.

-spec log_and_retry(
    string(), guild_id(), attempt(), term(), session_state()
) -> session_result().
log_and_retry(Msg, GuildId, Attempt, Reason, State) ->
    log_connect_failed(Msg, GuildId, Attempt, Reason, State),
    session_connection_retry:retry_or_fail(
        GuildId,
        Attempt,
        State,
        fun session_ready:mark_guild_unavailable/2
    ).

-spec log_connect_failed(
    string(), guild_id(), attempt(), term(), session_state()
) -> ok.
log_connect_failed(Msg, GuildId, Attempt, Reason, State) ->
    UserId = maps:get(user_id, State),
    logger:warning(
        Msg ++
            ": guild_id=~p user_id=~p"
            " attempt=~p reason=~p",
        [GuildId, UserId, Attempt, Reason]
    ).

-spec finalize_guild_connection(guild_id(), pid(), session_state(), fun(
    (session_state()) -> {noreply, session_state()} | {stop, normal, session_state()}
)) -> {noreply, session_state()} | {stop, normal, session_state()}.
finalize_guild_connection(GuildId, GuildPid, State, ReadyFun) ->
    Guilds0 = maps:get(guilds, State),
    case maps:find(GuildId, Guilds0) of
        {ok, {Pid, _}} when is_pid(Pid) -> {noreply, State};
        _ -> finalize_guild_monitor(GuildId, GuildPid, Guilds0, State, ReadyFun)
    end.

-spec finalize_guild_monitor(guild_id(), pid(), map(), session_state(), fun(
    (session_state()) -> {noreply, session_state()} | {stop, normal, session_state()}
)) -> {noreply, session_state()} | {stop, normal, session_state()}.
finalize_guild_monitor(GuildId, GuildPid, Guilds0, State, ReadyFun) ->
    MonitorRef = monitor(process, GuildPid),
    Guilds = Guilds0#{GuildId => {GuildPid, MonitorRef}},
    apply_ready_fun(GuildId, GuildPid, ReadyFun, State#{guilds => Guilds}).

-spec apply_ready_fun(
    guild_id(),
    pid(),
    fun((session_state()) -> session_result()),
    session_state()
) -> session_result().
apply_ready_fun(GuildId, GuildPid, ReadyFun, State) ->
    case ReadyFun(State) of
        {noreply, ReadyState} ->
            {noreply, maybe_replay_guild_subscriptions(GuildId, GuildPid, ReadyState)};
        {stop, normal, ReadyState} ->
            {stop, normal, ReadyState}
    end.

-spec maybe_replay_guild_subscriptions(guild_id(), pid(), session_state()) -> session_state().
maybe_replay_guild_subscriptions(GuildId, GuildPid, State) ->
    GuildSubscriptionState = maps:get(guild_subscription_state, State, #{}),
    case maps:find(GuildId, GuildSubscriptionState) of
        {ok, GuildSubData} when is_map(GuildSubData) ->
            SessionId = maps:get(id, State, undefined),
            SocketPid = maps:get(socket_pid, State, undefined),
            guild_unified_subscriptions:replay_guild_subscription(
                GuildId, GuildPid, GuildSubData, SessionId, SocketPid, State
            ),
            State;
        _ ->
            State
    end.

-spec repair_stalled_guild_connects(session_state()) -> session_state().
repair_stalled_guild_connects(State) ->
    Now = erlang:system_time(millisecond),
    LastRepairAt = last_repair_at(State),
    case Now - LastRepairAt >= ?GUILD_CONNECT_REPAIR_INTERVAL_MS of
        true ->
            do_repair_stalled_guild_connects(Now, State);
        false ->
            State
    end.

-spec last_repair_at(session_state()) -> integer().
last_repair_at(State) ->
    case maps:get(guild_connect_last_repair_at, State, 0) of
        At when is_integer(At) -> At;
        _ -> 0
    end.

-spec do_repair_stalled_guild_connects(integer(), session_state()) -> session_state().
do_repair_stalled_guild_connects(Now, State) ->
    Guilds = maps:get(guilds, State, #{}),
    Inflight = guild_connect_inflight(State),
    Stalled = [
        GuildId
     || {GuildId, GuildRef} <- maps:to_list(Guilds),
        is_stalled_guild_ref(GuildRef),
        not maps:is_key(GuildId, Inflight)
    ],
    ToRepair = lists:sublist(lists:sort(Stalled), ?GUILD_CONNECT_REPAIR_LIMIT),
    lists:foreach(fun(GuildId) -> self() ! {guild_connect, GuildId, 0} end, ToRepair),
    log_stalled_guild_repair(ToRepair, State),
    State#{guild_connect_last_repair_at => Now}.

-spec is_stalled_guild_ref(term()) -> boolean().
is_stalled_guild_ref(undefined) -> true;
is_stalled_guild_ref(unavailable) -> true;
is_stalled_guild_ref(_) -> false.

-spec log_stalled_guild_repair([guild_id()], session_state()) -> ok.
log_stalled_guild_repair([], _State) ->
    ok;
log_stalled_guild_repair(GuildIds, State) ->
    logger:info(
        "session_guild_connect_repair: session_id=~p user_id=~p guild_count=~p guild_ids=~p",
        [
            maps:get(id, State, undefined),
            maps:get(user_id, State, undefined),
            length(GuildIds),
            GuildIds
        ]
    ).

-spec ensure_bool(term()) -> boolean().
ensure_bool(true) -> true;
ensure_bool(_) -> false.
