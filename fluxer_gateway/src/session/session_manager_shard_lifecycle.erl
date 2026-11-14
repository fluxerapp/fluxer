%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_manager_shard_lifecycle).
-typing([eqwalizer]).

-export([
    handle_start_call/5,
    start_session_process/4,
    start_identify_fetch/5,
    admit_identify_fetch/3,
    handle_identify_queue_timeout/3,
    handle_other_down/3,
    resubmit_session_starts/1,
    complete_identify_fetch/3, complete_identify_fetch/4,
    handle_identify_fetch_timeout/3,
    maybe_fail_pending_identify/4,
    cleanup_identify_worker/2,
    resolve_identify_result/4,
    reply_to_waiters/2
]).

-export_type([session_id/0, session_ref/0, state/0, pending_identify/0, start_reply/0]).

-define(IDENTIFY_QUEUE_TIMEOUT_MS, 60000).
-define(IDENTIFY_FETCH_TIMEOUT_MS, 70000).
-define(RESUBMIT_RETRY_MS, 100).

-type session_id() :: binary().
-type session_ref() :: {pid(), reference()}.
-type state() :: #{
    sessions := #{session_id() => session_ref()},
    pending_identifies := #{session_id() => pending_identify()},
    identify_workers := #{reference() => {session_id(), pid()}},
    shard_index := non_neg_integer(),
    _ => _
}.
-type pending_identify() :: #{
    request := map(),
    socket_pid := pid(),
    froms := [gen_server:from()],
    worker_ref := reference() | undefined,
    worker_token := reference(),
    timeout_ref := reference(),
    queued_at := integer(),
    socket_ref => reference(),
    slot_held => boolean()
}.
-type start_reply() ::
    {success, pid()}
    | {error, term()}.

-spec handle_start_call(boolean(), map(), pid(), gen_server:from(), state()) ->
    {reply, start_reply(), state()} | {noreply, state()}.
handle_start_call(true, _Request, _SocketPid, _From, State) ->
    {reply, {error, draining}, State};
handle_start_call(false, Request, SocketPid, From, State) ->
    Sessions = maps:get(sessions, State),
    SessionId = maps:get(session_id, Request),
    case maps:get(SessionId, Sessions, undefined) of
        {Pid, Ref} ->
            handle_existing_session(Request, SocketPid, From, State, SessionId, Pid, Ref);
        undefined ->
            handle_new_session(Request, SocketPid, From, State, SessionId)
    end.

-spec handle_existing_session(
    map(),
    pid(),
    gen_server:from(),
    state(),
    session_id(),
    pid(),
    reference()
) ->
    {reply, start_reply(), state()} | {noreply, state()}.
handle_existing_session(Request, SocketPid, From, State, SessionId, Pid, Ref) ->
    case process_liveness:is_alive(Pid) of
        true ->
            {reply, {success, Pid}, State};
        false ->
            demonitor(Ref, [flush]),
            SessionName = process_registry:build_process_key(session, SessionId),
            process_registry:safe_unregister(SessionName),
            NewState = session_manager_shard_session_index:remove(SessionId, State),
            handle_start_call(false, Request, SocketPid, From, NewState)
    end.

-spec handle_new_session(map(), pid(), gen_server:from(), state(), session_id()) ->
    {reply, start_reply(), state()} | {noreply, state()}.
handle_new_session(Request, SocketPid, From, State, SessionId) ->
    SessionName = process_registry:build_process_key(session, SessionId),
    case process_registry:registry_whereis(SessionName) of
        undefined ->
            handle_unregistered_session(Request, SocketPid, From, State, SessionId);
        Pid ->
            Ref = monitor(process, Pid),
            NewState = session_manager_shard_session_index:put(SessionId, {Pid, Ref}, State),
            {reply, {success, Pid}, NewState}
    end.

-spec handle_unregistered_session(map(), pid(), gen_server:from(), state(), session_id()) ->
    {reply, start_reply(), state()} | {noreply, state()}.
handle_unregistered_session(Request, SocketPid, From, State, SessionId) ->
    PendingIdentifies = maps:get(pending_identifies, State),
    case maps:get(SessionId, PendingIdentifies, undefined) of
        undefined ->
            try_start_identify(Request, SocketPid, From, State, SessionId);
        PendingIdentify ->
            handle_existing_pending(
                Request,
                SocketPid,
                From,
                State,
                SessionId,
                PendingIdentifies,
                PendingIdentify
            )
    end.

-spec handle_existing_pending(
    map(),
    pid(),
    gen_server:from(),
    state(),
    session_id(),
    map(),
    pending_identify()
) ->
    {reply, start_reply(), state()} | {noreply, state()}.
handle_existing_pending(
    Request,
    SocketPid,
    From,
    State,
    SessionId,
    PendingIdentifies,
    PendingIdentify
) ->
    case maps:get(request, PendingIdentify) =:= Request of
        true ->
            {Watched, WatchedState} = rewatch_socket(
                SessionId, SocketPid, PendingIdentify, State
            ),
            UpdatedPending = Watched#{
                request => Request,
                froms => [From | maps:get(froms, PendingIdentify, [])]
            },
            NewPending = PendingIdentifies#{SessionId := UpdatedPending},
            {noreply, WatchedState#{pending_identifies := NewPending}};
        false ->
            supersede_identify_fetch(
                Request, SocketPid, From, State, SessionId, PendingIdentify
            )
    end.

-spec try_start_identify(map(), pid(), gen_server:from(), state(), session_id()) ->
    {reply, start_reply(), state()} | {noreply, state()}.
try_start_identify(Request, SocketPid, From, State, SessionId) ->
    start_identify_fetch(Request, SocketPid, SessionId, From, State).

-spec start_session_process(map(), session_id(), #{session_id() => session_ref()}, state()) ->
    {reply, start_reply(), state()}.
start_session_process(SessionData, SessionId, Sessions, State) ->
    SessionName = process_registry:build_process_key(session, SessionId),
    case process_registry:registry_whereis(SessionName) of
        undefined ->
            do_start_session(SessionData, SessionId, Sessions, State, SessionName);
        ExistingPid ->
            register_existing(ExistingPid, SessionId, Sessions, State, SessionName)
    end.

-spec do_start_session(
    map(),
    session_id(),
    map(),
    state(),
    process_registry:process_key()
) -> {reply, start_reply(), state()}.
do_start_session(SessionData, SessionId, Sessions, State, SessionName) ->
    case session:start_link(SessionData) of
        {ok, Pid} ->
            register_new(Pid, SessionId, Sessions, State, SessionName);
        ignore ->
            {reply, {error, ignored}, State};
        Error ->
            {reply, Error, State}
    end.

-spec register_new(
    pid(),
    session_id(),
    map(),
    state(),
    process_registry:process_key()
) -> {reply, start_reply(), state()}.
register_new(Pid, SessionId, Sessions, State, SessionName) when is_map(Sessions) ->
    RegResult = process_registry:register_and_monitor(
        SessionName, Pid, Sessions
    ),
    case RegResult of
        {ok, RegisteredPid, Ref, NewSessions0} when is_map(NewSessions0) ->
            CleanSessions = maps:remove(SessionName, NewSessions0),
            TypedSessions = filter_session_entries(CleanSessions),
            NewSessions = TypedSessions#{SessionId => {RegisteredPid, Ref}},
            NewState = session_manager_shard_session_index:replace_sessions(NewSessions, State),
            {reply, {success, RegisteredPid}, NewState};
        {error, _Reason} ->
            {reply, {error, registration_failed}, State}
    end.

-spec filter_session_entries(process_registry:process_map()) ->
    #{session_id() => session_ref()}.
filter_session_entries(Map) ->
    maps:fold(
        fun
            (K, {Pid, Ref}, Acc) when is_binary(K), is_pid(Pid), is_reference(Ref) ->
                Acc#{K => {Pid, Ref}};
            (_, _, Acc) ->
                Acc
        end,
        #{},
        Map
    ).

-spec register_existing(
    pid(),
    session_id(),
    map(),
    state(),
    process_registry:process_key()
) -> {reply, start_reply(), state()}.
register_existing(ExistingPid, SessionId, Sessions, State, SessionName) when is_map(Sessions) ->
    Ref = monitor(process, ExistingPid),
    CleanSessions = maps:remove(SessionName, Sessions),
    NewSessions = CleanSessions#{SessionId => {ExistingPid, Ref}},
    NewState = session_manager_shard_session_index:replace_sessions(NewSessions, State),
    {reply, {success, ExistingPid}, NewState}.

-spec start_identify_fetch(map(), pid(), session_id(), gen_server:from(), state()) ->
    {noreply, state()}.
start_identify_fetch(Request, SocketPid, SessionId, From, State) ->
    start_identify_fetch_with_waiters(Request, SocketPid, SessionId, [From], State).

-spec supersede_identify_fetch(
    map(),
    pid(),
    gen_server:from(),
    state(),
    session_id(),
    pending_identify()
) -> {noreply, state()}.
supersede_identify_fetch(Request, SocketPid, From, State, SessionId, PendingIdentify) ->
    State1 = drop_pending(SessionId, PendingIdentify, State),
    Waiters = [From | maps:get(froms, PendingIdentify, [])],
    start_identify_fetch_with_waiters(Request, SocketPid, SessionId, Waiters, State1).

-spec start_identify_fetch_with_waiters(
    map(),
    pid(),
    session_id(),
    [gen_server:from()],
    state()
) -> {noreply, state()}.
start_identify_fetch_with_waiters(Request, SocketPid, SessionId, Froms, State0) ->
    WorkerToken = make_ref(),
    State = ensure_concurrency_monitor(State0),
    ok = gateway_concurrency:request_session_start({SessionId, WorkerToken}),
    TimeoutRef = erlang:send_after(
        ?IDENTIFY_QUEUE_TIMEOUT_MS, self(), {identify_queue_timeout, SessionId, WorkerToken}
    ),
    SocketRef = erlang:monitor(process, SocketPid),
    PendingIdentifies = maps:get(pending_identifies, State),
    NewPending = PendingIdentifies#{
        SessionId => #{
            request => Request,
            socket_pid => SocketPid,
            socket_ref => SocketRef,
            froms => Froms,
            worker_ref => undefined,
            worker_token => WorkerToken,
            timeout_ref => TimeoutRef,
            queued_at => gateway_timings:start(),
            slot_held => false
        }
    },
    Monitors = maps:get(socket_monitors, State, #{}),
    {noreply, State#{
        pending_identifies := NewPending,
        socket_monitors => Monitors#{SocketRef => SessionId}
    }}.

-spec rewatch_socket(session_id(), pid(), pending_identify(), state()) ->
    {pending_identify(), state()}.
rewatch_socket(SessionId, SocketPid, PendingIdentify, State) ->
    case PendingIdentify of
        #{socket_pid := SocketPid, socket_ref := _} ->
            {PendingIdentify, State};
        _ ->
            watch_socket(SessionId, SocketPid, PendingIdentify, State)
    end.

-spec watch_socket(session_id(), pid(), pending_identify(), state()) ->
    {pending_identify(), state()}.
watch_socket(SessionId, SocketPid, PendingIdentify, State) ->
    Forgotten = forget_socket(PendingIdentify, State),
    SocketRef = erlang:monitor(process, SocketPid),
    Monitors = maps:get(socket_monitors, Forgotten, #{}),
    {PendingIdentify#{socket_pid => SocketPid, socket_ref => SocketRef}, Forgotten#{
        socket_monitors => Monitors#{SocketRef => SessionId}
    }}.

-spec forget_socket(pending_identify(), state()) -> state().
forget_socket(#{socket_ref := SocketRef}, State) ->
    demonitor(SocketRef, [flush]),
    State#{socket_monitors => maps:remove(SocketRef, maps:get(socket_monitors, State, #{}))};
forget_socket(_PendingIdentify, State) ->
    State.

-spec drop_pending(session_id(), pending_identify(), state()) -> state().
drop_pending(SessionId, PendingIdentify, State) ->
    cancel_identify_timeout(PendingIdentify),
    Forgotten = forget_socket(PendingIdentify, State),
    Stopped =
        case maps:get(worker_ref, PendingIdentify, undefined) of
            undefined ->
                Tag = {SessionId, maps:get(worker_token, PendingIdentify)},
                gateway_concurrency:cancel_session_start(Tag),
                Forgotten;
            WorkerRef ->
                kill_identify_worker(WorkerRef, PendingIdentify, Forgotten)
        end,
    Stopped#{
        pending_identifies := maps:remove(SessionId, maps:get(pending_identifies, Stopped))
    }.

-spec ensure_concurrency_monitor(state()) -> state().
ensure_concurrency_monitor(State) ->
    case {maps:get(concurrency_monitor, State, undefined), whereis(gateway_concurrency)} of
        {{_Ref, Pid}, Pid} ->
            State;
        {_Monitor, undefined} ->
            State;
        {_Monitor, Pid} ->
            State#{concurrency_monitor => {erlang:monitor(process, Pid), Pid}}
    end.

-spec handle_identify_queue_timeout(session_id(), reference(), state()) -> {noreply, state()}.
handle_identify_queue_timeout(SessionId, WorkerToken, State) ->
    PendingIdentifies = maps:get(pending_identifies, State),
    case maps:get(SessionId, PendingIdentifies, undefined) of
        #{worker_token := WorkerToken, worker_ref := undefined} = PendingIdentify ->
            reply_to_waiters(maps:get(froms, PendingIdentify, []), {error, at_capacity}),
            {noreply, drop_pending(SessionId, PendingIdentify, State)};
        _ ->
            {noreply, State}
    end.

-spec handle_other_down(reference(), pid(), state()) -> {noreply, state()}.
handle_other_down(Ref, Pid, State) ->
    case maps:get(Ref, maps:get(socket_monitors, State, #{}), undefined) of
        SessionId when is_binary(SessionId) ->
            handle_socket_down(SessionId, Ref, State);
        undefined ->
            handle_non_socket_down(Ref, Pid, State)
    end.

-spec handle_socket_down(session_id(), reference(), state()) -> {noreply, state()}.
handle_socket_down(SessionId, Ref, State) ->
    Monitors = maps:remove(Ref, maps:get(socket_monitors, State, #{})),
    Cleared = State#{socket_monitors => Monitors},
    case maps:get(SessionId, maps:get(pending_identifies, Cleared), undefined) of
        #{socket_ref := Ref} = PendingIdentify ->
            reply_to_waiters(maps:get(froms, PendingIdentify, []), {error, socket_closed}),
            {noreply, drop_pending(SessionId, PendingIdentify, Cleared)};
        _ ->
            {noreply, Cleared}
    end.

-spec handle_non_socket_down(reference(), pid(), state()) -> {noreply, state()}.
handle_non_socket_down(Ref, Pid, State) ->
    case maps:get(concurrency_monitor, State, undefined) of
        {Ref, Pid} ->
            erlang:send_after(?RESUBMIT_RETRY_MS, self(), resubmit_session_starts),
            {noreply, State#{concurrency_monitor => undefined}};
        _ ->
            {noreply, session_manager_shard_session_index:cleanup_down(Ref, Pid, State)}
    end.

-spec resubmit_session_starts(state()) -> {noreply, state()}.
resubmit_session_starts(State) ->
    case ensure_concurrency_monitor(State) of
        #{concurrency_monitor := {_Ref, _Pid}} = Monitored ->
            Queued = lists:keysort(1, [
                {QueuedAt, {SessionId, Token}}
             || SessionId := #{
                    worker_ref := undefined, worker_token := Token, queued_at := QueuedAt
                } <- maps:get(pending_identifies, Monitored)
            ]),
            lists:foreach(
                fun({_QueuedAt, Tag}) -> gateway_concurrency:request_session_start(Tag) end,
                Queued
            ),
            {noreply, Monitored};
        Unmonitored ->
            erlang:send_after(?RESUBMIT_RETRY_MS, self(), resubmit_session_starts),
            {noreply, Unmonitored}
    end.

-spec admit_identify_fetch(session_id(), reference(), state()) -> {noreply, state()}.
admit_identify_fetch(SessionId, WorkerToken, State) ->
    PendingIdentifies = maps:get(pending_identifies, State),
    case maps:get(SessionId, PendingIdentifies, undefined) of
        #{worker_token := WorkerToken, worker_ref := undefined} = PendingIdentify ->
            admit_pending_identify(SessionId, PendingIdentify, State);
        _ ->
            gateway_concurrency:release_session_start(),
            {noreply, State}
    end.

-spec admit_pending_identify(session_id(), pending_identify(), state()) -> {noreply, state()}.
admit_pending_identify(SessionId, PendingIdentify, State) ->
    cancel_identify_timeout(PendingIdentify),
    ManagerPid = self(),
    Request = maps:get(request, PendingIdentify),
    WorkerToken = maps:get(worker_token, PendingIdentify),
    QueuedAt = maps:get(queued_at, PendingIdentify),
    {WorkerPid, WorkerRef} = spawn_monitor(fun() ->
        erlang:process_flag(fullsweep_after, 0),
        run_identify_fetch(ManagerPid, SessionId, WorkerToken, Request, QueuedAt)
    end),
    PendingIdentifies = maps:get(pending_identifies, State),
    TimeoutRef = erlang:send_after(
        ?IDENTIFY_FETCH_TIMEOUT_MS, self(), {identify_fetch_timeout, SessionId, WorkerToken}
    ),
    Admitted = PendingIdentify#{
        worker_ref => WorkerRef, slot_held => true, timeout_ref => TimeoutRef
    },
    IdentifyWorkers = maps:get(identify_workers, State),
    {noreply, State#{
        pending_identifies := PendingIdentifies#{SessionId := Admitted},
        identify_workers := IdentifyWorkers#{WorkerRef => {SessionId, WorkerPid}}
    }}.

-spec run_identify_fetch(pid(), session_id(), reference(), map(), integer()) -> ok.
run_identify_fetch(ManagerPid, SessionId, WorkerToken, Request, QueuedAt) ->
    Result = fetch_identify_data(Request, QueuedAt),
    ManagerPid ! {identify_fetch_result, SessionId, WorkerToken, Result},
    ok.

-spec fetch_identify_data(map(), integer()) -> {term(), gateway_timings:recorder()}.
fetch_identify_data(Request, QueuedAt) ->
    GwTimings0 = maps:get(gw_timings, Request, gateway_timings:new()),
    WaitSpan = gateway_timings:span(
        <<"gateway_concurrency:request_session_start/1">>, QueuedAt
    ),
    FetchStartedAt = gateway_timings:start(),
    Result = session_manager_shard_drain:fetch_rpc_data(Request, maps:get(peer_ip, Request)),
    Remote = gateway_timings:api_remote_from_session_result(Result),
    RpcSpan = gateway_timings:span(
        <<"session_manager_shard_drain:fetch_rpc_data/2">>,
        FetchStartedAt,
        #{remote => Remote}
    ),
    GwTimings = gateway_timings:record_function(
        fetch_identify_data,
        <<"session_manager_shard_lifecycle:fetch_identify_data/2">>,
        QueuedAt,
        #{children => [WaitSpan, RpcSpan]},
        GwTimings0
    ),
    {Result, GwTimings}.

-spec complete_identify_fetch(session_id(), term(), state()) -> {noreply, state()}.
complete_identify_fetch(SessionId, FetchResult, State) ->
    complete_identify_fetch(SessionId, undefined, FetchResult, State).

-spec complete_identify_fetch(session_id(), reference() | undefined, term(), state()) ->
    {noreply, state()}.
complete_identify_fetch(SessionId, WorkerToken, FetchResult, State) ->
    PendingIdentifies = maps:get(pending_identifies, State),
    case maps:take(SessionId, PendingIdentifies) of
        error ->
            {noreply, State};
        {PendingIdentify, RemainingPending} ->
            do_complete_identify(
                WorkerToken, FetchResult, SessionId, PendingIdentify, RemainingPending, State
            )
    end.

-spec do_complete_identify(
    reference() | undefined,
    term(),
    session_id(),
    pending_identify(),
    map(),
    state()
) -> {noreply, state()}.
do_complete_identify(
    WorkerToken, FetchResult, SessionId, PendingIdentify, RemainingPending, State
) ->
    case identify_worker_token_matches(WorkerToken, PendingIdentify) of
        false ->
            {noreply, State};
        true ->
            cancel_identify_timeout(PendingIdentify),
            release_identify_slot(PendingIdentify),
            State1 = forget_socket(PendingIdentify, State#{
                pending_identifies := RemainingPending
            }),
            State2 = cleanup_identify_worker(SessionId, State1),
            {Reply, NewState} = resolve_identify_result(
                FetchResult, PendingIdentify, SessionId, State2
            ),
            reply_to_waiters(maps:get(froms, PendingIdentify, []), Reply),
            {noreply, NewState}
    end.

-spec release_identify_slot(pending_identify()) -> ok.
release_identify_slot(PendingIdentify) ->
    case maps:get(slot_held, PendingIdentify, false) of
        true ->
            gateway_concurrency:release_session_start(),
            ok;
        _ ->
            ok
    end.

-spec handle_identify_fetch_timeout(session_id(), reference(), state()) -> {noreply, state()}.
handle_identify_fetch_timeout(SessionId, WorkerToken, State) ->
    PendingIdentifies = maps:get(pending_identifies, State),
    case maps:get(SessionId, PendingIdentifies, undefined) of
        #{worker_token := WorkerToken, worker_ref := WorkerRef} = PendingIdentify when
            is_reference(WorkerRef)
        ->
            reply_to_waiters(maps:get(froms, PendingIdentify, []), {error, timeout}),
            {noreply, drop_pending(SessionId, PendingIdentify, State)};
        _Other ->
            {noreply, State}
    end.

-spec maybe_fail_pending_identify(session_id(), reference() | undefined, term(), state()) ->
    {noreply, state()}.
maybe_fail_pending_identify(_SessionId, _WorkerRef, Reason, State) when
    Reason =:= normal; Reason =:= shutdown
->
    {noreply, State};
maybe_fail_pending_identify(SessionId, WorkerRef, Reason, State) ->
    PendingIdentifies = maps:get(pending_identifies, State),
    case maps:take(SessionId, PendingIdentifies) of
        error ->
            {noreply, State};
        {PendingIdentify, RemainingPending} ->
            do_fail_pending(WorkerRef, Reason, PendingIdentify, RemainingPending, State)
    end.

-spec do_fail_pending(reference() | undefined, term(), pending_identify(), map(), state()) ->
    {noreply, state()}.
do_fail_pending(WorkerRef, Reason, PendingIdentify, RemainingPending, State) ->
    case identify_worker_ref_matches(WorkerRef, PendingIdentify) of
        true ->
            cancel_identify_timeout(PendingIdentify),
            release_identify_slot(PendingIdentify),
            reply_to_waiters(
                maps:get(froms, PendingIdentify, []), {error, {network_error, Reason}}
            ),
            {noreply,
                forget_socket(PendingIdentify, State#{pending_identifies := RemainingPending})};
        false ->
            {noreply, State}
    end.

-spec identify_worker_token_matches(reference() | undefined, pending_identify()) -> boolean().
identify_worker_token_matches(undefined, PendingIdentify) ->
    maps:get(worker_token, PendingIdentify, undefined) =:= undefined;
identify_worker_token_matches(WorkerToken, PendingIdentify) ->
    maps:get(worker_token, PendingIdentify, undefined) =:= WorkerToken.

-spec identify_worker_ref_matches(reference() | undefined, pending_identify()) -> boolean().
identify_worker_ref_matches(undefined, _PendingIdentify) ->
    true;
identify_worker_ref_matches(WorkerRef, PendingIdentify) ->
    maps:get(worker_ref, PendingIdentify, undefined) =:= WorkerRef.

-spec cleanup_identify_worker(session_id(), state()) -> state().
cleanup_identify_worker(SessionId, State) ->
    IdentifyWorkers = maps:get(identify_workers, State),
    RemainingWorkers = maps:fold(
        fun
            (Ref, {WorkerSessionId, _Pid}, Acc) when WorkerSessionId =:= SessionId ->
                demonitor(Ref, [flush]),
                Acc;
            (Ref, WorkerInfo, Acc) ->
                Acc#{Ref => WorkerInfo}
        end,
        #{},
        IdentifyWorkers
    ),
    State#{identify_workers := RemainingWorkers}.

-spec resolve_identify_result(term(), pending_identify(), session_id(), state()) ->
    {start_reply(), state()}.
resolve_identify_result(FetchResult0, PendingIdentify, SessionId, State) ->
    Request = maps:get(request, PendingIdentify),
    {FetchResult, GwTimings} = split_fetch_result(FetchResult0, Request),
    resolve_identify_result(FetchResult, GwTimings, PendingIdentify, SessionId, State).

-spec resolve_identify_result(
    term(), gateway_timings:recorder(), pending_identify(), session_id(), state()
) ->
    {start_reply(), state()}.
resolve_identify_result({ok, Data}, GwTimings, PendingIdentify, SessionId, State) when
    is_map(Data)
->
    Request = maps:get(request, PendingIdentify),
    IdentifyData = maps:get(identify_data, Request),
    Version = maps:get(version, Request),
    SocketPid = maps:get(socket_pid, PendingIdentify),
    Sessions = maps:get(sessions, State),
    case process_liveness:is_alive(SocketPid) of
        true ->
            {reply, Reply, NewState} = build_and_start_session(
                Data,
                IdentifyData,
                Version,
                SocketPid,
                SessionId,
                Sessions,
                State,
                GwTimings
            ),
            {Reply, NewState};
        false ->
            {{error, socket_closed}, State}
    end;
resolve_identify_result({ok, _Data}, _GwTimings, _PendingIdentify, _SessionId, State) ->
    {{error, invalid_identify_payload}, State};
resolve_identify_result({error, Reason}, _GwTimings, _PendingIdentify, _SessionId, State) ->
    {{error, Reason}, State}.

-spec split_fetch_result(term(), map()) -> {term(), gateway_timings:recorder()}.
split_fetch_result({Result, GwTimings} = FetchResult, Request) ->
    case gateway_timings:is_recorder(GwTimings) of
        true -> {Result, eqwalizer:dynamic_cast(GwTimings)};
        false -> {FetchResult, maps:get(gw_timings, Request, gateway_timings:new())}
    end;
split_fetch_result(Result, Request) ->
    {Result, maps:get(gw_timings, Request, gateway_timings:new())}.

-spec kill_identify_worker(reference() | undefined, pending_identify(), state()) -> state().
kill_identify_worker(undefined, _PendingIdentify, State) ->
    State;
kill_identify_worker(WorkerRef, PendingIdentify, State) ->
    IdentifyWorkers = maps:get(identify_workers, State),
    case maps:take(WorkerRef, IdentifyWorkers) of
        {{_SessionId, WorkerPid}, RemainingWorkers} ->
            demonitor(WorkerRef, [flush]),
            release_identify_slot(PendingIdentify),
            exit(WorkerPid, kill),
            State#{identify_workers := RemainingWorkers};
        error ->
            State
    end.

-spec cancel_identify_timeout(pending_identify()) -> ok.
cancel_identify_timeout(PendingIdentify) ->
    case maps:get(timeout_ref, PendingIdentify, undefined) of
        TimeoutRef when is_reference(TimeoutRef) ->
            _ = erlang:cancel_timer(TimeoutRef),
            ok;
        _Other ->
            ok
    end.

-spec reply_to_waiters([gen_server:from()], start_reply()) -> ok.
reply_to_waiters(Waiters, Reply) ->
    lists:foreach(fun(From) -> gen_server:reply(From, Reply) end, Waiters),
    ok.

-spec build_and_start_session(
    map(),
    map(),
    non_neg_integer(),
    pid(),
    session_id(),
    #{session_id() => session_ref()},
    state(),
    gateway_timings:recorder()
) ->
    {reply, start_reply(), state()}.
build_and_start_session(
    Data, IdentifyData, Version, SocketPid, SessionId, Sessions, State, GwTimings0
) ->
    session_manager_shard_start:build_and_start_session(
        Data, IdentifyData, Version, SocketPid, SessionId, Sessions, State, GwTimings0
    ).
