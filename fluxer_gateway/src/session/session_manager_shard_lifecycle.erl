%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_manager_shard_lifecycle).
-typing([eqwalizer]).

-export([
    handle_start_call/5,
    start_session_process/4,
    start_identify_fetch/5,
    note_identify_slot_acquired/3,
    complete_identify_fetch/3,
    complete_identify_fetch/4,
    handle_identify_fetch_timeout/3,
    maybe_fail_pending_identify/4,
    cleanup_identify_worker/2,
    resolve_identify_result/4,
    reply_to_waiters/2,
    build_and_start_session/7,
    build_and_start_session/8
]).

-export_type([session_id/0, session_ref/0, state/0, pending_identify/0, start_reply/0]).

-define(IDENTIFY_FETCH_TIMEOUT_MS, 130000).

-type session_id() :: binary().
-type session_ref() :: {pid(), reference()}.
-type state() :: #{
    sessions := #{session_id() => session_ref()},
    identify_attempts := [integer()],
    pending_identifies := #{session_id() => pending_identify()},
    identify_workers := #{reference() => {session_id(), pid()}},
    shard_index := non_neg_integer(),
    _ => _
}.
-type pending_identify() :: #{
    request := map(),
    socket_pid := pid(),
    froms := [gen_server:from()],
    worker_ref := reference(),
    worker_token := reference(),
    timeout_ref := reference(),
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
            UpdatedPending = PendingIdentify#{
                request => Request,
                socket_pid => SocketPid,
                froms => [From | maps:get(froms, PendingIdentify, [])]
            },
            NewPending = PendingIdentifies#{SessionId := UpdatedPending},
            {noreply, State#{pending_identifies := NewPending}};
        false ->
            supersede_identify_fetch(
                Request, SocketPid, From, State, SessionId, PendingIdentify
            )
    end.

-spec try_start_identify(map(), pid(), gen_server:from(), state(), session_id()) ->
    {reply, start_reply(), state()} | {noreply, state()}.
try_start_identify(Request, SocketPid, From, State, SessionId) ->
    Attempts = maps:get(identify_attempts, State),
    case session_manager_shard_drain:check_identify_rate_limit(Attempts) of
        {ok, NewAttempts} ->
            NewState = State#{identify_attempts := NewAttempts},
            start_identify_fetch(Request, SocketPid, SessionId, From, NewState);
        {error, rate_limited} ->
            {reply, {error, identify_rate_limited}, State}
    end.

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
    cancel_identify_timeout(PendingIdentify),
    State1 = kill_identify_worker(
        maps:get(worker_ref, PendingIdentify), PendingIdentify, State
    ),
    RemainingPending = maps:remove(SessionId, maps:get(pending_identifies, State1)),
    Waiters = [From | maps:get(froms, PendingIdentify, [])],
    start_identify_fetch_with_waiters(
        Request,
        SocketPid,
        SessionId,
        Waiters,
        State1#{pending_identifies := RemainingPending}
    ).

-spec start_identify_fetch_with_waiters(
    map(),
    pid(),
    session_id(),
    [gen_server:from()],
    state()
) -> {noreply, state()}.
start_identify_fetch_with_waiters(Request, SocketPid, SessionId, Froms, State) ->
    ManagerPid = self(),
    PeerIP = maps:get(peer_ip, Request),
    WorkerToken = make_ref(),
    {WorkerPid, WorkerRef} = spawn_monitor(fun() ->
        erlang:process_flag(fullsweep_after, 0),
        run_identify_fetch(ManagerPid, SessionId, WorkerToken, PeerIP, Request)
    end),
    TimeoutRef = erlang:send_after(
        ?IDENTIFY_FETCH_TIMEOUT_MS, self(), {identify_fetch_timeout, SessionId, WorkerRef}
    ),
    PendingIdentifies = maps:get(pending_identifies, State),
    NewPending = PendingIdentifies#{
        SessionId => #{
            request => Request,
            socket_pid => SocketPid,
            froms => Froms,
            worker_ref => WorkerRef,
            worker_token => WorkerToken,
            timeout_ref => TimeoutRef,
            slot_held => false
        }
    },
    IdentifyWorkers = maps:get(identify_workers, State),
    NewWorkers = IdentifyWorkers#{WorkerRef => {SessionId, WorkerPid}},
    {noreply, State#{pending_identifies := NewPending, identify_workers := NewWorkers}}.

-spec note_identify_slot_acquired(session_id(), reference(), state()) -> {noreply, state()}.
note_identify_slot_acquired(SessionId, WorkerToken, State) ->
    PendingIdentifies = maps:get(pending_identifies, State),
    case maps:get(SessionId, PendingIdentifies, undefined) of
        #{worker_token := WorkerToken} = PendingIdentify ->
            Updated = PendingIdentify#{slot_held => true},
            NewPending = PendingIdentifies#{SessionId := Updated},
            {noreply, State#{pending_identifies := NewPending}};
        _ ->
            gateway_concurrency:release_session_start(),
            {noreply, State}
    end.

-spec run_identify_fetch(pid(), session_id(), reference(), term(), map()) -> ok.
run_identify_fetch(ManagerPid, SessionId, WorkerToken, PeerIP, Request) ->
    Result = acquire_and_fetch(ManagerPid, SessionId, WorkerToken, Request, PeerIP),
    ManagerPid ! {identify_fetch_result, SessionId, WorkerToken, Result},
    ok.

-spec acquire_and_fetch(pid(), session_id(), reference(), map(), term()) -> term().
acquire_and_fetch(ManagerPid, SessionId, WorkerToken, Request, PeerIP) ->
    GwTimings0 = maps:get(gw_timings, Request, gateway_timings:new()),
    AcquireFetchStartedAt = gateway_timings:start(),
    AcquireStartedAt = gateway_timings:start(),
    AcquireResult = gateway_concurrency:try_acquire_session_start(),
    AcquireSpan = gateway_timings:span(
        <<"gateway_concurrency:try_acquire_session_start/0">>, AcquireStartedAt
    ),
    case AcquireResult of
        {error, at_capacity} ->
            GwTimings = gateway_timings:record_function(
                acquire_and_fetch,
                <<"session_manager_shard_lifecycle:acquire_and_fetch/2">>,
                AcquireFetchStartedAt,
                #{children => [AcquireSpan]},
                GwTimings0
            ),
            {{error, at_capacity}, GwTimings};
        ok ->
            ManagerPid ! {identify_slot_acquired, SessionId, WorkerToken},
            do_fetch_rpc(Request, PeerIP, GwTimings0, AcquireFetchStartedAt, [AcquireSpan])
    end.

-spec do_fetch_rpc(map(), term(), gateway_timings:recorder(), integer(), [map()]) ->
    {term(), gateway_timings:recorder()}.
do_fetch_rpc(Request, PeerIP, GwTimings0, AcquireFetchStartedAt, ParentChildren) ->
    RpcStartedAt = gateway_timings:start(),
    Result = session_manager_shard_drain:fetch_rpc_data(Request, PeerIP),
    Remote = gateway_timings:api_remote_from_session_result(Result),
    RpcSpan = gateway_timings:span(
        <<"session_manager_shard_drain:fetch_rpc_data/2">>,
        RpcStartedAt,
        #{remote => Remote}
    ),
    GwTimings = gateway_timings:record_function(
        acquire_and_fetch,
        <<"session_manager_shard_lifecycle:acquire_and_fetch/2">>,
        AcquireFetchStartedAt,
        #{children => ParentChildren ++ [RpcSpan]},
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
            State1 = State#{pending_identifies := RemainingPending},
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
handle_identify_fetch_timeout(SessionId, WorkerRef, State) ->
    PendingIdentifies = maps:get(pending_identifies, State),
    case maps:get(SessionId, PendingIdentifies, undefined) of
        #{worker_ref := WorkerRef} = PendingIdentify ->
            reply_to_waiters(maps:get(froms, PendingIdentify, []), {error, timeout}),
            State1 = kill_identify_worker(WorkerRef, PendingIdentify, State),
            RemainingPending = maps:remove(SessionId, maps:get(pending_identifies, State1)),
            {noreply, State1#{pending_identifies := RemainingPending}};
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
            {noreply, State#{pending_identifies := RemainingPending}};
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

-spec kill_identify_worker(reference(), pending_identify(), state()) -> state().
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
    state()
) ->
    {reply, start_reply(), state()}.
build_and_start_session(Data, IdentifyData, Version, SocketPid, SessionId, Sessions, State) ->
    session_manager_shard_start:build_and_start_session(
        Data, IdentifyData, Version, SocketPid, SessionId, Sessions, State
    ).

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
