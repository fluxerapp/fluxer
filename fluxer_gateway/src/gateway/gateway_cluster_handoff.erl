%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_cluster_handoff).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/0, drain_async/0, undrain/0, diagnostic_info/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(DEBOUNCE_MS, 2000).
-define(RECONCILE_MS, 60000).
-define(HANDOFF_WORKER_TIMEOUT_MS, 30000).

-type timer_state() :: undefined | {reference(), reference()}.
-type reconcile_timer() :: undefined | reference().
-type handoff_state() ::
    undefined
    | #{
        pid := pid(),
        ref := reference(),
        timer := reference(),
        members := [node()],
        role_members := #{atom() => [node()]}
    }
    | {pid(), reference(), [node()], #{atom() => [node()]}}.
-type state() :: #{
    members := [node()],
    role_members := #{atom() => [node()]},
    pending_members := undefined | [node()],
    timer := timer_state(),
    reconcile_timer := reconcile_timer(),
    handoff := handoff_state(),
    last_result := map()
}.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec drain_async() -> ok.
drain_async() ->
    persistent_term:put({fluxer_gateway, draining}, true),
    ok = drain_notify_role(),
    drain_dispatch().

-spec undrain() -> ok.
undrain() ->
    persistent_term:erase({fluxer_gateway, draining}),
    logger:info("Gateway un-cordoned: draining flag cleared"),
    ok.

-spec diagnostic_info() -> map().
diagnostic_info() ->
    try gen_server:call(?MODULE, diagnostic_info, 5000) of
        Info when is_map(Info) -> Info;
        _ -> #{}
    catch
        throw:_ -> #{};
        error:_ -> #{};
        exit:_ -> #{}
    end.

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 0),
    Normalize = fun gateway_cluster_handoff_transfer:normalize_members/1,
    Members = Normalize(gateway_cluster_membership:members()),
    RoleMembers = current_role_members(),
    ok = gateway_cluster_membership:subscribe(self()),
    {ok, #{
        members => Members,
        role_members => RoleMembers,
        pending_members => undefined,
        timer => undefined,
        reconcile_timer => schedule_reconcile(?DEBOUNCE_MS),
        handoff => undefined,
        last_result => #{}
    }}.

-spec handle_call(term(), gen_server:from(), state()) ->
    {reply, term(), state()}.
handle_call(diagnostic_info, _From, State) ->
    {reply, info(State), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(drain, State) ->
    cancel_timer(maps:get(timer, State, undefined)),
    DrainMembers = gateway_cluster_membership:members(),
    TargetMembers = gateway_cluster_handoff_transfer:drain_targets(DrainMembers),
    State1 = State#{pending_members := TargetMembers, timer := undefined},
    {noreply, maybe_start_handoff(State1)};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({cluster_membership_changed, Members}, State) ->
    Normalized = gateway_cluster_handoff_transfer:normalize_members(Members),
    {noreply, schedule_if_changed(Normalized, State)};
handle_info({handoff_timer, Token}, #{timer := {_Ref, Token}} = State) ->
    {noreply, maybe_start_handoff(State#{timer := undefined})};
handle_info({handoff_timer, _StaleToken}, State) ->
    {noreply, State};
handle_info(reconcile_topology, State) ->
    State1 = State#{reconcile_timer := schedule_reconcile(?RECONCILE_MS)},
    Members = gateway_cluster_handoff_transfer:normalize_members(
        gateway_cluster_membership:members()
    ),
    {noreply, maybe_start_handoff(schedule_if_changed(Members, State1))};
handle_info(
    {handoff_complete, Pid, Members, Result},
    #{handoff := Handoff} = State
) when is_pid(Pid), is_list(Members) ->
    NodeMembers = eqwalizer:dynamic_cast(Members),
    case handoff_completion_matches(Handoff, Pid, NodeMembers) of
        true ->
            cancel_handoff_timer(Handoff),
            demonitor_handoff_ref(handoff_ref(Handoff)),
            ResultMap = handoff_result_map(Result),
            {noreply,
                complete_handoff(NodeMembers, ResultMap, handoff_role_members(Handoff), State)};
        false ->
            {noreply, State}
    end;
handle_info(
    {'DOWN', Ref, process, _Pid, Reason},
    #{handoff := Handoff} = State
) ->
    case handoff_ref(Handoff) of
        Ref ->
            cancel_handoff_timer(Handoff),
            Result = #{status => error, reason => Reason},
            State1 = State#{handoff := undefined, last_result := Result},
            {noreply, retry_after_handoff_failure(handoff_members(Handoff), State1)};
        _ ->
            {noreply, State}
    end;
handle_info({handoff_worker_timeout, Ref}, #{handoff := Handoff} = State) ->
    case handoff_ref(Handoff) of
        Ref ->
            cancel_handoff_timer(Handoff),
            demonitor_handoff_ref(Ref),
            maybe_kill_handoff_pid(handoff_pid(Handoff)),
            Result = #{status => error, reason => timeout},
            State1 = State#{handoff := undefined, last_result := Result},
            {noreply, retry_after_handoff_failure(handoff_members(Handoff), State1)};
        _ ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    cancel_timer(maps:get(timer, State, undefined)),
    cancel_reconcile_timer(maps:get(reconcile_timer, State, undefined)),
    cleanup_handoff_worker(maps:get(handoff, State, undefined)),
    shard_utils:safe_apply(fun() -> gateway_cluster_membership:unsubscribe(self()) end, ok),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:process_flag(fullsweep_after, 0),
    erlang:garbage_collect(),
    {ok, State}.

-spec drain_notify_role() -> ok.
drain_notify_role() ->
    Role = fluxer_gateway_sup:current_role(),
    case Role of
        websocket ->
            Count = gateway_cluster_handoff_transfer:broadcast_ws_reconnect(),
            logger:info(
                "drain: broadcast session_reconnect to ~p "
                "ws processes (role=websocket)",
                [Count]
            );
        all ->
            _ = shard_utils:safe_apply(
                fun session_manager:reconnect_drain/0,
                {error, unavailable}
            ),
            ok;
        _ ->
            ok
    end.

-spec drain_dispatch() -> ok.
drain_dispatch() ->
    case whereis(?MODULE) of
        undefined ->
            ok = fallback_drain_handoff(),
            ok;
        _Pid ->
            shard_utils:safe_apply(fun cast_drain/0, ok),
            ok
    end.

-spec fallback_drain_handoff() -> ok.
fallback_drain_handoff() ->
    erlang:process_flag(fullsweep_after, 0),
    Targets = gateway_cluster_handoff_transfer:drain_targets(
        gateway_cluster_membership:members()
    ),
    _ = gateway_cluster_handoff_transfer:run_handoff(Targets),
    ok.

-spec cast_drain() -> ok.
cast_drain() ->
    gen_server:cast(?MODULE, drain).

-spec complete_handoff([node()], map(), #{atom() => [node()]}, state()) -> state().
complete_handoff(Members, Result, HandoffRoleMembers, State) ->
    case maps:get(status, Result, error) of
        ok ->
            State1 = State#{
                members := Members,
                role_members := HandoffRoleMembers,
                handoff := undefined,
                last_result := Result
            },
            maybe_reschedule_pending(State1);
        _ ->
            State1 = State#{
                handoff := undefined,
                last_result := Result
            },
            retry_after_handoff_failure(Members, State1)
    end.

-spec schedule_if_changed([node()], state()) -> state().
schedule_if_changed(
    Members,
    #{members := Members, pending_members := undefined} = S
) ->
    RoleMembers = current_role_members(),
    case maps:get(role_members, S, RoleMembers) =:= RoleMembers of
        true -> S;
        false -> schedule_handoff(Members, S)
    end;
schedule_if_changed(Members, State) ->
    schedule_handoff(Members, State).

-spec schedule_handoff([node()], state()) -> state().
schedule_handoff(Members, State) ->
    cancel_timer(maps:get(timer, State, undefined)),
    Token = make_ref(),
    TimerRef = erlang:send_after(
        ?DEBOUNCE_MS,
        self(),
        {handoff_timer, Token}
    ),
    State#{pending_members := Members, timer := {TimerRef, Token}}.

-spec current_role_members() -> #{atom() => [node()]}.
current_role_members() ->
    RoleMembers = gateway_cluster_membership:members_by_role(),
    normalize_role_members(RoleMembers).

-spec normalize_role_members(term()) -> #{atom() => [node()]}.
normalize_role_members(RoleMembers) when is_map(RoleMembers) ->
    maps:fold(
        fun
            (Role, Nodes, Acc) when is_atom(Role), is_list(Nodes) ->
                Acc#{Role => gateway_cluster_handoff_transfer:normalize_members(Nodes)};
            (_Role, _Nodes, Acc) ->
                Acc
        end,
        #{},
        RoleMembers
    ).

-spec maybe_start_handoff(state()) -> state().
maybe_start_handoff(#{handoff := {_Pid, _Ref, _Members, _HandoffRoleMembers}} = State) ->
    State;
maybe_start_handoff(#{pending_members := undefined} = State) ->
    State;
maybe_start_handoff(#{pending_members := Members} = State) ->
    Parent = self(),
    HandoffMembers = Members,
    HandoffRoleMembers = current_role_members(),
    {Pid, Ref} = spawn_monitor(fun() ->
        erlang:process_flag(fullsweep_after, 0),
        Result = gateway_cluster_handoff_transfer:run_handoff(HandoffMembers),
        Parent ! {handoff_complete, self(), HandoffMembers, Result}
    end),
    TimerRef = erlang:send_after(
        ?HANDOFF_WORKER_TIMEOUT_MS, self(), {handoff_worker_timeout, Ref}
    ),
    State#{
        handoff := #{
            pid => Pid,
            ref => Ref,
            timer => TimerRef,
            members => Members,
            role_members => HandoffRoleMembers
        },
        pending_members := undefined
    }.

-spec maybe_reschedule_pending(state()) -> state().
maybe_reschedule_pending(#{pending_members := undefined} = State) ->
    State;
maybe_reschedule_pending(#{pending_members := Members} = State) ->
    schedule_if_changed(Members, State#{pending_members := undefined}).

-spec cancel_timer(timer_state()) -> ok.
cancel_timer(undefined) ->
    ok;
cancel_timer({TimerRef, _Token}) ->
    _ = erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
    ok.

-spec schedule_reconcile(pos_integer()) -> reference().
schedule_reconcile(DelayMs) ->
    erlang:send_after(DelayMs, self(), reconcile_topology).
-spec cancel_reconcile_timer(reconcile_timer()) -> ok.
cancel_reconcile_timer(undefined) ->
    ok;
cancel_reconcile_timer(TimerRef) ->
    _ = erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
    ok.

-spec retry_after_handoff_failure([node()], state()) -> state().
retry_after_handoff_failure(Members, #{pending_members := undefined} = State) ->
    schedule_if_changed(Members, State);
retry_after_handoff_failure(_Members, State) ->
    maybe_reschedule_pending(State).

-spec handoff_result_map(term()) -> map().
handoff_result_map(Result) when is_map(Result) ->
    Result;
handoff_result_map(_Result) ->
    #{status => error, reason => bad_result}.

-spec handoff_completion_matches(handoff_state(), pid(), [node()]) -> boolean().
handoff_completion_matches(Handoff, Pid, Members) ->
    handoff_pid(Handoff) =:= Pid andalso handoff_members(Handoff) =:= Members.

-spec handoff_pid(handoff_state()) -> pid() | undefined.
handoff_pid(#{pid := Pid}) when is_pid(Pid) -> Pid;
handoff_pid({Pid, _Ref, _Members, _RoleMembers}) when is_pid(Pid) -> Pid;
handoff_pid(_) -> undefined.

-spec handoff_ref(handoff_state()) -> reference() | undefined.
handoff_ref(#{ref := Ref}) when is_reference(Ref) -> Ref;
handoff_ref({_Pid, Ref, _Members, _RoleMembers}) when is_reference(Ref) -> Ref;
handoff_ref(_) -> undefined.

-spec demonitor_handoff_ref(reference() | undefined) -> ok.
demonitor_handoff_ref(Ref) when is_reference(Ref) ->
    erlang:demonitor(Ref, [flush]),
    ok;
demonitor_handoff_ref(undefined) ->
    ok.

-spec handoff_members(handoff_state()) -> [node()].
handoff_members(#{members := Members}) when is_list(Members) -> Members;
handoff_members({_Pid, _Ref, Members, _RoleMembers}) when is_list(Members) -> Members;
handoff_members(_) -> [].

-spec handoff_role_members(handoff_state()) -> #{atom() => [node()]}.
handoff_role_members(#{role_members := RoleMembers}) when is_map(RoleMembers) -> RoleMembers;
handoff_role_members({_Pid, _Ref, _Members, RoleMembers}) when is_map(RoleMembers) ->
    RoleMembers;
handoff_role_members(_) ->
    #{}.

-spec cancel_handoff_timer(handoff_state()) -> ok.
cancel_handoff_timer(#{timer := TimerRef}) when is_reference(TimerRef) ->
    _ = erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
    ok;
cancel_handoff_timer(_) ->
    ok.

-spec cleanup_handoff_worker(handoff_state()) -> ok.
cleanup_handoff_worker(undefined) ->
    ok;
cleanup_handoff_worker(Handoff) ->
    cancel_handoff_timer(Handoff),
    case handoff_ref(Handoff) of
        Ref when is_reference(Ref) -> erlang:demonitor(Ref, [flush]);
        undefined -> ok
    end,
    maybe_kill_handoff_pid(handoff_pid(Handoff)).

-spec maybe_kill_handoff_pid(pid() | undefined) -> ok.
maybe_kill_handoff_pid(Pid) when is_pid(Pid) ->
    exit(Pid, kill),
    ok;
maybe_kill_handoff_pid(undefined) ->
    ok.

-spec info(state()) -> map().
info(State) ->
    #{
        members => maps:get(members, State),
        role_members => maps:get(role_members, State, #{}),
        pending_members => maps:get(pending_members, State),
        reconcile_timer_active =>
            maps:get(reconcile_timer, State, undefined) =/= undefined,
        handoff_in_flight => maps:get(handoff, State) =/= undefined,
        last_result => maps:get(last_result, State)
    }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_WAIT_LOOP_TIMEOUT_MS, 60000).

-spec restore_persistent_term(term(), term()) -> ok.
restore_persistent_term(Key, undefined) ->
    persistent_term:erase(Key),
    ok;
restore_persistent_term(Key, Value) ->
    persistent_term:put(Key, Value),
    ok.

-spec complete_handoff([node()], map(), state()) -> state().
complete_handoff(Members, Result, State0) ->
    {noreply, State1} = handle_info({handoff_complete, self(), Members, Result}, State0),
    State1.

-spec idle_state([node()]) -> state().
idle_state(Members) ->
    #{
        members => Members,
        role_members => #{},
        pending_members => undefined,
        timer => undefined,
        reconcile_timer => undefined,
        handoff => undefined,
        last_result => #{}
    }.

schedule_if_changed_sets_pending_test() ->
    State0 = idle_state([node()]),
    Members = lists:usort([node(), 'peer@host']),
    State1 = schedule_if_changed(Members, State0),
    ?assertEqual(Members, maps:get(pending_members, State1)),
    ?assertMatch({_TimerRef, _Token}, maps:get(timer, State1)),
    cancel_timer(maps:get(timer, State1)).

schedule_if_changed_sets_pending_on_role_topology_change_test() ->
    PreviousRoleMembers = persistent_term:get(
        {gateway_cluster_membership, members_by_role}, undefined
    ),
    Members = lists:usort([node(), 'peer@host']),
    persistent_term:put({gateway_cluster_membership, members_by_role}, #{
        guilds => Members
    }),
    State0 = #{
        members => Members,
        role_members => #{sessions => Members},
        pending_members => undefined,
        timer => undefined,
        reconcile_timer => undefined,
        handoff => undefined,
        last_result => #{}
    },
    State1 = schedule_if_changed(Members, State0),
    ?assertEqual(Members, maps:get(pending_members, State1)),
    ?assertMatch({_TimerRef, _Token}, maps:get(timer, State1)),
    cancel_timer(maps:get(timer, State1)),
    restore_persistent_term(
        {gateway_cluster_membership, members_by_role}, PreviousRoleMembers
    ).

handoff_complete_records_success_test() ->
    Members = lists:usort([node(), 'peer@host']),
    Result = #{status => ok, guilds => ok},
    State0 = #{
        members => [node()],
        role_members => #{},
        pending_members => undefined,
        timer => undefined,
        reconcile_timer => undefined,
        handoff => {self(), make_ref(), Members, #{}},
        last_result => #{}
    },
    State1 = complete_handoff(Members, Result, State0),
    ?assertEqual(Members, maps:get(members, State1)),
    ?assertEqual(undefined, maps:get(handoff, State1)),
    ?assertEqual(Result, maps:get(last_result, State1)).

handoff_complete_reschedules_pending_members_test() ->
    CompletedMembers = lists:usort([node(), 'peer@a']),
    PendingMembers = lists:usort([node(), 'peer@b']),
    State0 = #{
        members => [node()],
        role_members => #{},
        pending_members => PendingMembers,
        timer => undefined,
        reconcile_timer => undefined,
        handoff => {self(), make_ref(), CompletedMembers, #{}},
        last_result => #{}
    },
    State1 = complete_handoff(CompletedMembers, #{status => ok}, State0),
    ?assertEqual(CompletedMembers, maps:get(members, State1)),
    ?assertEqual(PendingMembers, maps:get(pending_members, State1)),
    ?assertMatch({_TimerRef, _Token}, maps:get(timer, State1)),
    cancel_timer(maps:get(timer, State1)).

handoff_complete_failure_keeps_old_members_and_retries_test() ->
    OldMembers = [node()],
    Members = lists:usort([node(), 'peer@host']),
    Result = #{status => error, guilds => #{attempted => 1, handed_off => 0}},
    State0 = #{
        members => OldMembers,
        role_members => #{},
        pending_members => Members,
        timer => undefined,
        reconcile_timer => undefined,
        handoff => {self(), make_ref(), Members, #{}},
        last_result => #{}
    },
    State1 = complete_handoff(Members, Result, State0),
    ?assertEqual(OldMembers, maps:get(members, State1)),
    ?assertEqual(Members, maps:get(pending_members, State1)),
    ?assertEqual(Result, maps:get(last_result, State1)),
    ?assertMatch({_TimerRef, _Token}, maps:get(timer, State1)),
    cancel_timer(maps:get(timer, State1)).

stale_handoff_complete_is_ignored_test() ->
    Members = lists:usort([node(), 'peer@host']),
    Pid = spawn(fun handoff_wait_loop/0),
    Ref = erlang:monitor(process, Pid),
    TimerRef = erlang:send_after(60000, self(), stale_handoff_timeout),
    Handoff = #{
        pid => Pid,
        ref => Ref,
        timer => TimerRef,
        members => Members,
        role_members => #{}
    },
    State0 = #{
        members => [node()],
        role_members => #{},
        pending_members => undefined,
        timer => undefined,
        reconcile_timer => undefined,
        handoff => Handoff,
        last_result => #{}
    },
    {noreply, State1} = handle_info(
        {handoff_complete, self(), Members, #{status => ok}},
        State0
    ),
    ?assertEqual(State0, State1),
    cleanup_handoff_worker(Handoff).

handoff_worker_timeout_records_timeout_and_retries_test() ->
    OldMembers = [node()],
    Members = lists:usort([node(), 'peer@host']),
    Pid = spawn(fun handoff_wait_loop/0),
    Ref = erlang:monitor(process, Pid),
    TimerRef = erlang:send_after(60000, self(), stale_handoff_timeout),
    Handoff = #{
        pid => Pid,
        ref => Ref,
        timer => TimerRef,
        members => Members,
        role_members => #{}
    },
    State0 = #{
        members => OldMembers,
        role_members => #{},
        pending_members => undefined,
        timer => undefined,
        reconcile_timer => undefined,
        handoff => Handoff,
        last_result => #{}
    },
    {noreply, State1} = handle_info({handoff_worker_timeout, Ref}, State0),
    ?assertEqual(OldMembers, maps:get(members, State1)),
    ?assertEqual(undefined, maps:get(handoff, State1)),
    ?assertEqual(#{status => error, reason => timeout}, maps:get(last_result, State1)),
    ?assertEqual(Members, maps:get(pending_members, State1)),
    ?assertMatch({_TimerRef, _Token}, maps:get(timer, State1)),
    gateway_retry_timer:wait(10),
    ?assertNot(erlang:is_process_alive(Pid)),
    cancel_timer(maps:get(timer, State1)).

reconcile_topology_skips_unchanged_current_topology_test() ->
    PreviousMembers = persistent_term:get({gateway_cluster_membership, members}, undefined),
    PreviousRoleMembers = persistent_term:get(
        {gateway_cluster_membership, members_by_role}, undefined
    ),
    persistent_term:put({gateway_cluster_membership, members}, [node()]),
    persistent_term:put({gateway_cluster_membership, members_by_role}, #{}),
    State0 = idle_state([node()]),
    {noreply, State1} = handle_info(reconcile_topology, State0),
    ?assertNotEqual(undefined, maps:get(reconcile_timer, State1)),
    ?assertEqual(undefined, maps:get(timer, State1)),
    ?assertEqual(undefined, maps:get(handoff, State1)),
    cancel_reconcile_timer(maps:get(reconcile_timer, State1)),
    restore_persistent_term({gateway_cluster_membership, members}, PreviousMembers),
    restore_persistent_term(
        {gateway_cluster_membership, members_by_role}, PreviousRoleMembers
    ).

undrain_clears_draining_flag_test() ->
    Previous = persistent_term:get({fluxer_gateway, draining}, undefined),
    persistent_term:put({fluxer_gateway, draining}, true),
    ?assertEqual(ok, undrain()),
    ?assertEqual(false, persistent_term:get({fluxer_gateway, draining}, false)),
    restore_persistent_term({fluxer_gateway, draining}, Previous).

handoff_wait_loop() ->
    receive
        stop -> ok
    after ?TEST_WAIT_LOOP_TIMEOUT_MS ->
        ok
    end.

-endif.
