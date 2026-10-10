%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(call_manager_ops).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    do_create_call/3,
    do_handoff_to_topology/2,
    do_lookup_call/2,
    do_start_transferred_call/3,
    do_stop_call/3,
    do_terminate_call/2,
    collect_active_call_ids/1
]).

-export_type([channel_id/0, call_data/0, state/0, handoff_result/0]).

-type channel_id() :: integer().
-type call_data() :: map().
-type state() :: #{calls := map()}.
-type handoff_result() :: #{attempted := non_neg_integer(), handed_off := non_neg_integer()}.

-spec do_create_call(channel_id(), call_data(), state()) ->
    {reply, {ok, pid()} | {error, already_exists | term()}, state()}.
do_create_call(ChannelId, CallData, #{calls := Calls} = State) ->
    case maps:get(ChannelId, Calls, undefined) of
        {Pid, _Ref} when is_pid(Pid) ->
            handle_existing_create(Pid, ChannelId, CallData, Calls, State);
        undefined ->
            attempt_create(ChannelId, CallData, Calls, State)
    end.

-spec handle_existing_create(pid(), channel_id(), call_data(), map(), state()) ->
    {reply, {ok, pid()} | {error, already_exists | term()}, state()}.
handle_existing_create(Pid, ChannelId, CallData, Calls, State) ->
    case process_liveness:is_alive(Pid) of
        true ->
            {reply, {error, already_exists}, State};
        false ->
            cleanup_stale(ChannelId),
            do_create_call(ChannelId, CallData, State#{calls := maps:remove(ChannelId, Calls)})
    end.

-spec attempt_create(channel_id(), call_data(), map(), state()) ->
    {reply, {ok, pid()} | {error, already_exists | term()}, state()}.
attempt_create(ChannelId, CallData, Calls, State) ->
    CallName = process_registry:build_process_key(call, ChannelId),
    case process_registry:registry_whereis(CallName) of
        undefined ->
            start_new_call(ChannelId, CallData, CallName, Calls, State);
        _ExistingPid ->
            {reply, {error, already_exists}, State}
    end.

-spec start_new_call(channel_id(), call_data(), process_registry:process_key(), map(), state()) ->
    {reply, {ok, pid()} | {error, term()}, state()}.
start_new_call(ChannelId, CallData, CallName, Calls, State) ->
    start_call_from_result(ChannelId, CallName, call:start_link(CallData), Calls, State).

-spec start_call_from_result(
    channel_id(), process_registry:process_key(), {ok, pid()} | {error, term()}, map(), state()
) -> {reply, {ok, pid()} | {error, term()}, state()}.
start_call_from_result(ChannelId, CallName, StartResult, Calls, State) ->
    case StartResult of
        {ok, Pid} ->
            start_registered_call(ChannelId, CallName, Pid, Calls, State);
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

-spec do_start_transferred_call(channel_id(), map(), state()) ->
    {reply, {ok, pid()} | {error, term()}, state()}.
do_start_transferred_call(ChannelId, TransferState, #{calls := Calls} = State) when
    is_map(TransferState)
->
    case ensure_local_owner(ChannelId) of
        ok ->
            ensure_start_transferred_call(ChannelId, TransferState, Calls, State);
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
do_start_transferred_call(_ChannelId, _TransferState, State) ->
    {reply, {error, invalid_transfer_state}, State}.

-spec ensure_start_transferred_call(channel_id(), map(), map(), state()) ->
    {reply, {ok, pid()} | {error, term()}, state()}.
ensure_start_transferred_call(ChannelId, TransferState, Calls, State) ->
    CallName = process_registry:build_process_key(call, ChannelId),
    case process_registry:registry_whereis(CallName) of
        undefined ->
            start_transferred_call(ChannelId, TransferState, CallName, Calls, State);
        ExistingPid when is_pid(ExistingPid) ->
            use_existing_transferred_call(ChannelId, ExistingPid, Calls, State)
    end.

-spec use_existing_transferred_call(channel_id(), pid(), map(), state()) ->
    {reply, {ok, pid()}, state()}.
use_existing_transferred_call(ChannelId, ExistingPid, Calls, State) ->
    Ref = monitor(process, ExistingPid),
    ets:insert(call_pid_cache, {ChannelId, ExistingPid}),
    {reply, {ok, ExistingPid}, State#{calls := Calls#{ChannelId => {ExistingPid, Ref}}}}.

-spec start_transferred_call(
    channel_id(), map(), process_registry:process_key(), map(), state()
) ->
    {reply, {ok, pid()} | {error, term()}, state()}.
start_transferred_call(ChannelId, TransferState, CallName, Calls, State) ->
    StartResult = call:start_link_from_state(TransferState#{channel_id => ChannelId}),
    start_call_from_result(ChannelId, CallName, StartResult, Calls, State).

-spec start_registered_call(
    channel_id(), process_registry:process_key(), pid(), map(), state()
) ->
    {reply, {ok, pid()} | {error, term()}, state()}.
start_registered_call(ChannelId, CallName, Pid, Calls, State) ->
    case process_registry:register_and_monitor(CallName, Pid, Calls) of
        {ok, RegPid, Ref, NewCalls0} ->
            register_call(ChannelId, RegPid, Ref, NewCalls0, State);
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

-spec register_call(channel_id(), pid(), reference(), map(), state()) ->
    {reply, {ok, pid()}, state()}.
register_call(ChannelId, RegPid, Ref, NewCalls0, State) ->
    CleanCalls = maps:remove(process_registry:build_process_key(call, ChannelId), NewCalls0),
    ets:insert(call_pid_cache, {ChannelId, RegPid}),
    {reply, {ok, RegPid}, State#{calls := CleanCalls#{ChannelId => {RegPid, Ref}}}}.

-spec do_lookup_call(channel_id(), state()) ->
    {reply, {ok, pid()} | {error, not_found}, state()}.
do_lookup_call(ChannelId, #{calls := Calls} = State) ->
    case ets:lookup(call_pid_cache, ChannelId) of
        [{ChannelId, Pid}] when is_pid(Pid) ->
            lookup_cached(ChannelId, Pid, Calls, State);
        _ ->
            lookup_fallback(ChannelId, Calls, State)
    end.

-spec lookup_cached(channel_id(), pid(), map(), state()) ->
    {reply, {ok, pid()} | {error, not_found}, state()}.
lookup_cached(ChannelId, Pid, Calls, State) ->
    case process_liveness:is_alive(Pid) of
        true ->
            {reply, {ok, Pid}, State};
        false ->
            ets:delete(call_pid_cache, ChannelId),
            lookup_fallback(ChannelId, Calls, State)
    end.

-spec lookup_fallback(channel_id(), map(), state()) ->
    {reply, {ok, pid()} | {error, not_found}, state()}.
lookup_fallback(ChannelId, Calls, State) ->
    case maps:get(ChannelId, Calls, undefined) of
        {Pid, _Ref} when is_pid(Pid) ->
            lookup_fallback_alive(ChannelId, Pid, Calls, State);
        undefined ->
            lookup_via_registry(ChannelId, Calls, State)
    end.

-spec lookup_fallback_alive(channel_id(), pid(), map(), state()) ->
    {reply, {ok, pid()} | {error, not_found}, state()}.
lookup_fallback_alive(ChannelId, Pid, Calls, State) ->
    case process_liveness:is_alive(Pid) of
        true ->
            ets:insert(call_pid_cache, {ChannelId, Pid}),
            {reply, {ok, Pid}, State};
        false ->
            cleanup_stale(ChannelId),
            UpdatedCalls = maps:remove(ChannelId, Calls),
            lookup_fallback(ChannelId, UpdatedCalls, State#{calls := UpdatedCalls})
    end.

-spec lookup_via_registry(channel_id(), map(), state()) ->
    {reply, {ok, pid()} | {error, not_found}, state()}.
lookup_via_registry(ChannelId, Calls, State) ->
    CallName = process_registry:build_process_key(call, ChannelId),
    case process_registry:lookup_or_monitor(CallName, ChannelId, Calls) of
        {ok, Pid, _Ref, NewCalls} ->
            ets:insert(call_pid_cache, {ChannelId, Pid}),
            {reply, {ok, Pid}, State#{calls := NewCalls}};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end.

-spec do_terminate_call(channel_id(), state()) ->
    {reply, ok | {error, not_found}, state()}.
do_terminate_call(ChannelId, #{calls := Calls} = State) ->
    do_stop_call(ChannelId, normal, State#{calls := Calls}).

-spec do_stop_call(channel_id(), term(), state()) ->
    {reply, ok | {error, not_found}, state()}.
do_stop_call(ChannelId, Reason, #{calls := Calls} = State) ->
    case maps:get(ChannelId, Calls, undefined) of
        {Pid, Ref} ->
            demonitor(Ref, [flush]),
            stop_call_if_alive(Pid, Reason),
            cleanup_stale(ChannelId),
            {reply, ok, State#{calls := maps:remove(ChannelId, Calls)}};
        undefined ->
            {reply, {error, not_found}, State}
    end.

-spec stop_call_if_alive(pid(), term()) -> ok.
stop_call_if_alive(Pid, Reason) ->
    case process_liveness:is_alive(Pid) of
        true -> gen_server:stop(Pid, Reason, ?SHUTDOWN_TIMEOUT);
        false -> ok
    end.

-spec do_handoff_to_topology([node()], state()) -> {handoff_result(), state()}.
do_handoff_to_topology(TargetNodes, State) when is_list(TargetNodes) ->
    HandoffTargets = [Node || Node <- lists:usort(TargetNodes), is_atom(Node)],
    case HandoffTargets of
        [] ->
            {empty_handoff_result(), State};
        _ ->
            handoff_call_ids(
                collect_active_call_ids(maps:get(calls, State)), HandoffTargets, State
            )
    end;
do_handoff_to_topology(_TargetNodes, State) ->
    {empty_handoff_result(), State}.

-spec collect_active_call_ids(map()) -> [channel_id()].
collect_active_call_ids(Calls) ->
    lists:sort(
        maps:fold(
            fun collect_active_call_id/3,
            [],
            Calls
        )
    ).

-spec collect_active_call_id(term(), term(), [channel_id()]) -> [channel_id()].
collect_active_call_id(ChannelId, {Pid, _Ref}, Acc) when
    is_integer(ChannelId),
    ChannelId > 0,
    is_pid(Pid)
->
    case process_liveness:is_alive(Pid) of
        true -> [ChannelId | Acc];
        false -> Acc
    end;
collect_active_call_id(_ChannelId, _Value, Acc) ->
    Acc.

-spec handoff_call_ids([channel_id()], [node()], state()) -> {handoff_result(), state()}.
handoff_call_ids(CallIds, TargetNodes, State) ->
    lists:foldl(
        fun(ChannelId, Acc) -> handoff_call_id(ChannelId, TargetNodes, Acc) end,
        {empty_handoff_result(), State},
        CallIds
    ).

-spec handoff_call_id(channel_id(), [node()], {handoff_result(), state()}) ->
    {handoff_result(), state()}.
handoff_call_id(ChannelId, TargetNodes, {Result, State}) ->
    case resolve_handoff_target(ChannelId, TargetNodes) of
        skip -> {Result, State};
        {handoff, TargetNode} -> handoff_call_id_to_owner(ChannelId, TargetNode, Result, State)
    end.

-spec handoff_call_id_to_owner(channel_id(), node(), handoff_result(), state()) ->
    {handoff_result(), state()}.
handoff_call_id_to_owner(ChannelId, TargetNode, Result, State) ->
    case handoff_call_to_owner(ChannelId, TargetNode, State) of
        {true, NewState} -> {mark_handoff_success(Result), NewState};
        {false, NewState} -> {mark_handoff_attempt(Result), NewState}
    end.

-spec resolve_handoff_target(channel_id(), [node()]) -> skip | {handoff, node()}.
resolve_handoff_target(ChannelId, TargetNodes) ->
    OwnerNode = gateway_node_router:select_owner_node(ChannelId, TargetNodes),
    case OwnerNode =:= node() of
        true -> skip;
        false -> {handoff, OwnerNode}
    end.

-spec handoff_call_to_owner(channel_id(), node(), state()) -> {boolean(), state()}.
handoff_call_to_owner(ChannelId, TargetNode, State) ->
    case export_local_call_state(ChannelId, State) of
        {ok, TransferState} ->
            start_transferred_call_on_owner(ChannelId, TargetNode, TransferState, State);
        {error, _Reason} ->
            {false, State}
    end.

-spec export_local_call_state(channel_id(), state()) -> {ok, map()} | {error, term()}.
export_local_call_state(ChannelId, #{calls := Calls}) ->
    case maps:get(ChannelId, Calls, undefined) of
        {Pid, _Ref} when is_pid(Pid) ->
            export_call_state(Pid);
        _ ->
            {error, not_found}
    end.

-spec export_call_state(pid()) -> {ok, map()} | {error, term()}.
export_call_state(Pid) ->
    case safe_gen_call(Pid, export_handoff_state, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {ok, TransferState} when is_map(TransferState) -> {ok, TransferState};
        {error, Reason} -> {error, Reason};
        Other -> {error, {unexpected_export_reply, Other}}
    end.

-spec start_transferred_call_on_owner(channel_id(), node(), map(), state()) ->
    {boolean(), state()}.
start_transferred_call_on_owner(ChannelId, TargetNode, TransferState, State) ->
    Request = {start_transferred, ChannelId, TransferState},
    case call_manager_node(TargetNode, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {ok, Pid} when is_pid(Pid) -> stop_local_call_after_handoff(ChannelId, State);
        _Other -> {false, State}
    end.

-spec stop_local_call_after_handoff(channel_id(), state()) -> {boolean(), state()}.
stop_local_call_after_handoff(ChannelId, State) ->
    case do_stop_call(ChannelId, {shutdown, handoff}, State) of
        {reply, ok, NewState} -> {true, NewState};
        {reply, _Other, NewState} -> {false, NewState}
    end.

-spec ensure_local_owner(channel_id()) -> ok | {error, term()}.
ensure_local_owner(ChannelId) ->
    case gateway_node_router:owner_node_result(ChannelId, calls) of
        {ok, OwnerNode} when OwnerNode =:= node() -> ok;
        {ok, OwnerNode} -> {error, {not_owner, OwnerNode}};
        {error, Reason} -> {error, Reason}
    end.

-spec cleanup_stale(channel_id()) -> ok.
cleanup_stale(ChannelId) ->
    CallName = process_registry:build_process_key(call, ChannelId),
    process_registry:safe_unregister(CallName),
    ets:delete(call_pid_cache, ChannelId),
    ok.

-spec call_manager_node(node(), term(), timeout()) -> term().
call_manager_node(TargetNode, Request, Timeout) ->
    try
        gen_server:call(call_manager_server_ref(TargetNode), Request, Timeout)
    catch
        exit:{timeout, _} -> {error, timeout};
        exit:{nodedown, _} -> {error, unavailable};
        exit:{noproc, _} -> {error, unavailable};
        exit:_ -> {error, unavailable}
    end.

-spec call_manager_server_ref(node()) -> atom() | {atom(), node()}.
call_manager_server_ref(TargetNode) when TargetNode =:= node() ->
    call_manager;
call_manager_server_ref(TargetNode) ->
    {call_manager, TargetNode}.

-spec safe_gen_call(pid(), term(), timeout()) -> term().
safe_gen_call(Pid, Request, Timeout) ->
    try
        gen_server:call(Pid, Request, Timeout)
    catch
        exit:{timeout, _} -> {error, timeout};
        exit:{nodedown, _} -> {error, unavailable};
        exit:{noproc, _} -> {error, not_found};
        exit:_ -> {error, unavailable}
    end.

-spec empty_handoff_result() -> handoff_result().
empty_handoff_result() ->
    #{attempted => 0, handed_off => 0}.

-spec mark_handoff_attempt(handoff_result()) -> handoff_result().
mark_handoff_attempt(#{attempted := Attempted, handed_off := HandedOff}) ->
    #{attempted => Attempted + 1, handed_off => HandedOff}.

-spec mark_handoff_success(handoff_result()) -> handoff_result().
mark_handoff_success(#{attempted := Attempted, handed_off := HandedOff}) ->
    #{attempted => Attempted + 1, handed_off => HandedOff + 1}.
