%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_cache_rebalance).
-typing([eqwalizer]).

-export([
    rebalance_ownership/1,
    handoff_all_to_target/2,
    evict_local/2,
    perform_anti_entropy/1,
    handle_anti_entropy_request/3,
    handle_anti_entropy_digest_request/3,
    merge_anti_entropy_entries/2,
    schedule_anti_entropy/0,
    cancel_anti_entropy_timer/1,
    start_nodedown_grace/2,
    cancel_nodedown_grace/2,
    process_nodedown_grace_expiry/2,
    cancel_all_grace_timers/1,
    sanitize_pending_operations/1,
    queue_pending_operation/3,
    clear_pending_operation/2,
    ensure_pending_retry_timer/1,
    cancel_pending_retry_timer/1,
    ensure_pending_state/1,
    count_pending_operations/1,
    increment_generation/1
]).

-export_type([pending_operation/0, pending_operations/0, state/0]).

-define(NODEDOWN_GRACE_PERIOD_MS, 15000).

-type pending_operation() :: {put, map()} | delete.
-type pending_operations() :: #{integer() => pending_operation()}.
-type state() :: map().

-spec rebalance_ownership(state()) -> state().
rebalance_ownership(State) ->
    case persistent_term:get(presence_noop, false) of
        true -> State;
        false -> rebalance_ownership_inner(State)
    end.

-spec handoff_all_to_target(node(), state()) -> state().
handoff_all_to_target(TargetNode, State) ->
    Snapshot = presence_cache:local_snapshot(State),
    maps:fold(
        fun(UserId, Presence, AccState) ->
            handoff_entry_to_target(TargetNode, UserId, Presence, AccState)
        end,
        State,
        Snapshot
    ).

-spec schedule_anti_entropy() -> reference().
schedule_anti_entropy() ->
    presence_cache_anti_entropy:schedule_anti_entropy().

-spec cancel_anti_entropy_timer(state()) -> ok.
cancel_anti_entropy_timer(State) ->
    presence_cache_anti_entropy:cancel_anti_entropy_timer(State).

-spec perform_anti_entropy(state()) -> state().
perform_anti_entropy(State) ->
    presence_cache_anti_entropy:perform_anti_entropy(State).

-spec handle_anti_entropy_request(node(), non_neg_integer(), state()) -> {noreply, state()}.
handle_anti_entropy_request(FromNode, RemoteGeneration, State) ->
    presence_cache_anti_entropy:handle_anti_entropy_request(
        FromNode, RemoteGeneration, State
    ).

-spec handle_anti_entropy_digest_request(node(), binary(), state()) -> {noreply, state()}.
handle_anti_entropy_digest_request(FromNode, RemoteDigest, State) ->
    presence_cache_anti_entropy:handle_anti_entropy_digest_request(
        FromNode, RemoteDigest, State
    ).

-spec merge_anti_entropy_entries(#{integer() => map()}, state()) -> state().
merge_anti_entropy_entries(Entries, State) ->
    presence_cache_anti_entropy:merge_anti_entropy_entries(Entries, State).

-spec start_nodedown_grace(node(), state()) -> state().
start_nodedown_grace(Node, State) ->
    PendingCleanups = maps:get(pending_nodedown_cleanups, State, #{}),
    case maps:is_key(Node, PendingCleanups) of
        true ->
            State;
        false ->
            GracePeriod = nodedown_grace_period_ms(),
            TimerRef = erlang:send_after(GracePeriod, self(), {nodedown_grace_expired, Node}),
            State#{pending_nodedown_cleanups => PendingCleanups#{Node => TimerRef}}
    end.

-spec cancel_nodedown_grace(node(), state()) -> state().
cancel_nodedown_grace(Node, State) ->
    PendingCleanups = maps:get(pending_nodedown_cleanups, State, #{}),
    case maps:find(Node, PendingCleanups) of
        {ok, TimerRef} ->
            _ = erlang:cancel_timer(TimerRef),
            State#{pending_nodedown_cleanups => maps:remove(Node, PendingCleanups)};
        error ->
            State
    end.

-spec process_nodedown_grace_expiry(node(), state()) -> state().
process_nodedown_grace_expiry(Node, State) ->
    PendingCleanups = maps:get(pending_nodedown_cleanups, State, #{}),
    State1 = State#{pending_nodedown_cleanups => maps:remove(Node, PendingCleanups)},
    rebalance_ownership(State1).

-spec cancel_all_grace_timers(state()) -> ok.
cancel_all_grace_timers(State) ->
    PendingCleanups = maps:get(pending_nodedown_cleanups, State, #{}),
    maps:foreach(
        fun(_Node, TimerRef) -> _ = erlang:cancel_timer(TimerRef) end, PendingCleanups
    ),
    ok.

-spec sanitize_pending_operations(term()) -> pending_operations().
sanitize_pending_operations(PendingOperations) ->
    presence_cache_pending:sanitize_pending_operations(PendingOperations).

-spec ensure_pending_state(state()) -> state().
ensure_pending_state(State) ->
    presence_cache_pending:ensure_pending_state(State).

-spec count_pending_operations(state()) -> non_neg_integer().
count_pending_operations(State) ->
    presence_cache_pending:count_pending_operations(State).

-spec queue_pending_operation(integer(), pending_operation(), state()) -> state().
queue_pending_operation(UserId, Operation, State) ->
    presence_cache_pending:queue_pending_operation(UserId, Operation, State).

-spec clear_pending_operation(integer(), state()) -> state().
clear_pending_operation(UserId, State) ->
    presence_cache_pending:clear_pending_operation(UserId, State).

-spec ensure_pending_retry_timer(state()) -> state().
ensure_pending_retry_timer(State) ->
    presence_cache_pending:ensure_pending_retry_timer(State).

-spec cancel_pending_retry_timer(state()) -> ok.
cancel_pending_retry_timer(State) ->
    presence_cache_pending:cancel_pending_retry_timer(State).

-spec evict_local(integer(), state()) -> state().
evict_local(UserId, State) ->
    {_Reply, NewState} = presence_cache_shards:forward_delete(UserId, State),
    increment_generation(NewState).

-spec increment_generation(state()) -> state().
increment_generation(State) ->
    Gen = maps:get(generation, State, 0),
    State#{generation => Gen + 1}.

-spec rebalance_ownership_inner(state()) -> state().
rebalance_ownership_inner(State) ->
    Snapshot = presence_cache:local_snapshot(State),
    Operations = merge_rebalance_operations(
        Snapshot,
        presence_cache_pending:pending_operations(State)
    ),
    {State1, FailedOps} = process_rebalance_operations(Operations, State),
    presence_cache_pending:set_pending_operations(FailedOps, State1).

-spec process_rebalance_operations(pending_operations(), state()) ->
    {state(), pending_operations()}.
process_rebalance_operations(Operations, State) ->
    maps:fold(fun fold_rebalance_op/3, {State, #{}}, Operations).

-spec fold_rebalance_op(integer(), pending_operation(), {state(), pending_operations()}) ->
    {state(), pending_operations()}.
fold_rebalance_op(UserId, Operation, {AccState, AccFailed}) ->
    case process_rebalance_op(UserId, Operation, AccState) of
        {ok, NextState} -> {NextState, AccFailed};
        {error, NextState} -> {NextState, AccFailed#{UserId => Operation}}
    end.

-spec merge_rebalance_operations(#{integer() => map()}, pending_operations()) ->
    pending_operations().
merge_rebalance_operations(Snapshot, PendingOps) ->
    maps:merge(
        snapshot_operations(Snapshot),
        presence_cache_pending:sanitize_pending_operations(PendingOps)
    ).

-spec snapshot_operations(#{integer() => map()}) -> pending_operations().
snapshot_operations(Snapshot) ->
    maps:fold(
        fun
            (UserId, Presence, Acc) when is_integer(UserId), UserId > 0, is_map(Presence) ->
                Acc#{UserId => {put, Presence}};
            (_UserId, _Presence, Acc) ->
                Acc
        end,
        #{},
        Snapshot
    ).

-spec process_rebalance_op(integer(), pending_operation(), state()) ->
    {ok, state()} | {error, state()}.
process_rebalance_op(UserId, Operation, State) ->
    OwnerNodes = presence_cache_bulk:resolve_owner_nodes(UserId),
    {Replicated, State1} = apply_to_owner_nodes(UserId, Operation, OwnerNodes, State),
    finalize_rebalance(UserId, Operation, OwnerNodes, Replicated, State1).

-spec apply_to_owner_nodes(integer(), pending_operation(), [node()], state()) ->
    {boolean(), state()}.
apply_to_owner_nodes(_UserId, _Operation, [], State) ->
    {false, State};
apply_to_owner_nodes(UserId, Operation, OwnerNodes, State) ->
    TargetNodes = lists:usort(OwnerNodes),
    lists:foldl(
        fun(TargetNode, {AllReplicated, AccState}) ->
            {Result, NextState} = apply_op_to_node(TargetNode, UserId, Operation, AccState),
            {AllReplicated andalso Result =:= ok, NextState}
        end,
        {true, State},
        TargetNodes
    ).

-spec apply_op_to_node(node(), integer(), pending_operation(), state()) ->
    {ok | error, state()}.
apply_op_to_node(TargetNode, UserId, {put, Presence}, State) ->
    case TargetNode =:= node() of
        true ->
            {_Reply, NewState} = presence_cache:put_local(UserId, Presence, State),
            {ok, NewState};
        false ->
            remote_apply_op(TargetNode, {put_local, UserId, Presence}, State)
    end;
apply_op_to_node(TargetNode, UserId, delete, State) ->
    case TargetNode =:= node() of
        true ->
            {_Reply, NewState} = presence_cache:delete_local(UserId, State),
            {ok, NewState};
        false ->
            remote_apply_op(TargetNode, {delete_local, UserId}, State)
    end.

-spec handoff_entry_to_target(node(), integer(), map(), state()) -> state().
handoff_entry_to_target(TargetNode, _UserId, _Presence, State) when TargetNode =:= node() ->
    State;
handoff_entry_to_target(TargetNode, UserId, Presence, State) ->
    case remote_apply_op(TargetNode, {put_local, UserId, Presence}, State) of
        {ok, State1} ->
            evict_local(UserId, State1);
        {error, State1} ->
            State1
    end.

-spec remote_apply_op(node(), term(), state()) -> {ok | error, state()}.
remote_apply_op(TargetNode, Request, State) ->
    case presence_cache_bulk:safe_remote_call(TargetNode, Request, {error, unavailable}) of
        ok -> {ok, State};
        _ -> {error, State}
    end.

-spec finalize_rebalance(integer(), pending_operation(), [node()], boolean(), state()) ->
    {ok, state()} | {error, state()}.
finalize_rebalance(UserId, delete, _OwnerNodes, true, State) ->
    {_Reply, State1} = presence_cache:delete_local(UserId, State),
    {ok, State1};
finalize_rebalance(_UserId, delete, _OwnerNodes, false, State) ->
    {error, State};
finalize_rebalance(UserId, {put, _Presence}, OwnerNodes, true, State) ->
    case lists:member(node(), OwnerNodes) of
        true ->
            {ok, State};
        false ->
            {ok, evict_local(UserId, State)}
    end;
finalize_rebalance(_UserId, {put, _Presence}, _OwnerNodes, false, State) ->
    {error, State}.

-spec nodedown_grace_period_ms() -> pos_integer().
nodedown_grace_period_ms() ->
    try fluxer_gateway_env:get(nodedown_grace_period_ms) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> ?NODEDOWN_GRACE_PERIOD_MS
    catch
        _:_ -> ?NODEDOWN_GRACE_PERIOD_MS
    end.
