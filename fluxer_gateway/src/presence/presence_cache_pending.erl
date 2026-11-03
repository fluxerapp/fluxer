%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_cache_pending).
-typing([eqwalizer]).

-export([
    sanitize_pending_operations/1,
    pending_operations/1,
    ensure_pending_state/1,
    queue_pending_operation/3,
    clear_pending_operation/2,
    set_pending_operations/2,
    ensure_pending_retry_timer/1,
    cancel_pending_retry_timer/1
]).

-export_type([pending_operation/0, pending_operations/0, state/0]).

-define(PENDING_HANDOFF_RETRY_MS, 1000).
-define(PENDING_HANDOFF_RETRY_MSG, pending_handoff_retry).
-define(PENDING_OPERATIONS_CAP, 10000).

-type pending_operation() :: {put, map()} | delete.
-type pending_operations() :: #{integer() => pending_operation()}.
-type state() :: map().

-spec sanitize_pending_operations(term()) -> pending_operations().
sanitize_pending_operations(PendingOperations) when is_map(PendingOperations) ->
    maps:fold(
        fun sanitize_pending_operation/3,
        #{},
        PendingOperations
    );
sanitize_pending_operations(_) ->
    #{}.

-spec pending_operations(state()) -> pending_operations().
pending_operations(State) ->
    sanitize_pending_operations(maps:get(pending_operations, State, #{})).

-spec ensure_pending_state(state()) -> state().
ensure_pending_state(State) ->
    Pending = pending_operations(State),
    {Capped, SeqMap, Counter} = cap_pending_operations(
        Pending, pending_seq_map(State, Pending), pending_seq_counter(State)
    ),
    State#{
        pending_operations => Capped,
        pending_seq => SeqMap,
        pending_seq_counter => Counter,
        pending_retry_timer => pending_retry_timer(State)
    }.

-spec queue_pending_operation(integer(), pending_operation(), state()) -> state().
queue_pending_operation(UserId, Operation, State) ->
    Pending = stored_pending_operations(State),
    SeqMap0 = stored_seq_map(State, Pending),
    Counter0 = pending_seq_counter(State),
    {SeqMap1, Counter1} = assign_seq(UserId, SeqMap0, Counter0),
    Updated = Pending#{UserId => Operation},
    {Capped, SeqMap2, Counter2} = cap_pending_operations(Updated, SeqMap1, Counter1),
    ensure_pending_retry_timer(State#{
        pending_operations => Capped,
        pending_seq => SeqMap2,
        pending_seq_counter => Counter2
    }).

-spec clear_pending_operation(integer(), state()) -> state().
clear_pending_operation(UserId, State) ->
    Pending = stored_pending_operations(State),
    SeqMap = stored_seq_map(State, Pending),
    Updated = maps:remove(UserId, Pending),
    ensure_pending_retry_timer(State#{
        pending_operations => Updated,
        pending_seq => maps:remove(UserId, SeqMap),
        pending_seq_counter => pending_seq_counter(State)
    }).

-spec set_pending_operations(pending_operations(), state()) -> state().
set_pending_operations(PendingOperations, State) ->
    Pending = sanitize_pending_operations(PendingOperations),
    {SeqMapCompleted, Counter} = complete_seq_map(
        maps:keys(Pending), stored_seq_map(State, Pending), pending_seq_counter(State)
    ),
    SeqMap = maps:with(maps:keys(Pending), SeqMapCompleted),
    ensure_pending_retry_timer(State#{
        pending_operations => Pending,
        pending_seq => SeqMap,
        pending_seq_counter => Counter
    }).

-spec ensure_pending_retry_timer(state()) -> state().
ensure_pending_retry_timer(State) ->
    HasPending = maps:size(stored_pending_operations(State)) > 0,
    case {HasPending, pending_retry_timer(State)} of
        {true, undefined} ->
            TimerRef = erlang:send_after(
                ?PENDING_HANDOFF_RETRY_MS, self(), ?PENDING_HANDOFF_RETRY_MSG
            ),
            State#{pending_retry_timer => TimerRef};
        {false, TimerRef} when is_reference(TimerRef) ->
            _ = erlang:cancel_timer(TimerRef),
            State#{pending_retry_timer => undefined};
        _ ->
            State
    end.

-spec cancel_pending_retry_timer(state()) -> ok.
cancel_pending_retry_timer(State) ->
    case pending_retry_timer(State) of
        TimerRef when is_reference(TimerRef) ->
            _ = erlang:cancel_timer(TimerRef),
            ok;
        undefined ->
            ok
    end.

-spec sanitize_pending_operation(term(), term(), pending_operations()) -> pending_operations().
sanitize_pending_operation(UserId, {put, Presence}, Acc) when
    is_integer(UserId), UserId > 0, is_map(Presence)
->
    Acc#{UserId => {put, Presence}};
sanitize_pending_operation(UserId, delete, Acc) when is_integer(UserId), UserId > 0 ->
    Acc#{UserId => delete};
sanitize_pending_operation(_, _, Acc) ->
    Acc.

-spec pending_retry_timer(state()) -> reference() | undefined.
pending_retry_timer(State) ->
    case maps:get(pending_retry_timer, State, undefined) of
        Ref when is_reference(Ref) -> Ref;
        _ -> undefined
    end.

-spec pending_seq_map(state(), pending_operations()) -> #{integer() => non_neg_integer()}.
pending_seq_map(State, Pending) ->
    case maps:get(pending_seq, State, undefined) of
        SeqMap when is_map(SeqMap) -> maps:with(maps:keys(Pending), SeqMap);
        _ -> reseed_seq_map(Pending)
    end.

-spec stored_pending_operations(state()) -> pending_operations().
stored_pending_operations(State) ->
    case maps:get(pending_operations, State, #{}) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    end.

-spec stored_seq_map(state(), pending_operations()) -> #{integer() => non_neg_integer()}.
stored_seq_map(State, Pending) ->
    case maps:get(pending_seq, State, undefined) of
        SeqMap when is_map(SeqMap) -> SeqMap;
        _ -> reseed_seq_map(Pending)
    end.

-spec reseed_seq_map(pending_operations()) -> #{integer() => non_neg_integer()}.
reseed_seq_map(Pending) ->
    {SeqMap, _Counter} = lists:foldl(
        fun(UserId, {Acc, Counter}) -> {Acc#{UserId => Counter}, Counter + 1} end,
        {#{}, 0},
        lists:sort(maps:keys(Pending))
    ),
    SeqMap.

-spec pending_seq_counter(state()) -> non_neg_integer().
pending_seq_counter(State) ->
    case maps:get(pending_seq_counter, State, 0) of
        Counter when is_integer(Counter), Counter >= 0 -> Counter;
        _ -> 0
    end.

-spec assign_seq(integer(), #{integer() => non_neg_integer()}, non_neg_integer()) ->
    {#{integer() => non_neg_integer()}, non_neg_integer()}.
assign_seq(UserId, SeqMap, Counter) ->
    case maps:is_key(UserId, SeqMap) of
        true -> {SeqMap, Counter};
        false -> {SeqMap#{UserId => Counter}, Counter + 1}
    end.

-spec cap_pending_operations(
    pending_operations(), #{integer() => non_neg_integer()}, non_neg_integer()
) ->
    {pending_operations(), #{integer() => non_neg_integer()}, non_neg_integer()}.
cap_pending_operations(PendingOperations, SeqMap, Counter) ->
    Size = maps:size(PendingOperations),
    case Size > ?PENDING_OPERATIONS_CAP of
        true -> evict_oldest_pending(PendingOperations, SeqMap, Counter, Size);
        false -> {PendingOperations, SeqMap, Counter}
    end.

-spec evict_oldest_pending(
    pending_operations(),
    #{integer() => non_neg_integer()},
    non_neg_integer(),
    non_neg_integer()
) ->
    {pending_operations(), #{integer() => non_neg_integer()}, non_neg_integer()}.
evict_oldest_pending(PendingOperations, SeqMap0, Counter0, Size) ->
    {SeqMapCompleted, Counter} = complete_seq_map(
        maps:keys(PendingOperations), SeqMap0, Counter0
    ),
    SeqMap = maps:with(maps:keys(PendingOperations), SeqMapCompleted),
    Ordered = lists:sort(
        fun({_KeyA, SeqA}, {_KeyB, SeqB}) -> SeqA =< SeqB end,
        maps:to_list(SeqMap)
    ),
    KeepPairs = lists:nthtail(Size - ?PENDING_OPERATIONS_CAP, Ordered),
    KeepKeys = [Key || {Key, _Seq} <- KeepPairs],
    {maps:with(KeepKeys, PendingOperations), maps:with(KeepKeys, SeqMap), Counter}.

-spec complete_seq_map([integer()], #{integer() => non_neg_integer()}, non_neg_integer()) ->
    {#{integer() => non_neg_integer()}, non_neg_integer()}.
complete_seq_map(Keys, SeqMap, Counter) ->
    lists:foldl(
        fun(Key, {AccMap, AccCounter}) -> assign_seq(Key, AccMap, AccCounter) end,
        {SeqMap, Counter},
        Keys
    ).
