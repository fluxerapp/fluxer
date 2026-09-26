%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_dispatch_relay_batch).
-typing([eqwalizer]).

-export([
    relay_or_direct_many/3,
    relay_or_direct/3,
    select_worker/1,
    current_workers/0,
    current_workers_tuple/0,
    current_workers_tuple_normalized/0,
    normalize_workers_tuple/1,
    message_queue_len/1,
    max_queue/0,
    inflight_ref/1,
    release_queue_slot/1,
    start_workers/1,
    start_worker/1,
    worker_index/3
]).

-define(STATE_KEY, {gateway_dispatch_relay, state}).
-define(INFLIGHT_KEY(Slot), {?MODULE, inflight, Slot}).
-define(INFLIGHT_INDEX, 1).
-define(INFLIGHT_UNSAMPLED, -1).
-define(CLAIM_DEADLINE_MS, 5000).
-define(CLAIM_BACKOFF_MS, 1).

-spec relay_or_direct_many([pid()], atom(), term()) -> ok.
relay_or_direct_many(SessionPids, Event, Payload) ->
    Workers = current_workers_tuple_normalized(),
    case tuple_size(Workers) of
        0 ->
            Grouped = gateway_dispatch_relay:group_by_node(SessionPids),
            gateway_dispatch_relay:dispatch_grouped(
                Grouped,
                Event,
                Payload,
                fun gateway_dispatch_relay:dispatch_direct/3
            );
        Count ->
            relay_many_to_shards(SessionPids, Event, Payload, Workers, Count)
    end.

-spec relay_many_to_shards([pid()], atom(), term(), tuple(), pos_integer()) -> ok.
relay_many_to_shards(SessionPids, Event, Payload, Workers, Count) ->
    ShardBuckets = build_shard_buckets(SessionPids, Count),
    Deferred = deliver_shard_buckets(1, Count, ShardBuckets, Event, Payload, Workers, []),
    deliver_deferred_shards(Deferred, Event, Payload, Workers).

-spec build_shard_buckets([pid()], pos_integer()) -> tuple().
build_shard_buckets(SessionPids, Count) ->
    lists:foldl(
        fun
            (Pid, Buckets) when is_pid(Pid) ->
                Index = erlang:phash2(Pid, Count) + 1,
                setelement(Index, Buckets, [Pid | element(Index, Buckets)]);
            (_, Buckets) ->
                Buckets
        end,
        erlang:make_tuple(Count, []),
        SessionPids
    ).

-spec deliver_shard_buckets(
    pos_integer(), pos_integer(), tuple(), atom(), term(), tuple(), [{pos_integer(), [pid()]}]
) -> [{pos_integer(), [pid()]}].
deliver_shard_buckets(Index, Count, _Buckets, _Event, _Payload, _Workers, Deferred) when
    Index > Count
->
    Deferred;
deliver_shard_buckets(Index, Count, Buckets, Event, Payload, Workers, Deferred) ->
    Next =
        case element(Index, Buckets) of
            [] -> Deferred;
            Pids -> deliver_shard(Index, Pids, Event, Payload, Workers, Deferred)
        end,
    deliver_shard_buckets(Index + 1, Count, Buckets, Event, Payload, Workers, Next).

-spec deliver_shard(
    pos_integer(), [pid()], atom(), term(), tuple(), [{pos_integer(), [pid()]}]
) -> [{pos_integer(), [pid()]}].
deliver_shard(Index, Pids, Event, Payload, Workers, Deferred) ->
    Msg = {deliver_many, Pids, Event, Payload},
    case enqueue_async(Index - 1, element(Index, Workers), Msg) of
        ok -> Deferred;
        full -> [{Index, Pids} | Deferred]
    end.

-spec deliver_deferred_shards([{pos_integer(), [pid()]}], atom(), term(), tuple()) -> ok.
deliver_deferred_shards([], _Event, _Payload, _Workers) ->
    ok;
deliver_deferred_shards([{Index, Pids} | Rest], Event, Payload, Workers) ->
    enqueue(Index - 1, element(Index, Workers), {deliver_many, Pids, Event, Payload}),
    deliver_deferred_shards(Rest, Event, Payload, Workers).

-spec relay_or_direct(pid(), atom(), term()) -> ok.
relay_or_direct(SessionPid, Event, Payload) ->
    case select_worker_slot(SessionPid) of
        undefined ->
            gateway_dispatch_relay:dispatch_direct(SessionPid, Event, Payload);
        {Slot, Worker} ->
            enqueue(Slot, Worker, {deliver, SessionPid, Event, Payload})
    end.

-spec enqueue(non_neg_integer(), pid(), term()) -> ok.
enqueue(Slot, Worker, Msg) ->
    case enqueue_async(Slot, Worker, Msg) of
        ok -> ok;
        full -> enqueue_blocking(Slot, Worker, Msg, claim_deadline())
    end.

-spec enqueue_blocking(non_neg_integer(), pid(), term(), integer()) -> ok.
enqueue_blocking(Slot, Worker, Msg, Deadline) ->
    case gateway_retry_timer:wait_until(?CLAIM_BACKOFF_MS, Deadline) of
        ok -> enqueue_retry(Slot, Worker, Msg, Deadline);
        _ -> enqueue_forced(Slot, Worker, Msg)
    end.

-spec enqueue_retry(non_neg_integer(), pid(), term(), integer()) -> ok.
enqueue_retry(Slot, Worker, Msg, Deadline) ->
    case enqueue_async(Slot, Worker, Msg) of
        ok -> ok;
        full -> enqueue_blocking(Slot, Worker, Msg, Deadline)
    end.

-spec enqueue_forced(non_neg_integer(), pid(), term()) -> ok.
enqueue_forced(Slot, Worker, Msg) ->
    ok = reserve_queue_slot(Slot),
    gen_server:cast(Worker, Msg).

-spec claim_deadline() -> integer().
claim_deadline() ->
    erlang:monotonic_time(millisecond) + ?CLAIM_DEADLINE_MS.

-spec enqueue_async(non_neg_integer(), pid(), term()) -> ok | full.
enqueue_async(Slot, Worker, Msg) ->
    case claim_queue_slot(Slot, Worker, max_queue()) of
        ok -> gen_server:cast(Worker, Msg);
        full -> full
    end.

-spec claim_queue_slot(non_neg_integer(), pid(), pos_integer()) -> ok | full.
claim_queue_slot(Slot, Worker, MaxQueue) ->
    case inflight_ref(Slot) of
        undefined -> sample_queue_slot(Worker, MaxQueue);
        Ref -> claim_inflight_slot(Ref, Worker, MaxQueue)
    end.

-spec claim_inflight_slot(atomics:atomics_ref(), pid(), pos_integer()) -> ok | full.
claim_inflight_slot(Ref, Worker, MaxQueue) ->
    case atomics:add_get(Ref, ?INFLIGHT_INDEX, 1) of
        Claimed when Claimed > 0, Claimed =< MaxQueue -> ok;
        _ -> resample_inflight_slot(Ref, Worker, MaxQueue)
    end.

-spec resample_inflight_slot(atomics:atomics_ref(), pid(), pos_integer()) -> ok | full.
resample_inflight_slot(Ref, Worker, MaxQueue) ->
    Current = atomics:sub_get(Ref, ?INFLIGHT_INDEX, 1),
    case message_queue_len(Worker) of
        Sampled when Sampled < MaxQueue -> exchange_inflight_slot(Ref, Current, Sampled + 1);
        _ -> full
    end.

-spec exchange_inflight_slot(atomics:atomics_ref(), integer(), pos_integer()) -> ok | full.
exchange_inflight_slot(Ref, Current, Desired) ->
    case atomics:compare_exchange(Ref, ?INFLIGHT_INDEX, Current, Desired) of
        ok -> ok;
        _ -> full
    end.

-spec sample_queue_slot(pid(), pos_integer()) -> ok | full.
sample_queue_slot(Worker, MaxQueue) ->
    case message_queue_len(Worker) < MaxQueue of
        true -> ok;
        false -> full
    end.

-spec reserve_queue_slot(non_neg_integer()) -> ok.
reserve_queue_slot(Slot) ->
    case inflight_ref(Slot) of
        undefined -> ok;
        Ref -> atomics:add(Ref, ?INFLIGHT_INDEX, 1)
    end.

-spec release_queue_slot(atomics:atomics_ref() | undefined) -> ok.
release_queue_slot(undefined) ->
    ok;
release_queue_slot(Ref) ->
    atomics:sub(Ref, ?INFLIGHT_INDEX, 1).

-spec inflight_ref(non_neg_integer()) -> atomics:atomics_ref() | undefined.
inflight_ref(Slot) ->
    persistent_term:get(?INFLIGHT_KEY(Slot), undefined).

-spec max_queue() -> pos_integer().
max_queue() ->
    Ceiling = process_health_watchdog:kill_threshold(),
    case gateway_rollout_config:gateway_dispatch_relay_max_queue() of
        Configured when is_integer(Configured), Configured > 0 -> min(Configured, Ceiling);
        _ -> Ceiling
    end.

-spec current_workers() -> [pid()].
current_workers() ->
    tuple_to_list(current_workers_tuple()).

-spec current_workers_tuple() -> tuple().
current_workers_tuple() ->
    try
        normalize_workers_tuple(maps:get(workers, persistent_term:get(?STATE_KEY), {}))
    catch
        error:badarg -> {}
    end.

-spec normalize_workers_tuple(term()) -> tuple().
normalize_workers_tuple(Workers) when is_tuple(Workers) -> Workers;
normalize_workers_tuple(Workers) when is_list(Workers) -> list_to_tuple(Workers);
normalize_workers_tuple(_) -> {}.

-spec current_workers_tuple_normalized() -> tuple().
current_workers_tuple_normalized() ->
    current_workers_tuple().

-spec select_worker(pid()) -> pid() | undefined.
select_worker(SessionPid) ->
    case select_worker_slot(SessionPid) of
        undefined -> undefined;
        {_Slot, Worker} -> Worker
    end.

-spec select_worker_slot(pid()) -> {non_neg_integer(), pid()} | undefined.
select_worker_slot(SessionPid) ->
    Workers = current_workers_tuple_normalized(),
    case tuple_size(Workers) of
        0 ->
            undefined;
        Count ->
            Index = erlang:phash2(SessionPid, Count) + 1,
            {Index - 1, element(Index, Workers)}
    end.

-spec message_queue_len(pid()) -> non_neg_integer().
message_queue_len(Pid) ->
    case process_info(Pid, message_queue_len) of
        {message_queue_len, Len} when is_integer(Len), Len >= 0 -> Len;
        _ -> 0
    end.

-spec start_workers(pos_integer()) -> [pid()].
start_workers(Count) ->
    [start_worker(Index) || Index <- lists:seq(0, Count - 1)].

-spec start_worker(non_neg_integer()) -> pid().
start_worker(Index) ->
    ok = reset_inflight(Index),
    {ok, Pid} = gen_server:start_link(
        gateway_dispatch_relay,
        {worker, Index},
        [{spawn_opt, [{message_queue_data, off_heap}]}]
    ),
    Pid.

-spec reset_inflight(non_neg_integer()) -> ok.
reset_inflight(Index) ->
    case inflight_ref(Index) of
        undefined -> persistent_term:put(?INFLIGHT_KEY(Index), new_inflight());
        Ref -> atomics:put(Ref, ?INFLIGHT_INDEX, ?INFLIGHT_UNSAMPLED)
    end.

-spec new_inflight() -> atomics:atomics_ref().
new_inflight() ->
    Ref = atomics:new(1, []),
    atomics:put(Ref, ?INFLIGHT_INDEX, ?INFLIGHT_UNSAMPLED),
    Ref.

-spec worker_index(pid(), tuple(), non_neg_integer()) -> non_neg_integer() | undefined.
worker_index(Pid, Workers, Index) ->
    worker_index(Pid, tuple_size(Workers), Workers, Index).

-spec worker_index(pid(), non_neg_integer(), tuple(), non_neg_integer()) ->
    non_neg_integer() | undefined.
worker_index(_Pid, Count, _Workers, Index) when Index >= Count -> undefined;
worker_index(Pid, _Count, Workers, Index) when element(Index + 1, Workers) =:= Pid -> Index;
worker_index(Pid, Count, Workers, Index) -> worker_index(Pid, Count, Workers, Index + 1).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

select_worker_returns_undefined_when_no_workers_test() ->
    persistent_term:erase(?STATE_KEY),
    ?assertEqual(undefined, select_worker(self())).

-endif.
