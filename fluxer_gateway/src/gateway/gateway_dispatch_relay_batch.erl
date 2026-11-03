%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_dispatch_relay_batch).
-typing([eqwalizer]).

-export([
    relay_or_direct_many/3,
    relay_or_direct/3,
    current_workers/0,
    normalize_workers_tuple/1,
    message_queue_len/1,
    release_credit/1,
    settle_debt/1,
    start_workers/1,
    start_worker/1,
    worker_index/3
]).

-define(STATE_KEY, {gateway_dispatch_relay, state}).
-define(GENERATION_KEY(Slot), {?MODULE, generation, Slot}).
-define(CREDITS, 1).
-define(DEBT, 2).
-define(UNSAMPLED, -1).
-define(CLAIM_DEADLINE_MS, 5000).
-define(CLAIM_BACKOFF_MS, 1).

-type generation() :: {pid(), atomics:atomics_ref() | unaccounted}.
-type deferred() :: [{generation(), [pid()]}].

-spec relay_or_direct_many([pid()], atom(), term()) -> ok.
relay_or_direct_many(SessionPids, Event, Payload) ->
    Workers = current_workers_tuple(),
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
    deliver_deferred_shards(Deferred, Event, Payload).

-spec build_shard_buckets([pid()], pos_integer()) -> tuple().
build_shard_buckets(SessionPids, Count) ->
    lists:foldl(
        fun
            (Pid, Buckets) when is_pid(Pid) ->
                Index = erlang:phash2(Pid, Count) + 1,
                setelement(Index, Buckets, [
                    Pid | eqwalizer:dynamic_cast(element(Index, Buckets))
                ]);
            (_, Buckets) ->
                Buckets
        end,
        erlang:make_tuple(Count, []),
        SessionPids
    ).

-spec deliver_shard_buckets(
    pos_integer(), pos_integer(), tuple(), atom(), term(), tuple(), deferred()
) -> deferred().
deliver_shard_buckets(Index, Count, _Buckets, _Event, _Payload, _Workers, Deferred) when
    Index > Count
->
    Deferred;
deliver_shard_buckets(Index, Count, Buckets, Event, Payload, Workers, Deferred) ->
    Next =
        case element(Index, Buckets) of
            [] ->
                Deferred;
            Pids ->
                Generation = generation(Index, Workers),
                deliver_shard(
                    Generation, eqwalizer:dynamic_cast(Pids), Event, Payload, Deferred
                )
        end,
    deliver_shard_buckets(Index + 1, Count, Buckets, Event, Payload, Workers, Next).

-spec deliver_shard(generation(), [pid()], atom(), term(), deferred()) -> deferred().
deliver_shard(Generation, Pids, Event, Payload, Deferred) ->
    case enqueue_async(Generation, {deliver_many, Pids, Event, Payload}) of
        ok -> Deferred;
        full -> [{Generation, Pids} | Deferred]
    end.

-spec deliver_deferred_shards(deferred(), atom(), term()) -> ok.
deliver_deferred_shards([], _Event, _Payload) ->
    ok;
deliver_deferred_shards([{Generation, Pids} | Rest], Event, Payload) ->
    enqueue(Generation, {deliver_many, Pids, Event, Payload}),
    deliver_deferred_shards(Rest, Event, Payload).

-spec relay_or_direct(pid(), atom(), term()) -> ok.
relay_or_direct(SessionPid, Event, Payload) ->
    Workers = current_workers_tuple(),
    case tuple_size(Workers) of
        0 ->
            gateway_dispatch_relay:dispatch_direct(SessionPid, Event, Payload);
        Count ->
            Generation = generation(erlang:phash2(SessionPid, Count) + 1, Workers),
            enqueue(Generation, {deliver, SessionPid, Event, Payload})
    end.

-spec generation(pos_integer(), tuple()) -> generation().
generation(Index, Workers) ->
    worker_generation(Index - 1, element(Index, Workers)).

-spec worker_generation(non_neg_integer(), term()) -> generation().
worker_generation(Slot, Worker) when is_pid(Worker) ->
    case persistent_term:get(?GENERATION_KEY(Slot), undefined) of
        {Worker, Credits} -> {Worker, Credits};
        _ -> {Worker, unaccounted}
    end.

-spec enqueue(generation(), term()) -> ok.
enqueue(Generation, Msg) ->
    case enqueue_async(Generation, Msg) of
        ok -> ok;
        full -> enqueue_blocking(Generation, Msg, claim_deadline())
    end.

-spec enqueue_blocking(generation(), term(), integer()) -> ok.
enqueue_blocking(Generation, Msg, Deadline) ->
    case gateway_retry_timer:wait_until(?CLAIM_BACKOFF_MS, Deadline) of
        ok -> enqueue_retry(Generation, Msg, Deadline);
        _ -> enqueue_forced(Generation, Msg)
    end.

-spec enqueue_retry(generation(), term(), integer()) -> ok.
enqueue_retry(Generation, Msg, Deadline) ->
    case enqueue_async(Generation, Msg) of
        ok -> ok;
        full -> enqueue_blocking(Generation, Msg, Deadline)
    end.

-spec enqueue_forced(generation(), term()) -> ok.
enqueue_forced({Worker, unaccounted}, Msg) ->
    gen_server:cast(Worker, Msg);
enqueue_forced({Worker, Credits}, Msg) ->
    ok = atomics:add(Credits, ?CREDITS, 1),
    gen_server:cast(Worker, Msg).

-spec claim_deadline() -> integer().
claim_deadline() ->
    erlang:monotonic_time(millisecond) + ?CLAIM_DEADLINE_MS.

-spec enqueue_async(generation(), term()) -> ok | full.
enqueue_async({Worker, _} = Generation, Msg) ->
    case claim(Generation, max_queue()) of
        ok -> gen_server:cast(Worker, Msg);
        full -> full
    end.

-spec claim(generation(), pos_integer()) -> ok | full.
claim({Worker, unaccounted}, MaxQueue) ->
    case message_queue_len(Worker) < MaxQueue of
        true -> ok;
        false -> full
    end;
claim({Worker, Credits}, MaxQueue) ->
    Capacity = MaxQueue - debt(Worker, Credits),
    claim_credit(Credits, Capacity, atomics:get(Credits, ?CREDITS)).

-spec claim_credit(atomics:atomics_ref(), integer(), integer()) -> ok | full.
claim_credit(_Credits, Capacity, Claimed) when Claimed >= Capacity ->
    full;
claim_credit(Credits, Capacity, Claimed) ->
    case atomics:compare_exchange(Credits, ?CREDITS, Claimed, Claimed + 1) of
        ok -> ok;
        Current -> claim_credit(Credits, Capacity, Current)
    end.

-spec debt(pid(), atomics:atomics_ref()) -> integer().
debt(Worker, Credits) ->
    case atomics:get(Credits, ?DEBT) of
        ?UNSAMPLED -> sample_debt(Worker, Credits);
        Debt -> Debt
    end.

-spec sample_debt(pid(), atomics:atomics_ref()) -> integer().
sample_debt(Worker, Credits) ->
    Sampled = message_queue_len(Worker),
    case atomics:compare_exchange(Credits, ?DEBT, ?UNSAMPLED, Sampled) of
        ok -> Sampled;
        Debt -> Debt
    end.

-spec release_credit(atomics:atomics_ref()) -> ok.
release_credit(Credits) ->
    atomics:sub(Credits, ?CREDITS, 1).

-spec settle_debt(atomics:atomics_ref()) -> ok.
settle_debt(Credits) ->
    case atomics:get(Credits, ?DEBT) of
        Debt when Debt > 0 -> lower_debt(Credits, Debt, message_queue_len(self()));
        _ -> ok
    end.

-spec lower_debt(atomics:atomics_ref(), pos_integer(), non_neg_integer()) -> ok.
lower_debt(Credits, Debt, Queued) when Queued < Debt ->
    atomics:put(Credits, ?DEBT, Queued);
lower_debt(_Credits, _Debt, _Queued) ->
    ok.

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
    Credits = atomics:new(2, []),
    ok = atomics:put(Credits, ?DEBT, ?UNSAMPLED),
    {ok, Pid} = gen_server:start_link(
        gateway_dispatch_relay,
        {worker, Index, Credits},
        [{spawn_opt, [{message_queue_data, off_heap}]}]
    ),
    persistent_term:put(?GENERATION_KEY(Index), {Pid, Credits}),
    Pid.

-spec worker_index(pid(), tuple(), non_neg_integer()) -> non_neg_integer() | undefined.
worker_index(Pid, Workers, Index) ->
    worker_index(Pid, tuple_size(Workers), Workers, Index).

-spec worker_index(pid(), non_neg_integer(), tuple(), non_neg_integer()) ->
    non_neg_integer() | undefined.
worker_index(_Pid, Count, _Workers, Index) when Index >= Count -> undefined;
worker_index(Pid, _Count, Workers, Index) when element(Index + 1, Workers) =:= Pid -> Index;
worker_index(Pid, Count, Workers, Index) -> worker_index(Pid, Count, Workers, Index + 1).
