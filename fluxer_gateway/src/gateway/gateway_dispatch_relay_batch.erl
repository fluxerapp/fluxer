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
    start_workers/1,
    start_worker/1,
    worker_index/3
]).

-define(STATE_KEY, {gateway_dispatch_relay, state}).
-define(SYNC_TIMEOUT_MS, 5000).
-define(SLOT_KEY(Worker), {?MODULE, Worker}).

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
    deliver_deferred_shards(Deferred, Event, Payload).

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
    pos_integer(), pos_integer(), tuple(), atom(), term(), tuple(), [{pid(), [pid()]}]
) -> [{pid(), [pid()]}].
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

-spec deliver_shard(pos_integer(), [pid()], atom(), term(), tuple(), [{pid(), [pid()]}]) ->
    [{pid(), [pid()]}].
deliver_shard(Index, Pids, Event, Payload, Workers, Deferred) ->
    Worker = element(Index, Workers),
    case enqueue_async(Worker, {deliver_many, Pids, Event, Payload}) of
        ok -> Deferred;
        full -> [{Worker, Pids} | Deferred]
    end.

-spec deliver_deferred_shards([{pid(), [pid()]}], atom(), term()) -> ok.
deliver_deferred_shards([], _Event, _Payload) ->
    ok;
deliver_deferred_shards([{Worker, Pids} | Rest], Event, Payload) ->
    enqueue_sync(Worker, {deliver_many, Pids, Event, Payload}),
    deliver_deferred_shards(Rest, Event, Payload).

-spec relay_or_direct(pid(), atom(), term()) -> ok.
relay_or_direct(SessionPid, Event, Payload) ->
    case select_worker(SessionPid) of
        undefined ->
            gateway_dispatch_relay:dispatch_direct(SessionPid, Event, Payload);
        Worker ->
            enqueue(Worker, {deliver, SessionPid, Event, Payload})
    end.

-spec enqueue(pid(), term()) -> ok.
enqueue(Worker, Msg) ->
    case enqueue_async(Worker, Msg) of
        ok -> ok;
        full -> enqueue_sync(Worker, Msg)
    end.

-spec enqueue_async(pid(), term()) -> ok | full.
enqueue_async(Worker, Msg) ->
    case claim_queue_slot(Worker, max_queue()) of
        ok -> gen_server:cast(Worker, Msg);
        full -> full
    end.

-spec claim_queue_slot(pid(), pos_integer()) -> ok | full.
claim_queue_slot(Worker, MaxQueue) ->
    case erlang:get(?SLOT_KEY(Worker)) of
        Claimed when is_integer(Claimed), Claimed < MaxQueue ->
            _ = erlang:put(?SLOT_KEY(Worker), Claimed + 1),
            ok;
        _ ->
            sample_queue_slot(Worker, MaxQueue)
    end.

-spec sample_queue_slot(pid(), pos_integer()) -> ok | full.
sample_queue_slot(Worker, MaxQueue) ->
    case message_queue_len(Worker) of
        Sampled when Sampled < MaxQueue ->
            _ = erlang:put(?SLOT_KEY(Worker), Sampled + 1),
            ok;
        _ ->
            _ = erlang:erase(?SLOT_KEY(Worker)),
            full
    end.

-spec enqueue_sync(pid(), term()) -> ok.
enqueue_sync(Worker, Msg) ->
    try gen_server:call(Worker, Msg, ?SYNC_TIMEOUT_MS) of
        _ -> ok
    catch
        exit:_Reason -> ok
    end.

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
    Workers = current_workers_tuple_normalized(),
    case tuple_size(Workers) of
        0 ->
            undefined;
        Count ->
            Index = erlang:phash2(SessionPid, Count) + 1,
            element(Index, Workers)
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
    {ok, Pid} = gen_server:start_link(
        gateway_dispatch_relay,
        {worker, Index},
        [{spawn_opt, [{message_queue_data, off_heap}]}]
    ),
    Pid.

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
