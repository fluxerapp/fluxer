%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_dispatch_relay_bound_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(STATE_KEY, {gateway_dispatch_relay, state}).
-define(CONFIG_KEY, gateway_rollout_config).
-define(BOUND, 8).
-define(FILL, 6).
-define(EVENT_COUNT, 6).
-define(PROBE_BOUND, 1000).
-define(PROBE_EVENT_COUNT, 200).
-define(PROBE_BUDGET, 2).
-define(SHARD_COUNT, 2).
-define(FAST_SHARD_TIMEOUT_MS, 1500).
-define(PRODUCER_COUNTS, [1, 10, 64]).
-define(PEAK_SAMPLES, 100).
-define(PEAK_INTERVAL_MS, 5).

dispatch_is_bounded_and_stays_ordered_test_() ->
    {timeout, 30, fun dispatch_is_bounded_and_stays_ordered/0}.

dispatch_many_is_bounded_and_stays_ordered_test_() ->
    {timeout, 30, fun dispatch_many_is_bounded_and_stays_ordered/0}.

max_queue_config_governs_the_bound_test_() ->
    {timeout, 30, fun max_queue_config_governs_the_bound/0}.

max_queue_zero_keeps_the_bound_test_() ->
    {timeout, 60, fun max_queue_zero_keeps_the_bound/0}.

max_queue_is_capped_by_the_watchdog_kill_threshold_test_() ->
    {timeout, 30, fun max_queue_is_capped_by_the_watchdog_kill_threshold/0}.

dispatch_does_not_probe_the_worker_per_event_test_() ->
    {timeout, 30, fun dispatch_does_not_probe_the_worker_per_event/0}.

saturated_shard_does_not_delay_other_shards_test_() ->
    {timeout, 60, fun saturated_shard_does_not_delay_other_shards/0}.

concurrent_producers_share_the_worker_bound_test_() ->
    {timeout, 120, fun concurrent_producers_share_the_worker_bound/0}.

restarted_worker_starts_from_an_empty_bound_test_() ->
    {timeout, 30, fun restarted_worker_starts_from_an_empty_bound/0}.

dispatch_is_bounded_and_stays_ordered() ->
    assert_bounded_and_ordered(fun send_dispatch/2).

dispatch_many_is_bounded_and_stays_ordered() ->
    assert_bounded_and_ordered(fun send_dispatch_many/2).

send_dispatch(SessionPid, N) ->
    gateway_dispatch_relay:dispatch(SessionPid, relay_bound_event, #{<<"n">> => N}).

send_dispatch_many(SessionPid, N) ->
    gateway_dispatch_relay:dispatch_many([SessionPid], relay_bound_event, #{<<"n">> => N}).

max_queue_config_governs_the_bound() ->
    ?assertEqual(blocked, producer_status_with_bound(?BOUND, ?BOUND)),
    ?assertEqual(finished, producer_status_with_bound(?BOUND * 8, ?BOUND)).

max_queue_zero_keeps_the_bound() ->
    Ceiling = process_health_watchdog:kill_threshold(),
    with_max_queue(0, fun() ->
        ?assertEqual(Ceiling, gateway_dispatch_relay_batch:max_queue())
    end),
    ?assertEqual(blocked, producer_status_with_bound(0, Ceiling)).

max_queue_is_capped_by_the_watchdog_kill_threshold() ->
    Ceiling = process_health_watchdog:kill_threshold(),
    with_max_queue(Ceiling * 4, fun() ->
        ?assertEqual(Ceiling, gateway_dispatch_relay_batch:max_queue())
    end),
    with_max_queue(Ceiling - 1, fun() ->
        ?assertEqual(Ceiling - 1, gateway_dispatch_relay_batch:max_queue())
    end).

dispatch_does_not_probe_the_worker_per_event() ->
    with_max_queue(?PROBE_BOUND, fun() ->
        with_worker(fun(Worker) ->
            Ref = make_ref(),
            Session = spawn_session(self(), Ref),
            Producer = spawn_gated_producer(Session, ?PROBE_EVENT_COUNT),
            Probes = count_probes(Worker, Producer),
            ?assertEqual(
                lists:seq(1, ?PROBE_EVENT_COUNT),
                collect_observed(Ref, ?PROBE_EVENT_COUNT, [])
            ),
            Session ! stop,
            ?assert(Probes =< ?PROBE_BUDGET)
        end)
    end).

saturated_shard_does_not_delay_other_shards() ->
    with_max_queue(?BOUND, fun() ->
        with_workers(?SHARD_COUNT, fun([Blocked, _Free]) ->
            SlowRef = make_ref(),
            FastRef = make_ref(),
            Slow = session_for_shard(1, self(), SlowRef),
            Fast = session_for_shard(2, self(), FastRef),
            ok = sys:suspend(Blocked),
            fill_queue(Blocked, ?BOUND),
            Producer = spawn_fanout_producer([Slow, Fast]),
            ?assertEqual([1], collect_within(FastRef, ?FAST_SHARD_TIMEOUT_MS)),
            ?assertEqual(blocked, producer_status(Producer, 100)),
            ok = sys:resume(Blocked),
            ?assertEqual([1], collect_observed(SlowRef, 1, [])),
            ?assertEqual(finished, producer_status(Producer, 20000)),
            Slow ! stop,
            Fast ! stop
        end)
    end).

restarted_worker_starts_from_an_empty_bound() ->
    with_max_queue(?BOUND, fun() ->
        Saturated = saturate_worker(),
        Ref = make_ref(),
        Session = spawn_session(self(), Ref),
        with_worker(fun(_Restarted) ->
            Producer = spawn_producer(fun send_dispatch/2, Session),
            ?assertEqual(finished, producer_status(Producer, 5000)),
            ?assertEqual(lists:seq(1, ?EVENT_COUNT), collect_observed(Ref, ?EVENT_COUNT, []))
        end),
        Session ! stop,
        stop_worker(Saturated)
    end).

saturate_worker() ->
    Worker = gateway_dispatch_relay_batch:start_worker(0),
    Session = spawn_session(self(), make_ref()),
    with_relay_workers([Worker], fun() ->
        ok = sys:suspend(Worker),
        fill_queue(Worker, ?BOUND),
        Producer = spawn_producer(fun send_dispatch/2, Session),
        ?assertEqual(blocked, producer_status(Producer, 500))
    end),
    Session ! stop,
    Worker.

concurrent_producers_share_the_worker_bound() ->
    lists:foreach(fun assert_peak_within_bound/1, ?PRODUCER_COUNTS).

assert_peak_within_bound(Producers) ->
    Verdict = bound_verdict(peak_queue_len(Producers)),
    ?assertEqual({Producers, within_bound}, {Producers, Verdict}).

bound_verdict(Peak) when Peak =< ?BOUND + 1 -> within_bound;
bound_verdict(Peak) -> {over_bound, Peak}.

peak_queue_len(Producers) ->
    with_max_queue(?BOUND, fun() ->
        with_worker(fun(Worker) -> concurrent_peak(Worker, Producers) end)
    end).

concurrent_peak(Worker, Producers) ->
    Ref = make_ref(),
    Session = spawn_session(self(), Ref),
    ok = sys:suspend(Worker),
    Pids = [spawn_producer(fun send_dispatch/2, Session) || _ <- lists:seq(1, Producers)],
    Peak = sample_peak(Worker, ?PEAK_SAMPLES, 0),
    ok = sys:resume(Worker),
    lists:foreach(fun(Pid) -> ok = await_producer(Pid, blocked) end, Pids),
    _ = collect_observed(Ref, Producers * ?EVENT_COUNT, []),
    Session ! stop,
    Peak.

sample_peak(_Worker, 0, Peak) ->
    Peak;
sample_peak(Worker, Remaining, Peak) ->
    ok = gateway_retry_timer:wait(?PEAK_INTERVAL_MS),
    Sampled = gateway_dispatch_relay_batch:message_queue_len(Worker),
    sample_peak(Worker, Remaining - 1, max(Peak, Sampled)).

assert_bounded_and_ordered(SendFun) ->
    {Status, QueueLen, Observed} = run_over_bound(SendFun),
    ?assertEqual(lists:seq(1, ?EVENT_COUNT), Observed),
    ?assertEqual(blocked, Status),
    ?assert(QueueLen =< ?BOUND + 1).

run_over_bound(SendFun) ->
    with_max_queue(?BOUND, fun() ->
        with_worker(fun(Worker) ->
            Ref = make_ref(),
            Session = spawn_session(self(), Ref),
            ok = sys:suspend(Worker),
            fill_queue(Worker, ?FILL),
            Producer = spawn_producer(SendFun, Session),
            Status = producer_status(Producer, 500),
            QueueLen = gateway_dispatch_relay_batch:message_queue_len(Worker),
            ok = sys:resume(Worker),
            ok = await_producer(Producer, Status),
            Observed = collect_observed(Ref, ?EVENT_COUNT, []),
            Session ! stop,
            {Status, QueueLen, Observed}
        end)
    end).

producer_status_with_bound(MaxQueue, Fill) ->
    with_max_queue(MaxQueue, fun() ->
        with_worker(fun(Worker) ->
            Ref = make_ref(),
            Session = spawn_session(self(), Ref),
            ok = sys:suspend(Worker),
            fill_queue(Worker, Fill),
            Producer = spawn_producer(fun send_dispatch/2, Session),
            Status = producer_status(Producer, 500),
            ok = sys:resume(Worker),
            ok = await_producer(Producer, Status),
            ?assertEqual(lists:seq(1, ?EVENT_COUNT), collect_observed(Ref, ?EVENT_COUNT, [])),
            Session ! stop,
            Status
        end)
    end).

spawn_producer(SendFun, Session) ->
    Parent = self(),
    spawn_link(fun() ->
        lists:foreach(fun(N) -> ok = SendFun(Session, N) end, lists:seq(1, ?EVENT_COUNT)),
        Parent ! {producer_finished, self()}
    end).

spawn_gated_producer(Session, Count) ->
    Parent = self(),
    spawn_link(fun() ->
        receive
            start -> ok
        end,
        lists:foreach(fun(N) -> ok = send_dispatch(Session, N) end, lists:seq(1, Count)),
        Parent ! {producer_finished, self()}
    end).

spawn_fanout_producer(Sessions) ->
    Parent = self(),
    spawn_link(fun() ->
        ok = gateway_dispatch_relay:dispatch_many(Sessions, relay_bound_event, #{<<"n">> => 1}),
        Parent ! {producer_finished, self()}
    end).

session_for_shard(Index, Parent, Ref) ->
    Session = spawn_session(Parent, Ref),
    case erlang:phash2(Session, ?SHARD_COUNT) + 1 of
        Index -> Session;
        _ -> session_for_shard(Index, Parent, Ref)
    end.

count_probes(Worker, Producer) ->
    1 = erlang:trace(Producer, true, [call]),
    _ = erlang:trace_pattern({erlang, process_info, 2}, true, [global]),
    Producer ! start,
    try
        drain_probes(Worker, Producer, 0)
    after
        _ = erlang:trace_pattern({erlang, process_info, 2}, false, [global]),
        untrace(Producer)
    end.

untrace(Producer) ->
    try erlang:trace(Producer, false, [call]) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

drain_probes(Worker, Producer, Count) ->
    receive
        {trace, Producer, call, {erlang, process_info, [Worker, message_queue_len]}} ->
            drain_probes(Worker, Producer, Count + 1);
        {producer_finished, Producer} ->
            Count
    after 20000 ->
        ?assert(false, {probe_count_timeout, Count})
    end.

await_producer(_Producer, finished) ->
    ok;
await_producer(Producer, blocked) ->
    ?assertEqual(finished, producer_status(Producer, 20000)),
    ok.

producer_status(Producer, Timeout) ->
    receive
        {producer_finished, Producer} -> finished
    after Timeout ->
        blocked
    end.

fill_queue(_Worker, 0) ->
    ok;
fill_queue(Worker, Remaining) ->
    Worker ! relay_bound_filler,
    fill_queue(Worker, Remaining - 1).

spawn_session(Parent, Ref) ->
    spawn_link(fun() -> session_loop(Parent, Ref) end).

session_loop(Parent, Ref) ->
    receive
        {'$gen_cast', {dispatch, relay_bound_event, #{<<"n">> := N}}} ->
            Parent ! {relay_bound_received, Ref, N},
            session_loop(Parent, Ref);
        stop ->
            ok
    after 30000 ->
        ok
    end.

collect_within(Ref, Timeout) ->
    receive
        {relay_bound_received, Ref, N} -> [N]
    after Timeout ->
        []
    end.

collect_observed(_Ref, 0, Acc) ->
    lists:reverse(Acc);
collect_observed(Ref, Remaining, Acc) ->
    receive
        {relay_bound_received, Ref, N} -> collect_observed(Ref, Remaining - 1, [N | Acc])
    after 10000 ->
        ?assert(false, {relay_bound_timeout, Remaining, lists:reverse(Acc)})
    end.

with_worker(Fun) ->
    with_workers(1, fun([Worker]) -> Fun(Worker) end).

with_workers(Count, Fun) ->
    Workers = [
        gateway_dispatch_relay_batch:start_worker(Index)
     || Index <- lists:seq(0, Count - 1)
    ],
    try
        with_relay_workers(Workers, fun() -> Fun(Workers) end)
    after
        lists:foreach(fun stop_worker/1, Workers)
    end.

with_relay_workers(Workers, Fun) ->
    Previous = persistent_term:get(?STATE_KEY, undefined),
    persistent_term:put(?STATE_KEY, #{
        workers => list_to_tuple(Workers),
        shard_count => length(Workers)
    }),
    try
        Fun()
    after
        restore_term(?STATE_KEY, Previous)
    end.

with_max_queue(MaxQueue, Fun) ->
    Previous = persistent_term:get(?CONFIG_KEY, undefined),
    Config = gateway_rollout_config:get(),
    persistent_term:put(?CONFIG_KEY, Config#{
        <<"gateway_dispatch_relay_max_queue">> => MaxQueue
    }),
    try
        Fun()
    after
        restore_term(?CONFIG_KEY, Previous)
    end.

restore_term(Key, undefined) ->
    _ = persistent_term:erase(Key),
    ok;
restore_term(Key, Previous) ->
    persistent_term:put(Key, Previous).

stop_worker(Pid) ->
    try gen_server:stop(Pid, normal, 5000) of
        _ -> ok
    catch
        exit:_ -> ok
    end.
