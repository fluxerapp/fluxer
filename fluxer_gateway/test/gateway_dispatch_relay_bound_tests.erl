%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_dispatch_relay_bound_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(STATE_KEY, {gateway_dispatch_relay, state}).
-define(CONFIG_KEY, gateway_rollout_config).
-define(BOUND, 8).
-define(FILL, 6).
-define(EVENT_COUNT, 6).

dispatch_is_bounded_and_stays_ordered_test_() ->
    {timeout, 30, fun dispatch_is_bounded_and_stays_ordered/0}.

dispatch_many_is_bounded_and_stays_ordered_test_() ->
    {timeout, 30, fun dispatch_many_is_bounded_and_stays_ordered/0}.

max_queue_config_governs_the_bound_test_() ->
    {timeout, 30, fun max_queue_config_governs_the_bound/0}.

max_queue_zero_disables_the_bound_test_() ->
    {timeout, 30, fun max_queue_zero_disables_the_bound/0}.

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

max_queue_zero_disables_the_bound() ->
    ?assertEqual(finished, producer_status_with_bound(0, ?BOUND)).

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

collect_observed(_Ref, 0, Acc) ->
    lists:reverse(Acc);
collect_observed(Ref, Remaining, Acc) ->
    receive
        {relay_bound_received, Ref, N} -> collect_observed(Ref, Remaining - 1, [N | Acc])
    after 10000 ->
        ?assert(false, {relay_bound_timeout, Remaining, lists:reverse(Acc)})
    end.

with_worker(Fun) ->
    Worker = gateway_dispatch_relay_batch:start_worker(0),
    try
        with_relay_workers([Worker], fun() -> Fun(Worker) end)
    after
        stop_worker(Worker)
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
