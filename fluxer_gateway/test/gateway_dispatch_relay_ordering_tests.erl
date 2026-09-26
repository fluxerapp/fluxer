%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_dispatch_relay_ordering_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(STATE_KEY, {gateway_dispatch_relay, state}).
-define(EVENT_COUNT, 8).

dispatch_preserves_order_when_worker_is_backpressured_test_() ->
    {timeout, 30, fun dispatch_preserves_order_when_worker_is_backpressured/0}.

dispatch_many_preserves_order_when_worker_is_backpressured_test_() ->
    {timeout, 30, fun dispatch_many_preserves_order_when_worker_is_backpressured/0}.

dispatch_preserves_order_when_worker_is_backpressured() ->
    assert_ordered_delivery(fun(SessionPid, N) ->
        gateway_dispatch_relay:dispatch(SessionPid, relay_order_event, #{<<"n">> => N})
    end).

dispatch_many_preserves_order_when_worker_is_backpressured() ->
    assert_ordered_delivery(fun(SessionPid, N) ->
        gateway_dispatch_relay:dispatch_many([SessionPid], relay_order_event, #{<<"n">> => N})
    end).

assert_ordered_delivery(SendFun) ->
    Worker = gateway_dispatch_relay_batch:start_worker(0),
    try
        with_relay_workers([Worker], fun() -> ordered_delivery(Worker, SendFun) end)
    after
        stop_worker(Worker)
    end.

ordered_delivery(Worker, SendFun) ->
    Ref = make_ref(),
    Parent = self(),
    SessionPid = spawn_link(fun() -> session_loop(Parent, Ref) end),
    MaxQueue = gateway_dispatch_relay_batch:max_queue(),
    ok = sys:suspend(Worker),
    fill_queue(Worker, MaxQueue - ?EVENT_COUNT),
    lists:foreach(fun(N) -> ok = SendFun(SessionPid, N) end, lists:seq(1, ?EVENT_COUNT)),
    ?assert(gateway_dispatch_relay_batch:message_queue_len(Worker) >= MaxQueue),
    ok = sys:resume(Worker),
    Observed = collect_observed(Ref, ?EVENT_COUNT, []),
    SessionPid ! stop,
    ?assertEqual(lists:seq(1, ?EVENT_COUNT), Observed).

fill_queue(_Worker, 0) ->
    ok;
fill_queue(Worker, Remaining) ->
    Worker ! relay_order_filler,
    fill_queue(Worker, Remaining - 1).

session_loop(Parent, Ref) ->
    receive
        {'$gen_cast', {dispatch, relay_order_event, #{<<"n">> := N}}} ->
            Parent ! {relay_order_received, Ref, N},
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
        {relay_order_received, Ref, N} -> collect_observed(Ref, Remaining - 1, [N | Acc])
    after 10000 ->
        ?assert(false, {relay_order_timeout, Remaining, lists:reverse(Acc)})
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
        restore_relay_state(Previous)
    end.

restore_relay_state(undefined) ->
    _ = persistent_term:erase(?STATE_KEY),
    ok;
restore_relay_state(Previous) ->
    persistent_term:put(?STATE_KEY, Previous).

stop_worker(Pid) ->
    try gen_server:stop(Pid, normal, 5000) of
        _ -> ok
    catch
        exit:_ -> ok
    end.
