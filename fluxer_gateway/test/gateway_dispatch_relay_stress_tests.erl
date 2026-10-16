%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_dispatch_relay_stress_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(STATE_KEY, {gateway_dispatch_relay, state}).
-define(RELAY_SESSION_COUNT, 2048).

dispatch_many_direct_fallback_delivers_once_to_many_sessions_test_() ->
    {timeout, 30, fun dispatch_many_direct_fallback_delivers_once_to_many_sessions/0}.

dispatch_many_worker_shards_deliver_once_to_many_sessions_test_() ->
    {timeout, 30, fun dispatch_many_worker_shards_deliver_once_to_many_sessions/0}.

dispatch_many_backpressured_worker_still_relays_delivery_test_() ->
    {timeout, 30, fun dispatch_many_backpressured_worker_still_relays_delivery/0}.

dispatch_many_direct_fallback_delivers_once_to_many_sessions() ->
    with_relay_state_cleared(fun() ->
        {Receivers, Ref} = start_receivers(?RELAY_SESSION_COUNT),
        try
            Payload = #{<<"stress">> => <<"direct">>},
            ok = gateway_dispatch_relay:dispatch_many(Receivers, relay_stress_event, Payload),
            assert_received_once(Receivers, Ref, relay_stress_event, Payload)
        after
            stop_receivers(Receivers)
        end
    end).

dispatch_many_worker_shards_deliver_once_to_many_sessions() ->
    Workers = gateway_dispatch_relay_batch:start_workers(8),
    with_relay_workers(Workers, fun() ->
        {Receivers, Ref} = start_receivers(?RELAY_SESSION_COUNT),
        try
            Payload = #{<<"stress">> => <<"workers">>},
            ok = gateway_dispatch_relay:dispatch_many(Receivers, relay_stress_event, Payload),
            assert_received_once(Receivers, Ref, relay_stress_event, Payload),
            assert_worker_queues_drained(Workers)
        after
            stop_receivers(Receivers)
        end
    end),
    stop_workers(Workers).

dispatch_many_backpressured_worker_still_relays_delivery() ->
    Worker = gateway_dispatch_relay_batch:start_worker(0),
    with_relay_workers([Worker], fun() ->
        ok = sys:suspend(Worker),
        backpressure_worker(Worker, 64),
        {Receivers, Ref} = start_receivers(512),
        try
            Payload = #{<<"stress">> => <<"backpressure">>},
            ok = gateway_dispatch_relay_batch:relay_or_direct_many(
                Receivers, relay_stress_event, Payload
            ),
            ?assert(gateway_dispatch_relay_batch:message_queue_len(Worker) >= 64),
            ok = sys:resume(Worker),
            assert_received_once(Receivers, Ref, relay_stress_event, Payload),
            assert_worker_queues_drained([Worker])
        after
            stop_receivers(Receivers)
        end
    end),
    stop_worker(Worker).

with_relay_state_cleared(Fun) ->
    Previous = persistent_term:get(?STATE_KEY, undefined),
    _ = persistent_term:erase(?STATE_KEY),
    try
        Fun()
    after
        restore_relay_state(Previous)
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
    persistent_term:put(?STATE_KEY, Previous),
    ok.

start_receivers(Count) ->
    Ref = make_ref(),
    Parent = self(),
    Receivers = [
        spawn_link(fun() -> receiver_loop(Parent, Ref) end)
     || _ <- lists:seq(1, Count)
    ],
    {Receivers, Ref}.

receiver_loop(Parent, Ref) ->
    receive
        {'$gen_cast', {dispatch, Event, Payload}} ->
            Parent ! {relay_stress_received, Ref, self(), Event, Payload},
            receiver_loop(Parent, Ref);
        stop ->
            ok
    after 30000 ->
        ok
    end.

stop_receivers(Receivers) ->
    lists:foreach(fun(Pid) -> Pid ! stop end, Receivers).

stop_workers(Workers) ->
    lists:foreach(fun stop_worker/1, Workers).

stop_worker(Pid) ->
    try gen_server:stop(Pid, normal, 5000) of
        _ -> ok
    catch
        exit:_ -> ok
    end.

backpressure_worker(Worker, MessageCount) ->
    lists:foreach(fun(I) -> Worker ! {queued, I} end, lists:seq(1, MessageCount)).

assert_received_once(Receivers, Ref, Event, Payload) ->
    Expected = maps:from_list([{Pid, false} || Pid <- Receivers]),
    Seen = collect_received(length(Receivers), Ref, Event, Payload, Expected),
    ?assertEqual(maps:from_list([{Pid, true} || Pid <- Receivers]), Seen),
    assert_no_extra_received(Ref).

collect_received(0, _Ref, _Event, _Payload, Seen) ->
    Seen;
collect_received(Remaining, Ref, Event, Payload, Seen) ->
    receive
        {relay_stress_received, Ref, Pid, Event, Payload} ->
            ?assertEqual(false, maps:get(Pid, Seen, duplicate)),
            collect_received(Remaining - 1, Ref, Event, Payload, Seen#{Pid := true});
        {relay_stress_received, Ref, Pid, OtherEvent, OtherPayload} ->
            ?assert(false, {unexpected_relay_payload, Pid, OtherEvent, OtherPayload})
    after 5000 ->
        Missing = [Pid || {Pid, false} <- maps:to_list(Seen)],
        ?assert(false, {relay_stress_timeout, Remaining, length(Missing)})
    end.

assert_no_extra_received(Ref) ->
    receive
        {relay_stress_received, Ref, Pid, Event, Payload} ->
            ?assert(false, {duplicate_relay_delivery, Pid, Event, Payload})
    after 100 ->
        ok
    end.

assert_worker_queues_drained(Workers) ->
    ok = wait_until(fun() ->
        lists:all(
            fun(Pid) -> gateway_dispatch_relay_batch:message_queue_len(Pid) =:= 0 end,
            Workers
        )
    end).

wait_until(Pred) ->
    Deadline = erlang:monotonic_time(millisecond) + 5000,
    wait_until(Pred, Deadline).

wait_until(Pred, Deadline) ->
    case Pred() of
        true ->
            ok;
        false ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true ->
                    ?assert(false, wait_until_timeout);
                false ->
                    timer:sleep(10),
                    wait_until(Pred, Deadline)
            end
    end.
