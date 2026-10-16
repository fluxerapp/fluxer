%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_dispatch_send_ordering_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(SATURATION_FILL, 600).
-define(RECEIVE_TIMEOUT_MS, 600).
-define(BROADCASTER_IDLE_TIMEOUT_MS, 30000).

saturated_broadcaster_keeps_dispatch_ordered_test_() ->
    {timeout, 30, fun saturated_broadcaster_keeps_dispatch_ordered/0}.

healthy_broadcaster_keeps_dispatch_ordered_test_() ->
    {timeout, 30, fun healthy_broadcaster_keeps_dispatch_ordered/0}.

absent_broadcaster_dispatches_through_relay_test_() ->
    {timeout, 30, fun absent_broadcaster_dispatches_through_relay/0}.

saturated_broadcaster_keeps_dispatch_ordered() ->
    Broadcaster = spawn_idle_broadcaster(),
    try
        saturate(Broadcaster),
        ?assertEqual(false, cast_event(Broadcaster)),
        QueueBefore = queue_len(Broadcaster),
        flush_dispatches(),
        dispatch(Broadcaster),
        ?assertNot(received_relay_dispatch()),
        ?assertEqual(QueueBefore + 1, queue_len(Broadcaster))
    after
        stop_broadcaster(Broadcaster)
    end.

healthy_broadcaster_keeps_dispatch_ordered() ->
    Broadcaster = spawn_draining_broadcaster(),
    try
        flush_dispatches(),
        dispatch(Broadcaster),
        ?assertNot(received_relay_dispatch())
    after
        stop_broadcaster(Broadcaster)
    end.

absent_broadcaster_dispatches_through_relay() ->
    flush_dispatches(),
    dispatch(undefined),
    ?assert(received_relay_dispatch()).

dispatch(Broadcaster) ->
    _ = guild_dispatch_send:dispatch_to_sessions(
        [session()], message_update, event_data(), state(Broadcaster)
    ),
    ok.

cast_event(Broadcaster) ->
    guild_broadcaster:cast_event(Broadcaster, message_update, {pre_encoded, <<"{}">>}, [self()]).

saturate(Broadcaster) ->
    lists:foreach(
        fun(N) -> Broadcaster ! {filler, N} end,
        lists:seq(1, ?SATURATION_FILL)
    ).

spawn_idle_broadcaster() ->
    spawn(fun idle_broadcaster/0).

idle_broadcaster() ->
    receive
        stop -> ok
    after ?BROADCASTER_IDLE_TIMEOUT_MS -> ok
    end.

spawn_draining_broadcaster() ->
    spawn(fun draining_broadcaster/0).

draining_broadcaster() ->
    receive
        stop -> ok;
        _Other -> draining_broadcaster()
    after ?BROADCASTER_IDLE_TIMEOUT_MS -> ok
    end.

stop_broadcaster(Broadcaster) ->
    Broadcaster ! stop,
    ok.

queue_len(Pid) ->
    {message_queue_len, Len} = erlang:process_info(Pid, message_queue_len),
    Len.

received_relay_dispatch() ->
    receive
        {'$gen_cast', {dispatch, message_update, _Payload}} -> true
    after ?RECEIVE_TIMEOUT_MS -> false
    end.

flush_dispatches() ->
    receive
        {'$gen_cast', _Cast} -> flush_dispatches()
    after 0 -> ok
    end.

state(undefined) ->
    base_state();
state(Broadcaster) ->
    (base_state())#{broadcaster_pid => Broadcaster}.

base_state() ->
    #{
        id => 42,
        member_count => 100,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [],
            <<"members">> => [],
            <<"channels">> => []
        }
    }.

session() ->
    {<<"ordered">>, #{
        session_id => <<"ordered">>,
        user_id => 10,
        pid => self(),
        active_guilds => sets:new(),
        bot => false,
        viewable_channels => #{100 => true}
    }}.

event_data() ->
    #{<<"guild_id">> => <<"42">>, <<"id">> => <<"7">>, <<"channel_id">> => <<"100">>}.
