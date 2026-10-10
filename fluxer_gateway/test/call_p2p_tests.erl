%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(call_p2p_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

agreed_joiners_form_a_mesh_without_pending_connections_test() ->
    with_stubs(fun() ->
        State = join_all([{1, true}, {2, true}], call_state()),
        ?assertEqual([true, true], p2p_flags(State)),
        ?assertEqual(#{}, maps:get(pending_connections, State)),
        ?assertEqual([], drain_voice_server_updates())
    end).

sfu_join_keeps_its_pending_connection_test() ->
    with_stubs(fun() ->
        State = join_all([{1, false}], call_state()),
        ?assert(maps:is_key(connection_id(1), maps:get(pending_connections, State)))
    end).

race_sfu_join_after_p2p_start_is_rejected_test() ->
    with_stubs(fun() ->
        State = join_all([{1, true}], call_state()),
        ?assertEqual(
            {reply, {error, voice_p2p_consent_required}, State}, join(2, false, State)
        ),
        ?assertEqual([], drain_voice_server_updates())
    end).

race_p2p_join_after_sfu_start_is_rejected_test() ->
    with_stubs(fun() ->
        State = join_all([{2, false}], call_state()),
        ?assertEqual(
            {reply, {error, voice_p2p_consent_required}, State}, join(1, true, State)
        ),
        ?assertEqual([], drain_voice_server_updates())
    end).

race_async_join_producing_mixed_states_is_rejected_test() ->
    with_stubs(fun() ->
        State = join_all([{1, true}], call_state()),
        ?assertEqual(
            {noreply, State},
            call_voice:handle_join_async(
                2, voice_state(2, false), session_id(2), erlang:get(call_p2p_sink), State
            )
        ),
        receive
            {call_join_result, 1, Result} ->
                ?assertEqual({error, voice_p2p_consent_required}, Result)
        after 1000 ->
            error(call_join_result_not_sent)
        end,
        ?assertEqual([], drain_voice_server_updates())
    end).

fifth_joiner_over_the_cap_is_rejected_test() ->
    with_stubs(fun() ->
        Mesh = join_all([{UserId, true} || UserId <- [1, 2, 3, 4]], call_state()),
        ?assertEqual([true, true, true, true], p2p_flags(Mesh)),
        ?assertEqual({reply, {error, voice_channel_full}, Mesh}, join(5, true, Mesh)),
        ?assertEqual([], drain_voice_server_updates())
    end).

race_join_past_the_configured_cap_is_rejected_test() ->
    with_stubs(fun() ->
        Mesh = join_all([{1, true}, {2, true}], call_state()),
        {reply, ok, Full} = call:handle_call(capped_join(3, 3), from(), Mesh),
        ?assertEqual([true, true, true], p2p_flags(Full)),
        ?assertEqual(
            {reply, {error, voice_channel_full}, Full},
            call:handle_call(capped_join(4, 3), from(), Full)
        ),
        ?assertNot(maps:is_key(4, maps:get(voice_states, Full))),
        ?assertEqual(#{}, maps:get(pending_connections, Full)),
        ?assertEqual([], drain_voice_server_updates())
    end).

join_without_a_cap_falls_back_to_the_hard_ceiling_test() ->
    with_stubs(fun() ->
        Mesh = join_all([{UserId, true} || UserId <- [1, 2, 3]], call_state()),
        {reply, ok, Full} = call:handle_call(
            {join, 4, voice_state(4, true), session_id(4), erlang:get(call_p2p_sink),
                connection_id(4)},
            from(),
            Mesh
        ),
        ?assertEqual([true, true, true, true], p2p_flags(Full))
    end).

session_p2p_grant_passes_the_cap_to_the_call_join_test() ->
    with_stubs(fun() ->
        meck:new(gateway_rpc_call_lookup, [passthrough, no_link]),
        try
            {reply, #{success := true}, _State} = dm_join(true, [voice_state(20, true)]),
            ?assertMatch(
                [{join, 10, #{<<"p2p">> := true}, <<"sess">>, _Pid, _ConnectionId, 3}],
                [
                    Request
                 || {_Caller, {gateway_rpc_call_lookup, safe_gen_server_call, [_, Request, _]},
                        _Result} <- meck:history(gateway_rpc_call_lookup),
                    element(1, Request) =:= join
                ]
            )
        after
            meck:unload(gateway_rpc_call_lookup)
        end
    end).

rejected_join_rolls_back_without_retrying_test() ->
    with_stubs(fun() ->
        erlang:put(call_p2p_join_reply, {error, voice_p2p_consent_required}),
        ok = dm_voice_token:join_or_create_call(
            100, 10, voice_state(10, false), <<"sess">>, self(), undefined
        ),
        ?assertEqual(
            [
                {voice_rejected, voice_p2p_consent_required},
                {call_force_disconnect, 100, connection_id(10)}
            ],
            drain_session_casts()
        ),
        ?assertEqual(1, meck:num_calls(call_manager, lookup, '_'))
    end).

explicit_p2p_false_update_converts_test() ->
    with_stubs(fun() ->
        State0 = join_all([{1, true}, {2, true}], call_state()),
        {reply, ok, State1} = call:handle_call(
            {update_voice_state, 1, voice_state(1, false)}, from(), State0
        ),
        ?assertEqual([false, false], p2p_flags(State1)),
        ?assertMatch(#{<<"p2p">> := false}, receive_voice_state_update(connection_id(2))),
        ?assertEqual(
            [connection_id(1), connection_id(2)], lists:sort(drain_voice_server_updates())
        )
    end).

update_keeping_p2p_keeps_the_mesh_test() ->
    with_stubs(fun() ->
        State0 = join_all([{1, true}, {2, true}], call_state()),
        {reply, ok, State1} = call:handle_call(
            {update_voice_state, 1, (voice_state(1, true))#{<<"self_mute">> => true}},
            from(),
            State0
        ),
        ?assertEqual([true, true], p2p_flags(State1)),
        ?assertEqual([], drain_voice_server_updates())
    end).

stale_p2p_update_after_conversion_stays_sfu_test() ->
    with_stubs(fun() ->
        Mesh = join_all([{1, true}, {2, true}], call_state()),
        {reply, ok, State0} = call:handle_call(
            {update_voice_state, 2, voice_state(2, false)}, from(), Mesh
        ),
        ?assertEqual(2, length(drain_voice_server_updates())),
        ok = flush_user_dispatches(),
        {reply, ok, State1} = call:handle_call(
            {update_voice_state, 1, voice_state(1, true)}, from(), State0
        ),
        ?assertEqual([false, false], p2p_flags(State1)),
        ?assertMatch(#{<<"p2p">> := false}, receive_voice_state_update(connection_id(1))),
        ?assertEqual([], drain_voice_server_updates())
    end).

same_region_update_converts_a_mesh_test() ->
    with_stubs(fun() ->
        State0 = join_all([{1, true}, {2, true}], (call_state())#{region => <<"us-east">>}),
        ok = flush_user_dispatches(),
        {reply, ok, State1} = call:handle_call({update_region, <<"us-east">>}, from(), State0),
        ?assertEqual([false, false], p2p_flags(State1)),
        ?assertEqual(
            [connection_id(1), connection_id(2)], lists:sort(drain_voice_server_updates())
        ),
        ?assertEqual(
            [<<"us-east">>, <<"us-east">>],
            [maps:get(<<"rtc_region">>, Request) || Request <- token_requests()]
        ),
        ?assertMatch(
            #{voice_states := [#{<<"p2p">> := false}, #{<<"p2p">> := false}]},
            receive_user_dispatch(call_update)
        )
    end).

same_region_update_leaves_an_sfu_call_alone_test() ->
    with_stubs(fun() ->
        State0 = join_all([{1, false}, {2, false}], (call_state())#{region => <<"us-east">>}),
        {reply, ok, _State1} = call:handle_call({update_region, <<"us-east">>}, from(), State0),
        ?assertEqual([], drain_voice_server_updates())
    end).

call_event_and_handoff_keep_the_p2p_flag_test() ->
    with_stubs(fun() ->
        State = join_all([{1, true}], call_state()),
        ?assertMatch(
            #{voice_states := [#{<<"p2p">> := true}]}, call_state:build_call_event(State)
        ),
        Restored = call_handoff:restore_state(call_handoff:export_state(State)),
        ?assertEqual([true], p2p_flags(Restored))
    end).

signal_relay_allowed_test() ->
    with_stubs(fun() ->
        State = join_all([{1, true}, {2, true}], call_state()),
        Data = #{<<"type">> => <<"answer">>, <<"sdp">> => <<"v=0">>},
        {noreply, State} = call:handle_cast(
            {voice_signal, session_id(1), connection_id(2), Data}, State
        ),
        ?assertEqual(
            #{
                <<"channel_id">> => <<"1">>,
                <<"from">> => connection_id(1),
                <<"user_id">> => <<"1">>,
                <<"data">> => Data
            },
            receive_voice_signal()
        )
    end).

signal_relay_denied_test() ->
    with_stubs(fun() ->
        Mesh = join_all([{1, true}, {2, true}], call_state()),
        Data = #{<<"type">> => <<"candidate">>},
        Denied = [
            {voice_signal, session_id(9), connection_id(2), Data},
            {voice_signal, session_id(1), connection_id(1), Data},
            {voice_signal, session_id(1), <<"unknown">>, Data}
        ],
        [{noreply, Mesh} = call:handle_cast(Signal, Mesh) || Signal <- Denied],
        {reply, ok, Sfu} = call:handle_call({update_region, <<"us-east">>}, from(), Mesh),
        {noreply, Sfu} = call:handle_cast(
            {voice_signal, session_id(1), connection_id(2), Data}, Sfu
        ),
        assert_no_voice_signal()
    end).

session_p2p_start_without_a_call_test() ->
    with_stubs(fun() ->
        {reply, #{success := true, connection_id := ConnectionId}, State} = dm_join(
            true, not_found
        ),
        ?assertMatch(
            #{
                <<"p2p">> := true,
                <<"p2p_initiator">> := true,
                <<"p2p_participant_count">> := 1,
                <<"country_code">> := <<"SE">>
            },
            lists:last(token_requests())
        ),
        ?assertEqual(
            true, maps:get(<<"p2p">>, maps:get(ConnectionId, maps:get(dm_voice_states, State)))
        ),
        Update = receive_session_voice_server_update(),
        ?assertEqual(
            #{
                <<"p2p">> => true,
                <<"ice_servers">> => [#{<<"urls">> => [<<"stun:stun.example:3478">>]}],
                <<"connection_id">> => ConnectionId,
                <<"channel_id">> => <<"100">>
            },
            Update
        )
    end).

session_p2p_join_of_an_existing_mesh_test() ->
    with_stubs(fun() ->
        {reply, #{success := true}, _State} = dm_join(true, [voice_state(20, true)]),
        ?assertMatch(
            #{
                <<"p2p">> := true,
                <<"p2p_initiator">> := false,
                <<"p2p_participant_count">> := 2
            },
            lists:last(token_requests())
        ),
        ?assertMatch(#{<<"p2p">> := true}, receive_session_voice_server_update())
    end).

session_join_from_a_second_device_counts_the_user_once_test() ->
    with_stubs(fun() ->
        {reply, #{success := true}, _State} = dm_join(
            true, [voice_state(20, true), voice_state(10, true)]
        ),
        ?assertMatch(
            #{
                <<"p2p">> := true,
                <<"p2p_initiator">> := false,
                <<"p2p_participant_count">> := 2
            },
            lists:last(token_requests())
        )
    end).

session_join_of_an_sfu_call_does_not_ask_for_p2p_test() ->
    with_stubs(fun() ->
        {reply, #{success := true, connection_id := ConnectionId}, State} = dm_join(
            true, [voice_state(20, false)]
        ),
        ?assertNot(maps:is_key(<<"p2p">>, lists:last(token_requests()))),
        ?assertEqual(
            false, maps:get(<<"p2p">>, maps:get(ConnectionId, maps:get(dm_voice_states, State)))
        ),
        Update = receive_session_voice_server_update(),
        ?assertEqual(<<"tok">>, maps:get(<<"token">>, Update)),
        ?assertNot(maps:is_key(<<"p2p">>, Update))
    end).

session_join_of_a_mesh_without_agreement_is_rejected_test() ->
    with_stubs(fun() ->
        ?assertMatch(
            {reply, {error, permission_denied, voice_p2p_consent_required}, #{
                dm_voice_states := #{}
            }},
            dm_join_reply(undefined, [voice_state(20, true)])
        ),
        ?assertEqual([], token_requests()),
        assert_no_session_voice_server_update()
    end).

session_bot_join_of_a_mesh_is_rejected_test() ->
    with_stubs(fun() ->
        erlang:put(call_p2p_call_voice_states, [voice_state(20, true)]),
        State = dm_session_state(#{}),
        ?assertMatch(
            {reply, {error, permission_denied, voice_p2p_consent_required}, _State},
            dm_voice_token:get_dm_voice_token_and_create_state(
                (dm_request(true, State))#{bot => true}
            )
        )
    end).

session_join_of_a_full_mesh_is_rejected_test() ->
    with_stubs(fun() ->
        Mesh = [voice_state(UserId, true) || UserId <- [20, 21, 22, 23]],
        ?assertMatch(
            {reply, {error, permission_denied, voice_channel_full}, _State},
            dm_join_reply(true, Mesh)
        ),
        ?assertEqual([], token_requests())
    end).

session_join_past_the_configured_cap_is_rejected_as_full_test() ->
    with_stubs(fun() ->
        erlang:put(call_p2p_stub_mode, channel_full),
        ?assertMatch(
            {reply, {error, permission_denied, voice_channel_full}, #{dm_voice_states := #{}}},
            dm_join_reply(true, [voice_state(20, true), voice_state(21, true)])
        ),
        ?assertMatch(
            #{
                <<"p2p">> := true,
                <<"p2p_initiator">> := false,
                <<"p2p_participant_count">> := 3
            },
            lists:last(token_requests())
        ),
        assert_no_session_voice_server_update()
    end).

session_declined_grant_into_a_mesh_is_rejected_test() ->
    with_stubs(fun() ->
        erlang:put(call_p2p_stub_mode, decline),
        ?assertMatch(
            {reply, {error, voice_error, voice_p2p_unavailable}, #{dm_voice_states := #{}}},
            dm_join_reply(true, [voice_state(20, true)])
        ),
        ?assertMatch(
            #{<<"p2p">> := true, <<"p2p_initiator">> := false}, lists:last(token_requests())
        ),
        assert_no_session_voice_server_update()
    end).

session_declined_grant_without_a_call_joins_sfu_test() ->
    with_stubs(fun() ->
        erlang:put(call_p2p_stub_mode, decline),
        {reply, #{success := true, connection_id := ConnectionId}, State} = dm_join(
            true, not_found
        ),
        ?assertEqual(
            false, maps:get(<<"p2p">>, maps:get(ConnectionId, maps:get(dm_voice_states, State)))
        ),
        ?assertEqual(<<"tok">>, maps:get(<<"token">>, receive_session_voice_server_update()))
    end).

session_update_with_explicit_p2p_false_clears_the_flag_test() ->
    with_stubs(fun() ->
        ?assertEqual(false, dm_updated_p2p(false)),
        ?assertEqual(true, dm_updated_p2p(undefined)),
        ?assertEqual(true, dm_updated_p2p(true))
    end).

dm_updated_p2p(P2p) ->
    Existing = (voice_state(10, true))#{
        <<"channel_id">> => <<"100">>, <<"session_id">> => <<"sess">>
    },
    ConnectionId = connection_id(10),
    State = dm_session_state(#{ConnectionId => Existing}),
    {reply, #{success := true}, NewState} = dm_voice_connect:handle_dm_connect_or_update(
        (dm_request(P2p, State))#{
            connection_id => ConnectionId, viewer_stream_keys => undefined
        }
    ),
    maps:get(<<"p2p">>, maps:get(ConnectionId, maps:get(dm_voice_states, NewState))).

dm_join_reply(P2p, CallVoiceStates) ->
    erlang:put(call_p2p_call_voice_states, CallVoiceStates),
    State = dm_session_state(#{}),
    dm_voice_token:get_dm_voice_token_and_create_state(dm_request(P2p, State)).

dm_join(P2p, CallVoiceStates) ->
    Reply = dm_join_reply(P2p, CallVoiceStates),
    receive
        {'$gen_cast', {call_monitor, 100, _CallPid}} -> Reply
    after 1000 ->
        error(call_not_joined)
    end.

dm_session_state(VoiceStates) ->
    #{
        dm_voice_states => VoiceStates,
        channels => #{100 => #{<<"type">> => 1, <<"recipient_ids">> => [20]}},
        user_id => 10,
        id => <<"sess">>,
        session_pid => erlang:get(call_p2p_sink)
    }.

dm_request(P2p, State) ->
    #{
        user_id => 10,
        channel_id => 100,
        session_id => <<"sess">>,
        connection_id => null,
        self_mute => false,
        self_deaf => false,
        self_video => false,
        self_stream => false,
        viewer_stream_keys => [],
        is_mobile => false,
        latitude => null,
        longitude => null,
        e2ee_capable => false,
        bot => false,
        p2p => P2p,
        country_code => <<"SE">>,
        voice_states => maps:get(dm_voice_states, State),
        state => State
    }.

join_all(Joins, State0) ->
    lists:foldl(
        fun({UserId, P2p}, State) ->
            {reply, ok, NewState} = join(UserId, P2p, State),
            NewState
        end,
        State0,
        Joins
    ).

join(UserId, P2p, State) ->
    call_voice:handle_join_internal(
        UserId,
        voice_state(UserId, P2p),
        session_id(UserId),
        erlang:get(call_p2p_sink),
        connection_id(UserId),
        undefined,
        State
    ).

capped_join(UserId, MaxParticipants) ->
    {join, UserId, voice_state(UserId, true), session_id(UserId), erlang:get(call_p2p_sink),
        connection_id(UserId), MaxParticipants}.

p2p_flags(State) ->
    [
        maps:get(<<"p2p">>, VoiceState)
     || {_UserId, VoiceState} <- lists:keysort(1, maps:to_list(maps:get(voice_states, State)))
    ].

voice_state(UserId, P2p) ->
    #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => connection_id(UserId),
        <<"session_id">> => session_id(UserId),
        <<"p2p">> => P2p
    }.

connection_id(UserId) ->
    <<"conn-", (integer_to_binary(UserId))/binary>>.

session_id(UserId) ->
    <<"sess-", (integer_to_binary(UserId))/binary>>.

from() ->
    {self(), make_ref()}.

call_state() ->
    #{
        channel_id => 1,
        message_id => 1,
        region => undefined,
        ringing => [],
        pending_ringing => [],
        recipients => [1],
        voice_states => #{},
        sessions => #{},
        pending_connections => #{},
        initiator_ready => true,
        ringing_timers => #{},
        idle_timer => undefined,
        created_at => 0,
        participants_history => sets:new(),
        last_call_event => undefined
    }.

token_requests() ->
    [
        Request
     || {_Pid, {rpc_client, call, [#{<<"type">> := <<"voice_get_token">>} = Request]}, _Result} <-
            meck:history(rpc_client)
    ].

drain_voice_server_updates() ->
    receive
        {'$gen_cast', {dispatch, voice_server_update, Payload}} ->
            ?assertEqual(<<"tok">>, maps:get(<<"token">>, Payload)),
            ?assertNot(maps:is_key(<<"p2p">>, Payload)),
            [maps:get(<<"connection_id">>, Payload) | drain_voice_server_updates()]
    after 200 ->
        []
    end.

receive_session_voice_server_update() ->
    receive
        {'$gen_cast', {dispatch, voice_server_update, Payload}} -> Payload
    after 1000 ->
        error(voice_server_update_not_dispatched)
    end.

assert_no_session_voice_server_update() ->
    receive
        {'$gen_cast', {dispatch, voice_server_update, Payload}} ->
            error({unexpected_voice_server_update, Payload})
    after 100 ->
        ok
    end.

drain_session_casts() ->
    receive
        {'$gen_cast', Cast} -> [Cast | drain_session_casts()]
    after 100 ->
        []
    end.

receive_voice_state_update(ConnectionId) ->
    case receive_user_dispatch(voice_state_update) of
        #{<<"connection_id">> := ConnectionId} = VoiceState -> VoiceState;
        _Other -> receive_voice_state_update(ConnectionId)
    end.

receive_user_dispatch(Event) ->
    receive
        {user_dispatch, Event, Data} -> Data
    after 1000 ->
        error({user_dispatch_not_received, Event})
    end.

flush_user_dispatches() ->
    receive
        {user_dispatch, _Event, _Data} -> flush_user_dispatches()
    after 100 ->
        ok
    end.

receive_voice_signal() ->
    receive
        {'$gen_cast', {dispatch, voice_signal, Payload}} -> Payload
    after 1000 ->
        error(voice_signal_not_dispatched)
    end.

assert_no_voice_signal() ->
    receive
        {'$gen_cast', {dispatch, voice_signal, Payload}} ->
            error({unexpected_voice_signal, Payload})
    after 100 ->
        ok
    end.

with_stubs(Fun) ->
    Test = self(),
    Sink = spawn(fun() -> sink(Test) end),
    erlang:put(call_p2p_sink, Sink),
    erlang:erase(call_p2p_call_voice_states),
    erlang:erase(call_p2p_stub_mode),
    erlang:erase(call_p2p_join_reply),
    meck:new(rpc_client, [passthrough, no_link, non_strict]),
    meck:expect(rpc_client, call, fun rpc_reply/1),
    meck:new(presence_manager, [passthrough, no_link]),
    meck:expect(presence_manager, dispatch_to_user, fun(_UserId, Event, Data) ->
        Sink ! {user_dispatch, Event, Data},
        ok
    end),
    CallPid = spawn(fun call_stub/0),
    meck:new(call_manager, [passthrough, no_link]),
    meck:expect(call_manager, lookup, fun(_ChannelId) ->
        case erlang:get(call_p2p_call_voice_states) of
            not_found ->
                {error, not_found};
            VoiceStates when is_list(VoiceStates) ->
                CallPid ! {voice_states, VoiceStates},
                {ok, CallPid};
            undefined ->
                CallPid ! {join_reply, erlang:get(call_p2p_join_reply)},
                {ok, CallPid}
        end
    end),
    try
        Fun()
    after
        CallPid ! stop,
        meck:unload(call_manager),
        meck:unload(presence_manager),
        meck:unload(rpc_client),
        stop_sink(Sink)
    end.

sink(Test) ->
    receive
        Message ->
            Test ! Message,
            sink(Test)
    after 30000 ->
        ok
    end.

stop_sink(Sink) ->
    exit(Sink, kill),
    flush_mailbox().

flush_mailbox() ->
    receive
        _Message -> flush_mailbox()
    after 0 ->
        ok
    end.

call_stub() ->
    call_stub([], ok).

call_stub(VoiceStates, JoinReply) ->
    receive
        {voice_states, NewVoiceStates} ->
            call_stub(NewVoiceStates, JoinReply);
        {join_reply, undefined} ->
            call_stub(VoiceStates, JoinReply);
        {join_reply, NewJoinReply} ->
            call_stub(VoiceStates, NewJoinReply);
        {'$gen_call', From, {get_state}} ->
            gen_server:reply(From, {ok, #{voice_states => VoiceStates, region => undefined}}),
            call_stub(VoiceStates, JoinReply);
        {'$gen_call', From, _Request} ->
            gen_server:reply(From, JoinReply),
            call_stub(VoiceStates, JoinReply);
        stop ->
            ok;
        _Other ->
            call_stub(VoiceStates, JoinReply)
    after 30000 ->
        ok
    end.

rpc_reply(#{<<"type">> := <<"voice_get_token">>} = Request) ->
    ConnectionId = maps:get(<<"connection_id">>, Request, <<"conn-new">>),
    case {maps:get(<<"p2p">>, Request, false), erlang:get(call_p2p_stub_mode)} of
        {true, channel_full} ->
            {ok, #{<<"p2p">> => false, <<"declineReason">> => <<"channel_full">>}};
        {true, Mode} when Mode =/= decline ->
            {ok, #{
                <<"p2p">> => true,
                <<"connectionId">> => ConnectionId,
                <<"iceServers">> => [#{<<"urls">> => [<<"stun:stun.example:3478">>]}],
                <<"maxParticipants">> => 3
            }};
        _ ->
            {ok, #{
                <<"token">> => <<"tok">>,
                <<"endpoint">> => <<"wss://voice.example">>,
                <<"connectionId">> => ConnectionId
            }}
    end;
rpc_reply(_Request) ->
    {ok, #{}}.
