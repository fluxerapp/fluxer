%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_p2p_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(GUILD_ID, 999).
-define(CHANNEL_ID, 100).
-define(OTHER_CHANNEL_ID, 200).
-define(PINNED_CHANNEL_ID, 300).

p2p_start_in_an_empty_channel_test() ->
    with_stubs(grant, fun(State0) ->
        {Reply, State1} = join(10, true, State0),
        ConnectionId = maps:get(connection_id, Reply),
        ?assertMatch(#{success := true, p2p := true, ice_servers := [_]}, Reply),
        ?assertNot(maps:is_key(token, Reply)),
        ?assertNot(maps:is_key(endpoint, Reply)),
        ?assertEqual(true, p2p_flag(ConnectionId, State1)),
        ?assertEqual(#{}, maps:get(pending_voice_connections, State1)),
        ?assertMatch(
            #{
                <<"p2p">> := true,
                <<"p2p_initiator">> := true,
                <<"p2p_participant_count">> := 1,
                <<"country_code">> := <<"SE">>
            },
            last_token_request()
        ),
        ?assertEqual([{ConnectionId, true}], drain_voice_state_updates())
    end).

agreed_joiner_joins_the_mesh_test() ->
    with_stubs(grant, fun(State0) ->
        {ReplyA, State1} = join(10, true, State0),
        {ReplyB, State2} = join(11, true, State1),
        ?assertMatch(#{p2p := true}, ReplyB),
        ?assertMatch(
            #{
                <<"p2p">> := true,
                <<"p2p_initiator">> := false,
                <<"p2p_participant_count">> := 2
            },
            last_token_request()
        ),
        ?assertEqual(true, p2p_flag(maps:get(connection_id, ReplyA), State2)),
        ?assertEqual(true, p2p_flag(maps:get(connection_id, ReplyB), State2)),
        ?assertEqual([], served_voice_server_updates(State2))
    end).

non_agreeing_joiner_is_rejected_and_nobody_is_converted_test() ->
    with_stubs(grant, fun(State0) ->
        {Mesh, State1} = join_mesh([10, 11], State0),
        Requests = token_request_count(),
        {Reply, State2} = join(12, undefined, State1),
        ?assertEqual({error, permission_denied, voice_p2p_consent_required}, Reply),
        ?assertEqual(Requests, token_request_count()),
        assert_untouched(Mesh, State1, State2)
    end).

bot_is_rejected_like_a_non_agreeing_joiner_test() ->
    with_stubs(grant, fun(State0) ->
        {Mesh, State1} = join_mesh([10, 11], State0),
        {reply, Reply, State2} = guild_voice_connection:voice_state_update(
            (request(12, true))#{bot => true}, State1
        ),
        ?assertEqual({error, permission_denied, voice_p2p_consent_required}, Reply),
        assert_untouched(Mesh, State1, State2)
    end).

fifth_joiner_is_rejected_as_full_test() ->
    with_stubs(grant, fun(State0) ->
        {Mesh, State1} = join_mesh([10, 11, 12, 13], State0),
        ?assertEqual(4, length(Mesh)),
        Requests = token_request_count(),
        {Reply, State2} = join(14, true, State1),
        ?assertEqual({error, permission_denied, voice_channel_full}, Reply),
        ?assertEqual(Requests, token_request_count()),
        assert_untouched(Mesh, State1, State2)
    end).

joiner_past_the_configured_cap_is_rejected_as_full_test() ->
    with_stubs({cap, 2}, fun(State0) ->
        {Mesh, State1} = join_mesh([10, 11], State0),
        {Reply, State2} = join(12, true, State1),
        ?assertMatch(
            #{
                <<"p2p">> := true,
                <<"p2p_initiator">> := false,
                <<"p2p_participant_count">> := 3
            },
            last_token_request()
        ),
        ?assertEqual({error, permission_denied, voice_channel_full}, Reply),
        assert_untouched(Mesh, State1, State2)
    end).

mesh_fills_up_to_the_configured_cap_test() ->
    with_stubs({cap, 3}, fun(State0) ->
        {[_, _, _] = Mesh, State1} = join_mesh([10, 11, 12], State0),
        [?assertEqual(true, p2p_flag(ConnectionId, State1)) || ConnectionId <- Mesh],
        {Reply, _State2} = join(13, true, State1),
        ?assertEqual({error, permission_denied, voice_channel_full}, Reply)
    end).

declined_grant_into_a_live_mesh_is_rejected_test() ->
    with_stubs(grant, fun(State0) ->
        {Mesh, State1} = join_mesh([10, 11], State0),
        erlang:put(p2p_stub_mode, decline),
        {Reply, State2} = join(12, true, State1),
        ?assertMatch(#{<<"p2p">> := true, <<"p2p_initiator">> := false}, last_token_request()),
        ?assertEqual({error, voice_error, voice_p2p_unavailable}, Reply),
        assert_untouched(Mesh, State1, State2)
    end).

declined_grant_on_an_empty_channel_joins_sfu_test() ->
    with_stubs(decline, fun(State0) ->
        {Reply, State1} = join(10, true, State0),
        ?assertMatch(#{<<"p2p">> := true, <<"p2p_initiator">> := true}, last_token_request()),
        ?assertMatch(#{success := true, token := _, endpoint := _}, Reply),
        ?assertNot(maps:is_key(p2p, Reply)),
        ?assertEqual(#{}, maps:get(voice_states, State1)),
        ?assertEqual(
            [maps:get(connection_id, Reply)],
            maps:keys(maps:get(pending_voice_connections, State1))
        )
    end).

pinned_empty_channel_joins_a_non_agreeing_joiner_on_sfu_test() ->
    with_stubs(grant, fun(State0) ->
        {reply, Reply, _State1} = guild_voice_connection:voice_state_update(
            (request(10, undefined))#{channel_id => ?PINNED_CHANNEL_ID}, State0
        ),
        ?assertMatch(#{success := true, token := _}, Reply),
        ?assertNot(maps:is_key(<<"p2p">>, last_token_request()))
    end).

pinned_channel_joins_sfu_when_the_grant_is_declined_test() ->
    with_stubs(decline, fun(State0) ->
        {reply, Reply, _State1} = guild_voice_connection:voice_state_update(
            (request(10, true))#{channel_id => ?PINNED_CHANNEL_ID}, State0
        ),
        ?assertMatch(#{success := true, token := _}, Reply)
    end).

pending_join_blocks_a_p2p_start_test() ->
    with_stubs(grant, fun(State0) ->
        {_Reply, State1} = join(10, undefined, State0),
        ?assertEqual(1, maps:size(maps:get(pending_voice_connections, State1))),
        {Reply, _State2} = join(11, true, State1),
        ?assertMatch(#{token := _}, Reply),
        ?assertNot(maps:is_key(<<"p2p">>, last_token_request()))
    end).

explicit_p2p_false_update_converts_test() ->
    with_stubs(grant, fun(State0) ->
        {[ConnectionA | _] = Mesh, State1} = join_mesh([10, 11], State0),
        {reply, #{success := true}, State2} = guild_voice_connection:voice_state_update(
            (request(10, false))#{connection_id => ConnectionA}, State1
        ),
        assert_converted(Mesh, State2)
    end).

explicit_p2p_false_update_is_ignored_in_a_pinned_channel_test() ->
    with_stubs(grant, fun(State0) ->
        {[ConnectionA | _] = Mesh, State1} = join_mesh([10, 11], ?PINNED_CHANNEL_ID, State0),
        _ = drain_voice_state_updates(),
        {reply, #{success := true}, State2} = guild_voice_connection:voice_state_update(
            (request(10, false))#{
                channel_id => ?PINNED_CHANNEL_ID, connection_id => ConnectionA
            },
            State1
        ),
        assert_untouched(Mesh, State1, State2)
    end).

update_without_p2p_keeps_the_mesh_test() ->
    with_stubs(grant, fun(State0) ->
        {[ConnectionA, ConnectionB], State1} = join_mesh([10, 11], State0),
        {reply, #{success := true}, State2} = guild_voice_connection:voice_state_update(
            (request(10, undefined))#{connection_id => ConnectionA, self_mute => true},
            State1
        ),
        ?assertEqual(true, p2p_flag(ConnectionA, State2)),
        ?assertEqual(true, p2p_flag(ConnectionB, State2)),
        ?assertEqual(
            true,
            maps:get(<<"self_mute">>, maps:get(ConnectionA, maps:get(voice_states, State2)))
        )
    end).

region_switch_converts_test() ->
    with_stubs(grant, fun(State0) ->
        {Mesh, State1} = join_mesh([10, 11], State0),
        {reply, #{success := true}, State2} = guild_voice_region:switch_voice_region_handler(
            #{channel_id => ?CHANNEL_ID}, State1
        ),
        assert_marked_sfu(Mesh, State2),
        GuildPid = maps:get(guild_pid, State2),
        GuildPid ! {serve, State2},
        ok = guild_voice_region:switch_voice_region(?GUILD_ID, ?CHANNEL_ID, GuildPid),
        ?assertEqual(lists:sort(Mesh), lists:sort(drain_voice_server_updates(length(Mesh))))
    end).

region_switch_keeps_a_pinned_channel_p2p_test() ->
    with_stubs(grant, fun(State0) ->
        {Mesh, State1} = join_mesh([10, 11], ?PINNED_CHANNEL_ID, State0),
        _ = drain_voice_state_updates(),
        {reply, #{success := true}, State2} = guild_voice_region:switch_voice_region_handler(
            #{channel_id => ?PINNED_CHANNEL_ID}, State1
        ),
        GuildPid = maps:get(guild_pid, State2),
        GuildPid ! {serve, State2},
        ok = guild_voice_region:switch_voice_region(?GUILD_ID, ?PINNED_CHANNEL_ID, GuildPid),
        assert_untouched(Mesh, State1, State2)
    end).

livekit_restore_into_a_mesh_is_refused_test() ->
    with_stubs(grant, fun(State0) ->
        {Mesh, State1} = join_mesh([10, 11], State0),
        Stale = (maps:get(hd(Mesh), maps:get(voice_states, State1)))#{
            <<"connection_id">> => <<"stale">>, <<"user_id">> => <<"12">>, <<"p2p">> => false
        },
        State2 = State1#{
            recently_disconnected_voice_states => #{
                <<"stale">> => #{
                    voice_state => Stale, disconnected_at => erlang:system_time(millisecond)
                }
            }
        },
        {reply, Reply, State3} =
            guild_voice_connection:confirm_voice_connection_from_livekit(
                #{connection_id => <<"stale">>}, State2
            ),
        ?assertEqual({error, permission_denied, voice_p2p_consent_required}, Reply),
        ?assertNot(maps:is_key(<<"stale">>, maps:get(voice_states, State3))),
        assert_untouched(Mesh, State2, State3)
    end).

livekit_activation_into_a_mesh_is_refused_test() ->
    with_stubs(grant, fun(State0) ->
        {Mesh, State1} = join_mesh([10, 11], State0),
        State2 = guild_voice_connection_pending:store_pending(
            <<"late">>,
            #{
                user_id => 12,
                guild_id => ?GUILD_ID,
                channel_id => ?CHANNEL_ID,
                session_id => session_id(12),
                token_nonce => <<"nonce">>,
                expires_at => erlang:system_time(millisecond) + 30000
            },
            State1
        ),
        {reply, Reply, State3} =
            guild_voice_connection:confirm_voice_connection_from_livekit(
                #{connection_id => <<"late">>, token_nonce => <<"nonce">>}, State2
            ),
        ?assertEqual({error, permission_denied, voice_p2p_consent_required}, Reply),
        ?assertEqual(lists:sort(Mesh), lists:sort(maps:keys(maps:get(voice_states, State3)))),
        ?assertEqual(#{}, maps:get(pending_voice_connections, State3)),
        assert_untouched(Mesh, State1, State3)
    end).

explicit_switch_converts_without_a_voice_server_test() ->
    with_stubs(grant, fun(State0) ->
        {[ConnectionA | _] = Mesh, State1} = join_mesh([10, 11], State0),
        Self = self(),
        Guild = spawn(fun() ->
            {reply, Reply, State2} = guild_voice_connection:voice_state_update(
                (request(10, false))#{connection_id => ConnectionA},
                maps:remove(guild_pid, State1)
            ),
            Self ! {converted, Reply, State2},
            guild_stub_loop(State2)
        end),
        receive
            {converted, Reply, State2} ->
                ?assertMatch(#{success := true}, Reply),
                assert_marked_sfu(Mesh, State2),
                ?assertEqual(lists:sort(Mesh), lists:sort(drain_voice_server_updates(2)))
        after 2000 ->
            error(explicit_switch_did_not_convert)
        end,
        Guild ! stop
    end).

moderator_move_into_a_mesh_is_refused_test() ->
    with_stubs(grant, fun(State0) ->
        {Mesh, State1} = join_mesh([10, 11], State0),
        State2 = with_sfu_voice_state(<<"moved">>, 12, ?OTHER_CHANNEL_ID, State1),
        {reply, Reply, State3} = guild_voice:move_member(
            move_request(12, ?CHANNEL_ID), State2
        ),
        ?assertEqual({error, permission_denied, voice_p2p_consent_required}, Reply),
        ?assert(maps:is_key(<<"moved">>, maps:get(voice_states, State3))),
        assert_untouched(Mesh, State2, State3)
    end).

moderator_move_into_an_empty_pinned_channel_moves_on_sfu_test() ->
    with_stubs(grant, fun(State0) ->
        State1 = with_sfu_voice_state(<<"moved">>, 12, ?OTHER_CHANNEL_ID, State0),
        {reply, Reply, _State2} = guild_voice:move_member(
            move_request(12, ?PINNED_CHANNEL_ID), State1
        ),
        ?assertMatch(#{success := true, needs_token := true}, Reply)
    end).

moderator_move_out_of_a_mesh_gets_an_sfu_grant_test() ->
    with_stubs(grant, fun(State0) ->
        {[ConnectionA, ConnectionB], State1} = join_mesh([10, 11], State0),
        {reply, #{success := true, needs_token := true, session_data := SessionData}, State2} =
            guild_voice:move_member(move_request(11, ?OTHER_CHANNEL_ID), State1),
        ?assertEqual([ConnectionA], maps:keys(maps:get(voice_states, State2))),
        ?assertEqual(true, p2p_flag(ConnectionA, State2)),
        GuildPid = maps:get(guild_pid, State2),
        GuildPid ! {serve, State2},
        ok = guild_voice_move_execute:send_single_voice_server_update(
            ?GUILD_ID, ?OTHER_CHANNEL_ID, hd(SessionData), GuildPid
        ),
        ?assertEqual([ConnectionB], drain_voice_server_updates(1)),
        ?assertNot(maps:is_key(<<"p2p">>, last_token_request()))
    end).

client_move_into_a_mesh_joins_it_test() ->
    with_stubs(grant, fun(State0) ->
        {[ConnectionA], State1} = join_mesh([10], State0),
        State2 = with_sfu_voice_state(<<"moved">>, 12, ?OTHER_CHANNEL_ID, State1),
        {reply, Reply, State3} = guild_voice_connection:voice_state_update(
            (request(12, true))#{connection_id => <<"moved">>}, State2
        ),
        ?assertMatch(#{success := true, p2p := true, connection_id := <<"moved">>}, Reply),
        ?assertEqual(true, p2p_flag(<<"moved">>, State3)),
        ?assertEqual(true, p2p_flag(ConnectionA, State3)),
        ?assertEqual(#{}, maps:get(pending_voice_connections, State3))
    end).

client_move_into_a_mesh_without_agreement_is_rejected_test() ->
    with_stubs(grant, fun(State0) ->
        {Mesh, State1} = join_mesh([10], State0),
        State2 = with_sfu_voice_state(<<"moved">>, 12, ?OTHER_CHANNEL_ID, State1),
        {reply, Reply, State3} = guild_voice_connection:voice_state_update(
            (request(12, undefined))#{connection_id => <<"moved">>}, State2
        ),
        ?assertEqual({error, permission_denied, voice_p2p_consent_required}, Reply),
        ?assertEqual(
            <<"200">>,
            maps:get(<<"channel_id">>, maps:get(<<"moved">>, maps:get(voice_states, State3)))
        ),
        assert_untouched(Mesh, State2, State3)
    end).

signal_relay_allowed_test() ->
    with_stubs(grant, fun(State0) ->
        {[ConnectionA, ConnectionB], State1} = join_mesh([10, 11], State0),
        Data = #{<<"type">> => <<"offer">>, <<"sdp">> => <<"v=0">>, <<"id">> => 7},
        {noreply, State1} = guild_voice_handler:handle_cast(
            {voice_signal, signal(<<"sess-10">>, ConnectionB, Data)}, State1
        ),
        ?assertEqual(
            #{
                <<"guild_id">> => <<"999">>,
                <<"channel_id">> => <<"100">>,
                <<"from">> => ConnectionA,
                <<"user_id">> => <<"10">>,
                <<"data">> => Data
            },
            receive_voice_signal()
        )
    end).

signal_relay_denied_test() ->
    with_stubs(grant, fun(State0) ->
        {[ConnectionA, ConnectionB], State1} = join_mesh([10, 11], State0),
        Data = #{<<"type">> => <<"candidate">>},
        Denied = [
            signal(<<"sess-12">>, ConnectionB, Data),
            signal(<<"sess-10">>, ConnectionA, Data),
            signal(<<"sess-10">>, <<"unknown">>, Data),
            (signal(<<"sess-10">>, ConnectionB, Data))#{channel_id => 200}
        ],
        [
            {noreply, State1} = guild_voice_handler:handle_cast({voice_signal, Request}, State1)
         || Request <- Denied
        ],
        State2 = guild_voice_region:switch_to_sfu(?CHANNEL_ID, State1),
        {noreply, State2} = guild_voice_handler:handle_cast(
            {voice_signal, signal(<<"sess-10">>, ConnectionB, Data)}, State2
        ),
        assert_no_voice_signal(),
        ?assertEqual(2, length(served_voice_server_updates(State2)))
    end).

signal(SessionId, To, Data) ->
    #{session_id => SessionId, channel_id => ?CHANNEL_ID, to => To, data => Data}.

with_sfu_voice_state(ConnectionId, UserId, ChannelId, State) ->
    VoiceState = voice_state_utils:complete_voice_state(#{
        <<"guild_id">> => <<"999">>,
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"user_id">> => integer_to_binary(UserId),
        <<"connection_id">> => ConnectionId,
        <<"session_id">> => session_id(UserId),
        <<"member">> => member(UserId)
    }),
    VoiceStates = maps:get(voice_states, State),
    State#{voice_states => VoiceStates#{ConnectionId => VoiceState}}.

move_request(UserId, ChannelId) ->
    #{
        user_id => UserId,
        moderator_id => 10,
        channel_id => ChannelId,
        mute => false,
        deaf => false
    }.

assert_converted(Mesh, State) ->
    assert_marked_sfu(Mesh, State),
    ?assertEqual(lists:sort(Mesh), lists:sort(served_voice_server_updates(State))).

assert_marked_sfu(Mesh, State) ->
    [?assertEqual(false, p2p_flag(ConnectionId, State)) || ConnectionId <- Mesh],
    Broadcasts = drain_voice_state_updates(),
    [?assert(lists:member({ConnectionId, false}, Broadcasts)) || ConnectionId <- Mesh].

assert_untouched(Mesh, Before, After) ->
    VoiceStates = maps:get(voice_states, Before),
    [
        ?assertEqual(
            maps:get(ConnectionId, VoiceStates),
            maps:get(ConnectionId, maps:get(voice_states, After))
        )
     || ConnectionId <- Mesh
    ],
    ?assertEqual(
        maps:get(pending_voice_connections, Before), maps:get(pending_voice_connections, After)
    ),
    Broadcasts = drain_voice_state_updates(),
    [?assertNot(lists:member({ConnectionId, false}, Broadcasts)) || ConnectionId <- Mesh],
    ?assertEqual([], served_voice_server_updates(After, 0)).

served_voice_server_updates(State) ->
    served_voice_server_updates(
        State,
        maps:size(
            voice_state_utils:channel_voice_states(?CHANNEL_ID, maps:get(voice_states, State))
        )
    ).

served_voice_server_updates(State, Expected) ->
    maps:get(guild_pid, State) ! {serve, State},
    drain_voice_server_updates(Expected).

drain_voice_server_updates(0) ->
    receive
        {'$gen_cast', {dispatch, voice_server_update, Payload}} ->
            error({unexpected_voice_server_update, Payload})
    after 100 ->
        []
    end;
drain_voice_server_updates(Remaining) ->
    receive
        {'$gen_cast', {dispatch, voice_server_update, Payload}} ->
            ?assertEqual(<<"tok">>, maps:get(<<"token">>, Payload)),
            ?assertEqual(<<"wss://voice.example">>, maps:get(<<"endpoint">>, Payload)),
            ?assertNot(maps:is_key(<<"p2p">>, Payload)),
            [maps:get(<<"connection_id">>, Payload) | drain_voice_server_updates(Remaining - 1)]
    after 300 ->
        []
    end.

drain_voice_state_updates() ->
    receive
        {'$gen_cast', {dispatch, voice_state_update, {pre_encoded, Encoded}}} ->
            #{<<"connection_id">> := ConnectionId, <<"p2p">> := P2p} = json:decode(Encoded),
            [{ConnectionId, P2p} | drain_voice_state_updates()]
    after 200 ->
        []
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

p2p_flag(ConnectionId, State) ->
    maps:get(<<"p2p">>, maps:get(ConnectionId, maps:get(voice_states, State))).

join_mesh(UserIds, State0) ->
    join_mesh(UserIds, ?CHANNEL_ID, State0).

join_mesh(UserIds, ChannelId, State0) ->
    lists:foldl(
        fun(UserId, {ConnectionIds, State}) ->
            {reply, #{p2p := true, connection_id := ConnectionId}, NewState} =
                guild_voice_connection:voice_state_update(
                    (request(UserId, true))#{channel_id => ChannelId}, State
                ),
            {ConnectionIds ++ [ConnectionId], NewState}
        end,
        {[], State0},
        UserIds
    ).

join(UserId, P2p, State) ->
    {reply, Reply, NewState} = guild_voice_connection:voice_state_update(
        request(UserId, P2p), State
    ),
    {Reply, NewState}.

request(UserId, P2p) ->
    #{
        user_id => UserId,
        channel_id => ?CHANNEL_ID,
        session_id => session_id(UserId),
        p2p => P2p,
        country_code => <<"SE">>
    }.

session_id(UserId) ->
    <<"sess-", (integer_to_binary(UserId))/binary>>.

last_token_request() ->
    lists:last(token_requests()).

token_request_count() ->
    length(token_requests()).

token_requests() ->
    [
        Request
     || {_Pid, {rpc_client, call, [#{<<"type">> := <<"voice_get_token">>} = Request]}, _Result} <-
            meck:history(rpc_client)
    ].

with_stubs(Mode, Fun) ->
    erlang:put(p2p_stub_mode, Mode),
    meck:new(rpc_client, [passthrough, no_link, non_strict]),
    meck:expect(rpc_client, call, fun rpc_reply/1),
    meck:new(guild_voice_server, [passthrough, no_link]),
    meck:expect(guild_voice_server, resolve, fun(_GuildId, GuildPid) -> GuildPid end),
    GuildPid = spawn(fun guild_stub/0),
    Test = self(),
    Sink = spawn(fun() -> sink(Test) end),
    try
        Fun(base_state(GuildPid, Sink))
    after
        GuildPid ! stop,
        meck:unload(guild_voice_server),
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

rpc_reply(#{<<"type">> := <<"voice_get_token">>} = Request) ->
    ConnectionId = maps:get(
        <<"connection_id">>,
        Request,
        <<"conn-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>
    ),
    case {maps:get(<<"p2p">>, Request, false), erlang:get(p2p_stub_mode)} of
        {true, grant} ->
            p2p_grant(ConnectionId);
        {true, {cap, Max}} ->
            case maps:get(<<"p2p_participant_count">>, Request) > Max of
                true -> {ok, #{<<"p2p">> => false, <<"declineReason">> => <<"channel_full">>}};
                false -> p2p_grant(ConnectionId)
            end;
        _ ->
            {ok, #{
                <<"token">> => <<"tok">>,
                <<"endpoint">> => <<"wss://voice.example">>,
                <<"connectionId">> => ConnectionId,
                <<"regionId">> => <<"us-east">>,
                <<"serverId">> => <<"voice-1">>
            }}
    end;
rpc_reply(_Request) ->
    {ok, #{}}.

p2p_grant(ConnectionId) ->
    {ok, #{
        <<"p2p">> => true,
        <<"connectionId">> => ConnectionId,
        <<"iceServers">> => [#{<<"urls">> => [<<"stun:stun.example:3478">>]}]
    }}.

guild_stub() ->
    receive
        {serve, State} -> guild_stub_loop(State);
        stop -> ok
    after 30000 ->
        ok
    end.

guild_stub_loop(State) ->
    receive
        {serve, NewState} ->
            guild_stub_loop(NewState);
        {'$gen_call', From, {get_voice_guild_state}} ->
            gen_server:reply(From, State),
            guild_stub_loop(State);
        {'$gen_call', From, _Request} ->
            gen_server:reply(From, ok),
            guild_stub_loop(State);
        stop ->
            ok;
        _Other ->
            guild_stub_loop(State)
    after 30000 ->
        ok
    end.

base_state(GuildPid, Sink) ->
    UserIds = [10, 11, 12, 13, 14],
    #{
        id => ?GUILD_ID,
        guild_pid => GuildPid,
        data => #{
            <<"id">> => <<"999">>,
            <<"guild">> => #{<<"owner_id">> => <<"10">>},
            <<"channels">> => [
                #{<<"id">> => <<"100">>, <<"type">> => 2, <<"user_limit">> => 0},
                #{<<"id">> => <<"200">>, <<"type">> => 2, <<"user_limit">> => 0},
                #{
                    <<"id">> => <<"300">>,
                    <<"type">> => 2,
                    <<"user_limit">> => 0,
                    <<"rtc_p2p">> => true
                }
            ],
            <<"members">> => [member(UserId) || UserId <- UserIds]
        },
        sessions => maps:from_list([
            {session_id(UserId), #{user_id => UserId, pid => Sink, pending_connect => false}}
         || UserId <- UserIds
        ]),
        voice_states => #{},
        pending_voice_connections => #{},
        test_perm_fun => fun(_UserId) ->
            constants:view_channel_permission() bor constants:connect_permission()
        end
    }.

member(UserId) ->
    #{
        <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
        <<"mute">> => false,
        <<"deaf">> => false
    }.
