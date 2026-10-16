%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(voice_p2p_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

peer(ConnectionId, SessionId, P2p) ->
    #{
        <<"connection_id">> => ConnectionId,
        <<"session_id">> => SessionId,
        <<"user_id">> => <<"10">>,
        <<"p2p">> => P2p
    }.

mesh(Count) ->
    [
        peer(integer_to_binary(Index), <<"sess-", (integer_to_binary(Index))/binary>>, true)
     || Index <- lists:seq(1, Count)
    ].

agreed_requires_an_explicit_true_from_a_user_test() ->
    ?assert(voice_p2p:agreed(true, false)),
    ?assertNot(voice_p2p:agreed(true, true)),
    ?assertNot(voice_p2p:agreed(false, false)),
    ?assertNot(voice_p2p:agreed(undefined, false)),
    ?assertNot(voice_p2p:agreed(<<"true">>, false)).

declined_requires_an_explicit_false_test() ->
    ?assert(voice_p2p:declined(false)),
    ?assertNot(voice_p2p:declined(true)),
    ?assertNot(voice_p2p:declined(undefined)),
    ?assertNot(voice_p2p:declined(null)).

channel_mode_test() ->
    ?assertEqual(empty, voice_p2p:channel_mode([])),
    ?assertEqual(p2p, voice_p2p:channel_mode(mesh(2))),
    ?assertEqual(sfu, voice_p2p:channel_mode([peer(<<"a">>, <<"s">>, false), #{}])),
    ?assertEqual(mixed, voice_p2p:channel_mode([peer(<<"a">>, <<"s">>, false) | mesh(1)])).

pinned_reads_the_channel_flag_test() ->
    ?assert(voice_p2p:pinned(#{<<"rtc_p2p">> => true})),
    ?assertNot(voice_p2p:pinned(#{<<"rtc_p2p">> => false})),
    ?assertNot(voice_p2p:pinned(#{})),
    ?assertNot(voice_p2p:pinned(undefined)).

join_decision_for_a_live_mesh_test() ->
    ?assertEqual({p2p, joiner, 2}, voice_p2p:join_decision(true, mesh(1), false, false)),
    ?assertEqual({p2p, joiner, 4}, voice_p2p:join_decision(true, mesh(3), false, false)),
    ?assertEqual(
        {reject, voice_p2p_consent_required},
        voice_p2p:join_decision(false, mesh(3), false, false)
    ),
    ?assertEqual(
        {reject, voice_p2p_consent_required},
        voice_p2p:join_decision(false, mesh(4), false, false)
    ),
    ?assertEqual(
        {reject, voice_channel_full}, voice_p2p:join_decision(true, mesh(4), false, false)
    ).

join_decision_for_an_empty_channel_test() ->
    ?assertEqual({p2p, initiator, 1}, voice_p2p:join_decision(true, [], false, false)),
    ?assertEqual({p2p, initiator, 1}, voice_p2p:join_decision(true, [], false, true)),
    ?assertEqual(sfu, voice_p2p:join_decision(false, [], false, false)),
    ?assertEqual(sfu, voice_p2p:join_decision(false, [], false, true)),
    ?assertEqual(sfu, voice_p2p:join_decision(true, [], true, false)),
    ?assertEqual(sfu, voice_p2p:join_decision(false, [], true, true)).

join_decision_for_an_sfu_call_ignores_the_flag_test() ->
    Sfu = [peer(<<"a">>, <<"s">>, false)],
    ?assertEqual(sfu, voice_p2p:join_decision(true, Sfu, false, false)),
    ?assertEqual(sfu, voice_p2p:join_decision(false, Sfu, false, true)).

admit_test() ->
    Grant = {p2p, <<"conn">>, [], 2},
    Token = #{token => <<"tok">>},
    ?assertEqual(Grant, voice_p2p:admit({p2p, initiator, 1}, Grant)),
    ?assertEqual(Grant, voice_p2p:admit({p2p, joiner, 3}, Grant)),
    ?assertEqual(sfu, voice_p2p:admit({p2p, initiator, 1}, Token)),
    ?assertEqual({reject, voice_p2p_unavailable}, voice_p2p:admit({p2p, joiner, 3}, Token)),
    ?assertEqual(sfu, voice_p2p:admit(sfu, Token)),
    ?assertEqual({reject, voice_token_failed}, voice_p2p:admit(sfu, Grant)).

admit_maps_a_size_decline_to_channel_full_test() ->
    Declined = {declined, voice_channel_full},
    ?assertEqual({reject, voice_channel_full}, voice_p2p:admit({p2p, joiner, 3}, Declined)),
    ?assertEqual({reject, voice_channel_full}, voice_p2p:admit({p2p, initiator, 1}, Declined)),
    ?assertEqual({reject, voice_token_failed}, voice_p2p:admit(sfu, Declined)).

invariant_violation_test() ->
    ?assertEqual(ok, voice_p2p:invariant_violation([])),
    ?assertEqual(ok, voice_p2p:invariant_violation(mesh(4))),
    ?assertEqual({error, voice_channel_full}, voice_p2p:invariant_violation(mesh(5))),
    ?assertEqual(
        {error, voice_p2p_consent_required},
        voice_p2p:invariant_violation([peer(<<"a">>, <<"s">>, false) | mesh(1)])
    ),
    ?assertEqual(ok, voice_p2p:invariant_violation([peer(<<"a">>, <<"s">>, false)])).

invariant_violation_holds_a_mesh_to_the_configured_cap_test() ->
    ?assertEqual(ok, voice_p2p:invariant_violation(mesh(2), 2)),
    ?assertEqual({error, voice_channel_full}, voice_p2p:invariant_violation(mesh(3), 2)),
    ?assertEqual(ok, voice_p2p:invariant_violation(mesh(4), undefined)),
    ?assertEqual({error, voice_channel_full}, voice_p2p:invariant_violation(mesh(5), 9)),
    ?assertEqual(ok, voice_p2p:invariant_violation([peer(<<"a">>, <<"s">>, false)], 1)).

reports_rejection_test() ->
    ?assert(voice_p2p:reports_rejection(voice_p2p_consent_required)),
    ?assert(voice_p2p:reports_rejection(voice_p2p_unavailable)),
    ?assert(voice_p2p:reports_rejection(voice_channel_full)),
    ?assertNot(voice_p2p:reports_rejection(voice_token_failed)).

to_sfu_clears_the_flag_and_bumps_the_version_test() ->
    VoiceState = (peer(<<"a">>, <<"s">>, true))#{<<"version">> => 3},
    Converted = voice_p2p:to_sfu(VoiceState),
    ?assertEqual(false, maps:get(<<"p2p">>, Converted)),
    ?assertEqual(4, maps:get(<<"version">>, Converted)).

add_to_token_request_test() ->
    Request = #{<<"type">> => <<"voice_get_token">>},
    ?assertEqual(Request, voice_p2p:add_to_token_request(Request, sfu, <<"SE">>)),
    ?assertEqual(
        Request#{
            <<"p2p">> => true,
            <<"p2p_initiator">> => true,
            <<"p2p_participant_count">> => 1,
            <<"country_code">> => <<"SE">>
        },
        voice_p2p:add_to_token_request(Request, {p2p, initiator, 1}, <<"SE">>)
    ),
    ?assertEqual(
        Request#{
            <<"p2p">> => true, <<"p2p_initiator">> => false, <<"p2p_participant_count">> => 3
        },
        voice_p2p:add_to_token_request(Request, {p2p, joiner, 3}, undefined)
    ).

grant_reads_a_p2p_grant_test() ->
    Stun = #{<<"urls">> => [<<"stun:stun.example:3478">>], <<"ignored">> => true},
    Data = #{<<"p2p">> => true, <<"connectionId">> => <<"conn-1">>, <<"iceServers">> => [Stun]},
    ?assertEqual(
        {p2p, <<"conn-1">>, [#{<<"urls">> => [<<"stun:stun.example:3478">>]}], 4},
        voice_p2p:grant(Data)
    ).

grant_reads_the_configured_cap_test() ->
    Data = #{<<"p2p">> => true, <<"connectionId">> => <<"conn-1">>, <<"iceServers">> => []},
    ?assertEqual(
        {p2p, <<"conn-1">>, [], 3}, voice_p2p:grant(Data#{<<"maxParticipants">> => 3})
    ),
    ?assertEqual(
        {p2p, <<"conn-1">>, [], 4}, voice_p2p:grant(Data#{<<"maxParticipants">> => 9})
    ),
    ?assertEqual(
        {p2p, <<"conn-1">>, [], 4}, voice_p2p:grant(Data#{<<"maxParticipants">> => 0})
    ).

grant_keeps_only_urls_of_each_ice_server_test() ->
    Server = #{
        <<"urls">> => [<<"stun:stun.example:3478">>],
        <<"username">> => <<"user">>,
        <<"credential">> => <<"secret">>
    },
    Data = #{
        <<"p2p">> => true, <<"connectionId">> => <<"conn-1">>, <<"iceServers">> => [Server]
    },
    ?assertEqual(
        {p2p, <<"conn-1">>, [#{<<"urls">> => [<<"stun:stun.example:3478">>]}], 4},
        voice_p2p:grant(Data)
    ).

grant_reads_a_size_decline_test() ->
    ?assertEqual(
        {declined, voice_channel_full},
        voice_p2p:grant(#{<<"p2p">> => false, <<"declineReason">> => <<"channel_full">>})
    ),
    ?assertEqual(
        sfu, voice_p2p:grant(#{<<"p2p">> => false, <<"declineReason">> => <<"unknown">>})
    ).

grant_reads_a_token_response_as_sfu_test() ->
    ?assertEqual(
        sfu,
        voice_p2p:grant(#{
            <<"token">> => <<"tok">>,
            <<"endpoint">> => <<"wss://voice.example">>,
            <<"connectionId">> => <<"conn-1">>
        })
    ).

voice_server_update_shape_test() ->
    IceServers = [#{<<"urls">> => [<<"stun:stun.example:3478">>]}],
    ?assertEqual(
        #{
            <<"p2p">> => true,
            <<"ice_servers">> => IceServers,
            <<"connection_id">> => <<"conn-1">>,
            <<"channel_id">> => <<"100">>,
            <<"guild_id">> => <<"999">>
        },
        voice_p2p:voice_server_update(<<"conn-1">>, 100, 999, IceServers)
    ),
    ?assertNot(
        maps:is_key(
            <<"guild_id">>, voice_p2p:voice_server_update(<<"conn-1">>, 100, null, IceServers)
        )
    ).

signal_route_allows_a_mesh_peer_test() ->
    [Sender, Target] = mesh(2),
    ?assertEqual({ok, Sender, Target}, voice_p2p:signal_route(mesh(2), <<"sess-1">>, <<"2">>)).

signal_route_denies_a_sender_outside_the_mesh_test() ->
    ?assertEqual(error, voice_p2p:signal_route(mesh(2), <<"sess-9">>, <<"2">>)).

signal_route_denies_an_unknown_target_test() ->
    ?assertEqual(error, voice_p2p:signal_route(mesh(2), <<"sess-1">>, <<"9">>)).

signal_route_denies_the_senders_own_connection_test() ->
    ?assertEqual(error, voice_p2p:signal_route(mesh(2), <<"sess-1">>, <<"1">>)).

signal_route_denies_sfu_voice_states_test() ->
    Sfu = [peer(<<"1">>, <<"sess-1">>, false), peer(<<"2">>, <<"sess-2">>, false)],
    ?assertEqual(error, voice_p2p:signal_route(Sfu, <<"sess-1">>, <<"2">>)),
    Mixed = [peer(<<"1">>, <<"sess-1">>, true), peer(<<"2">>, <<"sess-2">>, false)],
    ?assertEqual(error, voice_p2p:signal_route(Mixed, <<"sess-1">>, <<"2">>)).

signal_payload_shape_test() ->
    [Sender | _] = mesh(1),
    Data = #{<<"type">> => <<"offer">>, <<"sdp">> => <<"v=0">>},
    ?assertEqual(
        #{
            <<"guild_id">> => <<"999">>,
            <<"channel_id">> => <<"100">>,
            <<"from">> => <<"1">>,
            <<"user_id">> => <<"10">>,
            <<"data">> => Data
        },
        voice_p2p:signal_payload(999, 100, Sender, Data)
    ),
    ?assertNot(maps:is_key(<<"guild_id">>, voice_p2p:signal_payload(null, 100, Sender, Data))).
