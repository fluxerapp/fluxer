%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_broadcast_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

base_guild_state() ->
    #{
        id => 999,
        data => #{<<"channels">> => [#{<<"id">> => <<"100">>, <<"type">> => 2}]},
        sessions => #{
            <<"s1">> => #{
                pid => self(),
                user_id => 11,
                viewable_channels => #{100 => true}
            }
        },
        voice_states => #{},
        pending_voice_connections => #{}
    }.

voice_server_state(GuildState) ->
    GuildState#{guild_pid => self()}.

join_voice_state(ConnId) ->
    #{
        <<"user_id">> => <<"10">>,
        <<"guild_id">> => <<"999">>,
        <<"channel_id">> => <<"100">>,
        <<"connection_id">> => ConnId,
        <<"self_mute">> => false,
        <<"self_deaf">> => false
    }.

broadcast_payload(VoiceState) ->
    maps:merge(
        #{
            <<"session_id">> => null,
            <<"member">> => null,
            <<"mute">> => false,
            <<"deaf">> => false,
            <<"self_video">> => false,
            <<"self_stream">> => false,
            <<"is_mobile">> => false,
            <<"suppress">> => false,
            <<"viewer_stream_keys">> => [],
            <<"e2ee_capable">> => false,
            <<"version">> => 0
        },
        VoiceState
    ).

pending_join(ConnId) ->
    #{
        user_id => 10,
        guild_id => 999,
        channel_id => 100,
        voice_state => join_voice_state(ConnId)
    }.

receive_relay() ->
    receive
        {'$gen_cast', {relay_voice_state_update, _, _} = RelayMsg} -> RelayMsg
    after 1000 ->
        error(relay_voice_state_update_not_received)
    end.

pump_relay_to_guild(GuildState) ->
    RelayMsg = receive_relay(),
    {noreply, NewGuildState} = guild_voice_handler:handle_cast(RelayMsg, GuildState),
    NewGuildState.

collect_voice_state_dispatches() ->
    [decoded_voice_state(Payload) || Payload <- collect_raw_voice_state_dispatches()].

collect_raw_voice_state_dispatches() ->
    receive
        {'$gen_cast', {dispatch, voice_state_update, Payload}} ->
            [Payload | collect_raw_voice_state_dispatches()]
    after 0 ->
        []
    end.

decoded_voice_state({pre_encoded, Bin}) -> json:decode(Bin);
decoded_voice_state(Payload) -> Payload.

map_path_wire_bytes(VoiceState) ->
    iolist_to_binary(
        json:encode(guild_data_wire:payload(VoiceState), fun json:encode_value/2)
    ).

confirm_join(ConnId, GuildState) ->
    VoiceServerState = (voice_server_state(GuildState))#{
        pending_voice_connections => #{ConnId => pending_join(ConnId)}
    },
    {reply, #{success := true}, _} =
        guild_voice_connection:confirm_voice_connection_from_livekit(
            #{connection_id => ConnId}, VoiceServerState
        ),
    pump_relay_to_guild(GuildState).

single_join_dispatches_once_per_recipient_test() ->
    GuildState = confirm_join(<<"conn1">>, base_guild_state()),
    ?assertEqual(
        [broadcast_payload(join_voice_state(<<"conn1">>))], collect_voice_state_dispatches()
    ),
    ?assertEqual(
        join_voice_state(<<"conn1">>),
        maps:get(<<"conn1">>, maps:get(voice_states, GuildState))
    ).

relay_syncs_guild_cache_without_rebroadcast_test() ->
    VS = join_voice_state(<<"conn1">>),
    {noreply, NewGuildState} = guild_voice_handler:handle_cast(
        {relay_voice_state_update, VS, null}, base_guild_state()
    ),
    ?assertEqual(VS, maps:get(<<"conn1">>, maps:get(voice_states, NewGuildState))),
    ?assertEqual([], collect_voice_state_dispatches()).

mute_change_still_dispatches_test() ->
    GuildState0 = confirm_join(<<"conn1">>, base_guild_state()),
    ?assertEqual(
        [broadcast_payload(join_voice_state(<<"conn1">>))], collect_voice_state_dispatches()
    ),
    MutedVS = (join_voice_state(<<"conn1">>))#{
        <<"self_mute">> => true, <<"self_deaf">> => true
    },
    ok = guild_voice_broadcast:broadcast_voice_state_update(
        MutedVS, voice_server_state(GuildState0), <<"100">>
    ),
    GuildState1 = pump_relay_to_guild(GuildState0),
    ?assertEqual([broadcast_payload(MutedVS)], collect_voice_state_dispatches()),
    ?assertEqual(MutedVS, maps:get(<<"conn1">>, maps:get(voice_states, GuildState1))).

rejoin_after_leave_still_dispatches_test() ->
    GuildState0 = confirm_join(<<"conn1">>, base_guild_state()),
    ?assertEqual(
        [broadcast_payload(join_voice_state(<<"conn1">>))], collect_voice_state_dispatches()
    ),
    LeaveVS = (join_voice_state(<<"conn1">>))#{<<"channel_id">> => null},
    ok = guild_voice_broadcast:broadcast_voice_state_update(
        LeaveVS, voice_server_state(GuildState0), <<"100">>
    ),
    GuildState1 = pump_relay_to_guild(GuildState0),
    ?assertEqual([broadcast_payload(LeaveVS)], collect_voice_state_dispatches()),
    ?assertNot(maps:is_key(<<"conn1">>, maps:get(voice_states, GuildState1))),
    GuildState2 = confirm_join(<<"conn2">>, GuildState1),
    ?assertEqual(
        [broadcast_payload(join_voice_state(<<"conn2">>))], collect_voice_state_dispatches()
    ),
    ?assertEqual(
        join_voice_state(<<"conn2">>),
        maps:get(<<"conn2">>, maps:get(voice_states, GuildState2))
    ).

broadcast_sanitizes_routing_metadata_test() ->
    VS = (join_voice_state(<<"conn1">>))#{
        <<"latitude">> => <<"1.0">>,
        <<"longitude">> => <<"2.0">>,
        <<"region_id">> => <<"us-east">>,
        <<"server_id">> => <<"voice-1">>
    },
    GuildState = base_guild_state(),
    ok = guild_voice_broadcast:broadcast_voice_state_update(
        VS, voice_server_state(GuildState), null
    ),
    _ = pump_relay_to_guild(GuildState),
    ?assertEqual(
        [broadcast_payload(join_voice_state(<<"conn1">>))], collect_voice_state_dispatches()
    ).

guild_fanout_pre_encodes_identical_wire_bytes_test() ->
    VS = join_voice_state(<<"conn1">>),
    ?assertEqual(
        {pre_encoded, map_path_wire_bytes(broadcast_payload(VS))}, dispatch_payload_for(VS)
    ).

guild_fanout_payload_never_touches_dm_voice_states_test() ->
    {pre_encoded, Bin} = dispatch_payload_for(join_voice_state(<<"conn1">>)),
    State = #{dm_voice_states => #{}},
    EventData = #{} = json:decode(Bin),
    ?assertEqual(
        State,
        session_dispatch_guild:update_dm_voice_states_map(
            voice_state_update, EventData, State
        )
    ).

dm_scoped_voice_state_stays_a_map_and_updates_dm_voice_states_test() ->
    DmVS = (join_voice_state(<<"conn1">>))#{<<"guild_id">> => null},
    Payload = dispatch_payload_for(DmVS),
    ?assertEqual(broadcast_payload(DmVS), Payload),
    State = session_dispatch_guild:update_dm_voice_states_map(
        voice_state_update, Payload, #{dm_voice_states => #{}}
    ),
    #{dm_voice_states := #{<<"conn1">> := StoredVoiceState}} = State,
    ?assertEqual(
        broadcast_payload(DmVS),
        StoredVoiceState
    ).

dispatch_payload_for(VoiceState) ->
    GuildState = base_guild_state(),
    ok = guild_voice_broadcast:broadcast_voice_state_update(
        VoiceState, voice_server_state(GuildState), null
    ),
    _ = pump_relay_to_guild(GuildState),
    [Payload] = collect_raw_voice_state_dispatches(),
    Payload.
