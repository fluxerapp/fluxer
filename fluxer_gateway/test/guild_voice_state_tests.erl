%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_state_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

user_matches_voice_state_integer_test() ->
    VoiceState = #{<<"user_id">> => <<"10">>},
    ?assert(guild_voice_state:user_matches_voice_state(VoiceState, 10)),
    ?assertNot(guild_voice_state:user_matches_voice_state(VoiceState, 11)).

user_matches_voice_state_binary_test() ->
    VoiceState = #{<<"user_id">> => <<"123">>},
    ?assert(guild_voice_state:user_matches_voice_state(VoiceState, <<"123">>)),
    ?assertNot(guild_voice_state:user_matches_voice_state(VoiceState, <<"456">>)).

user_matches_voice_state_undefined_test() ->
    VoiceState = #{},
    ?assertNot(guild_voice_state:user_matches_voice_state(VoiceState, 10)).

extract_session_info_from_voice_state_test() ->
    VoiceState = #{
        <<"session_id">> => <<"sess">>,
        <<"self_mute">> => true,
        <<"self_deaf">> => false,
        <<"self_video">> => true,
        <<"self_stream">> => false,
        <<"is_mobile">> => true,
        <<"suppress">> => true,
        <<"e2ee_capable">> => true,
        <<"latitude">> => <<"1.0">>,
        <<"longitude">> => <<"2.0">>,
        <<"member">> => #{<<"id">> => <<"m">>}
    },
    Info = guild_voice_state:extract_session_info_from_voice_state(<<"conn">>, VoiceState),
    ?assertEqual(<<"conn">>, maps:get(connection_id, Info)),
    ?assertEqual(<<"sess">>, maps:get(session_id, Info)),
    ?assertEqual(true, maps:get(self_mute, Info)),
    ?assertEqual(true, maps:get(suppress, Info)),
    ?assertEqual(true, maps:get(e2ee_capable, Info)),
    ?assertEqual(<<"1.0">>, maps:get(latitude, Info)),
    ?assertEqual(<<"2.0">>, maps:get(longitude, Info)),
    ?assertEqual(#{<<"id">> => <<"m">>}, maps:get(member, Info)).

update_voice_state_data_no_change_reply_is_external_test() ->
    ExistingVS = #{
        <<"guild_id">> => <<"1">>,
        <<"channel_id">> => <<"2">>,
        <<"user_id">> => <<"3">>,
        <<"connection_id">> => <<"conn">>,
        <<"latitude">> => <<"1.0">>,
        <<"longitude">> => <<"2.0">>,
        <<"version">> => 4
    },
    Flags = #{
        self_mute => false,
        self_deaf => false,
        self_video => false,
        self_stream => false,
        is_mobile => false,
        suppress => false
    },
    {reply, Reply, _} = guild_voice_state:update_voice_state_data(#{
        connection_id => <<"conn">>,
        channel_id => <<"2">>,
        flags => Flags,
        member => #{},
        existing_voice_state => ExistingVS,
        voice_states => #{<<"conn">> => ExistingVS},
        state => #{voice_states => #{<<"conn">> => ExistingVS}},
        needs_token => false,
        viewer_stream_keys => []
    }),
    ?assertEqual(true, maps:get(success, Reply)),
    ReplyVS = maps:get(voice_state, Reply),
    ?assertNot(maps:is_key(<<"latitude">>, ReplyVS)),
    ?assertNot(maps:is_key(<<"longitude">>, ReplyVS)),
    ?assertEqual(4, maps:get(<<"version">>, ReplyVS)),
    ?assertEqual(null, maps:get(<<"member">>, ReplyVS)),
    ?assertEqual(false, maps:get(<<"suppress">>, ReplyVS)).

update_voice_state_data_change_bumps_version_and_refreshes_member_test() ->
    Member = #{
        <<"user">> => #{<<"id">> => <<"3">>},
        <<"nick">> => <<"fresh-nick">>,
        <<"mute">> => false,
        <<"deaf">> => false
    },
    ExistingVS = #{
        <<"guild_id">> => <<"1">>,
        <<"channel_id">> => <<"2">>,
        <<"user_id">> => <<"3">>,
        <<"connection_id">> => <<"conn">>,
        <<"self_mute">> => false,
        <<"member">> => #{<<"nick">> => <<"stale-nick">>},
        <<"latitude">> => <<"1.0">>,
        <<"longitude">> => <<"2.0">>,
        <<"version">> => 4
    },
    Flags = #{
        self_mute => true,
        self_deaf => false,
        self_video => false,
        self_stream => false,
        is_mobile => false,
        suppress => false
    },
    State = #{voice_states => #{<<"conn">> => ExistingVS}, sessions => #{}},
    {reply, Reply, NewState} = guild_voice_state:update_voice_state_data(#{
        connection_id => <<"conn">>,
        channel_id => <<"2">>,
        flags => Flags,
        member => Member,
        existing_voice_state => ExistingVS,
        voice_states => #{<<"conn">> => ExistingVS},
        state => State,
        needs_token => false,
        viewer_stream_keys => []
    }),
    ?assertEqual(true, maps:get(success, Reply)),
    StoredVS = maps:get(<<"conn">>, maps:get(voice_states, NewState)),
    ?assertEqual(5, maps:get(<<"version">>, StoredVS)),
    ?assertEqual(true, maps:get(<<"self_mute">>, StoredVS)),
    ?assertEqual(Member, maps:get(<<"member">>, StoredVS)),
    ?assertEqual(<<"1.0">>, maps:get(<<"latitude">>, StoredVS)),
    ReplyVS = maps:get(voice_state, Reply),
    ?assertNot(maps:is_key(<<"latitude">>, ReplyVS)),
    ?assertEqual(Member, maps:get(<<"member">>, ReplyVS)),
    ?assertEqual(5, maps:get(<<"version">>, ReplyVS)).

has_voice_state_change_no_change_test() ->
    ExistingVoiceState = existing_voice_state(#{<<"self_mute">> => true}),
    ?assertNot(default_self_mute_change(ExistingVoiceState)).

has_voice_state_change_channel_change_test() ->
    ExistingVoiceState = existing_voice_state(#{}),
    ?assert(
        guild_voice_state:has_voice_state_change(
            ExistingVoiceState,
            change_fields(#{channel_id => <<"200">>})
        )
    ).

has_voice_state_change_self_mute_change_test() ->
    ExistingVoiceState = existing_voice_state(#{}),
    ?assert(default_self_mute_change(ExistingVoiceState)).

has_voice_state_change_server_mute_change_test() ->
    ExistingVoiceState = existing_voice_state(#{}),
    ?assert(
        guild_voice_state:has_voice_state_change(
            ExistingVoiceState, change_fields(#{server_mute => true})
        )
    ).

has_voice_state_change_suppress_change_test() ->
    ExistingVoiceState = existing_voice_state(#{}),
    ?assert(
        guild_voice_state:has_voice_state_change(
            ExistingVoiceState, change_fields(#{suppress => true})
        )
    ).

has_voice_state_change_viewer_stream_keys_change_test() ->
    ExistingVoiceState = existing_voice_state(#{}),
    ?assert(
        guild_voice_state:has_voice_state_change(
            ExistingVoiceState,
            change_fields(#{viewer_stream_keys => [<<"999:100:conn">>]})
        )
    ).

has_voice_state_change_defaults_test() ->
    ExistingVoiceState = #{},
    ?assertNot(
        guild_voice_state:has_voice_state_change(
            ExistingVoiceState, change_fields(#{channel_id => null})
        )
    ).

existing_voice_state(Overrides) ->
    maps:merge(
        #{
            <<"channel_id">> => <<"100">>,
            <<"mute">> => false,
            <<"deaf">> => false,
            <<"self_mute">> => false,
            <<"self_deaf">> => false,
            <<"self_video">> => false,
            <<"self_stream">> => false,
            <<"is_mobile">> => false,
            <<"suppress">> => false,
            <<"viewer_stream_keys">> => []
        },
        Overrides
    ).

default_self_mute_change(ExistingVoiceState) ->
    guild_voice_state:has_voice_state_change(
        ExistingVoiceState, change_fields(#{self_mute => true})
    ).

change_fields(Overrides) ->
    maps:merge(
        #{
            channel_id => <<"100">>,
            server_mute => false,
            server_deaf => false,
            self_mute => false,
            self_deaf => false,
            self_video => false,
            self_stream => false,
            is_mobile => false,
            suppress => false,
            viewer_stream_keys => []
        },
        Overrides
    ).
