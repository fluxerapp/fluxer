%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_connection_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

required_voice_perms() ->
    constants:view_channel_permission() bor constants:connect_permission().

base_test_member(UserId) ->
    #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}}.

base_test_channel(ChannelId) ->
    #{<<"id">> => integer_to_binary(ChannelId), <<"type">> => 2, <<"user_limit">> => 0}.

base_test_state() ->
    #{
        id => 999,
        data => #{
            <<"channels">> => [base_test_channel(100)],
            <<"members">> => [base_test_member(10)]
        },
        voice_states => #{},
        test_perm_fun => fun(_) -> required_voice_perms() end
    }.

replace_channels(State, Channels) ->
    Data0 = maps:get(data, State, #{}),
    maps:put(data, maps:put(<<"channels">>, Channels, Data0), State).

replace_guild_id(State, GuildIdValue) ->
    Data0 = maps:get(data, State, #{}),
    maps:put(data, maps:put(<<"id">>, GuildIdValue, Data0), State).

replace_guild_meta_id(State, GuildIdValue) ->
    Data0 = maps:get(data, State, #{}),
    Guild0 = maps:get(<<"guild">>, Data0, #{}),
    Guild1 = maps:put(<<"id">>, GuildIdValue, Guild0),
    maps:put(data, maps:put(<<"guild">>, Guild1, Data0), State).

voice_state_update_invalid_user_id_test() ->
    {reply, {error, validation_error, voice_invalid_user_id}, _} =
        guild_voice_connection:voice_state_update(#{channel_id => null}, #{}).

voice_state_update_member_not_found_test() ->
    State = base_test_state(),
    {reply, {error, not_found, voice_member_not_found}, _} =
        guild_voice_connection:voice_state_update(#{user_id => 99, channel_id => null}, State).

voice_state_update_invalid_channel_id_test() ->
    State = base_test_state(),
    {reply, {error, validation_error, voice_invalid_channel_id}, _} =
        guild_voice_connection:voice_state_update(
            #{user_id => 10, channel_id => undefined}, State
        ).

voice_state_update_guild_leave_null_connection_id_test() ->
    State = connected_state(<<"10">>),
    {reply, {error, validation_error, voice_missing_connection_id}, NewState} =
        guild_voice_connection:voice_state_update(
            #{user_id => 10, channel_id => null, connection_id => null}, State
        ),
    ?assertEqual(connected_voice_states(<<"10">>), maps:get(voice_states, NewState)).

voice_state_update_guild_leave_omitted_connection_id_test() ->
    State = connected_state(<<"10">>),
    {reply, {error, validation_error, voice_missing_connection_id}, NewState} =
        guild_voice_connection:voice_state_update(
            #{user_id => 10, channel_id => null}, State
        ),
    ?assertEqual(connected_voice_states(<<"10">>), maps:get(voice_states, NewState)).

voice_state_update_guild_leave_blank_connection_id_test() ->
    State = connected_state(<<"10">>),
    assert_voice_state_update_error(
        #{user_id => 10, channel_id => null, connection_id => <<>>},
        State,
        {error, validation_error, voice_missing_connection_id}
    ).

voice_state_update_channel_not_found_test() ->
    State = replace_channels(base_test_state(), []),
    {reply, {error, not_found, voice_channel_not_found}, _} =
        guild_voice_connection:voice_state_update(#{user_id => 10, channel_id => 999}, State).

voice_state_update_connection_not_found_test() ->
    State = base_test_state(),
    Request = #{user_id => 10, channel_id => 100, connection_id => <<"missing-conn">>},
    assert_voice_state_update_error(
        Request, State, {error, not_found, voice_connection_not_found}
    ).

voice_state_update_connection_not_found_returns_rejected_ack_test() ->
    State = base_test_state(),
    Request = #{
        user_id => 10,
        channel_id => 100,
        connection_id => <<"missing-conn">>,
        mutation_id => <<"m-missing-connection">>,
        runtime_epoch => <<"epoch-1">>,
        base_version => 0
    },
    Ack = rejected_voice_state_ack(Request, State),
    ?assertEqual(<<"rejected">>, maps:get(<<"status">>, Ack)),
    ?assertEqual(0, maps:get(<<"server_version">>, Ack)),
    ?assertEqual(#{}, maps:get(<<"canonical_state">>, Ack)),
    ?assertEqual(<<"VOICE_CONNECTION_NOT_FOUND">>, maps:get(<<"error_code">>, Ack)).

voice_state_update_invalid_viewer_stream_keys_test() ->
    State = connected_state(<<"10">>),
    Request = #{
        user_id => 10,
        channel_id => 100,
        connection_id => <<"conn-1">>,
        viewer_stream_keys => 123
    },
    assert_voice_state_update_error(
        Request, State, {error, validation_error, voice_invalid_state}
    ).

voice_state_update_viewer_stream_keys_missing_connection_test() ->
    State = connected_state(<<"10">>),
    Request = #{
        user_id => 10,
        channel_id => 100,
        connection_id => <<"conn-1">>,
        viewer_stream_keys => [<<"999:100:missing-conn">>]
    },
    assert_voice_state_update_error(
        Request, State, {error, not_found, voice_connection_not_found}
    ).

voice_state_update_rejects_dm_scope_viewer_stream_key_test() ->
    VoiceStates = #{
        <<"conn-1">> => connected_voice_state(<<"conn-1">>, <<"10">>, 100, []),
        <<"conn-stream">> => connected_voice_state(<<"conn-stream">>, <<"20">>, 100, [])
    },
    State = maps:put(voice_states, VoiceStates, base_test_state()),
    Request = #{
        user_id => 10,
        channel_id => 100,
        connection_id => <<"conn-1">>,
        viewer_stream_keys => [<<"dm:100:conn-stream">>]
    },
    assert_voice_state_update_error(
        Request, State, {error, validation_error, voice_invalid_state}
    ).

voice_state_update_viewer_stream_keys_missing_connection_returns_rejected_ack_test() ->
    VoiceStates = #{
        <<"conn-1">> => #{
            <<"channel_id">> => <<"100">>,
            <<"connection_id">> => <<"conn-1">>,
            <<"user_id">> => <<"10">>,
            <<"version">> => 2
        }
    },
    State = maps:put(voice_states, VoiceStates, base_test_state()),
    Request = #{
        user_id => 10,
        channel_id => 100,
        connection_id => <<"conn-1">>,
        mutation_id => <<"m-missing-watched">>,
        runtime_epoch => <<"epoch-1">>,
        base_version => 2,
        viewer_stream_keys => [<<"999:100:missing-conn">>]
    },
    Ack = rejected_voice_state_ack(Request, State),
    ?assertEqual(<<"rejected">>, maps:get(<<"status">>, Ack)),
    ?assertEqual(<<"VOICE_CONNECTION_NOT_FOUND">>, maps:get(<<"error_code">>, Ack)).

voice_state_update_stale_base_version_returns_rejected_ack_test() ->
    VoiceStates = #{
        <<"conn-1">> => #{
            <<"channel_id">> => <<"100">>,
            <<"connection_id">> => <<"conn-1">>,
            <<"user_id">> => <<"10">>,
            <<"version">> => 5
        }
    },
    State = maps:put(voice_states, VoiceStates, base_test_state()),
    Request = #{
        user_id => 10,
        channel_id => 100,
        connection_id => <<"conn-1">>,
        mutation_id => <<"m1">>,
        runtime_epoch => <<"epoch-1">>,
        base_version => 3
    },
    Ack = rejected_voice_state_ack(Request, State),
    ?assertEqual(<<"rejected">>, maps:get(<<"status">>, Ack)),
    ?assertEqual(5, maps:get(<<"server_version">>, Ack)),
    ?assertEqual(<<"stale_base_version">>, maps:get(<<"error_code">>, Ack)).

voice_state_update_stale_base_version_no_superseded_arm_test() ->
    VoiceStates = #{
        <<"conn-2">> => #{
            <<"channel_id">> => <<"100">>,
            <<"connection_id">> => <<"conn-2">>,
            <<"user_id">> => <<"10">>,
            <<"version">> => 10
        }
    },
    State = maps:put(voice_states, VoiceStates, base_test_state()),
    Request = #{
        user_id => 10,
        channel_id => 100,
        connection_id => <<"conn-2">>,
        mutation_id => <<"m-reg">>,
        runtime_epoch => <<"epoch-reg">>,
        base_version => 7
    },
    Ack = rejected_voice_state_ack(Request, State),
    ?assertEqual(<<"rejected">>, maps:get(<<"status">>, Ack)),
    ?assertEqual(10, maps:get(<<"server_version">>, Ack)),
    ?assertEqual(<<"stale_base_version">>, maps:get(<<"error_code">>, Ack)).

voice_state_update_invalid_viewer_stream_keys_returns_rejected_ack_test() ->
    VoiceStates = #{
        <<"conn-1">> => #{
            <<"channel_id">> => <<"100">>,
            <<"connection_id">> => <<"conn-1">>,
            <<"user_id">> => <<"10">>,
            <<"version">> => 2
        }
    },
    State = maps:put(voice_states, VoiceStates, base_test_state()),
    Request = #{
        user_id => 10,
        channel_id => 100,
        connection_id => <<"conn-1">>,
        mutation_id => <<"m2">>,
        runtime_epoch => <<"epoch-1">>,
        base_version => 2,
        viewer_stream_keys => 123
    },
    Ack = rejected_voice_state_ack(Request, State),
    ?assertEqual(<<"rejected">>, maps:get(<<"status">>, Ack)),
    ?assertEqual(<<"VOICE_INVALID_STATE">>, maps:get(<<"error_code">>, Ack)).

voice_state_update_guild_id_missing_test() ->
    State0 = base_test_state(),
    State1 = replace_guild_id(State0, undefined),
    State2 = replace_guild_meta_id(State1, undefined),
    State = maps:remove(id, State2),
    Request = #{user_id => 10, channel_id => 100},
    {reply, {error, validation_error, voice_guild_id_missing}, _} =
        guild_voice_connection:voice_state_update(Request, State).

voice_state_update_invalid_guild_id_test() ->
    State0 = base_test_state(),
    State1 = replace_guild_id(State0, <<"nope">>),
    State2 = replace_guild_meta_id(State1, <<"nope">>),
    State = maps:put(id, undefined, State2),
    Request = #{user_id => 10, channel_id => 100},
    {reply, {error, validation_error, voice_invalid_guild_id}, _} =
        guild_voice_connection:voice_state_update(Request, State).

voice_state_update_connection_user_mismatch_test() ->
    VoiceStates = connected_voice_states(<<"20">>),
    State = maps:put(voice_states, VoiceStates, base_test_state()),
    Request = #{user_id => 10, channel_id => 100, connection_id => <<"conn-1">>},
    {reply, {error, validation_error, voice_user_mismatch}, _} =
        guild_voice_connection:voice_state_update(Request, State).

voice_state_update_connection_owner_match_proceeds_test() ->
    VoiceStates = connected_voice_states(<<"10">>),
    State = maps:put(voice_states, VoiceStates, base_test_state()),
    Request = #{user_id => 10, channel_id => 100, connection_id => <<"conn-1">>},
    case guild_voice_connection:voice_state_update(Request, State) of
        {reply, {error, validation_error, voice_user_mismatch}, _} ->
            error(should_not_get_user_mismatch);
        {reply, _, _} ->
            ok
    end.

voice_state_update_preserves_and_clears_viewer_keys_test() ->
    StreamKey = <<"999:100:conn-stream">>,
    VoiceStates = #{
        <<"conn-1">> => connected_voice_state(<<"conn-1">>, <<"10">>, 100, []),
        <<"conn-stream">> => connected_voice_state(<<"conn-stream">>, <<"20">>, 100, [])
    },
    State0 = maps:put(voice_states, VoiceStates, base_test_state()),
    {reply, #{success := true}, State1} = guild_voice_connection:voice_state_update(
        update_request(#{viewer_stream_keys => [StreamKey]}), State0
    ),
    ?assertEqual([StreamKey], viewer_keys(State1)),
    {reply, #{success := true}, State2} = guild_voice_connection:voice_state_update(
        update_request(#{self_mute => true}), State1
    ),
    ?assertEqual(true, maps:get(<<"self_mute">>, updated_voice_state(State2))),
    ?assertEqual([StreamKey], viewer_keys(State2)),
    {reply, #{success := true}, State3} = guild_voice_connection:voice_state_update(
        update_request(#{viewer_stream_keys => null}), State2
    ),
    ?assertEqual([], viewer_keys(State3)).

voice_state_update_stress_many_watch_unwatch_updates_test() ->
    StreamKey = <<"999:100:conn-stream">>,
    VoiceStates = #{
        <<"conn-1">> => connected_voice_state(<<"conn-1">>, <<"10">>, 100, []),
        <<"conn-stream">> => connected_voice_state(<<"conn-stream">>, <<"20">>, 100, [])
    },
    InitialState = maps:put(voice_states, VoiceStates, base_test_state()),
    FinalState = lists:foldl(
        fun(Index, State) ->
            Keys =
                case Index rem 4 of
                    0 -> [];
                    _ -> [StreamKey]
                end,
            SelfDeaf = Index rem 5 =:= 0,
            {reply, #{success := true}, NextState} = guild_voice_connection:voice_state_update(
                update_request(#{self_deaf => SelfDeaf, viewer_stream_keys => Keys}), State
            ),
            Updated = updated_voice_state(NextState),
            ?assertEqual(SelfDeaf, maps:get(<<"self_deaf">>, Updated)),
            ?assertEqual(Keys, maps:get(<<"viewer_stream_keys">>, Updated)),
            NextState
        end,
        InitialState,
        lists:seq(1, 80)
    ),
    ?assertMatch(#{<<"conn-1">> := #{}}, maps:get(voice_states, FinalState)).

voice_state_update_clears_self_stream_without_stream_permission_test() ->
    State = connected_state(<<"10">>),
    {reply, #{success := true}, NewState} = guild_voice_connection:voice_state_update(
        update_request(#{self_stream => true, self_video => true}), State
    ),
    Updated = updated_voice_state(NewState),
    ?assertEqual(false, maps:get(<<"self_stream">>, Updated)),
    ?assertEqual(false, maps:get(<<"self_video">>, Updated)).

voice_state_update_keeps_self_stream_with_stream_permission_test() ->
    State = maps:put(
        voice_states,
        connected_voice_states(<<"10">>),
        with_stream_permission(base_test_state())
    ),
    {reply, #{success := true}, NewState} = guild_voice_connection:voice_state_update(
        update_request(#{self_stream => true, self_video => true}), State
    ),
    Updated = updated_voice_state(NewState),
    ?assertEqual(true, maps:get(<<"self_stream">>, Updated)),
    ?assertEqual(true, maps:get(<<"self_video">>, Updated)).

voice_state_update_allows_twenty_fifth_camera_sharer_test() ->
    State = camera_state(24),
    {reply, #{success := true}, NewState} = guild_voice_connection:voice_state_update(
        update_request(#{self_video => true}), State
    ),
    ?assertEqual(true, maps:get(<<"self_video">>, updated_voice_state(NewState))).

voice_state_update_blocks_twenty_sixth_camera_sharer_test() ->
    State = camera_state(25),
    assert_voice_state_update_error(
        update_request(#{self_video => true}),
        State,
        {error, permission_denied, voice_camera_user_limit}
    ).

voice_state_update_camera_off_users_do_not_count_toward_camera_limit_test() ->
    CameraOff = maps:from_list([
        {camera_off_conn_id(N), camera_voice_state(camera_off_conn_id(N), 300 + N, false)}
     || N <- lists:seq(1, 25)
    ]),
    VoiceStates = maps:merge(camera_test_voice_states(24), CameraOff),
    State = maps:put(voice_states, VoiceStates, camera_base_state(24)),
    {reply, #{success := true}, NewState} = guild_voice_connection:voice_state_update(
        update_request(#{self_video => true}), State
    ),
    ?assertEqual(true, maps:get(<<"self_video">>, updated_voice_state(NewState))).

voice_state_update_same_user_second_connection_does_not_change_camera_count_test() ->
    VoiceStates = maps:put(
        <<"conn-1b">>,
        camera_voice_state(<<"conn-1b">>, 10, true),
        camera_test_voice_states(24)
    ),
    State = maps:put(voice_states, VoiceStates, camera_base_state(24)),
    {reply, #{success := true}, NewState} = guild_voice_connection:voice_state_update(
        update_request(#{self_video => true}), State
    ),
    ?assertEqual(true, maps:get(<<"self_video">>, updated_voice_state(NewState))).

voice_state_update_camera_off_frees_slot_for_other_user_test() ->
    State0 = camera_state(25),
    assert_voice_state_update_error(
        update_request(#{self_video => true}),
        State0,
        {error, permission_denied, voice_camera_user_limit}
    ),
    {reply, #{success := true}, State1} = guild_voice_connection:voice_state_update(
        update_request(#{
            user_id => 201, connection_id => camera_conn_id(1), self_video => false
        }),
        State0
    ),
    {reply, #{success := true}, State2} = guild_voice_connection:voice_state_update(
        update_request(#{self_video => true}), State1
    ),
    ?assertEqual(true, maps:get(<<"self_video">>, updated_voice_state(State2))).

camera_conn_id(N) ->
    <<"camera-conn-", (integer_to_binary(N))/binary>>.

camera_off_conn_id(N) ->
    <<"camera-off-conn-", (integer_to_binary(N))/binary>>.

camera_voice_state(ConnId, UserId, SelfVideo) ->
    (connected_voice_state(ConnId, integer_to_binary(UserId), 100, []))#{
        <<"self_video">> => SelfVideo
    }.

camera_test_voice_states(SharerCount) ->
    Sharers = maps:from_list([
        {camera_conn_id(N), camera_voice_state(camera_conn_id(N), 200 + N, true)}
     || N <- lists:seq(1, SharerCount)
    ]),
    maps:put(<<"conn-1">>, camera_voice_state(<<"conn-1">>, 10, false), Sharers).

camera_base_state(SharerCount) ->
    Members = [
        base_test_member(10) | [base_test_member(200 + N) || N <- lists:seq(1, SharerCount)]
    ],
    State = with_stream_permission(base_test_state()),
    Data0 = maps:get(data, State, #{}),
    maps:put(data, maps:put(<<"members">>, Members, Data0), State).

with_stream_permission(State) ->
    Roles = [
        #{
            <<"id">> => <<"999">>,
            <<"permissions">> => integer_to_binary(constants:stream_permission())
        }
    ],
    Data0 = maps:get(data, State, #{}),
    maps:put(data, maps:put(<<"roles">>, Roles, Data0), State).

camera_state(SharerCount) ->
    maps:put(
        voice_states, camera_test_voice_states(SharerCount), camera_base_state(SharerCount)
    ).

connected_voice_states(UserId) ->
    #{<<"conn-1">> => #{<<"channel_id">> => <<"100">>, <<"user_id">> => UserId}}.

connected_state(UserId) ->
    maps:put(voice_states, connected_voice_states(UserId), base_test_state()).

connected_voice_state(ConnectionId, UserId, ChannelId, ViewerKeys) ->
    #{
        <<"connection_id">> => ConnectionId,
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"user_id">> => UserId,
        <<"version">> => 0,
        <<"self_mute">> => false,
        <<"self_deaf">> => false,
        <<"self_video">> => false,
        <<"self_stream">> => false,
        <<"is_mobile">> => false,
        <<"viewer_stream_keys">> => ViewerKeys
    }.

update_request(Overrides) ->
    maps:merge(
        #{
            user_id => 10,
            channel_id => 100,
            connection_id => <<"conn-1">>,
            self_mute => false,
            self_deaf => false,
            self_video => false,
            self_stream => false,
            is_mobile => false
        },
        Overrides
    ).

updated_voice_state(State) ->
    maps:get(<<"conn-1">>, maps:get(voice_states, State)).

viewer_keys(State) ->
    maps:get(<<"viewer_stream_keys">>, updated_voice_state(State)).

assert_voice_state_update_error(Request, State, Error) ->
    {reply, Error, _} = guild_voice_connection:voice_state_update(Request, State).

-spec rejected_voice_state_ack(map(), map()) -> map().
rejected_voice_state_ack(Request, State) ->
    {reply, #{ack := #{} = Ack, success := false}, _} =
        guild_voice_connection:voice_state_update(Request, State),
    Ack.
