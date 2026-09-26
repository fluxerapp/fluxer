%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_permissions_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

voice_permissions_missing_view_test() ->
    State = permission_test_state(0, fun(_) -> constants:view_channel_permission() end),
    Result = guild_voice_permissions:check_voice_permissions_and_limits(
        1, 10, #{<<"user_limit">> => 0}, #{}, State, false
    ),
    ?assertMatch({error, permission_denied, voice_permission_denied}, Result).

voice_permissions_full_channel_test() ->
    State = permission_test_state(2, fun(_) -> required_voice_perms() end),
    VoiceStates = #{
        <<"conn1">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"1">>},
        <<"conn2">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"2">>}
    },
    Result = guild_voice_permissions:check_voice_permissions_and_limits(
        3, 10, #{<<"user_limit">> => 2}, VoiceStates, State, false
    ),
    ?assertMatch({error, permission_denied, voice_channel_full}, Result).

voice_permissions_existing_user_update_test() ->
    State = permission_test_state(2, fun(_) -> required_voice_perms() end),
    VoiceStates = #{
        <<"conn1">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"1">>},
        <<"conn2">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"2">>}
    },
    Result = guild_voice_permissions:check_voice_permissions_and_limits(
        1, 10, #{<<"user_limit">> => 2}, VoiceStates, State, true
    ),
    ?assertEqual({ok, allowed}, Result).

voice_permissions_unlimited_channel_camera_caps_at_25_test() ->
    State = permission_test_state(2, fun(_) -> required_voice_perms() end),
    VoiceStates = camera_limited_voice_states(25, 10),
    Result = guild_voice_permissions:check_voice_permissions_and_limits(
        26, 10, #{<<"user_limit">> => 0}, VoiceStates, State, false
    ),
    ?assertMatch({error, permission_denied, voice_channel_full}, Result).

voice_permissions_connection_limit_reached_test() ->
    State = permission_test_state(2, fun(_) -> required_voice_perms() end),
    VoiceStates = #{
        <<"conn1">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"1">>},
        <<"conn2">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"1">>}
    },
    Result = guild_voice_permissions:check_voice_permissions_and_limits(
        1,
        10,
        #{<<"user_limit">> => 0, <<"voice_connection_limit">> => 2},
        VoiceStates,
        State,
        false
    ),
    ?assertMatch({error, permission_denied, voice_connection_limit_reached}, Result),
    OtherUserResult = guild_voice_permissions:check_voice_permissions_and_limits(
        2,
        10,
        #{<<"user_limit">> => 0, <<"voice_connection_limit">> => 2},
        VoiceStates,
        State,
        false
    ),
    ?assertEqual({ok, allowed}, OtherUserResult).

voice_permissions_connection_limit_counts_pending_test() ->
    Future = erlang:system_time(millisecond) + 1000,
    State0 = permission_test_state(2, fun(_) -> required_voice_perms() end),
    State = maps:put(
        pending_voice_connections,
        #{
            <<"pending">> => #{
                user_id => 1,
                channel_id => 10,
                expires_at => Future
            }
        },
        State0
    ),
    VoiceStates = #{
        <<"conn1">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"1">>},
        <<"conn2">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"1">>},
        <<"conn3">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"1">>},
        <<"conn4">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"1">>}
    },
    Result = guild_voice_permissions:check_voice_permissions_and_limits(
        1, 10, #{<<"user_limit">> => 0}, VoiceStates, State, false
    ),
    ?assertMatch({error, permission_denied, voice_connection_limit_reached}, Result).

voice_permissions_virtual_access_bypass_test() ->
    Base = permission_test_state(7, fun(_) -> 0 end),
    State = Base#{virtual_channel_access => #{1 => sets:from_list([10])}},
    Result = guild_voice_permissions:check_voice_permissions_and_limits(
        1, 10, #{<<"user_limit">> => 0}, #{}, State, false
    ),
    ?assertEqual({ok, allowed}, Result).

voice_permissions_move_pending_bypass_test() ->
    Base = permission_test_state(7, fun(_) -> 0 end),
    State = Base#{virtual_channel_access_move_pending => #{1 => sets:from_list([10])}},
    Result = guild_voice_permissions:check_voice_permissions_and_limits(
        1, 10, #{<<"user_limit">> => 0}, #{}, State, false
    ),
    ?assertEqual({ok, allowed}, Result).

voice_permissions_without_lease_denied_test() ->
    State = permission_test_state(7, fun(_) -> 0 end),
    Result = guild_voice_permissions:check_voice_permissions_and_limits(
        1, 10, #{<<"user_limit">> => 0}, #{}, State, false
    ),
    ?assertMatch({error, permission_denied, voice_permission_denied}, Result).

voice_permissions_virtual_access_other_channel_denied_test() ->
    Base = permission_test_state(7, fun(_) -> 0 end),
    State = Base#{virtual_channel_access => #{1 => sets:from_list([99])}},
    Result = guild_voice_permissions:check_voice_permissions_and_limits(
        1, 10, #{<<"user_limit">> => 0}, #{}, State, false
    ),
    ?assertMatch({error, permission_denied, voice_permission_denied}, Result).

users_in_channel_test() ->
    VoiceStates = #{
        <<"conn1">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"1">>},
        <<"conn2">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"2">>},
        <<"conn3">> => #{<<"channel_id">> => <<"20">>, <<"user_id">> => <<"3">>}
    },
    Result = guild_voice_permissions:users_in_channel(10, VoiceStates),
    ?assertEqual(2, sets:size(Result)),
    ?assert(sets:is_element(1, Result)),
    ?assert(sets:is_element(2, Result)),
    ?assertNot(sets:is_element(3, Result)).

permission_sync_disconnects_when_view_channel_lost_test() ->
    ok = drain_mailbox(),
    ConnectOnly = constants:connect_permission(),
    {State, UserId, ChannelId, GuildId} = build_perm_sync_state(ConnectOnly),
    ok = guild_voice_permission_sync:sync_user_voice_permissions(UserId, State),
    receive
        {synced, GuildId, ChannelId, UserId, <<"test-conn">>, Perms} ->
            ?assertEqual(true, maps:get(disconnected, Perms, false))
    after 200 ->
        ?assert(false)
    end.

permission_sync_does_not_disconnect_with_full_perms_test() ->
    ok = drain_mailbox(),
    FullPerms =
        constants:view_channel_permission() bor
            constants:connect_permission() bor
            constants:speak_permission(),
    {State, UserId, ChannelId, GuildId} = build_perm_sync_state(FullPerms),
    ok = guild_voice_permission_sync:sync_user_voice_permissions(UserId, State),
    receive
        {synced, GuildId, ChannelId, UserId, <<"test-conn">>, Perms} ->
            ?assertEqual(true, maps:get(can_speak, Perms, false)),
            ?assertNot(maps:is_key(disconnected, Perms))
    after 200 ->
        ?assert(false)
    end.

permission_sync_runs_on_role_permission_update_test() ->
    ok = drain_mailbox(),
    {State, UserId, ChannelId, GuildId, RoleId} = build_sync_wiring_state(),
    _ = guild_state:update_state(
        guild_role_update,
        #{
            <<"role">> => #{
                <<"id">> => integer_to_binary(RoleId),
                <<"permissions">> => <<"0">>
            }
        },
        State
    ),
    receive
        {synced, GuildId, ChannelId, UserId, <<"test-conn">>, Perms} ->
            ?assertEqual(false, maps:get(can_speak, Perms)),
            ?assertEqual(false, maps:get(can_stream, Perms))
    after 200 ->
        ?assert(false)
    end.

permission_sync_runs_on_channel_overwrite_update_test() ->
    ok = drain_mailbox(),
    {State, UserId, ChannelId, GuildId, RoleId} = build_sync_wiring_state(),
    _ = guild_state:update_state(
        channel_update,
        #{
            <<"id">> => integer_to_binary(ChannelId),
            <<"type">> => 2,
            <<"permission_overwrites">> => [
                #{
                    <<"id">> => integer_to_binary(RoleId),
                    <<"type">> => 0,
                    <<"allow">> => <<"0">>,
                    <<"deny">> => integer_to_binary(constants:stream_permission())
                }
            ]
        },
        State
    ),
    receive
        {synced, GuildId, ChannelId, UserId, <<"test-conn">>, Perms} ->
            ?assertEqual(true, maps:get(can_speak, Perms)),
            ?assertEqual(false, maps:get(can_stream, Perms))
    after 200 ->
        ?assert(false)
    end.

permission_sync_runs_on_member_role_removal_test() ->
    ok = drain_mailbox(),
    {State, UserId, ChannelId, GuildId, _RoleId} = build_sync_wiring_state(),
    _ = guild_state:update_state(
        guild_member_update,
        #{
            <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
            <<"roles">> => []
        },
        State
    ),
    receive
        {synced, GuildId, ChannelId, UserId, <<"test-conn">>, Perms} ->
            ?assertEqual(false, maps:get(can_speak, Perms)),
            ?assertEqual(false, maps:get(can_stream, Perms))
    after 200 ->
        ?assert(false)
    end.

build_sync_wiring_state() ->
    Self = self(),
    TestFun = fun(GId, ChId, UId, ConnId, Perms) ->
        Self ! {synced, GId, ChId, UId, ConnId, Perms}
    end,
    UserId = 10,
    ChannelId = 500,
    GuildId = 42,
    RoleId = 999,
    RoleIdBin = integer_to_binary(RoleId),
    EveryonePerms = required_voice_perms(),
    RolePerms = constants:speak_permission() bor constants:stream_permission(),
    State = #{
        id => GuildId,
        voice_states => #{
            <<"conn">> => #{
                <<"user_id">> => integer_to_binary(UserId),
                <<"channel_id">> => integer_to_binary(ChannelId),
                <<"connection_id">> => <<"test-conn">>,
                <<"deaf">> => false
            }
        },
        member_list_subscriptions => guild_member_list_subs:new(),
        test_permission_sync_fun => TestFun,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"1">>},
            <<"roles">> => [
                #{<<"id">> => RoleIdBin, <<"permissions">> => integer_to_binary(RolePerms)},
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(EveryonePerms)
                }
            ],
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => [RoleIdBin]
                }
            ],
            <<"channels">> => [
                #{
                    <<"id">> => integer_to_binary(ChannelId),
                    <<"type">> => 2,
                    <<"permission_overwrites">> => []
                }
            ]
        }
    },
    {State, UserId, ChannelId, GuildId, RoleId}.

build_perm_sync_state(Permissions) ->
    Self = self(),
    TestFun = fun(GId, ChId, UId, ConnId, Perms) ->
        Self ! {synced, GId, ChId, UId, ConnId, Perms}
    end,
    UserId = 10,
    ChannelId = 500,
    GuildId = 42,
    RoleId = 999,
    RoleIdBin = integer_to_binary(RoleId),
    GuildIdBin = integer_to_binary(GuildId),
    UserIdBin = integer_to_binary(UserId),
    ChIdBin = integer_to_binary(ChannelId),
    PermsBin = integer_to_binary(Permissions),
    VoiceState = #{
        <<"user_id">> => UserIdBin,
        <<"channel_id">> => ChIdBin,
        <<"connection_id">> => <<"test-conn">>,
        <<"deaf">> => false
    },
    State = #{
        id => GuildId,
        voice_states => #{<<"conn">> => VoiceState},
        test_permission_sync_fun => TestFun,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"1">>},
            <<"roles">> => [
                #{<<"id">> => RoleIdBin, <<"permissions">> => PermsBin},
                #{<<"id">> => GuildIdBin, <<"permissions">> => <<"0">>}
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => UserIdBin}, <<"roles">> => [RoleIdBin]}
            ],
            <<"channels">> => [
                #{<<"id">> => ChIdBin, <<"permission_overwrites">> => []}
            ]
        }
    },
    {State, UserId, ChannelId, GuildId}.

required_voice_perms() ->
    constants:view_channel_permission() bor constants:connect_permission().

permission_test_state(GuildId, PermFun) ->
    #{id => GuildId, test_perm_fun => PermFun}.

camera_limited_voice_states(Count, ChannelId) ->
    lists:foldl(
        fun(UserId, Acc) ->
            ConnId = <<"conn", (integer_to_binary(UserId))/binary>>,
            Acc#{
                ConnId => #{
                    <<"channel_id">> => integer_to_binary(ChannelId),
                    <<"user_id">> => integer_to_binary(UserId),
                    <<"self_video">> => UserId =:= 1
                }
            }
        end,
        #{},
        lists:seq(1, Count)
    ).

drain_mailbox() ->
    receive
        _ -> drain_mailbox()
    after 0 ->
        ok
    end.
