%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_visibility_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

preserves_connected_channel_visibility_on_permission_loss_test() ->
    UserId = 10,
    GuildId = 1,
    ChannelId = 5,
    ViewPerm = constants:view_channel_permission(),
    OldState = visibility_state(GuildId, UserId, ChannelId, ViewPerm, true),
    NewState = visibility_state(GuildId, UserId, ChannelId, 0, true),
    UpdatedState = guild_visibility_overwrites:compute_and_dispatch_visibility_changes(
        OldState, NewState
    ),
    ?assert(guild_virtual_channel_access:has_virtual_access(UserId, ChannelId, UpdatedState)),
    ?assertEqual(
        false,
        guild_virtual_channel_access:is_pending_join(UserId, ChannelId, UpdatedState)
    ),
    ?assert(
        guild_permissions_check:can_view_channel(UserId, ChannelId, undefined, UpdatedState)
    ).

does_not_add_virtual_access_when_not_connected_test() ->
    UserId = 20,
    GuildId = 2,
    ChannelId = 6,
    ViewPerm = constants:view_channel_permission(),
    OldState = visibility_state(GuildId, UserId, ChannelId, ViewPerm, false),
    NewState = visibility_state(GuildId, UserId, ChannelId, 0, false),
    UpdatedState = guild_visibility_overwrites:compute_and_dispatch_visibility_changes(
        OldState, NewState
    ),
    ?assertNot(
        guild_virtual_channel_access:has_virtual_access(UserId, ChannelId, UpdatedState)
    ).

does_not_add_virtual_access_when_permission_remains_test() ->
    UserId = 30,
    GuildId = 3,
    ChannelId = 7,
    ViewPerm = constants:view_channel_permission(),
    OldState = visibility_state(GuildId, UserId, ChannelId, ViewPerm, true),
    NewState = visibility_state(GuildId, UserId, ChannelId, ViewPerm, true),
    UpdatedState = guild_visibility_overwrites:compute_and_dispatch_visibility_changes(
        OldState, NewState
    ),
    ?assertNot(
        guild_virtual_channel_access:has_virtual_access(UserId, ChannelId, UpdatedState)
    ).

compute_and_dispatch_visibility_changes_for_users_targets_selected_users_test() ->
    GuildId = 33,
    ChannelId = 77,
    RoleId = 101,
    ViewPerm = constants:view_channel_permission(),
    OldState = users_visibility_state(GuildId, ChannelId, RoleId, ViewPerm, role, role),
    NewState = users_visibility_state(GuildId, ChannelId, RoleId, ViewPerm, none, role),
    UpdatedState = guild_visibility_overwrites:compute_and_dispatch_visibility_changes_for_users(
        [10], OldState, NewState
    ),
    UpdatedSessions = maps:get(sessions, UpdatedState),
    UpdatedSession10 = maps:get(<<"s10">>, UpdatedSessions),
    UpdatedSession20 = maps:get(<<"s20">>, UpdatedSessions),
    ?assertEqual(
        false,
        maps:is_key(ChannelId, maps:get(viewable_channels, UpdatedSession10, #{}))
    ),
    ?assertEqual(
        true,
        maps:is_key(ChannelId, maps:get(viewable_channels, UpdatedSession20, #{}))
    ).

compute_and_dispatch_visibility_changes_for_channels_limits_test() ->
    GuildId = 44,
    UserId = 10,
    ChannelA = 100,
    ChannelB = 101,
    ViewPerm = constants:view_channel_permission(),
    BaseRole = #{
        <<"id">> => integer_to_binary(GuildId),
        <<"permissions">> => integer_to_binary(ViewPerm)
    },
    OldChannels = [visible_channel(ChannelA), visible_channel(ChannelB)],
    NewChannels = [denied_channel(ChannelA, GuildId, ViewPerm), visible_channel(ChannelB)],
    OldState = channels_visibility_state(
        GuildId, UserId, BaseRole, ChannelA, ChannelB, OldChannels
    ),
    NewState = channels_visibility_state(
        GuildId, UserId, BaseRole, ChannelA, ChannelB, NewChannels
    ),
    UpdatedState = guild_visibility_overwrites:compute_and_dispatch_visibility_changes_for_channels(
        [ChannelA], OldState, NewState
    ),
    UpdatedSession = maps:get(<<"s10">>, maps:get(sessions, UpdatedState)),
    UpdatedViewable = maps:get(viewable_channels, UpdatedSession, #{}),
    ?assertEqual(false, maps:is_key(ChannelA, UpdatedViewable)),
    ?assertEqual(true, maps:is_key(ChannelB, UpdatedViewable)).

connected_voice_channel_sets_groups_channels_by_user_test() ->
    State = #{
        voice_states => #{
            <<"a">> => #{<<"user_id">> => <<"10">>, <<"channel_id">> => <<"100">>},
            <<"b">> => #{<<"user_id">> => <<"10">>, <<"channel_id">> => <<"101">>},
            <<"c">> => #{<<"user_id">> => <<"20">>, <<"channel_id">> => <<"200">>},
            <<"bad">> => #{<<"user_id">> => <<"20">>, <<"channel_id">> => null}
        }
    },
    Result = guild_visibility_channels:connected_voice_channel_sets(State),
    ?assertEqual([100, 101], lists:sort(sets:to_list(maps:get(10, Result)))),
    ?assertEqual([200], sets:to_list(maps:get(20, Result))).

users_visibility_state(GuildId, ChannelId, RoleId, ViewPerm, User10Role, User20Role) ->
    Session10 = visibility_session(<<"s10">>, 10, #{ChannelId => true}),
    Session20 = visibility_session(<<"s20">>, 20, #{ChannelId => true}),
    #{
        id => GuildId,
        sessions => #{<<"s10">> => Session10, <<"s20">> => Session20},
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => visibility_roles(GuildId, RoleId, ViewPerm),
            <<"members">> => #{
                10 => visibility_member(10, role_ids(User10Role, RoleId)),
                20 => visibility_member(20, role_ids(User20Role, RoleId))
            },
            <<"channels">> => [visible_channel(ChannelId)]
        }
    }.

channels_visibility_state(GuildId, UserId, BaseRole, ChannelA, ChannelB, Channels) ->
    Session = visibility_session(<<"s10">>, UserId, #{ChannelA => true, ChannelB => true}),
    #{
        id => GuildId,
        sessions => #{<<"s10">> => Session},
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [BaseRole],
            <<"members">> => #{UserId => visibility_member(UserId, [])},
            <<"channels">> => Channels
        }
    }.

visibility_session(SessionId, UserId, ViewableChannels) ->
    #{
        session_id => SessionId,
        user_id => UserId,
        pid => self(),
        viewable_channels => ViewableChannels
    }.

visibility_roles(GuildId, RoleId, ViewPerm) ->
    [
        #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>},
        #{
            <<"id">> => integer_to_binary(RoleId),
            <<"permissions">> => integer_to_binary(ViewPerm)
        }
    ].

visibility_member(UserId, Roles) ->
    #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => Roles}.

role_ids(role, RoleId) ->
    [integer_to_binary(RoleId)];
role_ids(none, _RoleId) ->
    [].

visible_channel(ChannelId) ->
    #{<<"id">> => integer_to_binary(ChannelId), <<"permission_overwrites">> => []}.

denied_channel(ChannelId, GuildId, ViewPerm) ->
    #{
        <<"id">> => integer_to_binary(ChannelId),
        <<"permission_overwrites">> => [
            #{
                <<"id">> => integer_to_binary(GuildId),
                <<"type">> => 0,
                <<"allow">> => <<"0">>,
                <<"deny">> => integer_to_binary(ViewPerm)
            }
        ]
    }.

visibility_state(GuildId, UserId, ChannelId, Perms, Connected) ->
    VoiceStates =
        case Connected of
            true ->
                #{
                    <<"conn">> => #{
                        <<"user_id">> => integer_to_binary(UserId),
                        <<"guild_id">> => integer_to_binary(GuildId),
                        <<"channel_id">> => integer_to_binary(ChannelId),
                        <<"connection_id">> => <<"conn">>
                    }
                };
            false ->
                #{}
        end,
    Sessions = #{<<"s1">> => #{user_id => UserId, pid => self()}},
    Data = #{
        <<"guild">> => #{<<"owner_id">> => <<"999">>},
        <<"roles">> => [
            #{
                <<"id">> => integer_to_binary(GuildId),
                <<"permissions">> => integer_to_binary(Perms)
            }
        ],
        <<"members">> => [
            #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => []}
        ],
        <<"channels">> => [
            #{<<"id">> => integer_to_binary(ChannelId), <<"permission_overwrites">> => []}
        ]
    },
    #{id => GuildId, data => Data, sessions => Sessions, voice_states => VoiceStates}.
