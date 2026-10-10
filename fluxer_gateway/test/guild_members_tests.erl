%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_members_tests).
-typing([eqwalizer]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_users_to_mention_by_roles_basic_test() ->
    State = test_state(),
    Request = #{channel_id => 500, role_ids => [200], author_id => 1},
    {reply, #{user_ids := UserIds}, _} = guild_members:get_users_to_mention_by_roles(
        Request, State
    ),
    ?assertEqual([2], UserIds).

get_members_with_role_test() ->
    State = test_state(),
    {reply, #{user_ids := UserIds}, _} = guild_members:get_members_with_role(
        #{role_id => 200}, State
    ),
    ?assertEqual([2], UserIds).

get_members_with_role_rejects_malformed_role_id_test() ->
    State = test_state(),
    {reply, #{user_ids := UserIds}, _} = guild_members:get_members_with_role(
        #{role_id => <<"0200">>}, State
    ),
    ?assertEqual([], UserIds).

resolve_all_mentions_test() ->
    State = test_state(),
    Request = #{
        channel_id => 500,
        author_id => 1,
        mention_everyone => false,
        mention_here => false,
        role_ids => [200],
        user_ids => [3]
    },
    {reply, #{user_ids := UserIds}, _} = guild_members:resolve_all_mentions(Request, State),
    ?assertEqual([2, 3], UserIds).

resolve_channel_mentions_skips_invalid_channel_ids_test() ->
    State = channel_mentions_state([]),
    {reply, #{channels := Channels}, _} = guild_members:resolve_channel_mentions(
        #{channel_ids => [<<"001">>, <<"500">>]},
        State
    ),
    ?assertEqual([#{id => 500, name => <<"general">>, type => 0}], Channels).

resolve_channel_mentions_ignores_user_zero_overwrite_test() ->
    ViewPerm = constants:view_channel_permission(),
    Overwrites = [
        #{
            <<"id">> => <<"0">>,
            <<"type">> => 1,
            <<"allow">> => <<"0">>,
            <<"deny">> => integer_to_binary(ViewPerm)
        }
    ],
    State = channel_mentions_state(Overwrites),
    {reply, #{channels := Channels}, _} = guild_members:resolve_channel_mentions(
        #{channel_ids => [<<"500">>]},
        State
    ),
    ?assertEqual([#{id => 500, name => <<"general">>, type => 0}], Channels).

get_assignable_roles_owner_test() ->
    State = test_state(),
    {reply, #{role_ids := RoleIds}, _} = guild_members:get_assignable_roles(
        #{user_id => 1}, State
    ),
    ?assertEqual(lists:sort([100, 200, 201]), lists:sort(RoleIds)).

get_viewable_channels_skips_malformed_channel_id_test() ->
    State0 = test_state(),
    Data0 = maps:get(data, State0),
    Channels = maps:get(<<"channels">>, Data0),
    BadChannel = #{<<"id">> => <<"0502">>, <<"type">> => 0, <<"permission_overwrites">> => []},
    State = State0#{data => Data0#{<<"channels">> => [BadChannel | Channels]}},
    {reply, #{channel_ids := ChannelIds}, _} = guild_members:get_viewable_channels(
        #{user_id => 1}, State
    ),
    ?assert(lists:member(500, ChannelIds)),
    ?assertNot(lists:member(502, ChannelIds)).

check_target_member_test() ->
    State = test_state(),
    {reply, #{can_manage := true}, _} = guild_members:check_target_member(
        #{user_id => 1, target_user_id => 2}, State
    ),
    {reply, #{can_manage := false}, _} = guild_members:check_target_member(
        #{user_id => 2, target_user_id => 1}, State
    ).

test_state() ->
    GuildId = 100,
    OwnerId = 1,
    MemberId = 2,
    OtherId = 3,
    ChannelId = 500,
    RoleMod = 200,
    RoleHigh = 201,
    ViewPerm = constants:view_channel_permission(),
    ManageRoles = constants:manage_roles_permission(),
    #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => integer_to_binary(OwnerId)},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm bor ManageRoles),
                    <<"position">> => 0
                },
                #{
                    <<"id">> => integer_to_binary(RoleMod),
                    <<"permissions">> => integer_to_binary(ViewPerm),
                    <<"position">> => 10
                },
                #{
                    <<"id">> => integer_to_binary(RoleHigh),
                    <<"permissions">> => integer_to_binary(ViewPerm),
                    <<"position">> => 20
                }
            ],
            <<"channels">> => [
                #{
                    <<"id">> => integer_to_binary(ChannelId),
                    <<"type">> => 0,
                    <<"permission_overwrites">> => []
                },
                #{
                    <<"id">> => integer_to_binary(ChannelId + 1),
                    <<"type">> => 2,
                    <<"permission_overwrites">> => []
                }
            ],
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(OwnerId)},
                    <<"roles">> => [integer_to_binary(GuildId)]
                },
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(MemberId)},
                    <<"roles">> => [integer_to_binary(RoleMod)],
                    <<"joined_at">> => <<"2024-01-01T00:00:00Z">>
                },
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(OtherId)},
                    <<"roles">> => [integer_to_binary(RoleHigh)],
                    <<"joined_at">> => <<"2024-01-02T00:00:00Z">>
                }
            ]
        }
    }.

channel_mentions_state(Overwrites) ->
    GuildId = 100,
    ViewPerm = constants:view_channel_permission(),
    #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"1">>},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                }
            ],
            <<"channels">> => [
                #{
                    <<"id">> => <<"500">>,
                    <<"name">> => <<"general">>,
                    <<"type">> => 0,
                    <<"permission_overwrites">> => Overwrites
                },
                #{
                    <<"id">> => <<"001">>,
                    <<"name">> => <<"bad">>,
                    <<"type">> => 0,
                    <<"permission_overwrites">> => []
                }
            ],
            <<"members">> => []
        }
    }.

-endif.
