%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_state_roles_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

handle_role_delete_strips_from_members_test() ->
    RoleIdToDelete = <<"200">>,
    Data = guild_data_index:normalize_map(#{
        <<"roles">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"Admin">>},
            #{<<"id">> => <<"200">>, <<"name">> => <<"Moderator">>}
        ],
        <<"members">> => #{
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"100">>, <<"200">>]},
            2 => #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => [<<"200">>]},
            3 => #{<<"user">> => #{<<"id">> => <<"3">>}, <<"roles">> => [<<"100">>]}
        },
        <<"channels">> => []
    }),
    EventData = #{<<"role_id">> => RoleIdToDelete},
    Result = guild_state_roles:handle_role_delete(EventData, Data),
    Members = maps:get(<<"members">>, Result),
    M1 = maps:get(1, Members),
    M2 = maps:get(2, Members),
    M3 = maps:get(3, Members),
    ?assertEqual([100], maps:get(<<"roles">>, M1)),
    ?assertEqual([], maps:get(<<"roles">>, M2)),
    ?assertEqual([100], maps:get(<<"roles">>, M3)).

handle_role_delete_strips_from_channel_overwrites_test() ->
    RoleIdToDelete = <<"200">>,
    Data = #{
        <<"roles">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"Everyone">>},
            #{<<"id">> => <<"200">>, <<"name">> => <<"Moderator">>}
        ],
        <<"members">> => [],
        <<"channels">> => [
            #{
                <<"id">> => <<"500">>,
                <<"permission_overwrites">> => [
                    #{
                        <<"id">> => <<"100">>,
                        <<"type">> => 0,
                        <<"allow">> => <<"0">>,
                        <<"deny">> => <<"1024">>
                    },
                    #{
                        <<"id">> => <<"200">>,
                        <<"type">> => 0,
                        <<"allow">> => <<"1024">>,
                        <<"deny">> => <<"0">>
                    },
                    #{
                        <<"id">> => <<"1">>,
                        <<"type">> => 1,
                        <<"allow">> => <<"2048">>,
                        <<"deny">> => <<"0">>
                    }
                ]
            }
        ]
    },
    EventData = #{<<"role_id">> => RoleIdToDelete},
    Result = guild_state_roles:handle_role_delete(EventData, Data),
    Channels = maps:get(<<"channels">>, Result),
    [Ch1] = Channels,
    Ch1Overwrites = maps:get(<<"permission_overwrites">>, Ch1),
    ?assertEqual(2, length(Ch1Overwrites)).

handle_role_create_test() ->
    Data = #{
        <<"roles">> => [#{<<"id">> => <<"1">>, <<"name">> => <<"Everyone">>}],
        <<"members">> => [],
        <<"channels">> => []
    },
    EventData = #{<<"role">> => #{<<"id">> => <<"2">>, <<"name">> => <<"New">>}},
    Result = guild_state_roles:handle_role_create(EventData, Data),
    Roles = guild_data_index:role_list(Result),
    ?assertEqual(2, length(Roles)),
    RoleIndex = guild_data_index:role_index(Result),
    ?assertMatch(#{2 := _}, RoleIndex).

handle_role_update_replaces_role_test() ->
    Data = #{
        <<"roles">> => [
            #{<<"id">> => <<"1">>, <<"name">> => <<"Old">>},
            #{<<"id">> => <<"2">>, <<"name">> => <<"Keep">>}
        ],
        <<"members">> => [],
        <<"channels">> => []
    },
    EventData = #{<<"role">> => #{<<"id">> => <<"1">>, <<"name">> => <<"Updated">>}},
    Result = guild_state_roles:handle_role_update(EventData, Data),
    Roles = guild_data_index:role_list(Result),
    [R1, R2] = Roles,
    ?assertEqual(<<"Updated">>, maps:get(<<"name">>, R1)),
    ?assertEqual(<<"Keep">>, maps:get(<<"name">>, R2)).

handle_role_update_bulk_test() ->
    Data = #{
        <<"roles">> => [
            #{<<"id">> => <<"1">>, <<"name">> => <<"A">>},
            #{<<"id">> => <<"2">>, <<"name">> => <<"B">>},
            #{<<"id">> => <<"3">>, <<"name">> => <<"C">>}
        ],
        <<"members">> => [],
        <<"channels">> => []
    },
    EventData = #{
        <<"roles">> => [
            #{<<"id">> => <<"1">>, <<"name">> => <<"A2">>},
            #{<<"id">> => <<"3">>, <<"name">> => <<"C2">>}
        ]
    },
    Result = guild_state_roles:handle_role_update_bulk(EventData, Data),
    Roles = guild_data_index:role_list(Result),
    [R1, R2, R3] = Roles,
    ?assertEqual(<<"A2">>, maps:get(<<"name">>, R1)),
    ?assertEqual(<<"B">>, maps:get(<<"name">>, R2)),
    ?assertEqual(<<"C2">>, maps:get(<<"name">>, R3)).

handle_role_delete_removes_role_from_list_test() ->
    Data = #{
        <<"roles">> => [
            #{<<"id">> => <<"1">>, <<"name">> => <<"Keep">>},
            #{<<"id">> => <<"2">>, <<"name">> => <<"Delete">>}
        ],
        <<"members">> => [],
        <<"channels">> => []
    },
    EventData = #{<<"role_id">> => <<"2">>},
    Result = guild_state_roles:handle_role_delete(EventData, Data),
    Roles = guild_data_index:role_list(Result),
    ?assertEqual(1, length(Roles)),
    ?assertEqual(<<"Keep">>, maps:get(<<"name">>, hd(Roles))).

strip_role_from_members_no_affected_users_test() ->
    Data = guild_data_index:normalize_map(#{
        <<"roles">> => [],
        <<"members">> => #{
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"100">>]}
        },
        <<"channels">> => []
    }),
    Result = guild_state_roles_delete:strip_role_from_members(999, Data),
    #{<<"roles">> := Roles} = guild_data_index:get_member(1, Result),
    ?assertEqual([100], Roles).

strip_role_from_channel_overwrites_preserves_user_overwrites_test() ->
    Data = #{
        <<"channels">> => [
            #{
                <<"id">> => <<"500">>,
                <<"permission_overwrites">> => [
                    #{
                        <<"id">> => <<"100">>,
                        <<"type">> => 0,
                        <<"allow">> => <<"0">>,
                        <<"deny">> => <<"0">>
                    },
                    #{
                        <<"id">> => <<"1">>,
                        <<"type">> => 1,
                        <<"allow">> => <<"1024">>,
                        <<"deny">> => <<"0">>
                    }
                ]
            }
        ]
    },
    Result = guild_state_roles_delete:strip_role_from_channel_overwrites(100, Data),
    [Ch] = guild_data_index:channel_list(Result),
    Overwrites = maps:get(<<"permission_overwrites">>, Ch),
    ?assertEqual(1, length(Overwrites)),
    ?assertEqual(1, maps:get(<<"type">>, hd(Overwrites))).

extract_role_ids_from_role_update_test() ->
    EventData = #{<<"role">> => #{<<"id">> => <<"42">>}},
    ?assertEqual([42], guild_state_roles:extract_role_ids_from_role_update(EventData)).

extract_role_ids_from_role_update_missing_id_test() ->
    EventData = #{<<"role">> => #{}},
    ?assertEqual([], guild_state_roles:extract_role_ids_from_role_update(EventData)).

extract_role_ids_from_role_update_missing_role_test() ->
    ?assertEqual([], guild_state_roles:extract_role_ids_from_role_update(#{})).

extract_role_ids_from_role_update_invalid_id_test() ->
    ?assertEqual(
        [],
        guild_state_roles:extract_role_ids_from_role_update(
            #{<<"role">> => #{<<"id">> => <<"001">>}}
        )
    ),
    ?assertEqual(
        [],
        guild_state_roles:extract_role_ids_from_role_update(
            #{<<"role">> => #{<<"id">> => <<"0">>}}
        )
    ).

extract_role_ids_from_role_update_bulk_test() ->
    EventData = #{<<"roles">> => [#{<<"id">> => <<"1">>}, #{<<"id">> => <<"2">>}]},
    ?assertEqual([1, 2], guild_state_roles:extract_role_ids_from_role_update_bulk(EventData)).

extract_role_ids_from_role_update_bulk_empty_test() ->
    ?assertEqual([], guild_state_roles:extract_role_ids_from_role_update_bulk(#{})).

extract_role_ids_from_role_delete_test() ->
    EventData = #{<<"role_id">> => <<"55">>},
    ?assertEqual([55], guild_state_roles:extract_role_ids_from_role_delete(EventData)).

extract_role_ids_from_role_delete_missing_test() ->
    ?assertEqual([], guild_state_roles:extract_role_ids_from_role_delete(#{})).

handle_role_delete_invalid_id_noop_test() ->
    Data = #{<<"roles">> => [#{<<"id">> => <<"1">>, <<"name">> => <<"Keep">>}]},
    ?assertEqual(
        Data, guild_state_roles:handle_role_delete(#{<<"role_id">> => <<"001">>}, Data)
    ).
