%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_index_members_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

member_map_edge_cases_test() ->
    ?assertEqual(#{}, guild_data_index_members:member_map(invalid_guild_data())),
    ?assertEqual(#{}, guild_data_index_members:member_map(#{<<"members">> => <<"invalid">>})),
    ?assertEqual(
        #{},
        guild_data_index_members:member_map(#{<<"members">> => [#{<<"nick">> => <<"orphan">>}]})
    ).

member_map_duplicate_user_ids_last_wins_test() ->
    Data = #{
        <<"members">> => [
            #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"first">>},
            #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"second">>}
        ]
    },
    MemberMap = guild_data_index_members:member_map(Data),
    ?assertEqual(<<"second">>, maps:get(<<"nick">>, maps:get(1, MemberMap))).

member_map_rejects_malformed_snowflake_ids_test() ->
    Data = #{
        <<"members">> => #{
            <<"001">> => #{<<"user">> => #{<<"id">> => <<"001">>}},
            <<"2">> => #{<<"user">> => #{<<"id">> => <<"02">>}},
            <<"bad">> => #{<<"user">> => #{<<"id">> => <<"3">>}}
        }
    },
    ?assertEqual(
        #{3 => #{<<"user">> => #{<<"id">> => 3}}}, guild_data_index_members:member_map(Data)
    ).

member_list_uses_cache_consistently_test() ->
    Base = guild_data_index_members:put_member_map(
        #{
            5 => #{<<"user">> => #{<<"id">> => <<"5">>}},
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}}
        },
        #{<<"members">> => #{}}
    ),
    ?assert(maps:is_key(members_sorted_ids, Base)),
    ?assertEqual([1, 5], [member_id(M) || M <- guild_data_index_members:member_list(Base)]),
    Added = guild_data_index_members:put_member(
        #{<<"user">> => #{<<"id">> => <<"3">>}}, Base
    ),
    ?assertNot(maps:is_key(members_sorted_ids, Added)),
    ?assertEqual([1, 3, 5], [member_id(M) || M <- guild_data_index_members:member_list(Added)]),
    Removed = guild_data_index_members:remove_member(1, Added),
    ?assertEqual([3, 5], [member_id(M) || M <- guild_data_index_members:member_list(Removed)]).

member_id(Member) ->
    maps:get(<<"id">>, maps:get(<<"user">>, Member)).

member_role_index_test() ->
    ?assertEqual(#{}, guild_data_index_members:member_role_index(invalid_guild_data())),
    Data = #{
        <<"members">> => #{
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"10">>, <<"11">>]},
            2 => #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => [<<"11">>]}
        }
    },
    Index = guild_data_index_members:member_role_index(Data),
    ?assertEqual(#{1 => true}, maps:get(10, Index)),
    ?assertEqual(#{1 => true, 2 => true}, maps:get(11, Index)),
    ?assertEqual(
        #{},
        guild_data_index_members:member_role_index(#{
            <<"members">> => #{
                1 => #{<<"user">> => #{<<"id">> => <<"1">>}}
            }
        })
    ).

role_index_sync_test() ->
    Data0 = #{
        <<"members">> => #{
            3 => #{<<"user">> => #{<<"id">> => <<"3">>}, <<"roles">> => [<<"20">>]}
        }
    },
    Data1 = guild_data_index_members:put_member(
        #{<<"user">> => #{<<"id">> => <<"3">>}, <<"roles">> => [<<"30">>]}, Data0
    ),
    ?assertEqual(
        undefined, maps:get(20, guild_data_index_members:member_role_index(Data1), undefined)
    ),
    ?assertEqual(#{3 => true}, maps:get(30, guild_data_index_members:member_role_index(Data1))),
    Data2 = guild_data_index_members:remove_member(3, Data1),
    ?assertEqual(
        undefined, maps:get(30, guild_data_index_members:member_role_index(Data2), undefined)
    ).

invalid_guild_data() ->
    eqwalizer:dynamic_cast(not_a_map).
