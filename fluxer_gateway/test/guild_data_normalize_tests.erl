%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_normalize_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

snowflake_uses_fluxer_response_shape_test() ->
    ?assertEqual(123, snowflake_id:parse(<<"123">>)),
    ?assertEqual(123, snowflake_id:parse(123)),
    ?assertError({invalid_snowflake, <<"0">>}, snowflake_id:parse(<<"0">>)),
    ?assertError({invalid_snowflake, 0}, snowflake_id:parse(0)),
    ?assertError({invalid_snowflake, <<"001">>}, snowflake_id:parse(<<"001">>)),
    ?assertError({invalid_snowflake, <<"+1">>}, snowflake_id:parse(<<"+1">>)),
    ?assertError({invalid_snowflake, <<"-1">>}, snowflake_id:parse(<<"-1">>)),
    ?assertError({invalid_snowflake, null}, snowflake_id:parse(null)).

permission_accepts_unsigned_decimal_strings_test() ->
    ?assertEqual(0, bitset:parse(<<"0">>)),
    ?assertEqual(1, bitset:parse(<<"001">>)),
    ?assertEqual(42, bitset:parse(42)),
    ?assertError({invalid_bitset, <<"+1">>}, bitset:parse(<<"+1">>)),
    ?assertError({invalid_bitset, <<"-1">>}, bitset:parse(<<"-1">>)).

member_role_ids_are_integer_keyed_in_memory_test() ->
    Member = normalized_member(#{
        <<"user">> => #{<<"id">> => <<"42">>},
        <<"roles">> => [<<"10">>, 20]
    }),
    ?assertEqual(42, maps:get(<<"id">>, maps:get(<<"user">>, Member))),
    ?assertEqual([10, 20], maps:get(<<"roles">>, Member)).

member_role_ids_reject_invalid_snowflakes_test() ->
    ?assertError(
        {invalid_snowflake, <<"001">>},
        normalized_member(#{<<"user">> => #{<<"id">> => <<"42">>}, <<"roles">> => [<<"001">>]})
    ).

role_permissions_are_integer_in_memory_test() ->
    Role = normalized_role(#{
        <<"id">> => <<"55">>,
        <<"permissions">> => <<"1024">>
    }),
    ?assertEqual(55, maps:get(<<"id">>, Role)),
    ?assertEqual(1024, maps:get(<<"permissions">>, Role)).

int_fields_follow_fluxer_non_negative_int32_schema_test() ->
    Guild = #{
        <<"id">> => <<"10">>,
        <<"owner_id">> => <<"20">>,
        <<"member_count">> => <<"0">>,
        <<"online_count">> => <<"-1">>,
        <<"banner_width">> => -1
    },
    Normalized = maps:get(<<"guild">>, normalized_guild_data(#{<<"guild">> => Guild})),
    ?assertEqual(0, maps:get(<<"member_count">>, Normalized)),
    ?assertNot(maps:is_key(<<"online_count">>, Normalized)),
    ?assertNot(maps:is_key(<<"banner_width">>, Normalized)).

guild_normalizes_only_fluxer_response_snowflakes_test() ->
    Guild = #{
        <<"id">> => <<"10">>,
        <<"owner_id">> => <<"20">>,
        <<"system_channel_id">> => null,
        <<"rules_channel_id">> => <<"30">>,
        <<"permissions">> => <<"8">>
    },
    ?assertEqual(
        #{
            <<"id">> => 10,
            <<"owner_id">> => 20,
            <<"system_channel_id">> => null,
            <<"rules_channel_id">> => 30,
            <<"permissions">> => 8
        },
        maps:get(<<"guild">>, normalized_guild_data(#{<<"guild">> => Guild}))
    ).

guild_rejects_invalid_snowflake_fields_test() ->
    Guild = #{
        <<"id">> => <<"10">>,
        <<"owner_id">> => <<"20">>,
        <<"afk_channel_id">> => <<"invalid">>
    },
    ?assertError(
        {invalid_snowflake, <<"invalid">>}, normalized_guild_data(#{<<"guild">> => Guild})
    ).

channel_normalizes_required_optional_and_nullable_ids_test() ->
    Channel = normalized_channel(#{
        <<"id">> => <<"100">>,
        <<"guild_id">> => null,
        <<"owner_id">> => null,
        <<"parent_id">> => <<"200">>,
        <<"last_message_id">> => <<"300">>
    }),
    ?assertEqual(100, maps:get(<<"id">>, Channel)),
    ?assertNot(maps:is_key(<<"guild_id">>, Channel)),
    ?assertEqual(null, maps:get(<<"owner_id">>, Channel)),
    ?assertEqual(200, maps:get(<<"parent_id">>, Channel)),
    ?assertEqual(300, maps:get(<<"last_message_id">>, Channel)).

channel_rejects_invalid_nullable_snowflake_test() ->
    ?assertError(
        {invalid_snowflake, <<"001">>},
        normalized_channel(#{<<"id">> => <<"100">>, <<"last_message_id">> => <<"001">>})
    ).

channel_rejects_invalid_permission_overwrites_test() ->
    ?assertError(
        {invalid_snowflake, <<"011">>},
        normalized_channel(#{
            <<"id">> => <<"100">>,
            <<"permission_overwrites">> => [
                #{
                    <<"id">> => <<"011">>,
                    <<"type">> => 0,
                    <<"allow">> => <<"1">>,
                    <<"deny">> => <<"0">>
                }
            ]
        })
    ),
    ?assertError(
        {invalid_bitset, <<"bad">>},
        normalized_channel(#{
            <<"id">> => <<"100">>,
            <<"permission_overwrites">> => [
                #{
                    <<"id">> => <<"12">>,
                    <<"type">> => 0,
                    <<"allow">> => <<"bad">>,
                    <<"deny">> => <<"0">>
                }
            ]
        })
    ).

channel_filters_non_permission_overwrite_schema_test() ->
    Channel = normalized_channel(#{
        <<"id">> => <<"100">>,
        <<"permission_overwrites">> => [
            #{
                <<"id">> => <<"10">>,
                <<"type">> => 0,
                <<"allow">> => <<"1">>,
                <<"deny">> => <<"0">>
            },
            #{<<"type">> => 0, <<"allow">> => <<"1">>, <<"deny">> => <<"0">>},
            #{<<"id">> => <<"13">>, <<"type">> => 0, <<"allow">> => <<"1">>},
            #{<<"id">> => <<"14">>, <<"type">> => 0, <<"deny">> => <<"0">>},
            #{
                <<"id">> => <<"15">>,
                <<"type">> => 2,
                <<"allow">> => <<"1">>,
                <<"deny">> => <<"0">>
            }
        ]
    }),
    ?assertEqual(
        [
            #{<<"id">> => 10, <<"type">> => 0, <<"allow">> => 1, <<"deny">> => 0}
        ],
        maps:get(<<"permission_overwrites">>, Channel)
    ).

channel_overwrites_keep_only_fluxer_schema_fields_test() ->
    Channel = normalized_channel(#{
        <<"id">> => <<"100">>,
        <<"permission_overwrites">> => [
            #{
                <<"id">> => <<"10">>,
                <<"type">> => 1,
                <<"allow">> => <<"2048">>,
                <<"deny">> => <<"0">>,
                <<"extra">> => <<"drop">>
            }
        ]
    }),
    ?assertEqual(
        [
            #{<<"id">> => 10, <<"type">> => 1, <<"allow">> => 2048, <<"deny">> => 0}
        ],
        maps:get(<<"permission_overwrites">>, Channel)
    ).

nicks_are_integer_keyed_in_memory_test() ->
    Channel = normalized_channel(#{
        <<"id">> => <<"100">>,
        <<"nicks">> => #{<<"10">> => <<"Ada">>}
    }),
    ?assertEqual(#{10 => <<"Ada">>}, maps:get(<<"nicks">>, Channel)).

nicks_reject_invalid_snowflake_keys_test() ->
    ?assertError(
        {invalid_snowflake, <<"bad">>},
        normalized_channel(#{
            <<"id">> => <<"100">>, <<"nicks">> => #{<<"bad">> => <<"ignored">>}
        })
    ).

normalized_member(Member) ->
    #{} = guild_data_normalize:member(Member).

normalized_role(Role) ->
    #{} = guild_data_normalize:role(Role).

normalized_guild_data(Data) ->
    #{} = guild_data_normalize:guild_data(Data).

normalized_channel(Channel) ->
    #{} = guild_data_normalize:channel(Channel).
