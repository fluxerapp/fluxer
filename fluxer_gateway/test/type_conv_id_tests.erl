%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(type_conv_id_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

to_snowflake_with_valid_values_test() ->
    ?assertEqual(123, snowflake_id:parse(123)),
    ?assertEqual(456, snowflake_id:parse(<<"456">>)),
    ?assertEqual(789, snowflake_id:parse("789")).

to_snowflake_rejects_malformed_values_test() ->
    ?assertError({invalid_snowflake, 0}, snowflake_id:parse(0)),
    ?assertError({invalid_snowflake, <<"0">>}, snowflake_id:parse(<<"0">>)),
    ?assertError({invalid_snowflake, -1}, snowflake_id:parse(-1)),
    ?assertError({invalid_snowflake, <<"-1">>}, snowflake_id:parse(<<"-1">>)),
    ?assertError({invalid_snowflake, <<"+1">>}, snowflake_id:parse(<<"+1">>)),
    ?assertError({invalid_snowflake, <<"001">>}, snowflake_id:parse(<<"001">>)),
    ?assertError({invalid_snowflake, "001"}, snowflake_id:parse("001")),
    ?assertError({invalid_snowflake, <<"">>}, snowflake_id:parse(<<"">>)),
    ?assertError({invalid_snowflake, <<"abc">>}, snowflake_id:parse(<<"abc">>)),
    ?assertError({invalid_snowflake, 12.34}, snowflake_id:parse(12.34)).

to_snowflake_rejects_values_above_int64_test() ->
    ?assertEqual(9223372036854775807, snowflake_id:parse(9223372036854775807)),
    ?assertEqual(9223372036854775807, snowflake_id:parse(<<"9223372036854775807">>)),
    ?assertError(
        {invalid_snowflake, 9223372036854775808}, snowflake_id:parse(9223372036854775808)
    ),
    ?assertError(
        {invalid_snowflake, <<"9223372036854775808">>},
        snowflake_id:parse(<<"9223372036854775808">>)
    ),
    ?assertError(
        {invalid_snowflake, "9223372036854775808"}, snowflake_id:parse("9223372036854775808")
    ).

extract_id_with_atom_key_valid_test() ->
    ?assertEqual(123, type_conv:extract_id(#{user_id => 123}, user_id)),
    ?assertEqual(456, type_conv:extract_id(#{user_id => <<"456">>}, user_id)),
    ?assertEqual(789, type_conv:extract_id(#{user_id => "789"}, user_id)).

extract_id_with_atom_key_rejects_malformed_ids_test() ->
    ?assertEqual(undefined, type_conv:extract_id(#{user_id => 0}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id(#{user_id => -1}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id(#{user_id => <<"-1">>}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id(#{user_id => <<"001">>}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id(#{user_id => <<"+1">>}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id(#{user_id => "001"}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id(#{user_id => "invalid"}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id(#{user_id => [1, 2, 3]}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id(#{user_id => 9223372036854775808}, user_id)),
    ?assertEqual(
        undefined, type_conv:extract_id(#{user_id => <<"9223372036854775808">>}, user_id)
    ).

extract_id_with_atom_key_missing_or_invalid_test() ->
    ?assertEqual(undefined, type_conv:extract_id(#{other_field => 999}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id(#{user_id => undefined}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id(#{user_id => #{}}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id(not_a_map, user_id)).

extract_id_with_binary_key_valid_test() ->
    ?assertEqual(123, type_conv:extract_id(#{<<"user_id">> => 123}, <<"user_id">>)),
    ?assertEqual(456, type_conv:extract_id(#{<<"user_id">> => <<"456">>}, <<"user_id">>)),
    ?assertEqual(789, type_conv:extract_id(#{<<"user_id">> => "789"}, <<"user_id">>)).

extract_id_with_binary_key_rejects_malformed_ids_test() ->
    ?assertEqual(undefined, type_conv:extract_id(#{<<"user_id">> => <<"0">>}, <<"user_id">>)),
    ?assertEqual(undefined, type_conv:extract_id(#{<<"user_id">> => -1}, <<"user_id">>)),
    ?assertEqual(undefined, type_conv:extract_id(#{<<"user_id">> => <<"-1">>}, <<"user_id">>)),
    ?assertEqual(undefined, type_conv:extract_id(#{<<"user_id">> => <<"001">>}, <<"user_id">>)),
    ?assertEqual(undefined, type_conv:extract_id(#{<<"user_id">> => 12.34}, <<"user_id">>)).

extract_id_with_invalid_key_type_test() ->
    Map = #{user_id => 123},
    ?assertEqual(undefined, type_conv:extract_id(Map, 123)),
    ?assertEqual(undefined, type_conv:extract_id(Map, "user_id")),
    ?assertEqual(undefined, type_conv:extract_id(Map, {user_id})),
    ?assertEqual(undefined, type_conv:extract_id(Map, [user_id])).

extract_id_required_returns_snowflake_or_undefined_test() ->
    ?assertEqual(123, type_conv:extract_id_required(#{user_id => 123}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id_required(#{user_id => <<"0">>}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id_required(#{user_id => -1}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id_required(#{user_id => <<"001">>}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id_required(#{other => value}, user_id)),
    ?assertEqual(undefined, type_conv:extract_id_required(not_a_map, user_id)).
