%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_utils_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

extract_origin_test() ->
    ?assertEqual(
        <<"https://example.com">>,
        push_utils:extract_origin(<<"https://example.com/path/to/resource">>)
    ),
    ?assertEqual(
        <<"http://localhost:8080">>, push_utils:extract_origin(<<"http://localhost:8080/api">>)
    ),
    ?assertEqual(<<"invalid">>, push_utils:extract_origin(<<"invalid">>)).

get_default_avatar_url_test() ->
    Url = push_utils:get_default_avatar_url(<<"123">>),
    ?assert(is_binary(Url)),
    ?assertMatch(<<"http://localhost:8088/avatars/", _/binary>>, Url).

avatar_index_test() ->
    ?assertEqual(undefined, push_utils:avatar_index(<<"0">>)),
    ?assertEqual(1, push_utils:avatar_index(<<"1">>)),
    ?assertEqual(2, push_utils:avatar_index(<<"2">>)),
    ?assertEqual(0, push_utils:avatar_index(<<"6">>)),
    ?assertEqual(undefined, push_utils:avatar_index(<<"invalid">>)),
    ?assertEqual(undefined, push_utils:avatar_index(<<"001">>)).

wrap_avatar_index_test() ->
    ?assertEqual(0, push_utils:wrap_avatar_index(0)),
    ?assertEqual(1, push_utils:wrap_avatar_index(1)),
    ?assertEqual(0, push_utils:wrap_avatar_index(6)),
    ?assertEqual(1, push_utils:wrap_avatar_index(7)).

parse_timestamp_valid_test() ->
    ?assertEqual(123456789, push_utils:parse_timestamp(<<"123456789">>)),
    ?assertEqual(0, push_utils:parse_timestamp(<<"0">>)).

parse_timestamp_invalid_test() ->
    ?assertEqual(undefined, push_utils:parse_timestamp(<<"not_a_number">>)),
    ?assertEqual(undefined, push_utils:parse_timestamp(123)),
    ?assertEqual(undefined, push_utils:parse_timestamp(undefined)).

base64url_encode_test() ->
    Encoded = push_utils:base64url_encode(<<"test">>),
    ?assert(is_binary(Encoded)).

base64url_decode_test() ->
    Encoded = push_utils:base64url_encode(<<"test">>),
    ?assertEqual(<<"test">>, push_utils:base64url_decode(Encoded)).

hkdf_expand_test() ->
    IKM = crypto:strong_rand_bytes(32),
    Salt = crypto:strong_rand_bytes(16),
    Info = <<"test info">>,
    Result = push_utils:hkdf_expand(IKM, Salt, Info, 32),
    ?assertEqual(32, byte_size(Result)).

assert_vapid_pair_accepts_generated_pair_test() ->
    {Pub, Priv} = generate_vapid_pair(),
    ?assertEqual(
        ok,
        push_utils:assert_vapid_pair(
            push_utils:base64url_encode(Pub),
            push_utils:base64url_encode(Priv)
        )
    ).

assert_vapid_pair_rejects_mismatched_scalar_test() ->
    {Pub, _} = generate_vapid_pair(),
    {_, OtherPriv} = generate_vapid_pair(),
    ?assertError(
        {vapid_keys_mismatched, _},
        push_utils:assert_vapid_pair(
            push_utils:base64url_encode(Pub),
            push_utils:base64url_encode(OtherPriv)
        )
    ).

assert_vapid_pair_rejects_malformed_public_point_test() ->
    {Pub, Priv} = generate_vapid_pair(),
    ?assertError(
        {vapid_keys_malformed, 64, 32},
        push_utils:assert_vapid_pair(
            push_utils:base64url_encode(binary:part(Pub, 0, 64)),
            push_utils:base64url_encode(Priv)
        )
    ).

assert_vapid_pair_rejects_short_scalar_test() ->
    {Pub, Priv} = generate_vapid_pair(),
    ?assertError(
        {vapid_keys_malformed, 65, 31},
        push_utils:assert_vapid_pair(
            push_utils:base64url_encode(Pub),
            push_utils:base64url_encode(binary:part(Priv, 0, 31))
        )
    ).

generate_vapid_pair() ->
    case crypto:generate_key(ecdh, prime256v1) of
        {<<4, _:64/binary>> = Pub, <<_:32/binary>> = Priv} -> {Pub, Priv};
        _ -> generate_vapid_pair()
    end.
