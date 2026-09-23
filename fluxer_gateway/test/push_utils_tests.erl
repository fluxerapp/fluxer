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

base64url_encode_test() ->
    Encoded = push_utils:base64url_encode(<<"test">>),
    ?assert(is_binary(Encoded)).

base64url_decode_test() ->
    Encoded = push_utils:base64url_encode(<<"test">>),
    ?assertEqual(<<"test">>, push_utils:base64url_decode(Encoded)).

plaintext_budget_test() ->
    ?assertEqual(2713, push_utils:plaintext_budget(2816)),
    ?assertEqual(3993, push_utils:plaintext_budget(4096)),
    ?assertEqual(0, push_utils:plaintext_budget(1)).

encrypt_payload_fills_the_record_at_the_budget_test() ->
    RecordSize = push_sender_retry:initial_record_size(),
    Budget = push_utils:plaintext_budget(RecordSize),
    {PeerPub, _PeerPriv} = crypto:generate_key(ecdh, prime256v1),
    P256dh = push_utils:base64url_encode(PeerPub),
    Auth = push_utils:base64url_encode(crypto:strong_rand_bytes(16)),
    {ok, Body} = push_utils:encrypt_payload(
        binary:copy(<<"x">>, Budget), P256dh, Auth, RecordSize
    ),
    ?assertEqual(RecordSize, byte_size(Body)),
    ?assertEqual(
        {error, max_pad_exceeded},
        push_utils:encrypt_payload(
            binary:copy(<<"x">>, Budget + 1), P256dh, Auth, RecordSize
        )
    ).

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
