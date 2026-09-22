%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_utils).
-typing([eqwalizer]).

-export([
    construct_avatar_url/2,
    construct_static_asset_url/1,
    get_default_avatar_url/1,
    extract_origin/1,
    generate_vapid_token/3,
    assert_vapid_pair/2,
    generate_jwt_from_pem/3,
    base64url_encode/1,
    base64url_decode/1,
    encrypt_payload/4,
    decode_subscription_key/1,
    hkdf_expand/4,
    hkdf_expand_loop/6,
    parse_timestamp/1,
    normalize_binary/1,
    normalize_binary/2,
    avatar_index/1,
    wrap_avatar_index/1
]).

-spec construct_avatar_url(binary(), binary()) -> binary().
construct_avatar_url(UserId, Hash) ->
    MediaProxyBin = media_proxy_endpoint_binary(),
    iolist_to_binary([
        MediaProxyBin,
        <<"/avatars/">>,
        UserId,
        <<"/">>,
        Hash,
        <<".png">>
    ]).

-spec get_default_avatar_url(binary() | undefined) -> binary().
get_default_avatar_url(UserId) ->
    Index = default_avatar_index(avatar_index(UserId)),
    construct_static_asset_url([
        <<"avatars/">>,
        integer_to_binary(Index),
        <<".png">>
    ]).

-spec construct_static_asset_url(iodata()) -> binary().
construct_static_asset_url(Path) ->
    StaticCdnBin = static_cdn_endpoint_binary(),
    iolist_to_binary([StaticCdnBin, <<"/">>, Path]).

-spec avatar_index(term()) -> non_neg_integer() | undefined.
avatar_index(UserId) ->
    case snowflake_id:parse_maybe(UserId) of
        undefined -> undefined;
        Value -> wrap_avatar_index(Value)
    end.

-spec default_avatar_index(non_neg_integer() | undefined) -> non_neg_integer().
default_avatar_index(undefined) -> 0;
default_avatar_index(Index) -> Index.

-spec wrap_avatar_index(non_neg_integer()) -> non_neg_integer().
wrap_avatar_index(Value) ->
    case Value rem 6 of
        Index when Index >= 0 -> Index;
        _ -> 0
    end.

-spec media_proxy_endpoint_binary() -> binary().
media_proxy_endpoint_binary() ->
    case fluxer_gateway_env:get(media_proxy_endpoint) of
        undefined ->
            erlang:error({missing_config, media_proxy_endpoint});
        Endpoint ->
            strip_trailing_slashes(value_to_binary(Endpoint))
    end.

-spec static_cdn_endpoint_binary() -> binary().
static_cdn_endpoint_binary() ->
    case fluxer_gateway_env:get(static_cdn_endpoint) of
        undefined ->
            erlang:error({missing_config, static_cdn_endpoint});
        Endpoint ->
            strip_trailing_slashes(value_to_binary(Endpoint))
    end.

-spec value_to_binary(term()) -> binary().
value_to_binary(Value) when is_binary(Value) ->
    Value;
value_to_binary(Value) when is_list(Value) ->
    case type_conv:to_binary(Value) of
        Bin when is_binary(Bin) -> Bin;
        undefined -> erlang:error({invalid_binary_value, Value})
    end;
value_to_binary(Value) ->
    erlang:error({invalid_binary_value, Value}).

-spec strip_trailing_slashes(binary()) -> binary().
strip_trailing_slashes(<<>>) ->
    <<>>;
strip_trailing_slashes(Value) ->
    Size = byte_size(Value),
    case binary:part(Value, Size - 1, 1) of
        <<"/">> -> strip_trailing_slashes(binary:part(Value, 0, Size - 1));
        _ -> Value
    end.

-spec extract_origin(binary()) -> binary().
extract_origin(Url) ->
    case binary:split(Url, <<"://">>) of
        [Protocol, Rest] ->
            extract_origin_host(Protocol, Rest, Url);
        _ ->
            Url
    end.

-spec extract_origin_host(binary(), binary(), binary()) -> binary().
extract_origin_host(Protocol, Rest, Url) ->
    case binary:split(Rest, <<"/">>) of
        [Host | _] -> <<Protocol/binary, "://", Host/binary>>;
        _ -> Url
    end.

-spec generate_vapid_token(map(), binary(), binary()) -> binary().
generate_vapid_token(Claims, PublicKeyB64Url, PrivateKeyB64Url) ->
    try
        ensure_crypto_started(),
        PrivRaw = decode_or_error(PrivateKeyB64Url, invalid_private_key),
        PubRaw = decode_or_error(PublicKeyB64Url, invalid_public_key),
        JWK = build_ec_jwk(PrivRaw, PubRaw),
        Header = #{<<"alg">> => <<"ES256">>, <<"typ">> => <<"JWT">>},
        sign_and_compact(JWK, Header, Claims)
    catch
        C:R:_Stack ->
            erlang:error({vapid_token_generation_failed, C, R})
    end.

-spec ensure_crypto_started() -> ok.
ensure_crypto_started() ->
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(public_key),
    {ok, _} = application:ensure_all_started(jose),
    ok.

-spec decode_or_error(binary(), atom()) -> binary().
decode_or_error(B64Url, ErrorAtom) ->
    case base64url_decode(B64Url) of
        error -> erlang:error(ErrorAtom);
        Decoded -> Decoded
    end.

-spec build_ec_jwk(binary(), binary()) -> term().
build_ec_jwk(PrivRaw, PubRaw) ->
    <<4, X:32/binary, Y:32/binary>> = PubRaw,
    JWKMap = #{
        <<"kty">> => <<"EC">>,
        <<"crv">> => <<"P-256">>,
        <<"d">> => base64url_encode(PrivRaw),
        <<"x">> => base64url_encode(X),
        <<"y">> => base64url_encode(Y)
    },
    unwrap_jwk(jose_jwk:from_map(JWKMap)).

-spec unwrap_jwk(term()) -> term().
unwrap_jwk({JW, _Fields}) -> JW;
unwrap_jwk(JW) -> JW.

-spec assert_vapid_pair(binary(), binary()) -> ok.
assert_vapid_pair(PublicKeyB64Url, PrivateKeyB64Url) ->
    ensure_crypto_started(),
    PubRaw = decode_or_error(PublicKeyB64Url, invalid_public_key),
    PrivRaw = decode_or_error(PrivateKeyB64Url, invalid_private_key),
    case {PubRaw, PrivRaw} of
        {<<4, _:64/binary>>, <<_:32/binary>>} ->
            assert_vapid_scalar_derives_point(PublicKeyB64Url, PubRaw, PrivRaw);
        _ ->
            erlang:error({vapid_keys_malformed, byte_size(PubRaw), byte_size(PrivRaw)})
    end.

-spec assert_vapid_scalar_derives_point(binary(), binary(), binary()) -> ok.
assert_vapid_scalar_derives_point(PublicKeyB64Url, PubRaw, PrivRaw) ->
    Derived =
        try crypto:generate_key(ecdh, prime256v1, PrivRaw) of
            {Point, _} -> Point
        catch
            _:_ -> undefined
        end,
    case Derived of
        PubRaw -> ok;
        _ -> erlang:error({vapid_keys_mismatched, PublicKeyB64Url})
    end.

-spec sign_and_compact(term(), map(), map()) -> binary().
sign_and_compact(JWK, Header, Claims) ->
    JWS = jose_jwt:sign(JWK, Header, Claims),
    case jose_jws:compact(JWS) of
        {_Meta, Bin} when is_binary(Bin) -> Bin;
        Other -> erlang:error({unexpected_compact_return, Other})
    end.

-spec generate_jwt_from_pem(binary(), map(), map()) -> {ok, binary()} | {error, term()}.
generate_jwt_from_pem(Pem, Header, Claims) when
    is_binary(Pem), is_map(Header), is_map(Claims)
->
    try
        {ok, _} = application:ensure_all_started(crypto),
        {ok, _} = application:ensure_all_started(public_key),
        {ok, _} = application:ensure_all_started(jose),
        JWK0 = jose_jwk:from_pem(Pem),
        JWK =
            case JWK0 of
                {JW, _Fields} -> JW;
                JW -> JW
            end,
        JWS = jose_jwt:sign(JWK, Header, Claims),
        Compact0 = jose_jws:compact(JWS),
        CompactBin =
            case Compact0 of
                {_Meta, Bin} when is_binary(Bin) -> Bin;
                Other -> erlang:error({unexpected_compact_return, Other})
            end,
        {ok, CompactBin}
    catch
        C:R:Stack ->
            logger:error(
                "Push: JWT signing from PEM failed",
                #{class => C, reason => R, stack => Stack}
            ),
            {error, jwt_signing_failed}
    end.

-spec base64url_encode(binary()) -> binary().
base64url_encode(Data) ->
    jose_base64url:encode(Data).

-spec base64url_decode(binary()) -> binary() | error.
base64url_decode(Data) ->
    case jose_base64url:decode(Data) of
        {ok, Decoded} -> Decoded;
        error -> error
    end.

-spec encrypt_payload(binary(), binary(), binary(), non_neg_integer()) ->
    {ok, binary()} | {error, term()}.
encrypt_payload(Message, PeerPubB64, AuthSecretB64, RecordSize0) ->
    try
        PeerPub = decode_subscription_key(PeerPubB64),
        AuthSecret = decode_subscription_key(AuthSecretB64),
        RecordSize =
            case RecordSize0 of
                0 -> 4096;
                _ -> RecordSize0
            end,
        Salt = crypto:strong_rand_bytes(16),
        {LocalPub, LocalPriv} = generate_local_ecdh_key(),
        <<4, _/binary>> = PeerPub,
        Keys = derive_encryption_keys(PeerPub, LocalPub, LocalPriv, AuthSecret, Salt),
        encrypt_and_build_body(Message, Salt, RecordSize, LocalPub, Keys)
    catch
        C:R:Stack ->
            logger:error(
                "Push: encrypt_payload failed",
                #{class => C, reason => R, stack => Stack}
            ),
            {error, encryption_failed}
    end.

-spec generate_local_ecdh_key() -> {binary(), binary()}.
generate_local_ecdh_key() ->
    case crypto:generate_key(ecdh, prime256v1) of
        {LocalPub, LocalPriv} when is_binary(LocalPub), is_binary(LocalPriv) ->
            {LocalPub, LocalPriv};
        Other ->
            erlang:error({invalid_ecdh_key, Other})
    end.

-spec derive_encryption_keys(binary(), binary(), binary(), binary(), binary()) ->
    {binary(), binary()}.
derive_encryption_keys(PeerPub, LocalPub, LocalPriv, AuthSecret, Salt) ->
    Secret = crypto:compute_key(ecdh, PeerPub, LocalPriv, prime256v1),
    PRKInfo = <<"WebPush: info", 0, PeerPub/binary, LocalPub/binary>>,
    IKM = hkdf_expand(Secret, AuthSecret, PRKInfo, 32),
    CEK = hkdf_expand(IKM, Salt, <<"Content-Encoding: aes128gcm", 0>>, 16),
    Nonce = hkdf_expand(IKM, Salt, <<"Content-Encoding: nonce", 0>>, 12),
    {CEK, Nonce}.

-spec encrypt_and_build_body(
    binary(), binary(), non_neg_integer(), binary(), {binary(), binary()}
) -> {ok, binary()} | {error, term()}.
encrypt_and_build_body(Message, Salt, RecordSize, LocalPub, {CEK, Nonce}) ->
    HeaderLen = 16 + 4 + 1 + byte_size(LocalPub),
    RecordLen = RecordSize - 16,
    Data0 = <<Message/binary, 16#02>>,
    Required = RecordLen - HeaderLen,
    case byte_size(Data0) > Required of
        true ->
            {error, max_pad_exceeded};
        false ->
            Data = pad_data(Data0, Required),
            {Cipher, Tag} = crypto:crypto_one_time_aead(
                aes_gcm, CEK, Nonce, Data, <<>>, 16, true
            ),
            Ciphertext = <<Cipher/binary, Tag/binary>>,
            Body =
                <<Salt/binary, RecordSize:32/big-unsigned-integer, (byte_size(LocalPub)):8,
                    LocalPub/binary, Ciphertext/binary>>,
            {ok, Body}
    end.

-spec pad_data(binary(), non_neg_integer()) -> binary().
pad_data(Data0, Required) ->
    PadLen = Required - byte_size(Data0),
    Padding =
        case PadLen of
            0 -> <<>>;
            _ -> binary:copy(<<0>>, PadLen)
        end,
    <<Data0/binary, Padding/binary>>.

-spec decode_subscription_key(binary()) -> binary().
decode_subscription_key(B64) when is_binary(B64) ->
    Padded =
        case byte_size(B64) rem 4 of
            0 -> B64;
            Rem -> <<B64/binary, (binary:copy(<<"=">>, 4 - Rem))/binary>>
        end,
    case jose_base64url:decode(Padded) of
        {ok, Decoded} ->
            Decoded;
        _ ->
            decode_base64_subscription_key(Padded)
    end.

-spec decode_base64_subscription_key(binary()) -> binary().
decode_base64_subscription_key(Padded) ->
    try base64:decode(Padded) of
        Decoded when is_binary(Decoded) -> Decoded
    catch
        _:_ -> erlang:error(decode_key_error)
    end.

-spec hkdf_expand(binary(), binary(), binary(), pos_integer()) -> binary().
hkdf_expand(IKM, Salt, Info, Length) ->
    PRK = crypto:mac(hmac, sha256, Salt, IKM),
    hkdf_expand_loop(PRK, Info, Length, 1, <<>>, <<>>).

-spec hkdf_expand_loop(binary(), binary(), pos_integer(), pos_integer(), binary(), binary()) ->
    binary().
hkdf_expand_loop(_PRK, _Info, Length, _I, _Tprev, Acc) when byte_size(Acc) >= Length ->
    binary:part(Acc, 0, Length);
hkdf_expand_loop(PRK, Info, Length, I, Tprev, Acc) ->
    T = crypto:mac(hmac, sha256, PRK, <<Tprev/binary, Info/binary, I:8/integer>>),
    hkdf_expand_loop(PRK, Info, Length, I + 1, T, <<Acc/binary, T/binary>>).

-spec parse_timestamp(binary() | term()) -> integer() | undefined.
parse_timestamp(Str) when is_binary(Str) ->
    try
        binary_to_integer(Str)
    catch
        _:_ -> undefined
    end;
parse_timestamp(_) ->
    undefined.

-spec normalize_binary(term()) -> binary() | undefined.
normalize_binary(Value) when is_binary(Value) -> Value;
normalize_binary(Value) when is_list(Value) -> type_conv:to_binary(Value);
normalize_binary(_) -> undefined.

-spec normalize_binary
    (term(), binary()) -> binary();
    (term(), undefined) -> binary() | undefined.
normalize_binary(Value, _Default) when is_binary(Value) -> Value;
normalize_binary(Value, Default) when is_list(Value) ->
    case type_conv:to_binary(Value) of
        Bin when is_binary(Bin) -> Bin;
        undefined -> Default
    end;
normalize_binary(null, Default) ->
    Default;
normalize_binary(undefined, Default) ->
    Default;
normalize_binary(_, Default) ->
    Default.
