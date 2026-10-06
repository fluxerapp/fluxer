%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_utils).
-typing([eqwalizer]).

-export([
    construct_avatar_url/2,
    construct_static_asset_url/1,
    get_default_avatar_url/1,
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
