%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_unclaimed_account_utils).
-typing([eqwalizer]).

-export([parse_unclaimed_error/1]).

-spec parse_unclaimed_error(iodata() | term()) -> boolean().
parse_unclaimed_error(Body) when is_binary(Body) ->
    decode_unclaimed_error(Body);
parse_unclaimed_error(Body) when is_list(Body) ->
    case guild_voice_connection_normalize:normalize_optional_binary(Body) of
        Binary when is_binary(Binary) -> decode_unclaimed_error(Binary);
        undefined -> false
    end;
parse_unclaimed_error(_) ->
    false.

-spec decode_unclaimed_error(binary()) -> boolean().
decode_unclaimed_error(Body) ->
    try json:decode(Body) of
        Map when is_map(Map) -> has_unclaimed_error_code(Map);
        _ -> false
    catch
        _:_ -> false
    end.

-spec has_unclaimed_error_code(map()) -> boolean().
has_unclaimed_error_code(Map) ->
    case get_unclaimed_error_code(Map) of
        Code when is_binary(Code) -> is_voice_unclaimed_error_code(Code);
        _ -> false
    end.

-spec get_unclaimed_error_code(map()) -> binary() | undefined.
get_unclaimed_error_code(Map) when is_map(Map) ->
    case maps:get(<<"code">>, Map, undefined) of
        Code when is_binary(Code) ->
            Code;
        _ ->
            get_nested_unclaimed_error_code(Map)
    end.

-spec get_nested_unclaimed_error_code(map()) -> binary() | undefined.
get_nested_unclaimed_error_code(Map) ->
    case maps:get(<<"error">>, Map, undefined) of
        Error when is_map(Error) -> maps:get(<<"code">>, Error, undefined);
        _ -> undefined
    end.

-spec is_voice_unclaimed_error_code(binary()) -> boolean().
is_voice_unclaimed_error_code(Code) when is_binary(Code) ->
    lists:member(
        Code,
        [
            <<"UNCLAIMED_ACCOUNT_CANNOT_JOIN_ONE_ON_ONE_VOICE_CALLS">>,
            <<"UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS">>
        ]
    ).
