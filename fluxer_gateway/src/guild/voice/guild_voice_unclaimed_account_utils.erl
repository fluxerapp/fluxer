%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_unclaimed_account_utils).
-typing([eqwalizer]).

-export([voice_account_error/1]).
-export_type([voice_account_error/0]).

-type voice_account_error() ::
    voice_unclaimed_account | voice_account_verification_required | none.

-spec voice_account_error(iodata() | term()) -> voice_account_error().
voice_account_error(Body) when is_binary(Body) ->
    decode_voice_account_error(Body);
voice_account_error(Body) when is_list(Body) ->
    case guild_voice_connection_normalize:normalize_optional_binary(Body) of
        Binary when is_binary(Binary) -> decode_voice_account_error(Binary);
        undefined -> none
    end;
voice_account_error(_) ->
    none.

-spec decode_voice_account_error(binary()) -> voice_account_error().
decode_voice_account_error(Body) ->
    try json:decode(Body) of
        Map when is_map(Map) -> classify_error_code(get_unclaimed_error_code(Map));
        _ -> none
    catch
        _:_ -> none
    end.

-spec classify_error_code(binary() | undefined) -> voice_account_error().
classify_error_code(<<"ACCOUNT_VERIFICATION_REQUIRED">>) ->
    voice_account_verification_required;
classify_error_code(Code) when is_binary(Code) ->
    case is_voice_unclaimed_error_code(Code) of
        true -> voice_unclaimed_account;
        false -> none
    end;
classify_error_code(_) ->
    none.

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
