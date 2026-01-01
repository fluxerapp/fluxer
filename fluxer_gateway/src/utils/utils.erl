%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(utils).
-import(type_conv, [to_integer/1]).
-export([
    binary_to_integer_safe/1,
    generate_session_id/0,
    generate_resume_token/0,
    hash_token/1,
    parse_status/1,
    safe_json_decode/1,
    check_user_data_differs/2,
    partial_user_fields/0,
    parse_iso8601_to_unix_ms/1
]).

binary_to_integer_safe(Bin) when is_binary(Bin) ->
    try
        binary_to_integer(Bin)
    catch
        _:_ ->
            try
                list_to_integer(binary_to_list(Bin))
            catch
                _:_ -> undefined
            end
    end;
binary_to_integer_safe(Int) when is_integer(Int) -> Int;
binary_to_integer_safe(_) ->
    undefined.

generate_session_id() ->
    Bytes = crypto:strong_rand_bytes(constants:random_session_bytes()),
    binary:encode_hex(Bytes).

generate_resume_token() ->
    Bytes = crypto:strong_rand_bytes(32),
    base64url:encode(Bytes).

hash_token(Token) ->
    crypto:hash(sha256, Token).

parse_status(Status) when is_binary(Status) ->
    constants:status_type_atom(Status);
parse_status(Status) when is_atom(Status) ->
    Status;
parse_status(_) ->
    online.

safe_json_decode(Bin) ->
    try
        jsx:decode(Bin, [{return_maps, true}])
    catch
        _:_ -> #{}
    end.

parse_iso8601_to_unix_ms(Binary) when is_binary(Binary) ->
    Pattern =
        <<"^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})(?:\\.(\\d{1,9}))?Z$">>,
    case re:run(Binary, Pattern, [{capture, [1, 2, 3, 4, 5, 6, 7], list}]) of
        {match, [YearBin, MonthBin, DayBin, HourBin, MinuteBin, SecondBin, FractionBin]} ->
            Year = to_integer(YearBin),
            Month = to_integer(MonthBin),
            Day = to_integer(DayBin),
            Hour = to_integer(HourBin),
            Minute = to_integer(MinuteBin),
            Second = to_integer(SecondBin),
            FractionMs = fractional_ms(FractionBin),
            case {Year, Month, Day, Hour, Minute, Second} of
                {Y, M, D, H, Min, S} when
                    is_integer(Y) and is_integer(M) and is_integer(D) and is_integer(H) and
                        is_integer(Min) and is_integer(S)
                ->
                    Seconds = calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H, Min, S}}),
                    Seconds * 1000 + FractionMs;
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end;
parse_iso8601_to_unix_ms(_) ->
    undefined.

fractional_ms(Fraction) when is_list(Fraction) ->
    Normalized =
        case length(Fraction) of
            Len when Len >= 3 -> lists:sublist(Fraction, 3);
            Len when Len > 0 -> Fraction ++ lists:duplicate(3 - Len, $0);
            _ -> "000"
        end,
    case Normalized of
        [] ->
            0;
        _ ->
            case catch list_to_integer(Normalized) of
                {'EXIT', _} -> 0;
                Value -> Value
            end
    end;
fractional_ms(_) ->
    0.

partial_user_fields() ->
    [
        <<"id">>,
        <<"username">>,
        <<"discriminator">>,
        <<"global_name">>,
        <<"avatar">>,
        <<"avatar_color">>,
        <<"bot">>,
        <<"system">>,
        <<"flags">>,
        <<"banner">>,
        <<"banner_color">>
    ].

check_user_data_differs(CurrentUserData, NewUserData) ->
    CheckedFields = partial_user_fields(),
    lists:any(
        fun(Field) ->
            CurrentValue = maps:get(Field, CurrentUserData, undefined),
            NewValue = maps:get(Field, NewUserData, undefined),
            CurrentValue =/= NewValue orelse
                (maps:is_key(Field, CurrentUserData) andalso not maps:is_key(Field, NewUserData))
        end,
        CheckedFields
    ).
