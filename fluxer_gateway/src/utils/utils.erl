%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(utils).
-typing([eqwalizer]).

-export([
    binary_to_integer_safe/1,
    generate_session_id/0,
    generate_resume_token/0,
    hash_token/1,
    parse_status/1,
    safe_json_decode/1,
    check_user_data_differs/2,
    parse_iso8601_to_unix_ms/1
]).

-spec binary_to_integer_safe(binary() | integer() | term()) -> integer() | undefined.
binary_to_integer_safe(Int) when is_integer(Int) ->
    Int;
binary_to_integer_safe(Bin) when is_binary(Bin) ->
    type_conv:to_integer(Bin);
binary_to_integer_safe(_) ->
    undefined.

-spec generate_session_id() -> binary().
generate_session_id() ->
    Bytes = crypto:strong_rand_bytes(constants:random_session_bytes()),
    binary:encode_hex(Bytes).

-spec generate_resume_token() -> binary().
generate_resume_token() ->
    Bytes = crypto:strong_rand_bytes(32),
    base64url:encode(Bytes).

-spec hash_token(binary()) -> binary().
hash_token(Token) ->
    crypto:hash(sha256, Token).

-spec parse_status(binary() | atom() | term()) -> atom().
parse_status(Status) when is_binary(Status) ->
    case constants:status_type_atom(Status) of
        Parsed when is_atom(Parsed) -> Parsed;
        _ -> online
    end;
parse_status(Status) when is_atom(Status) ->
    Status;
parse_status(_) ->
    online.

-spec safe_json_decode(binary()) -> map().
safe_json_decode(Bin) ->
    try json:decode(Bin) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    catch
        error:_ -> #{};
        throw:_ -> #{}
    end.

-define(GREGORIAN_SECONDS_TO_UNIX_EPOCH, 62167219200).

-spec parse_iso8601_to_unix_ms(binary() | term()) -> integer() | undefined.
parse_iso8601_to_unix_ms(Binary) when is_binary(Binary) ->
    Pattern =
        <<"^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})(?:\\.(\\d{1,9}))?(?:Z|[+-]00:?00)$">>,
    CaptureOpts = [{capture, [1, 2, 3, 4, 5, 6, 7], list}],
    case re:run(Binary, Pattern, CaptureOpts) of
        {match, [YS, MS, DS, HS, MiS, SS, FS]} ->
            convert_iso8601_parts(YS, MS, DS, HS, MiS, SS, FS);
        _ ->
            undefined
    end;
parse_iso8601_to_unix_ms(_) ->
    undefined.

-spec convert_iso8601_parts(list(), list(), list(), list(), list(), list(), list()) ->
    integer() | undefined.
convert_iso8601_parts(YearBin, MonthBin, DayBin, HourBin, MinuteBin, SecondBin, FractionBin) ->
    Y = type_conv:to_integer(YearBin),
    M = type_conv:to_integer(MonthBin),
    D = type_conv:to_integer(DayBin),
    H = type_conv:to_integer(HourBin),
    Min = type_conv:to_integer(MinuteBin),
    S = type_conv:to_integer(SecondBin),
    FractionMs = fractional_ms(FractionBin),
    datetime_to_unix_ms(Y, M, D, H, Min, S, FractionMs).

-spec datetime_to_unix_ms(term(), term(), term(), term(), term(), term(), non_neg_integer()) ->
    integer() | undefined.
datetime_to_unix_ms(Y, M, D, H, Min, S, FractionMs) when
    is_integer(Y),
    is_integer(M),
    is_integer(D),
    is_integer(H),
    is_integer(Min),
    is_integer(S)
->
    try calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H, Min, S}}) of
        Gregorian ->
            UnixSeconds = Gregorian - ?GREGORIAN_SECONDS_TO_UNIX_EPOCH,
            UnixSeconds * 1000 + FractionMs
    catch
        error:badarg -> undefined
    end;
datetime_to_unix_ms(_, _, _, _, _, _, _) ->
    undefined.

-spec fractional_ms(list()) -> non_neg_integer().
fractional_ms([]) ->
    0;
fractional_ms(Fraction) when is_list(Fraction) ->
    Normalized =
        case length(Fraction) of
            Len when Len >= 3 -> lists:sublist(Fraction, 3);
            Len when Len > 0 -> Fraction ++ lists:duplicate(3 - Len, $0);
            _ -> "000"
        end,
    try list_to_integer(Normalized) of
        Value -> Value
    catch
        error:badarg -> 0
    end;
fractional_ms(_) ->
    0.

-spec check_user_data_differs(map(), map()) -> boolean().
check_user_data_differs(CurrentUserData, NewUserData) ->
    NormalizedCurrentUserData = user_utils:normalize_user(CurrentUserData),
    NormalizedNewUserData = user_utils:normalize_user(NewUserData),
    CheckedFields = user_utils:partial_user_fields(),
    lists:any(
        fun(Field) ->
            user_field_differs(Field, NormalizedCurrentUserData, NormalizedNewUserData)
        end,
        CheckedFields
    ).

-spec user_field_differs(binary(), map(), map()) -> boolean().
user_field_differs(Field, CurrentUserData, NewUserData) ->
    case maps:is_key(Field, NewUserData) of
        false ->
            false;
        true ->
            CurrentValue = maps:get(Field, CurrentUserData, undefined),
            NewValue = maps:get(Field, NewUserData, undefined),
            CurrentValue =/= NewValue
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

binary_to_integer_safe_integer_test() ->
    ?assertEqual(42, binary_to_integer_safe(42)),
    ?assertEqual(0, binary_to_integer_safe(0)),
    ?assertEqual(-100, binary_to_integer_safe(-100)).

binary_to_integer_safe_binary_test() ->
    ?assertEqual(123, binary_to_integer_safe(<<"123">>)),
    ?assertEqual(0, binary_to_integer_safe(<<"0">>)),
    ?assertEqual(-456, binary_to_integer_safe(<<"-456">>)).

binary_to_integer_safe_invalid_test() ->
    ?assertEqual(undefined, binary_to_integer_safe(<<"not_a_number">>)),
    ?assertEqual(undefined, binary_to_integer_safe(<<"12.34">>)),
    ?assertEqual(undefined, binary_to_integer_safe(<<"">>)),
    ?assertEqual(undefined, binary_to_integer_safe(atom)),
    ?assertEqual(undefined, binary_to_integer_safe(#{})).

generate_session_id_test() ->
    SessionId = generate_session_id(),
    ?assert(is_binary(SessionId)),
    ?assertEqual(32, byte_size(SessionId)).

generate_resume_token_test() ->
    Token = generate_resume_token(),
    ?assert(is_binary(Token)),
    ?assert(byte_size(Token) > 0).

hash_token_test() ->
    Hash = hash_token(<<"test_token">>),
    ?assert(is_binary(Hash)),
    ?assertEqual(32, byte_size(Hash)).

parse_status_binary_test() ->
    ?assertEqual(online, parse_status(<<"online">>)),
    ?assertEqual(dnd, parse_status(<<"dnd">>)),
    ?assertEqual(idle, parse_status(<<"idle">>)),
    ?assertEqual(invisible, parse_status(<<"invisible">>)),
    ?assertEqual(offline, parse_status(<<"offline">>)).

parse_status_atom_test() ->
    ?assertEqual(online, parse_status(online)),
    ?assertEqual(dnd, parse_status(dnd)),
    ?assertEqual(idle, parse_status(idle)).

parse_status_default_test() ->
    ?assertEqual(online, parse_status(123)),
    ?assertEqual(online, parse_status(#{})).

safe_json_decode_valid_test() ->
    Result = safe_json_decode(<<"{\"key\": \"value\"}">>),
    ?assertEqual(#{<<"key">> => <<"value">>}, Result).

safe_json_decode_invalid_test() ->
    ?assertEqual(#{}, safe_json_decode(<<"not json">>)),
    ?assertEqual(#{}, safe_json_decode(<<"">>)).

parse_iso8601_to_unix_ms_valid_test() ->
    ?assertEqual(1705321845000, parse_iso8601_to_unix_ms(<<"2024-01-15T12:30:45Z">>)),
    ?assertEqual(0, parse_iso8601_to_unix_ms(<<"1970-01-01T00:00:00Z">>)).

parse_iso8601_to_unix_ms_with_fraction_test() ->
    ?assertEqual(1705321845123, parse_iso8601_to_unix_ms(<<"2024-01-15T12:30:45.123Z">>)).

parse_iso8601_to_unix_ms_past_expired_test() ->
    Past = parse_iso8601_to_unix_ms(<<"2020-01-01T00:00:00Z">>),
    ?assert(Past < erlang:system_time(millisecond)).

parse_iso8601_to_unix_ms_invalid_test() ->
    ?assertEqual(undefined, parse_iso8601_to_unix_ms(<<"invalid">>)),
    ?assertEqual(undefined, parse_iso8601_to_unix_ms(<<"2024-01-15">>)),
    ?assertEqual(undefined, parse_iso8601_to_unix_ms(123)).

check_user_data_differs_same_test() ->
    User = #{<<"id">> => <<"123">>, <<"username">> => <<"test">>},
    ?assertEqual(false, check_user_data_differs(User, User)).

check_user_data_differs_different_test() ->
    Current = #{<<"id">> => <<"123">>, <<"username">> => <<"test">>},
    New = #{<<"id">> => <<"123">>, <<"username">> => <<"changed">>},
    ?assertEqual(true, check_user_data_differs(Current, New)).

check_user_data_differs_missing_field_test() ->
    Current = #{<<"id">> => <<"123">>, <<"username">> => <<"test">>},
    New = #{<<"id">> => <<"123">>},
    ?assertEqual(false, check_user_data_differs(Current, New)).

check_user_data_differs_normalizes_equivalent_user_ids_test() ->
    Current = #{<<"id">> => 123, <<"username">> => <<"test">>},
    New = #{<<"id">> => <<"123">>, <<"username">> => <<"test">>},
    ?assertEqual(false, check_user_data_differs(Current, New)).

check_user_data_differs_null_field_test() ->
    Current = #{<<"id">> => <<"123">>, <<"username">> => <<"test">>},
    New = #{<<"username">> => null},
    ?assertEqual(true, check_user_data_differs(Current, New)).

-endif.
