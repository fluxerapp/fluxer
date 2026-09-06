%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(snowflake_id).
-typing([eqwalizer]).

-export([
    parse/1,
    parse_optional/1,
    parse_maybe/1,
    parse_list/1,
    first/1,
    filter/1,
    require/1,
    to_integer/1,
    to_binary/1,
    is_valid/1,
    equal/2,
    member/2,
    get/3
]).

-export_type([
    t/0,
    user_id/0,
    guild_id/0,
    channel_id/0,
    role_id/0,
    message_id/0
]).

-type t() :: pos_integer().
-type user_id() :: t().
-type guild_id() :: t().
-type channel_id() :: t().
-type role_id() :: t().
-type message_id() :: t().

-define(MAX_SNOWFLAKE, 16#7FFFFFFFFFFFFFFF).

-spec parse(term()) -> t().
parse(Value) when is_integer(Value), Value > 0, Value =< ?MAX_SNOWFLAKE ->
    Value;
parse(Value) when is_binary(Value) ->
    require_parsed(parse_binary(Value), Value);
parse(Value) when is_list(Value) ->
    require_parsed(parse_list_value(Value), Value);
parse(Value) ->
    erlang:error({invalid_snowflake, Value}).

-spec parse_optional(term()) -> t() | undefined.
parse_optional(null) ->
    undefined;
parse_optional(undefined) ->
    undefined;
parse_optional(Value) ->
    parse(Value).

-spec parse_maybe(term()) -> t() | undefined.
parse_maybe(Value) ->
    try parse_optional(Value) of
        Id -> Id
    catch
        error:{invalid_snowflake, _} -> undefined
    end.

-spec parse_list(term()) -> [t()].
parse_list(undefined) ->
    [];
parse_list(null) ->
    [];
parse_list(Values) when is_list(Values) ->
    [parse(Value) || Value <- Values];
parse_list(Values) ->
    erlang:error({invalid_snowflake_list, Values}).

-spec first([term()]) -> t() | undefined.
first([]) ->
    undefined;
first([Value | Rest]) ->
    case parse_optional(Value) of
        undefined -> first(Rest);
        Id -> Id
    end.

-spec filter(term()) -> {true, t()} | false.
filter(Value) ->
    case parse_maybe(Value) of
        undefined -> false;
        Id -> {true, Id}
    end.

-spec require(term()) -> t().
require(Value) ->
    parse(Value).

-spec to_integer(t()) -> pos_integer().
to_integer(Id) when is_integer(Id), Id > 0 ->
    Id.

-spec to_binary(t()) -> binary().
to_binary(Id) when is_integer(Id), Id > 0 ->
    integer_to_binary(Id).

-spec is_valid(term()) -> boolean().
is_valid(Value) ->
    try parse(Value) of
        _Id -> true
    catch
        error:{invalid_snowflake, _} -> false
    end.

-spec equal(t(), term()) -> boolean().
equal(Id, Value) when is_integer(Id), Id > 0 ->
    case parse_optional(Value) of
        Id -> true;
        _ -> false
    end.

-spec member(t(), [term()]) -> boolean().
member(Id, Values) when is_integer(Id), Id > 0 ->
    lists:any(fun(Value) -> equal(Id, Value) end, Values).

-spec get(t(), map(), term()) -> term().
get(Id, Map, Default) when is_integer(Id), Id > 0, is_map(Map) ->
    case maps:get(Id, Map, undefined) of
        undefined -> maps:get(to_binary(Id), Map, Default);
        Value -> Value
    end;
get(_Id, _Map, Default) ->
    Default.

-spec parse_binary(binary()) -> t() | undefined.
parse_binary(<<First, Rest/binary>> = Value) when First >= $1, First =< $9 ->
    case all_digits(Rest) of
        true -> bounded(binary_to_integer(Value));
        false -> undefined
    end;
parse_binary(_) ->
    undefined.

-spec parse_list_value([term()]) -> t() | undefined.
parse_list_value([First | Rest]) when is_integer(First), First >= $1, First =< $9 ->
    parse_digits_list(Rest, First - $0);
parse_list_value(_) ->
    undefined.

-spec parse_digits_list([term()], pos_integer()) -> t() | undefined.
parse_digits_list([], Acc) ->
    bounded(Acc);
parse_digits_list([Digit | Rest], Acc) when is_integer(Digit), Digit >= $0, Digit =< $9 ->
    parse_digits_list(Rest, Acc * 10 + Digit - $0);
parse_digits_list(_, _Acc) ->
    undefined.

-spec bounded(pos_integer()) -> t() | undefined.
bounded(Id) when Id =< ?MAX_SNOWFLAKE ->
    Id;
bounded(_) ->
    undefined.

-spec require_parsed(t() | undefined, term()) -> t().
require_parsed(Id, _Value) when is_integer(Id) ->
    Id;
require_parsed(undefined, Value) ->
    erlang:error({invalid_snowflake, Value}).

-spec all_digits(binary()) -> boolean().
all_digits(<<>>) ->
    true;
all_digits(<<Digit, Rest/binary>>) when Digit >= $0, Digit =< $9 ->
    all_digits(Rest);
all_digits(_) ->
    false.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_accepts_integer_binary_and_list_test() ->
    ?assertEqual(123, parse(123)),
    ?assertEqual(456, parse(<<"456">>)),
    ?assertEqual(789, parse("789")).

parse_rejects_non_canonical_values_test() ->
    ?assertError({invalid_snowflake, 0}, parse(0)),
    ?assertError({invalid_snowflake, <<"0">>}, parse(<<"0">>)),
    ?assertError({invalid_snowflake, -1}, parse(-1)),
    ?assertError({invalid_snowflake, <<"001">>}, parse(<<"001">>)),
    ?assertError({invalid_snowflake, <<"+1">>}, parse(<<"+1">>)),
    ?assertError({invalid_snowflake, <<"abc">>}, parse(<<"abc">>)).

parse_rejects_values_above_int64_test() ->
    ?assertEqual(9223372036854775807, parse(9223372036854775807)),
    ?assertEqual(9223372036854775807, parse(<<"9223372036854775807">>)),
    ?assertEqual(9223372036854775807, parse("9223372036854775807")),
    ?assertError({invalid_snowflake, 9223372036854775808}, parse(9223372036854775808)),
    ?assertError(
        {invalid_snowflake, <<"9223372036854775808">>}, parse(<<"9223372036854775808">>)
    ),
    ?assertError({invalid_snowflake, "9223372036854775808"}, parse("9223372036854775808")).

member_handles_mixed_edge_values_test() ->
    ?assertEqual(true, member(123, [<<"123">>, 456])),
    ?assertError({invalid_snowflake, <<"00123">>}, member(123, [<<"00123">>, 456])).

first_returns_first_valid_snowflake_test() ->
    ?assertEqual(123, first([undefined, <<"123">>, 456])),
    ?assertError({invalid_snowflake, <<"001">>}, first([undefined, <<"001">>, <<"123">>])),
    ?assertError({invalid_snowflake, <<"0">>}, first([undefined, <<"0">>])).

get_prefers_integer_key_test() ->
    Map = #{123 => int, <<"123">> => bin},
    ?assertEqual(int, get(123, Map, missing)).

get_falls_back_to_binary_key_test() ->
    ?assertEqual(bin, get(123, #{<<"123">> => bin}, missing)).

-endif.
