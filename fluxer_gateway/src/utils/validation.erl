%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(validation).
-typing([eqwalizer]).

-export([
    validate_snowflake/1,
    validate_snowflake/2,
    validate_optional_snowflake/1,
    validate_snowflake_list/1,
    validate_snowflake_list/2,
    snowflake_or_throw/2,
    snowflake_list_or_throw/2
]).

-spec validate_snowflake(term()) -> {ok, pos_integer()} | {error, atom(), atom()}.
validate_snowflake(Id) when is_integer(Id); is_binary(Id) ->
    try snowflake_id:parse_optional(Id) of
        Snowflake when is_integer(Snowflake), Snowflake > 0 -> {ok, Snowflake};
        _ -> gateway_errors:error(validation_invalid_snowflake)
    catch
        error:{invalid_snowflake, _} -> gateway_errors:error(validation_invalid_snowflake)
    end;
validate_snowflake(null) ->
    gateway_errors:error(validation_null_snowflake);
validate_snowflake(_) ->
    gateway_errors:error(validation_invalid_snowflake).

-spec validate_snowflake(binary(), term()) -> {ok, pos_integer()} | {error, atom(), atom()}.
validate_snowflake(_FieldName, Value) ->
    validate_snowflake(Value).

-spec validate_optional_snowflake(term()) ->
    {ok, pos_integer() | null} | {error, atom(), atom()}.
validate_optional_snowflake(null) ->
    {ok, null};
validate_optional_snowflake(Value) ->
    validate_snowflake(Value).

-spec validate_snowflake_list(term()) -> {ok, [pos_integer()]} | {error, atom(), atom()}.
validate_snowflake_list(List) when is_list(List) ->
    validate_snowflake_list_items(List, []);
validate_snowflake_list(_) ->
    gateway_errors:error(validation_expected_list).

-spec validate_snowflake_list(binary(), term()) ->
    {ok, [pos_integer()]} | {error, atom(), atom()}.
validate_snowflake_list(_FieldName, Value) ->
    validate_snowflake_list(Value).

-spec snowflake_or_throw(binary(), term()) -> pos_integer().
snowflake_or_throw(FieldName, Value) ->
    case validate_snowflake(FieldName, Value) of
        {ok, Id} -> Id;
        {error, _, Reason} -> erlang:error({validation, Reason})
    end.

-spec snowflake_list_or_throw(binary(), term()) -> [pos_integer()].
snowflake_list_or_throw(FieldName, Value) ->
    case validate_snowflake_list(FieldName, Value) of
        {ok, Ids} -> Ids;
        {error, _, Reason} -> erlang:error({validation, Reason})
    end.

-spec validate_snowflake_list_items([term()], [pos_integer()]) ->
    {ok, [pos_integer()]} | {error, atom(), atom()}.
validate_snowflake_list_items([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_snowflake_list_items([Item | Rest], Acc) ->
    case validate_snowflake(Item) of
        {ok, Id} -> validate_snowflake_list_items(Rest, [Id | Acc]);
        {error, _, _} -> gateway_errors:error(validation_invalid_snowflake_list)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

validate_snowflake_integer_test() ->
    ?assertEqual({ok, 123}, validate_snowflake(123)),
    ?assertMatch({error, _, _}, validate_snowflake(0)),
    ?assertMatch({error, _, _}, validate_snowflake(-1)).

validate_snowflake_binary_test() ->
    ?assertEqual({ok, 123}, validate_snowflake(<<"123">>)),
    ?assertMatch({error, _, _}, validate_snowflake(<<"0">>)).

validate_snowflake_invalid_test() ->
    ?assertMatch({error, _, _}, validate_snowflake(null)),
    ?assertMatch({error, _, _}, validate_snowflake(<<"abc">>)),
    ?assertMatch({error, _, _}, validate_snowflake(<<"001">>)),
    ?assertMatch({error, _, _}, validate_snowflake(<<"-1">>)),
    ?assertMatch({error, _, _}, validate_snowflake(1.5)).

validate_optional_snowflake_test() ->
    ?assertEqual({ok, null}, validate_optional_snowflake(null)),
    ?assertEqual({ok, 123}, validate_optional_snowflake(123)),
    ?assertEqual({ok, 456}, validate_optional_snowflake(<<"456">>)).

validate_snowflake_list_test() ->
    ?assertEqual({ok, [1, 2, 3]}, validate_snowflake_list([1, 2, 3])),
    ?assertEqual({ok, [1, 2]}, validate_snowflake_list([<<"1">>, <<"2">>])),
    ?assertEqual({ok, []}, validate_snowflake_list([])).

validate_snowflake_list_invalid_test() ->
    ?assertMatch({error, _, _}, validate_snowflake_list([1, <<"abc">>])),
    ?assertMatch({error, _, _}, validate_snowflake_list([1, <<"0">>])),
    ?assertMatch({error, _, _}, validate_snowflake_list(not_a_list)).

-endif.
