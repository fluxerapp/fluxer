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
    snowflake_list_or_throw/2,
    extract_snowflake/2,
    extract_snowflakes/2,
    get_field/2,
    get_field/3,
    get_required_field/3,
    get_optional_field/3
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

-spec extract_snowflake(binary(), map()) -> {ok, pos_integer()} | {error, atom(), atom()}.
extract_snowflake(FieldName, Map) ->
    case get_field(FieldName, Map) of
        {ok, Value} ->
            validate_snowflake(FieldName, Value);
        {error, _, _} = Error ->
            Error
    end.

-spec extract_snowflakes(list({atom(), binary()}), map()) ->
    {ok, #{atom() => pos_integer()}} | {error, atom(), atom()}.
extract_snowflakes(FieldSpecs, Map) ->
    extract_snowflakes_loop(FieldSpecs, Map, #{}).

-spec extract_snowflakes_loop(list({atom(), binary()}), map(), map()) ->
    {ok, #{atom() => pos_integer()}} | {error, atom(), atom()}.
extract_snowflakes_loop([], _Map, Acc) ->
    {ok, Acc};
extract_snowflakes_loop([{KeyAtom, FieldName} | Rest], Map, Acc) ->
    case extract_snowflake(FieldName, Map) of
        {ok, Value} ->
            extract_snowflakes_loop(Rest, Map, Acc#{KeyAtom => Value});
        {error, _, _} = Error ->
            Error
    end.

-spec get_field(term(), term()) -> {ok, term()} | {error, atom(), atom()}.
get_field(Key, Map) when is_map(Map) ->
    case maps:get(Key, Map, undefined) of
        undefined ->
            gateway_errors:error(validation_missing_field);
        Value ->
            {ok, Value}
    end;
get_field(_Key, _NotMap) ->
    gateway_errors:error(validation_expected_map).

-spec get_field(term(), term(), term()) -> term().
get_field(Key, Map, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
get_field(_Key, _NotMap, Default) ->
    Default.

-spec get_required_field(
    binary(), term(), fun((term()) -> {ok, term()} | {error, atom(), atom()})
) ->
    {ok, term()} | {error, atom(), atom()}.
get_required_field(FieldName, Map, Validator) ->
    case get_field(FieldName, Map) of
        {ok, Value} ->
            Validator(Value);
        {error, _, _} = Error ->
            Error
    end.

-spec get_optional_field(term(), map(), fun((term()) -> {ok, term()} | {error, atom(), atom()})) ->
    {ok, term() | undefined} | {error, atom(), atom()}.
get_optional_field(FieldName, Map, Validator) ->
    case maps:get(FieldName, Map, undefined) of
        undefined ->
            {ok, undefined};
        Value ->
            Validator(Value)
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

get_field_test() ->
    Map = #{<<"key">> => <<"value">>},
    ?assertEqual({ok, <<"value">>}, get_field(<<"key">>, Map)),
    ?assertMatch({error, _, _}, get_field(<<"missing">>, Map)).

get_field_with_default_test() ->
    Map = #{<<"key">> => <<"value">>},
    ?assertEqual(<<"value">>, get_field(<<"key">>, Map, <<"default">>)),
    ?assertEqual(<<"default">>, get_field(<<"missing">>, Map, <<"default">>)),
    ?assertEqual(<<"default">>, get_field(<<"key">>, not_a_map, <<"default">>)).

extract_snowflake_test() ->
    Map = #{<<"id">> => <<"123">>},
    ?assertEqual({ok, 123}, extract_snowflake(<<"id">>, Map)),
    ?assertMatch({error, _, _}, extract_snowflake(<<"missing">>, Map)).

extract_snowflakes_test() ->
    Map = #{<<"user_id">> => <<"123">>, <<"guild_id">> => <<"456">>},
    Specs = [{user, <<"user_id">>}, {guild, <<"guild_id">>}],
    {ok, Result} = extract_snowflakes(Specs, Map),
    ?assertEqual(123, maps:get(user, Result)),
    ?assertEqual(456, maps:get(guild, Result)).

get_required_field_test() ->
    Map = #{<<"id">> => <<"123">>},
    Validator = fun validation:validate_snowflake/1,
    ?assertEqual({ok, 123}, get_required_field(<<"id">>, Map, Validator)),
    ?assertMatch({error, _, _}, get_required_field(<<"missing">>, Map, Validator)).

get_optional_field_test() ->
    Map = #{<<"id">> => <<"123">>},
    Validator = fun validation:validate_snowflake/1,
    ?assertEqual({ok, 123}, get_optional_field(<<"id">>, Map, Validator)),
    ?assertEqual({ok, undefined}, get_optional_field(<<"missing">>, Map, Validator)).

-endif.
