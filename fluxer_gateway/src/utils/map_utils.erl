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

-module(map_utils).

-export([
    get_safe/3,
    get_nested/3,
    ensure_map/1,
    ensure_list/1,
    filter_by_field/3,
    find_by_field/3,
    get_integer/3,
    get_binary/3
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type key() :: atom() | binary() | term().
-type path() :: [key()].
-type default() :: term().

-spec get_safe(Map :: map() | term(), Key :: key(), Default :: default()) -> term().
get_safe(Map, Key, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
get_safe(_NotMap, _Key, Default) ->
    Default.

-spec get_nested(Map :: map() | term(), Path :: path(), Default :: default()) -> term().
get_nested(Map, [], _Default) when is_map(Map) ->
    Map;
get_nested(_NotMap, [], Default) ->
    Default;
get_nested(Map, [Key | Rest], Default) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            get_nested(Value, Rest, Default);
        error ->
            Default
    end;
get_nested(_NotMap, _Path, Default) ->
    Default.

-spec ensure_map(term()) -> map().
ensure_map(Map) when is_map(Map) ->
    Map;
ensure_map(_NotMap) ->
    #{}.

-spec ensure_list(term()) -> list().
ensure_list(List) when is_list(List) ->
    List;
ensure_list(_NotList) ->
    [].

-spec get_integer(map() | term(), key(), term()) -> integer() | term().
get_integer(Map, Key, Default) when is_map(Map) ->
    case type_conv:to_integer(maps:get(Key, Map, undefined)) of
        undefined -> Default;
        Value -> Value
    end;
get_integer(_NotMap, _Key, Default) ->
    Default.

-spec get_binary(map() | term(), key(), term()) -> binary() | term().
get_binary(Map, Key, Default) when is_map(Map) ->
    case type_conv:to_binary(maps:get(Key, Map, undefined)) of
        undefined -> Default;
        Value -> Value
    end;
get_binary(_NotMap, _Key, Default) ->
    Default.

-spec filter_by_field(List :: list(), Field :: key(), Value :: term()) -> list(map()).
filter_by_field(List, Field, Value) when is_list(List) ->
    lists:filter(
        fun
            (Item) when is_map(Item) ->
                case maps:find(Field, Item) of
                    {ok, Value} -> true;
                    _ -> false
                end;
            (_NotMap) ->
                false
        end,
        List
    );
filter_by_field(_NotList, _Field, _Value) ->
    [].

-spec find_by_field(List :: list(), Field :: key(), Value :: term()) -> {ok, map()} | error.
find_by_field(List, Field, Value) when is_list(List) ->
    find_by_field_loop(List, Field, Value);
find_by_field(_NotList, _Field, _Value) ->
    error.

-spec find_by_field_loop(list(), key(), term()) -> {ok, map()} | error.
find_by_field_loop([], _Field, _Value) ->
    error;
find_by_field_loop([Item | Rest], Field, Value) when is_map(Item) ->
    case maps:find(Field, Item) of
        {ok, Value} ->
            {ok, Item};
        _ ->
            find_by_field_loop(Rest, Field, Value)
    end;
find_by_field_loop([_NotMap | Rest], Field, Value) ->
    find_by_field_loop(Rest, Field, Value).

-ifdef(TEST).

get_safe_basic_test() ->
    Map = #{key => value, number => 42},

    ?assertEqual(value, get_safe(Map, key, default)),
    ?assertEqual(42, get_safe(Map, number, 0)),

    ?assertEqual(default, get_safe(Map, missing, default)),
    ?assertEqual(0, get_safe(Map, missing, 0)).

get_safe_various_input_types_test() ->
    ?assertEqual(default, get_safe(not_a_map, key, default)),
    ?assertEqual(default, get_safe([], key, default)),
    ?assertEqual(default, get_safe(123, key, default)),
    ?assertEqual(default, get_safe(<<"binary">>, key, default)),
    ?assertEqual(default, get_safe(undefined, key, default)),
    ?assertEqual(default, get_safe(atom, key, default)),
    ?assertEqual(default, get_safe({tuple, value}, key, default)),
    ?assertEqual(default, get_safe(self(), key, default)).

get_safe_various_key_types_test() ->
    Map = #{
        atom_key => atom_value,
        <<"binary_key">> => binary_value,
        123 => number_key_value,
        {tuple, key} => tuple_key_value
    },

    ?assertEqual(atom_value, get_safe(Map, atom_key, default)),
    ?assertEqual(binary_value, get_safe(Map, <<"binary_key">>, default)),
    ?assertEqual(number_key_value, get_safe(Map, 123, default)),
    ?assertEqual(tuple_key_value, get_safe(Map, {tuple, key}, default)),

    ?assertEqual(default, get_safe(Map, missing_atom, default)),
    ?assertEqual(default, get_safe(Map, <<"missing_binary">>, default)),
    ?assertEqual(default, get_safe(Map, 999, default)).

get_safe_default_types_test() ->
    Map = #{key => value},

    ?assertEqual(nil, get_safe(Map, missing, nil)),
    ?assertEqual(0, get_safe(Map, missing, 0)),
    ?assertEqual(<<"default">>, get_safe(Map, missing, <<"default">>)),
    ?assertEqual([], get_safe(Map, missing, [])),
    ?assertEqual(#{}, get_safe(Map, missing, #{})),
    ?assertEqual({tuple, default}, get_safe(Map, missing, {tuple, default})).

get_nested_basic_test() ->
    Map = #{
        level1 => #{
            level2 => #{
                level3 => deep_value
            },
            other => other_value
        },
        simple => simple_value
    },

    ?assertEqual(#{level3 => deep_value}, get_nested(Map, [level1, level2], default)),
    ?assertEqual(
        #{other => other_value, level2 => #{level3 => deep_value}},
        get_nested(Map, [level1], default)
    ),

    ?assertEqual(default, get_nested(Map, [level1, level2, level3], default)),
    ?assertEqual(default, get_nested(Map, [level1, other], default)),
    ?assertEqual(default, get_nested(Map, [simple], default)),

    ?assertEqual(Map, get_nested(Map, [], default)),

    ?assertEqual(default, get_nested(Map, [level1, missing], default)),
    ?assertEqual(default, get_nested(Map, [missing, level2], default)),
    ?assertEqual(default, get_nested(Map, [level1, level2, missing], default)).

get_nested_deep_nesting_test() ->
    DeepMap = #{
        l1 => #{
            l2 => #{
                l3 => #{
                    l4 => #{
                        l5 => final_value,
                        other5 => value5
                    },
                    other4 => value4
                },
                other3 => value3
            }
        }
    },

    Level4Map = get_nested(DeepMap, [l1, l2, l3, l4], default),
    ?assert(is_map(Level4Map)),
    ?assertEqual(final_value, maps:get(l5, Level4Map)),
    ?assertEqual(value5, maps:get(other5, Level4Map)),

    Level3Map = get_nested(DeepMap, [l1, l2, l3], default),
    ?assert(is_map(Level3Map)),
    ?assertEqual(value4, maps:get(other4, Level3Map)),

    Level2Map = get_nested(DeepMap, [l1, l2], default),
    ?assert(is_map(Level2Map)),
    ?assertEqual(value3, maps:get(other3, Level2Map)),

    ?assertEqual(default, get_nested(DeepMap, [l1, l2, l3, l4, l5], default)),
    ?assertEqual(default, get_nested(DeepMap, [l1, l2, l3, l4, other5], default)),
    ?assertEqual(default, get_nested(DeepMap, [l1, l2, l3, other4], default)),
    ?assertEqual(default, get_nested(DeepMap, [l1, l2, other3], default)),

    ?assertEqual(default, get_nested(DeepMap, [l1, l2, l3, l4, l5, extra], default)),

    ?assertEqual(default, get_nested(DeepMap, [l1, l2, missing, l4, l5], default)),
    ?assertEqual(default, get_nested(DeepMap, [missing, l2, l3, l4, l5], default)).

get_nested_partial_paths_test() ->
    Map = #{
        user => #{
            name => <<"Alice">>,
            age => 30,
            address => #{
                city => <<"New York">>,
                zip => 10001
            }
        },
        count => 42,
        tags => [tag1, tag2, tag3]
    },

    UserMap = get_nested(Map, [user], default),
    ?assert(is_map(UserMap)),
    ?assertEqual(<<"Alice">>, maps:get(name, UserMap)),

    AddressMap = get_nested(Map, [user, address], default),
    ?assert(is_map(AddressMap)),
    ?assertEqual(<<"New York">>, maps:get(city, AddressMap)),

    ?assertEqual(default, get_nested(Map, [count], default)),
    ?assertEqual(default, get_nested(Map, [user, name], default)),
    ?assertEqual(default, get_nested(Map, [user, age], default)),
    ?assertEqual(default, get_nested(Map, [tags], default)),

    ?assertEqual(default, get_nested(Map, [count, extra], default)),
    ?assertEqual(default, get_nested(Map, [count, deep, path], default)),
    ?assertEqual(default, get_nested(Map, [user, name, extra], default)),
    ?assertEqual(default, get_nested(Map, [tags, extra], default)),
    ?assertEqual(default, get_nested(Map, [user, age, extra, path], default)).

get_nested_edge_cases_test() ->
    Map = #{key => #{nested => value}},

    ?assertEqual(default, get_nested(not_a_map, [], default)),
    ?assertEqual(default, get_nested([], [], default)),
    ?assertEqual(default, get_nested(123, [], default)),

    ?assertEqual(default, get_nested(not_a_map, [key], default)),
    ?assertEqual(default, get_nested([], [key], default)),
    ?assertEqual(default, get_nested(123, [key, nested], default)),
    ?assertEqual(default, get_safe(undefined, key, default)),

    ?assertEqual(#{nested => value}, get_nested(Map, [key], default)),

    ?assertEqual(default, get_nested(Map, [key, nested], default)),

    BinaryMap = #{<<"key">> => #{<<"nested">> => <<"value">>}},
    ?assertEqual(
        #{<<"nested">> => <<"value">>},
        get_nested(BinaryMap, [<<"key">>], default)
    ),
    ?assertEqual(default, get_nested(BinaryMap, [<<"key">>, <<"nested">>], default)).

get_integer_basic_test() ->
    Map = #{id => <<"42">>, <<"count">> => 10},
    ?assertEqual(42, get_integer(Map, id, 0)),
    ?assertEqual(10, get_integer(Map, <<"count">>, 0)),
    ?assertEqual(99, get_integer(Map, missing, 99)).

get_integer_invalid_input_test() ->
    ?assertEqual(7, get_integer(undefined, id, 7)),
    ?assertEqual(undefined, get_integer(#{}, id, undefined)),
    ?assertEqual(0, get_integer(#{id => <<"abc">>}, id, 0)).

get_binary_basic_test() ->
    Map = #{<<"name">> => <<"fluxer">>, tag => atom},
    ?assertEqual(<<"fluxer">>, get_binary(Map, <<"name">>, <<"default">>)),
    ?assertEqual(<<"atom">>, get_binary(Map, tag, <<"default">>)).

get_binary_invalid_input_test() ->
    ?assertEqual(<<"default">>, get_binary(not_a_map, <<"id">>, <<"default">>)),
    ?assertEqual(undefined, get_binary(#{}, <<"missing">>, undefined)),
    ?assertEqual(<<"default">>, get_binary(#{num => 123}, <<"num">>, <<"default">>)).

ensure_map_test() ->
    Map = #{key => value, nested => #{inner => data}},
    ?assertEqual(Map, ensure_map(Map)),

    ?assertEqual(#{}, ensure_map(#{})).

ensure_map_all_input_types_test() ->
    ?assertEqual(#{}, ensure_map(not_a_map)),
    ?assertEqual(#{}, ensure_map([])),
    ?assertEqual(#{}, ensure_map([1, 2, 3])),
    ?assertEqual(#{}, ensure_map(123)),
    ?assertEqual(#{}, ensure_map(123.456)),
    ?assertEqual(#{}, ensure_map(<<"binary">>)),
    ?assertEqual(#{}, ensure_map("string")),
    ?assertEqual(#{}, ensure_map(undefined)),
    ?assertEqual(#{}, ensure_map(atom)),
    ?assertEqual(#{}, ensure_map(true)),
    ?assertEqual(#{}, ensure_map(false)),
    ?assertEqual(#{}, ensure_map({tuple, value})),
    ?assertEqual(#{}, ensure_map(self())),
    ?assertEqual(#{}, ensure_map(make_ref())),
    ?assertEqual(#{}, ensure_map(fun() -> ok end)).

ensure_list_test() ->
    List = [1, 2, 3],
    ?assertEqual(List, ensure_list(List)),

    ComplexList = [#{a => 1}, {tuple}, <<"binary">>, atom],
    ?assertEqual(ComplexList, ensure_list(ComplexList)),

    ?assertEqual([], ensure_list([])).

ensure_list_all_input_types_test() ->
    ?assertEqual([], ensure_list(not_a_list)),
    ?assertEqual([], ensure_list(#{})),
    ?assertEqual([], ensure_list(#{key => value})),
    ?assertEqual([], ensure_list(123)),
    ?assertEqual([], ensure_list(123.456)),
    ?assertEqual([], ensure_list(<<"binary">>)),
    ?assertEqual("string", ensure_list("string")),
    ?assert(is_list(ensure_list("string"))),
    ?assertEqual([], ensure_list(undefined)),
    ?assertEqual([], ensure_list(atom)),
    ?assertEqual([], ensure_list(true)),
    ?assertEqual([], ensure_list(false)),
    ?assertEqual([], ensure_list({tuple, value})),
    ?assertEqual([], ensure_list(self())),
    ?assertEqual([], ensure_list(make_ref())),
    ?assertEqual([], ensure_list(fun() -> ok end)).

filter_by_field_basic_test() ->
    List = [
        #{id => 1, type => a, name => <<"first">>},
        #{id => 2, type => b, name => <<"second">>},
        #{id => 3, type => a, name => <<"third">>},
        #{id => 4, type => c},
        #{id => 5, type => a}
    ],

    Filtered = filter_by_field(List, type, a),
    ?assertEqual(3, length(Filtered)),
    ?assert(lists:all(fun(M) -> maps:get(type, M) =:= a end, Filtered)),

    ?assertEqual(
        [#{id => 2, type => b, name => <<"second">>}],
        filter_by_field(List, id, 2)
    ),

    ?assertEqual([], filter_by_field(List, type, nonexistent)),

    ?assertEqual([], filter_by_field(List, missing_field, value)).

filter_by_field_mixed_lists_test() ->
    MixedList = [
        #{id => 1, type => a},
        not_a_map,
        #{id => 2, type => b},
        123,
        #{id => 3, type => a},
        <<"binary">>,
        undefined,
        #{id => 4, type => a},
        [],
        {tuple, value},
        #{id => 5, type => c}
    ],

    Result = filter_by_field(MixedList, type, a),
    ?assertEqual(3, length(Result)),
    ?assert(lists:all(fun is_map/1, Result)),
    ?assert(lists:all(fun(M) -> maps:get(type, M) =:= a end, Result)),

    Ids = [maps:get(id, M) || M <- Result],
    ?assertEqual([1, 3, 4], Ids),

    ResultB = filter_by_field(MixedList, type, b),
    ?assertEqual(1, length(ResultB)),
    ?assertEqual([#{id => 2, type => b}], ResultB).

filter_by_field_edge_cases_test() ->
    ?assertEqual([], filter_by_field([], field, value)),

    NonMaps = [123, atom, <<"binary">>, {tuple}, []],
    ?assertEqual([], filter_by_field(NonMaps, field, value)),

    NoFieldList = [#{a => 1}, #{b => 2}, #{c => 3}],
    ?assertEqual([], filter_by_field(NoFieldList, missing, value)),

    ?assertEqual([], filter_by_field(not_a_list, field, value)),
    ?assertEqual([], filter_by_field(#{}, field, value)),
    ?assertEqual([], filter_by_field(123, field, value)),

    BinaryList = [
        #{<<"key">> => <<"value1">>},
        #{<<"key">> => <<"value2">>},
        #{<<"other">> => <<"value1">>}
    ],
    ?assertEqual(
        [#{<<"key">> => <<"value1">>}],
        filter_by_field(BinaryList, <<"key">>, <<"value1">>)
    ),

    ComplexList = [
        #{data => #{nested => value}},
        #{data => [1, 2, 3]},
        #{data => #{nested => value}},
        #{other => data}
    ],
    ComplexFiltered = filter_by_field(ComplexList, data, #{nested => value}),
    ?assertEqual(2, length(ComplexFiltered)).

find_by_field_basic_test() ->
    List = [
        #{id => 1, type => a},
        #{id => 2, type => b},
        #{id => 3, type => a},
        #{id => 4, type => c}
    ],

    ?assertEqual({ok, #{id => 2, type => b}}, find_by_field(List, id, 2)),
    ?assertEqual({ok, #{id => 4, type => c}}, find_by_field(List, id, 4)),

    ?assertEqual(error, find_by_field(List, id, 999)),
    ?assertEqual(error, find_by_field(List, type, nonexistent)),

    ?assertEqual(error, find_by_field([], id, 1)).

find_by_field_multiple_matches_test() ->
    List = [
        #{id => 1, type => a, order => first},
        #{id => 2, type => b, order => second},
        #{id => 3, type => a, order => third},
        #{id => 4, type => c, order => fourth},
        #{id => 5, type => a, order => fifth}
    ],

    {ok, First} = find_by_field(List, type, a),
    ?assertEqual(1, maps:get(id, First)),
    ?assertEqual(first, maps:get(order, First)),

    ?assertNotEqual(third, maps:get(order, First)),
    ?assertNotEqual(fifth, maps:get(order, First)),

    List2 = [
        #{name => <<"Alice">>, age => 25},
        #{name => <<"Bob">>, age => 30},
        #{name => <<"Charlie">>, age => 25},
        #{name => <<"Diana">>, age => 25}
    ],

    {ok, FirstAge25} = find_by_field(List2, age, 25),
    ?assertEqual(<<"Alice">>, maps:get(name, FirstAge25)).

find_by_field_no_matches_test() ->
    List = [
        #{id => 1, type => a},
        #{id => 2, type => b},
        #{id => 3, type => c}
    ],

    ?assertEqual(error, find_by_field(List, id, 999)),
    ?assertEqual(error, find_by_field(List, type, z)),
    ?assertEqual(error, find_by_field(List, missing_field, value)),
    ?assertEqual(error, find_by_field(List, id, <<"wrong_type">>)),

    ?assertEqual(error, find_by_field([], any_field, any_value)).

find_by_field_with_non_maps_test() ->
    MixedList = [
        not_a_map,
        123,
        #{id => 1, type => a},
        <<"binary">>,
        undefined,
        #{id => 2, type => b},
        [],
        #{id => 3, type => a}
    ],

    {ok, Found1} = find_by_field(MixedList, type, a),
    ?assertEqual(1, maps:get(id, Found1)),

    {ok, Found2} = find_by_field(MixedList, id, 2),
    ?assertEqual(b, maps:get(type, Found2)),

    MixedList2 = [atom, 456, {tuple}, #{id => 5, type => z}],
    ?assertEqual({ok, #{id => 5, type => z}}, find_by_field(MixedList2, id, 5)),

    OnlyNonMaps = [atom, 123, <<"binary">>, {tuple}, []],
    ?assertEqual(error, find_by_field(OnlyNonMaps, field, value)).

find_by_field_invalid_input_test() ->
    ?assertEqual(error, find_by_field(not_a_list, field, value)),
    ?assertEqual(error, find_by_field(#{}, field, value)),
    ?assertEqual(error, find_by_field(123, field, value)),
    ?assertEqual(error, find_by_field(<<"binary">>, field, value)),
    ?assertEqual(error, find_by_field(undefined, field, value)),
    ?assertEqual(error, find_by_field(atom, field, value)),
    ?assertEqual(error, find_by_field({tuple}, field, value)).

find_by_field_complex_values_test() ->
    List = [
        #{<<"id">> => <<"first">>, <<"data">> => <<"value1">>},
        #{<<"id">> => <<"second">>, <<"data">> => <<"value2">>},
        #{<<"id">> => <<"third">>, <<"data">> => <<"value1">>}
    ],

    {ok, Found} = find_by_field(List, <<"data">>, <<"value1">>),
    ?assertEqual(<<"first">>, maps:get(<<"id">>, Found)),

    ComplexList = [
        #{key => #{nested => value1}, id => 1},
        #{key => [1, 2, 3], id => 2},
        #{key => #{nested => value1}, id => 3}
    ],

    {ok, ComplexFound} = find_by_field(ComplexList, key, #{nested => value1}),
    ?assertEqual(1, maps:get(id, ComplexFound)),

    {ok, ListFound} = find_by_field(ComplexList, key, [1, 2, 3]),
    ?assertEqual(2, maps:get(id, ListFound)).

-endif.
