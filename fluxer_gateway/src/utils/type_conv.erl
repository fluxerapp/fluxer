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

-module(type_conv).

-export([
    to_integer/1,
    to_binary/1,
    to_list/1,
    extract_id/2,
    extract_id_required/2
]).

-type convertible_to_integer() :: integer() | binary() | list() | atom().
-type convertible_to_binary() :: binary() | integer() | list() | atom().
-type convertible_to_list() :: list() | binary() | atom().

-spec to_integer(convertible_to_integer() | undefined) -> integer() | undefined.
to_integer(undefined) ->
    undefined;
to_integer(Value) when is_integer(Value) ->
    Value;
to_integer(Value) when is_binary(Value) ->
    try
        binary_to_integer(Value)
    catch
        error:badarg ->
            undefined
    end;
to_integer(Value) when is_list(Value) ->
    try
        list_to_integer(Value)
    catch
        error:badarg ->
            undefined
    end;
to_integer(Value) when is_atom(Value) ->
    try
        list_to_integer(atom_to_list(Value))
    catch
        error:badarg ->
            undefined
    end;
to_integer(_) ->
    undefined.

-spec to_binary(convertible_to_binary() | undefined) -> binary() | undefined.
to_binary(undefined) ->
    undefined;
to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
to_binary(Value) when is_list(Value) ->
    try
        list_to_binary(Value)
    catch
        error:badarg ->
            undefined
    end;
to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_binary(_) ->
    undefined.

-spec to_list(convertible_to_list() | undefined) -> list() | undefined.
to_list(undefined) ->
    undefined;
to_list(Value) when is_list(Value) ->
    Value;
to_list(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_list(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_list(_) ->
    undefined.

-spec extract_id(map(), atom() | binary()) -> integer() | undefined.
extract_id(Map, Field) when is_map(Map), is_atom(Field) ->
    case maps:get(Field, Map, undefined) of
        undefined ->
            undefined;
        Value ->
            to_integer(Value)
    end;
extract_id(Map, Field) when is_map(Map), is_binary(Field) ->
    case maps:get(Field, Map, undefined) of
        undefined ->
            undefined;
        Value ->
            to_integer(Value)
    end;
extract_id(_, _) ->
    undefined.

-spec extract_id_required(map(), atom() | binary()) -> integer().
extract_id_required(Map, Field) ->
    case extract_id(Map, Field) of
        undefined ->
            0;
        Value when is_integer(Value) ->
            Value
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_integer_with_integer_test() ->
    ?assertEqual(42, to_integer(42)),
    ?assertEqual(0, to_integer(0)),
    ?assertEqual(-100, to_integer(-100)).

to_integer_with_integer_edge_cases_test() ->
    ?assertEqual(1234567890123456789, to_integer(1234567890123456789)),
    ?assertEqual(9223372036854775807, to_integer(9223372036854775807)),
    ?assertEqual(-9223372036854775807, to_integer(-9223372036854775807)).

to_integer_with_binary_valid_test() ->
    ?assertEqual(123, to_integer(<<"123">>)),
    ?assertEqual(0, to_integer(<<"0">>)),
    ?assertEqual(-456, to_integer(<<"-456">>)).

to_integer_with_binary_edge_cases_test() ->
    ?assertEqual(1234567890123456789, to_integer(<<"1234567890123456789">>)),
    ?assertEqual(9223372036854775807, to_integer(<<"9223372036854775807">>)),
    ?assertEqual(-9223372036854775807, to_integer(<<"-9223372036854775807">>)),
    ?assertEqual(1, to_integer(<<"1">>)),
    ?assertEqual(123, to_integer(<<"00123">>)),
    ?assertEqual(0, to_integer(<<"0">>)).

to_integer_with_binary_invalid_test() ->
    ?assertEqual(undefined, to_integer(<<"not_a_number">>)),
    ?assertEqual(undefined, to_integer(<<"12.34">>)),
    ?assertEqual(undefined, to_integer(<<"">>)),
    ?assertEqual(undefined, to_integer(<<"   ">>)),
    ?assertEqual(undefined, to_integer(<<"abc123">>)),
    ?assertEqual(undefined, to_integer(<<"123abc">>)),
    ?assertEqual(undefined, to_integer(<<"12 34">>)),
    ?assertEqual(undefined, to_integer(<<"--123">>)),
    ?assertEqual(undefined, to_integer(<<"+-123">>)).

to_integer_with_binary_special_chars_test() ->
    ?assertEqual(undefined, to_integer(<<"!@#$%">>)),
    ?assertEqual(undefined, to_integer(<<"∞">>)),
    ?assertEqual(undefined, to_integer(<<"①②③">>)),
    ?assertEqual(undefined, to_integer(<<"一二三">>)),
    ?assertEqual(undefined, to_integer(<<"null">>)),
    ?assertEqual(undefined, to_integer(<<"NaN">>)),
    ?assertEqual(undefined, to_integer(<<"Infinity">>)).

to_integer_with_list_valid_test() ->
    ?assertEqual(789, to_integer("789")),
    ?assertEqual(-123, to_integer("-123")),
    ?assertEqual(0, to_integer("0")).

to_integer_with_list_edge_cases_test() ->
    ?assertEqual(1234567890123456789, to_integer("1234567890123456789")),
    ?assertEqual(5, to_integer("5")),
    ?assertEqual(42, to_integer("00042")),
    ?assertEqual(-42, to_integer("-00042")).

to_integer_with_list_invalid_test() ->
    ?assertEqual(undefined, to_integer("invalid")),
    ?assertEqual(undefined, to_integer("12.34")),
    ?assertEqual(undefined, to_integer("")),
    ?assertEqual(undefined, to_integer("   ")),
    ?assertEqual(undefined, to_integer("abc")),
    ?assertEqual(undefined, to_integer("123abc")),
    ?assertEqual(undefined, to_integer("12 34")),
    ?assertEqual(undefined, to_integer([1, 2, 3])).

to_integer_with_list_special_chars_test() ->
    ?assertEqual(undefined, to_integer("!@#$%")),
    ?assertEqual(undefined, to_integer("hello world")),
    ?assertEqual(undefined, to_integer("--456")),
    ?assertEqual(undefined, to_integer("null")).

to_integer_with_atom_valid_test() ->
    ?assertEqual(123, to_integer('123')),
    ?assertEqual(-456, to_integer('-456')),
    ?assertEqual(0, to_integer('0')).

to_integer_with_atom_invalid_test() ->
    ?assertEqual(undefined, to_integer(test)),
    ?assertEqual(undefined, to_integer('not_a_number')),
    ?assertEqual(undefined, to_integer(hello)),
    ?assertEqual(undefined, to_integer(true)),
    ?assertEqual(undefined, to_integer(false)),
    ?assertEqual(undefined, to_integer(nil)),
    ?assertEqual(undefined, to_integer('')).

to_integer_with_undefined_test() ->
    ?assertEqual(undefined, to_integer(undefined)).

to_integer_with_invalid_types_test() ->
    ?assertEqual(undefined, to_integer(12.34)),
    ?assertEqual(undefined, to_integer(-45.67)),
    ?assertEqual(undefined, to_integer(0.0)),
    ?assertEqual(undefined, to_integer(#{key => value})),
    ?assertEqual(undefined, to_integer(#{})),
    ?assertEqual(undefined, to_integer({1, 2, 3})),
    ?assertEqual(undefined, to_integer({})),
    Ref = make_ref(),
    ?assertEqual(undefined, to_integer(Ref)),
    ?assertEqual(undefined, to_integer(self())),
    ?assertEqual(undefined, to_integer(erlang:list_to_port("#Port<0.0>"))).

to_binary_with_binary_test() ->
    ?assertEqual(<<"test">>, to_binary(<<"test">>)),
    ?assertEqual(<<>>, to_binary(<<>>)).

to_binary_with_binary_edge_cases_test() ->
    ?assertEqual(<<"hello world">>, to_binary(<<"hello world">>)),
    ?assertEqual(<<"!@#$%^&*()">>, to_binary(<<"!@#$%^&*()">>)),
    ?assertEqual(<<"line1\nline2">>, to_binary(<<"line1\nline2">>)),
    ?assertEqual(<<"tab\there">>, to_binary(<<"tab\there">>)),
    LongBinary = binary:copy(<<"x">>, 10000),
    ?assertEqual(LongBinary, to_binary(LongBinary)).

to_binary_with_binary_unicode_test() ->
    ?assertEqual(<<"Hello 世界"/utf8>>, to_binary(<<"Hello 世界"/utf8>>)),
    ?assertEqual(<<"Здравствуй мир"/utf8>>, to_binary(<<"Здравствуй мир"/utf8>>)),
    ?assertEqual(<<"مرحبا بالعالم"/utf8>>, to_binary(<<"مرحبا بالعالم"/utf8>>)),
    ?assertEqual(<<"🚀🌟💻"/utf8>>, to_binary(<<"🚀🌟💻"/utf8>>)),
    ?assertEqual(<<"Ñoño"/utf8>>, to_binary(<<"Ñoño"/utf8>>)),
    ?assertEqual(<<"Café"/utf8>>, to_binary(<<"Café"/utf8>>)).

to_binary_with_integer_test() ->
    ?assertEqual(<<"42">>, to_binary(42)),
    ?assertEqual(<<"0">>, to_binary(0)),
    ?assertEqual(<<"-100">>, to_binary(-100)).

to_binary_with_integer_edge_cases_test() ->
    ?assertEqual(<<"1234567890123456789">>, to_binary(1234567890123456789)),
    ?assertEqual(<<"9223372036854775807">>, to_binary(9223372036854775807)),
    ?assertEqual(<<"-9223372036854775807">>, to_binary(-9223372036854775807)),
    ?assertEqual(<<"1">>, to_binary(1)),
    ?assertEqual(<<"-1">>, to_binary(-1)).

to_binary_with_list_valid_test() ->
    ?assertEqual(<<"hello">>, to_binary("hello")),
    ?assertEqual(<<>>, to_binary("")).

to_binary_with_list_edge_cases_test() ->
    ?assertEqual(<<"hello world">>, to_binary("hello world")),
    ?assertEqual(<<"!@#$%">>, to_binary("!@#$%")),
    ?assertEqual(<<"line1\nline2">>, to_binary("line1\nline2")),
    LongString = lists:duplicate(10000, $x),
    LongBinary = binary:copy(<<"x">>, 10000),
    ?assertEqual(LongBinary, to_binary(LongString)).

to_binary_with_list_unicode_test() ->
    ?assertEqual(undefined, to_binary([72, 101, 108, 108, 111, 32, 19990, 30028])),
    ?assertEqual(undefined, to_binary([128640, 127775, 128187])),

    ?assertEqual(<<67, 97, 102, 233>>, to_binary([67, 97, 102, 233])),

    ?assertEqual(<<"Hello">>, to_binary([72, 101, 108, 108, 111])),

    ?assertEqual(<<0, 1, 127, 255>>, to_binary([0, 1, 127, 255])).

to_binary_with_list_invalid_test() ->
    ?assertEqual(<<1, 2, 3>>, to_binary([1, 2, 3])),
    ?assertEqual(undefined, to_binary([256])),
    ?assertEqual(undefined, to_binary([1000])),
    ?assertEqual(undefined, to_binary([-1])),
    ?assertEqual(undefined, to_binary([hello, world])),
    ?assertEqual(undefined, to_binary([1, 2, atom])).

to_binary_with_atom_test() ->
    ?assertEqual(<<"test">>, to_binary(test)),
    ?assertEqual(<<"hello_world">>, to_binary(hello_world)),
    ?assertEqual(<<"true">>, to_binary(true)),
    ?assertEqual(<<"false">>, to_binary(false)),
    ?assertEqual(<<"">>, to_binary('')).

to_binary_with_atom_edge_cases_test() ->
    ?assertEqual(<<"Hello World">>, to_binary('Hello World')),
    ?assertEqual(<<"123">>, to_binary('123')),
    ?assertEqual(<<"hello-world">>, to_binary('hello-world')),
    ?assertEqual(<<"test@example">>, to_binary('test@example')),
    ?assertEqual(undefined, to_binary(undefined)).

to_binary_with_invalid_types_test() ->
    ?assertEqual(undefined, to_binary(12.34)),
    ?assertEqual(undefined, to_binary(-45.67)),
    ?assertEqual(undefined, to_binary(0.0)),
    ?assertEqual(undefined, to_binary(#{key => value})),
    ?assertEqual(undefined, to_binary(#{})),
    ?assertEqual(undefined, to_binary({1, 2, 3})),
    ?assertEqual(undefined, to_binary({})),
    Ref = make_ref(),
    ?assertEqual(undefined, to_binary(Ref)),
    ?assertEqual(undefined, to_binary(self())),
    ?assertEqual(undefined, to_binary(erlang:list_to_port("#Port<0.0>"))).

to_list_with_list_test() ->
    ?assertEqual("test", to_list("test")),
    ?assertEqual([], to_list([])),
    ?assertEqual([1, 2, 3], to_list([1, 2, 3])).

to_list_with_list_edge_cases_test() ->
    ?assertEqual("hello world", to_list("hello world")),
    ?assertEqual("!@#$%^&*()", to_list("!@#$%^&*()")),
    ?assertEqual([true, false, nil], to_list([true, false, nil])),
    ?assertEqual([[1, 2], [3, 4]], to_list([[1, 2], [3, 4]])),
    LongList = lists:duplicate(10000, $x),
    ?assertEqual(LongList, to_list(LongList)).

to_list_with_list_unicode_test() ->
    ?assertEqual(
        [72, 101, 108, 108, 111, 32, 19990, 30028],
        to_list([72, 101, 108, 108, 111, 32, 19990, 30028])
    ),
    ?assertEqual([67, 97, 102, 233], to_list([67, 97, 102, 233])),
    ?assertEqual([128640, 127775, 128187], to_list([128640, 127775, 128187])).

to_list_with_binary_test() ->
    ?assertEqual("hello", to_list(<<"hello">>)),
    ?assertEqual("", to_list(<<>>)).

to_list_with_binary_edge_cases_test() ->
    ?assertEqual("hello world", to_list(<<"hello world">>)),
    ?assertEqual("!@#$%", to_list(<<"!@#$%">>)),
    ?assertEqual("line1\nline2", to_list(<<"line1\nline2">>)),
    ?assertEqual("tab\there", to_list(<<"tab\there">>)),
    LongBinary = binary:copy(<<"x">>, 10000),
    LongList = lists:duplicate(10000, $x),
    ?assertEqual(LongList, to_list(LongBinary)).

to_list_with_binary_unicode_test() ->
    ?assertEqual(
        [72, 101, 108, 108, 111, 32, 228, 184, 150, 231, 149, 140], to_list(<<"Hello 世界"/utf8>>)
    ),
    ?assertEqual([67, 97, 102, 195, 169], to_list(<<"Café"/utf8>>)),
    ?assertEqual(<<"Hello 世界"/utf8>>, list_to_binary(to_list(<<"Hello 世界"/utf8>>))).

to_list_with_atom_test() ->
    ?assertEqual("test", to_list(test)),
    ?assertEqual("hello_world", to_list(hello_world)),
    ?assertEqual("true", to_list(true)),
    ?assertEqual("false", to_list(false)),
    ?assertEqual("", to_list('')).

to_list_with_atom_edge_cases_test() ->
    ?assertEqual([72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100], to_list('Hello World')),
    ?assertEqual([49, 50, 51], to_list('123')),
    ?assertEqual([104, 101, 108, 108, 111, 45, 119, 111, 114, 108, 100], to_list('hello-world')),
    ?assertEqual(undefined, to_list(undefined)).

to_list_with_invalid_types_test() ->
    ?assertEqual(undefined, to_list(42)),
    ?assertEqual(undefined, to_list(-123)),
    ?assertEqual(undefined, to_list(0)),
    ?assertEqual(undefined, to_list(12.34)),
    ?assertEqual(undefined, to_list(-45.67)),
    ?assertEqual(undefined, to_list(0.0)),
    ?assertEqual(undefined, to_list(#{key => value})),
    ?assertEqual(undefined, to_list(#{})),
    ?assertEqual(undefined, to_list({1, 2, 3})),
    ?assertEqual(undefined, to_list({})),
    Ref = make_ref(),
    ?assertEqual(undefined, to_list(Ref)),
    ?assertEqual(undefined, to_list(self())),
    ?assertEqual(undefined, to_list(erlang:list_to_port("#Port<0.0>"))).

extract_id_with_atom_key_integer_test() ->
    Map1 = #{user_id => 123},
    ?assertEqual(123, extract_id(Map1, user_id)),

    Map2 = #{user_id => 0},
    ?assertEqual(0, extract_id(Map2, user_id)),

    Map3 = #{user_id => -456},
    ?assertEqual(-456, extract_id(Map3, user_id)).

extract_id_with_atom_key_binary_test() ->
    Map1 = #{user_id => <<"456">>},
    ?assertEqual(456, extract_id(Map1, user_id)),

    Map2 = #{user_id => <<"0">>},
    ?assertEqual(0, extract_id(Map2, user_id)),

    Map3 = #{user_id => <<"-789">>},
    ?assertEqual(-789, extract_id(Map3, user_id)).

extract_id_with_atom_key_list_test() ->
    Map1 = #{user_id => "789"},
    ?assertEqual(789, extract_id(Map1, user_id)),

    Map2 = #{user_id => "0"},
    ?assertEqual(0, extract_id(Map2, user_id)),

    Map3 = #{user_id => "-123"},
    ?assertEqual(-123, extract_id(Map3, user_id)).

extract_id_with_atom_key_edge_cases_test() ->
    Map1 = #{user_id => 1234567890123456789},
    ?assertEqual(1234567890123456789, extract_id(Map1, user_id)),

    Map2 = #{user_id => <<"9223372036854775807">>},
    ?assertEqual(9223372036854775807, extract_id(Map2, user_id)),

    Map3 = #{user_id => <<"00123">>},
    ?assertEqual(123, extract_id(Map3, user_id)).

extract_id_with_atom_key_missing_test() ->
    Map1 = #{other_field => 999},
    ?assertEqual(undefined, extract_id(Map1, user_id)),

    Map2 = #{},
    ?assertEqual(undefined, extract_id(Map2, user_id)).

extract_id_with_atom_key_undefined_value_test() ->
    Map1 = #{user_id => undefined},
    ?assertEqual(undefined, extract_id(Map1, user_id)).

extract_id_with_atom_key_invalid_value_test() ->
    Map1 = #{user_id => "invalid"},
    ?assertEqual(undefined, extract_id(Map1, user_id)),

    Map2 = #{user_id => <<"not_a_number">>},
    ?assertEqual(undefined, extract_id(Map2, user_id)),

    Map3 = #{user_id => "12.34"},
    ?assertEqual(undefined, extract_id(Map3, user_id)),

    Map4 = #{user_id => #{nested => map}},
    ?assertEqual(undefined, extract_id(Map4, user_id)),

    Map5 = #{user_id => [1, 2, 3]},
    ?assertEqual(undefined, extract_id(Map5, user_id)),

    Map6 = #{user_id => 12.34},
    ?assertEqual(undefined, extract_id(Map6, user_id)).

extract_id_with_binary_key_integer_test() ->
    Map1 = #{<<"user_id">> => 123},
    ?assertEqual(123, extract_id(Map1, <<"user_id">>)),

    Map2 = #{<<"user_id">> => 0},
    ?assertEqual(0, extract_id(Map2, <<"user_id">>)),

    Map3 = #{<<"user_id">> => -789},
    ?assertEqual(-789, extract_id(Map3, <<"user_id">>)).

extract_id_with_binary_key_binary_test() ->
    Map1 = #{<<"user_id">> => <<"456">>},
    ?assertEqual(456, extract_id(Map1, <<"user_id">>)),

    Map2 = #{<<"user_id">> => <<"0">>},
    ?assertEqual(0, extract_id(Map2, <<"user_id">>)),

    Map3 = #{<<"user_id">> => <<"-123">>},
    ?assertEqual(-123, extract_id(Map3, <<"user_id">>)).

extract_id_with_binary_key_list_test() ->
    Map1 = #{<<"user_id">> => "789"},
    ?assertEqual(789, extract_id(Map1, <<"user_id">>)),

    Map2 = #{<<"user_id">> => "0"},
    ?assertEqual(0, extract_id(Map2, <<"user_id">>)).

extract_id_with_binary_key_edge_cases_test() ->
    Map1 = #{<<"user_id">> => 1234567890123456789},
    ?assertEqual(1234567890123456789, extract_id(Map1, <<"user_id">>)),

    Map2 = #{<<>> => 123},
    ?assertEqual(123, extract_id(Map2, <<>>)),

    Map3 = #{<<"user:id">> => 456},
    ?assertEqual(456, extract_id(Map3, <<"user:id">>)).

extract_id_with_binary_key_missing_test() ->
    Map1 = #{<<"other_field">> => 999},
    ?assertEqual(undefined, extract_id(Map1, <<"user_id">>)),

    Map2 = #{},
    ?assertEqual(undefined, extract_id(Map2, <<"user_id">>)).

extract_id_with_binary_key_undefined_value_test() ->
    Map1 = #{<<"user_id">> => undefined},
    ?assertEqual(undefined, extract_id(Map1, <<"user_id">>)).

extract_id_with_binary_key_invalid_value_test() ->
    Map1 = #{<<"user_id">> => "invalid"},
    ?assertEqual(undefined, extract_id(Map1, <<"user_id">>)),

    Map2 = #{<<"user_id">> => <<"not_a_number">>},
    ?assertEqual(undefined, extract_id(Map2, <<"user_id">>)),

    Map3 = #{<<"user_id">> => 12.34},
    ?assertEqual(undefined, extract_id(Map3, <<"user_id">>)).

extract_id_with_invalid_map_test() ->
    ?assertEqual(undefined, extract_id(not_a_map, user_id)),
    ?assertEqual(undefined, extract_id(123, user_id)),
    ?assertEqual(undefined, extract_id("string", user_id)),
    ?assertEqual(undefined, extract_id(<<"binary">>, user_id)),
    ?assertEqual(undefined, extract_id([1, 2, 3], user_id)),
    ?assertEqual(undefined, extract_id({tuple}, user_id)),
    ?assertEqual(undefined, extract_id(undefined, user_id)).

extract_id_with_invalid_key_type_test() ->
    Map = #{user_id => 123},
    ?assertEqual(undefined, extract_id(Map, 123)),
    ?assertEqual(undefined, extract_id(Map, "user_id")),
    ?assertEqual(undefined, extract_id(Map, {user_id})),
    ?assertEqual(undefined, extract_id(Map, [user_id])),
    ?assertEqual(undefined, extract_id(Map, 12.34)).

extract_id_with_both_invalid_test() ->
    ?assertEqual(undefined, extract_id(not_a_map, 123)),
    ?assertEqual(undefined, extract_id(undefined, undefined)),
    ?assertEqual(undefined, extract_id(123, "key")).

extract_id_required_with_valid_integer_test() ->
    Map1 = #{user_id => 123},
    ?assertEqual(123, extract_id_required(Map1, user_id)),

    Map2 = #{user_id => 0},
    ?assertEqual(0, extract_id_required(Map2, user_id)),

    Map3 = #{user_id => -456},
    ?assertEqual(-456, extract_id_required(Map3, user_id)).

extract_id_required_with_valid_binary_test() ->
    Map1 = #{user_id => <<"456">>},
    ?assertEqual(456, extract_id_required(Map1, user_id)),

    Map2 = #{<<"user_id">> => <<"789">>},
    ?assertEqual(789, extract_id_required(Map2, <<"user_id">>)).

extract_id_required_with_valid_list_test() ->
    Map1 = #{user_id => "123"},
    ?assertEqual(123, extract_id_required(Map1, user_id)),

    Map2 = #{user_id => "-456"},
    ?assertEqual(-456, extract_id_required(Map2, user_id)).

extract_id_required_with_edge_cases_test() ->
    Map1 = #{user_id => 1234567890123456789},
    ?assertEqual(1234567890123456789, extract_id_required(Map1, user_id)),

    Map2 = #{<<"user_id">> => <<"9223372036854775807">>},
    ?assertEqual(9223372036854775807, extract_id_required(Map2, <<"user_id">>)).

extract_id_required_with_missing_field_test() ->
    Map1 = #{other_field => 999},
    ?assertEqual(0, extract_id_required(Map1, user_id)),

    Map2 = #{},
    ?assertEqual(0, extract_id_required(Map2, user_id)),

    Map3 = #{<<"other_field">> => 999},
    ?assertEqual(0, extract_id_required(Map3, <<"user_id">>)).

extract_id_required_with_undefined_value_test() ->
    Map1 = #{user_id => undefined},
    ?assertEqual(0, extract_id_required(Map1, user_id)),

    Map2 = #{<<"user_id">> => undefined},
    ?assertEqual(0, extract_id_required(Map2, <<"user_id">>)).

extract_id_required_with_invalid_value_test() ->
    Map1 = #{user_id => "invalid"},
    ?assertEqual(0, extract_id_required(Map1, user_id)),

    Map2 = #{user_id => <<"not_a_number">>},
    ?assertEqual(0, extract_id_required(Map2, user_id)),

    Map3 = #{user_id => "12.34"},
    ?assertEqual(0, extract_id_required(Map3, user_id)),

    Map4 = #{user_id => #{nested => map}},
    ?assertEqual(0, extract_id_required(Map4, user_id)),

    Map5 = #{user_id => [1, 2, 3]},
    ?assertEqual(0, extract_id_required(Map5, user_id)),

    Map6 = #{user_id => 12.34},
    ?assertEqual(0, extract_id_required(Map6, user_id)),

    Map7 = #{user_id => test_atom},
    ?assertEqual(0, extract_id_required(Map7, user_id)).

extract_id_required_with_invalid_map_test() ->
    ?assertEqual(0, extract_id_required(not_a_map, user_id)),
    ?assertEqual(0, extract_id_required(123, user_id)),
    ?assertEqual(0, extract_id_required("string", user_id)),
    ?assertEqual(0, extract_id_required(<<"binary">>, user_id)),
    ?assertEqual(0, extract_id_required([1, 2, 3], user_id)),
    ?assertEqual(0, extract_id_required({tuple}, user_id)),
    ?assertEqual(0, extract_id_required(undefined, user_id)).

extract_id_required_with_invalid_key_test() ->
    Map = #{user_id => 123},
    ?assertEqual(0, extract_id_required(Map, 123)),
    ?assertEqual(0, extract_id_required(Map, "user_id")),
    ?assertEqual(0, extract_id_required(Map, {user_id})),
    ?assertEqual(0, extract_id_required(Map, [user_id])).

extract_id_required_with_both_invalid_test() ->
    ?assertEqual(0, extract_id_required(not_a_map, 123)),
    ?assertEqual(0, extract_id_required(undefined, undefined)),
    ?assertEqual(0, extract_id_required(123, "key")).

extract_id_required_returns_integer_test() ->
    Map1 = #{user_id => 123},
    Result1 = extract_id_required(Map1, user_id),
    ?assert(is_integer(Result1)),

    Map2 = #{other => value},
    Result2 = extract_id_required(Map2, user_id),
    ?assert(is_integer(Result2)),
    ?assertEqual(0, Result2),

    Result3 = extract_id_required(not_a_map, user_id),
    ?assert(is_integer(Result3)),
    ?assertEqual(0, Result3).

-endif.
