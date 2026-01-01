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

-module(validation).

-export([
    validate_snowflake/1,
    validate_snowflake/2,
    validate_optional_snowflake/1,
    validate_snowflake_list/1,
    validate_snowflake_list/2,
    snowflake_or_throw/2,
    snowflake_or_default/2,
    snowflake_or_default/3,
    snowflake_list_or_throw/2,
    extract_snowflake/2,
    extract_snowflake/3,
    extract_snowflakes/2,
    get_field/2,
    get_field/3,
    get_required_field/3,
    get_optional_field/3,
    error_category_to_close_code/1
]).

-spec validate_snowflake(term()) -> {ok, integer()} | {error, atom(), atom()}.
validate_snowflake(Id) when is_integer(Id) ->
    {ok, Id};
validate_snowflake(Bin) when is_binary(Bin) ->
    try
        Id = binary_to_integer(Bin),
        {ok, Id}
    catch
        error:badarg ->
            gateway_errors:error(validation_invalid_snowflake)
    end;
validate_snowflake(null) ->
    gateway_errors:error(validation_null_snowflake);
validate_snowflake(_) ->
    gateway_errors:error(validation_invalid_snowflake).

-spec validate_snowflake(binary(), term()) -> {ok, integer()} | {error, atom(), atom()}.
validate_snowflake(_FieldName, Value) ->
    validate_snowflake(Value).

-spec validate_optional_snowflake(term()) -> {ok, integer() | null} | {error, atom(), atom()}.
validate_optional_snowflake(null) ->
    {ok, null};
validate_optional_snowflake(Value) ->
    validate_snowflake(Value).

-spec validate_snowflake_list(list()) -> {ok, [integer()]} | {error, atom(), atom()}.
validate_snowflake_list(List) when is_list(List) ->
    try
        Ids = lists:map(
            fun(Item) ->
                case validate_snowflake(Item) of
                    {ok, Id} -> Id;
                    {error, _, _} -> throw(invalid)
                end
            end,
            List
        ),
        {ok, Ids}
    catch
        throw:invalid ->
            gateway_errors:error(validation_invalid_snowflake_list)
    end;
validate_snowflake_list(_) ->
    gateway_errors:error(validation_expected_list).

-spec validate_snowflake_list(binary(), list()) -> {ok, [integer()]} | {error, atom(), atom()}.
validate_snowflake_list(_FieldName, Value) ->
    validate_snowflake_list(Value).

-spec snowflake_or_throw(binary(), term()) -> integer().
snowflake_or_throw(FieldName, Value) ->
    case validate_snowflake(FieldName, Value) of
        {ok, Id} -> Id;
        {error, _, Reason} -> throw({error, Reason})
    end.

-spec snowflake_or_default(term(), integer()) -> integer().
snowflake_or_default(Value, Default) ->
    case validate_snowflake(Value) of
        {ok, Id} -> Id;
        {error, _, _} -> Default
    end.

-spec snowflake_or_default(binary(), term(), integer()) -> integer().
snowflake_or_default(FieldName, Value, Default) ->
    case validate_snowflake(FieldName, Value) of
        {ok, Id} -> Id;
        {error, _, _} -> Default
    end.

-spec snowflake_list_or_throw(binary(), list()) -> [integer()].
snowflake_list_or_throw(FieldName, Value) ->
    case validate_snowflake_list(FieldName, Value) of
        {ok, Ids} -> Ids;
        {error, _, Reason} -> throw({error, Reason})
    end.

-spec extract_snowflake(binary(), map()) -> {ok, integer()} | {error, atom(), atom()}.
extract_snowflake(FieldName, Map) ->
    case get_field(FieldName, Map) of
        {ok, Value} ->
            validate_snowflake(FieldName, Value);
        {error, _, _} = Error ->
            Error
    end.

-spec extract_snowflake(binary(), map(), integer()) -> integer().
extract_snowflake(FieldName, Map, Default) ->
    case get_field(FieldName, Map) of
        {ok, Value} ->
            snowflake_or_default(FieldName, Value, Default);
        {error, _, _} ->
            Default
    end.

-spec extract_snowflakes(list({atom(), binary()}), map()) ->
    {ok, #{atom() => integer()}} | {error, atom(), atom()}.
extract_snowflakes(FieldSpecs, Map) ->
    extract_snowflakes_loop(FieldSpecs, Map, #{}).

extract_snowflakes_loop([], _Map, Acc) ->
    {ok, Acc};
extract_snowflakes_loop([{KeyAtom, FieldName} | Rest], Map, Acc) ->
    case extract_snowflake(FieldName, Map) of
        {ok, Value} ->
            extract_snowflakes_loop(Rest, Map, maps:put(KeyAtom, Value, Acc));
        {error, _, _} = Error ->
            Error
    end.

-spec get_field(term(), map()) -> {ok, term()} | {error, atom(), atom()}.
get_field(Key, Map) when is_map(Map) ->
    case maps:get(Key, Map, undefined) of
        undefined ->
            gateway_errors:error(validation_missing_field);
        Value ->
            {ok, Value}
    end;
get_field(_Key, _NotMap) ->
    gateway_errors:error(validation_expected_map).

-spec get_field(term(), map(), term()) -> term().
get_field(Key, Map, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
get_field(_Key, _NotMap, Default) ->
    Default.

-spec get_required_field(binary(), map(), fun((term()) -> {ok, term()} | {error, atom(), atom()})) ->
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

-spec error_category_to_close_code(atom()) -> integer().
error_category_to_close_code(rate_limited) ->
    constants:close_code_to_num(rate_limited);
error_category_to_close_code(auth_failed) ->
    constants:close_code_to_num(authentication_failed);
error_category_to_close_code(_) ->
    constants:close_code_to_num(unknown_error).
