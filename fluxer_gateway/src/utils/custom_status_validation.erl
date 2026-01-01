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

-module(custom_status_validation).

-export([
    validate/2
]).

-spec validate(integer(), map() | null) -> {ok, map()} | {error, term()}.
validate(_UserId, null) ->
    {ok, null};
validate(UserId, CustomStatus) when is_map(CustomStatus) ->
    Request = build_request(UserId, CustomStatus),
    rpc_client:call(Request).

build_request(UserId, CustomStatus) ->
    #{
        <<"type">> => <<"validate_custom_status">>,
        <<"user_id">> => type_conv:to_binary(UserId),
        <<"custom_status">> => build_custom_status_payload(CustomStatus)
    }.

build_custom_status_payload(CustomStatus) ->
    Field1 = put_optional_field(
        maps:new(),
        <<"text">>,
        maps:get(<<"text">>, CustomStatus, undefined)
    ),
    Field2 = put_optional_field(
        Field1,
        <<"expires_at">>,
        maps:get(<<"expires_at">>, CustomStatus, undefined)
    ),
    Field3 = put_optional_field(
        Field2,
        <<"emoji_id">>,
        maps:get(<<"emoji_id">>, CustomStatus, undefined)
    ),
    put_optional_field(
        Field3,
        <<"emoji_name">>,
        maps:get(<<"emoji_name">>, CustomStatus, undefined)
    ).

put_optional_field(Map, _Key, undefined) ->
    Map;
put_optional_field(Map, Key, Value) ->
    maps:put(Key, Value, Map).
