%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(custom_status_validation).
-typing([eqwalizer]).

-export([validate/2]).

-spec validate(integer(), map() | null) -> {ok, map() | null} | {error, term()}.
validate(_UserId, null) ->
    {ok, null};
validate(UserId, CustomStatus) when is_map(CustomStatus) ->
    Request = build_request(UserId, CustomStatus),
    rpc_client:call(Request).

-spec build_request(integer(), map()) -> map().
build_request(UserId, CustomStatus) ->
    #{
        <<"type">> => <<"validate_custom_status">>,
        <<"user_id">> => type_conv:to_binary(UserId),
        <<"custom_status">> => build_custom_status_payload(CustomStatus)
    }.

-spec build_custom_status_payload(map()) -> map().
build_custom_status_payload(CustomStatus) ->
    Fields = [<<"text">>, <<"expires_at">>, <<"emoji_id">>, <<"emoji_name">>],
    maps:filter(fun(_Key, Value) -> Value =/= undefined end, maps:with(Fields, CustomStatus)).
