%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_router).
-typing([eqwalizer]).

-export([execute/2]).

-spec execute(binary(), map()) -> term().
execute(Method, Params) ->
    route_method(Method, Params).

-spec route_method(binary(), map()) -> term().
route_method(<<"guild.", _/binary>> = Method, Params) ->
    gateway_rpc_guild:execute_method(Method, Params);
route_method(<<"presence.", _/binary>> = Method, Params) ->
    gateway_rpc_presence:execute_method(Method, Params);
route_method(<<"push.", _/binary>> = Method, Params) ->
    gateway_rpc_push:execute_method(Method, Params);
route_method(<<"call.", _/binary>> = Method, Params) ->
    gateway_rpc_call:execute_method(Method, Params);
route_method(<<"voice.", _/binary>> = Method, Params) ->
    gateway_rpc_voice:execute_method(Method, Params);
route_method(<<"process.", _/binary>> = Method, Params) ->
    gateway_rpc_misc:execute_method(Method, Params);
route_method(Method, _Params) ->
    gateway_rpc_error:raise(<<"Unknown method: ", Method/binary>>).
