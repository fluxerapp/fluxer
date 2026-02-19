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

-module(rpc_client).

-export([call/1]).

-define(NATS_RPC_SUBJECT, <<"rpc.api">>).
-define(NATS_RPC_TIMEOUT_MS, 10000).

-type rpc_request() :: map().
-type rpc_response() :: {ok, map()} | {error, term()}.

-spec call(rpc_request()) -> rpc_response().
call(Request) ->
    case gateway_nats_rpc:get_connection() of
        {ok, undefined} ->
            {error, not_connected};
        {ok, Conn} ->
            do_request(Conn, Request);
        {error, Reason} ->
            {error, {not_connected, Reason}}
    end.

-spec do_request(nats:conn(), rpc_request()) -> rpc_response().
do_request(Conn, Request) ->
    Payload = iolist_to_binary(json:encode(Request)),
    case nats:request(Conn, ?NATS_RPC_SUBJECT, Payload, #{timeout => ?NATS_RPC_TIMEOUT_MS}) of
        {ok, {ResponseBin, _MsgOpts}} ->
            handle_nats_response(ResponseBin);
        {error, timeout} ->
            {error, timeout};
        {error, no_responders} ->
            {error, no_responders};
        {error, Reason} ->
            {error, Reason}
    end.

-spec handle_nats_response(iodata()) -> rpc_response().
handle_nats_response(ResponseBin) ->
    Response = json:decode(iolist_to_binary(ResponseBin)),
    case maps:get(<<"_error">>, Response, undefined) of
        undefined ->
            Data = maps:get(<<"data">>, Response, #{}),
            {ok, Data};
        _ ->
            Status = maps:get(<<"status">>, Response, 500),
            Message = maps:get(<<"message">>, Response, <<"unknown error">>),
            {error, {rpc_error, Status, Message}}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

handle_nats_response_ok_test() ->
    Response = json:encode(#{
        <<"type">> => <<"session">>,
        <<"data">> => #{<<"user">> => <<"test">>}
    }),
    ?assertEqual({ok, #{<<"user">> => <<"test">>}}, handle_nats_response(Response)).

handle_nats_response_error_401_test() ->
    Response = json:encode(#{<<"_error">> => true, <<"status">> => 401, <<"message">> => <<"Unauthorized">>}),
    ?assertEqual({error, {rpc_error, 401, <<"Unauthorized">>}}, handle_nats_response(Response)).

handle_nats_response_error_429_test() ->
    Response = json:encode(#{<<"_error">> => true, <<"status">> => 429, <<"message">> => <<"Rate limited">>}),
    ?assertEqual({error, {rpc_error, 429, <<"Rate limited">>}}, handle_nats_response(Response)).

handle_nats_response_error_500_test() ->
    Response = json:encode(#{<<"_error">> => true, <<"status">> => 500, <<"message">> => <<"Internal error">>}),
    ?assertEqual({error, {rpc_error, 500, <<"Internal error">>}}, handle_nats_response(Response)).

-endif.
