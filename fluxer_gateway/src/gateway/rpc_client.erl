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

-export([
    call/1,
    call/2,
    get_rpc_url/0,
    get_rpc_url/1,
    get_rpc_headers/0
]).

-type rpc_request() :: map().
-type rpc_response() :: {ok, map()} | {error, term()}.

-spec call(rpc_request()) -> rpc_response().
call(Request) ->
    call(Request, #{}).

-spec call(rpc_request(), map()) -> rpc_response().
call(Request, _Options) ->
    Url = get_rpc_url(),
    Headers = get_rpc_headers(),
    Body = jsx:encode(Request),

    case
        hackney:request(post, Url, Headers, Body, [{recv_timeout, 30000}, {connect_timeout, 5000}])
    of
        {ok, 200, _RespHeaders, ClientRef} ->
            case hackney:body(ClientRef) of
                {ok, RespBody} ->
                    Response = jsx:decode(RespBody, [return_maps]),
                    Data = maps:get(<<"data">>, Response, #{}),
                    {ok, Data};
                {error, Reason} ->
                    logger:error("[rpc_client] Failed to read response body: ~p", [Reason]),
                    {error, {body_read_failed, Reason}}
            end;
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            case hackney:body(ClientRef) of
                {ok, RespBody} ->
                    hackney:close(ClientRef),
                    logger:error("[rpc_client] RPC request failed with status ~p: ~s", [
                        StatusCode, RespBody
                    ]),
                    {error, {http_error, StatusCode, RespBody}};
                {error, Reason} ->
                    hackney:close(ClientRef),
                    logger:error(
                        "[rpc_client] Failed to read error response body (status ~p): ~p", [
                            StatusCode, Reason
                        ]
                    ),
                    {error, {http_error, StatusCode, body_read_failed}}
            end;
        {error, Reason} ->
            logger:error("[rpc_client] RPC request failed: ~p", [Reason]),
            {error, Reason}
    end.

get_rpc_url() ->
    ApiHost = fluxer_gateway_env:get(api_host),
    get_rpc_url(ApiHost).

get_rpc_url(ApiHost) ->
    "http://" ++ ApiHost ++ "/_rpc".

get_rpc_headers() ->
    RpcSecretKey = fluxer_gateway_env:get(rpc_secret_key),
    [{<<"Authorization">>, <<"Bearer ", RpcSecretKey/binary>>}].
