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

-module(gateway_rpc_http_handler).

-export([init/2]).

-define(JSON_HEADERS, #{<<"content-type">> => <<"application/json">>}).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            handle_post(Req0, State);
        _ ->
            Req = cowboy_req:reply(405, #{<<"allow">> => <<"POST">>}, <<>>, Req0),
            {ok, Req, State}
    end.

handle_post(Req0, State) ->
    case authorize(Req0) of
        ok ->
            case read_body(Req0) of
                {ok, Decoded, Req1} ->
                    case maps:get(<<"method">>, Decoded, undefined) of
                        undefined ->
                            respond(400, #{<<"error">> => <<"Missing method">>}, Req1, State);
                        Method when is_binary(Method) ->
                            ParamsValue = maps:get(<<"params">>, Decoded, #{}),
                            case is_map(ParamsValue) of
                                true ->
                                    execute_method(Method, ParamsValue, Req1, State);
                                false ->
                                    respond(
                                        400, #{<<"error">> => <<"Invalid params">>}, Req1, State
                                    )
                            end;
                        _ ->
                            respond(400, #{<<"error">> => <<"Invalid method">>}, Req1, State)
                    end;
                {error, ErrorBody, Req1} ->
                    respond(400, ErrorBody, Req1, State)
            end;
        {error, Req1} ->
            {ok, Req1, State}
    end.

authorize(Req0) ->
    case cowboy_req:header(<<"authorization">>, Req0) of
        undefined ->
            Req = cowboy_req:reply(
                401,
                ?JSON_HEADERS,
                jsx:encode(#{<<"error">> => <<"Unauthorized">>}),
                Req0
            ),
            {error, Req};
        AuthHeader ->
            case fluxer_gateway_env:get(rpc_secret_key) of
                undefined ->
                    Req = cowboy_req:reply(
                        500,
                        ?JSON_HEADERS,
                        jsx:encode(#{<<"error">> => <<"RPC secret not configured">>}),
                        Req0
                    ),
                    {error, Req};
                Secret when is_binary(Secret) ->
                    Expected = <<"Bearer ", Secret/binary>>,
                    case AuthHeader of
                        Expected ->
                            ok;
                        _ ->
                            Req = cowboy_req:reply(
                                401,
                                ?JSON_HEADERS,
                                jsx:encode(#{<<"error">> => <<"Unauthorized">>}),
                                Req0
                            ),
                            {error, Req}
                    end
            end
    end.

read_body(Req0) ->
    read_body(Req0, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Body, Req1} ->
            FullBody = <<Acc/binary, Body/binary>>,
            decode_body(FullBody, Req1);
        {more, Body, Req1} ->
            read_body(Req1, <<Acc/binary, Body/binary>>)
    end.

decode_body(Body, Req0) ->
    case catch jsx:decode(Body, [return_maps]) of
        {'EXIT', _Reason} ->
            {error, #{<<"error">> => <<"Invalid JSON payload">>}, Req0};
        Decoded when is_map(Decoded) ->
            {ok, Decoded, Req0};
        _ ->
            {error, #{<<"error">> => <<"Invalid request body">>}, Req0}
    end.

execute_method(Method, Params, Req0, State) ->
    try
        Result = gateway_rpc_router:execute(Method, Params),
        respond(200, #{<<"result">> => Result}, Req0, State)
    catch
        throw:{error, Message} ->
            respond(400, #{<<"error">> => Message}, Req0, State);
        _:_ ->
            respond(500, #{<<"error">> => <<"Internal error">>}, Req0, State)
    end.

respond(Status, Body, Req0, State) ->
    Req = cowboy_req:reply(Status, ?JSON_HEADERS, jsx:encode(Body), Req0),
    {ok, Req, State}.
