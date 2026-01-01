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

-module(hot_reload_handler).

-export([init/2]).

-define(JSON_HEADERS, #{<<"content-type">> => <<"application/json">>}).
-define(MAX_MODULES, 600).
-define(MAX_BODY_BYTES, 26214400).

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
                    handle_reload(Decoded, Req1, State);
                {error, Status, ErrorBody, Req1} ->
                    respond(Status, ErrorBody, Req1, State)
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
            case os:getenv("GATEWAY_ADMIN_SECRET") of
                false ->
                    Req = cowboy_req:reply(
                        500,
                        ?JSON_HEADERS,
                        jsx:encode(#{<<"error">> => <<"GATEWAY_ADMIN_SECRET not configured">>}),
                        Req0
                    ),
                    {error, Req};
                Secret ->
                    Expected = <<"Bearer ", (list_to_binary(Secret))/binary>>,
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
    case cowboy_req:body_length(Req0) of
        Length when is_integer(Length), Length > ?MAX_BODY_BYTES ->
            {error, 413, #{<<"error">> => <<"Request body too large">>}, Req0};
        _ ->
            read_body(Req0, <<>>)
    end.

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0, #{length => 1048576}) of
        {ok, Body, Req1} ->
            FullBody = <<Acc/binary, Body/binary>>,
            decode_body(FullBody, Req1);
        {more, Body, Req1} ->
            NewAcc = <<Acc/binary, Body/binary>>,
            case byte_size(NewAcc) > ?MAX_BODY_BYTES of
                true ->
                    {error, 413, #{<<"error">> => <<"Request body too large">>}, Req1};
                false ->
                    read_body(Req1, NewAcc)
            end
    end.

decode_body(<<>>, Req0) ->
    {ok, #{}, Req0};
decode_body(Body, Req0) ->
    case catch jsx:decode(Body, [return_maps]) of
        {'EXIT', _Reason} ->
            {error, 400, #{<<"error">> => <<"Invalid JSON payload">>}, Req0};
        Decoded when is_map(Decoded) ->
            {ok, Decoded, Req0};
        _ ->
            {error, 400, #{<<"error">> => <<"Invalid request body">>}, Req0}
    end.

handle_reload(Params, Req0, State) ->
    try
        Purge = parse_purge(maps:get(<<"purge">>, Params, <<"soft">>)),
        case maps:get(<<"beams">>, Params, undefined) of
            undefined ->
                handle_modules_reload(Params, Purge, Req0, State);
            Beams when is_list(Beams) ->
                case length(Beams) =< ?MAX_MODULES of
                    true ->
                        Pairs = decode_beams(Beams),
                        {ok, Results} = hot_reload:reload_beams(Pairs, #{purge => Purge}),
                        respond(200, #{<<"results">> => Results}, Req0, State);
                    false ->
                        respond(400, #{<<"error">> => <<"Too many modules">>}, Req0, State)
                end;
            _ ->
                respond(400, #{<<"error">> => <<"beams must be an array">>}, Req0, State)
        end
    catch
        error:badarg ->
            respond(400, #{<<"error">> => <<"Invalid module name or beam payload">>}, Req0, State);
        error:invalid_beam ->
            respond(400, #{<<"error">> => <<"Invalid module name or beam payload">>}, Req0, State);
        error:{beam_module_mismatch, _, _} ->
            respond(400, #{<<"error">> => <<"Invalid module name or beam payload">>}, Req0, State);
        _:Reason ->
            logger:error("hot_reload_handler: Error during reload: ~p", [Reason]),
            respond(500, #{<<"error">> => <<"Internal error">>}, Req0, State)
    end.

handle_modules_reload(Params, Purge, Req0, State) ->
    case maps:get(<<"modules">>, Params, []) of
        [] ->
            {ok, Results} = hot_reload:reload_all_changed(Purge),
            respond(200, #{<<"results">> => Results}, Req0, State);
        Modules when is_list(Modules) ->
            case length(Modules) =< ?MAX_MODULES of
                true ->
                    ModuleAtoms = lists:map(fun to_module_atom/1, Modules),
                    {ok, Results} = hot_reload:reload_modules(ModuleAtoms, #{purge => Purge}),
                    respond(200, #{<<"results">> => Results}, Req0, State);
                false ->
                    respond(400, #{<<"error">> => <<"Too many modules">>}, Req0, State)
            end;
        _ ->
            respond(400, #{<<"error">> => <<"modules must be an array">>}, Req0, State)
    end.

decode_beams(Beams) ->
    lists:map(
        fun(Elem) ->
            case Elem of
                #{<<"module">> := Mod0, <<"beam_b64">> := B640} ->
                    ModBin = to_binary(Mod0),
                    Module = to_module_atom(ModBin),
                    B64Bin = to_binary(B640),
                    BeamBin = base64:decode(B64Bin),
                    case beam_lib:md5(BeamBin) of
                        {ok, {Module, _}} -> ok;
                        {ok, {Other, _}} -> erlang:error({beam_module_mismatch, Module, Other});
                        _ -> erlang:error(invalid_beam)
                    end,
                    {Module, BeamBin};
                _ ->
                    erlang:error(badarg)
            end
        end,
        Beams
    ).

to_binary(B) when is_binary(B) ->
    B;
to_binary(L) when is_list(L) ->
    list_to_binary(L);
to_binary(_) ->
    erlang:error(badarg).

parse_purge(<<"none">>) -> none;
parse_purge(<<"soft">>) -> soft;
parse_purge(<<"hard">>) -> hard;
parse_purge(none) -> none;
parse_purge(soft) -> soft;
parse_purge(hard) -> hard;
parse_purge(_) -> soft.

to_module_atom(B) when is_binary(B) ->
    case is_allowed_module_name(B) of
        true -> erlang:binary_to_atom(B, utf8);
        false -> erlang:error(badarg)
    end;
to_module_atom(L) when is_list(L) ->
    to_module_atom(list_to_binary(L));
to_module_atom(_) ->
    erlang:error(badarg).

is_allowed_module_name(Bin) when is_binary(Bin) ->
    byte_size(Bin) > 0 andalso byte_size(Bin) < 128 andalso
        is_safe_chars(Bin) andalso has_allowed_prefix(Bin).

is_safe_chars(Bin) ->
    lists:all(
        fun(C) ->
            (C >= $a andalso C =< $z) orelse
                (C >= $0 andalso C =< $9) orelse
                (C =:= $_)
        end,
        binary_to_list(Bin)
    ).

has_allowed_prefix(Bin) ->
    Prefixes = [
        <<"fluxer_">>,
        <<"gateway">>,
        <<"session">>,
        <<"guild">>,
        <<"presence">>,
        <<"push">>,
        <<"call">>,
        <<"health">>,
        <<"hot_reload">>,
        <<"rpc_client">>,
        <<"rendezvous">>,
        <<"process_">>,
        <<"metrics_">>,
        <<"dm_voice">>,
        <<"voice_">>,
        <<"constants">>,
        <<"validation">>,
        <<"backoff_">>,
        <<"list_ops">>,
        <<"map_utils">>,
        <<"type_conv">>,
        <<"utils">>,
        <<"user_utils">>,
        <<"custom_status">>
    ],
    lists:any(
        fun(P) ->
            Sz = byte_size(P),
            byte_size(Bin) >= Sz andalso binary:part(Bin, 0, Sz) =:= P
        end,
        Prefixes
    ).

respond(Status, Body, Req0, State) ->
    Req = cowboy_req:reply(Status, ?JSON_HEADERS, jsx:encode(Body), Req0),
    {ok, Req, State}.
