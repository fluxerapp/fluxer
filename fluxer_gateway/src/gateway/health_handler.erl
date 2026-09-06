%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(health_handler).
-typing([eqwalizer]).

-export([init/2]).

-type mode() :: liveness | readiness | drain | undrain.

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req0, Mode0) ->
    Mode = normalize_mode(Mode0),
    {StatusCode, Body} = response_for_mode(Mode, Req0),
    Req = cowboy_req:reply(
        StatusCode,
        gateway_build_info:version_headers(#{<<"content-type">> => <<"text/plain">>}),
        Body,
        Req0
    ),
    {ok, Req, Mode}.

-spec normalize_mode(term()) -> mode().
normalize_mode(drain) -> drain;
normalize_mode(undrain) -> undrain;
normalize_mode(readiness) -> readiness;
normalize_mode(_) -> liveness.

-spec response_for_mode(mode(), cowboy_req:req()) -> {200 | 403 | 409 | 503, binary()}.
response_for_mode(liveness, _Req) ->
    {200, <<"OK">>};
response_for_mode(readiness, Req) ->
    readiness_response(Req);
response_for_mode(drain, Req) ->
    drain_response(Req);
response_for_mode(undrain, Req) ->
    undrain_response(Req).

-spec readiness_response(cowboy_req:req()) -> {200 | 403 | 503, binary()}.
readiness_response(Req) ->
    case is_loopback_request(Req) of
        false ->
            {403, <<"FORBIDDEN">>};
        true ->
            readiness_status(gateway_node_router:is_ready())
    end.

-spec readiness_status(boolean()) -> {200 | 503, binary()}.
readiness_status(true) ->
    {200, <<"OK">>};
readiness_status(false) ->
    {503, <<"DRAINING">>}.

-spec drain_response(cowboy_req:req()) -> {200 | 403, binary()}.
drain_response(Req) ->
    case is_loopback_request(Req) of
        false ->
            {403, <<"FORBIDDEN">>};
        true ->
            ok = activate_drain(),
            {200, <<"DRAINING">>}
    end.

-spec undrain_response(cowboy_req:req()) -> {200 | 403 | 409, binary()}.
undrain_response(Req) ->
    case is_loopback_request(Req) of
        false ->
            {403, <<"FORBIDDEN">>};
        true ->
            undrain_status(deactivate_drain())
    end.

-spec undrain_status(term()) -> {200 | 409, binary()}.
undrain_status(ok) ->
    {200, <<"READY">>};
undrain_status({error, handoff_in_flight}) ->
    {409, <<"HANDOFF_IN_FLIGHT">>};
undrain_status(_Error) ->
    {409, <<"UNAVAILABLE">>}.

-spec activate_drain() -> ok.
activate_drain() ->
    gateway_cluster_handoff:drain_async().

-spec deactivate_drain() -> ok | {error, term()}.
deactivate_drain() ->
    gateway_cluster_handoff:undrain().

-spec is_loopback_request(cowboy_req:req()) -> boolean().
is_loopback_request(Req) ->
    case cowboy_req:peer(Req) of
        {{127, 0, 0, 1}, _Port} ->
            true;
        {{0, 0, 0, 0, 0, 0, 0, 1}, _Port} ->
            true;
        _ ->
            false
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

normalize_mode_test() ->
    ?assertEqual(liveness, normalize_mode(undefined)),
    ?assertEqual(liveness, normalize_mode([])),
    ?assertEqual(readiness, normalize_mode(readiness)),
    ?assertEqual(drain, normalize_mode(drain)),
    ?assertEqual(undrain, normalize_mode(undrain)).

readiness_status_test() ->
    ?assertEqual({200, <<"OK">>}, readiness_status(true)),
    ?assertEqual({503, <<"DRAINING">>}, readiness_status(false)).

undrain_status_test() ->
    ?assertEqual({200, <<"READY">>}, undrain_status(ok)),
    ?assertEqual({409, <<"HANDOFF_IN_FLIGHT">>}, undrain_status({error, handoff_in_flight})),
    ?assertEqual({409, <<"UNAVAILABLE">>}, undrain_status({error, unavailable})).

activate_drain_sets_draining_flag_test() ->
    persistent_term:erase({fluxer_gateway, draining}),
    ?assertEqual(ok, activate_drain()),
    ?assert(gateway_node_router:is_draining()),
    persistent_term:erase({fluxer_gateway, draining}).

deactivate_drain_clears_draining_flag_test() ->
    persistent_term:erase({fluxer_gateway, draining}),
    ?assertEqual(ok, activate_drain()),
    ?assert(gateway_node_router:is_draining()),
    ?assertEqual(ok, deactivate_drain()),
    ?assertNot(gateway_node_router:is_draining()),
    ?assertEqual({200, <<"OK">>}, readiness_status(gateway_node_router:is_ready())),
    persistent_term:erase({fluxer_gateway, draining}).

-endif.
