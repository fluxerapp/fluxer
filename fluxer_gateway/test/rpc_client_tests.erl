%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(rpc_client_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

handle_http_response_ok_test() ->
    Response = json:encode(#{
        <<"type">> => <<"session">>,
        <<"data">> => #{<<"user">> => <<"test">>}
    }),
    ?assertEqual(
        {ok, #{<<"user">> => <<"test">>}}, rpc_client:handle_http_response(200, Response)
    ).

handle_http_response_error_401_test() ->
    Response = json:encode(#{<<"message">> => <<"Unauthorized">>}),
    ?assertEqual(
        {error, {rpc_error, 401, <<"Unauthorized">>}},
        rpc_client:handle_http_response(401, Response)
    ).

handle_http_response_error_non_json_test() ->
    ?assertEqual(
        {error, {rpc_error, 503, <<"unavailable">>}},
        rpc_client:handle_http_response(503, <<"unavailable">>)
    ).

rpc_headers_uses_request_ip_test() ->
    Headers = rpc_client:rpc_headers(#{
        <<"type">> => <<"session">>,
        <<"ip">> => <<"203.0.113.7">>
    }),
    ?assertEqual(<<"203.0.113.7">>, proplists:get_value(<<"x-forwarded-for">>, Headers)).

rpc_headers_falls_back_to_loopback_without_request_ip_test() ->
    Headers = rpc_client:rpc_headers(#{<<"type">> => <<"guild_collection">>}),
    ?assertEqual(<<"127.0.0.1">>, proplists:get_value(<<"x-forwarded-for">>, Headers)).

rpc_url_uses_internal_api_when_endpoint_is_undefined_test() ->
    OldConfig = fluxer_gateway_env:get_map(),
    try
        _ = fluxer_gateway_env:patch(#{
            api_internal_url => <<"http://127.0.0.1:8080">>,
            api_rpc_endpoint => undefined
        }),
        ?assertEqual(<<"http://127.0.0.1:8080/internal/rpc">>, rpc_client:rpc_url())
    after
        _ = fluxer_gateway_env:update(fun(_) -> OldConfig end)
    end.

rpc_url_uses_configured_endpoint_without_trailing_slash_test() ->
    OldConfig = fluxer_gateway_env:get_map(),
    try
        _ = fluxer_gateway_env:patch(#{
            api_internal_url => <<"http://127.0.0.1:8080">>,
            api_rpc_endpoint => <<"http://api.internal/internal/rpc/">>
        }),
        ?assertEqual(<<"http://api.internal/internal/rpc">>, rpc_client:rpc_url())
    after
        _ = fluxer_gateway_env:update(fun(_) -> OldConfig end)
    end.

rpc_url_meta_matches_derive_path_test() ->
    OldConfig = fluxer_gateway_env:get_map(),
    Endpoints = [
        <<"http://fluxer-api.fluxer.svc.cluster.local/internal/rpc">>,
        <<"https://api.internal/internal/rpc">>,
        <<"http://fluxer-api.fluxer.svc.cluster.local:8080/internal/rpc">>,
        <<"HTTPS://Fluxer-API.Fluxer.SVC.Cluster.Local:8443/internal/rpc">>
    ],
    try
        lists:foreach(fun assert_rpc_url_meta_for_endpoint/1, Endpoints)
    after
        _ = fluxer_gateway_env:update(fun(_) -> OldConfig end)
    end.

assert_rpc_url_meta_for_endpoint(Endpoint) ->
    _ = fluxer_gateway_env:patch(#{api_rpc_endpoint => Endpoint}),
    Url = rpc_client:rpc_url(),
    {HostKey, IsHttps} = gateway_http_client_request:url_metadata(Url),
    ?assertEqual(HostKey, gateway_http_client_request:extract_host_key(Url)),
    ?assertEqual({Url, HostKey, IsHttps}, rpc_client:rpc_url_meta()),
    ?assertEqual({Url, HostKey, IsHttps}, rpc_client:rpc_url_meta()).

rpc_url_meta_expected_values_test() ->
    OldConfig = fluxer_gateway_env:get_map(),
    try
        _ = fluxer_gateway_env:patch(#{
            api_rpc_endpoint =>
                <<"https://Fluxer-API.Fluxer.SVC.Cluster.Local:8443/internal/rpc">>
        }),
        ?assertEqual(
            {
                <<"https://Fluxer-API.Fluxer.SVC.Cluster.Local:8443/internal/rpc">>,
                <<"fluxer-api.fluxer.svc.cluster.local">>,
                true
            },
            rpc_client:rpc_url_meta()
        )
    after
        _ = fluxer_gateway_env:update(fun(_) -> OldConfig end)
    end.

is_retryable_timeout_test() ->
    ?assert(rpc_client:is_retryable(timeout)).

is_retryable_no_responders_test() ->
    ?assert(rpc_client:is_retryable(no_responders)).

is_retryable_5xx_test() ->
    ?assert(rpc_client:is_retryable({rpc_error, 500, <<"Internal server error">>})),
    ?assert(rpc_client:is_retryable({rpc_error, 502, <<"Bad gateway">>})),
    ?assert(rpc_client:is_retryable({rpc_error, 503, <<"Service unavailable">>})).

is_retryable_4xx_test() ->
    ?assertNot(rpc_client:is_retryable({rpc_error, 401, <<"Unauthorized">>})),
    ?assertNot(rpc_client:is_retryable({rpc_error, 404, <<"Not found">>})),
    ?assertNot(rpc_client:is_retryable({rpc_error, 429, <<"Rate limited">>})).

is_retryable_other_test() ->
    ?assertNot(rpc_client:is_retryable(not_connected)),
    ?assertNot(rpc_client:is_retryable({rpc_error, 400, <<"Bad request">>})).

backoff_delay_exponential_test() ->
    Config = {3, 1000, 30000, 0},
    ?assertEqual(1000, rpc_client:backoff_delay(1, Config)),
    ?assertEqual(2000, rpc_client:backoff_delay(2, Config)),
    ?assertEqual(4000, rpc_client:backoff_delay(3, Config)).

backoff_delay_caps_at_max_test() ->
    Config = {3, 1000, 3000, 0},
    ?assertEqual(1000, rpc_client:backoff_delay(1, Config)),
    ?assertEqual(2000, rpc_client:backoff_delay(2, Config)),
    ?assertEqual(3000, rpc_client:backoff_delay(3, Config)).

backoff_delay_includes_jitter_test() ->
    Config = {3, 1000, 30000, 500},
    Delay = rpc_client:backoff_delay(1, Config),
    ?assert(Delay >= 1000),
    ?assert(Delay =< 1500).

handle_http_response_ok_with_no_data_key_test() ->
    Response = json:encode(#{<<"type">> => <<"ok">>}),
    ?assertEqual({ok, #{}}, rpc_client:handle_http_response(200, Response)).

handle_http_response_500_is_retryable_test() ->
    ?assert(rpc_client:is_retryable({rpc_error, 500, <<"error">>})),
    ?assert(rpc_client:is_retryable({rpc_error, 503, <<"unavailable">>})).

handle_http_response_400_not_retryable_test() ->
    ?assertNot(rpc_client:is_retryable({rpc_error, 400, <<"bad request">>})),
    ?assertNot(rpc_client:is_retryable({rpc_error, 403, <<"forbidden">>})).
