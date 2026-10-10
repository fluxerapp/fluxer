%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(rpc_client_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

rpc_headers_uses_request_ip_test() ->
    Headers = rpc_client:rpc_headers(#{
        <<"type">> => <<"session">>,
        <<"ip">> => <<"203.0.113.7">>
    }),
    ?assertEqual(<<"203.0.113.7">>, proplists:get_value(<<"x-forwarded-for">>, Headers)).

rpc_headers_falls_back_to_loopback_without_request_ip_test() ->
    Headers = rpc_client:rpc_headers(#{<<"type">> => <<"guild_collection">>}),
    ?assertEqual(<<"127.0.0.1">>, proplists:get_value(<<"x-forwarded-for">>, Headers)).

is_retryable_5xx_test() ->
    ?assert(rpc_client:is_retryable({rpc_error, 500, <<"Internal server error">>})),
    ?assert(rpc_client:is_retryable({rpc_error, 502, <<"Bad gateway">>})),
    ?assert(rpc_client:is_retryable({rpc_error, 503, <<"Service unavailable">>})).

is_retryable_4xx_test() ->
    ?assertNot(rpc_client:is_retryable({rpc_error, 401, <<"Unauthorized">>})),
    ?assertNot(rpc_client:is_retryable({rpc_error, 404, <<"Not found">>})),
    ?assertNot(rpc_client:is_retryable({rpc_error, 429, <<"Rate limited">>})).

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
