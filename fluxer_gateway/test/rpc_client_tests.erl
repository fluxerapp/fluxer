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
