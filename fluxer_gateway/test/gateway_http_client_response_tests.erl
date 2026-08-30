%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_http_client_response_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(CIRCUIT_TABLE, gateway_http_circuit_breaker).
-define(CIRCUIT_WINDOW_TABLE, gateway_http_circuit_window).

allow_circuit_request_uses_recovery_timeout_test() ->
    cleanup_circuit_tables(),
    ensure_circuit_tables(),
    Key = {rpc, <<"example.test">>},
    Now = erlang:system_time(millisecond),
    OpenedAt = Now - 4000,
    ets:insert(?CIRCUIT_TABLE, {Key, open, OpenedAt, OpenedAt}),
    ?assertEqual(
        {error, circuit_open}, gateway_http_client_response:allow_circuit_request(Key, 5000)
    ),
    ?assertEqual(ok, gateway_http_client_response:allow_circuit_request(Key, 3000)),
    ?assertEqual(half_open, circuit_state(Key)),
    cleanup_circuit_tables().

update_circuit_state_uses_failure_threshold_test() ->
    cleanup_circuit_tables(),
    ensure_circuit_tables(),
    Key = {push, <<"push.example.test">>},
    record(Key, failure(), 2),
    ?assertEqual(closed, circuit_state(Key)),
    record(Key, failure(), 1),
    ?assertEqual(open, circuit_state(Key)),
    ?assertEqual(
        {error, circuit_open}, gateway_http_client_response:allow_circuit_request(Key, 60000)
    ),
    cleanup_circuit_tables().

closed_circuit_stays_closed_under_successes_test() ->
    cleanup_circuit_tables(),
    ensure_circuit_tables(),
    Key = {rpc, <<"healthy.example.test">>},
    record(Key, success(), 20),
    ?assertEqual(closed, circuit_state(Key)),
    ?assertEqual(ok, gateway_http_client_response:allow_circuit_request(Key, 5000)),
    cleanup_circuit_tables().

open_circuit_half_opens_then_recloses_on_success_test() ->
    cleanup_circuit_tables(),
    ensure_circuit_tables(),
    Key = {rpc, <<"recovering.example.test">>},
    record(Key, failure(), 3),
    ?assertEqual(open, circuit_state(Key)),
    age_opened_at(Key, 6000),
    ?assertEqual(ok, gateway_http_client_response:allow_circuit_request(Key, 5000)),
    ?assertEqual(half_open, circuit_state(Key)),
    ?assertEqual([], ets:lookup(?CIRCUIT_WINDOW_TABLE, Key)),
    record(Key, success(), 1),
    ?assertEqual(closed, circuit_state(Key)),
    ?assertEqual(ok, gateway_http_client_response:allow_circuit_request(Key, 5000)),
    cleanup_circuit_tables().

circuit_window_is_kept_out_of_the_state_record_test() ->
    cleanup_circuit_tables(),
    ensure_circuit_tables(),
    Key = {push, <<"window.example.test">>},
    record(Key, failure(), 1),
    ?assertMatch([{Key, closed, undefined, _}], ets:lookup(?CIRCUIT_TABLE, Key)),
    ?assertMatch([{Key, [{true, _}]}], ets:lookup(?CIRCUIT_WINDOW_TABLE, Key)),
    record(Key, success(), 1),
    ?assertMatch([{Key, [{false, _}, {true, _}]}], ets:lookup(?CIRCUIT_WINDOW_TABLE, Key)),
    cleanup_circuit_tables().

is_stale_circuit_uses_state_specific_timestamps_test() ->
    Now = 10000,
    ?assertEqual(
        true,
        gateway_http_client_response:is_stale_circuit({closed, undefined, 8000}, Now, 1000)
    ),
    ?assertEqual(
        false,
        gateway_http_client_response:is_stale_circuit({closed, undefined, 9500}, Now, 1000)
    ),
    ?assertEqual(
        true, gateway_http_client_response:is_stale_circuit({open, 8000, 9500}, Now, 1000)
    ),
    ?assertEqual(
        false, gateway_http_client_response:is_stale_circuit({open, 9500, 8000}, Now, 1000)
    ),
    ?assertEqual(
        false, gateway_http_client_response:is_stale_circuit({half_open, 1, 1}, Now, 1000)
    ).

failure() ->
    {ok, 503, [], <<>>}.

success() ->
    {ok, 200, [], <<>>}.

record(Key, Result, Count) ->
    lists:foreach(
        fun(_) ->
            ok = gateway_http_client_response:update_circuit_state_direct(Key, Result, 3)
        end,
        lists:seq(1, Count)
    ).

circuit_state(Key) ->
    [{Key, State, _OpenedAt, _UpdatedAt}] = ets:lookup(?CIRCUIT_TABLE, Key),
    State.

age_opened_at(Key, DeltaMs) ->
    [{Key, State, OpenedAt, UpdatedAt}] = ets:lookup(?CIRCUIT_TABLE, Key),
    ets:insert(?CIRCUIT_TABLE, {Key, State, OpenedAt - DeltaMs, UpdatedAt}),
    ok.

ensure_circuit_tables() ->
    ensure_table(?CIRCUIT_TABLE),
    ensure_table(?CIRCUIT_WINDOW_TABLE).

ensure_table(Name) ->
    case ets:whereis(Name) of
        undefined ->
            ets:new(Name, [named_table, public, set]),
            ok;
        _ ->
            ok
    end.

cleanup_circuit_tables() ->
    delete_table(?CIRCUIT_TABLE),
    delete_table(?CIRCUIT_WINDOW_TABLE).

delete_table(Name) ->
    try ets:delete(Name) of
        _ -> ok
    catch
        error:badarg -> ok
    end.
