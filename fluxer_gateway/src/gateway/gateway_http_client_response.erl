%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_http_client_response).
-typing([eqwalizer]).

-export([
    allow_circuit_request/2,
    update_circuit_state_direct/3,
    acquire_inflight_slot/2,
    release_inflight_slot/1,
    prune_circuit_table/0,
    is_stale_circuit/3
]).
-export_type([response/0, circuit/0]).

-define(CIRCUIT_TABLE, gateway_http_circuit_breaker).
-define(CIRCUIT_WINDOW_TABLE, gateway_http_circuit_window).
-define(INFLIGHT_TABLE, gateway_http_inflight).
-define(CIRCUIT_STATE_POS, 2).

-define(CB_WINDOW_MS, 10000).
-define(CB_FAILURE_RATE_PCT, 80).

-type response() :: {ok, non_neg_integer(), [{binary(), binary()}], binary()} | {error, term()}.
-type circuit_state() :: closed | open | half_open.
-type circuit() :: {circuit_state(), integer() | undefined, integer()}.
-type window_entry() :: {boolean(), integer()}.

-spec allow_circuit_request({atom(), binary()}, pos_integer()) -> ok | {error, circuit_open}.
allow_circuit_request(CircuitKey, RecoveryTimeoutMs) ->
    case circuit_state(CircuitKey) of
        open ->
            maybe_transition_half_open(CircuitKey, RecoveryTimeoutMs);
        _ ->
            ok
    end.

-spec update_circuit_state_direct({atom(), binary()}, response(), pos_integer()) -> ok.
update_circuit_state_direct(CircuitKey, Result, FailureThreshold) ->
    Now = erlang:system_time(millisecond),
    IsFailure = is_countable_circuit_failure(CircuitKey, Result),
    record_result(CircuitKey, IsFailure, Now, FailureThreshold).

-spec acquire_inflight_slot(atom(), pos_integer()) -> ok | {error, overloaded}.
acquire_inflight_slot(Workload, MaxConcurrency) ->
    case safe_update_counter(?INFLIGHT_TABLE, Workload, {2, 1}) of
        {ok, Count} when Count =< MaxConcurrency ->
            ok;
        {ok, _Count} ->
            _ = safe_update_counter(?INFLIGHT_TABLE, Workload, {2, -1}),
            {error, overloaded};
        {error, _Reason} ->
            {error, overloaded}
    end.

-spec release_inflight_slot(atom()) -> ok.
release_inflight_slot(Workload) ->
    case safe_update_counter(?INFLIGHT_TABLE, Workload, {2, -1}) of
        {ok, V} when V < 0 ->
            reset_inflight_slot(Workload);
        _ ->
            ok
    end.

-spec reset_inflight_slot(atom()) -> ok.
reset_inflight_slot(Workload) ->
    try ets:insert(?INFLIGHT_TABLE, {Workload, 0}) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec prune_circuit_table() -> ok.
prune_circuit_table() ->
    Now = erlang:system_time(millisecond),
    MaxAgeMs = gateway_http_client:cleanup_max_age_ms(),
    ok = ensure_named_table(?CIRCUIT_TABLE),
    ok = ensure_named_table(?CIRCUIT_WINDOW_TABLE),
    try
        _ = ets:foldl(
            fun(Record, Acc) ->
                prune_circuit_record(Record, Now, MaxAgeMs),
                Acc
            end,
            ok,
            ?CIRCUIT_TABLE
        ),
        ok
    catch
        error:badarg -> ok
    end.

-spec prune_circuit_record(tuple(), integer(), integer()) -> ok.
prune_circuit_record({Key, State, OpenedAt, UpdatedAt}, Now, MaxAgeMs) ->
    delete_stale_circuit(Key, {State, OpenedAt, UpdatedAt}, Now, MaxAgeMs);
prune_circuit_record(_Record, _Now, _MaxAgeMs) ->
    ok.

-spec delete_stale_circuit({atom(), binary()}, circuit(), integer(), integer()) -> ok.
delete_stale_circuit(Key, Circuit, Now, MaxAgeMs) ->
    case is_stale_circuit(Circuit, Now, MaxAgeMs) of
        true ->
            safe_delete(?CIRCUIT_TABLE, Key),
            clear_window(Key);
        false ->
            ok
    end.

-spec is_stale_circuit(circuit(), integer(), integer()) -> boolean().
is_stale_circuit({open, OpenedAt, _UpdatedAt}, Now, MaxAgeMs) when is_integer(OpenedAt) ->
    Now - OpenedAt > MaxAgeMs;
is_stale_circuit({closed, _OpenedAt, UpdatedAt}, Now, MaxAgeMs) when is_integer(UpdatedAt) ->
    Now - UpdatedAt > MaxAgeMs;
is_stale_circuit(_Circuit, _Now, _MaxAgeMs) ->
    false.

-spec maybe_transition_half_open({atom(), binary()}, pos_integer()) ->
    ok | {error, circuit_open}.
maybe_transition_half_open(CircuitKey, RecoveryTimeoutMs) ->
    Now = erlang:system_time(millisecond),
    case lookup_circuit(CircuitKey) of
        {open, OpenedAt, _UpdatedAt} when is_integer(OpenedAt) ->
            transition_half_open(CircuitKey, OpenedAt, Now, RecoveryTimeoutMs);
        _ ->
            ok
    end.

-spec transition_half_open({atom(), binary()}, integer(), integer(), pos_integer()) ->
    ok | {error, circuit_open}.
transition_half_open(CircuitKey, OpenedAt, Now, RecoveryTimeoutMs) ->
    case Now - OpenedAt >= RecoveryTimeoutMs of
        true ->
            clear_window(CircuitKey),
            insert_circuit(CircuitKey, half_open, OpenedAt, Now);
        false ->
            {error, circuit_open}
    end.

-spec circuit_state({atom(), binary()}) -> circuit_state().
circuit_state(CircuitKey) ->
    try ets:lookup_element(?CIRCUIT_TABLE, CircuitKey, ?CIRCUIT_STATE_POS, closed) of
        open -> open;
        half_open -> half_open;
        _ -> closed
    catch
        error:badarg -> closed
    end.

-spec lookup_circuit({atom(), binary()}) -> circuit() | none.
lookup_circuit(CircuitKey) ->
    try ets:lookup(?CIRCUIT_TABLE, CircuitKey) of
        [{_Key, State, OpenedAt, UpdatedAt}] -> {State, OpenedAt, UpdatedAt};
        _ -> none
    catch
        error:badarg -> none
    end.

-spec lookup_window({atom(), binary()}) -> [window_entry()].
lookup_window(CircuitKey) ->
    try ets:lookup(?CIRCUIT_WINDOW_TABLE, CircuitKey) of
        [{_Key, Results}] -> Results;
        _ -> []
    catch
        error:badarg -> []
    end.

-spec safe_update_counter(atom(), term(), {pos_integer(), integer()}) ->
    {ok, integer()} | {error, term()}.
safe_update_counter(Table, Key, Op) ->
    try
        {ok, ets:update_counter(Table, Key, Op, {Key, 0})}
    catch
        error:badarg ->
            ok = gateway_http_client:ensure_started(),
            ok = ensure_named_table(Table),
            retry_update_counter(Table, Key, Op)
    end.

-spec retry_update_counter(atom(), term(), {pos_integer(), integer()}) ->
    {ok, integer()} | {error, badarg}.
retry_update_counter(Table, Key, Op) ->
    try
        {ok, ets:update_counter(Table, Key, Op, {Key, 0})}
    catch
        error:badarg -> {error, badarg}
    end.

-spec is_countable_circuit_failure({atom(), binary()}, response()) -> boolean().
is_countable_circuit_failure({rpc, _Host}, Result) ->
    is_countable_failure_with_transport(Result);
is_countable_circuit_failure({_Workload, _Host}, Result) ->
    is_countable_failure_without_transport(Result).

-spec is_countable_failure_without_transport(response()) -> boolean().
is_countable_failure_without_transport({error, nxdomain}) -> false;
is_countable_failure_without_transport({error, {failed_connect, _}}) -> false;
is_countable_failure_without_transport({error, timeout}) -> false;
is_countable_failure_without_transport({error, {timeout, _}}) -> false;
is_countable_failure_without_transport({error, _}) -> true;
is_countable_failure_without_transport({ok, StatusCode, _, _}) when StatusCode >= 500 -> true;
is_countable_failure_without_transport(_) -> false.

-spec is_countable_failure_with_transport(response()) -> boolean().
is_countable_failure_with_transport({error, nxdomain}) -> true;
is_countable_failure_with_transport({error, {failed_connect, _}}) -> true;
is_countable_failure_with_transport({error, timeout}) -> true;
is_countable_failure_with_transport({error, {timeout, _}}) -> true;
is_countable_failure_with_transport({error, _}) -> true;
is_countable_failure_with_transport({ok, StatusCode, _, _}) when StatusCode >= 500 -> true;
is_countable_failure_with_transport(_) -> false.

-spec record_result({atom(), binary()}, boolean(), integer(), pos_integer()) -> ok.
record_result(CircuitKey, IsFailure, Now, FailureThreshold) ->
    Entry = {IsFailure, Now},
    case lookup_circuit(CircuitKey) of
        none ->
            record_new(CircuitKey, Entry, Now);
        {half_open, _OpenedAt, _UpdatedAt} ->
            record_half_open(CircuitKey, IsFailure, Entry, Now);
        {open, OpenedAt, _UpdatedAt} ->
            insert_circuit(CircuitKey, open, OpenedAt, Now);
        {_State, _OpenedAt, _UpdatedAt} ->
            record_closed(CircuitKey, Entry, Now, FailureThreshold)
    end.

-spec record_new({atom(), binary()}, window_entry(), integer()) -> ok.
record_new(CircuitKey, Entry, Now) ->
    insert_window(CircuitKey, [Entry]),
    insert_circuit(CircuitKey, closed, undefined, Now).

-spec record_half_open({atom(), binary()}, boolean(), window_entry(), integer()) -> ok.
record_half_open(CircuitKey, false, Entry, Now) ->
    insert_window(CircuitKey, [Entry]),
    insert_circuit(CircuitKey, closed, undefined, Now);
record_half_open(CircuitKey, true, _Entry, Now) ->
    insert_circuit(CircuitKey, open, Now, Now).

-spec record_closed({atom(), binary()}, window_entry(), integer(), pos_integer()) -> ok.
record_closed(CircuitKey, Entry, Now, FailureThreshold) ->
    Cutoff = Now - ?CB_WINDOW_MS,
    Pruned = [R || {_, T} = R <- lookup_window(CircuitKey), T > Cutoff],
    NewResults = lists:sublist([Entry | Pruned], erlang:max(100, FailureThreshold)),
    insert_window(CircuitKey, NewResults),
    case has_open_failure_rate(NewResults, FailureThreshold) of
        true ->
            open_circuit(CircuitKey, NewResults, Now);
        false ->
            insert_circuit(CircuitKey, closed, undefined, Now)
    end.

-spec has_open_failure_rate([window_entry()], pos_integer()) -> boolean().
has_open_failure_rate(NewResults, FailureThreshold) ->
    Total = length(NewResults),
    Failures = length([1 || {true, _} <- NewResults]),
    Rate = (Failures * 100) div Total,
    Failures >= FailureThreshold andalso Rate >= ?CB_FAILURE_RATE_PCT.

-spec open_circuit({atom(), binary()}, [window_entry()], integer()) -> ok.
open_circuit(CircuitKey, NewResults, Now) ->
    Total = length(NewResults),
    Failures = length([1 || {true, _} <- NewResults]),
    Rate = (Failures * 100) div Total,
    logger:warning("Circuit breaker opening", #{
        failure_rate => Rate, failures => Failures, total => Total
    }),
    insert_circuit(CircuitKey, open, Now, Now).

-spec insert_circuit({atom(), binary()}, circuit_state(), integer() | undefined, integer()) ->
    ok.
insert_circuit(CircuitKey, State, OpenedAt, UpdatedAt) ->
    safe_insert(?CIRCUIT_TABLE, {CircuitKey, State, OpenedAt, UpdatedAt}).

-spec insert_window({atom(), binary()}, [window_entry()]) -> ok.
insert_window(CircuitKey, Results) ->
    safe_insert(?CIRCUIT_WINDOW_TABLE, {CircuitKey, Results}).

-spec clear_window({atom(), binary()}) -> ok.
clear_window(CircuitKey) ->
    safe_delete(?CIRCUIT_WINDOW_TABLE, CircuitKey).

-spec safe_insert(atom(), tuple()) -> ok.
safe_insert(Table, Record) ->
    try
        ets:insert(Table, Record),
        ok
    catch
        error:badarg ->
            ok = gateway_http_client:ensure_started(),
            ok = ensure_named_table(Table),
            retry_insert(Table, Record)
    end.

-spec retry_insert(atom(), tuple()) -> ok.
retry_insert(Table, Record) ->
    try
        ets:insert(Table, Record),
        ok
    catch
        error:badarg -> ok
    end.

-spec ensure_named_table(atom()) -> ok.
ensure_named_table(Name) ->
    case ets:whereis(Name) of
        undefined -> create_named_table(Name);
        _ -> ok
    end.

-spec create_named_table(atom()) -> ok.
create_named_table(Name) ->
    try
        _ = ets:new(Name, [
            named_table,
            public,
            set,
            {read_concurrency, true},
            {write_concurrency, true}
        ]),
        ok
    catch
        error:badarg -> ok
    end.

-spec safe_delete(atom(), term()) -> ok.
safe_delete(Table, Key) ->
    try
        ets:delete(Table, Key),
        ok
    catch
        error:badarg -> ok
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

circuit_ignores_transport_failures_for_non_rpc_workloads_test() ->
    ?assertEqual(false, is_countable_circuit_failure({push, <<"h">>}, {error, nxdomain})),
    ?assertEqual(
        false, is_countable_circuit_failure({push, <<"h">>}, {error, {failed_connect, []}})
    ),
    ?assertEqual(false, is_countable_circuit_failure({push, <<"h">>}, {error, timeout})),
    ?assertEqual(
        false, is_countable_circuit_failure({push, <<"h">>}, {error, {timeout, connect}})
    ),
    ?assertEqual(true, is_countable_circuit_failure({push, <<"h">>}, {error, closed})),
    ?assertEqual(true, is_countable_circuit_failure({push, <<"h">>}, {ok, 500, [], <<>>})),
    ?assertEqual(false, is_countable_circuit_failure({push, <<"h">>}, {ok, 200, [], <<>>})).

circuit_counts_transport_failures_for_rpc_test() ->
    ?assertEqual(true, is_countable_circuit_failure({rpc, <<"h">>}, {error, nxdomain})),
    ?assertEqual(
        true, is_countable_circuit_failure({rpc, <<"h">>}, {error, {failed_connect, []}})
    ),
    ?assertEqual(true, is_countable_circuit_failure({rpc, <<"h">>}, {error, timeout})),
    ?assertEqual(
        true, is_countable_circuit_failure({rpc, <<"h">>}, {error, {timeout, connect}})
    ),
    ?assertEqual(true, is_countable_circuit_failure({rpc, <<"h">>}, {error, closed})),
    ?assertEqual(true, is_countable_circuit_failure({rpc, <<"h">>}, {ok, 500, [], <<>>})),
    ?assertEqual(false, is_countable_circuit_failure({rpc, <<"h">>}, {ok, 200, [], <<>>})).

-endif.
