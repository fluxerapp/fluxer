%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_cluster_metrics).
-typing([eqwalizer]).

-export([
    init/0,
    record_discovery_resolve_failure/0,
    record_membership_transition/1,
    record_owner_resolution/1,
    record_resume/0,
    resumes_total/0,
    record_dispatch/0,
    dispatches_total/0,
    record_dispatch_drop/0,
    dispatch_drops_total/0,
    snapshot/0,
    reset_for_tests/0
]).

-define(COUNTERS_KEY, gateway_cluster_metrics_counters).
-define(DISCOVERY_RESOLVE_FAILURES_IDX, 1).
-define(MEMBERSHIP_UP_IDX, 2).
-define(MEMBERSHIP_DOWN_IDX, 3).
-define(OWNER_SELF_IDX, 4).
-define(OWNER_PEER_IDX, 5).
-define(RESUMES_IDX, 6).
-define(DISPATCHES_IDX, 7).
-define(DISPATCH_DROPS_IDX, 8).
-define(COUNTER_COUNT, 8).

-spec init() -> ok.
init() ->
    _ = counters_ref(),
    ok.

-spec record_discovery_resolve_failure() -> ok.
record_discovery_resolve_failure() ->
    add(?DISCOVERY_RESOLVE_FAILURES_IDX, 1).

-spec record_membership_transition(up | down) -> ok.
record_membership_transition(up) ->
    add(?MEMBERSHIP_UP_IDX, 1);
record_membership_transition(down) ->
    add(?MEMBERSHIP_DOWN_IDX, 1).

-spec record_owner_resolution(self | peer) -> ok.
record_owner_resolution(self) ->
    add(?OWNER_SELF_IDX, 1);
record_owner_resolution(peer) ->
    add(?OWNER_PEER_IDX, 1).

-spec record_resume() -> ok.
record_resume() ->
    add(?RESUMES_IDX, 1).

-spec resumes_total() -> non_neg_integer().
resumes_total() ->
    counters:get(counters_ref(), ?RESUMES_IDX).

-spec record_dispatch() -> ok.
record_dispatch() ->
    add(?DISPATCHES_IDX, 1).

-spec dispatches_total() -> non_neg_integer().
dispatches_total() ->
    counters:get(counters_ref(), ?DISPATCHES_IDX).

-spec record_dispatch_drop() -> ok.
record_dispatch_drop() ->
    add(?DISPATCH_DROPS_IDX, 1).

-spec dispatch_drops_total() -> non_neg_integer().
dispatch_drops_total() ->
    counters:get(counters_ref(), ?DISPATCH_DROPS_IDX).

-spec snapshot() -> map().
snapshot() ->
    Counters = counters_ref(),
    #{
        <<"gateway_cluster_member_count">> => gateway_cluster_membership:alive_count(),
        <<"session_resumes_total">> => counters:get(Counters, ?RESUMES_IDX),
        <<"websocket_dispatches_total">> => counters:get(Counters, ?DISPATCHES_IDX),
        <<"websocket_dispatch_drops_total">> => counters:get(Counters, ?DISPATCH_DROPS_IDX),
        <<"gateway_cluster_discovery_resolve_failures_total">> =>
            counters:get(Counters, ?DISCOVERY_RESOLVE_FAILURES_IDX),
        <<"gateway_cluster_membership_transitions_total">> => #{
            <<"up">> => counters:get(Counters, ?MEMBERSHIP_UP_IDX),
            <<"down">> => counters:get(Counters, ?MEMBERSHIP_DOWN_IDX)
        },
        <<"gateway_node_router_owner_resolutions_total">> => #{
            <<"self">> => counters:get(Counters, ?OWNER_SELF_IDX),
            <<"peer">> => counters:get(Counters, ?OWNER_PEER_IDX)
        }
    }.

-spec add(pos_integer(), integer()) -> ok.
add(Index, Amount) ->
    counters:add(counters_ref(), Index, Amount),
    ok.

-spec counters_ref() -> counters:counters_ref().
counters_ref() ->
    case persistent_term:get(?COUNTERS_KEY, undefined) of
        undefined ->
            new_counters();
        Counters ->
            valid_counters_or_new(Counters)
    end.

-spec valid_counters_or_new(term()) -> counters:counters_ref().
valid_counters_or_new(Counters) ->
    case validate_counters(Counters) of
        {ok, Ref} -> Ref;
        error -> new_counters()
    end.

-spec validate_counters(term()) -> {ok, counters:counters_ref()} | error.
validate_counters(Counters) ->
    try
        Ref = eqwalizer:dynamic_cast(Counters),
        _ = counters:get(Ref, ?COUNTER_COUNT),
        {ok, Ref}
    catch
        _:_ -> error
    end.

-spec new_counters() -> counters:counters_ref().
new_counters() ->
    Counters = counters:new(?COUNTER_COUNT, [write_concurrency]),
    persistent_term:put(?COUNTERS_KEY, Counters),
    Counters.

-spec reset_for_tests() -> ok.
reset_for_tests() ->
    persistent_term:erase(?COUNTERS_KEY),
    ok.
