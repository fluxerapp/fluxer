%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_manager_routing_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

start_call_with_drain_guard_rejects_during_drain_test() ->
    State = #{shards => #{}, shard_count => 1},
    ?assertEqual(
        {{error, draining}, State},
        session_manager_routing:start_call_with_drain_guard(true, #{}, self(), State)
    ).
