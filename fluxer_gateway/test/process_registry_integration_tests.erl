%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(process_registry_integration_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

-define(REGISTRY_TABLE, process_registry_table).

force_stop_process_kills_unresponsive_test_() ->
    {timeout, 15, fun() ->
        Pid = spawn(fun trapped_wait_loop/0),
        ?assert(process_liveness:is_alive(Pid)),
        process_registry:force_stop_process(Pid),
        timer:sleep(100),
        ?assertEqual(false, process_liveness:is_alive(Pid))
    end}.

register_and_monitor_duplicate_stops_loser_test_() ->
    {timeout, 15, fun() ->
        process_registry:init(),
        Key = {test_reg, 1007},
        WinnerPid = spawn_sleep(5000),
        ets:insert(?REGISTRY_TABLE, {Key, WinnerPid}),
        LoserPid = spawn(fun trapped_wait_loop/0),
        ?assert(process_liveness:is_alive(LoserPid)),
        Result = process_registry:register_and_monitor(Key, LoserPid, #{}),
        ?assertMatch({ok, WinnerPid, _, _}, Result),
        timer:sleep(100),
        ?assertEqual(false, process_liveness:is_alive(LoserPid)),
        process_registry:registry_unregister(Key)
    end}.

spawn_sleep(Milliseconds) ->
    spawn(fun() -> timer:sleep(Milliseconds) end).

trapped_wait_loop() ->
    process_flag(trap_exit, true),
    receive
        never_arrives -> ok
    after infinity ->
        ok
    end.
