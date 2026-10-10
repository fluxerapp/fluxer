%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(process_registry_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

-define(REGISTRY_TABLE, process_registry_table).

register_and_monitor_success_test() ->
    process_registry:init(),
    Key = {test_reg, 1001},
    ProcessMap = #{},

    Pid = spawn_sleep(100),

    Result = process_registry:register_and_monitor(Key, Pid, ProcessMap),

    ?assertMatch({ok, Pid, _Ref, _NewMap}, Result),
    {ok, ReturnedPid, Ref, NewMap} = Result,

    ?assertEqual(Pid, ReturnedPid),
    ?assertEqual(Pid, process_registry:registry_whereis(Key)),

    ?assertEqual(1, maps:size(NewMap)),
    ?assertEqual({Pid, Ref}, maps:get(Key, NewMap)),

    ?assert(is_reference(Ref)),

    process_registry:registry_unregister(Key).

register_and_monitor_existing_map_test() ->
    process_registry:init(),
    Key = {test_reg, 1002},
    ExistingPid = list_to_pid("<0.100.0>"),
    ExistingRef = make_ref(),
    ProcessMap = #{other_process => {ExistingPid, ExistingRef}},

    Pid = spawn_sleep(100),

    {ok, _ReturnedPid, _Ref, NewMap} = process_registry:register_and_monitor(
        Key, Pid, ProcessMap
    ),

    ?assertEqual(2, maps:size(NewMap)),
    ?assert(maps:is_key(other_process, NewMap)),
    ?assert(maps:is_key(Key, NewMap)),

    process_registry:registry_unregister(Key).

register_and_monitor_race_condition_test() ->
    process_registry:init(),
    Key = {test_reg, 1003},
    ProcessMap = #{},

    WinnerPid = spawn_sleep(200),
    ets:insert(?REGISTRY_TABLE, {Key, WinnerPid}),

    LoserPid = spawn_sleep(100),

    Result = process_registry:register_and_monitor(Key, LoserPid, ProcessMap),

    ?assertMatch({ok, WinnerPid, _Ref, _NewMap}, Result),
    {ok, ReturnedPid, Ref, NewMap} = Result,

    ?assertEqual(WinnerPid, ReturnedPid),
    ?assertEqual(WinnerPid, process_registry:registry_whereis(Key)),

    timer:sleep(50),
    ?assertEqual(false, process_liveness:is_alive(LoserPid)),

    ?assertEqual({WinnerPid, Ref}, maps:get(Key, NewMap)),

    process_registry:registry_unregister(Key).

register_and_monitor_race_dead_test() ->
    process_registry:init(),
    Key = {test_reg, 1004},
    ProcessMap = #{},

    DeadPid = spawn(fun() -> ok end),
    timer:sleep(10),
    ?assertEqual(false, process_liveness:is_alive(DeadPid)),

    NewPid = spawn_sleep(100),

    Result = process_registry:register_and_monitor(Key, NewPid, ProcessMap),
    ?assertMatch({ok, NewPid, _Ref, _NewMap}, Result),

    process_registry:registry_unregister(Key).

register_and_monitor_dead_process_test() ->
    process_registry:init(),
    Key = {test_reg, 1005},
    ProcessMap = #{},

    DeadPid = spawn(fun() -> exit(normal) end),
    timer:sleep(10),
    ?assertEqual(false, process_liveness:is_alive(DeadPid)),

    Result = process_registry:register_and_monitor(Key, DeadPid, ProcessMap),

    case Result of
        {ok, DeadPid, _Ref, _NewMap} ->
            ok;
        {error, _} ->
            ok
    end,
    process_registry:registry_unregister(Key).

register_and_monitor_concurrent_test_() ->
    {timeout, 10, fun() ->
        process_registry:init(),
        Key = {test_reg, 1006},

        Parent = self(),
        Pids = [spawn_register_worker(Parent, Key) || _ <- lists:seq(1, 5)],

        Results = [
            receive
                {P, R} -> R
            after 2000 -> timeout
            end
         || P <- Pids
        ],

        ?assertEqual(5, length(Results)),

        SuccessResults = [R || R <- Results, is_tuple(R), element(1, R) =:= ok],
        RaceErrors = [R || R <- Results, R =:= {error, registration_race_condition}],
        Timeouts = [R || R <- Results, R =:= timeout],

        ?assertEqual(0, length(Timeouts)),

        ?assertMatch([_ | _], SuccessResults),

        ?assertEqual(5, length(SuccessResults) + length(RaceErrors)),

        assert_success_results_share_pid(SuccessResults),

        process_registry:registry_unregister(Key)
    end}.

registry_whereis_alive_test() ->
    process_registry:init(),
    Key = {test_whereis, 2001},
    Pid = spawn_sleep(200),
    ets:insert(?REGISTRY_TABLE, {Key, Pid}),
    ?assertEqual(Pid, process_registry:registry_whereis(Key)),
    process_registry:registry_unregister(Key).

registry_whereis_dead_test() ->
    process_registry:init(),
    Key = {test_whereis, 2002},
    Pid = spawn(fun() -> ok end),
    timer:sleep(10),
    ets:insert(?REGISTRY_TABLE, {Key, Pid}),
    ?assertEqual(undefined, process_registry:registry_whereis(Key)).

registry_whereis_not_found_test() ->
    process_registry:init(),
    ?assertEqual(undefined, process_registry:registry_whereis({test_whereis, 2003})).

spawn_sleep(Milliseconds) ->
    spawn(fun() -> timer:sleep(Milliseconds) end).

spawn_register_worker(Parent, Key) ->
    spawn(fun() ->
        Pid = spawn_sleep(200),
        Result = process_registry:register_and_monitor(Key, Pid, #{}),
        Parent ! {self(), Result}
    end).

assert_success_results_share_pid([{ok, FirstPid, _, _} | RestResults]) ->
    AllSamePid = lists:all(
        fun(Result) -> success_pid_matches(FirstPid, Result) end,
        RestResults
    ),
    ?assert(AllSamePid).

success_pid_matches(FirstPid, {ok, Pid, _, _}) ->
    Pid =:= FirstPid;
success_pid_matches(_FirstPid, _Result) ->
    false.
