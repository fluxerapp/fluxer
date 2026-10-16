%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(process_registry_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

-define(REGISTRY_TABLE, process_registry_table).

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

registry_whereis_dead_test() ->
    process_registry:init(),
    Key = {test_whereis, 2002},
    Pid = spawn(fun() -> ok end),
    timer:sleep(10),
    ets:insert(?REGISTRY_TABLE, {Key, Pid}),
    ?assertEqual(undefined, process_registry:registry_whereis(Key)).

spawn_sleep(Milliseconds) ->
    spawn(fun() -> timer:sleep(Milliseconds) end).

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

spawn_register_worker(Parent, Key) ->
    spawn(fun() ->
        Pid = spawn_sleep(200),
        Result = process_registry:register_and_monitor(Key, Pid, #{}),
        Parent ! {self(), Result}
    end).
