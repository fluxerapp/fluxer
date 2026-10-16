%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(process_registry_lookup_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

-define(REGISTRY_TABLE, process_registry_table).

lookup_or_monitor_reuses_existing_ref_for_same_pid_test() ->
    process_registry:init(),
    Key = {test_lookup, 3006},
    MapKey = same_pid_key,

    Pid = spawn(fun() -> timer:sleep(300) end),
    ets:insert(?REGISTRY_TABLE, {Key, Pid}),

    {ok, Pid, Ref1, Map1} = process_registry:lookup_or_monitor(Key, MapKey, #{}),
    {ok, Pid, Ref2, Map2} = process_registry:lookup_or_monitor(Key, MapKey, Map1),

    ?assertEqual(Ref1, Ref2),
    ?assertEqual({Pid, Ref1}, maps:get(MapKey, Map2)),

    process_registry:registry_unregister(Key).

safe_unregister_pid_conditional_keeps_other_owner_test() ->
    process_registry:init(),
    Key = {test_unreg, 4007},
    OldPid = list_to_pid("<0.100.0>"),
    NewPid = list_to_pid("<0.101.0>"),

    ets:insert(?REGISTRY_TABLE, {Key, NewPid}),

    ?assertEqual(ok, process_registry:safe_unregister(Key, OldPid)),
    ?assertEqual([{Key, NewPid}], ets:lookup(?REGISTRY_TABLE, Key)),

    ?assertEqual(ok, process_registry:safe_unregister(Key, NewPid)),
    ?assertEqual([], ets:lookup(?REGISTRY_TABLE, Key)).
