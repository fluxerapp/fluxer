%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(process_registry_cleanup_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

cleanup_on_down_preserves_loading_test() ->
    DeadPid = list_to_pid("<0.100.0>"),
    AlivePid = list_to_pid("<0.101.0>"),
    Ref1 = make_ref(),
    Ref2 = make_ref(),

    Map = #{
        guild_1 => {DeadPid, Ref1},
        guild_2 => loading,
        guild_3 => {AlivePid, Ref2}
    },

    Result = process_registry:cleanup_on_down(DeadPid, Map),

    ?assertEqual(2, maps:size(Result)),
    ?assertEqual(loading, maps:get(guild_2, Result)),
    ?assertEqual({AlivePid, Ref2}, maps:get(guild_3, Result)),
    ?assertEqual(false, maps:is_key(guild_1, Result)).

cleanup_on_down_multiple_loading_test() ->
    DeadPid = list_to_pid("<0.100.0>"),
    AlivePid = list_to_pid("<0.101.0>"),
    Ref1 = make_ref(),
    Ref2 = make_ref(),

    Map = #{
        guild_1 => {DeadPid, Ref1},
        guild_2 => loading,
        guild_3 => {AlivePid, Ref2},
        guild_4 => loading,
        guild_5 => loading
    },

    Result = process_registry:cleanup_on_down(DeadPid, Map),

    ?assertEqual(4, maps:size(Result)),
    ?assertEqual(loading, maps:get(guild_2, Result)),
    ?assertEqual(loading, maps:get(guild_4, Result)),
    ?assertEqual(loading, maps:get(guild_5, Result)),
    ?assertEqual({AlivePid, Ref2}, maps:get(guild_3, Result)),
    ?assertEqual(false, maps:is_key(guild_1, Result)).

cleanup_on_down_single_removal_test() ->
    DeadPid = list_to_pid("<0.100.0>"),
    AlivePid1 = list_to_pid("<0.101.0>"),
    AlivePid2 = list_to_pid("<0.102.0>"),
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Ref3 = make_ref(),

    Map = #{
        guild_1 => {AlivePid1, Ref1},
        guild_2 => {DeadPid, Ref2},
        guild_3 => {AlivePid2, Ref3}
    },

    Result = process_registry:cleanup_on_down(DeadPid, Map),

    ?assertEqual(2, maps:size(Result)),
    ?assertEqual({AlivePid1, Ref1}, maps:get(guild_1, Result)),
    ?assertEqual({AlivePid2, Ref3}, maps:get(guild_3, Result)),
    ?assertEqual(false, maps:is_key(guild_2, Result)).

cleanup_on_down_empty_test() ->
    DeadPid = list_to_pid("<0.100.0>"),
    Result = process_registry:cleanup_on_down(DeadPid, #{}),
    ?assertEqual(#{}, Result).

cleanup_on_down_only_loading_test() ->
    DeadPid = list_to_pid("<0.100.0>"),
    Map = #{
        guild_1 => loading,
        guild_2 => loading
    },
    Result = process_registry:cleanup_on_down(DeadPid, Map),
    ?assertEqual(Map, Result).

cleanup_on_down_pid_not_found_test() ->
    DeadPid = list_to_pid("<0.100.0>"),
    AlivePid = list_to_pid("<0.101.0>"),
    Ref = make_ref(),

    Map = #{
        guild_1 => {AlivePid, Ref},
        guild_2 => loading
    },

    Result = process_registry:cleanup_on_down(DeadPid, Map),
    ?assertEqual(Map, Result).

cleanup_on_down_duplicate_pids_test() ->
    DeadPid = list_to_pid("<0.100.0>"),
    Ref1 = make_ref(),
    Ref2 = make_ref(),

    Map = #{
        guild_1 => {DeadPid, Ref1},
        guild_2 => {DeadPid, Ref2}
    },

    Result = process_registry:cleanup_on_down(DeadPid, Map),
    ?assertEqual(0, maps:size(Result)).

typed_cleanup_on_down_unregisters_runtime_prefixes_test() ->
    process_registry:init(),
    DeadPid = list_to_pid("<0.100.0>"),
    RuntimeEntries = [
        {call, 9001},
        {guild, 9002},
        {presence, 9003},
        {session, <<"session-9004">>}
    ],
    try
        lists:foreach(
            fun({Prefix, Id}) ->
                Key = process_registry:build_process_key(Prefix, Id),
                ets:insert(process_registry_table, {Key, DeadPid}),
                Result = process_registry:cleanup_on_down(
                    Prefix, DeadPid, #{Id => {DeadPid, make_ref()}}
                ),
                ?assertEqual(#{}, Result),
                ?assertEqual([], ets:lookup(process_registry_table, Key))
            end,
            RuntimeEntries
        )
    after
        lists:foreach(
            fun({Prefix, Id}) ->
                process_registry:safe_unregister(process_registry:build_process_key(Prefix, Id))
            end,
            RuntimeEntries
        )
    end.

typed_cleanup_on_down_keeps_replacement_registry_pid_test() ->
    process_registry:init(),
    Id = 9101,
    Key = process_registry:build_process_key(presence, Id),
    DeadPid = list_to_pid("<0.100.0>"),
    ReplacementPid = list_to_pid("<0.101.0>"),
    try
        ets:insert(process_registry_table, {Key, ReplacementPid}),
        Result = process_registry:cleanup_on_down(
            presence, DeadPid, #{Id => {DeadPid, make_ref()}}
        ),
        ?assertEqual(#{}, Result),
        ?assertEqual([{Key, ReplacementPid}], ets:lookup(process_registry_table, Key))
    after
        process_registry:safe_unregister(Key)
    end.

typed_cleanup_on_down_ignores_unkeyable_ids_test() ->
    process_registry:init(),
    DeadPid = list_to_pid("<0.100.0>"),
    Result = process_registry:cleanup_on_down(
        guild,
        DeadPid,
        #{
            bad_id => {DeadPid, make_ref()},
            9201 => loading
        }
    ),
    ?assertEqual(#{9201 => loading}, Result).
