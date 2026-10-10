%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager_shard_lifecycle_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

start_new_guild_skips_start_when_already_registered_test() ->
    process_registry:init(),
    GuildId = 77777,
    GuildKey = process_registry:build_process_key(guild, GuildId),
    ExistingPid = spawn(fun mock_guild_loop/0),
    ets:insert(process_registry_table, {GuildKey, ExistingPid}),
    try
        State0 = #{guilds => #{}, pending_requests => #{}, shard_index => 0},
        Data = #{<<"guild">> => #{<<"id">> => <<"77777">>, <<"features">> => []}},
        Result = guild_manager_shard_lifecycle:start_new_guild(GuildId, Data, GuildKey, State0),
        ?assertMatch({ok, ExistingPid, _}, Result),
        {ok, RetPid, _NewState} = Result,
        ?assertEqual(ExistingPid, RetPid)
    after
        process_registry:registry_unregister(GuildKey),
        ExistingPid ! stop
    end.

normalize_transferred_guild_state_keeps_only_transferable_fields_test() ->
    GuildId = 99998,
    TransferState = #{
        id => 1,
        data => #{<<"guild">> => #{<<"id">> => <<"99998">>}},
        sessions => #{<<"s1">> => #{pid => self()}},
        voice_states => #{<<"v1">> => #{}},
        virtual_channel_access => #{10 => sets:from_list([20])},
        thread_handoff_resends => [<<"s1">>],
        thread_handoff_syncs => [<<"s2">>],
        thread_handoff_lists => [30],
        thread_resend_pending => #{<<"s1">> => true},
        member_list_engine => make_ref(),
        channel_member_list_engines => #{<<"500">> => make_ref()},
        voice_server_pid => self()
    },
    Normalized = guild_manager_shard_lifecycle:normalize_transferred_guild_state(
        GuildId, TransferState
    ),
    ?assertEqual(GuildId, maps:get(id, Normalized)),
    ?assertEqual(#{<<"guild">> => #{<<"id">> => <<"99998">>}}, maps:get(data, Normalized)),
    ?assertEqual(#{<<"s1">> => #{pid => self()}}, maps:get(sessions, Normalized)),
    ?assertEqual(#{<<"v1">> => #{}}, maps:get(voice_states, Normalized)),
    ?assertNot(maps:is_key(member_list_engine, Normalized)),
    ?assertNot(maps:is_key(channel_member_list_engines, Normalized)),
    ?assertEqual([<<"s1">>], maps:get(thread_handoff_resends, Normalized)),
    ?assertEqual([<<"s2">>], maps:get(thread_handoff_syncs, Normalized)),
    ?assertEqual([30], maps:get(thread_handoff_lists, Normalized)),
    ?assertNot(maps:is_key(thread_resend_pending, Normalized)),
    ?assertNot(maps:is_key(voice_server_pid, Normalized)).

register_and_monitor_race_kills_duplicate_test_() ->
    {timeout, 15, fun() ->
        process_registry:init(),
        GuildId = 66666,
        GuildKey = process_registry:build_process_key(guild, GuildId),
        WinnerPid = spawn(fun mock_guild_loop/0),
        ets:insert(process_registry_table, {GuildKey, WinnerPid}),
        LoserPid = spawn(fun mock_guild_loop/0),
        try
            Result = process_registry:register_and_monitor(GuildKey, LoserPid, #{}),
            ?assertMatch({ok, WinnerPid, _, _}, Result),
            timer:sleep(200),
            ?assertEqual(false, process_liveness:is_alive(LoserPid)),
            ?assert(process_liveness:is_alive(WinnerPid))
        after
            process_registry:registry_unregister(GuildKey),
            WinnerPid ! stop,
            LoserPid ! stop
        end
    end}.

shard_terminate_drains_pending_requests_test() ->
    ReplyRef1 = make_ref(),
    ReplyRef2 = make_ref(),
    State = #{
        guilds => #{111 => loading, 222 => loading},
        pending_requests => #{
            111 => [{self(), ReplyRef1}],
            222 => [{self(), ReplyRef2}]
        },
        fetch_workers => #{},
        shard_index => 0
    },
    guild_manager_shard:terminate(shutdown, State),
    receive
        {ReplyRef1, {error, shard_shutdown}} -> ok
    after 100 ->
        ?assert(false)
    end,
    receive
        {ReplyRef2, {error, shard_shutdown}} -> ok
    after 100 ->
        ?assert(false)
    end.

mock_guild_loop() ->
    receive
        {'$gen_call', From, _Msg} ->
            gen_server:reply(From, ok),
            mock_guild_loop();
        stop ->
            ok;
        _Other ->
            mock_guild_loop()
    after infinity ->
        ok
    end.
