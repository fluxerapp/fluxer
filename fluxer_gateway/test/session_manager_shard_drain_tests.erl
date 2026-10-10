%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_manager_shard_drain_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

should_debounce_reactions_test() ->
    ?assertEqual(false, session_manager_shard_drain:should_debounce_reactions(#{})),
    ?assertEqual(false, session_manager_shard_drain:should_debounce_reactions(#{flags => 0})),
    ?assertEqual(false, session_manager_shard_drain:should_debounce_reactions(#{flags => 1})),
    ?assertEqual(true, session_manager_shard_drain:should_debounce_reactions(#{flags => 2})),
    ?assertEqual(true, session_manager_shard_drain:should_debounce_reactions(#{flags => 3})),
    ?assertEqual(true, session_manager_shard_drain:should_debounce_reactions(#{flags => 18})),
    ?assertEqual(false, session_manager_shard_drain:should_debounce_reactions(#{flags => -1})),
    ok.

thread_channels_capable_reads_identify_flag_bit_two_test() ->
    ?assertEqual(false, session_manager_shard_drain:thread_channels_capable(#{})),
    ?assertEqual(false, session_manager_shard_drain:thread_channels_capable(#{flags => 0})),
    ?assertEqual(false, session_manager_shard_drain:thread_channels_capable(#{flags => 2})),
    ?assertEqual(true, session_manager_shard_drain:thread_channels_capable(#{flags => 4})),
    ?assertEqual(true, session_manager_shard_drain:thread_channels_capable(#{flags => 6})),
    ?assertEqual(false, session_manager_shard_drain:thread_channels_capable(#{flags => -4})),
    ?assertEqual(
        false, session_manager_shard_drain:thread_channels_capable(#{flags => <<"4">>})
    ).

build_session_data_carries_thread_channels_capability_test() ->
    UserDataMap = #{
        <<"id">> => <<"123">>,
        <<"username">> => <<"tester">>,
        <<"discriminator">> => <<"0001">>,
        <<"avatar">> => null,
        <<"flags">> => 0
    },
    Data = #{<<"guilds">> => [], <<"user">> => UserDataMap, <<"user_settings">> => null},
    Base = #{properties => #{}, token => <<"token">>, presence => null},
    Capable = build_test_session_data(Data, Base#{flags => 6}, UserDataMap),
    Incapable = build_test_session_data(Data, Base#{flags => 2}, UserDataMap),
    Unflagged = build_test_session_data(Data, Base, UserDataMap),
    ?assertEqual(true, maps:get(thread_channels_capable, Capable)),
    ?assertEqual(true, maps:get(debounce_reactions, Capable)),
    ?assertEqual(false, maps:get(thread_channels_capable, Incapable)),
    ?assertEqual(false, maps:get(thread_channels_capable, Unflagged)).

session_rpc_sends_thread_channels_capable_only_when_flagged_test() ->
    meck:new(api_rpc_client, [passthrough, no_link]),
    Self = self(),
    meck:expect(api_rpc_client, call_with_retry, fun(Request, _Config) ->
        Self ! {session_rpc_request, Request},
        {ok, #{}}
    end),
    try
        Identify = #{token => <<"token">>, properties => #{}},
        {ok, _} = session_manager_shard_drain:fetch_rpc_data(
            #{identify_data => Identify#{flags => 4}, version => 1}, <<"127.0.0.1">>
        ),
        ?assertMatch(#{<<"thread_channels_capable">> := true}, receive_session_rpc_request()),
        {ok, _} = session_manager_shard_drain:fetch_rpc_data(
            #{identify_data => Identify#{flags => 2}, version => 1}, <<"127.0.0.1">>
        ),
        ?assertNot(maps:is_key(<<"thread_channels_capable">>, receive_session_rpc_request()))
    after
        meck:unload(api_rpc_client)
    end.

receive_session_rpc_request() ->
    receive
        {session_rpc_request, Request} -> Request
    after 1000 -> erlang:error(session_rpc_request_not_sent)
    end.

reconnect_drain_casts_each_active_session_test() ->
    TestPid = self(),
    SessionPidA = spawn(fun() -> reconnect_drain_test_session(TestPid) end),
    SessionPidB = spawn(fun() -> reconnect_drain_test_session(TestPid) end),
    State = #{
        sessions => #{
            <<"session-a">> => {SessionPidA, make_ref()},
            <<"session-b">> => {SessionPidB, make_ref()}
        }
    },
    ?assertEqual(2, session_manager_shard_drain:broadcast_reconnect_drain(State)),
    ?assertEqual(
        lists:sort([SessionPidA, SessionPidB]),
        lists:sort(collect_reconnect_drain_pids(2, []))
    ),
    ok.

reconnect_drain_ignores_dead_sessions_test() ->
    TestPid = self(),
    LiveSessionPid = spawn(fun() -> reconnect_drain_test_session(TestPid) end),
    DeadSessionPid = spawn(fun() ->
        receive
            stop -> ok
        after infinity ->
            ok
        end
    end),
    exit(DeadSessionPid, kill),
    timer:sleep(10),
    State = #{
        sessions => #{
            <<"session-live">> => {LiveSessionPid, make_ref()},
            <<"session-dead">> => {DeadSessionPid, make_ref()}
        }
    },
    ?assertEqual(1, session_manager_shard_drain:broadcast_reconnect_drain(State)),
    ?assertEqual([LiveSessionPid], collect_reconnect_drain_pids(1, [])),
    ok.

handoff_to_topology_skips_sessions_owned_by_local_node_test() ->
    with_session_state_transfer(fun() ->
        TestPid = self(),
        SessionId = <<"session-transfer-topology">>,
        TransferState = #{token_hash => utils:hash_token(<<"resume-token">>), seq => 7},
        SessionPid = spawn(fun() -> transfer_test_session_loop(TestPid, TransferState) end),
        State = #{
            sessions => #{
                SessionId => {SessionPid, make_ref()}
            }
        },
        ?assertEqual(
            #{attempted => 0, handed_off => 0},
            session_manager_shard_drain:handoff_to_topology([node()], State)
        ),
        ?assertEqual({error, not_found}, session_state_transfer:pop_state(SessionId)),
        receive
            {transfer_reconnect_drain_cast, SessionPid} ->
                ?assert(false, unexpected_transfer_reconnect_drain)
        after 200 ->
            ok
        end,
        SessionPid ! stop
    end).

handoff_to_topology_reports_failed_remote_transfer_test() ->
    TestPid = self(),
    SessionId = <<"session-handoff-fail">>,
    TransferState = #{token_hash => utils:hash_token(<<"resume-token">>), seq => 7},
    SessionPid = spawn(fun() -> transfer_test_session_loop(TestPid, TransferState) end),
    State = #{
        sessions => #{
            SessionId => {SessionPid, make_ref()}
        }
    },
    ?assertEqual(
        #{attempted => 1, handed_off => 0},
        session_manager_shard_drain:handoff_to_topology(['missing@127.0.0.1'], State)
    ),
    receive
        {transfer_reconnect_drain_cast, SessionPid} ->
            ?assert(false, unexpected_transfer_reconnect_drain)
    after 200 ->
        ok
    end,
    SessionPid ! stop.

push_and_drain_session_success_drains_session_test() ->
    with_session_state_transfer(fun() ->
        TestPid = self(),
        SessionId = <<"session-push-ok">>,
        TransferState = #{
            token_hash => utils:hash_token(<<"resume-token">>),
            seq => 5,
            socket_pid => undefined
        },
        SessionPid = spawn(fun() -> transfer_test_session_loop(TestPid, TransferState) end),
        ?assertEqual(
            ok,
            session_manager_shard_drain:push_and_drain_session(node(), SessionId, SessionPid)
        ),
        receive
            {transfer_reconnect_drain_cast, SessionPid} -> ok
        after 2000 ->
            ?assert(false, session_not_drained_after_successful_push)
        end,
        {ok, RecoveredState} = session_state_transfer:pop_state(SessionId),
        ?assertEqual(5, maps:get(seq, RecoveredState)),
        SessionPid ! stop
    end).

export_state_failure_does_not_drain_session_test() ->
    TestPid = self(),
    SessionId = <<"session-export-fail">>,
    SessionPid = spawn(fun() -> export_failing_session_loop(TestPid) end),
    ?assertMatch(
        {error, _},
        session_manager_shard_drain:push_and_drain_session(node(), SessionId, SessionPid)
    ),
    receive
        {transfer_reconnect_drain_cast, SessionPid} ->
            ?assert(false, session_drained_despite_export_failure)
    after 300 ->
        ok
    end,
    ?assert(is_process_alive(SessionPid)),
    SessionPid ! stop.

build_ready_data_for_session_strips_guilds_and_detaches_binaries_test() ->
    Large = binary:copy(<<"x">>, 1048576),
    Small = binary:part(Large, 0, 8),
    Data = #{
        <<"guilds">> => [#{<<"id">> => Small}],
        <<"trace">> => Small
    },
    ReadyData = session_manager_shard_drain:build_ready_data_for_session(Data),
    ?assertEqual([], maps:get(<<"guilds">>, ReadyData)),
    Detached = maps:get(<<"trace">>, ReadyData),
    ?assertEqual(Detached, Small),
    ?assert(binary:referenced_byte_size(Detached) < byte_size(Large)).

build_session_data_normalizes_current_user_for_ready_test() ->
    UserDataMap = #{
        <<"id">> => <<"123">>,
        <<"username">> => <<"tester">>,
        <<"discriminator">> => <<"0001">>,
        <<"avatar">> => null,
        <<"avatar_color">> => null,
        <<"flags">> => 0,
        <<"mention_flags">> => 1
    },
    Data = #{<<"guilds">> => [], <<"user">> => UserDataMap, <<"user_settings">> => null},
    IdentifyData = #{properties => #{}, token => <<"token">>, presence => null},
    SessionData = build_test_session_data(Data, IdentifyData, UserDataMap),
    UserData = maps:get(user_data, SessionData),
    ?assertEqual(123, maps:get(<<"id">>, UserData)),
    ?assertEqual(null, maps:get(<<"global_name">>, UserData)),
    ?assertEqual(1, maps:get(<<"mention_flags">>, UserData)),
    ?assertEqual(false, maps:get(<<"is_staff">>, UserData)).

build_session_data_stores_the_geoip_country_code_test() ->
    UserDataMap = #{
        <<"id">> => <<"123">>,
        <<"username">> => <<"tester">>,
        <<"discriminator">> => <<"0001">>,
        <<"avatar">> => null,
        <<"flags">> => 0
    },
    IdentifyData = #{properties => #{}, token => <<"token">>, presence => null},
    Data = fun(CountryCode) ->
        #{
            <<"guilds">> => [],
            <<"user">> => UserDataMap,
            <<"user_settings">> => null,
            <<"geoip_country_code">> => CountryCode
        }
    end,
    CountryCode = fun(Value) ->
        maps:get(
            geoip_country_code, build_test_session_data(Data(Value), IdentifyData, UserDataMap)
        )
    end,
    ?assertEqual(<<"SE">>, CountryCode(<<"SE">>)),
    ?assertEqual(undefined, CountryCode(null)),
    ?assertEqual(
        undefined,
        maps:get(
            geoip_country_code,
            build_test_session_data(
                maps:remove(<<"geoip_country_code">>, Data(null)), IdentifyData, UserDataMap
            )
        )
    ).

build_session_data_filters_guilds_for_identify_shard_test() ->
    Guild0 = guild_id_for_shard(0, 4, 0),
    Guild1 = guild_id_for_shard(1, 4, 0),
    Guild2 = guild_id_for_shard(2, 4, 0),
    Guild5 = guild_id_for_shard(1, 4, 1),
    UserDataMap = #{
        <<"id">> => <<"123">>,
        <<"username">> => <<"tester">>,
        <<"discriminator">> => <<"0001">>,
        <<"avatar">> => null,
        <<"avatar_color">> => null,
        <<"flags">> => 0,
        <<"bot">> => true
    },
    Data = #{
        <<"guilds">> => [
            guild_wire(Guild0),
            guild_wire(Guild1),
            guild_wire(Guild2),
            guild_wire(Guild5)
        ],
        <<"user">> => UserDataMap,
        <<"user_settings">> => null
    },
    IdentifyData = #{
        properties => #{}, token => <<"token">>, presence => null, shard => {1, 4}
    },
    SessionData = build_test_session_data(Data, IdentifyData, UserDataMap),
    ?assertEqual([Guild1, Guild5], maps:get(guilds, SessionData)),
    ?assertEqual({1, 4}, maps:get(shard, SessionData)).

validate_identify_sharding_requires_shards_for_large_unsharded_bot_test() ->
    Guilds = [guild_wire(guild_id_for_shard(0, 1, I)) || I <- lists:seq(0, 2500)],
    Data = #{<<"guilds">> => Guilds},
    ?assertEqual(
        {error, sharding_required},
        session_manager_shard_drain:validate_identify_sharding(Data, #{}, true)
    ),
    ?assertEqual(ok, session_manager_shard_drain:validate_identify_sharding(Data, #{}, false)),
    ?assertEqual(
        ok,
        session_manager_shard_drain:validate_identify_sharding(Data, #{shard => {1, 2}}, true)
    ).

validate_identify_sharding_counts_only_selected_shard_test() ->
    Guilds = [guild_wire(guild_id_for_shard(0, 2, I)) || I <- lists:seq(0, 2500)],
    Data = #{<<"guilds">> => Guilds},
    ?assertEqual(
        {error, sharding_required},
        session_manager_shard_drain:validate_identify_sharding(
            Data, #{shard => {0, 2}}, true
        )
    ),
    ?assertEqual(
        ok,
        session_manager_shard_drain:validate_identify_sharding(Data, #{shard => {1, 2}}, true)
    ).

guild_wire(GuildId) ->
    #{<<"id">> => integer_to_binary(GuildId)}.

guild_id_for_shard(ShardId, NumShards, Offset) ->
    ((Offset * NumShards + ShardId) bsl 22) + 1.

build_test_session_data(Data, IdentifyData, UserDataMap) ->
    session_manager_shard_drain:build_session_data(
        Data, IdentifyData, 1, self(), <<"session-id">>, UserDataMap, 123
    ).

reconnect_drain_test_session(TestPid) ->
    receive
        {'$gen_cast', reconnect_drain} ->
            TestPid ! {reconnect_drain_cast, self()}
    after infinity ->
        ok
    end.

transfer_test_session_loop(TestPid, TransferState) ->
    receive
        {'$gen_call', From, export_state} ->
            gen_server:reply(From, {ok, TransferState}),
            transfer_test_session_loop(TestPid, TransferState);
        {'$gen_cast', handoff_fence} ->
            TestPid ! {transfer_reconnect_drain_cast, self()},
            transfer_test_session_loop(TestPid, TransferState);
        {'$gen_cast', {reconnect_drain, _SocketPid}} ->
            TestPid ! {transfer_reconnect_drain_cast, self()},
            transfer_test_session_loop(TestPid, TransferState);
        {'$gen_cast', reconnect_drain} ->
            TestPid ! {transfer_reconnect_drain_cast, self()},
            transfer_test_session_loop(TestPid, TransferState);
        stop ->
            ok
    after infinity ->
        ok
    end.

export_failing_session_loop(TestPid) ->
    receive
        {'$gen_call', From, export_state} ->
            gen_server:reply(From, {error, crashed}),
            export_failing_session_loop(TestPid);
        {'$gen_cast', handoff_fence} ->
            TestPid ! {transfer_reconnect_drain_cast, self()},
            export_failing_session_loop(TestPid);
        {'$gen_cast', {reconnect_drain, _SocketPid}} ->
            TestPid ! {transfer_reconnect_drain_cast, self()},
            export_failing_session_loop(TestPid);
        {'$gen_cast', reconnect_drain} ->
            TestPid ! {transfer_reconnect_drain_cast, self()},
            export_failing_session_loop(TestPid);
        stop ->
            ok
    after infinity ->
        ok
    end.

collect_reconnect_drain_pids(0, Acc) ->
    Acc;
collect_reconnect_drain_pids(Remaining, Acc) ->
    receive
        {reconnect_drain_cast, Pid} ->
            collect_reconnect_drain_pids(Remaining - 1, [Pid | Acc])
    after 200 ->
        ?assert(false, reconnect_drain_cast_timeout)
    end.

with_session_state_transfer(Fun) ->
    process_registry:init(),
    case whereis(session_state_transfer) of
        undefined ->
            {ok, Pid} = session_state_transfer:start_link(),
            try
                Fun()
            after
                try
                    gen_server:stop(Pid)
                catch
                    _:_ -> ok
                end
            end;
        _Pid ->
            Fun()
    end.
