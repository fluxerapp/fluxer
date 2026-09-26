%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(call_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

integer_list_to_binaries_test() ->
    ?assertEqual([<<"1">>, <<"2">>, <<"3">>], call_state:integer_list_to_binaries([1, 2, 3])),
    ?assertEqual([], call_state:integer_list_to_binaries([])).

find_session_by_pid_test() ->
    Pid1 = self(),
    Sessions = #{
        <<"session1">> => {100, Pid1, make_ref()},
        <<"session2">> => {200, spawn(fun() -> ok end), make_ref()}
    },
    ?assertMatch({ok, <<"session1">>, 100}, call_state:find_session_by_pid(Pid1, Sessions)),
    ?assertEqual(not_found, call_state:find_session_by_pid(spawn(fun() -> ok end), Sessions)).

format_voice_state_test() ->
    VoiceState = #{
        <<"user_id">> => 123,
        <<"channel_id">> => 456,
        <<"guild_id">> => 789,
        <<"mute">> => false
    },
    Result = call_state:format_voice_state(VoiceState),
    ?assertEqual(<<"123">>, maps:get(<<"user_id">>, Result)),
    ?assertEqual(<<"456">>, maps:get(<<"channel_id">>, Result)),
    ?assertEqual(<<"789">>, maps:get(<<"guild_id">>, Result)),
    ?assertEqual(false, maps:get(<<"mute">>, Result)).

format_pending_connections_normalizes_and_filters_malformed_entries_test() ->
    PendingConnections = #{
        <<"conn-a">> => #{user_id => <<"42">>, token_nonce => 123, joined_at => <<"1000">>},
        "conn-b" => #{user_id => "84", token_nonce => "nonce", joined_at => "2000"},
        <<"missing-user">> => #{token_nonce => <<"nonce">>, joined_at => 1000},
        <<"bad-user">> => #{user_id => <<"bad">>, joined_at => 1000},
        <<"leading-zero-user">> => #{user_id => <<"0042">>, joined_at => 1000},
        <<"leading-zero-list-user">> => #{user_id => "0084", joined_at => 1000},
        <<"bad-joined-at">> => #{user_id => 168, joined_at => <<"bad">>},
        <<"not-a-map">> => not_a_map
    },
    Result = call_state:format_pending_connections(PendingConnections),
    ?assertEqual(3, length(Result)),
    ?assert(
        has_pending_connection(
            #{connection_id => <<"conn-a">>, user_id => <<"42">>, token_nonce => <<"123">>},
            Result
        )
    ),
    ?assert(
        has_pending_connection(
            #{connection_id => <<"conn-b">>, user_id => <<"84">>, token_nonce => <<"nonce">>},
            Result
        )
    ),
    ?assert(
        has_pending_connection(
            #{connection_id => <<"bad-joined-at">>, user_id => <<"168">>}, Result
        )
    ),
    ?assertEqual([], call_state:format_pending_connections(not_a_map)).

remove_users_from_ringing_test() ->
    State = new_call_test_state(#{
        ringing => [1, 2, 3],
        pending_ringing => [4, 5]
    }),
    Result = call_ringing:remove_users_from_ringing([2, 4], State),
    ?assertEqual([1, 3], maps:get(ringing, Result)),
    ?assertEqual([5], maps:get(pending_ringing, Result)).

join_from_ringing_adds_voice_state_test() ->
    UserId = 42,
    SessionId = <<"session-42">>,
    ConnectionId = <<"conn-42">>,
    VoiceState = #{
        <<"user_id">> => <<"42">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => ConnectionId
    },
    State = new_call_test_state(#{
        ringing => [UserId],
        initiator_ready => true
    }),
    {reply, ok, NewState} = call_voice:handle_join_internal(
        UserId, VoiceState, SessionId, self(), ConnectionId, State
    ),
    ?assert(maps:is_key(UserId, maps:get(voice_states, NewState))),
    ?assertNot(lists:member(UserId, maps:get(ringing, NewState))).

pending_connection_timeout_preserves_active_voice_state_test() ->
    UserId = 42,
    SessionId = <<"session-42">>,
    ConnectionId = <<"conn-42">>,
    VoiceState = #{
        <<"user_id">> => <<"42">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => ConnectionId
    },
    State = new_call_test_state(#{
        voice_states => #{UserId => VoiceState},
        pending_connections => #{
            ConnectionId => #{
                user_id => UserId,
                session_id => SessionId,
                connection_id => ConnectionId,
                channel_id => 1
            }
        },
        initiator_ready => true
    }),
    {noreply, NewState} = call_voice:disconnect_user_after_pending_timeout(
        ConnectionId, UserId, SessionId, State
    ),
    ?assert(maps:is_key(UserId, maps:get(voice_states, NewState))),
    ?assertEqual(
        undefined,
        maps:get(
            ConnectionId, maps:get(pending_connections, NewState), undefined
        )
    ).

leave_then_rejoin_keeps_voice_state_test() ->
    UserId = 42,
    OtherUserId = 84,
    SessionId = <<"session-42">>,
    OtherSessionId = <<"session-84">>,
    VoiceState = #{
        <<"user_id">> => <<"42">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => <<"conn-42">>
    },
    OtherVoiceState = #{
        <<"user_id">> => <<"84">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => <<"conn-84">>
    },
    State = new_call_test_state(#{
        voice_states => #{
            UserId => VoiceState,
            OtherUserId => OtherVoiceState
        },
        sessions => #{
            SessionId => {UserId, self(), make_ref()},
            OtherSessionId => {OtherUserId, self(), make_ref()}
        },
        initiator_ready => true
    }),
    {reply, ok, PostLeave} = call:handle_call(
        {leave, SessionId}, {self(), make_ref()}, State
    ),
    ?assertEqual(
        undefined,
        maps:get(
            UserId, maps:get(voice_states, PostLeave), undefined
        )
    ),
    ?assertEqual(
        undefined,
        maps:get(
            SessionId, maps:get(sessions, PostLeave), undefined
        )
    ),
    {reply, ok, Rejoined} = call_voice:handle_join_internal(
        UserId, VoiceState, SessionId, self(), <<"conn-42">>, PostLeave
    ),
    ?assert(maps:is_key(UserId, maps:get(voice_states, Rejoined))).

disconnect_user_if_in_channel_notifies_session_cleanup_test() ->
    UserId = 42,
    OtherUserId = 84,
    SessionId = <<"session-42">>,
    OtherSessionId = <<"session-84">>,
    ConnectionId = <<"conn-42">>,
    VoiceState = #{
        <<"user_id">> => <<"42">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => ConnectionId
    },
    OtherVoiceState = #{
        <<"user_id">> => <<"84">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => <<"conn-84">>
    },
    State = new_call_test_state(#{
        voice_states => #{
            UserId => VoiceState,
            OtherUserId => OtherVoiceState
        },
        sessions => #{
            SessionId => {UserId, self(), make_ref()},
            OtherSessionId => {OtherUserId, self(), make_ref()}
        },
        initiator_ready => true
    }),
    {reply, #{success := true}, NewState} = call:handle_call(
        {disconnect_user_if_in_channel, UserId, 1, ConnectionId},
        {self(), make_ref()},
        State
    ),
    ?assertEqual(
        undefined,
        maps:get(
            UserId, maps:get(voice_states, NewState), undefined
        )
    ),
    receive
        {'$gen_cast', {call_force_disconnect, 1, ConnectionId}} ->
            ok
    after 1000 ->
        ?assert(false)
    end.

disconnect_user_if_in_channel_ignores_a_stale_connection_id_test() ->
    UserId = 42,
    SessionId = <<"session-42">>,
    LiveConnectionId = <<"conn-live">>,
    StaleConnectionId = <<"conn-stale">>,
    VoiceState = #{
        <<"user_id">> => <<"42">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => LiveConnectionId
    },
    State = new_call_test_state(#{
        voice_states => #{UserId => VoiceState},
        sessions => #{SessionId => {UserId, self(), make_ref()}},
        initiator_ready => true
    }),
    {reply, Reply, NewState} = call:handle_call(
        {disconnect_user_if_in_channel, UserId, 1, StaleConnectionId},
        {self(), make_ref()},
        State
    ),
    ?assertMatch(#{ignored := true, reason := <<"not_in_call">>}, Reply),
    ?assertEqual(
        VoiceState,
        maps:get(UserId, maps:get(voice_states, NewState), undefined)
    ),
    receive
        {'$gen_cast', {call_force_disconnect, _, _}} ->
            ?assert(false, force_disconnected_a_live_connection)
    after 200 ->
        ok
    end.

leave_removes_voice_state_count_without_full_rebuild_test() ->
    UserId = 42,
    SessionId = <<"session-count-leave">>,
    ConnectionId = <<"conn-count-leave">>,
    RegionId = <<"count-test-region">>,
    ServerId = <<"count-test-server">>,
    VoiceState = #{
        <<"user_id">> => <<"42">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => ConnectionId,
        <<"region_id">> => RegionId,
        <<"server_id">> => ServerId
    },
    ok = voice_state_counts_cache:remove_connection(ConnectionId),
    ok = voice_state_counts_cache:upsert_voice_state(VoiceState),
    ?assertEqual(1, voice_count(<<"servers">>, <<"server_id">>, ServerId)),
    State = new_call_test_state(#{
        voice_states => #{UserId => VoiceState},
        sessions => #{SessionId => {UserId, self(), make_ref()}},
        initiator_ready => true
    }),
    {stop, normal, ok, _PostLeave} = call:handle_call(
        {leave, SessionId}, {self(), make_ref()}, State
    ),
    ?assertEqual(0, voice_count(<<"regions">>, <<"region_id">>, RegionId)),
    ?assertEqual(0, voice_count(<<"servers">>, <<"server_id">>, ServerId)),
    ok = voice_state_counts_cache:remove_connection(ConnectionId).

terminate_abnormal_dispatches_call_delete_test() ->
    State = new_call_test_state(#{
        recipients => [100, 200],
        voice_states => #{},
        ringing => [],
        initiator_ready => true
    }),
    ?assertEqual(ok, call:terminate(killed, State)).

terminate_handoff_skips_cleanup_test() ->
    State = new_call_test_state(#{
        recipients => [100],
        voice_states => #{42 => #{<<"connection_id">> => <<"conn-42">>}},
        initiator_ready => true
    }),
    ?assertEqual(ok, call:terminate({shutdown, handoff}, State)),
    ?assertEqual(ok, call:terminate(handoff, State)).

terminate_normal_cleans_counts_test() ->
    State = new_call_test_state(#{
        recipients => [],
        voice_states => #{},
        initiator_ready => true
    }),
    ?assertEqual(ok, call:terminate(normal, State)).

call_manager_cleanup_call_pid_cache_on_down_test() ->
    case ets:info(call_pid_cache) of
        undefined ->
            ets:new(call_pid_cache, [
                named_table,
                public,
                set,
                {read_concurrency, true},
                {write_concurrency, true}
            ]);
        _ ->
            ok
    end,
    FakePid = spawn(fun() ->
        receive
            stop -> ok
        after 30000 ->
            ok
        end
    end),
    ChannelId = 999888,
    ets:insert(call_pid_cache, {ChannelId, FakePid}),
    Ref = make_ref(),
    Calls = #{ChannelId => {FakePid, Ref}},
    State = #{calls => Calls},
    {noreply, NewState} = call_manager:handle_info(
        {'DOWN', Ref, process, FakePid, normal}, State
    ),
    ?assertEqual([], ets:lookup(call_pid_cache, ChannelId)),
    ?assertNot(maps:is_key(ChannelId, maps:get(calls, NewState))),
    FakePid ! stop.

voice_count(CollectionKey, IdKey, Id) ->
    Counts = voice_state_counts_cache:get_local_counts(),
    Entries = maps:get(CollectionKey, Counts, []),
    lists:foldl(
        fun(Entry, Acc) ->
            case
                {maps:get(IdKey, Entry, undefined), maps:get(<<"voice_state_count">>, Entry, 0)}
            of
                {Id, Count} when is_integer(Count) -> Count;
                _ -> Acc
            end
        end,
        0,
        Entries
    ).

has_pending_connection(Expected, Result) ->
    Keys = maps:keys(Expected),
    lists:any(fun(Connection) -> maps:with(Keys, Connection) =:= Expected end, Result).

leave_with_second_session_keeps_voice_state_test() ->
    UserId = 42,
    VoiceState = #{
        <<"user_id">> => <<"42">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => <<"conn-42">>
    },
    State = new_call_test_state(#{
        voice_states => #{UserId => VoiceState},
        sessions => #{
            <<"session-a">> => {UserId, self(), make_ref()},
            <<"session-b">> => {UserId, self(), make_ref()}
        },
        initiator_ready => true
    }),
    {reply, ok, AfterFirstLeave} = call:handle_call(
        {leave, <<"session-a">>}, {self(), make_ref()}, State
    ),
    ?assertEqual(VoiceState, maps:get(UserId, maps:get(voice_states, AfterFirstLeave))),
    ?assertNot(maps:is_key(<<"session-a">>, maps:get(sessions, AfterFirstLeave))),
    ?assert(maps:is_key(<<"session-b">>, maps:get(sessions, AfterFirstLeave))),
    {stop, normal, ok, AfterSecondLeave} = call:handle_call(
        {leave, <<"session-b">>}, {self(), make_ref()}, AfterFirstLeave
    ),
    ?assertNot(maps:is_key(UserId, maps:get(voice_states, AfterSecondLeave))),
    ?assertEqual(#{}, maps:get(sessions, AfterSecondLeave)).

session_down_with_second_session_keeps_voice_state_test() ->
    UserId = 42,
    VoiceState = #{
        <<"user_id">> => <<"42">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => <<"conn-42">>
    },
    DownPid = spawn(fun() ->
        receive
            stop -> ok
        after 30000 ->
            ok
        end
    end),
    State = new_call_test_state(#{
        voice_states => #{UserId => VoiceState},
        sessions => #{
            <<"session-a">> => {UserId, DownPid, make_ref()},
            <<"session-b">> => {UserId, self(), make_ref()}
        },
        initiator_ready => true
    }),
    {noreply, NewState} = call_voice:handle_session_down(DownPid, State),
    ?assertEqual(VoiceState, maps:get(UserId, maps:get(voice_states, NewState))),
    ?assertNot(maps:is_key(<<"session-a">>, maps:get(sessions, NewState))),
    ?assert(maps:is_key(<<"session-b">>, maps:get(sessions, NewState))),
    DownPid ! stop.

disconnect_user_removes_all_user_sessions_test() ->
    UserId = 42,
    VoiceStates = #{
        UserId => #{
            <<"user_id">> => <<"42">>,
            <<"channel_id">> => <<"1">>,
            <<"connection_id">> => <<"conn-42">>
        }
    },
    Sessions = #{
        <<"session-a">> => {UserId, self(), make_ref()},
        <<"session-b">> => {UserId, self(), make_ref()},
        <<"session-other">> => {84, self(), make_ref()}
    },
    TestPid = self(),
    CleanupFun = fun(CleanupUserId, SessionId) ->
        TestPid ! {cleanup, CleanupUserId, SessionId},
        ok
    end,
    {ok, NewVoiceStates, NewSessions} = voice_disconnect_common:disconnect_user_if_in_channel(
        UserId, 1, VoiceStates, Sessions, CleanupFun
    ),
    ?assertEqual(#{}, NewVoiceStates),
    ?assertEqual([<<"session-other">>], maps:keys(NewSessions)),
    CleanedSessions = lists:sort([
        receive
            {cleanup, UserId, SId} -> SId
        after 1000 ->
            error(cleanup_not_called)
        end
     || _ <- [1, 2]
    ]),
    ?assertEqual([<<"session-a">>, <<"session-b">>], CleanedSessions).

new_call_test_state(Overrides) ->
    maps:merge(
        #{
            channel_id => 1,
            message_id => 1,
            region => undefined,
            ringing => [],
            pending_ringing => [],
            recipients => [],
            voice_states => #{},
            sessions => #{},
            pending_connections => #{},
            initiator_ready => false,
            ringing_timers => #{},
            idle_timer => undefined,
            created_at => 0,
            participants_history => sets:new(),
            last_call_event => undefined
        },
        Overrides
    ).
