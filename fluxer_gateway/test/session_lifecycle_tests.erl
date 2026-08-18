%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_lifecycle_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

serialize_state_test() ->
    State = #{
        id => <<"session123">>,
        user_id => 12345,
        user_data => #{<<"username">> => <<"test">>},
        version => 9,
        seq => 10,
        ack_seq => 5,
        properties => #{},
        status => online,
        afk => false,
        mobile => false,
        buffer => [],
        ready => undefined,
        guilds => #{},
        active_guilds => sets:from_list([123]),
        shard => {2, 8},
        collected_guild_states => [],
        collected_sessions => [],
        collected_presences => []
    },
    Serialized = session_lifecycle:serialize_state(State),
    ?assertEqual(<<"session123">>, maps:get(id, Serialized)),
    ?assertEqual(<<"12345">>, maps:get(user_id, Serialized)),
    ?assertEqual({2, 8}, maps:get(shard, Serialized)),
    ?assert(sets:is_element(123, maps:get(active_guilds, Serialized))),
    ?assertEqual(10, maps:get(seq, Serialized)),
    ok.

serialize_transfer_state_includes_resume_fields_test() ->
    State = #{
        id => <<"session-transfer">>,
        user_id => 42,
        user_data => #{<<"username">> => <<"test">>},
        custom_status => null,
        version => 9,
        token_hash => <<"token_hash">>,
        auth_session_id_hash => <<"auth_hash">>,
        properties => #{},
        status => online,
        resume_status => idle,
        afk => false,
        mobile => true,
        guilds => #{123 => undefined},
        active_guilds => sets:from_list([123]),
        ready => #{},
        bot => false,
        shard => {2, 8},
        ignored_events => #{<<"TYPING_START">> => true},
        initial_guild_id => 123,
        debounce_reactions => true,
        channels => #{},
        relationships => #{},
        seq => 10,
        ack_seq => 8,
        buffer => [#{seq => 9}],
        collected_guild_states => [],
        collected_sessions => [],
        collected_presences => []
    },
    TransferState = session_lifecycle:serialize_transfer_state(State),
    ?assertEqual(<<"token_hash">>, maps:get(token_hash, TransferState)),
    ?assertEqual({2, 8}, maps:get(shard, TransferState)),
    ?assertEqual(idle, maps:get(resume_status, TransferState)),
    ?assert(sets:is_element(123, maps:get(active_guilds, TransferState))),
    ?assertEqual(10, maps:get(seq, TransferState)),
    ?assertEqual(8, maps:get(ack_seq, TransferState)),
    ?assertEqual([#{seq => 9}], maps:get(buffer, TransferState)).

serialize_transfer_state_strips_socket_pid_test() ->
    State = #{
        id => <<"session-transfer-socket">>,
        user_id => 42,
        user_data => #{},
        custom_status => null,
        version => 9,
        token_hash => <<"token_hash">>,
        auth_session_id_hash => <<"auth_hash">>,
        properties => #{},
        status => online,
        resume_status => online,
        afk => false,
        mobile => false,
        socket_pid => self(),
        guilds => #{},
        ready => #{},
        bot => false,
        shard => undefined,
        ignored_events => #{},
        initial_guild_id => undefined,
        active_guilds => sets:new(),
        debounce_reactions => false,
        channels => #{},
        relationships => #{},
        seq => 0,
        ack_seq => 0,
        buffer => [],
        collected_guild_states => [],
        collected_sessions => [],
        collected_presences => []
    },
    TransferState = session_lifecycle:serialize_transfer_state(State),
    ?assertEqual(undefined, maps:get(socket_pid, TransferState)).

heartbeat_ack_recalculates_buffer_bytes_test() ->
    Event1 = #{seq => 1, event => message_create, data => #{<<"content">> => <<"one">>}},
    Event2 = #{seq => 2, event => message_create, data => #{<<"content">> => <<"two">>}},
    State0 = #{ack_seq => 0, buffer => [Event1, Event2], buffer_bytes => 999999},
    {reply, true, State1} = session_lifecycle:handle_heartbeat_ack(1, State0),
    ?assertEqual([Event2], maps:get(buffer, State1)),
    ?assertEqual(session_init:replay_buffer_bytes([Event2]), maps:get(buffer_bytes, State1)).

heartbeat_ack_tolerates_backwards_ack_without_trimming_test() ->
    Event = #{seq => 5, event => message_create, data => #{}},
    State0 = #{ack_seq => 5, buffer => [Event], buffer_bytes => 123},
    {reply, true, State1} = session_lifecycle:handle_heartbeat_ack(3, State0),
    ?assertEqual(5, maps:get(ack_seq, State1)),
    ?assertEqual([Event], maps:get(buffer, State1)).

handle_resume_clamps_truncated_gap_in_replay_buffer_test() ->
    State0 = resume_test_state(#{
        seq => 5,
        buffer => [
            #{seq => 4, event => message_create, data => #{}},
            #{seq => 5, event => message_create, data => #{}}
        ]
    }),
    {reply, {ok, Missed, 5}, State1} = session_lifecycle:handle_resume(2, self(), State0),
    ?assertEqual([4, 5], [maps:get(seq, Event) || Event <- Missed]),
    ?assertEqual(self(), maps:get(socket_pid, State1)).

handle_resume_clamps_skipped_event_hole_test() ->
    State0 = resume_test_state(#{
        seq => 5,
        buffer => [
            #{seq => 3, event => message_create, data => #{}},
            #{seq => 5, event => message_create, data => #{}}
        ]
    }),
    {reply, {ok, Missed, 5}, _State1} = session_lifecycle:handle_resume(2, self(), State0),
    ?assertEqual([3, 5], [maps:get(seq, Event) || Event <- Missed]).

handle_resume_rejects_seq_ahead_of_current_test() ->
    State0 = resume_test_state(#{
        seq => 5,
        buffer => [#{seq => 5, event => message_create, data => #{}}]
    }),
    {reply, invalid_seq, State0} = session_lifecycle:handle_resume(9, self(), State0).

handle_resume_rejects_seq_below_ack_seq_test() ->
    State0 = resume_test_state(#{
        seq => 10,
        ack_seq => 8,
        buffer => [#{seq => 9, event => message_create, data => #{}}]
    }),
    {reply, invalid_seq, State0} = session_lifecycle:handle_resume(5, self(), State0).

handle_resume_accepts_seq_at_ack_seq_test() ->
    State0 = resume_test_state(#{
        seq => 10,
        ack_seq => 8,
        presence_pid => undefined,
        buffer => [
            #{seq => 9, event => message_create, data => #{}},
            #{seq => 10, event => message_create, data => #{}}
        ]
    }),
    {reply, {ok, Missed, 10}, _State1} = session_lifecycle:handle_resume(8, self(), State0),
    ?assertEqual([9, 10], [maps:get(seq, Event) || Event <- Missed]).

handle_resume_accepts_contiguous_replay_buffer_test() ->
    State0 = resume_test_state(#{
        seq => 5,
        buffer => [
            #{seq => 3, event => message_create, data => #{}},
            #{seq => 4, event => message_create, data => #{}},
            #{seq => 5, event => message_create, data => #{}}
        ]
    }),
    {reply, {ok, Missed, 5}, State1} = session_lifecycle:handle_resume(2, self(), State0),
    ?assertEqual([3, 4, 5], [maps:get(seq, Event) || Event <- Missed]),
    ?assertEqual(self(), maps:get(socket_pid, State1)).

handle_resume_cancels_pending_resume_timer_test() ->
    Token = make_ref(),
    TimerRef = erlang:send_after(constants:resume_timeout(), self(), {resume_timeout, Token}),
    State0 = resume_test_state(#{
        seq => 1,
        buffer => [#{seq => 1, event => message_create, data => #{}}],
        resume_timer => {Token, TimerRef}
    }),
    State1 = resume_success_state(State0),
    ?assertEqual(undefined, maps:get(resume_timer, State1)).

handle_resume_replaces_existing_socket_test() ->
    TestPid = self(),
    OldSocketPid = spawn(fun() -> old_socket_test_loop(TestPid) end),
    OldRef = monitor(process, OldSocketPid),
    State0 = resume_test_state(#{
        seq => 1,
        buffer => [#{seq => 1, event => message_create, data => #{}}],
        socket_pid => OldSocketPid,
        socket_mref => OldRef
    }),
    State1 = resume_success_state(State0),
    ?assertEqual(self(), maps:get(socket_pid, State1)),
    ?assert(is_reference(maps:get(socket_mref, State1))),
    ?assertNotEqual(OldRef, maps:get(socket_mref, State1)),
    receive
        {old_socket, session_reconnect} -> ok
    after 200 ->
        ?assert(false)
    end.

handle_resume_restores_resume_status_after_offline_timer_test() ->
    State0 = resume_test_state(#{
        seq => 1,
        buffer => [#{seq => 1, event => message_create, data => #{}}],
        status => offline,
        resume_status => dnd,
        presence_pid => self()
    }),
    {reply, {ok, _Missed, 1}, State1} = session_lifecycle:handle_resume(0, self(), State0),
    ?assertEqual(dnd, maps:get(status, State1)),
    ?assertEqual(dnd, maps:get(resume_status, State1)),
    receive
        {'$gen_call', {Worker, Tag}, {session_connect, PresenceUpdate}} ->
            Worker ! {Tag, ok},
            ?assertEqual(dnd, maps:get(status, PresenceUpdate))
    after 200 ->
        ?assert(false)
    end.

handle_resume_cancels_pending_offline_timer_test() ->
    Token = make_ref(),
    TimerRef = erlang:send_after(5000, self(), {resume_offline_timeout, Token}),
    State0 = resume_test_state(#{
        seq => 1,
        buffer => [#{seq => 1, event => message_create, data => #{}}],
        offline_timer => {Token, TimerRef}
    }),
    State1 = resume_success_state(State0),
    ?assertEqual(undefined, maps:get(offline_timer, State1)).

handle_resume_reconnects_presence_when_unattached_test() ->
    SocketPid = spawn(fun resume_loop/0),
    State0 = resume_test_state(#{
        seq => 5,
        presence_pid => undefined,
        buffer => [#{seq => 5, event => message_create, data => #{}}]
    }),
    {reply, {ok, _Missed, 5}, State1} = session_lifecycle:handle_resume(5, SocketPid, State0),
    ?assertEqual(undefined, maps:get(presence_pid, State1)),
    ?assert(received_presence_connect()),
    SocketPid ! stop.

handle_resume_reconnects_presence_when_owner_moved_test() ->
    with_presence_owner(['presence@nohost'], fun() ->
        SocketPid = spawn(fun resume_loop/0),
        StalePresence = spawn(fun resume_loop/0),
        StaleRef = monitor(process, StalePresence),
        State0 = resume_test_state(#{
            seq => 1,
            user_id => 9999,
            presence_pid => StalePresence,
            presence_mref => StaleRef,
            buffer => [#{seq => 1, event => message_create, data => #{}}]
        }),
        {reply, {ok, _Missed, 1}, State1} = session_lifecycle:handle_resume(
            0, SocketPid, State0
        ),
        ?assertEqual(undefined, maps:get(presence_pid, State1)),
        ?assert(received_presence_connect()),
        SocketPid ! stop,
        StalePresence ! stop
    end).

handle_resume_refreshes_healthy_presence_attachment_test() ->
    SocketPid = spawn(fun resume_loop/0),
    State0 = resume_test_state(#{
        seq => 1,
        presence_pid => self(),
        buffer => [#{seq => 1, event => message_create, data => #{}}]
    }),
    {reply, {ok, _Missed, 1}, State1} = session_lifecycle:handle_resume(0, SocketPid, State0),
    ?assertEqual(self(), maps:get(presence_pid, State1)),
    receive
        {'$gen_call', {Worker, Tag}, {session_connect, Req}} ->
            Worker ! {Tag, ok},
            ?assertEqual(<<"session-resume-test">>, maps:get(session_id, Req))
    after 1000 ->
        ?assert(false, healthy_resume_did_not_refresh_presence)
    end,
    SocketPid ! stop.

resume_loop() ->
    receive
        stop -> ok;
        _ -> resume_loop()
    after 5000 -> ok
    end.

received_presence_connect() ->
    receive
        {presence_connect, 0} -> true
    after 200 -> false
    end.

with_presence_owner(RoleNodes, Fun) ->
    MembersKey = {gateway_cluster_membership, members},
    RoleKey = {gateway_cluster_membership, members_by_role},
    PrevMembers = persistent_term:get(MembersKey, undefined),
    PrevRoles = persistent_term:get(RoleKey, undefined),
    persistent_term:put(MembersKey, lists:usort([node() | RoleNodes])),
    persistent_term:put(RoleKey, #{presence => RoleNodes}),
    try
        Fun()
    after
        restore_persistent_term(MembersKey, PrevMembers),
        restore_persistent_term(RoleKey, PrevRoles)
    end.

restore_persistent_term(Key, undefined) ->
    persistent_term:erase(Key);
restore_persistent_term(Key, Value) ->
    persistent_term:put(Key, Value).

handle_resume_offline_timeout_preserves_resume_status_test() ->
    Token = make_ref(),
    State0 = resume_test_state(#{
        status => idle,
        resume_status => idle,
        presence_pid => self(),
        offline_timer => {Token, make_ref()}
    }),
    {noreply, State1} = session_lifecycle:handle_resume_offline_timeout(
        {resume_offline_timeout, Token}, State0
    ),
    ?assertEqual(offline, maps:get(status, State1)),
    ?assertEqual(idle, maps:get(resume_status, State1)),
    ?assertEqual(undefined, maps:get(offline_timer, State1)),
    receive
        {'$gen_cast', {presence_update, PresenceUpdate}} ->
            ?assertEqual(offline, maps:get(status, PresenceUpdate))
    after 200 ->
        ?assert(false)
    end.

old_socket_test_loop(TestPid) ->
    receive
        Msg -> TestPid ! {old_socket, Msg}
    after 1000 ->
        ok
    end.

resume_success_state(State0) ->
    {reply, {ok, _Missed, 1}, State1} = session_lifecycle:handle_resume(0, self(), State0),
    State1.

handle_resume_registers_the_session_process_with_presence_test() ->
    SessionPid = self(),
    Presence = spawn(fun() -> fake_presence_loop(SessionPid) end),
    State0 = resume_test_state(#{presence_pid => Presence, seq => 1}),
    {reply, {ok, [], 1}, _State1} = session_lifecycle:handle_resume(1, self(), State0),
    receive
        {presence_session_connect, Request} ->
            ?assertEqual(SessionPid, maps:get(session_pid, Request)),
            ?assertEqual(<<"session-resume-test">>, maps:get(session_id, Request))
    after 2000 ->
        ?assert(false, presence_session_connect_not_received)
    end.

fake_presence_loop(Parent) ->
    receive
        {'$gen_call', From, {session_connect, Request}} ->
            Parent ! {presence_session_connect, Request},
            gen_server:reply(From, {ok, []}),
            fake_presence_loop(Parent);
        _Other ->
            fake_presence_loop(Parent)
    after 5000 ->
        ok
    end.

resume_test_state(Overrides) ->
    maps:merge(
        #{
            id => <<"session-resume-test">>,
            status => online,
            afk => false,
            mobile => false,
            presence_pid => undefined,
            socket_pid => undefined,
            socket_mref => undefined,
            resume_timer => undefined,
            buffer => [],
            seq => 0
        },
        Overrides
    ).

terminate_cleans_up_guild_monitors_test() ->
    GuildPid1 = spawn(fun test_wait_for_stop/0),
    GuildPid2 = spawn(fun test_wait_for_stop/0),
    GuildRef1 = monitor(process, GuildPid1),
    GuildRef2 = monitor(process, GuildPid2),
    State = #{
        user_id => 1,
        guilds => #{
            100 => {GuildPid1, GuildRef1},
            200 => {GuildPid2, GuildRef2},
            300 => undefined,
            400 => unavailable
        },
        calls => #{},
        presence_mref => undefined,
        socket_mref => undefined
    },
    ok = session_lifecycle:terminate(normal, State),
    GuildPid1 ! stop,
    GuildPid2 ! stop,
    receive
        {'DOWN', GuildRef1, process, _, _} ->
            ?assert(false, guild_monitor_1_not_cleaned_up)
    after 100 -> ok
    end,
    receive
        {'DOWN', GuildRef2, process, _, _} ->
            ?assert(false, guild_monitor_2_not_cleaned_up)
    after 100 -> ok
    end.

terminate_cleans_up_call_monitors_test() ->
    CallPid = spawn(fun test_wait_for_stop/0),
    CallRef = monitor(process, CallPid),
    State = #{
        user_id => 1,
        guilds => #{},
        calls => #{500 => {CallPid, CallRef}},
        presence_mref => undefined,
        socket_mref => undefined
    },
    ok = session_lifecycle:terminate(normal, State),
    CallPid ! stop,
    receive
        {'DOWN', CallRef, process, _, _} ->
            ?assert(false, call_monitor_not_cleaned_up)
    after 100 -> ok
    end.

terminate_cleans_up_presence_monitor_test() ->
    PresencePid = spawn(fun test_wait_for_stop/0),
    PresenceRef = monitor(process, PresencePid),
    State = #{
        user_id => 1,
        guilds => #{},
        calls => #{},
        presence_pid => PresencePid,
        presence_mref => PresenceRef,
        socket_mref => undefined
    },
    ok = session_lifecycle:terminate(normal, State),
    PresencePid ! stop,
    receive
        {'DOWN', PresenceRef, process, _, _} ->
            ?assert(false, presence_monitor_not_cleaned_up)
    after 100 -> ok
    end.

terminate_cleans_up_socket_monitor_test() ->
    SocketPid = spawn(fun test_wait_for_stop/0),
    SocketRef = monitor(process, SocketPid),
    State = #{
        user_id => 1,
        guilds => #{},
        calls => #{},
        presence_mref => undefined,
        socket_pid => SocketPid,
        socket_mref => SocketRef
    },
    ok = session_lifecycle:terminate(normal, State),
    SocketPid ! stop,
    receive
        {'DOWN', SocketRef, process, _, _} ->
            ?assert(false, socket_monitor_not_cleaned_up)
    after 100 -> ok
    end.

terminate_handles_empty_state_gracefully_test() ->
    ok = session_lifecycle:terminate(normal, #{}).

terminate_handles_missing_fields_gracefully_test() ->
    ok = session_lifecycle:terminate(shutdown, #{user_id => 1}).

test_wait_for_stop() ->
    receive
        stop -> ok
    after 30000 -> ok
    end.
