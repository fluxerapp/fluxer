%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_move_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

token_rpc_data() ->
    #{
        <<"token">> => <<"tok">>,
        <<"endpoint">> => <<"wss://voice.example">>,
        <<"connectionId">> => <<"new-conn">>,
        <<"regionId">> => <<"us-east">>,
        <<"serverId">> => <<"voice-1">>
    }.

test_member(UserId) ->
    #{
        <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
        <<"mute">> => false,
        <<"deaf">> => false
    }.

existing_voice_state(ConnId, UserId, ChannelIdBin) ->
    #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"guild_id">> => <<"999">>,
        <<"channel_id">> => ChannelIdBin,
        <<"connection_id">> => ConnId,
        <<"session_id">> => <<"sess1">>,
        <<"self_mute">> => false,
        <<"self_deaf">> => true,
        <<"self_video">> => false,
        <<"self_stream">> => false,
        <<"is_mobile">> => false,
        <<"suppress">> => false,
        <<"mute">> => false,
        <<"deaf">> => false,
        <<"viewer_stream_keys">> => [],
        <<"e2ee_capable">> => false,
        <<"member">> => test_member(UserId),
        <<"version">> => 3
    }.

base_move_state(VoiceStates) ->
    #{
        id => 999,
        data => #{
            <<"id">> => <<"999">>,
            <<"guild">> => #{<<"owner_id">> => <<"10">>},
            <<"channels">> => [
                #{<<"id">> => <<"100">>, <<"type">> => 2},
                #{<<"id">> => <<"200">>, <<"type">> => 2}
            ],
            <<"members">> => [test_member(10)]
        },
        sessions => #{},
        voice_states => VoiceStates,
        pending_voice_connections => #{}
    }.

move_context() ->
    #{
        user_id => 10,
        session_id => <<"sess1">>,
        connection_id => <<"old-conn">>,
        self_mute => false,
        self_deaf => true,
        self_video => false,
        self_stream => false,
        is_mobile => false,
        viewer_stream_keys => undefined,
        latitude => <<"1.0">>,
        longitude => <<"2.0">>,
        e2ee_capable => false,
        bot => false
    }.

with_token_rpc(Result, Fun) ->
    drain_stale_dispatches(),
    meck:new(rpc_client, [passthrough, no_link, non_strict]),
    meck:expect(rpc_client, call, fun(_Request) -> Result end),
    try
        Fun()
    after
        meck:unload(rpc_client)
    end.

with_guild_voice_server_stub(Fun) ->
    meck:new(guild_voice_server, [passthrough, no_link]),
    meck:expect(guild_voice_server, resolve, fun(_GuildId, GuildPid) -> GuildPid end),
    try
        Fun()
    after
        meck:unload(guild_voice_server)
    end.

drain_stale_dispatches() ->
    receive
        {'$gen_cast', {dispatch, _Event, _Payload}} ->
            drain_stale_dispatches()
    after 0 ->
        ok
    end.

assert_no_dispatch() ->
    receive
        {'$gen_cast', {dispatch, Event, Payload}} ->
            ?assert(false, {unexpected_dispatch, Event, Payload})
    after 50 ->
        ok
    end.

reply_map(Reply) when is_map(Reply) -> Reply.

run_client_move(VoiceStates, State) ->
    {reply, Reply, NewState} = guild_voice_connection_move:handle_client_channel_move(
        move_context(), 200, test_member(10), <<"old-conn">>, VoiceStates, State
    ),
    {Reply, NewState}.

client_move_token_failure_preserves_connection_test() ->
    with_token_rpc({error, timeout}, fun client_move_token_failure_scenario/0).

client_move_token_failure_scenario() ->
    ExistingVS = existing_voice_state(<<"old-conn">>, 10, <<"100">>),
    VoiceStates = #{<<"old-conn">> => ExistingVS},
    State = base_move_state(VoiceStates),
    {Error, NewState} = run_client_move(VoiceStates, State),
    ?assertMatch({error, _, voice_token_failed}, Error),
    ?assertEqual(
        ExistingVS,
        maps:get(<<"old-conn">>, maps:get(voice_states, NewState))
    ),
    ?assertEqual(#{}, maps:get(pending_voice_connections, NewState)),
    assert_no_dispatch().

client_move_success_disconnects_and_stores_pending_test() ->
    with_token_rpc({ok, token_rpc_data()}, fun client_move_success_scenario/0).

client_move_success_scenario() ->
    ExistingVS = existing_voice_state(<<"old-conn">>, 10, <<"100">>),
    VoiceStates = #{<<"old-conn">> => ExistingVS},
    State = base_move_state(VoiceStates),
    {Reply0, NewState} = run_client_move(VoiceStates, State),
    Reply = reply_map(Reply0),
    ?assertEqual(true, maps:get(success, Reply)),
    ?assertEqual(true, maps:get(needs_token, Reply)),
    ?assertEqual(<<"tok">>, maps:get(token, Reply)),
    ?assertEqual(<<"new-conn">>, maps:get(connection_id, Reply)),
    ?assertNot(maps:is_key(<<"old-conn">>, maps:get(voice_states, NewState))),
    Pending = maps:get(pending_voice_connections, NewState),
    ?assert(maps:is_key(<<"new-conn">>, Pending)),
    Meta = maps:get(<<"new-conn">>, Pending),
    ?assert(is_binary(maps:get(token_nonce, Meta))),
    ?assert(maps:get(expires_at, Meta) > maps:get(created_at, Meta)),
    ?assertEqual(
        ok,
        guild_voice_connection_pending:validate_pending_nonce_and_expiry(
            maps:get(token_nonce, Meta), Meta
        )
    ).

client_move_reply_voice_state_is_complete_and_coordinate_free_test() ->
    with_token_rpc({ok, token_rpc_data()}, fun client_move_reply_shape_scenario/0).

client_move_reply_shape_scenario() ->
    ExistingVS = existing_voice_state(<<"old-conn">>, 10, <<"100">>),
    VoiceStates = #{<<"old-conn">> => ExistingVS},
    State = base_move_state(VoiceStates),
    {Reply0, NewState} = run_client_move(VoiceStates, State),
    Reply = reply_map(Reply0),
    ReplyVS = maps:get(voice_state, Reply),
    ?assertNot(maps:is_key(<<"latitude">>, ReplyVS)),
    ?assertNot(maps:is_key(<<"longitude">>, ReplyVS)),
    ?assertEqual(<<"200">>, maps:get(<<"channel_id">>, ReplyVS)),
    ?assertEqual(<<"new-conn">>, maps:get(<<"connection_id">>, ReplyVS)),
    ?assertEqual(true, maps:get(<<"self_deaf">>, ReplyVS)),
    assert_complete_voice_state_fields(ReplyVS),
    PendingVS = maps:get(
        voice_state, maps:get(<<"new-conn">>, maps:get(pending_voice_connections, NewState))
    ),
    ?assertEqual(<<"1.0">>, maps:get(<<"latitude">>, PendingVS)).

assert_complete_voice_state_fields(VS) ->
    Fields = [
        <<"member">>,
        <<"mute">>,
        <<"deaf">>,
        <<"suppress">>,
        <<"viewer_stream_keys">>,
        <<"e2ee_capable">>,
        <<"version">>,
        <<"session_id">>
    ],
    lists:foreach(fun(Field) -> ?assert(maps:is_key(Field, VS), {missing, Field}) end, Fields).

moderator_move_reply_includes_session_data_test() ->
    ExistingVS = existing_voice_state(<<"old-conn">>, 10, <<"100">>),
    VoiceStates = #{<<"old-conn">> => ExistingVS},
    State = base_move_state(VoiceStates),
    {reply, Reply0, NewState} = guild_voice_move_execute:handle_move(
        #{<<"old-conn">> => ExistingVS}, 200, 10, 10, <<"old-conn">>, VoiceStates, State
    ),
    Reply = reply_map(Reply0),
    ?assertEqual(true, maps:get(success, Reply)),
    ?assertEqual(true, maps:get(needs_token, Reply)),
    [SessionInfo] = maps:get(session_data, Reply),
    ?assertEqual(<<"old-conn">>, maps:get(connection_id, SessionInfo)),
    ?assertEqual(<<"sess1">>, maps:get(session_id, SessionInfo)),
    ?assertEqual(true, maps:get(self_deaf, SessionInfo)),
    ?assertEqual(false, maps:get(e2ee_capable, SessionInfo)),
    ?assertNot(maps:is_key(<<"old-conn">>, maps:get(voice_states, NewState))).

moderator_move_to_unknown_channel_fails_test() ->
    ExistingVS = existing_voice_state(<<"old-conn">>, 10, <<"100">>),
    VoiceStates = #{<<"old-conn">> => ExistingVS},
    State = base_move_state(VoiceStates),
    {reply, Error, _} = guild_voice_move_execute:handle_move(
        #{<<"old-conn">> => ExistingVS}, 12345, 10, 10, <<"old-conn">>, VoiceStates, State
    ),
    ?assertMatch({error, _, voice_channel_not_found}, Error).

moderator_move_without_connections_fails_test() ->
    State = base_move_state(#{}),
    {reply, ErrorWithConn, _} = guild_voice_move_execute:handle_move(
        #{}, 200, 10, 10, <<"old-conn">>, #{}, State
    ),
    ?assertMatch({error, _, voice_connection_not_found}, ErrorWithConn),
    {reply, ErrorNoConn, _} = guild_voice_move_execute:handle_move(
        #{}, 200, 10, 10, null, #{}, State
    ),
    ?assertMatch({error, _, voice_user_not_in_voice}, ErrorNoConn).

moderator_disconnect_move_purges_counts_cache_test() ->
    RegionId = <<"region-move-test">>,
    VS = (existing_voice_state(<<"move-count-conn">>, 10, <<"100">>))#{
        <<"region_id">> => RegionId,
        <<"server_id">> => <<"server-move-test">>
    },
    _ = voice_state_counts_cache:upsert_voice_state(VS),
    ?assertEqual(1, region_count(RegionId)),
    VoiceStates = #{<<"move-count-conn">> => VS},
    State = base_move_state(VoiceStates),
    {reply, Reply0, NewState} = guild_voice_move_execute:handle_move(
        VoiceStates, null, 10, 20, <<"move-count-conn">>, VoiceStates, State
    ),
    Reply = reply_map(Reply0),
    ?assertEqual(true, maps:get(success, Reply)),
    ?assertEqual(10, maps:get(user_id, Reply)),
    ?assertEqual(0, region_count(RegionId)),
    ?assertNot(maps:is_key(<<"move-count-conn">>, maps:get(voice_states, NewState))).

region_count(RegionId) ->
    Counts = voice_state_counts_cache:get_local_counts(),
    Regions = maps:get(<<"regions">>, Counts, []),
    lists:foldl(
        fun(Entry, Acc) -> add_region_entry_count(Entry, RegionId, Acc) end,
        0,
        Regions
    ).

add_region_entry_count(Entry, RegionId, Acc) ->
    case maps:get(<<"region_id">>, Entry, undefined) of
        RegionId -> Acc + maps:get(<<"voice_state_count">>, Entry, 0);
        _ -> Acc
    end.

guild_server_stub(StateData, TestPid) ->
    spawn_link(fun() -> guild_server_stub_loop(StateData, TestPid) end).

guild_server_stub_loop(StateData, TestPid) ->
    receive
        {'$gen_call', From, {get_voice_guild_state}} ->
            gen:reply(From, undefined),
            guild_server_stub_loop(StateData, TestPid);
        {'$gen_call', From, {get_sessions}} ->
            gen:reply(From, StateData),
            guild_server_stub_loop(StateData, TestPid);
        {'$gen_call', From, {store_pending_connection, ConnId, Meta}} ->
            TestPid ! {stored_pending, ConnId, Meta},
            gen:reply(From, ok),
            guild_server_stub_loop(StateData, TestPid)
    after 5000 ->
        ok
    end.

receive_stored_pending() ->
    receive
        {stored_pending, ConnId, Meta} -> {ConnId, Meta}
    after 1000 ->
        error(pending_connection_not_stored)
    end.

receive_voice_server_update() ->
    receive
        {'$gen_cast', {dispatch, voice_server_update, Payload}} -> Payload
    after 1000 ->
        error(voice_server_update_not_dispatched)
    end.

send_moderator_move_update(StateData) ->
    ExistingVS = existing_voice_state(<<"old-conn">>, 10, <<"100">>),
    StubPid = guild_server_stub(StateData, self()),
    SessionInfo = guild_voice_state:extract_session_info_from_voice_state(
        <<"old-conn">>, ExistingVS
    ),
    ok = guild_voice_move_execute:send_single_voice_server_update(
        999, 200, SessionInfo, StubPid
    ),
    receive_stored_pending().

moderator_move_token_delivery_stores_restorable_pending_test() ->
    with_token_rpc({ok, token_rpc_data()}, fun() ->
        with_guild_voice_server_stub(fun moderator_move_pending_scenario/0)
    end).

moderator_move_pending_scenario() ->
    StateData = (base_move_state(#{}))#{
        sessions => #{<<"sess1">> => #{pid => self(), user_id => 10}}
    },
    {ConnId, Meta} = send_moderator_move_update(StateData),
    ?assertEqual(<<"new-conn">>, ConnId),
    ?assert(is_binary(maps:get(token_nonce, Meta))),
    ?assert(is_integer(maps:get(created_at, Meta))),
    ?assert(maps:get(expires_at, Meta) > maps:get(created_at, Meta)),
    ?assertEqual(10, maps:get(user_id, Meta)),
    ?assertEqual(200, maps:get(channel_id, Meta)),
    ?assertEqual(true, maps:get(self_deaf, Meta)),
    ?assertEqual(
        ok,
        guild_voice_connection_pending:validate_pending_nonce_and_expiry(
            maps:get(token_nonce, Meta), Meta
        )
    ),
    ?assertEqual(
        {error, voice_nonce_mismatch},
        guild_voice_connection_pending:validate_pending_nonce_and_expiry(
            <<"wrong-nonce">>, Meta
        )
    ),
    Payload = receive_voice_server_update(),
    ?assertEqual(<<"tok">>, maps:get(<<"token">>, Payload)),
    ?assertEqual(<<"new-conn">>, maps:get(<<"connection_id">>, Payload)),
    assert_pending_restores(ConnId, Meta).

assert_pending_restores(ConnId, Meta) ->
    RestoreState = #{
        id => 999,
        sessions => #{},
        voice_states => #{},
        pending_voice_connections => #{ConnId => Meta}
    },
    {ok, RestoredVoiceStates, _RestoredState} =
        guild_voice_connection_pending:maybe_restore_pending_connection(
            ConnId, 200, 10, #{}, RestoreState
        ),
    RestoredVS = maps:get(ConnId, RestoredVoiceStates),
    ?assertEqual(<<"200">>, maps:get(<<"channel_id">>, RestoredVS)),
    ?assertEqual(<<"10">>, maps:get(<<"user_id">>, RestoredVS)),
    ?assertEqual(true, maps:get(<<"self_deaf">>, RestoredVS)),
    ?assert(maps:is_key(<<"member">>, RestoredVS)),
    ?assert(maps:is_key(<<"version">>, RestoredVS)).

moderator_move_pending_sweep_expires_stale_entries_test() ->
    with_token_rpc({ok, token_rpc_data()}, fun() ->
        with_guild_voice_server_stub(fun moderator_move_sweep_scenario/0)
    end).

moderator_move_sweep_scenario() ->
    {ConnId, Meta} = send_moderator_move_update(base_move_state(#{})),
    ExpiredMeta = Meta#{
        created_at => maps:get(created_at, Meta) - 600000,
        expires_at => maps:get(expires_at, Meta) - 600000
    },
    SweepState = #{
        id => 999,
        sessions => #{},
        voice_states => #{},
        pending_voice_connections => #{ConnId => ExpiredMeta}
    },
    SweptState = guild_voice_connection_pending:sweep_expired_pending_joins(SweepState),
    ?assertEqual(#{}, maps:get(pending_voice_connections, SweptState)),
    ?assertEqual(
        {error, voice_pending_expired},
        guild_voice_connection_pending:validate_pending_nonce_and_expiry(
            maps:get(token_nonce, ExpiredMeta), ExpiredMeta
        )
    ).

private_member(UserId) ->
    #{
        <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
        <<"roles">> => [],
        <<"mute">> => false,
        <<"deaf">> => false
    }.

private_move_state(VoiceStates, VirtualAccess) ->
    #{
        id => 999,
        data => #{
            <<"id">> => <<"999">>,
            <<"guild">> => #{<<"owner_id">> => <<"20">>},
            <<"roles">> => [#{<<"id">> => <<"999">>, <<"permissions">> => <<"0">>}],
            <<"members">> => [private_member(10), private_member(20)],
            <<"channels">> => [
                #{<<"id">> => <<"100">>, <<"type">> => 2, <<"permission_overwrites">> => []},
                #{<<"id">> => <<"200">>, <<"type">> => 2, <<"permission_overwrites">> => []}
            ]
        },
        sessions => #{<<"s1">> => #{pid => self(), user_id => 10}},
        voice_states => VoiceStates,
        pending_voice_connections => #{},
        virtual_channel_access => VirtualAccess
    }.

receive_channel_dispatch(Event) ->
    receive
        {'$gen_cast', {dispatch, Event, Payload}} -> Payload
    after 1000 ->
        error({dispatch_not_received, Event})
    end.

moderator_move_into_private_channel_grants_lease_test() ->
    drain_stale_dispatches(),
    ExistingVS = existing_voice_state(<<"old-conn">>, 10, <<"100">>),
    VoiceStates = #{<<"old-conn">> => ExistingVS},
    State = private_move_state(VoiceStates, #{}),
    {reply, Reply0, NewState} = guild_voice_move_execute:handle_move(
        VoiceStates, 200, 10, 20, <<"old-conn">>, VoiceStates, State
    ),
    Reply = reply_map(Reply0),
    ?assertEqual(true, maps:get(success, Reply)),
    ?assertEqual(true, maps:get(needs_token, Reply)),
    ?assert(guild_virtual_channel_access:has_virtual_access(10, 200, NewState)),
    ?assert(guild_virtual_channel_access:is_move_pending(10, 200, NewState)),
    Payload = receive_channel_dispatch(channel_create),
    ?assertEqual(<<"200">>, maps:get(<<"id">>, Payload)),
    ?assertEqual(<<"999">>, maps:get(<<"guild_id">>, Payload)).

moderator_move_out_of_leased_channel_revokes_lease_test() ->
    drain_stale_dispatches(),
    ExistingVS = existing_voice_state(<<"old-conn">>, 10, <<"100">>),
    VoiceStates = #{<<"old-conn">> => ExistingVS},
    State = private_move_state(VoiceStates, #{10 => sets:from_list([100])}),
    {reply, Reply0, NewState} = guild_voice_move_execute:handle_move(
        VoiceStates, 200, 10, 20, <<"old-conn">>, VoiceStates, State
    ),
    ?assertEqual(true, maps:get(success, reply_map(Reply0))),
    ?assertNot(guild_virtual_channel_access:has_virtual_access(10, 100, NewState)),
    ?assert(guild_virtual_channel_access:has_virtual_access(10, 200, NewState)),
    DeletePayload = receive_channel_dispatch(channel_delete),
    ?assertEqual(<<"100">>, maps:get(<<"id">>, DeletePayload)),
    ?assertEqual(<<"999">>, maps:get(<<"guild_id">>, DeletePayload)).

moderator_move_without_lease_dispatches_no_channel_delete_test() ->
    drain_stale_dispatches(),
    ExistingVS = existing_voice_state(<<"old-conn">>, 10, <<"100">>),
    VoiceStates = #{<<"old-conn">> => ExistingVS},
    State = (base_move_state(VoiceStates))#{
        sessions => #{<<"s1">> => #{pid => self(), user_id => 10}}
    },
    {reply, Reply0, _NewState} = guild_voice_move_execute:handle_move(
        VoiceStates, 200, 10, 10, <<"old-conn">>, VoiceStates, State
    ),
    ?assertEqual(true, maps:get(success, reply_map(Reply0))),
    assert_no_channel_delete().

assert_no_channel_delete() ->
    receive
        {'$gen_cast', {dispatch, channel_delete, Payload}} ->
            ?assert(false, {unexpected_channel_delete, Payload})
    after 50 ->
        ok
    end.

self_move_out_of_leased_channel_revokes_lease_test() ->
    with_token_rpc({ok, token_rpc_data()}, fun self_move_revokes_lease_scenario/0).

self_move_revokes_lease_scenario() ->
    ExistingVS = existing_voice_state(<<"old-conn">>, 10, <<"100">>),
    VoiceStates = #{<<"old-conn">> => ExistingVS},
    State = (base_move_state(VoiceStates))#{
        sessions => #{<<"s1">> => #{pid => self(), user_id => 10}},
        virtual_channel_access => #{10 => sets:from_list([100])}
    },
    {_Reply, NewState} = run_client_move(VoiceStates, State),
    ?assertNot(guild_virtual_channel_access:has_virtual_access(10, 100, NewState)),
    DeletePayload = receive_channel_dispatch(channel_delete),
    ?assertEqual(<<"100">>, maps:get(<<"id">>, DeletePayload)).
