%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

get_members_cursor_returns_atom_keys_test() ->
    Member1 = #{<<"user">> => #{<<"id">> => <<"2">>}},
    Member2 = #{<<"user">> => #{<<"id">> => <<"1">>}},
    State = #{data => #{<<"members">> => [Member1, Member2]}},
    {reply, Reply, _NewState} = guild_member_list:get_members_cursor(
        #{<<"limit">> => 1}, State
    ),
    ?assert(maps:is_key(members, Reply)),
    ?assert(maps:is_key(total, Reply)),
    ?assertNot(maps:is_key(<<"members">>, Reply)),
    ?assertNot(maps:is_key(<<"total">>, Reply)).

get_members_cursor_with_after_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>}},
        #{<<"user">> => #{<<"id">> => <<"3">>}}
    ],
    State = #{data => #{<<"members">> => Members}},
    {reply, Reply, _} = guild_member_list:get_members_cursor(
        #{<<"limit">> => 10, <<"after">> => 1}, State
    ),
    ReturnedMembers = maps:get(members, Reply),
    ?assertEqual(2, length(ReturnedMembers)),
    Ids = [guild_member_list_common:get_member_user_id(M) || M <- ReturnedMembers],
    ?assert(lists:all(fun(Id) -> Id > 1 end, Ids)).

get_members_cursor_empty_guild_test() ->
    State = #{data => #{<<"members">> => []}},
    {reply, Reply, _} = guild_member_list:get_members_cursor(#{<<"limit">> => 10}, State),
    ?assertEqual([], maps:get(members, Reply)),
    ?assertEqual(0, maps:get(total, Reply)).

get_members_cursor_zero_limit_test() ->
    Members = [#{<<"user">> => #{<<"id">> => <<"1">>}}],
    State = #{data => #{<<"members">> => Members}},
    {reply, Reply, _} = guild_member_list:get_members_cursor(#{<<"limit">> => 0}, State),
    ?assertEqual([], maps:get(members, Reply)).

get_members_cursor_after_last_member_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>}}
    ],
    State = #{data => #{<<"members">> => Members}},
    {reply, Reply, _} = guild_member_list:get_members_cursor(
        #{<<"limit">> => 10, <<"after">> => 999}, State
    ),
    ?assertEqual([], maps:get(members, Reply)).

subscribe_builds_channel_store_lazily_test() ->
    State = base_state(make_subs_tab()),
    {NewState, ShouldSync, Ranges} =
        guild_member_list:subscribe_ranges(<<"s1">>, <<"500">>, [{0, 99}], State),
    try
        Engines = maps:get(channel_member_list_engines, NewState, #{}),
        ?assertEqual(true, ShouldSync),
        ?assertEqual([{0, 99}], Ranges),
        ?assert(maps:is_key(<<"500">>, Engines)),
        ?assertEqual(#{}, maps:get(channel_member_list_engines, State, #{}))
    after
        guild_member_list_channel_engine:destroy_all(NewState)
    end.

subscribe_prunes_other_session_channel_lists_test() ->
    SubsTab = make_subs_tab(),
    State0 = base_state(SubsTab),
    {State1, true, [{0, 99}]} =
        guild_member_list:subscribe_ranges(<<"s1">>, <<"500">>, [{0, 99}], State0),
    {State2, true, [{10, 20}]} =
        guild_member_list:subscribe_ranges(<<"s1">>, <<"600">>, [{10, 20}], State1),
    try
        Engines = maps:get(channel_member_list_engines, State2, #{}),
        ?assertNot(guild_member_list_subs:is_subscribed(<<"500">>, <<"s1">>, SubsTab)),
        ?assert(guild_member_list_subs:is_subscribed(<<"600">>, <<"s1">>, SubsTab)),
        ?assertNot(maps:is_key(<<"500">>, Engines)),
        ?assert(maps:is_key(<<"600">>, Engines))
    after
        guild_member_list_channel_engine:destroy_all(State2)
    end.

unsubscribe_last_session_drops_channel_store_test() ->
    Ref = guild_member_list_engine:new(),
    State = (base_state(make_subs_tab([{<<"500">>, <<"s1">>, [{0, 99}]}])))#{
        channel_member_list_engines => #{<<"500">> => Ref}
    },
    NewState = guild_member_list:unsubscribe_session(<<"s1">>, State),
    Engines = maps:get(channel_member_list_engines, NewState, #{}),
    ?assertNot(maps:is_key(<<"500">>, Engines)),
    ?assertEqual(undefined, ets:info(Ref, name)).

empty_ranges_subscribe_drops_channel_engine_test() ->
    Ref = guild_member_list_engine:new(),
    State = (base_state(make_subs_tab([{<<"500">>, <<"s1">>, [{0, 99}]}])))#{
        channel_member_list_engines => #{<<"500">> => Ref}
    },
    {NewState, _ShouldSync, _Ranges} = guild_member_list:subscribe_ranges(
        <<"s1">>, <<"500">>, [], State
    ),
    Engines = maps:get(channel_member_list_engines, NewState, #{}),
    ?assertNot(maps:is_key(<<"500">>, Engines)),
    ?assertEqual(undefined, ets:info(Ref, name)).

broadcast_channel_list_dispatches_sync_immediately_test() ->
    with_sync_dispatch_mock(fun run_broadcast_channel_list_dispatches_sync_immediately/0).

run_broadcast_channel_list_dispatches_sync_immediately() ->
    Ref = guild_member_list_engine:new(),
    try
        Member = #{
            <<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"one">>},
            <<"roles">> => []
        },
        OldState = channel_list_state(Ref, make_subs_tab([{<<"500">>, <<"s1">>, [{0, 99}]}]), [
            Member
        ]),
        NewState = OldState#{member_presence => #{1 => #{<<"status">> => <<"online">>}}},
        {ok, QueuedState} = guild_member_list:broadcast_member_list_updates(
            1, OldState, NewState
        ),
        ?assertNot(maps:is_key(pending_member_list_sync_batch, QueuedState)),
        assert_single_channel_sync_dispatch(NewState, QueuedState),
        FlushedState = guild_member_list:flush_pending_member_list_syncs(QueuedState),
        ?assertNot(maps:is_key(pending_member_list_sync_batch, FlushedState)),
        assert_no_sync_dispatch()
    after
        guild_member_list_engine:destroy(Ref)
    end.

broadcast_channel_list_fans_out_repeated_syncs_test() ->
    with_sync_dispatch_mock(fun run_broadcast_channel_list_fans_out_repeated_syncs/0).

run_broadcast_channel_list_fans_out_repeated_syncs() ->
    Ref = guild_member_list_engine:new(),
    try
        Members = [
            #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"one">>}},
            #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"two">>}}
        ],
        OldState = channel_list_state(
            Ref, make_subs_tab([{<<"500">>, <<"s1">>, [{0, 99}]}]), Members
        ),
        NewState1 = OldState#{coalesce_marker => first},
        {ok, QueuedState1} = guild_member_list:broadcast_member_list_updates(
            1, OldState, NewState1
        ),
        ?assertNot(maps:is_key(pending_member_list_sync_batch, QueuedState1)),
        assert_channel_sync_dispatch_marker(first, QueuedState1),
        NewState2 = QueuedState1#{coalesce_marker => final},
        {ok, QueuedState2} = guild_member_list:broadcast_member_list_updates(
            2, QueuedState1, NewState2
        ),
        ?assertNot(maps:is_key(pending_member_list_sync_batch, QueuedState2)),
        assert_channel_sync_dispatch_marker(final, QueuedState2),
        FlushedState = guild_member_list:flush_pending_member_list_syncs(QueuedState2),
        ?assertEqual(QueuedState2, FlushedState),
        assert_no_sync_dispatch()
    after
        guild_member_list_engine:destroy(Ref)
    end.

sync_batch_fans_out_each_subscribed_channel_list_test() ->
    with_sync_dispatch_mock(fun run_sync_batch_fans_out_each_subscribed_channel_list/0).

run_sync_batch_fans_out_each_subscribed_channel_list() ->
    Ref500 = guild_member_list_engine:new(),
    Ref600 = guild_member_list_engine:new(),
    try
        SubsTab = make_subs_tab([
            {<<"500">>, <<"s1">>, [{0, 99}]},
            {<<"600">>, <<"s2">>, [{10, 20}]}
        ]),
        State = two_channel_list_state(SubsTab, Ref500, Ref600),
        SyncedState = guild_member_list_sync_batch:queue_subscribed_list_syncs(State, SubsTab),
        ?assertEqual(State, SyncedState),
        ?assertNot(maps:is_key(pending_member_list_sync_batch, SyncedState)),
        Dispatches = collect_sync_dispatches(2),
        assert_no_sync_dispatch(),
        assert_channel_sync_dispatches(Dispatches)
    after
        guild_member_list_engine:destroy(Ref500),
        guild_member_list_engine:destroy(Ref600)
    end.

sync_batch_fans_out_before_subscription_churn_test() ->
    with_sync_dispatch_mock(fun run_sync_batch_fans_out_before_subscription_churn/0).

run_sync_batch_fans_out_before_subscription_churn() ->
    SubsTab = make_subs_tab(),
    State0 = base_state(SubsTab),
    {State1, true, [{0, 10}]} =
        guild_member_list:subscribe_ranges(<<"s1">>, <<"500">>, [{0, 10}], State0),
    SyncedState = guild_member_list_sync_batch:queue_list_sync(<<"500">>, State1),
    assert_single_channel_sync_dispatch_with_ranges([{0, 10}], SyncedState),
    UnsubscribedState = guild_member_list:unsubscribe_session(<<"s1">>, SyncedState),
    ?assertNot(
        maps:is_key(<<"500">>, maps:get(channel_member_list_engines, UnsubscribedState, #{}))
    ),
    {ResubscribedState, true, [{20, 30}]} =
        guild_member_list:subscribe_ranges(<<"s1">>, <<"500">>, [{20, 30}], UnsubscribedState),
    try
        FlushedState = guild_member_list_sync_batch:flush_pending_syncs(ResubscribedState),
        ?assertEqual(ResubscribedState, FlushedState),
        assert_no_sync_dispatch()
    after
        guild_member_list_channel_engine:destroy_all(ResubscribedState)
    end.

sync_batch_skips_unsubscribed_lists_at_queue_time_test() ->
    with_sync_dispatch_mock(fun run_sync_batch_skips_unsubscribed_lists_at_queue_time/0).

run_sync_batch_skips_unsubscribed_lists_at_queue_time() ->
    SubsTab = make_subs_tab(),
    State0 = base_state(SubsTab),
    {State1, true, _Ranges} =
        guild_member_list:subscribe_ranges(<<"s1">>, <<"500">>, [{0, 99}], State0),
    _SyncedState = guild_member_list_sync_batch:queue_list_sync(<<"500">>, State1),
    _ = collect_sync_dispatches(1),
    UnsubscribedState = guild_member_list:unsubscribe_session(<<"s1">>, State1),
    ResyncedState = guild_member_list_sync_batch:queue_list_sync(<<"500">>, UnsubscribedState),
    ?assertNot(maps:is_key(pending_member_list_sync_batch, ResyncedState)),
    ?assertNot(
        maps:is_key(<<"500">>, maps:get(channel_member_list_engines, ResyncedState, #{}))
    ),
    assert_no_sync_dispatch().

guild_handle_info_flushes_stale_pending_member_list_batch_test() ->
    with_sync_dispatch_mock(
        fun run_guild_handle_info_flushes_stale_pending_member_list_batch/0
    ).

run_guild_handle_info_flushes_stale_pending_member_list_batch() ->
    Ref = guild_member_list_engine:new(),
    try
        State = channel_list_state(Ref, make_subs_tab([{<<"500">>, <<"s1">>, [{0, 99}]}]), []),
        QueuedState = State#{
            pending_member_list_sync_batch => #{
                pending_list_ids => #{<<"500">> => true}
            }
        },
        {noreply, FlushedState} = guild:handle_info(flush_member_list_sync_batch, QueuedState),
        ?assertNot(maps:is_key(pending_member_list_sync_batch, FlushedState)),
        assert_single_channel_sync_dispatch(State, FlushedState),
        assert_no_sync_dispatch()
    after
        guild_member_list_engine:destroy(Ref)
    end.

sync_batch_fans_out_large_subscribed_set_immediately_test() ->
    with_sync_dispatch_mock(fun run_sync_batch_fans_out_large_subscribed_set_immediately/0).

send_member_list_update_encodes_wire_payload_test() ->
    with_member_list_relay_mock(fun run_send_member_list_update_encodes_wire_payload/0).

run_sync_batch_fans_out_large_subscribed_set_immediately() ->
    ChannelIds = lists:seq(1000, 1063),
    {State, Refs} = stress_channel_list_state(ChannelIds, 4),
    SubsTab = maps:get(member_list_subscriptions, State),
    try
        SyncedState = guild_member_list_sync_batch:queue_subscribed_list_syncs(State, SubsTab),
        ?assertNot(maps:is_key(pending_member_list_sync_batch, SyncedState)),
        Dispatches = collect_sync_dispatches(length(ChannelIds)),
        assert_no_sync_dispatch(),
        assert_channel_sync_dispatches_for_ids(ChannelIds, Dispatches)
    after
        lists:foreach(fun guild_member_list_engine:destroy/1, Refs)
    end.

run_send_member_list_update_encodes_wire_payload() ->
    Sessions = #{<<"s1">> => #{pid => self(), user_id => 42}},
    State = (base_state(make_subs_tab()))#{
        data => #{
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => <<"42">>}, <<"roles">> => []}
            ]
        },
        sessions => Sessions
    },
    Payload = #{
        id => 100,
        <<"ops">> => [
            #{
                <<"op">> => <<"SYNC">>,
                <<"range">> => [0, 0],
                <<"items">> => [#{123 => #{id => 456, <<"roles">> => [789]}}]
            }
        ]
    },
    guild_member_list_subscribe:send_member_list_update_to_sessions(
        <<"0">>, #{<<"s1">> => [{0, 0}]}, Sessions, Payload, State
    ),
    receive
        {member_list_dispatch, [Self], guild_member_list_update, 100, Decoded} when
            Self =:= self()
        ->
            ?assertEqual(
                #{
                    <<"id">> => <<"100">>,
                    <<"ops">> => [
                        #{
                            <<"op">> => <<"SYNC">>,
                            <<"range">> => [0, 0],
                            <<"items">> => [
                                #{
                                    <<"123">> => #{
                                        <<"id">> => <<"456">>, <<"roles">> => [<<"789">>]
                                    }
                                }
                            ]
                        }
                    ]
                },
                Decoded
            )
    after 1000 ->
        ?assert(false)
    end.

broadcast_presence_delta_outside_list_payload_skips_sync_test() ->
    with_sync_dispatch_mock(fun run_broadcast_presence_delta_outside_list_payload_skips_sync/0).

run_broadcast_presence_delta_outside_list_payload_skips_sync() ->
    Ref = guild_member_list_engine:new(),
    try
        State = presence_delta_state(Ref),
        {ok, NewState} = guild_member_list:broadcast_member_list_updates(
            1,
            State,
            State,
            presence_map(<<"online">>, false, null),
            presence_map(<<"online">>, true, null)
        ),
        ?assertEqual(State, NewState),
        assert_no_sync_dispatch()
    after
        guild_member_list_engine:destroy(Ref)
    end.

broadcast_status_change_dispatches_sync_test() ->
    with_sync_dispatch_mock(fun run_broadcast_status_change_dispatches_sync/0).

run_broadcast_status_change_dispatches_sync() ->
    Ref = guild_member_list_engine:new(),
    try
        State = presence_delta_state(Ref),
        {ok, NewState} = guild_member_list:broadcast_member_list_updates(
            1,
            State,
            State,
            presence_map(<<"online">>, false, null),
            presence_map(<<"idle">>, false, null)
        ),
        assert_single_channel_sync_dispatch(State, NewState),
        assert_no_sync_dispatch()
    after
        guild_member_list_engine:destroy(Ref)
    end.

broadcast_custom_status_change_dispatches_sync_test() ->
    with_sync_dispatch_mock(fun run_broadcast_custom_status_change_dispatches_sync/0).

run_broadcast_custom_status_change_dispatches_sync() ->
    Ref = guild_member_list_engine:new(),
    try
        State = presence_delta_state(Ref),
        {ok, NewState} = guild_member_list:broadcast_member_list_updates(
            1,
            State,
            State,
            presence_map(<<"online">>, false, null),
            presence_map(<<"online">>, false, #{<<"text">> => <<"hi">>})
        ),
        assert_single_channel_sync_dispatch(State, NewState),
        assert_no_sync_dispatch()
    after
        guild_member_list_engine:destroy(Ref)
    end.

broadcast_offline_transition_dispatches_sync_test() ->
    with_sync_dispatch_mock(fun run_broadcast_offline_transition_dispatches_sync/0).

run_broadcast_offline_transition_dispatches_sync() ->
    Ref = guild_member_list_engine:new(),
    try
        State = presence_delta_state(Ref),
        {ok, NewState} = guild_member_list:broadcast_member_list_updates(
            1,
            State,
            State,
            presence_map(<<"online">>, false, null),
            presence_map(<<"offline">>, false, null)
        ),
        assert_single_channel_sync_dispatch(State, NewState),
        assert_no_sync_dispatch()
    after
        guild_member_list_engine:destroy(Ref)
    end.

presence_delta_state(Ref) ->
    Member = #{
        <<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"one">>},
        <<"roles">> => []
    },
    channel_list_state(Ref, make_subs_tab([{<<"500">>, <<"s1">>, [{0, 99}]}]), [Member]).

presence_map(Status, Mobile, CustomStatus) ->
    #{
        <<"status">> => Status,
        <<"mobile">> => Mobile,
        <<"afk">> => false,
        <<"custom_status">> => CustomStatus
    }.

has_subs_reports_subscribers_per_list_test() ->
    SubsTab = make_subs_tab([
        {<<"10">>, <<"s0">>, [{0, 99}]},
        {<<"100">>, <<"s1">>, [{0, 99}]},
        {<<"100">>, <<"s2">>, [{0, 49}]},
        {<<"300">>, <<"s3">>, [{0, 99}]}
    ]),
    ?assert(guild_member_list_subs:has_subs(<<"10">>, SubsTab)),
    ?assert(guild_member_list_subs:has_subs(<<"100">>, SubsTab)),
    ?assert(guild_member_list_subs:has_subs(<<"300">>, SubsTab)),
    ?assertNot(guild_member_list_subs:has_subs(<<"1">>, SubsTab)),
    ?assertNot(guild_member_list_subs:has_subs(<<"050">>, SubsTab)),
    ?assertNot(guild_member_list_subs:has_subs(<<"200">>, SubsTab)),
    ?assertNot(guild_member_list_subs:has_subs(<<"400">>, SubsTab)),
    guild_member_list_subs:unsubscribe_session(<<"s3">>, SubsTab),
    ?assertNot(guild_member_list_subs:has_subs(<<"300">>, SubsTab)),
    guild_member_list_subs:unsubscribe_session(<<"s0">>, SubsTab),
    ?assertNot(guild_member_list_subs:has_subs(<<"10">>, SubsTab)),
    ?assert(guild_member_list_subs:has_subs(<<"100">>, SubsTab)),
    guild_member_list_subs:destroy(SubsTab).

has_subs_on_empty_table_test() ->
    SubsTab = make_subs_tab(),
    ?assertNot(guild_member_list_subs:has_subs(<<"500">>, SubsTab)),
    ?assertEqual([], collect_list_ids(SubsTab)),
    guild_member_list_subs:destroy(SubsTab).

fold_list_ids_visits_each_list_once_test() ->
    SubsTab = make_subs_tab([
        {<<"10">>, <<"s0">>, [{0, 99}]},
        {<<"100">>, <<"s1">>, [{0, 99}]},
        {<<"100">>, <<"s2">>, [{0, 49}]},
        {<<"300">>, <<"s3">>, [{0, 99}]}
    ]),
    ListIds = collect_list_ids(SubsTab),
    ?assertEqual([<<"10">>, <<"100">>, <<"300">>], ListIds),
    ?assertEqual(guild_member_list_subs:list_ids(SubsTab), ListIds),
    guild_member_list_subs:destroy(SubsTab).

collect_list_ids(SubsTab) ->
    lists:reverse(
        guild_member_list_subs:fold_list_ids(
            fun(ListId, Acc) -> [ListId | Acc] end, [], SubsTab
        )
    ).

channel_list_state(Ref, SubsTab, Members) ->
    (base_state(SubsTab))#{
        data => #{
            <<"members">> => Members,
            <<"channels">> => [#{<<"id">> => <<"500">>}]
        },
        channel_member_list_engines => #{<<"500">> => Ref}
    }.

two_channel_list_state(SubsTab, Ref500, Ref600) ->
    (base_state(SubsTab))#{
        data => #{
            <<"channels">> => [#{<<"id">> => <<"500">>}, #{<<"id">> => <<"600">>}],
            <<"members">> => []
        },
        channel_member_list_engines => #{<<"500">> => Ref500, <<"600">> => Ref600}
    }.

stress_channel_list_state(ChannelIds, SessionsPerChannel) ->
    SubEntries = stress_sub_entries(ChannelIds, SessionsPerChannel),
    SubsTab = make_subs_tab(SubEntries),
    RefsByListId = maps:from_list([
        {integer_to_binary(ChannelId), guild_member_list_engine:new()}
     || ChannelId <- ChannelIds
    ]),
    State = (base_state(SubsTab))#{
        data => #{
            <<"channels">> => [
                #{<<"id">> => integer_to_binary(ChannelId)}
             || ChannelId <- ChannelIds
            ],
            <<"members">> => []
        },
        channel_member_list_engines => RefsByListId,
        sessions => stress_sessions(SubEntries)
    },
    {State, maps:values(RefsByListId)}.

stress_sub_entries(ChannelIds, SessionsPerChannel) ->
    [
        {
            integer_to_binary(ChannelId),
            stress_session_id(ChannelId, SessionIdx),
            stress_ranges(SessionIdx)
        }
     || ChannelId <- ChannelIds,
        SessionIdx <- lists:seq(1, SessionsPerChannel)
    ].

stress_sessions(SubEntries) ->
    maps:from_list([
        {SessionId, #{pid => self(), user_id => 200000 + Idx}}
     || {Idx, {_ListId, SessionId, _Ranges}} <- lists:zip(
            lists:seq(1, length(SubEntries)), SubEntries
        )
    ]).

stress_session_id(ChannelId, SessionIdx) ->
    <<"stress_", (integer_to_binary(ChannelId))/binary, "_",
        (integer_to_binary(SessionIdx))/binary>>.

stress_ranges(SessionIdx) ->
    case SessionIdx rem 3 of
        0 -> [{0, 20}, {15, 35}];
        1 -> [{10, 30}];
        _ -> [{40, 55}, {56, 70}]
    end.

assert_single_channel_sync_dispatch(NewState, FlushedState) ->
    receive
        {sync_dispatch, ListSubs, Sessions, 500, 100, DispatchState, SyncFun} ->
            ?assertEqual(#{<<"s1">> => [{0, 99}]}, ListSubs),
            ?assertEqual(maps:get(sessions, NewState), Sessions),
            ?assertEqual(FlushedState, DispatchState),
            Payload = SyncFun([{0, 99}]),
            ?assertEqual(<<"500">>, maps:get(<<"channel_id">>, Payload)),
            [Op] = maps:get(<<"ops">>, Payload),
            ?assertEqual(<<"SYNC">>, maps:get(<<"op">>, Op)),
            ?assertEqual([0, 99], maps:get(<<"range">>, Op))
    after 1000 ->
        ?assert(false)
    end.

assert_single_channel_sync_dispatch_with_ranges(ExpectedRanges, FlushedState) ->
    receive
        {sync_dispatch, ListSubs, _Sessions, 500, 100, DispatchState, SyncFun} ->
            ?assertEqual(#{<<"s1">> => ExpectedRanges}, ListSubs),
            ?assertEqual(FlushedState, DispatchState),
            Payload = SyncFun(ExpectedRanges),
            [Op] = maps:get(<<"ops">>, Payload),
            [ExpectedRange] = ExpectedRanges,
            {Start, End} = ExpectedRange,
            ?assertEqual([Start, End], maps:get(<<"range">>, Op))
    after 1000 ->
        ?assert(false)
    end.

assert_channel_sync_dispatch_marker(ExpectedMarker, ExpectedState) ->
    receive
        {sync_dispatch, _ListSubs, _Sessions, 500, 100, DispatchState, _SyncFun} ->
            ?assertEqual(ExpectedMarker, maps:get(coalesce_marker, DispatchState)),
            ?assertEqual(ExpectedState, DispatchState)
    after 1000 ->
        ?assert(false)
    end.

assert_channel_sync_dispatches(Dispatches) ->
    ChannelIds = lists:sort([
        ChannelId
     || {sync_dispatch, _ListSubs, _Sessions, ChannelId, 100, _State, _SyncFun} <-
            Dispatches
    ]),
    ?assertEqual([500, 600], ChannelIds),
    lists:foreach(fun assert_channel_sync_dispatch/1, Dispatches).

assert_channel_sync_dispatches_for_ids(ExpectedChannelIds, Dispatches) ->
    ChannelIds = lists:sort([
        ChannelId
     || {sync_dispatch, _ListSubs, _Sessions, ChannelId, 100, _State, _SyncFun} <-
            Dispatches
    ]),
    ?assertEqual(ExpectedChannelIds, ChannelIds),
    lists:foreach(fun assert_stress_channel_sync_dispatch/1, Dispatches).

with_sync_dispatch_mock(Fun) ->
    meck:new(guild_member_list_subscribe, [passthrough, no_link]),
    Parent = self(),
    meck:expect(
        guild_member_list_subscribe,
        dispatch_sync_to_subscribed_sessions,
        fun(ListSubs, Sessions, ChannelId, GuildId, State, SyncFun) ->
            Parent ! {sync_dispatch, ListSubs, Sessions, ChannelId, GuildId, State, SyncFun},
            ok
        end
    ),
    meck:expect(
        guild_member_list_subscribe,
        dispatch_sync_to_subscribed_list,
        fun dispatch_sync_to_subscribed_list_mock/7
    ),
    try
        Fun()
    after
        meck:unload(guild_member_list_subscribe)
    end.

dispatch_sync_to_subscribed_list_mock(
    ListId, SubsTab, Sessions, ChannelId, GuildId, State, SyncFun
) ->
    ListSubs = guild_member_list_subs:get_list_subs(ListId, SubsTab),
    maybe_send_sync_dispatch(ListSubs, Sessions, ChannelId, GuildId, State, SyncFun),
    ok.

maybe_send_sync_dispatch(ListSubs, _Sessions, _ChannelId, _GuildId, _State, _SyncFun) when
    map_size(ListSubs) =:= 0
->
    ok;
maybe_send_sync_dispatch(ListSubs, Sessions, ChannelId, GuildId, State, SyncFun) ->
    self() ! {sync_dispatch, ListSubs, Sessions, ChannelId, GuildId, State, SyncFun},
    ok.

with_member_list_relay_mock(Fun) ->
    meck:new(gateway_dispatch_relay, [passthrough, no_link]),
    Parent = self(),
    meck:expect(
        gateway_dispatch_relay,
        dispatch_many,
        fun(Pids, Event, {pre_encoded, Bin}, GuildId) when is_binary(Bin) ->
            Parent ! {member_list_dispatch, Pids, Event, GuildId, json:decode(Bin)},
            ok
        end
    ),
    try
        Fun()
    after
        meck:unload(gateway_dispatch_relay)
    end.

assert_channel_sync_dispatch(
    {sync_dispatch, ListSubs, _Sessions, ChannelId, 100, _State, SyncFun}
) ->
    ListId = integer_to_binary(ChannelId),
    [Ranges] = maps:values(ListSubs),
    Payload = SyncFun(Ranges),
    ?assertEqual(ListId, maps:get(<<"id">>, Payload)),
    ?assertEqual(ListId, maps:get(<<"channel_id">>, Payload)),
    [Op] = maps:get(<<"ops">>, Payload),
    [Range] = Ranges,
    {Start, End} = Range,
    ?assertEqual([Start, End], maps:get(<<"range">>, Op)).

assert_stress_channel_sync_dispatch(
    {sync_dispatch, ListSubs, _Sessions, ChannelId, 100, _State, SyncFun}
) ->
    ?assertEqual(4, map_size(ListSubs)),
    Payload = SyncFun([{0, 20}, {40, 70}]),
    ListId = integer_to_binary(ChannelId),
    ?assertEqual(ListId, maps:get(<<"id">>, Payload)),
    ?assertEqual(ListId, maps:get(<<"channel_id">>, Payload)),
    ?assertEqual(2, length(maps:get(<<"ops">>, Payload))).

collect_sync_dispatches(Count) ->
    collect_sync_dispatches(Count, []).

collect_sync_dispatches(0, Acc) ->
    lists:reverse(Acc);
collect_sync_dispatches(Count, Acc) ->
    receive
        {sync_dispatch, _ListSubs, _Sessions, _ChannelId, _GuildId, _State, _SyncFun} = Msg ->
            collect_sync_dispatches(Count - 1, [Msg | Acc])
    after 1000 ->
        ?assert(false)
    end.

assert_no_sync_dispatch() ->
    receive
        {sync_dispatch, _ListSubs, _Sessions, _ChannelId, _GuildId, _State, _SyncFun} = Msg ->
            ?assert(false, {unexpected_sync_dispatch, Msg})
    after 0 ->
        ok
    end.

make_subs_tab() ->
    guild_member_list_subs:new().

make_subs_tab(Entries) ->
    Tab = guild_member_list_subs:new(),
    lists:foreach(
        fun({ListId, SessionId, Ranges}) ->
            guild_member_list_subs:subscribe(SessionId, ListId, Ranges, Tab)
        end,
        Entries
    ),
    Tab.

base_state(SubsTab) ->
    #{
        id => 100,
        data => #{<<"channels">> => [#{<<"id">> => <<"500">>}], <<"members">> => []},
        sessions => #{},
        member_presence => #{},
        member_list_subscriptions => SubsTab
    }.
