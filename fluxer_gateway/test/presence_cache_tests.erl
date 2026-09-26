%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_cache_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_PENDING_CAP, 10000).

put_and_get_visible_status_test() ->
    {ok, Pid} = maybe_start_for_test(),
    Presence = #{<<"status">> => <<"online">>},
    ?assertEqual(ok, presence_cache:put(1, Presence)),
    _ = sys:get_state(Pid),
    ?assertMatch({ok, _}, presence_cache:get(1)),
    ?assertEqual(ok, gen_server:stop(Pid)).

put_offline_evicted_test() ->
    {ok, Pid} = maybe_start_for_test(),
    Presence = #{<<"status">> => <<"offline">>},
    ?assertEqual(ok, presence_cache:put(2, Presence)),
    _ = sys:get_state(Pid),
    ?assertEqual(not_found, presence_cache:get(2)),
    ?assertEqual(ok, gen_server:stop(Pid)).

bulk_get_across_shards_test() ->
    {ok, Pid} = maybe_start_for_test(),
    Visible = #{<<"status">> => <<"online">>, <<"user">> => #{<<"id">> => <<"3">>}},
    presence_cache:put(3, Visible),
    presence_cache:put(4, Visible),
    _ = sys:get_state(Pid),
    Results = presence_cache:bulk_get([3, 4, 3]),
    ?assertEqual(2, length(Results)),
    ?assertEqual(ok, gen_server:stop(Pid)).

select_shard_test() ->
    ?assert(presence_cache_bulk:select_shard(100, 4) >= 0),
    ?assert(presence_cache_bulk:select_shard(100, 4) < 4).

find_shard_by_ref_test() ->
    Ref1 = make_ref(),
    Shards = #{0 => #{pid => self(), ref => Ref1}},
    ?assertEqual({ok, 0}, presence_cache_shards:find_by_ref(Ref1, Shards)),
    ?assertEqual(not_found, presence_cache_shards:find_by_ref(make_ref(), Shards)).

put_pending_operation_overwrites_test() ->
    Pending0 = #{100 => delete},
    Pending1 = Pending0#{100 => {put, #{<<"status">> => <<"online">>}}},
    ?assertEqual({put, #{<<"status">> => <<"online">>}}, maps:get(100, Pending1)).

merge_rebalance_operations_prefers_pending_test() ->
    Snapshot = #{42 => #{<<"status">> => <<"online">>}},
    SnapshotOps = #{42 => {put, #{<<"status">> => <<"online">>}}},
    Pending = #{42 => delete, 43 => {put, #{<<"status">> => <<"idle">>}}},
    Operations = maps:merge(
        SnapshotOps, presence_cache_rebalance:sanitize_pending_operations(Pending)
    ),
    ?assertEqual(delete, maps:get(42, Operations)),
    ?assertEqual({put, #{<<"status">> => <<"idle">>}}, maps:get(43, Operations)),
    _ = Snapshot.

sanitize_pending_operations_filters_invalid_entries_test() ->
    Pending = #{
        1 => delete,
        2 => {put, #{}},
        -3 => delete,
        4 => {put, not_a_map},
        bad_key => delete
    },
    ?assertEqual(
        #{1 => delete, 2 => {put, #{}}},
        presence_cache_rebalance:sanitize_pending_operations(Pending)
    ).

nodedown_grace_period_preserves_entries_test() ->
    {ok, Pid} = maybe_start_for_test(),
    Presence = #{<<"status">> => <<"online">>},
    ?assertEqual(ok, presence_cache:put(100, Presence)),
    _ = sys:get_state(Pid),
    gen_server:cast(presence_cache, {nodedown_grace, 'lost@node'}),
    timer:sleep(50),
    ?assertMatch({ok, _}, presence_cache:get(100)),
    ?assertEqual(ok, gen_server:stop(Pid)).

nodeup_cancels_grace_period_test() ->
    {ok, Pid} = maybe_start_for_test(),
    gen_server:cast(presence_cache, {nodedown_grace, 'lost@node'}),
    timer:sleep(50),
    gen_server:cast(presence_cache, {nodeup_cancel_grace, 'lost@node'}),
    timer:sleep(50),
    ?assertEqual(ok, gen_server:stop(Pid)).

anti_entropy_no_op_when_in_sync_test() ->
    {ok, Pid} = maybe_start_for_test(),
    State = cache_state(sys:get_state(Pid)),
    Gen = maps:get(generation, State, 0),
    {noreply, State1} = presence_cache_rebalance:handle_anti_entropy_request(
        node(), Gen, State
    ),
    ?assertEqual(Gen, maps:get(generation, State1, 0)),
    ?assertEqual(ok, gen_server:stop(Pid)).

generation_increments_on_write_test() ->
    {ok, Pid} = maybe_start_for_test(),
    Gen0 = presence_cache:generation(),
    ?assertEqual(ok, presence_cache:put(200, #{<<"status">> => <<"online">>})),
    _ = sys:get_state(Pid),
    Gen1 = presence_cache:generation(),
    ?assert(Gen1 > Gen0),
    ?assertEqual(ok, gen_server:stop(Pid)).

cap_pending_operations_enforces_limit_test() ->
    Large = maps:from_list([{I, delete} || I <- lists:seq(1, 10500)]),
    BaseState = #{pending_operations => Large, pending_retry_timer => undefined},
    State1 = presence_cache_rebalance:ensure_pending_state(BaseState),
    ?assertEqual(10000, maps:size(maps:get(pending_operations, State1))).

cap_pending_operations_no_op_under_limit_test() ->
    Small = #{1 => delete, 2 => {put, #{}}},
    BaseState = #{pending_operations => Small, pending_retry_timer => undefined},
    State1 = presence_cache_rebalance:ensure_pending_state(BaseState),
    ?assertEqual(Small, maps:get(pending_operations, State1)).

put_to_unreachable_remote_owner_queues_pending_operation_test() ->
    RemoteNode = 'missing_presence@127.0.0.1',
    Presence = #{<<"status">> => <<"online">>},
    with_presence_members(RemoteNode, fun() ->
        UserId = remote_owned_user_id(RemoteNode),
        State1 = presence_cache_ops:handle_put(UserId, Presence, base_pending_state()),
        try
            ?assertEqual(
                {put, Presence},
                maps:get(UserId, maps:get(pending_operations, State1))
            )
        after
            presence_cache_rebalance:cancel_pending_retry_timer(State1)
        end
    end).

rebalance_keeps_pending_put_when_remote_owner_unreachable_test() ->
    RemoteNode = 'missing_presence_rebalance@127.0.0.1',
    with_presence_members(RemoteNode, fun() ->
        with_presence_cache(fun(Pid) -> assert_rebalance_keeps_pending_put(RemoteNode, Pid) end)
    end).

handoff_to_unreachable_target_keeps_local_entry_test() ->
    RemoteNode = 'missing_presence_handoff@127.0.0.1',
    UserId = 33001,
    Presence = #{
        <<"status">> => <<"online">>,
        <<"user">> => #{<<"id">> => integer_to_binary(UserId)}
    },
    with_presence_cache(fun(Pid) ->
        State0 = cache_state(sys:get_state(Pid)),
        {_Reply, State1} = presence_cache:put_local(UserId, Presence, State0),
        State2 = presence_cache_rebalance:handoff_all_to_target(RemoteNode, State1),
        ?assertMatch({{ok, Presence}, _}, presence_cache_ops:get_local(UserId, State2))
    end).

content_digest_changes_on_write_test() ->
    with_presence_cache(fun(Pid) ->
        State0 = cache_state(sys:get_state(Pid)),
        Digest0 = presence_cache_shards:content_digest(State0),
        Presence = #{
            <<"status">> => <<"online">>,
            <<"user">> => #{<<"id">> => <<"54001">>}
        },
        {_Reply, State1} = presence_cache:put_local(54001, Presence, State0),
        Digest1 = presence_cache_shards:content_digest(State1),
        ?assert(is_binary(Digest0)),
        ?assertNotEqual(Digest0, Digest1)
    end).

anti_entropy_digest_request_noop_when_digests_match_test() ->
    with_presence_cache(fun(Pid) ->
        State = cache_state(sys:get_state(Pid)),
        Digest = presence_cache_shards:content_digest(State),
        {noreply, State1} = presence_cache_rebalance:handle_anti_entropy_digest_request(
            node(), Digest, State
        ),
        ?assertEqual(State, State1)
    end).

rebalance_keeps_local_entry_when_remote_delete_unreachable_test() ->
    RemoteNode = 'missing_presence_delete@127.0.0.1',
    with_presence_members(RemoteNode, fun() ->
        with_presence_cache(fun(Pid) ->
            assert_rebalance_keeps_local_on_failed_delete(RemoteNode, Pid)
        end)
    end).

assert_rebalance_keeps_local_on_failed_delete(RemoteNode, Pid) ->
    UserId = remote_owned_user_id(RemoteNode),
    Presence = #{
        <<"status">> => <<"online">>,
        <<"user">> => #{<<"id">> => integer_to_binary(UserId)}
    },
    State0 = cache_state(sys:get_state(Pid)),
    {_Reply, State1} = presence_cache:put_local(UserId, Presence, State0),
    State2 = presence_cache_rebalance:queue_pending_operation(UserId, delete, State1),
    State3 = presence_cache_rebalance:rebalance_ownership(State2),
    try
        ?assertEqual(delete, maps:get(UserId, maps:get(pending_operations, State3))),
        ?assertMatch({{ok, Presence}, _}, presence_cache_ops:get_local(UserId, State3))
    after
        presence_cache_rebalance:cancel_pending_retry_timer(State3)
    end.

evict_oldest_pending_drops_oldest_inserted_test() ->
    State0 = #{
        pending_operations => #{},
        pending_seq => #{},
        pending_seq_counter => 0,
        pending_retry_timer => undefined
    },
    State1 = lists:foldl(
        fun(UserId, AccState) ->
            presence_cache_rebalance:queue_pending_operation(UserId, delete, AccState)
        end,
        State0,
        lists:seq(1, ?TEST_PENDING_CAP + 5)
    ),
    try
        Pending = maps:get(pending_operations, State1),
        ?assertEqual(?TEST_PENDING_CAP, maps:size(Pending)),
        ?assertNot(maps:is_key(1, Pending)),
        ?assertNot(maps:is_key(5, Pending)),
        ?assert(maps:is_key(?TEST_PENDING_CAP + 5, Pending))
    after
        presence_cache_rebalance:cancel_pending_retry_timer(State1)
    end.

anti_entropy_does_not_resurrect_deleted_presence_test() ->
    with_local_presence_cache(fun(Pid) -> assert_delete_survives_stale_peer_entry(Pid) end).

anti_entropy_repairs_missing_presence_test() ->
    with_local_presence_cache(fun(Pid) -> assert_merge_repairs_missing_entry(Pid) end).

anti_entropy_merges_once_tombstone_expires_test() ->
    with_local_presence_cache(fun(Pid) -> assert_merge_resumes_after_expiry(Pid) end).

anti_entropy_tombstone_cleared_when_user_returns_test() ->
    with_local_presence_cache(fun(Pid) -> assert_tombstone_cleared_on_put(Pid) end).

assert_delete_survives_stale_peer_entry(Pid) ->
    UserId = 77001,
    Presence = anti_entropy_presence(UserId),
    State0 = cache_state(sys:get_state(Pid)),
    ?assertEqual([node()], presence_cache_bulk:resolve_owner_nodes(UserId)),
    {_PutReply, State1} = presence_cache:put_local(UserId, Presence, State0),
    {_DeleteReply, State2} = presence_cache:delete_local(UserId, State1),
    ?assertMatch({not_found, _}, presence_cache_ops:get_local(UserId, State2)),
    State3 = presence_cache_rebalance:merge_anti_entropy_entries(#{UserId => Presence}, State2),
    ?assertMatch({not_found, _}, presence_cache_ops:get_local(UserId, State3)).

assert_merge_repairs_missing_entry(Pid) ->
    UserId = 77002,
    Presence = anti_entropy_presence(UserId),
    State0 = cache_state(sys:get_state(Pid)),
    State1 = presence_cache_rebalance:merge_anti_entropy_entries(#{UserId => Presence}, State0),
    ?assertMatch({{ok, Presence}, _}, presence_cache_ops:get_local(UserId, State1)).

assert_merge_resumes_after_expiry(Pid) ->
    UserId = 77003,
    Presence = anti_entropy_presence(UserId),
    State0 = cache_state(sys:get_state(Pid)),
    {_PutReply, State1} = presence_cache:put_local(UserId, Presence, State0),
    {_DeleteReply, State2} = presence_cache:delete_local(UserId, State1),
    Expired = State2#{
        delete_tombstones => #{UserId => erlang:monotonic_time(millisecond) - 1}
    },
    State3 = presence_cache_rebalance:merge_anti_entropy_entries(
        #{UserId => Presence}, Expired
    ),
    ?assertMatch({{ok, Presence}, _}, presence_cache_ops:get_local(UserId, State3)),
    ?assertEqual(#{}, maps:get(delete_tombstones, State3)).

assert_tombstone_cleared_on_put(Pid) ->
    UserId = 77004,
    Presence = anti_entropy_presence(UserId),
    State0 = cache_state(sys:get_state(Pid)),
    {_FirstPut, State1} = presence_cache:put_local(UserId, Presence, State0),
    {_DeleteReply, State2} = presence_cache:delete_local(UserId, State1),
    ?assert(maps:is_key(UserId, maps:get(delete_tombstones, State2))),
    {_SecondPut, State3} = presence_cache:put_local(UserId, Presence, State2),
    ?assertNot(maps:is_key(UserId, maps:get(delete_tombstones, State3))).

rebalance_drop_does_not_suppress_repair_test() ->
    RemoteNode = 'moved_presence_rebalance@127.0.0.1',
    with_presence_cache(fun(Pid) -> assert_rebalance_drop_repairs(RemoteNode, Pid) end).

handoff_drop_does_not_suppress_repair_test() ->
    with_local_presence_cache(fun(Pid) -> assert_handoff_drop_repairs(Pid) end).

rebalance_delete_still_tombstones_test() ->
    RemoteNode = 'deleted_presence_rebalance@127.0.0.1',
    with_presence_cache(fun(Pid) -> assert_rebalance_delete_tombstones(RemoteNode, Pid) end).

handle_delete_still_tombstones_test() ->
    with_local_presence_cache(fun(Pid) -> assert_handle_delete_tombstones(Pid) end).

assert_rebalance_drop_repairs(RemoteNode, Pid) ->
    State0 = cache_state(sys:get_state(Pid)),
    {UserId, Presence, State2} = with_presence_member_nodes([node(), RemoteNode], fun() ->
        MovedUserId = remote_owned_user_id(RemoteNode),
        MovedPresence = anti_entropy_presence(MovedUserId),
        {_PutReply, State1} = presence_cache:put_local(MovedUserId, MovedPresence, State0),
        Dropped = with_reachable_remote(fun() ->
            presence_cache_rebalance:rebalance_ownership(State1)
        end),
        {MovedUserId, MovedPresence, Dropped}
    end),
    ?assertMatch({not_found, _}, presence_cache_ops:get_local(UserId, State2)),
    with_presence_member_nodes([node()], fun() ->
        State3 = presence_cache_rebalance:merge_anti_entropy_entries(
            #{UserId => Presence}, State2
        ),
        ?assertMatch({{ok, Presence}, _}, presence_cache_ops:get_local(UserId, State3))
    end),
    ?assertNot(maps:is_key(UserId, maps:get(delete_tombstones, State2))).

assert_handoff_drop_repairs(Pid) ->
    UserId = 77005,
    Presence = anti_entropy_presence(UserId),
    State0 = cache_state(sys:get_state(Pid)),
    {_PutReply, State1} = presence_cache:put_local(UserId, Presence, State0),
    State2 = with_reachable_remote(fun() ->
        presence_cache_rebalance:handoff_all_to_target('handoff_target@127.0.0.1', State1)
    end),
    ?assertMatch({not_found, _}, presence_cache_ops:get_local(UserId, State2)),
    State3 = presence_cache_rebalance:merge_anti_entropy_entries(#{UserId => Presence}, State2),
    ?assertMatch({{ok, Presence}, _}, presence_cache_ops:get_local(UserId, State3)),
    ?assertNot(maps:is_key(UserId, maps:get(delete_tombstones, State2))).

assert_rebalance_delete_tombstones(RemoteNode, Pid) ->
    State0 = cache_state(sys:get_state(Pid)),
    {UserId, Presence, State3} = with_presence_member_nodes([node(), RemoteNode], fun() ->
        GoneUserId = remote_owned_user_id(RemoteNode),
        GonePresence = anti_entropy_presence(GoneUserId),
        {_PutReply, State1} = presence_cache:put_local(GoneUserId, GonePresence, State0),
        State2 = presence_cache_rebalance:queue_pending_operation(GoneUserId, delete, State1),
        Deleted = with_reachable_remote(fun() ->
            presence_cache_rebalance:rebalance_ownership(State2)
        end),
        {GoneUserId, GonePresence, Deleted}
    end),
    presence_cache_rebalance:cancel_pending_retry_timer(State3),
    ?assert(maps:is_key(UserId, maps:get(delete_tombstones, State3))),
    with_presence_member_nodes([node()], fun() ->
        State4 = presence_cache_rebalance:merge_anti_entropy_entries(
            #{UserId => Presence}, State3
        ),
        ?assertMatch({not_found, _}, presence_cache_ops:get_local(UserId, State4))
    end).

assert_handle_delete_tombstones(Pid) ->
    UserId = 77006,
    Presence = anti_entropy_presence(UserId),
    State0 = cache_state(sys:get_state(Pid)),
    {_PutReply, State1} = presence_cache:put_local(UserId, Presence, State0),
    State2 = presence_cache_ops:handle_delete(UserId, State1),
    ?assert(maps:is_key(UserId, maps:get(delete_tombstones, State2))),
    State3 = presence_cache_rebalance:merge_anti_entropy_entries(#{UserId => Presence}, State2),
    ?assertMatch({not_found, _}, presence_cache_ops:get_local(UserId, State3)).

with_reachable_remote(Fun) ->
    meck:new(presence_cache_bulk, [passthrough, no_link]),
    meck:expect(presence_cache_bulk, safe_remote_call, fun(_Node, _Request, _Fallback) -> ok end),
    try
        Fun()
    after
        meck:unload(presence_cache_bulk)
    end.

anti_entropy_presence(UserId) ->
    #{
        <<"status">> => <<"online">>,
        <<"user">> => #{<<"id">> => integer_to_binary(UserId)}
    }.

with_local_presence_cache(Fun) ->
    with_presence_member_nodes([node()], fun() -> with_presence_cache(Fun) end).

maybe_start_for_test() ->
    case whereis(presence_cache) of
        undefined -> presence_cache:start_link();
        Existing when is_pid(Existing) -> {ok, Existing}
    end.

base_pending_state() ->
    #{pending_operations => #{}, pending_retry_timer => undefined}.

with_presence_cache(Fun) ->
    {ok, Pid} = maybe_start_for_test(),
    try
        Fun(Pid)
    after
        try
            gen_server:stop(Pid)
        catch
            error:_ -> ok;
            exit:_ -> ok
        end
    end.

with_presence_members(RemoteNode, Fun) ->
    with_presence_member_nodes([node(), RemoteNode], Fun).

with_presence_member_nodes(Nodes, Fun) ->
    MembersKey = {gateway_cluster_membership, members},
    RoleMembersKey = {gateway_cluster_membership, members_by_role},
    OldMembers = persistent_term:get(MembersKey, undefined),
    OldRoleMembers = persistent_term:get(RoleMembersKey, undefined),
    persistent_term:put(MembersKey, Nodes),
    persistent_term:put(RoleMembersKey, #{presence => Nodes}),
    try
        Fun()
    after
        restore_persistent_term(MembersKey, OldMembers),
        restore_persistent_term(RoleMembersKey, OldRoleMembers)
    end.

restore_persistent_term(Key, undefined) ->
    persistent_term:erase(Key);
restore_persistent_term(Key, Value) ->
    persistent_term:put(Key, Value).

remote_owned_user_id(RemoteNode) ->
    remote_owned_user_id(RemoteNode, 1).

remote_owned_user_id(RemoteNode, UserId) when UserId =< 10000 ->
    case presence_cache_bulk:resolve_owner_nodes(UserId) of
        [RemoteNode] -> UserId;
        _ -> remote_owned_user_id(RemoteNode, UserId + 1)
    end;
remote_owned_user_id(RemoteNode, _UserId) ->
    error({remote_owner_not_found, RemoteNode}).

assert_rebalance_keeps_pending_put(RemoteNode, Pid) ->
    UserId = remote_owned_user_id(RemoteNode),
    Presence = #{
        <<"status">> => <<"online">>,
        <<"user">> => #{<<"id">> => integer_to_binary(UserId)}
    },
    State0 = cache_state(sys:get_state(Pid)),
    {_Reply, State1} = presence_cache:put_local(UserId, Presence, State0),
    State2 = presence_cache_rebalance:rebalance_ownership(State1),
    try
        ?assertEqual(
            {put, Presence},
            maps:get(UserId, maps:get(pending_operations, State2))
        ),
        ?assertMatch({{ok, Presence}, _}, presence_cache_ops:get_local(UserId, State2))
    after
        presence_cache_rebalance:cancel_pending_retry_timer(State2)
    end.

cache_state(State) when is_map(State) ->
    State.
