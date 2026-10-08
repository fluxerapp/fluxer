%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_counts_cache_tests).
-typing([eqwalizer]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

update_get_delete_local_test() ->
    {Pid, Started} = start_for_test(),
    ?assertEqual(ok, guild_counts_cache:update(1001, 50, 25)),
    ?assertEqual({ok, 50, 25}, guild_counts_cache:get(1001)),
    ?assertEqual(ok, guild_counts_cache:delete(1001)),
    ?assertEqual(miss, guild_counts_cache:get(1001)),
    stop_for_test(Pid, Started).

bulk_get_returns_map_test() ->
    {Pid, Started} = start_for_test(),
    ?assertEqual(ok, guild_counts_cache:update(2001, 10, 3)),
    ?assertEqual(ok, guild_counts_cache:update(2002, 20, 6)),
    Reply = guild_counts_cache:bulk_get([2001, 2002, 2001, 9999]),
    ?assertEqual(#{2001 => {10, 3}, 2002 => {20, 6}}, Reply),
    ?assertEqual(ok, guild_counts_cache:delete(2001)),
    ?assertEqual(ok, guild_counts_cache:delete(2002)),
    stop_for_test(Pid, Started).

group_guild_ids_by_owner_groups_and_deduplicates_test() ->
    LocalNode = node(),
    RemoteNode = 'gateway_c@127.0.0.1',
    Groups = clustered_ets_cache:group_keys_by_owner(
        [1, 2, 3, 2],
        fun
            (GuildId) when GuildId rem 2 =:= 0 -> RemoteNode;
            (_GuildId) -> LocalNode
        end
    ),
    ?assertEqual(
        lists:sort([{LocalNode, [1, 3]}, {RemoteNode, [2]}]),
        lists:sort(Groups)
    ).

determine_shard_count_prefers_cache_specific_value_test() ->
    ?assertEqual(
        {6, guild_counts_cache_shards},
        clustered_ets_cache:determine_shard_count(
            [guild_counts_cache_shards, guild_shards],
            fun
                (guild_counts_cache_shards) -> 6;
                (guild_shards) -> 3;
                (_) -> undefined
            end
        )
    ).

determine_shard_count_falls_back_to_guild_shards_test() ->
    ?assertEqual(
        {4, guild_shards},
        clustered_ets_cache:determine_shard_count(
            [guild_counts_cache_shards, guild_shards],
            fun
                (guild_counts_cache_shards) -> undefined;
                (guild_shards) -> 4;
                (_) -> undefined
            end
        )
    ).

determine_shard_count_defaults_when_missing_test() ->
    {Count, Source} = clustered_ets_cache:determine_shard_count(
        [guild_counts_cache_shards, guild_shards],
        fun(_) -> undefined end
    ),
    ?assert(is_integer(Count)),
    ?assert(Count >= 1),
    ?assertEqual(auto, Source).

pending_handoff_count_public_api_test() ->
    {Pid, Started} = start_for_test(),
    ?assertEqual(0, guild_counts_cache:pending_handoff_count()),
    _ = sys:replace_state(Pid, fun(St) ->
        guild_counts_cache_remote:enqueue_pending_upsert(3001, 7, 3, eqwalizer:dynamic_cast(St))
    end),
    ?assertEqual(1, guild_counts_cache:pending_handoff_count()),
    ?assertEqual(ok, guild_counts_cache:rebalance()),
    ?assertEqual(0, guild_counts_cache:pending_handoff_count()),
    stop_for_test(Pid, Started).

merge_pending_handoffs_with_snapshot_uses_latest_counts_test() ->
    RemoteNode = 'gateway_d@127.0.0.1',
    Pending = #{10 => delete, 11 => {upsert, {1, 1}}},
    Snapshot = #{10 => {100, 20}, 12 => {4, 2}},
    ResolverFun = fun
        (12) -> node();
        (_GuildId) -> RemoteNode
    end,
    NormalizedPending = guild_counts_cache_remote:sanitize_pending_handoffs(Pending),
    Merged = maps:fold(
        fun
            (GuildId, {MC, OC}, AccPending) when
                is_integer(GuildId),
                GuildId > 0,
                is_integer(MC),
                MC >= 0,
                is_integer(OC),
                OC >= 0
            ->
                case guild_counts_cache_query:resolve_owner_node_safe(GuildId, ResolverFun) of
                    LocalNode when LocalNode =:= node() ->
                        maps:remove(GuildId, AccPending);
                    _OwnerNode ->
                        AccPending#{GuildId => {upsert, {MC, OC}}}
                end;
            (_GuildId, _Counts, AccPending) ->
                AccPending
        end,
        NormalizedPending,
        Snapshot
    ),
    ?assertEqual({upsert, {100, 20}}, maps:get(10, Merged)),
    ?assertEqual({upsert, {1, 1}}, maps:get(11, Merged)),
    ?assertNot(maps:is_key(12, Merged)).

process_pending_handoffs_retries_failed_delete_test() ->
    State = base_test_state(),
    RemoteNode = 'gateway_e@127.0.0.1',
    Pending = #{1 => delete, 2 => delete},
    ResolverFun = fun(_GuildId) -> RemoteNode end,
    RemoteCallFun = fun
        (_Node, {delete_local, 1}, _Fallback) -> ok;
        (_Node, {delete_local, 2}, _Fallback) -> {error, unavailable}
    end,
    {PendingRemaining, _State1} = test_process_pending_handoffs(
        Pending, State, ResolverFun, RemoteCallFun
    ),
    ?assertEqual(#{2 => delete}, PendingRemaining).

remote_handoff_success_clears_matching_pending_operation_test() ->
    State0 = base_test_state(),
    State1 = guild_counts_cache_remote:enqueue_pending_upsert(42, 7, 3, State0),
    State2 = handle_completed_upsert_handoff(State1),
    ?assertEqual(#{}, guild_counts_cache_remote:pending_handoffs(State2)).

remote_handoff_success_preserves_newer_pending_operation_test() ->
    State0 = base_test_state(),
    State1 = guild_counts_cache_remote:enqueue_pending_delete(42, State0),
    State2 = handle_completed_upsert_handoff(State1),
    ?assertEqual(#{42 => delete}, guild_counts_cache_remote:pending_handoffs(State2)),
    _ = guild_counts_cache_shard_mgmt:cancel_rebalance_retry_timer(State2),
    ok.

remote_handoff_failure_keeps_pending_operation_test() ->
    State0 = base_test_state(),
    State1 = guild_counts_cache_remote:enqueue_pending_upsert(42, 7, 3, State0),
    State2 = guild_counts_cache_remote:handle_remote_handoff_result(
        42, {upsert, {7, 3}}, keep_local, {error, timeout}, State1
    ),
    ?assertEqual(
        #{42 => {upsert, {7, 3}}},
        guild_counts_cache_remote:pending_handoffs(State2)
    ),
    _ = guild_counts_cache_shard_mgmt:cancel_rebalance_retry_timer(State2),
    ok.

start_pending_handoff_attempts_drops_local_owner_test() ->
    State0 = base_test_state(),
    Pending = #{42 => {upsert, {7, 3}}, 43 => delete},
    ResolverFun = fun(_GuildId) -> node() end,
    Sanitized = guild_counts_cache_remote:sanitize_pending_handoffs(Pending),
    {PendingRemaining, State1} = maps:fold(
        fun(GuildId, Operation, {AccPending, AccState}) ->
            case guild_counts_cache_query:resolve_owner_node_safe(GuildId, ResolverFun) of
                LocalNode when LocalNode =:= node() ->
                    {AccPending, AccState};
                _OwnerNode ->
                    {AccPending#{GuildId => Operation}, AccState}
            end
        end,
        {#{}, State0},
        Sanitized
    ),
    ?assertEqual(#{}, PendingRemaining),
    ?assertEqual(State0, State1).

pending_operation_success_action_test() ->
    ?assertEqual(
        delete_local_if_remote_owner,
        guild_counts_cache_remote:pending_operation_success_action({upsert, {7, 3}})
    ),
    ?assertEqual(
        keep_local, guild_counts_cache_remote:pending_operation_success_action(delete)
    ).

split_local_remote_groups_keeps_owner_groups_order_test() ->
    RemoteNode = 'gateway_f@127.0.0.1',
    Groups = [{RemoteNode, [2]}, {node(), [1, 3]}, {RemoteNode, [4]}],
    ?assertEqual(
        {[{node(), [1, 3]}], [{RemoteNode, [2]}, {RemoteNode, [4]}]},
        guild_counts_cache_query:split_local_remote_groups(Groups)
    ).

refresh_rebalance_retry_timer_schedules_and_cancels_test() ->
    State0 = base_test_state(),
    State1 = guild_counts_cache_remote:set_pending_handoffs(#{9 => delete}, State0),
    State2 = guild_counts_cache_shard_mgmt:refresh_rebalance_retry_timer(State1),
    TimerRef = guild_counts_cache_shard_mgmt:rebalance_retry_timer_ref(State2),
    ?assert(is_reference(TimerRef)),
    State3 = guild_counts_cache_shard_mgmt:refresh_rebalance_retry_timer(
        guild_counts_cache_remote:set_pending_handoffs(#{}, State2)
    ),
    ?assertEqual(undefined, guild_counts_cache_shard_mgmt:rebalance_retry_timer_ref(State3)),
    receive
        {timeout, TimerRef, rebalance_retry} -> ok
    after 0 -> ok
    end.

cap_pending_handoffs_enforces_limit_test() ->
    Large = maps:from_list([{I, delete} || I <- lists:seq(1, 10500)]),
    Capped = guild_counts_cache_remote:cap_pending_handoffs(Large),
    ?assertEqual(10000, maps:size(Capped)).

cap_pending_handoffs_no_op_under_limit_test() ->
    Small = #{1 => delete, 2 => {upsert, {10, 5}}},
    ?assertEqual(Small, guild_counts_cache_remote:cap_pending_handoffs(Small)).

base_test_state() ->
    #{
        shards => #{},
        shard_count => 1,
        pending_handoffs => #{},
        rebalance_retry_timer => undefined
    }.

start_for_test() ->
    case whereis(guild_counts_cache) of
        undefined ->
            {ok, Pid} = guild_counts_cache:start_link(),
            {Pid, true};
        ExistingPid when is_pid(ExistingPid) ->
            {ExistingPid, false}
    end.

stop_for_test(Pid, true) ->
    ?assertEqual(ok, gen_server:stop(Pid));
stop_for_test(_Pid, false) ->
    ok.

test_process_pending_handoffs(Pending, State, ResolveOwnerNodeFun, RemoteCallFun) ->
    maps:fold(
        fun(GuildId, Operation, {AccPending, AccState}) ->
            case
                test_attempt_handoff(
                    GuildId, Operation, AccState, ResolveOwnerNodeFun, RemoteCallFun
                )
            of
                {success, NextState} -> {AccPending, NextState};
                {retry, NextState} -> {AccPending#{GuildId => Operation}, NextState}
            end
        end,
        {#{}, State},
        guild_counts_cache_remote:sanitize_pending_handoffs(Pending)
    ).

test_attempt_handoff(GuildId, Operation, State, ResolveOwnerNodeFun, RemoteCallFun) ->
    case guild_counts_cache_query:resolve_owner_node_safe(GuildId, ResolveOwnerNodeFun) of
        LocalNode when LocalNode =:= node() ->
            {success, State};
        OwnerNode ->
            test_attempt_remote(GuildId, Operation, OwnerNode, State, RemoteCallFun)
    end.

test_attempt_remote(GuildId, {upsert, {MC, OC}}, OwnerNode, State, RemoteCallFun) ->
    case RemoteCallFun(OwnerNode, {update_local, GuildId, MC, OC}, {error, unavailable}) of
        ok ->
            {_Reply, NextState} = guild_counts_cache_query:delete_local(GuildId, State),
            {success, NextState};
        _ ->
            {retry, State}
    end;
test_attempt_remote(GuildId, delete, OwnerNode, State, RemoteCallFun) ->
    case RemoteCallFun(OwnerNode, {delete_local, GuildId}, {error, unavailable}) of
        ok -> {success, State};
        _ -> {retry, State}
    end.

handle_completed_upsert_handoff(State) ->
    guild_counts_cache_remote:handle_remote_handoff_result(
        42, {upsert, {7, 3}}, keep_local, ok, State
    ).

-endif.
