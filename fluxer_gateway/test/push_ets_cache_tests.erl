%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_ets_cache_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

init_creates_tables_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    ?assertNotEqual(undefined, ets:whereis(push_user_guild_settings)),
    ?assertNotEqual(undefined, ets:whereis(push_subscriptions)),
    ?assertNotEqual(undefined, ets:whereis(push_blocked_ids)),
    ?assertNotEqual(undefined, ets:whereis(push_badge_counts)),
    ?assertNotEqual(undefined, ets:whereis(push_endpoint_verdicts)),
    cleanup_tables().

init_idempotent_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    ok = push_ets_cache:init(),
    ?assertNotEqual(undefined, ets:whereis(push_user_guild_settings)),
    cleanup_tables().

user_guild_settings_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    ?assertEqual(undefined, push_ets_cache:get_user_guild_settings(1, 2)),
    ok = push_ets_cache:put_user_guild_settings(1, 2, #{muted => true}),
    ?assertEqual(#{muted => true}, push_ets_cache:get_user_guild_settings(1, 2)),
    ok = push_ets_cache:delete_user_guild_settings(1, 2),
    ?assertEqual(undefined, push_ets_cache:get_user_guild_settings(1, 2)),
    cleanup_tables().

subscriptions_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    ?assertEqual(undefined, push_ets_cache:get_subscriptions(1)),
    ok = seed_subscriptions(1, [sub1, sub2]),
    ?assertEqual([sub1, sub2], push_ets_cache:get_subscriptions(1)),
    ok = push_ets_cache:delete_subscriptions(1),
    ?assertEqual(undefined, push_ets_cache:get_subscriptions(1)),
    cleanup_tables().

blocked_ids_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    ?assertEqual(undefined, push_ets_cache:get_blocked_ids(1)),
    ok = push_ets_cache:put_blocked_ids(1, [2, 3]),
    ?assertEqual([2, 3], push_ets_cache:get_blocked_ids(1)),
    cleanup_tables().

badge_count_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    ?assertEqual(undefined, push_ets_cache:get_badge_count(1)),
    ok = seed_badge_count(1, 5, 1000),
    ?assertEqual({5, 1000}, push_ets_cache:get_badge_count(1)),
    ok = push_ets_cache:delete_badge_count(1),
    ?assertEqual(undefined, push_ets_cache:get_badge_count(1)),
    cleanup_tables().

badge_count_keeps_fresher_timestamp_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    First = push_ets_cache:reserve_badge_counts([1]),
    ok = push_ets_cache:put_badge_count(1, 5, 2000, First),
    ok = push_ets_cache:put_badge_count(1, 9, 1000, First),
    ?assertEqual({5, 2000}, push_ets_cache:get_badge_count(1)),
    ok = seed_badge_count(1, 7, 3000),
    ?assertEqual({7, 3000}, push_ets_cache:get_badge_count(1)),
    ok = seed_badge_count(1, 8, 3000),
    ?assertEqual({8, 3000}, push_ets_cache:get_badge_count(1)),
    cleanup_tables().

cache_stats_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    ok = seed_subscriptions(1, []),
    ok = seed_subscriptions(2, []),
    Stats = push_ets_cache:cache_stats(),
    ?assertEqual(2, maps:get(push_subscriptions_size, Stats)),
    ?assertEqual(0, maps:get(user_guild_settings_size, Stats)),
    cleanup_tables().

evict_tables_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    lists:foreach(fun(I) -> ok = seed_subscriptions(I, []) end, lists:seq(1, 10)),
    ?assertEqual(10, push_ets_cache:table_size(push_subscriptions)),
    ok = push_ets_cache:evict_tables(#{subscriptions => 5}),
    ?assertEqual(5, push_ets_cache:table_size(push_subscriptions)),
    cleanup_tables().

rebalance_evicts_remote_owned_entries_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    RemoteNode = 'push_cache_remote@127.0.0.1',
    Members = lists:usort([node(), RemoteNode]),
    {LocalUserId, RemoteUserId} = find_split_user_ids(Members, RemoteNode),
    RoleMap = #{push => Members, all => Members},
    persistent_term:put({gateway_cluster_membership, members}, Members),
    persistent_term:put({gateway_cluster_membership, members_by_role}, RoleMap),
    ok = seed_subscriptions(LocalUserId, [local]),
    ok = seed_subscriptions(RemoteUserId, [remote]),
    ok = push_ets_cache:put_user_guild_settings(LocalUserId, 10, #{local => true}),
    ok = push_ets_cache:put_user_guild_settings(RemoteUserId, 10, #{remote => true}),
    ok = push_ets_cache:rebalance(),
    ?assertEqual([local], push_ets_cache:get_subscriptions(LocalUserId)),
    ?assertEqual(undefined, push_ets_cache:get_subscriptions(RemoteUserId)),
    ?assertEqual(#{local => true}, push_ets_cache:get_user_guild_settings(LocalUserId, 10)),
    ?assertEqual(undefined, push_ets_cache:get_user_guild_settings(RemoteUserId, 10)),
    persistent_term:erase({gateway_cluster_membership, members}),
    persistent_term:erase({gateway_cluster_membership, members_by_role}),
    cleanup_tables().

endpoint_verdict_round_trip_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    ?assertEqual(undefined, push_ets_cache:get_endpoint_verdict(<<"push.example.com">>)),
    ok = push_ets_cache:put_endpoint_verdict(<<"push.example.com">>, ok, 300),
    ?assertEqual({ok, ok}, push_ets_cache:get_endpoint_verdict(<<"push.example.com">>)),
    ok = push_ets_cache:put_endpoint_verdict(
        <<"bad.example.com">>, {error, endpoint_blocked}, 30
    ),
    ?assertEqual(
        {ok, {error, endpoint_blocked}},
        push_ets_cache:get_endpoint_verdict(<<"bad.example.com">>)
    ),
    cleanup_tables().

endpoint_verdicts_expire_and_are_reclaimed_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    ok = push_ets_cache:put_endpoint_verdict(<<"stale.example.com">>, ok, 300),
    Stale = erlang:system_time(second) - 1,
    true = ets:insert(push_endpoint_verdicts, {<<"stale.example.com">>, ok, Stale}),
    ?assertEqual(undefined, push_ets_cache:get_endpoint_verdict(<<"stale.example.com">>)),
    ok = push_ets_cache:evict_tables(#{}),
    ?assertEqual([], ets:lookup(push_endpoint_verdicts, <<"stale.example.com">>)),
    cleanup_tables().

an_oversized_host_is_never_cached_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    Oversized = binary:copy(<<"a">>, 254),
    ok = push_ets_cache:put_endpoint_verdict(Oversized, ok, 300),
    ?assertEqual(undefined, push_ets_cache:get_endpoint_verdict(Oversized)),
    ?assertEqual(0, push_ets_cache:table_size(push_endpoint_verdicts)),
    AtLimit = binary:copy(<<"a">>, 253),
    ok = push_ets_cache:put_endpoint_verdict(AtLimit, ok, 300),
    ?assertEqual({ok, ok}, push_ets_cache:get_endpoint_verdict(AtLimit)),
    cleanup_tables().

endpoint_verdicts_stay_bounded_under_max_length_hosts_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    lists:foreach(fun seed_max_length_verdict/1, lists:seq(1, 20000)),
    Size = push_ets_cache:table_size(push_endpoint_verdicts),
    ?assert(Size >= 1500),
    ?assert(Size =< 2048),
    Bytes = ets:info(push_endpoint_verdicts, memory) * erlang:system_info(wordsize),
    ?assert(Bytes =< 4 * 1024 * 1024),
    cleanup_tables().

seed_max_length_verdict(N) ->
    Suffix = integer_to_binary(N),
    Host = <<(binary:copy(<<"a">>, 253 - byte_size(Suffix)))/binary, Suffix/binary>>,
    ok = push_ets_cache:put_endpoint_verdict(Host, ok, 300),
    ?assert(push_ets_cache:table_size(push_endpoint_verdicts) =< 2048).

endpoint_verdicts_are_reported_in_cache_stats_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    ok = push_ets_cache:put_endpoint_verdict(<<"push.example.com">>, ok, 300),
    Stats = push_ets_cache:cache_stats(),
    ?assertEqual(1, maps:get(endpoint_verdicts_size, Stats)),
    cleanup_tables().

seed_subscriptions(UserId, Subscriptions) ->
    push_ets_cache:put_subscriptions(
        UserId, Subscriptions, push_ets_cache:reserve_subscriptions([UserId])
    ).

seed_badge_count(UserId, Count, CachedAt) ->
    push_ets_cache:put_badge_count(
        UserId, Count, CachedAt, push_ets_cache:reserve_badge_counts([UserId])
    ).

find_split_user_ids(Members, RemoteNode) ->
    Local =
        hd([
            Id
         || Id <- lists:seq(1, 2000),
            gateway_node_router:select_owner_node(Id, Members) =:= node()
        ]),
    Remote =
        hd([
            Id
         || Id <- lists:seq(1, 2000),
            gateway_node_router:select_owner_node(Id, Members) =:= RemoteNode
        ]),
    {Local, Remote}.

cleanup_tables() ->
    delete_table(push_user_guild_settings),
    delete_table(push_subscriptions),
    delete_table(push_blocked_ids),
    delete_table(push_badge_counts),
    delete_table(push_bearer_tokens),
    delete_table(push_endpoint_verdicts),
    ok.

delete_table(Table) ->
    try ets:delete(Table) of
        _ -> ok
    catch
        throw:_ -> ok;
        error:_ -> ok;
        exit:_ -> ok
    end.
