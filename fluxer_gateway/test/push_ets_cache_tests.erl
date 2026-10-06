%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_ets_cache_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

init_creates_tables_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    ?assertNotEqual(undefined, ets:whereis(push_user_guild_settings)),
    ?assertNotEqual(undefined, ets:whereis(push_blocked_ids)),
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

blocked_ids_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    ?assertEqual(undefined, push_ets_cache:get_blocked_ids(1)),
    ok = push_ets_cache:put_blocked_ids(1, [2, 3]),
    ?assertEqual([2, 3], push_ets_cache:get_blocked_ids(1)),
    cleanup_tables().

cache_stats_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    ok = push_ets_cache:put_blocked_ids(1, []),
    ok = push_ets_cache:put_blocked_ids(2, []),
    Stats = push_ets_cache:cache_stats(),
    ?assertEqual(
        #{blocked_ids_size => 2, user_guild_settings_size => 0},
        Stats
    ),
    cleanup_tables().

evict_tables_test() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    lists:foreach(
        fun(I) -> ok = push_ets_cache:put_user_guild_settings(I, 1, #{}) end,
        lists:seq(1, 10)
    ),
    ?assertEqual(10, push_ets_cache:table_size(push_user_guild_settings)),
    ok = push_ets_cache:evict_tables(#{user_guild_settings => 5}),
    ?assertEqual(5, push_ets_cache:table_size(push_user_guild_settings)),
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
    ok = push_ets_cache:put_blocked_ids(LocalUserId, [1]),
    ok = push_ets_cache:put_blocked_ids(RemoteUserId, [2]),
    ok = push_ets_cache:put_user_guild_settings(LocalUserId, 10, #{local => true}),
    ok = push_ets_cache:put_user_guild_settings(RemoteUserId, 10, #{remote => true}),
    ok = push_ets_cache:rebalance(),
    ?assertEqual([1], push_ets_cache:get_blocked_ids(LocalUserId)),
    ?assertEqual(undefined, push_ets_cache:get_blocked_ids(RemoteUserId)),
    ?assertEqual(#{local => true}, push_ets_cache:get_user_guild_settings(LocalUserId, 10)),
    ?assertEqual(undefined, push_ets_cache:get_user_guild_settings(RemoteUserId, 10)),
    persistent_term:erase({gateway_cluster_membership, members}),
    persistent_term:erase({gateway_cluster_membership, members_by_role}),
    cleanup_tables().

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
    delete_table(push_blocked_ids),
    ok.

delete_table(Table) ->
    try ets:delete(Table) of
        _ -> ok
    catch
        throw:_ -> ok;
        error:_ -> ok;
        exit:_ -> ok
    end.
