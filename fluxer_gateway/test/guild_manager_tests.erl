%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

handoff_guild_ids_counts_attempts_and_successes_test() ->
    LocalNode = 'gateway_a@127.0.0.1',
    OwnerResolver = fun
        (1) -> LocalNode;
        (2) -> 'gateway_b@127.0.0.1';
        (3) -> 'gateway_c@127.0.0.1'
    end,
    HandoffFun = fun
        (2, 'gateway_b@127.0.0.1', AccState) -> {true, AccState#{shard_count := 2}};
        (3, 'gateway_c@127.0.0.1', AccState) -> {false, AccState#{shard_count := 3}}
    end,
    {Result, FinalState} = guild_manager_handoff:handoff_guild_ids(
        [1, 2, 3], LocalNode, OwnerResolver, HandoffFun, empty_handoff_state()
    ),
    ?assertEqual(#{attempted => 2, handed_off => 1}, Result),
    ?assertEqual(3, maps:get(shard_count, FinalState)).

handoff_to_topology_keeps_guild_the_router_owns_here_test() ->
    GuildId = 77,
    ShardPid = spawn(fun() -> local_ids_stub_loop([GuildId]) end),
    State = #{shards => #{0 => #{pid => ShardPid, ref => make_ref()}}, shard_count => 1},
    persistent_term:put({gateway_cluster_membership, members}, [node()]),
    persistent_term:put({gateway_cluster_membership, members_by_role}, #{guilds => [node()]}),
    try
        {Result, _State} = guild_manager_handoff:perform_handoff_to_topology(
            ['gateway_b@127.0.0.1'], State
        ),
        ?assertEqual(#{attempted => 0, handed_off => 0}, Result)
    after
        ShardPid ! stop,
        persistent_term:erase({gateway_cluster_membership, members}),
        persistent_term:erase({gateway_cluster_membership, members_by_role})
    end.

local_ids_stub_loop(GuildIds) ->
    receive
        stop ->
            ok;
        {'$gen_call', From, get_local_guild_ids} ->
            gen_server:reply(From, {ok, GuildIds}),
            local_ids_stub_loop(GuildIds);
        {'$gen_call', From, _Request} ->
            gen_server:reply(From, {error, not_found}),
            local_ids_stub_loop(GuildIds)
    end.

empty_handoff_state() ->
    #{shards => #{}, shard_count => 1}.

cleanup_guild_from_cache_does_not_remove_new_pid_test() ->
    delete_table(guild_pid_cache),
    guild_manager_cache:ensure_guild_pid_cache(),
    try
        OldPid = spawn(fun() -> ok end),
        timer:sleep(10),
        NewPid = spawn(fun() -> timer:sleep(1000) end),
        ets:insert(guild_pid_cache, {42, NewPid}),
        guild_manager_cache:cleanup_guild_from_cache(OldPid),
        [{42, FoundPid}] = ets:lookup(guild_pid_cache, 42),
        ?assertEqual(NewPid, FoundPid)
    after
        delete_table(guild_pid_cache)
    end.

start_or_lookup_does_not_bypass_manager_owner_gate_test_() ->
    {timeout, 10, fun() ->
        delete_table(guild_pid_cache),
        delete_table(guild_manager_shard_table),
        guild_manager_cache:ensure_guild_pid_cache(),
        guild_manager_cache:ensure_shard_table(),
        GuildId = 101,
        GuildPid = spawn(fun() -> timer:sleep(1000) end),
        ShardPid = spawn(fun() -> shard_stub_loop(GuildId, GuildPid) end),
        ets:insert(guild_manager_shard_table, {shard_count, 1}),
        ets:insert(guild_manager_shard_table, {{shard_pid, 0}, ShardPid}),
        try
            ?assertEqual({error, unavailable}, guild_manager:start_or_lookup(GuildId))
        after
            ShardPid ! stop,
            delete_table(guild_manager_shard_table),
            delete_table(guild_pid_cache)
        end
    end}.

delete_table(Name) ->
    try ets:delete(Name) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

shard_stub_loop(GuildId, GuildPid) ->
    receive
        stop ->
            ok;
        {'$gen_call', From, {start_or_lookup, GuildId}} ->
            gen_server:reply(From, {ok, GuildPid}),
            shard_stub_loop(GuildId, GuildPid);
        {'$gen_call', From, {lookup, GuildId}} ->
            gen_server:reply(From, {ok, GuildPid}),
            shard_stub_loop(GuildId, GuildPid);
        {'$gen_call', From, _Request} ->
            gen_server:reply(From, {error, unsupported}),
            shard_stub_loop(GuildId, GuildPid);
        _ ->
            shard_stub_loop(GuildId, GuildPid)
    after infinity ->
        ok
    end.
