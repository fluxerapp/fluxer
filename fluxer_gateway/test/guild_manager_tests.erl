%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

default_shard_count_positive_test() ->
    Count = guild_manager_shards:default_shard_count(),
    ?assert(Count >= 1).

select_shard_deterministic_test() ->
    GuildId = 12345,
    ShardCount = 8,
    Shard1 = guild_manager_shards:select_shard(GuildId, ShardCount),
    Shard2 = guild_manager_shards:select_shard(GuildId, ShardCount),
    ?assertEqual(Shard1, Shard2).

select_shard_in_range_test() ->
    ShardCount = 8,
    lists:foreach(
        fun(GuildId) ->
            Shard = guild_manager_shards:select_shard(GuildId, ShardCount),
            ?assert(Shard >= 0 andalso Shard < ShardCount)
        end,
        lists:seq(1, 100)
    ).

group_ids_by_shard_test() ->
    GuildIds = [1, 2, 3, 4, 5],
    ShardCount = 2,
    Groups = guild_manager_shards:group_ids_by_shard(GuildIds, ShardCount),
    AllIds = lists:flatten([Ids || {_Index, Ids} <- Groups]),
    ?assertEqual(lists:sort(GuildIds), lists:sort(AllIds)).

resolve_handoff_target_skips_local_owner_test() ->
    LocalNode = 'gateway_a@127.0.0.1',
    ?assertEqual(
        skip,
        guild_manager_handoff:resolve_handoff_target(10, LocalNode, fun(_GuildId) ->
            LocalNode
        end)
    ).

resolve_handoff_target_handoffs_remote_owner_test() ->
    LocalNode = 'gateway_a@127.0.0.1',
    RemoteNode = 'gateway_b@127.0.0.1',
    ?assertEqual(
        {handoff, RemoteNode},
        guild_manager_handoff:resolve_handoff_target(10, LocalNode, fun(_GuildId) ->
            RemoteNode
        end)
    ).

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

find_shard_by_ref_found_test() ->
    Ref = make_ref(),
    Shards = #{0 => #{pid => self(), ref => Ref}},
    ?assertMatch({ok, 0}, guild_manager_shards:find_shard_by_ref(Ref, Shards)).

find_shard_by_ref_not_found_test() ->
    Shards = #{0 => #{pid => self(), ref => make_ref()}},
    ?assertEqual(not_found, guild_manager_shards:find_shard_by_ref(make_ref(), Shards)).

find_shard_by_pid_found_test() ->
    Pid = self(),
    Shards = #{0 => #{pid => Pid, ref => make_ref()}},
    ?assertMatch({ok, 0}, guild_manager_shards:find_shard_by_pid(Pid, Shards)).

empty_handoff_state() ->
    #{shards => #{}, shard_count => 1}.

forward_call_to_shard_async_replies_test_() ->
    {timeout, 15, fun() ->
        delete_table(guild_pid_cache),
        GuildId = 99999,
        GuildPid = spawn(fun() -> timer:sleep(5000) end),
        ShardPid = spawn(fun() -> shard_stub_loop(GuildId, GuildPid) end),
        ShardRef = erlang:monitor(process, ShardPid),
        State = #{shards => #{0 => #{pid => ShardPid, ref => ShardRef}}, shard_count => 1},
        guild_manager_cache:ensure_guild_pid_cache(),
        try
            From = {self(), make_ref()},
            Result = guild_manager_router:forward_call_to_shard(
                GuildId, {start_or_lookup, GuildId}, From, State
            ),
            ?assertMatch({noreply, _NewState}, Result),
            ?assert(process_liveness:is_alive(ShardPid))
        after
            ShardPid ! stop,
            delete_table(guild_pid_cache)
        end
    end}.

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
