%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild_pid_cache_tests).

-include_lib("eunit/include/eunit.hrl").

-define(GUILD_ID, 1539791936896897024).

dead_remote_guild_pid_leaves_cache_test_() ->
    {setup, fun start_peer/0, fun stop_peer/1, fun(Ctx) ->
        {timeout, 60, fun() -> dead_remote_pid_is_forgotten(Ctx) end}
    end}.

dead_remote_pid_is_forgotten(#{node := PeerNode}) ->
    guild_manager_cache:ensure_tables(),
    true = register(guild_manager, self()),
    try
        Old = spawn_remote_guild(PeerNode),
        gateway_rpc_guild_infra:cache_guild_pid(?GUILD_ID, Old),
        gateway_rpc_guild_infra:cache_guild_pid(?GUILD_ID, Old),
        State0 = #{shards => #{}, shard_count => 1},
        State1 = pump_manager(State0),
        ?assertEqual(#{{?GUILD_ID, Old} => true}, maps:get(cached_pid_watches, State1)),
        ?assertEqual({monitors, [{process, Old}]}, process_info(self(), monitors)),
        exit(Old, kill),
        State2 = pump_manager(State1),
        ?assertEqual(#{}, maps:get(cached_pid_watches, State2)),
        ?assert(lists:member(PeerNode, nodes())),
        ?assertEqual([], ets:lookup(guild_pid_cache, ?GUILD_ID)),
        Stale = spawn_remote_guild(PeerNode),
        gateway_rpc_guild_infra:cache_guild_pid(?GUILD_ID, Stale),
        New = spawn_remote_guild(PeerNode),
        gateway_rpc_guild_infra:cache_guild_pid(?GUILD_ID, New),
        State3 = pump_manager(State2),
        exit(Stale, kill),
        State4 = pump_manager(State3),
        ?assertEqual([{?GUILD_ID, New}], ets:lookup(guild_pid_cache, ?GUILD_ID)),
        exit(New, kill),
        _ = pump_manager(State4),
        ?assertEqual([], ets:lookup(guild_pid_cache, ?GUILD_ID))
    after
        unregister(guild_manager),
        guild_manager_cache:delete_tables()
    end.

pump_manager(State) ->
    receive
        {'$gen_cast', Msg} ->
            {noreply, Next} = guild_manager:handle_cast(Msg, State),
            pump_manager(Next);
        Info when is_tuple(Info), element(3, Info) =:= process ->
            {noreply, Next} = guild_manager:handle_info(Info, State),
            pump_manager(Next)
    after 500 ->
        State
    end.

spawn_remote_guild(PeerNode) ->
    Pid = erlang:spawn(PeerNode, timer, sleep, [infinity]),
    true = is_pid(Pid),
    Pid.

start_peer() ->
    _ = os:cmd("epmd -daemon"),
    case node() of
        nonode@nohost ->
            {ok, _} = net_kernel:start(list_to_atom("pidcache" ++ os:getpid()), #{
                name_domain => shortnames
            });
        _ ->
            ok
    end,
    {ok, Peer, Node} = peer:start_link(#{name => peer:random_name()}),
    #{peer => Peer, node => Node}.

stop_peer(#{peer := Peer}) ->
    peer:stop(Peer).
