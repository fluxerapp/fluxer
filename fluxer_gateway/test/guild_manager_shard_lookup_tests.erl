%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager_shard_lookup_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

do_start_or_lookup_loading_deduplicates_requests_test() ->
    GuildId = 4444,
    From1 = {self(), make_ref()},
    From2 = {self(), make_ref()},
    State0 = #{guilds => #{GuildId => loading}, pending_requests => #{}, shard_index => 0},
    {noreply, State1} = guild_manager_shard_lookup:do_start_or_lookup(GuildId, From1, State0),
    Pending1 = maps:get(pending_requests, State1),
    ?assertEqual([From1], maps:get(GuildId, Pending1)),
    {noreply, State2} = guild_manager_shard_lookup:do_start_or_lookup(GuildId, From2, State1),
    Requests = maps:get(GuildId, maps:get(pending_requests, State2)),
    ?assertEqual(2, length(Requests)),
    ?assert(lists:member(From1, Requests)),
    ?assert(lists:member(From2, Requests)).

lookup_or_fetch_rejects_zero_guild_rollout_test() ->
    process_registry:init(),
    OldConfig = gateway_rollout_config:get(),
    persistent_term:put(gateway_rollout_config, OldConfig#{<<"guild_rollout_percentage">> => 0}),
    GuildId = 99999,
    From = {self(), make_ref()},
    State = #{guilds => #{}, pending_requests => #{}, shard_index => 0},
    try
        Result = guild_manager_shard_lookup:lookup_or_fetch(GuildId, From, State),
        ?assertMatch({reply, {error, not_eligible}, _}, Result)
    after
        persistent_term:put(gateway_rollout_config, OldConfig)
    end.

stale_fetch_success_uses_superseding_tracked_guild_test() ->
    GuildId = 7321,
    ReplyRef = make_ref(),
    WorkerRef = make_ref(),
    FetchToken = make_ref(),
    SupersedingPid = spawn(fun() ->
        receive
            stop -> ok
        after infinity ->
            ok
        end
    end),
    timer:sleep(10),
    State0 = #{
        guilds => #{GuildId => {SupersedingPid, make_ref()}},
        pending_requests => #{GuildId => [{self(), ReplyRef}]},
        fetch_workers => #{WorkerRef => {GuildId, self(), FetchToken}},
        shard_index => 0
    },
    try
        {noreply, State1} = guild_manager_shard_lookup:handle_guild_data_fetched(
            GuildId,
            FetchToken,
            {ok, #{<<"guild">> => #{<<"id">> => <<"7321">>, <<"features">> => []}}},
            State0
        ),
        {TrackedPid, _Ref} = maps:get(GuildId, maps:get(guilds, State1)),
        ?assertEqual(SupersedingPid, TrackedPid),
        ?assertEqual(#{}, maps:get(pending_requests, State1)),
        receive
            {ReplyRef, {ok, SupersedingPid}} -> ok
        after 100 ->
            ?assert(false)
        end
    after
        SupersedingPid ! stop
    end.
