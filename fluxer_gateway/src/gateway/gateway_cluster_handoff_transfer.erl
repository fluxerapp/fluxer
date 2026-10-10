%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_cluster_handoff_transfer).
-typing([eqwalizer]).

-export([
    run_handoff/1,
    normalize_members/1,
    drain_targets/1,
    broadcast_ws_reconnect/0
]).

-ifdef(TEST).
-export([handoff_status/1]).
-endif.

-spec run_handoff([node()]) -> map().
run_handoff(Members) ->
    logger:info("Gateway cluster handoff start", #{members => Members}),
    Pause = maybe_enable_handoff_pause(),
    Result0 = handoff_result(Members, Pause),
    Result = Result0#{status => handoff_status(Result0)},
    logger:info("Gateway cluster handoff complete", #{result => Result}),
    Result.

-spec handoff_result([node()], enabled | disabled) -> map().
handoff_result(Members, Pause) ->
    GuildNodes = role_aware_targets(guilds, Members),
    SessionNodes = role_aware_targets(sessions, Members),
    CallNodes = role_aware_targets(calls, Members),
    try
        #{
            members => Members,
            handoff_event_pause => Pause,
            guilds => role_apply(guilds, guild_manager, handoff_to_topology, [GuildNodes]),
            sessions => role_apply(
                sessions, session_manager, handoff_to_topology, [SessionNodes]
            ),
            presence => role_apply(presence, presence_manager, handoff_for_drain, []),
            calls => role_apply(calls, call_manager, handoff_to_topology, [CallNodes]),
            caches => cache_handoff_result()
        }
    after
        maybe_disable_handoff_pause(Pause)
    end.

-spec cache_handoff_result() -> map().
cache_handoff_result() ->
    #{
        presence => role_apply(presence, presence_cache, rebalance, []),
        guild_counts => role_apply(guilds, guild_counts_cache, rebalance, []),
        push => role_apply(push, push_ets_cache, rebalance, [])
    }.

-spec handoff_status(term()) -> ok | error.
handoff_status(Result) when is_map(Result) ->
    case handoff_value_ok(Result) of
        true -> ok;
        false -> error
    end.

-spec handoff_value_ok(term()) -> boolean().
handoff_value_ok(skipped) ->
    true;
handoff_value_ok(ok) ->
    true;
handoff_value_ok({ok, Value}) ->
    handoff_value_ok(Value);
handoff_value_ok({error, _Reason}) ->
    false;
handoff_value_ok(#{attempted := Attempted, handed_off := HandedOff}) ->
    is_integer(Attempted) andalso is_integer(HandedOff) andalso HandedOff >= Attempted;
handoff_value_ok(Value) when is_map(Value) ->
    lists:all(fun handoff_value_ok/1, maps:values(Value));
handoff_value_ok(_Value) ->
    true.

-spec role_apply(atom(), module(), atom(), [term()]) -> term().
role_apply(Role, Module, Function, Args) ->
    case fluxer_gateway_sup:role_enabled(Role) of
        true -> safe_apply(Module, Function, Args);
        false -> skipped
    end.

-spec maybe_enable_handoff_pause() -> enabled | disabled.
maybe_enable_handoff_pause() ->
    case handoff_event_pause_enabled() of
        true ->
            shard_utils:safe_apply(fun gateway_event_pause:enable/0, ok),
            shard_utils:safe_apply(fun gateway_event_pause:freeze/0, ok),
            enabled;
        false ->
            disabled
    end.

-spec maybe_disable_handoff_pause(enabled | disabled) -> ok.
maybe_disable_handoff_pause(enabled) ->
    shard_utils:safe_apply(fun gateway_event_pause:disable/0, ok),
    ok;
maybe_disable_handoff_pause(disabled) ->
    ok.

-spec handoff_event_pause_enabled() -> boolean().
handoff_event_pause_enabled() ->
    try fluxer_gateway_env:get(handoff_enable_event_pause) of
        true -> true;
        _ -> false
    catch
        throw:_ -> false;
        error:_ -> false;
        exit:_ -> false
    end.

-spec safe_apply(module(), atom(), [term()]) -> term().
safe_apply(Module, Function, Args) ->
    try
        apply(Module, Function, Args)
    catch
        throw:Reason -> {error, {throw, Reason}};
        error:Reason -> {error, {error, Reason}};
        exit:Reason -> {error, {exit, Reason}}
    end.

-spec normalize_members(term()) -> [node()].
normalize_members(Members) when is_list(Members) ->
    lists:usort([Node || Node <- Members, is_atom(Node)]);
normalize_members(_) ->
    [node()].

-spec drain_targets(term()) -> [node()].
drain_targets(Members0) ->
    Members = normalize_members(Members0),
    Survivors = [M || M <- Members, M =/= node()],
    case Survivors of
        [] -> Members;
        _ -> Survivors
    end.

-spec role_aware_targets(atom(), term()) -> [node()].
role_aware_targets(Role, FallbackMembers) ->
    RoleNodes = normalize_members(
        shard_utils:safe_apply(fun() -> gateway_node_router:active_nodes(Role) end, [])
    ),
    FallbackTargets = normalize_members(FallbackMembers),
    case RoleNodes of
        [] ->
            [];
        _ ->
            filter_role_targets(RoleNodes, FallbackTargets)
    end.

-spec filter_role_targets([node()], [node()]) -> [node()].
filter_role_targets(RoleNodes, FallbackTargets) ->
    case [N || N <- RoleNodes, lists:member(N, FallbackTargets)] of
        [] -> RoleNodes;
        Targets -> Targets
    end.

-spec broadcast_ws_reconnect() -> non_neg_integer().
broadcast_ws_reconnect() ->
    Pids = normalize_pids(
        shard_utils:safe_apply(fun() -> ranch:procs(http, connections) end, [])
    ),
    lists:foreach(fun(Pid) -> Pid ! session_reconnect end, Pids),
    length(Pids).

-spec normalize_pids(term()) -> [pid()].
normalize_pids(Pids) when is_list(Pids) ->
    [Pid || Pid <- Pids, is_pid(Pid)];
normalize_pids(_Pids) ->
    [].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

run_handoff_handles_missing_managers_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{gateway_role => websocket}
    ),
    Result = run_handoff([node()]),
    ?assertMatch(
        #{status := ok, members := [Node]} when
            Node =:= node(),
        Result
    ),
    ?assertEqual(skipped, maps:get(guilds, Result)),
    ?assertEqual(skipped, maps:get(sessions, Result)),
    ?assertEqual(skipped, maps:get(calls, Result)),
    ?assert(maps:is_key(caches, Result)),
    persistent_term:erase({fluxer_gateway, runtime_config}).

handoff_status_marks_partial_handoff_as_error_test() ->
    Result = #{
        guilds => #{attempted => 2, handed_off => 1},
        sessions => {ok, #{attempted => 0, handed_off => 0}},
        caches => #{presence => ok}
    },
    ?assertEqual(error, handoff_status(Result)).

handoff_status_marks_failed_session_handoff_as_error_test() ->
    Result = #{
        guilds => #{attempted => 0, handed_off => 0},
        sessions => {ok, #{attempted => 2, handed_off => 1}},
        caches => #{presence => ok}
    },
    ?assertEqual(error, handoff_status(Result)).

handoff_status_accepts_complete_handoff_test() ->
    Result = #{
        guilds => #{attempted => 2, handed_off => 2},
        sessions => {ok, #{attempted => 3, handed_off => 3}},
        caches => #{presence => ok, push => skipped}
    },
    ?assertEqual(ok, handoff_status(Result)).

drain_targets_prefers_survivors_test() ->
    ?assertEqual(
        ['peer@host'],
        drain_targets([node(), 'peer@host'])
    ),
    ?assertEqual([node()], drain_targets([node()])).

handoff_pause_lifecycle_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{handoff_enable_event_pause => true}
    ),
    case whereis(gateway_event_pause) of
        undefined -> ok;
        Existing when is_pid(Existing) -> gen_server:stop(Existing)
    end,
    {ok, Pid} = gateway_event_pause:start_link(),
    ?assertEqual(enabled, maybe_enable_handoff_pause()),
    ?assert(gateway_event_pause:is_paused()),
    ?assert(gateway_event_pause:is_frozen()),
    ?assertEqual(ok, maybe_disable_handoff_pause(enabled)),
    ?assertNot(gateway_event_pause:is_paused()),
    ?assertNot(gateway_event_pause:is_frozen()),
    gen_server:stop(Pid),
    persistent_term:erase({fluxer_gateway, runtime_config}).

role_aware_targets_uses_role_nodes_test() ->
    persistent_term:put(
        {gateway_cluster_membership, members_by_role},
        #{
            guilds => ['guilds@a', 'guilds@b'],
            sessions => ['sessions@a'],
            all => ['all@a']
        }
    ),
    GuildTargets = role_aware_targets(guilds, [node(), 'fallback@a']),
    ?assert(lists:member('guilds@a', GuildTargets)),
    ?assert(lists:member('all@a', GuildTargets)),
    ?assertNot(lists:member('fallback@a', GuildTargets)),
    SessionTargets = role_aware_targets(
        sessions,
        [node(), 'fallback@a']
    ),
    ?assert(lists:member('sessions@a', SessionTargets)),
    ?assert(lists:member('all@a', SessionTargets)),
    persistent_term:erase(
        {gateway_cluster_membership, members_by_role}
    ).

role_aware_targets_does_not_fall_back_to_wrong_role_test() ->
    persistent_term:put(
        {gateway_cluster_membership, members_by_role},
        #{sessions => ['sessions@a']}
    ),
    Fallback = ['peer@a', 'peer@b'],
    ?assertEqual([], role_aware_targets(guilds, Fallback)),
    persistent_term:erase(
        {gateway_cluster_membership, members_by_role}
    ).

role_aware_targets_keeps_self_for_normal_topology_test() ->
    persistent_term:put(
        {gateway_cluster_membership, members_by_role},
        #{guilds => [node(), 'guilds@a']}
    ),
    Targets = role_aware_targets(guilds, [node()]),
    ?assertEqual([node()], Targets),
    persistent_term:erase(
        {gateway_cluster_membership, members_by_role}
    ).

role_aware_targets_keeps_self_when_only_node_test() ->
    persistent_term:put(
        {gateway_cluster_membership, members_by_role},
        #{guilds => [node()]}
    ),
    Targets = role_aware_targets(guilds, [node()]),
    ?assertEqual([node()], Targets),
    persistent_term:erase(
        {gateway_cluster_membership, members_by_role}
    ).

-endif.
