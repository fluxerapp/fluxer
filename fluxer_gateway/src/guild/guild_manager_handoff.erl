%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager_handoff).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    perform_handoff_for_drain/1,
    perform_handoff_to_target/2,
    perform_handoff_to_topology/2,
    merge_handoff_results/2,
    collect_local_guild_ids/1,
    handoff_guild_ids/5,
    resolve_handoff_target/3
]).

-export_type([guild_id/0, handoff_result/0, state/0]).

-define(MAX_DRAIN_HANDOFF_ITERATIONS, 6).
-define(DRAIN_HANDOFF_RETRY_DELAY_MS, 100).

-type guild_id() :: integer().
-type shard_map() :: #{pid := pid(), ref := reference()}.
-type handoff_result() :: #{attempted := non_neg_integer(), handed_off := non_neg_integer()}.
-type state() :: #{shards := #{non_neg_integer() => shard_map()}, shard_count := pos_integer()}.
-type continue_fun() :: fun(
    (state(), non_neg_integer(), handoff_result(), handoff_result()) -> {
        handoff_result(), state()
    }
).

-spec perform_handoff_for_drain(state()) -> {handoff_result(), state()}.
perform_handoff_for_drain(State) ->
    perform_handoff_for_drain(State, 0, empty_result()).

-spec perform_handoff_to_target(node(), state()) -> {handoff_result(), state()}.
perform_handoff_to_target(TargetNode, State) ->
    perform_handoff_to_target(TargetNode, State, 0, empty_result()).

-spec perform_handoff_to_topology([node()], state()) -> {handoff_result(), state()}.
perform_handoff_to_topology(TargetNodes, State) ->
    perform_handoff_to_topology(TargetNodes, State, 0, empty_result()).

-spec merge_handoff_results(handoff_result(), handoff_result()) -> handoff_result().
merge_handoff_results(
    #{attempted := LeftAttempted, handed_off := LeftHandedOff},
    #{attempted := RightAttempted, handed_off := RightHandedOff}
) ->
    #{
        attempted => LeftAttempted + RightAttempted,
        handed_off => LeftHandedOff + RightHandedOff
    }.

-spec collect_local_guild_ids(state()) -> [guild_id()].
collect_local_guild_ids(State) ->
    Shards = maps:get(shards, State),
    GuildIds = lists:flatmap(fun collect_shard_guild_ids/1, maps:values(Shards)),
    lists:usort(GuildIds).

-spec handoff_guild_ids(
    [guild_id()],
    node(),
    fun((guild_id()) -> term()),
    fun((guild_id(), node(), state()) -> {boolean(), state()}),
    state()
) -> {handoff_result(), state()}.
handoff_guild_ids(GuildIds, LocalNode, OwnerResolver, HandoffFun, State) ->
    lists:foldl(
        fun(GuildId, Acc) ->
            handoff_one_guild(GuildId, LocalNode, OwnerResolver, HandoffFun, Acc)
        end,
        {empty_result(), State},
        GuildIds
    ).

-spec resolve_handoff_target(
    guild_id(), node(), fun((guild_id()) -> term())
) -> skip | {handoff, node()}.
resolve_handoff_target(GuildId, LocalNode, OwnerResolver) ->
    case safe_resolve_owner(GuildId, OwnerResolver) of
        {ok, OwnerNode} when is_atom(OwnerNode), OwnerNode =/= LocalNode ->
            {handoff, OwnerNode};
        _ ->
            skip
    end.

-spec perform_handoff_for_drain(state(), non_neg_integer(), handoff_result()) ->
    {handoff_result(), state()}.
perform_handoff_for_drain(State, Iteration, AccResult) when
    Iteration >= ?MAX_DRAIN_HANDOFF_ITERATIONS
->
    {AccResult, State};
perform_handoff_for_drain(State, Iteration, AccResult) ->
    GuildIds = collect_local_guild_ids(State),
    continue_handoff_for_drain(GuildIds, State, Iteration, AccResult).

-spec perform_handoff_to_target(node(), state(), non_neg_integer(), handoff_result()) ->
    {handoff_result(), state()}.
perform_handoff_to_target(_TargetNode, State, Iteration, AccResult) when
    Iteration >= ?MAX_DRAIN_HANDOFF_ITERATIONS
->
    {AccResult, State};
perform_handoff_to_target(TargetNode, State, Iteration, AccResult) ->
    GuildIds = collect_local_guild_ids(State),
    continue_handoff_to_target(TargetNode, GuildIds, State, Iteration, AccResult).

-spec perform_handoff_to_topology([node()], state(), non_neg_integer(), handoff_result()) ->
    {handoff_result(), state()}.
perform_handoff_to_topology(_TargetNodes, State, Iteration, AccResult) when
    Iteration >= ?MAX_DRAIN_HANDOFF_ITERATIONS
->
    {AccResult, State};
perform_handoff_to_topology(TargetNodes, State, Iteration, AccResult) ->
    GuildIds = collect_local_guild_ids(State),
    continue_handoff_to_topology(TargetNodes, GuildIds, State, Iteration, AccResult).

-spec continue_handoff_for_drain([guild_id()], state(), non_neg_integer(), handoff_result()) ->
    {handoff_result(), state()}.
continue_handoff_for_drain([], State, _Iteration, AccResult) ->
    {AccResult, State};
continue_handoff_for_drain(GuildIds, State, Iteration, AccResult) ->
    Resolver = fun(GId) ->
        gateway_node_router:owner_node_result(GId, guilds)
    end,
    run_and_continue(
        GuildIds, Resolver, AccResult, State, fun maybe_continue_drain/4, Iteration
    ).

-spec continue_handoff_to_target(
    node(), [guild_id()], state(), non_neg_integer(), handoff_result()
) -> {handoff_result(), state()}.
continue_handoff_to_target(_TargetNode, [], State, _Iteration, AccResult) ->
    {AccResult, State};
continue_handoff_to_target(TargetNode, GuildIds, State, Iteration, AccResult) ->
    Resolver = fun(_GId) -> TargetNode end,
    run_and_continue(
        GuildIds,
        Resolver,
        AccResult,
        State,
        fun(S1, I, M, B) ->
            maybe_continue_target(TargetNode, S1, I, M, B)
        end,
        Iteration
    ).

-spec continue_handoff_to_topology(
    [node()], [guild_id()], state(), non_neg_integer(), handoff_result()
) -> {handoff_result(), state()}.
continue_handoff_to_topology(_TargetNodes, [], State, _Iteration, AccResult) ->
    {AccResult, State};
continue_handoff_to_topology(TargetNodes, GuildIds, State, Iteration, AccResult) ->
    run_and_continue(
        GuildIds,
        topology_owner_resolver(),
        AccResult,
        State,
        fun(S1, I, M, B) ->
            maybe_continue_topology(TargetNodes, S1, I, M, B)
        end,
        Iteration
    ).

-spec topology_owner_resolver() -> fun((guild_id()) -> term()).
topology_owner_resolver() ->
    fun(GId) ->
        gateway_node_router:owner_node_result(GId, guilds)
    end.

-spec run_and_continue(
    [guild_id()],
    fun((guild_id()) -> term()),
    handoff_result(),
    state(),
    continue_fun(),
    non_neg_integer()
) -> {handoff_result(), state()}.
run_and_continue(GuildIds, Resolver, AccResult, State, ContinueFun, Iteration) ->
    HandoffFun = fun guild_manager_handoff_transfer:handoff_guild_to_owner/3,
    {BatchResult, State1} = handoff_guild_ids(
        GuildIds, node(), Resolver, HandoffFun, State
    ),
    Merged = merge_handoff_results(AccResult, BatchResult),
    ContinueFun(State1, Iteration, Merged, BatchResult).

-spec maybe_continue_drain(state(), non_neg_integer(), handoff_result(), handoff_result()) ->
    {handoff_result(), state()}.
maybe_continue_drain(State, Iteration, AccResult, BatchResult) ->
    maybe_retry(BatchResult, AccResult, State, fun() ->
        perform_handoff_for_drain(State, Iteration + 1, AccResult)
    end).

-spec maybe_continue_target(
    node(), state(), non_neg_integer(), handoff_result(), handoff_result()
) -> {handoff_result(), state()}.
maybe_continue_target(TargetNode, State, Iteration, AccResult, BatchResult) ->
    maybe_retry(BatchResult, AccResult, State, fun() ->
        perform_handoff_to_target(TargetNode, State, Iteration + 1, AccResult)
    end).

-spec maybe_continue_topology(
    [node()],
    state(),
    non_neg_integer(),
    handoff_result(),
    handoff_result()
) -> {handoff_result(), state()}.
maybe_continue_topology(TargetNodes, State, Iteration, AccResult, BatchResult) ->
    maybe_retry(BatchResult, AccResult, State, fun() ->
        perform_handoff_to_topology(TargetNodes, State, Iteration + 1, AccResult)
    end).

-spec maybe_retry(
    handoff_result(),
    handoff_result(),
    state(),
    fun(() -> {handoff_result(), state()})
) -> {handoff_result(), state()}.
maybe_retry(BatchResult, AccResult, State, RetryFun) ->
    case attempted(BatchResult) > 0 of
        true ->
            ok = gateway_retry_timer:wait(?DRAIN_HANDOFF_RETRY_DELAY_MS),
            RetryFun();
        false ->
            {AccResult, State}
    end.

-spec handoff_one_guild(
    guild_id(),
    node(),
    fun((guild_id()) -> term()),
    fun((guild_id(), node(), state()) -> {boolean(), state()}),
    {handoff_result(), state()}
) -> {handoff_result(), state()}.
handoff_one_guild(GuildId, LocalNode, OwnerResolver, HandoffFun, {AccResult, AccState}) ->
    case resolve_handoff_target(GuildId, LocalNode, OwnerResolver) of
        skip ->
            {AccResult, AccState};
        {handoff, TargetNode} ->
            apply_handoff(GuildId, TargetNode, HandoffFun, AccResult, AccState)
    end.

-spec apply_handoff(
    guild_id(),
    node(),
    fun((guild_id(), node(), state()) -> {boolean(), state()}),
    handoff_result(),
    state()
) -> {handoff_result(), state()}.
apply_handoff(GuildId, TargetNode, HandoffFun, AccResult, AccState) ->
    case HandoffFun(GuildId, TargetNode, AccState) of
        {true, NewState} -> {mark_handoff_success(AccResult), NewState};
        {false, NewState} -> {mark_handoff_attempt(AccResult), NewState}
    end.

-spec collect_shard_guild_ids(shard_map()) -> [guild_id()].
collect_shard_guild_ids(#{pid := Pid}) ->
    case
        shard_utils:safe_gen_call_remote(Pid, get_local_guild_ids, ?DEFAULT_GEN_SERVER_TIMEOUT)
    of
        {ok, Ids} when is_list(Ids) ->
            [GuildId || GuildId <- Ids, is_integer(GuildId), GuildId > 0];
        _ ->
            []
    end.

-spec safe_resolve_owner(guild_id(), fun((guild_id()) -> term())) -> {ok, node()} | error.
safe_resolve_owner(GuildId, OwnerResolver) ->
    try OwnerResolver(GuildId) of
        {ok, OwnerNode} when is_atom(OwnerNode) -> {ok, OwnerNode};
        {error, _Reason} -> error;
        OwnerNode when is_atom(OwnerNode) -> {ok, OwnerNode};
        _Other -> error
    catch
        error:badarg -> error;
        error:undef -> error;
        exit:{noproc, _Call} -> error;
        exit:{nodedown, _Node} -> error
    end.

-spec empty_result() -> handoff_result().
empty_result() ->
    #{attempted => 0, handed_off => 0}.

-spec attempted(handoff_result()) -> non_neg_integer().
attempted(#{attempted := Attempted}) ->
    Attempted.

-spec mark_handoff_attempt(handoff_result()) -> handoff_result().
mark_handoff_attempt(#{attempted := Attempted, handed_off := HandedOff}) ->
    #{attempted => Attempted + 1, handed_off => HandedOff}.

-spec mark_handoff_success(handoff_result()) -> handoff_result().
mark_handoff_success(#{attempted := Attempted, handed_off := HandedOff}) ->
    #{attempted => Attempted + 1, handed_off => HandedOff + 1}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

topology_handoff_keeps_a_guild_pinned_to_this_node_test() ->
    GuildId = 1100000000000000001,
    Peers = ['guilds_a@h', 'guilds_b@h'],
    Resolver = topology_owner_resolver(),
    persistent_term:put({gateway_cluster_membership, members}, [node() | Peers]),
    persistent_term:put({gateway_cluster_membership, members_by_role}, #{guilds => Peers}),
    try
        ?assertMatch({handoff, _}, resolve_handoff_target(GuildId, node(), Resolver)),
        application:set_env(fluxer_gateway, guild_owner_pins, #{GuildId => node()}),
        ?assertEqual(skip, resolve_handoff_target(GuildId, node(), Resolver)),
        ?assertMatch({handoff, _}, resolve_handoff_target(GuildId + 1, node(), Resolver))
    after
        application:unset_env(fluxer_gateway, guild_owner_pins),
        persistent_term:erase({gateway_cluster_membership, members}),
        persistent_term:erase({gateway_cluster_membership, members_by_role})
    end.

-endif.
