%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(clustered_ets_cache).
-typing([eqwalizer]).

-export([
    determine_shard_count/1,
    determine_shard_count/2,
    default_shard_count/0,
    select_shard/2,
    resolve_owner_node/1,
    resolve_owner_node/2,
    resolve_owner_node/3,
    resolve_owner_nodes/2,
    resolve_owner_nodes/3,
    group_keys_by_owner/1,
    group_keys_by_owner/2
]).
-export_type([shard_source/0]).

-type shard_source() :: atom() | auto.

-define(OWNER_HASH_LIMIT, 16#FFFFFFFF).

-spec determine_shard_count([atom()]) -> {pos_integer(), shard_source()}.
determine_shard_count(ConfigKeys) when is_list(ConfigKeys) ->
    determine_shard_count(ConfigKeys, fun fluxer_gateway_env:get/1).

-spec determine_shard_count([atom()], fun((atom()) -> term())) ->
    {pos_integer(), shard_source()}.
determine_shard_count([], _GetConfigValue) ->
    {default_shard_count(), auto};
determine_shard_count([ConfigKey | Rest], GetConfigValue) ->
    case validate_shard_count(safe_config_value(ConfigKey, GetConfigValue)) of
        {ok, Count} ->
            {Count, ConfigKey};
        error ->
            determine_shard_count(Rest, GetConfigValue)
    end.

-spec safe_config_value(atom(), fun((atom()) -> term())) -> term().
safe_config_value(ConfigKey, GetConfigValue) ->
    try GetConfigValue(ConfigKey) of
        Value -> Value
    catch
        error:_Reason -> undefined;
        exit:_Reason -> undefined
    end.

-spec default_shard_count() -> pos_integer().
default_shard_count() ->
    shard_utils:max_positive([
        erlang:system_info(logical_processors_available),
        erlang:system_info(schedulers_online)
    ]).

-spec select_shard(term(), pos_integer()) -> non_neg_integer().
select_shard(Key, Count) when Count > 0 ->
    rendezvous_router:select(Key, Count).

-spec resolve_owner_node(term()) -> node().
resolve_owner_node(Key) ->
    resolve_owner_node(Key, node(), fun gateway_node_router:owner_node/1).

-spec resolve_owner_node(term(), atom()) -> node() | unavailable.
resolve_owner_node(Key, Role) when is_atom(Role) ->
    owner_node_from_result(safe_owner_node_result(Key, Role)).

-spec resolve_owner_node(term(), node(), fun((term()) -> term())) -> node().
resolve_owner_node(Key, LocalNode, OwnerResolver) ->
    owner_node_or_local(safe_owner_node(Key, OwnerResolver), LocalNode).

-spec owner_node_from_result({ok, node()} | unavailable) -> node() | unavailable.
owner_node_from_result({ok, OwnerNode}) ->
    OwnerNode;
owner_node_from_result(unavailable) ->
    unavailable.

-spec owner_node_or_local(term(), node()) -> node().
owner_node_or_local(OwnerNode, LocalNode) when is_atom(OwnerNode) ->
    case lists:member($@, atom_to_list(OwnerNode)) of
        true -> OwnerNode;
        false -> LocalNode
    end;
owner_node_or_local(_OwnerNode, LocalNode) ->
    LocalNode.

-spec safe_owner_node(term(), fun((term()) -> term())) -> term().
safe_owner_node(Key, OwnerResolver) ->
    try OwnerResolver(Key) of
        OwnerNode -> OwnerNode
    catch
        error:_Reason -> undefined;
        exit:_Reason -> undefined
    end.

-spec safe_owner_node_result(term(), atom() | fun((term()) -> term())) ->
    {ok, node()} | unavailable.
safe_owner_node_result(Key, Role) when is_atom(Role) ->
    safe_owner_node_result(
        Key,
        fun(OwnerKey) -> gateway_node_router:owner_node_result(OwnerKey, Role) end
    );
safe_owner_node_result(Key, OwnerResolver) ->
    try OwnerResolver(Key) of
        {ok, OwnerNode} when is_atom(OwnerNode) ->
            normalize_owner_node(OwnerNode);
        OwnerNode when is_atom(OwnerNode) ->
            normalize_owner_node(OwnerNode);
        {error, _Reason} ->
            unavailable;
        _ ->
            unavailable
    catch
        error:_Reason -> unavailable;
        exit:_Reason -> unavailable;
        throw:_Reason -> unavailable
    end.

-spec normalize_owner_node(atom()) -> {ok, node()} | unavailable.
normalize_owner_node(OwnerNode) when is_atom(OwnerNode) ->
    case lists:member($@, atom_to_list(OwnerNode)) of
        true -> {ok, OwnerNode};
        false -> unavailable
    end.

-spec resolve_owner_nodes(term(), pos_integer()) -> [node()].
resolve_owner_nodes(Key, ReplicaCount) when is_integer(ReplicaCount), ReplicaCount > 0 ->
    PrimaryNode = resolve_owner_node(Key),
    CandidateNodes = resolve_candidate_nodes(PrimaryNode),
    select_owner_nodes(Key, CandidateNodes, ReplicaCount).

-spec resolve_owner_nodes(term(), pos_integer(), atom()) -> [node()].
resolve_owner_nodes(Key, ReplicaCount, Role) when is_integer(ReplicaCount), ReplicaCount > 0 ->
    case resolve_owner_node(Key, Role) of
        unavailable ->
            [];
        PrimaryNode when is_atom(PrimaryNode) ->
            CandidateNodes = resolve_candidate_nodes(PrimaryNode, Role),
            select_owner_nodes(Key, CandidateNodes, ReplicaCount)
    end.

-spec group_keys_by_owner([term()]) -> [{node(), [term()]}].
group_keys_by_owner(Keys) ->
    group_keys_by_owner(Keys, fun gateway_node_router:owner_node/1).

-spec group_keys_by_owner([term()], atom() | fun((term()) -> term())) -> [{node(), [term()]}].
group_keys_by_owner(Keys, Role) when is_atom(Role) ->
    group_keys_by_owner(Keys, owner_node_result_resolver(Role));
group_keys_by_owner(Keys, OwnerResolver) ->
    UniqueKeys = lists:usort(Keys),
    GroupedMap = lists:foldl(
        fun(Key, Acc) ->
            add_owner_key(Key, OwnerResolver, Acc)
        end,
        #{},
        UniqueKeys
    ),
    lists:sort(
        maps:fold(
            fun(OwnerNode, OwnerKeys, Acc) ->
                [{OwnerNode, lists:reverse(OwnerKeys)} | Acc]
            end,
            [],
            GroupedMap
        )
    ).

-spec owner_node_result_resolver(atom()) -> fun((term()) -> term()).
owner_node_result_resolver(Role) ->
    fun(OwnerKey) -> gateway_node_router:owner_node_result(OwnerKey, Role) end.

-spec add_owner_key(term(), fun((term()) -> term()), #{node() => [term()]}) ->
    #{node() => [term()]}.
add_owner_key(Key, OwnerResolver, Acc) ->
    case safe_owner_node_result(Key, OwnerResolver) of
        {ok, OwnerNode} ->
            OwnerKeys = maps:get(OwnerNode, Acc, []),
            Acc#{OwnerNode => [Key | OwnerKeys]};
        unavailable ->
            Acc
    end.

-spec resolve_candidate_nodes(node()) -> [node()].
resolve_candidate_nodes(PrimaryNode) ->
    resolve_candidate_nodes(PrimaryNode, undefined).

-spec resolve_candidate_nodes(node(), atom() | undefined) -> [node()].
resolve_candidate_nodes(PrimaryNode, Role) ->
    ActiveNodes = [Node || Node <- safe_active_nodes_for_role(Role), is_atom(Node)],
    lists:usort([PrimaryNode | ActiveNodes]).

-spec safe_active_nodes_for_role(atom() | undefined) -> [node()].
safe_active_nodes_for_role(Role) ->
    try active_nodes_for_role(Role) of
        Nodes -> Nodes
    catch
        error:_Reason -> [];
        exit:_Reason -> []
    end.

-spec active_nodes_for_role(atom() | undefined) -> [node()].
active_nodes_for_role(undefined) ->
    gateway_node_router:active_nodes();
active_nodes_for_role(Role) ->
    gateway_node_router:active_nodes(Role).

-spec select_owner_nodes(term(), [node(), ...], pos_integer()) -> [node()].
select_owner_nodes(Key, CandidateNodes, ReplicaCount) ->
    case lists:usort(CandidateNodes) of
        [SingleNode] ->
            [SingleNode];
        UniqueNodes when ReplicaCount =:= 1 ->
            [select_top_owner_node(Key, UniqueNodes)];
        UniqueNodes ->
            select_sorted_owner_nodes(Key, UniqueNodes, ReplicaCount)
    end.

-spec select_top_owner_node(term(), [node(), ...]) -> node().
select_top_owner_node(Key, [FirstNode | RestNodes]) ->
    {TopNode, _TopWeight} = lists:foldl(
        fun(Node, Best) -> select_higher_owner(Key, Node, Best) end,
        {FirstNode, owner_node_weight(Key, FirstNode)},
        RestNodes
    ),
    TopNode.

-spec select_higher_owner(term(), node(), {node(), non_neg_integer()}) ->
    {node(), non_neg_integer()}.
select_higher_owner(Key, Node, {BestNode, BestWeight} = Best) ->
    Weight = owner_node_weight(Key, Node),
    case higher_priority(Weight, Node, BestWeight, BestNode) of
        true -> {Node, Weight};
        false -> Best
    end.

-spec higher_priority(non_neg_integer(), node(), non_neg_integer(), node()) -> boolean().
higher_priority(WeightA, NodeA, WeightB, NodeB) ->
    (WeightA > WeightB) orelse (WeightA =:= WeightB andalso NodeA < NodeB).

-spec select_sorted_owner_nodes(term(), [node()], pos_integer()) -> [node()].
select_sorted_owner_nodes(Key, UniqueNodes, ReplicaCount) ->
    WeightedNodes = [{Node, owner_node_weight(Key, Node)} || Node <- UniqueNodes],
    SortedNodes = lists:sort(
        fun({NodeA, WeightA}, {NodeB, WeightB}) ->
            higher_priority(WeightA, NodeA, WeightB, NodeB)
        end,
        WeightedNodes
    ),
    LimitedNodes = lists:sublist(SortedNodes, erlang:min(ReplicaCount, length(SortedNodes))),
    [Node || {Node, _Weight} <- LimitedNodes].

-spec owner_node_weight(term(), node()) -> non_neg_integer().
owner_node_weight(Key, Node) ->
    erlang:phash2({Key, Node}, ?OWNER_HASH_LIMIT).

-spec validate_shard_count(term()) -> {ok, pos_integer()} | error.
validate_shard_count(Value) when is_integer(Value), Value > 0 ->
    {ok, Value};
validate_shard_count(_) ->
    error.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

resolve_owner_node_uses_remote_owner_test() ->
    LocalNode = node(),
    RemoteNode = 'gateway_b@127.0.0.1',
    ?assertEqual(
        RemoteNode,
        resolve_owner_node(42, LocalNode, fun(_Key) -> RemoteNode end)
    ).

resolve_owner_node_rejects_invalid_role_owner_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{gateway_role => websocket}
    ),
    persistent_term:put({gateway_cluster_membership, members_by_role}, #{
        websocket => [node()]
    }),
    ?assertEqual(
        unavailable,
        resolve_owner_node(42, presence)
    ),
    persistent_term:erase({gateway_cluster_membership, members_by_role}),
    persistent_term:erase({fluxer_gateway, runtime_config}).

group_keys_by_owner_skips_unavailable_role_owner_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{gateway_role => websocket}
    ),
    persistent_term:put({gateway_cluster_membership, members_by_role}, #{
        websocket => [node()]
    }),
    ?assertEqual([], group_keys_by_owner([1, 2], presence)),
    persistent_term:erase({gateway_cluster_membership, members_by_role}),
    persistent_term:erase({fluxer_gateway, runtime_config}).

group_keys_by_owner_deduplicates_and_sorts_test() ->
    NodeA = 'gateway_a@127.0.0.1',
    NodeB = 'gateway_b@127.0.0.1',
    Groups = group_keys_by_owner(
        [2, 1, 2, 4, 3],
        fun
            (Key) when Key rem 2 =:= 0 -> NodeB;
            (_Key) -> NodeA
        end
    ),
    ?assertEqual([{NodeA, [1, 3]}, {NodeB, [2, 4]}], Groups).

group_keys_by_owner_accepts_role_test() ->
    persistent_term:put({gateway_cluster_membership, members_by_role}, #{
        presence => ['presence_a@127.0.0.1'],
        all => ['gateway_a@127.0.0.1']
    }),
    Groups = group_keys_by_owner([1, 2], presence),
    Owners = [Owner || {Owner, _Keys} <- Groups],
    ?assert(
        lists:all(
            fun(Owner) ->
                lists:member(Owner, ['presence_a@127.0.0.1', 'gateway_a@127.0.0.1'])
            end,
            Owners
        )
    ),
    persistent_term:erase({gateway_cluster_membership, members_by_role}).

resolve_owner_nodes_accepts_role_test() ->
    persistent_term:put({gateway_cluster_membership, members_by_role}, #{
        presence => ['presence_a@127.0.0.1', 'presence_b@127.0.0.1']
    }),
    Nodes = resolve_owner_nodes(123, 2, presence),
    ?assertEqual(2, length(Nodes)),
    ?assert(
        lists:all(
            fun(Node) ->
                lists:member(Node, ['presence_a@127.0.0.1', 'presence_b@127.0.0.1'])
            end,
            Nodes
        )
    ),
    persistent_term:erase({gateway_cluster_membership, members_by_role}).

select_owner_nodes_respects_replica_count_test() ->
    Nodes = ['gateway_a@127.0.0.1', 'gateway_b@127.0.0.1', 'gateway_c@127.0.0.1'],
    SelectedNodes = select_owner_nodes(<<"guild-123">>, Nodes, 2),
    ?assertEqual(2, length(SelectedNodes)),
    ?assertEqual(length(SelectedNodes), length(lists:usort(SelectedNodes))).

resolve_owner_nodes_includes_primary_owner_test() ->
    Key = <<"presence-owner-set">>,
    PrimaryNode = resolve_owner_node(Key, node(), fun(_AnyKey) -> 'gateway_z@127.0.0.1' end),
    CandidateNodes = [PrimaryNode, 'gateway_a@127.0.0.1'],
    SelectedNodes = select_owner_nodes(Key, CandidateNodes, 2),
    ?assert(lists:member(PrimaryNode, SelectedNodes)).

determine_shard_count_uses_first_valid_key_test() ->
    {Count, Source} = determine_shard_count(
        [guild_counts_cache_shards, guild_shards],
        fun
            (guild_counts_cache_shards) -> undefined;
            (guild_shards) -> 5;
            (_) -> undefined
        end
    ),
    ?assertEqual(5, Count),
    ?assertEqual(guild_shards, Source).

determine_shard_count_defaults_when_no_keys_set_test() ->
    {Count, Source} = determine_shard_count([presence_cache_shards], fun(_) -> undefined end),
    ?assert(Count >= 1),
    ?assertEqual(auto, Source).

-endif.
