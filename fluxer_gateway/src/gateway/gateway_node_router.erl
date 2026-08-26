%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_node_router).
-typing([eqwalizer]).

-export([
    owner_node/1, owner_node/2,
    owner_node_result/2,
    select_owner_node/2,
    active_nodes/0, active_nodes/1,
    is_ready/0,
    is_draining/0
]).

-spec owner_node(term()) -> node().
owner_node(Key) ->
    Owner = select_owner_node(Key, active_nodes()),
    gateway_cluster_metrics:record_owner_resolution(owner_result(Owner)),
    Owner.

-spec owner_node(term(), atom()) -> node().
owner_node(Key, Role) ->
    case owner_node_result(Key, Role) of
        {ok, Owner} ->
            gateway_cluster_metrics:record_owner_resolution(owner_result(Owner)),
            Owner;
        {error, Reason} ->
            error(Reason)
    end.

-spec owner_node_result(term(), atom()) -> {ok, node()} | {error, {no_active_nodes, atom()}}.
owner_node_result(Key, Role) ->
    case active_nodes(Role) of
        [] -> fallback_owner_for_role(Role);
        Nodes -> {ok, select_owner_node(Key, Nodes)}
    end.

-spec select_owner_node(term(), [node()]) -> node().
select_owner_node(_Key, []) ->
    node();
select_owner_node(_Key, [Only]) ->
    Only;
select_owner_node(Key, Nodes) when is_list(Nodes) ->
    Hashable = hashable_key(Key),
    Sorted = lists:usort(Nodes),
    case rendezvous_router:select_node(Hashable, Sorted) of
        undefined -> node();
        Owner -> Owner
    end.

-spec active_nodes() -> [node()].
active_nodes() ->
    case persistent_term:get({gateway_cluster_membership, members}, undefined) of
        undefined -> [node()];
        Members when is_list(Members), Members =/= [] -> Members;
        _ -> [node()]
    end.

-spec active_nodes(atom()) -> [node()].
active_nodes(Role) ->
    RoleMembers = persistent_term:get({gateway_cluster_membership, members_by_role}, undefined),
    role_active_nodes(Role, RoleMembers).

-spec role_active_nodes(atom(), term()) -> [node()].
role_active_nodes(Role, RoleMembers) when is_map(RoleMembers) ->
    Specific0 = maps:get(Role, RoleMembers, []),
    Monoliths0 = maps:get(all, RoleMembers, []),
    Specific = filter_atoms(Specific0),
    Monoliths = filter_atoms(Monoliths0),
    intersect_current_members(lists:usort(Specific ++ Monoliths));
role_active_nodes(Role, _RoleMembers) ->
    case fluxer_gateway_sup:role_enabled(Role) of
        true -> [node()];
        false -> []
    end.

-spec filter_atoms(term()) -> [atom()].
filter_atoms(List) when is_list(List) ->
    [X || X <- List, is_atom(X)];
filter_atoms(_) ->
    [].

-spec intersect_current_members([node()]) -> [node()].
intersect_current_members(RoleNodes) ->
    case persistent_term:get({gateway_cluster_membership, members}, undefined) of
        Members when is_list(Members), Members =/= [] ->
            [Node || Node <- RoleNodes, lists:member(Node, Members)];
        _ ->
            RoleNodes
    end.

-spec fallback_owner_for_role(atom()) -> {ok, node()} | {error, {no_active_nodes, atom()}}.
fallback_owner_for_role(Role) ->
    case fluxer_gateway_sup:role_enabled(Role) of
        true -> {ok, node()};
        false -> {error, {no_active_nodes, Role}}
    end.

-spec is_ready() -> boolean().
is_ready() ->
    not is_draining().

-spec is_draining() -> boolean().
is_draining() ->
    case persistent_term:get({fluxer_gateway, draining}, false) of
        false -> false;
        _ -> true
    end.

-spec hashable_key(term()) -> binary().
hashable_key(Key) when is_binary(Key) -> Key;
hashable_key(Key) when is_integer(Key) -> integer_to_binary(Key);
hashable_key(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
hashable_key(Key) -> term_to_binary(Key).

-spec owner_result(node()) -> self | peer.
owner_result(Owner) when Owner =:= node() ->
    self;
owner_result(_Owner) ->
    peer.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

active_nodes_falls_back_to_self_test() ->
    persistent_term:erase({gateway_cluster_membership, members}),
    ?assertEqual([node()], active_nodes()).

active_nodes_falls_back_when_empty_test() ->
    persistent_term:put({gateway_cluster_membership, members}, []),
    ?assertEqual([node()], active_nodes()),
    persistent_term:erase({gateway_cluster_membership, members}).

active_nodes_returns_membership_test() ->
    Peers = [node(), 'peer@a', 'peer@b'],
    persistent_term:put({gateway_cluster_membership, members}, Peers),
    ?assertEqual(Peers, active_nodes()),
    persistent_term:erase({gateway_cluster_membership, members}).

active_nodes_for_role_returns_role_and_monolith_nodes_test() ->
    persistent_term:put({gateway_cluster_membership, members}, ['sessions@a', 'all@a']),
    persistent_term:put({gateway_cluster_membership, members_by_role}, #{
        websocket => ['web@a'],
        sessions => ['sessions@a'],
        all => ['all@a']
    }),
    ?assertEqual(['all@a', 'sessions@a'], active_nodes(sessions)),
    persistent_term:erase({gateway_cluster_membership, members}),
    persistent_term:erase({gateway_cluster_membership, members_by_role}).

active_nodes_for_role_does_not_route_to_wrong_role_test() ->
    persistent_term:put({gateway_cluster_membership, members_by_role}, #{
        websocket => [node()]
    }),
    ?assertEqual([], active_nodes(sessions)),
    persistent_term:erase({gateway_cluster_membership, members_by_role}).

active_nodes_for_role_excludes_stale_role_members_test() ->
    persistent_term:put({gateway_cluster_membership, members}, [node()]),
    persistent_term:put({gateway_cluster_membership, members_by_role}, #{
        sessions => ['stale@a', node()]
    }),
    ?assertEqual([node()], active_nodes(sessions)),
    persistent_term:erase({gateway_cluster_membership, members}),
    persistent_term:erase({gateway_cluster_membership, members_by_role}).

active_nodes_for_role_without_role_map_keeps_local_role_only_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{gateway_role => sessions}
    ),
    persistent_term:erase({gateway_cluster_membership, members_by_role}),
    persistent_term:put(
        {gateway_cluster_membership, members},
        [node(), 'websocket@a']
    ),
    ?assertEqual([node()], active_nodes(sessions)),
    ?assertEqual([], active_nodes(guilds)),
    persistent_term:erase({gateway_cluster_membership, members}),
    persistent_term:erase({fluxer_gateway, runtime_config}).

owner_node_for_role_without_role_members_errors_on_wrong_role_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{gateway_role => websocket}
    ),
    persistent_term:put({gateway_cluster_membership, members_by_role}, #{
        websocket => [node()]
    }),
    ?assertError({no_active_nodes, sessions}, owner_node(<<"session">>, sessions)),
    persistent_term:erase({gateway_cluster_membership, members_by_role}),
    persistent_term:erase({fluxer_gateway, runtime_config}).

owner_node_with_single_member_returns_self_test() ->
    persistent_term:erase({gateway_cluster_membership, members}),
    ?assertEqual(node(), owner_node(<<"guild-123">>)),
    ?assertEqual(node(), owner_node(456)),
    ?assertEqual(node(), owner_node(some_atom)).

select_owner_node_deterministic_for_same_input_test() ->
    Peers = ['n@a', 'n@b', 'n@c'],
    ?assertEqual(
        select_owner_node(<<"guild-1">>, Peers),
        select_owner_node(<<"guild-1">>, Peers)
    ),
    ?assertEqual(
        select_owner_node(42, Peers),
        select_owner_node(42, Peers)
    ).

select_owner_node_partitions_keys_across_nodes_test() ->
    Peers = ['n@a', 'n@b', 'n@c'],
    Owners = [
        select_owner_node(<<"key-", (integer_to_binary(I))/binary>>, Peers)
     || I <- lists:seq(1, 300)
    ],
    Unique = lists:usort(Owners),
    ?assert(length(Unique) >= 2).

select_owner_node_stable_under_sort_order_test() ->
    Peers1 = ['n@a', 'n@b', 'n@c'],
    Peers2 = ['n@c', 'n@a', 'n@b'],
    ?assertEqual(
        select_owner_node(<<"k">>, Peers1),
        select_owner_node(<<"k">>, Peers2)
    ).

select_owner_node_with_empty_returns_self_test() ->
    ?assertEqual(node(), select_owner_node(<<"k">>, [])).

owner_node_uses_membership_when_present_test() ->
    gateway_cluster_metrics:reset_for_tests(),
    Peers = [node(), 'peer@b'],
    persistent_term:put({gateway_cluster_membership, members}, Peers),
    Owner = owner_node(<<"some-key">>),
    ?assert(lists:member(Owner, Peers)),
    Snapshot = gateway_cluster_metrics:snapshot(),
    Owners = maps:get(<<"gateway_node_router_owner_resolutions_total">>, Snapshot),
    ?assertEqual(1, maps:get(<<"self">>, Owners) + maps:get(<<"peer">>, Owners)),
    persistent_term:erase({gateway_cluster_membership, members}).

is_draining_reads_persistent_term_test() ->
    persistent_term:erase({fluxer_gateway, draining}),
    ?assertNot(is_draining()),
    persistent_term:put({fluxer_gateway, draining}, true),
    ?assert(is_draining()),
    persistent_term:erase({fluxer_gateway, draining}).

is_ready_inverts_is_draining_test() ->
    persistent_term:erase({fluxer_gateway, draining}),
    ?assert(is_ready()),
    persistent_term:put({fluxer_gateway, draining}, true),
    ?assertNot(is_ready()),
    persistent_term:erase({fluxer_gateway, draining}).

-endif.
