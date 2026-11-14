%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_node_router).
-typing([eqwalizer]).

-export([
    owner_node/2,
    owner_node_result/2,
    select_owner_node/2,
    pinned_guild_owner/1,
    active_nodes/0, active_nodes/1,
    is_ready/0,
    is_draining/0
]).

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
    case pinned_owner(Key, Role) of
        {ok, Pinned} -> {ok, Pinned};
        none -> hashed_owner_node_result(Key, Role)
    end.

-spec hashed_owner_node_result(term(), atom()) ->
    {ok, node()} | {error, {no_active_nodes, atom()}}.
hashed_owner_node_result(Key, Role) ->
    case active_nodes(Role) of
        [] -> fallback_owner_for_role(Role);
        Nodes -> {ok, select_owner_node(Key, Nodes)}
    end.

-spec pinned_owner(term(), atom()) -> {ok, node()} | none.
pinned_owner(Key, guilds) ->
    pinned_guild_owner(Key);
pinned_owner(_Key, _Role) ->
    none.

-spec pinned_guild_owner(term()) -> {ok, node()} | none.
pinned_guild_owner(GuildId) when is_integer(GuildId) ->
    case application:get_env(fluxer_gateway, guild_owner_pins) of
        {ok, #{GuildId := Node}} when is_atom(Node) -> live_pin(Node, nodes());
        _ -> none
    end;
pinned_guild_owner(_GuildId) ->
    none.

-spec live_pin(node(), [node()]) -> {ok, node()} | none.
live_pin(Node, _Connected) when Node =:= node() ->
    {ok, Node};
live_pin(Node, Connected) ->
    case lists:member(Node, Connected) of
        true -> {ok, Node};
        false -> none
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
    Active = intersect_current_members(lists:usort(Specific ++ Monoliths)),
    exclude_pinned_only_nodes(Role, Active);
role_active_nodes(Role, _RoleMembers) ->
    exclude_pinned_only_nodes(Role, local_role_nodes(Role)).

-spec local_role_nodes(atom()) -> [node()].
local_role_nodes(Role) ->
    case fluxer_gateway_sup:role_enabled(Role) of
        true -> [node()];
        false -> []
    end.

-spec exclude_pinned_only_nodes(atom(), [node()]) -> [node()].
exclude_pinned_only_nodes(guilds, Nodes) ->
    case application:get_env(fluxer_gateway, guild_pinned_only_nodes) of
        {ok, [_ | _] = Excluded} -> [Node || Node <- Nodes, not lists:member(Node, Excluded)];
        _ -> Nodes
    end;
exclude_pinned_only_nodes(_Role, Nodes) ->
    Nodes.

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
    case exclude_pinned_only_nodes(Role, local_role_nodes(Role)) of
        [Self] -> {ok, Self};
        _ -> {error, {no_active_nodes, Role}}
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
