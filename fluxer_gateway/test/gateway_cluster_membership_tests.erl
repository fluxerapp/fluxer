%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_cluster_membership_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

-define(MEMBERS_KEY, {gateway_cluster_membership, members}).

-define(ROLE_MEMBERS_KEY, {gateway_cluster_membership, members_by_role}).
-define(NODE_POD_NAMES_KEY, {gateway_cluster_membership, node_pod_names}).

members_defaults_to_self_test() ->
    with_clean_cluster_terms(fun() ->
        ?assertEqual([node()], gateway_cluster_membership:members()),
        ?assertEqual(1, gateway_cluster_membership:alive_count()),
        ?assert(gateway_cluster_membership:is_member(node()))
    end).

init_records_self_under_current_role_test() ->
    with_clean_cluster_terms(fun() ->
        persistent_term:put(
            {fluxer_gateway, runtime_config},
            #{gateway_role => guilds}
        ),
        {ok, State} = gateway_cluster_membership:init(#{auto_subscribe => false}),
        RoleMembers = maps:get(members_by_role, State),
        ?assertEqual([node()], maps:get(guilds, RoleMembers)),
        ?assertEqual(RoleMembers, persistent_term:get(?ROLE_MEMBERS_KEY)),
        gateway_cluster_membership:terminate(normal, State)
    end).

apply_discovered_adds_only_connectable_peers_test() ->
    with_clean_cluster_terms(fun() ->
        Connect = fun connect_good_host/1,
        State0 = base_state(#{connect_fun => Connect}),
        State1 = gateway_cluster_membership:apply_discovered(['good@host', 'bad@host'], State0),
        ?assertEqual(
            lists:usort([node(), 'good@host']),
            maps:get(members, State1)
        )
    end).

connect_good_host('good@host') ->
    true;
connect_good_host(_) ->
    false.

apply_discovered_strips_self_test() ->
    with_clean_cluster_terms(fun() ->
        State0 = base_state(#{connect_fun => fun(_) -> true end}),
        State1 = gateway_cluster_membership:apply_discovered([node()], State0),
        ?assertEqual([node()], maps:get(members, State1))
    end).

nodedown_removes_member_but_keeps_self_test() ->
    with_clean_cluster_terms(fun() ->
        State0 = base_state(#{
            connect_fun => fun(_) -> true end
        }),
        State1 = gateway_cluster_membership:apply_discovered(['peer@a', 'peer@b'], State0),
        ?assert(lists:member('peer@a', maps:get(members, State1))),
        State2 = gateway_cluster_membership:maybe_remove_member('peer@a', State1),
        ?assertNot(lists:member('peer@a', maps:get(members, State2))),
        ?assert(lists:member(node(), maps:get(members, State2))),
        State3 = gateway_cluster_membership:maybe_remove_member(node(), State2),
        ?assert(lists:member(node(), maps:get(members, State3)))
    end).

nodeup_for_undiscovered_peer_is_ignored_test() ->
    with_clean_cluster_terms(fun() ->
        State0 = base_state(#{
            role_fun => fun(_) -> unknown end
        }),
        State1 = gateway_cluster_membership:maybe_add_member('stranger@host', State0),
        ?assertNot(lists:member('stranger@host', maps:get(members, State1)))
    end).

nodeup_for_discovered_peer_adds_it_test() ->
    with_clean_cluster_terms(fun() ->
        State0 = (base_state(#{}))#{discovered := ['peer@a']},
        State1 = gateway_cluster_membership:maybe_add_member('peer@a', State0),
        ?assert(lists:member('peer@a', maps:get(members, State1)))
    end).

update_members_skips_persistent_term_when_unchanged_test() ->
    with_clean_cluster_terms(fun() ->
        State0 = base_state(#{members => [node()]}),
        State1 = gateway_cluster_membership:update_members([node()], State0),
        ?assertEqual(State0, State1),
        ?assertEqual([node()], persistent_term:get(?MEMBERS_KEY, [node()]))
    end).

subscriber_receives_initial_state_then_transitions_test() ->
    with_clean_cluster_terms(fun() ->
        Self = self(),
        State0 = base_state(#{
            connect_fun => fun(_) -> true end,
            members => [node()]
        }),
        State1 = gateway_cluster_membership:add_subscriber(Self, State0),
        receive
            {cluster_membership_changed, M} -> ?assertEqual([node()], M)
        after 100 -> ?assert(false)
        end,
        State2 = gateway_cluster_membership:apply_discovered(['peer@x'], State1),
        receive
            {cluster_membership_changed, M2} ->
                ?assert(lists:member('peer@x', M2)),
                ?assert(lists:member(node(), M2))
        after 100 -> ?assert(false)
        end,
        _State3 = gateway_cluster_membership:apply_discovered(['peer@x'], State2),
        receive
            {cluster_membership_changed, _} -> ?assert(false)
        after 50 -> ok
        end
    end).

reconcile_removes_stale_members_test() ->
    with_clean_cluster_terms(fun() ->
        StaleNode = 'stale@host',
        LiveNode = 'live@host',
        State0 = base_state(#{
            members => [node(), StaleNode, LiveNode],
            connected_nodes_fun => fun() -> [LiveNode] end
        }),
        State1 = gateway_cluster_membership:reconcile_against_connected_nodes(State0),
        ?assertNot(lists:member(StaleNode, maps:get(members, State1))),
        ?assert(lists:member(LiveNode, maps:get(members, State1))),
        ?assert(lists:member(node(), maps:get(members, State1)))
    end).

reconcile_ignores_connected_non_members_missing_from_discovery_test() ->
    with_clean_cluster_terms(fun() ->
        LiveNode = 'live@host',
        State0 = base_state(#{
            members => [node()],
            connected_nodes_fun => fun() -> [LiveNode] end,
            role_fun => fun
                (Node) when Node =:= LiveNode -> guilds;
                (_) -> websocket
            end
        }),
        State1 = gateway_cluster_membership:reconcile_against_connected_nodes(State0),
        ?assertNot(lists:member(LiveNode, maps:get(members, State1))),
        ?assertEqual(undefined, maps:get(guilds, maps:get(members_by_role, State1), undefined))
    end).

reconcile_noop_when_all_connected_test() ->
    with_clean_cluster_terms(fun() ->
        LiveNode = 'live@host',
        State0 = base_state(#{
            members => [node(), LiveNode],
            connected_nodes_fun => fun() -> [LiveNode] end
        }),
        State1 = gateway_cluster_membership:reconcile_against_connected_nodes(State0),
        ?assertEqual(lists:usort(maps:get(members, State0)), maps:get(members, State1))
    end).

reconcile_keeps_previous_role_when_lookup_temporarily_unknown_test() ->
    with_clean_cluster_terms(fun() ->
        LiveNode = 'live@host',
        State0 = base_state(#{
            members => [node(), LiveNode],
            members_by_role => #{sessions => [LiveNode], websocket => [node()]},
            connected_nodes_fun => fun() -> [LiveNode] end,
            role_fun => fun
                (Node) when Node =:= LiveNode -> unknown;
                (_) -> websocket
            end
        }),
        State1 = gateway_cluster_membership:reconcile_against_connected_nodes(State0),
        ?assertEqual([LiveNode], maps:get(sessions, maps:get(members_by_role, State1)))
    end).

apply_discovered_retains_connected_existing_member_missing_from_dns_test() ->
    with_clean_cluster_terms(fun() ->
        ExistingNode = 'fluxer_gateway@127.0.0.2',
        NewNode = 'fluxer_gateway@127.0.0.3',
        State0 = membership_state(#{
            members => [node(), ExistingNode],
            discovered => [ExistingNode],
            connected_nodes => [ExistingNode],
            roles => #{node() => websocket, ExistingNode => sessions, NewNode => guilds}
        }),
        State1 = gateway_cluster_membership:apply_discovered([NewNode], State0),
        ?assertEqual(lists:usort([node(), ExistingNode, NewNode]), maps:get(members, State1)),
        RoleMembers = maps:get(members_by_role, State1),
        ?assertEqual([ExistingNode], maps:get(sessions, RoleMembers)),
        ?assertEqual([NewNode], maps:get(guilds, RoleMembers))
    end).

apply_discovered_ignores_unrelated_connected_controller_test() ->
    with_clean_cluster_terms(fun() ->
        ControllerNode = 'controller@127.0.0.1',
        DiscoveredNode = 'fluxer_gateway@127.0.0.4',
        State0 = membership_state(#{
            members => [node()],
            discovered => [],
            connected_nodes => [ControllerNode],
            roles => #{
                node() => websocket,
                ControllerNode => all,
                DiscoveredNode => sessions
            }
        }),
        State1 = gateway_cluster_membership:apply_discovered([DiscoveredNode], State0),
        ?assertEqual(lists:usort([node(), DiscoveredNode]), maps:get(members, State1)),
        ?assertNot(lists:member(ControllerNode, maps:get(members, State1))),
        RoleMembers = maps:get(members_by_role, State1),
        ?assertEqual([DiscoveredNode], maps:get(sessions, RoleMembers)),
        ?assertEqual(undefined, maps:get(all, RoleMembers, undefined))
    end).

maybe_add_member_rejects_valid_role_node_not_seen_in_discovery_test() ->
    with_clean_cluster_terms(fun() ->
        RemoteNode = 'fluxer_gateway@127.0.0.4',
        State0 = membership_state(#{
            members => [node()],
            discovered => [],
            connected_nodes => [RemoteNode],
            roles => #{node() => websocket, RemoteNode => presence}
        }),
        State1 = gateway_cluster_membership:maybe_add_member(RemoteNode, State0),
        ?assertEqual([node()], maps:get(members, State1)),
        ?assertEqual(
            undefined,
            maps:get(presence, maps:get(members_by_role, State1), undefined)
        )
    end).

maybe_add_member_rejects_unknown_role_node_not_seen_in_dns_test() ->
    with_clean_cluster_terms(fun() ->
        RemoteNode = 'fluxer_gateway@127.0.0.5',
        State0 = membership_state(#{
            members => [node()],
            discovered => [],
            connected_nodes => [RemoteNode],
            roles => #{node() => websocket, RemoteNode => unknown}
        }),
        State1 = gateway_cluster_membership:maybe_add_member(RemoteNode, State0),
        ?assertEqual([node()], maps:get(members, State1))
    end).

update_members_refreshes_role_for_existing_member_test() ->
    with_clean_cluster_terms(fun() ->
        RemoteNode = 'fluxer_gateway@127.0.0.6',
        RoleKey = {gateway_cluster_membership_test_role, RemoteNode},
        persistent_term:put(RoleKey, unknown),
        RoleFun = fun
            (Node) when Node =:= node() -> websocket;
            (Node) -> persistent_term:get({gateway_cluster_membership_test_role, Node}, unknown)
        end,
        State0 = base_state(#{
            members => [node(), RemoteNode],
            members_by_role => #{websocket => [node()]},
            discovered => [RemoteNode],
            connect_fun => fun(_Node) -> true end,
            connected_nodes_fun => fun() -> [RemoteNode] end,
            role_fun => RoleFun
        }),
        ?assertEqual(
            undefined, maps:get(sessions, maps:get(members_by_role, State0), undefined)
        ),
        persistent_term:put(RoleKey, sessions),
        State1 = gateway_cluster_membership:update_members([node(), RemoteNode], State0),
        ?assertEqual([RemoteNode], maps:get(sessions, maps:get(members_by_role, State1))),
        persistent_term:erase(RoleKey)
    end).

update_members_role_lookups_run_concurrently_test() ->
    with_clean_cluster_terms(fun() ->
        Nodes = [
            'fluxer_gateway@127.0.0.11',
            'fluxer_gateway@127.0.0.12',
            'fluxer_gateway@127.0.0.13',
            'fluxer_gateway@127.0.0.14',
            'fluxer_gateway@127.0.0.15'
        ],
        State0 = base_state(#{
            members => [],
            role_fun => fun(_Node) ->
                timer:sleep(100),
                sessions
            end
        }),
        Start = erlang:monotonic_time(millisecond),
        State1 = gateway_cluster_membership:update_members(Nodes, State0),
        ElapsedMs = erlang:monotonic_time(millisecond) - Start,
        ?assert(ElapsedMs < 300),
        ?assertEqual(lists:usort(Nodes), maps:get(sessions, maps:get(members_by_role, State1)))
    end).

member_nodedown_starts_presence_grace_and_nodeup_cancels_it_test() ->
    with_clean_cluster_terms(fun() ->
        with_presence_cache(fun(CachePid) ->
            Peer = 'peer@a',
            State0 = (base_state(#{members => lists:usort([node(), Peer])}))#{
                discovered => [Peer]
            },
            {noreply, State1} = gateway_cluster_membership:handle_info(
                {nodedown, Peer}, State0
            ),
            ?assertNot(lists:member(Peer, maps:get(members, State1))),
            ?assert(maps:is_key(Peer, pending_nodedown_cleanups(CachePid))),
            {noreply, State2} = gateway_cluster_membership:handle_info({nodeup, Peer}, State1),
            ?assert(lists:member(Peer, maps:get(members, State2))),
            ?assertEqual(#{}, pending_nodedown_cleanups(CachePid))
        end)
    end).

non_member_nodedown_does_not_start_presence_grace_test() ->
    with_clean_cluster_terms(fun() ->
        with_presence_cache(fun(CachePid) ->
            State0 = base_state(#{}),
            {noreply, _State1} = gateway_cluster_membership:handle_info(
                {nodedown, 'remsh@host'}, State0
            ),
            ?assertEqual(#{}, pending_nodedown_cleanups(CachePid))
        end)
    end).

with_presence_cache(Fun) ->
    {ok, Pid} =
        case whereis(presence_cache) of
            undefined -> presence_cache:start_link();
            Existing -> {ok, Existing}
        end,
    try
        Fun(Pid)
    after
        unlink(Pid),
        gen_server:stop(Pid)
    end.

pending_nodedown_cleanups(CachePid) ->
    maps:get(pending_nodedown_cleanups, sys:get_state(CachePid)).

base_state(Overrides) ->
    Defaults = #{
        members => [node()],
        members_by_role => #{all => [node()]},
        discovered => [],
        connect_fun => fun(_) -> false end,
        connected_nodes_fun => fun() -> [] end,
        role_fun => fun(_) -> all end,
        subscribers => [],
        node_reconcile_timer => undefined
    },
    maps:merge(Defaults, Overrides).

membership_state(Options) ->
    Members = maps:get(members, Options, [node()]),
    ConnectedNodes = maps:get(connected_nodes, Options, []),
    Roles = maps:get(roles, Options, #{node() => websocket}),
    RoleFun = fun(Node) -> maps:get(Node, Roles, unknown) end,
    base_state(#{
        members => Members,
        members_by_role => test_role_members(Members, RoleFun),
        discovered => maps:get(discovered, Options, []),
        connect_fun => fun(_Node) -> true end,
        connected_nodes_fun => fun() -> ConnectedNodes end,
        role_fun => RoleFun
    }).

test_role_members(Members, RoleFun) ->
    lists:foldl(
        fun(Node, Acc) ->
            Role = RoleFun(Node),
            Nodes = maps:get(Role, Acc, []),
            Acc#{Role => lists:usort([Node | Nodes])}
        end,
        #{},
        Members
    ).

with_clean_cluster_terms(Fun) ->
    with_restored_terms(
        [
            ?MEMBERS_KEY,
            ?ROLE_MEMBERS_KEY,
            ?NODE_POD_NAMES_KEY,
            {fluxer_gateway, runtime_config}
        ],
        fun() ->
            persistent_term:erase(?MEMBERS_KEY),
            persistent_term:erase(?ROLE_MEMBERS_KEY),
            persistent_term:erase(?NODE_POD_NAMES_KEY),
            persistent_term:erase({fluxer_gateway, runtime_config}),
            Fun()
        end
    ).

with_restored_terms(Keys, Fun) ->
    Saved = [{Key, persistent_term:get(Key, undefined)} || Key <- Keys],
    try
        Fun()
    after
        lists:foreach(fun restore_persistent_term/1, Saved)
    end.

restore_persistent_term({Key, undefined}) ->
    persistent_term:erase(Key);
restore_persistent_term({Key, Value}) ->
    persistent_term:put(Key, Value).
