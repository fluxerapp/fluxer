%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_cluster_discovery_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

-define(PEERS_KEY, {gateway_cluster_discovery, peers}).

ip_addrs_to_peers_filters_self_test() ->
    Self = node(),
    SelfStr = atom_to_list(Self),
    case string:split(SelfStr, "@") of
        [Base, HostStr] ->
            case inet:parse_address(HostStr) of
                {ok, SelfAddr} ->
                    Others = other_ip_addrs(),
                    Peers = gateway_cluster_discovery:ip_addrs_to_peers(
                        [SelfAddr | Others], Base
                    ),
                    ?assertNot(lists:member(Self, Peers)),
                    ?assertEqual(2, length(Peers));
                _ ->
                    Peers = gateway_cluster_discovery:ip_addrs_to_peers(
                        other_ip_addrs(), Base
                    ),
                    ?assertEqual(2, length(Peers))
            end;
        _ ->
            ok
    end.

ip_addrs_to_peers_rejects_invalid_node_basename_test() ->
    ?assertEqual(
        [],
        gateway_cluster_discovery:ip_addrs_to_peers([{10, 0, 0, 1}], "bad node")
    ).

poll_with_undefined_dns_returns_empty_test() ->
    persistent_term:erase(?PEERS_KEY),
    State = base_state(#{dns_name => undefined}),
    {Result, _} = gateway_cluster_discovery:poll(State),
    ?assertEqual([], Result).

poll_with_static_peers_uses_static_peers_test() ->
    persistent_term:erase(?PEERS_KEY),
    StaticPeers = [list_to_atom("n@10.0.0.1"), node(), list_to_atom("n@10.0.0.2")],
    Resolver = fun(_Name) -> error(static_peer_resolver_called) end,
    State = base_state(#{
        static_peers => StaticPeers,
        resolver => Resolver,
        subscribers => []
    }),
    {Result, _} = gateway_cluster_discovery:poll(State),
    ?assertEqual(
        [list_to_atom("n@10.0.0.1"), list_to_atom("n@10.0.0.2")],
        Result
    ),
    ?assertEqual(Result, persistent_term:get(?PEERS_KEY)),
    persistent_term:erase(?PEERS_KEY).

other_ip_addrs() ->
    [{10, 0, 0, 2}, {10, 0, 0, 3}].

poll_publishes_peers_and_notifies_subscribers_test() ->
    persistent_term:erase(?PEERS_KEY),
    Self = self(),
    Resolver = fun("svc.test") -> {ok, [{10, 0, 0, 1}, {10, 0, 0, 2}]} end,
    State0 = base_state(#{
        dns_name => "svc.test",
        resolver => Resolver,
        subscribers => [{Self, erlang:monitor(process, Self)}]
    }),
    {Peers, State1} = gateway_cluster_discovery:poll(State0),
    ?assertEqual(
        [
            list_to_atom("n@10.0.0.1"),
            list_to_atom("n@10.0.0.2")
        ],
        Peers
    ),
    ?assertEqual(Peers, persistent_term:get(?PEERS_KEY)),
    receive
        {cluster_peers_changed, P} -> ?assertEqual(Peers, P)
    after 100 -> ?assert(false)
    end,
    {Peers2, _State2} = gateway_cluster_discovery:poll(State1),
    ?assertEqual(Peers, Peers2),
    receive
        {cluster_peers_changed, _} -> ?assert(false)
    after 50 -> ok
    end,
    persistent_term:erase(?PEERS_KEY).

poll_with_resolver_error_keeps_old_peers_test() ->
    persistent_term:erase(?PEERS_KEY),
    gateway_cluster_metrics:reset_for_tests(),
    OldPeers = [list_to_atom("n@10.0.0.9")],
    Resolver = fun("svc.test") -> {error, nxdomain} end,
    State = base_state(#{
        dns_name => "svc.test",
        resolver => Resolver,
        peers => OldPeers
    }),
    {Result, _} = gateway_cluster_discovery:poll(State),
    ?assertEqual(OldPeers, Result),
    Snapshot = gateway_cluster_metrics:snapshot(),
    ?assertEqual(1, maps:get(<<"gateway_cluster_discovery_resolve_failures_total">>, Snapshot)),
    persistent_term:erase(?PEERS_KEY).

subscriber_messages_include_initial_state_test() ->
    persistent_term:erase(?PEERS_KEY),
    Self = self(),
    InitialPeers = [list_to_atom("n@10.0.0.1")],
    State = base_state(#{peers => InitialPeers}),
    NewState = gateway_cluster_discovery:add_subscriber(Self, State),
    receive
        {cluster_peers_changed, P} -> ?assertEqual(InitialPeers, P)
    after 100 -> ?assert(false)
    end,
    ?assertMatch(#{subscribers := [{Self, _}]}, NewState),
    persistent_term:erase(?PEERS_KEY).

base_state(Overrides) ->
    Defaults = #{
        dns_name => "svc.test",
        node_basename => "n",
        poll_interval_ms => 5000,
        resolver => fun(_) -> {ok, []} end,
        static_peers => [],
        peers => [],
        subscribers => [],
        timer => undefined
    },
    maps:merge(Defaults, Overrides).
