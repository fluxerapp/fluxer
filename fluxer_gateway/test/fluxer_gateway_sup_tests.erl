%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(fluxer_gateway_sup_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

cluster_children_empty_when_flag_unset_test() ->
    persistent_term:put({fluxer_gateway, runtime_config}, #{}),
    ?assertEqual([], fluxer_gateway_sup:cluster_children()),
    persistent_term:erase({fluxer_gateway, runtime_config}).

cluster_children_empty_when_flag_false_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{cluster_enabled => false}
    ),
    ?assertEqual([], fluxer_gateway_sup:cluster_children()),
    persistent_term:erase({fluxer_gateway, runtime_config}).

cluster_children_populated_when_flag_true_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{cluster_enabled => true}
    ),
    [Discovery, Membership, PgScope] = fluxer_gateway_sup:cluster_children(),
    ?assertMatch(
        #{
            id := gateway_cluster_discovery,
            start := {gateway_cluster_discovery, start_link, []}
        },
        Discovery
    ),
    ?assertMatch(
        #{
            id := gateway_cluster_membership,
            start := {gateway_cluster_membership, start_link, []}
        },
        Membership
    ),
    ?assertMatch(
        #{
            id := gateway_pg_scope,
            start := {gateway_pg_scope, start_link, []}
        },
        PgScope
    ),
    persistent_term:erase({fluxer_gateway, runtime_config}).

cluster_children_only_truthy_atom_true_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{cluster_enabled => <<"true">>}
    ),
    ?assertEqual([], fluxer_gateway_sup:cluster_children()),
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{cluster_enabled => 1}
    ),
    ?assertEqual([], fluxer_gateway_sup:cluster_children()),
    persistent_term:erase({fluxer_gateway, runtime_config}).

role_enabled_allows_monolith_and_exact_role_only_test() ->
    ?assert(fluxer_gateway_sup:role_enabled(sessions, all)),
    ?assert(fluxer_gateway_sup:role_enabled(guilds, guilds)),
    ?assertNot(fluxer_gateway_sup:role_enabled(sessions, websocket)),
    ?assertNot(fluxer_gateway_sup:role_enabled(push, guilds)).

invalid_gateway_role_falls_back_to_stateless_websocket_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{gateway_role => <<"guild_session_typo">>}
    ),
    {ok, {_SupFlags, Children}} = fluxer_gateway_sup:init([]),
    Ids = child_ids(Children),
    ?assert(lists:member(gateway_rollout_config, Ids)),
    ?assert(lists:member(gateway_dispatch_relay, Ids)),
    ?assertNot(lists:member(session_manager, Ids)),
    ?assertNot(lists:member(guild_manager, Ids)),
    ?assertNot(lists:member(presence_manager, Ids)),
    persistent_term:erase({fluxer_gateway, runtime_config}).

init_returns_baseline_children_without_cluster_flag_test() ->
    persistent_term:put({fluxer_gateway, runtime_config}, #{}),
    {ok, {_SupFlags, Children}} = fluxer_gateway_sup:init([]),
    Ids = child_ids(Children),
    ?assertNot(lists:member(gateway_cluster_discovery, Ids)),
    ?assertNot(lists:member(gateway_cluster_membership, Ids)),
    ?assertNot(lists:member(gateway_pg_scope, Ids)),
    ?assertNot(lists:member(gateway_cluster_handoff, Ids)),
    ?assert(lists:member(gateway_rollout_config, Ids)),
    ?assert(lists:member(session_manager, Ids)),
    persistent_term:erase({fluxer_gateway, runtime_config}).

init_websocket_role_excludes_stateful_children_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{gateway_role => websocket}
    ),
    {ok, {_SupFlags, Children}} = fluxer_gateway_sup:init([]),
    Ids = child_ids(Children),
    ?assert(lists:member(gateway_rollout_config, Ids)),
    ?assert(lists:member(gateway_dispatch_relay, Ids)),
    ?assertNot(lists:member(presence_bus, Ids)),
    ?assertNot(lists:member(session_manager, Ids)),
    ?assertNot(lists:member(presence_manager, Ids)),
    ?assertNot(lists:member(guild_manager, Ids)),
    ?assertNot(lists:member(call_manager, Ids)),
    ?assertNot(lists:member(push, Ids)),
    persistent_term:erase({fluxer_gateway, runtime_config}).

init_guilds_role_includes_only_guild_state_children_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{cluster_enabled => true, gateway_role => guilds}
    ),
    {ok, {_SupFlags, Children}} = fluxer_gateway_sup:init([]),
    Ids = child_ids(Children),
    ?assert(lists:member(presence_bus, Ids)),
    ?assert(lists:member(guild_manager, Ids)),
    ?assert(lists:member(guild_counts_cache, Ids)),
    ?assert(lists:member(gateway_clock_offset, Ids)),
    ?assert(lists:member(voice_state_counts_sync, Ids)),
    ?assert(lists:member(gateway_cluster_handoff, Ids)),
    ?assertNot(lists:member(session_manager, Ids)),
    ?assertNot(lists:member(presence_manager, Ids)),
    ?assertNot(lists:member(call_manager, Ids)),
    ?assertNot(lists:member(push, Ids)),
    persistent_term:erase({fluxer_gateway, runtime_config}).

init_sessions_role_includes_cluster_handoff_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{cluster_enabled => true, gateway_role => sessions}
    ),
    {ok, {_SupFlags, Children}} = fluxer_gateway_sup:init([]),
    Ids = child_ids(Children),
    ?assert(lists:member(session_manager, Ids)),
    ?assert(lists:member(session_state_transfer, Ids)),
    ?assert(lists:member(gateway_broadcast, Ids)),
    ?assert(lists:member(gateway_cluster_handoff, Ids)),
    ?assertNot(lists:member(guild_manager, Ids)),
    ?assertNot(lists:member(gateway_clock_offset, Ids)),
    ?assertNot(lists:member(presence_manager, Ids)),
    persistent_term:erase({fluxer_gateway, runtime_config}).

init_presence_role_includes_cluster_handoff_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{cluster_enabled => true, gateway_role => presence}
    ),
    {ok, {_SupFlags, Children}} = fluxer_gateway_sup:init([]),
    Ids = child_ids(Children),
    ?assert(lists:member(presence_manager, Ids)),
    ?assert(lists:member(presence_cache, Ids)),
    ?assert(lists:member(gateway_cluster_handoff, Ids)),
    ?assertNot(lists:member(voice_state_counts_sync, Ids)),
    ?assertNot(lists:member(session_manager, Ids)),
    ?assertNot(lists:member(guild_manager, Ids)),
    persistent_term:erase({fluxer_gateway, runtime_config}).

init_calls_role_includes_voice_state_counts_sync_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{cluster_enabled => true, gateway_role => calls}
    ),
    {ok, {_SupFlags, Children}} = fluxer_gateway_sup:init([]),
    Ids = child_ids(Children),
    ?assert(lists:member(call_manager, Ids)),
    ?assert(lists:member(voice_state_counts_sync, Ids)),
    ?assert(lists:member(gateway_cluster_handoff, Ids)),
    ?assertNot(lists:member(guild_manager, Ids)),
    ?assertNot(lists:member(presence_manager, Ids)),
    persistent_term:erase({fluxer_gateway, runtime_config}).

init_push_role_includes_cluster_handoff_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{cluster_enabled => true, gateway_role => push}
    ),
    {ok, {_SupFlags, Children}} = fluxer_gateway_sup:init([]),
    Ids = child_ids(Children),
    ?assert(lists:member(push_outbox, Ids)),
    ?assert(lists:member(push, Ids)),
    ?assert(lists:member(gateway_cluster_handoff, Ids)),
    ?assertNot(lists:member(session_manager, Ids)),
    ?assertNot(lists:member(guild_manager, Ids)),
    persistent_term:erase({fluxer_gateway, runtime_config}).

init_includes_cluster_children_when_flag_set_test() ->
    persistent_term:put(
        {fluxer_gateway, runtime_config},
        #{cluster_enabled => true}
    ),
    {ok, {_SupFlags, Children}} = fluxer_gateway_sup:init([]),
    Ids = child_ids(Children),
    ?assert(lists:member(gateway_cluster_discovery, Ids)),
    ?assert(lists:member(gateway_cluster_membership, Ids)),
    ?assert(lists:member(gateway_pg_scope, Ids)),
    ?assert(lists:member(gateway_cluster_handoff, Ids)),
    DiscoveryIdx = index_of(gateway_cluster_discovery, Ids),
    MembershipIdx = index_of(gateway_cluster_membership, Ids),
    PgScopeIdx = index_of(gateway_pg_scope, Ids),
    PresenceBusIdx = index_of(presence_bus, Ids),
    GuildManagerIdx = index_of(guild_manager, Ids),
    HandoffAfterGuild = index_of_after(gateway_cluster_handoff, Ids, GuildManagerIdx),
    ?assert(DiscoveryIdx < MembershipIdx),
    ?assert(MembershipIdx < PgScopeIdx),
    ?assert(PgScopeIdx < PresenceBusIdx),
    ?assert(GuildManagerIdx < HandoffAfterGuild),
    ?assertEqual(length(Ids), length(lists:usort(Ids))),
    persistent_term:erase({fluxer_gateway, runtime_config}).

index_of(Item, List) ->
    index_of(Item, List, 1).
index_of(_Item, [], _Idx) -> not_found;
index_of(Item, [Item | _], Idx) -> Idx;
index_of(Item, [_ | Rest], Idx) -> index_of(Item, Rest, Idx + 1).

index_of_after(Item, List, AfterIdx) ->
    index_of_after(Item, List, AfterIdx, 1).
index_of_after(_Item, [], _AfterIdx, _Idx) ->
    not_found;
index_of_after(Item, [Item | _], AfterIdx, Idx) when Idx > AfterIdx -> Idx;
index_of_after(Item, [_ | Rest], AfterIdx, Idx) ->
    index_of_after(Item, Rest, AfterIdx, Idx + 1).

child_ids(Children) ->
    [child_id(Child) || Child <- Children].

child_id(#{id := Id}) -> Id;
child_id({Id, _Start, _Restart, _Shutdown, _Type, _Modules}) -> Id.
