%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(fluxer_gateway_sup).
-typing([eqwalizer]).
-behaviour(supervisor).
-export([
    start_link/0, init/1, cluster_children/0, role_enabled/1, role_enabled/2, current_role/0
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok, Pid} -> {ok, Pid};
        ignore -> {error, ignore};
        {error, E} -> {error, E}
    end.

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    Children = common_children() ++ role_children(current_role()),
    {ok, {SupFlags, Children}}.

-spec common_children() -> [supervisor:child_spec()].
common_children() ->
    [
        child_spec(gateway_http_client, gateway_http_client),
        child_spec(guild_ets_owner, guild_ets_owner),
        child_spec(gateway_nats_rpc, gateway_nats_rpc),
        child_spec(gateway_nats_pool, gateway_nats_pool),
        child_spec(gateway_event_pause, gateway_event_pause),
        child_spec(gateway_concurrency, gateway_concurrency),
        child_spec(gateway_rollout_config, gateway_rollout_config)
    ] ++ cluster_children() ++
        [
            child_spec(gateway_dispatch_relay, gateway_dispatch_relay),
            child_spec(gateway_periodic_gc, gateway_periodic_gc),
            child_spec(process_health_watchdog, process_health_watchdog)
        ].

-spec role_children(atom()) -> [supervisor:child_spec()].
role_children(Role) ->
    RoleSpecs =
        presence_bus_children(Role) ++
            lists:append(
                [
                    role_child_specs(sessions, Role),
                    role_child_specs(presence, Role),
                    role_child_specs(guilds, Role),
                    role_child_specs(calls, Role),
                    role_child_specs(push, Role)
                ]
            ),
    RoleSpecs ++ role_handoff_children(Role).

-spec presence_bus_children(atom()) -> [supervisor:child_spec()].
presence_bus_children(Role) ->
    case role_enabled(presence, Role) orelse role_enabled(guilds, Role) of
        true ->
            [child_spec(presence_bus, presence_bus)];
        false ->
            []
    end.

-spec role_child_specs(atom(), atom()) -> [supervisor:child_spec()].
role_child_specs(RoleName, Role) ->
    case role_enabled(RoleName, Role) of
        true -> role_specs(RoleName, Role);
        false -> []
    end.

-spec role_specs(atom(), atom()) -> [supervisor:child_spec()].
role_specs(sessions, _Role) ->
    [
        child_spec(session_state_transfer, session_state_transfer),
        child_spec(session_manager, session_manager)
    ];
role_specs(presence, _Role) ->
    [
        child_spec(presence_cache, presence_cache),
        child_spec(presence_manager, presence_manager)
    ];
role_specs(guilds, _Role) ->
    [
        child_spec(guild_counts_cache, guild_counts_cache),
        child_spec(guild_manager, guild_manager),
        child_spec(voice_state_counts_sync, voice_state_counts_sync)
    ];
role_specs(calls, Role) ->
    [child_spec(call_manager, call_manager)] ++ calls_voice_state_counts_sync_children(Role);
role_specs(push, _Role) ->
    [
        child_spec(push_dispatcher, push_dispatcher),
        child_spec(push, push)
    ].

-spec role_handoff_children(atom()) -> [supervisor:child_spec()].
role_handoff_children(Role) ->
    case
        role_enabled(sessions, Role) orelse role_enabled(presence, Role) orelse
            role_enabled(guilds, Role) orelse role_enabled(calls, Role) orelse
            role_enabled(push, Role)
    of
        true -> cluster_handoff_children();
        false -> []
    end.

-spec calls_voice_state_counts_sync_children(atom()) -> [supervisor:child_spec()].
calls_voice_state_counts_sync_children(Role) ->
    case role_enabled(guilds, Role) of
        true -> [];
        false -> [child_spec(voice_state_counts_sync, voice_state_counts_sync)]
    end.

-spec current_role() -> all | websocket | sessions | presence | guilds | calls | push.
current_role() ->
    normalize_role(
        try fluxer_gateway_env:get(gateway_role) of
            Value -> Value
        catch
            throw:_Reason -> all;
            error:_Reason -> all;
            exit:_Reason -> all
        end
    ).

-spec role_enabled(atom()) -> boolean().
role_enabled(Role) ->
    role_enabled(Role, current_role()).

-spec role_enabled(atom(), atom()) -> boolean().
role_enabled(_Role, all) ->
    true;
role_enabled(Role, Role) ->
    true;
role_enabled(_Role, _CurrentRole) ->
    false.

-spec normalize_role(term()) -> all | websocket | sessions | presence | guilds | calls | push.
normalize_role(websocket) ->
    websocket;
normalize_role(sessions) ->
    sessions;
normalize_role(presence) ->
    presence;
normalize_role(guilds) ->
    guilds;
normalize_role(calls) ->
    calls;
normalize_role(push) ->
    push;
normalize_role(all) ->
    all;
normalize_role(undefined) ->
    all;
normalize_role(<<"websocket">>) ->
    websocket;
normalize_role(<<"sessions">>) ->
    sessions;
normalize_role(<<"presence">>) ->
    presence;
normalize_role(<<"guilds">>) ->
    guilds;
normalize_role(<<"calls">>) ->
    calls;
normalize_role(<<"push">>) ->
    push;
normalize_role(<<"all">>) ->
    all;
normalize_role(Value) when is_list(Value) ->
    normalize_role(type_conv:ensure_binary(Value));
normalize_role(_) ->
    websocket.

-spec cluster_children() -> [supervisor:child_spec()].
cluster_children() ->
    case fluxer_gateway_env:get(cluster_enabled) of
        true ->
            [
                child_spec(gateway_cluster_discovery, gateway_cluster_discovery),
                child_spec(gateway_cluster_membership, gateway_cluster_membership),
                child_spec(gateway_pg_scope, gateway_pg_scope)
            ];
        _ ->
            []
    end.

-spec cluster_handoff_children() -> [supervisor:child_spec()].
cluster_handoff_children() ->
    case fluxer_gateway_env:get(cluster_enabled) of
        true -> [child_spec(gateway_cluster_handoff, gateway_cluster_handoff)];
        _ -> []
    end.

-spec child_spec(atom(), module()) -> supervisor:child_spec().
child_spec(Id, Module) ->
    #{
        id => Id,
        start => {Module, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    }.

-ifdef(TEST).

guild_role_starts_ets_owner_before_guild_workers_test() ->
    Ids = init_child_ids(#{cluster_enabled => true, gateway_role => guilds}),
    ?assert(lists:member(guild_ets_owner, Ids)),
    ?assert(owner_precedes(guild_ets_owner, gateway_nats_rpc, Ids)),
    ?assert(owner_precedes(guild_ets_owner, guild_manager, Ids)),
    ?assert(owner_precedes(guild_ets_owner, voice_state_counts_sync, Ids)).

calls_role_starts_ets_owner_before_call_workers_test() ->
    Ids = init_child_ids(#{cluster_enabled => true, gateway_role => calls}),
    ?assert(lists:member(guild_ets_owner, Ids)),
    ?assert(owner_precedes(guild_ets_owner, call_manager, Ids)),
    ?assert(owner_precedes(guild_ets_owner, voice_state_counts_sync, Ids)).

all_role_starts_single_ets_owner_test() ->
    Ids = init_child_ids(#{cluster_enabled => true, gateway_role => all}),
    ?assertEqual(1, count_id(guild_ets_owner, Ids)).

sessions_role_starts_ets_owner_before_session_workers_test() ->
    Ids = init_child_ids(#{cluster_enabled => true, gateway_role => sessions}),
    ?assert(lists:member(guild_ets_owner, Ids)),
    ?assert(owner_precedes(guild_ets_owner, session_manager, Ids)).

websocket_role_starts_ets_owner_before_rpc_workers_test() ->
    Ids = init_child_ids(#{cluster_enabled => true, gateway_role => websocket}),
    ?assert(lists:member(guild_ets_owner, Ids)),
    ?assert(owner_precedes(guild_ets_owner, gateway_nats_rpc, Ids)).

init_child_ids(Config) ->
    persistent_term:put({fluxer_gateway, runtime_config}, Config),
    try
        {ok, {_SupFlags, Children}} = init([]),
        [maps:get(id, Child) || Child <- Children, is_map(Child)]
    after
        persistent_term:erase({fluxer_gateway, runtime_config})
    end.

owner_precedes(Owner, Worker, Ids) ->
    index_of(Owner, Ids) < index_of(Worker, Ids).

index_of(Id, Ids) ->
    index_of(Id, Ids, 1).

index_of(Id, [Id | _Rest], Index) ->
    Index;
index_of(Id, [_Other | Rest], Index) ->
    index_of(Id, Rest, Index + 1);
index_of(_Id, [], _Index) ->
    1000000.

count_id(Id, Ids) ->
    length([Item || Item <- Ids, Item =:= Id]).

-endif.
