%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_misc_session).
-typing([eqwalizer]).

-export([
    get_local_node_id/0,
    get_local_node_stats/0,
    get_local_memory_stats/1,
    collect_and_aggregate_node_stats/1,
    collect_and_limit_memory_stats/2,
    safe_node_call/4,
    normalize_nodes/1,
    node_id/1,
    decode_integer/1,
    compare_node_ids/2
]).

-define(NODE_RPC_TIMEOUT, 10000).
-define(MEMORY_RPC_TIMEOUT, 5000).
-define(MANAGER_COUNT_TIMEOUT, 5000).

-spec get_local_node_id() -> binary().
get_local_node_id() ->
    case local_hostname() of
        undefined -> erlang_node_id(node());
        Hostname -> Hostname
    end.

-spec get_local_node_stats() -> map().
get_local_node_stats() ->
    SessionCount = get_manager_count(session_manager),
    GuildCount = get_manager_count(guild_manager),
    PresenceCount = get_manager_count(presence_manager),
    CallCount = get_manager_count(call_manager),
    MemoryInfo = erlang:memory(),
    TotalMemory = decode_integer(proplists:get_value(total, MemoryInfo, undefined)),
    ProcessMemory = decode_integer(proplists:get_value(processes, MemoryInfo, undefined)),
    SystemMemory = decode_integer(proplists:get_value(system, MemoryInfo, undefined)),
    #{
        <<"node_id">> => node_id(node()),
        <<"status">> => <<"healthy">>,
        <<"sessions">> => SessionCount,
        <<"guilds">> => GuildCount,
        <<"presences">> => PresenceCount,
        <<"calls">> => CallCount,
        <<"memory">> => #{
            <<"total">> => integer_to_binary(TotalMemory),
            <<"processes">> => integer_to_binary(ProcessMemory),
            <<"system">> => integer_to_binary(SystemMemory)
        },
        <<"process_count">> => erlang:system_info(process_count),
        <<"process_limit">> => erlang:system_info(process_limit),
        <<"uptime_seconds">> => element(1, erlang:statistics(wall_clock)) div 1000,
        <<"cluster_metrics">> => gateway_cluster_metrics:snapshot()
    }.

-spec get_local_memory_stats(pos_integer()) -> [map()].
get_local_memory_stats(Limit) -> process_memory_stats:get_guild_memory_stats(Limit).

-spec collect_and_aggregate_node_stats([node()]) -> map().
collect_and_aggregate_node_stats(Nodes) ->
    aggregate_node_stats([fetch_node_stats(N) || N <- normalize_nodes(Nodes)]).

-spec collect_and_limit_memory_stats([node()], pos_integer()) -> [map()].
collect_and_limit_memory_stats(Nodes, Limit) ->
    Guilds = lists:flatmap(
        fun(N) -> fetch_node_memory_stats(N, Limit) end, normalize_nodes(Nodes)
    ),
    lists:sublist(lists:sort(fun compare_guild_memory/2, Guilds), Limit).

-spec safe_node_call(node(), atom(), [term()], timeout()) -> term().
safe_node_call(TargetNode, Function, Args, _Timeout) when TargetNode =:= node() ->
    try apply(gateway_rpc_misc, Function, Args) of
        Response -> Response
    catch
        _:_ -> error
    end;
safe_node_call(TargetNode, Function, Args, Timeout) ->
    try rpc:call(TargetNode, gateway_rpc_misc, Function, Args, Timeout) of
        {badrpc, _} -> error;
        Response -> Response
    catch
        _:_ -> error
    end.

-spec normalize_nodes([term()]) -> [node()].
normalize_nodes(Nodes) ->
    ValidNodes = valid_nodes(Nodes, []),
    lists:sort(lists:usort([node() | ValidNodes])).

-spec valid_nodes([term()], [node()]) -> [node()].
valid_nodes([], Acc) ->
    Acc;
valid_nodes([NodeValue | Rest], Acc) when is_atom(NodeValue) ->
    case lists:member($@, atom_to_list(NodeValue)) of
        true -> valid_nodes(Rest, [NodeValue | Acc]);
        false -> valid_nodes(Rest, Acc)
    end;
valid_nodes([_NodeValue | Rest], Acc) ->
    valid_nodes(Rest, Acc).

-spec node_id(node()) -> binary().
node_id(TargetNode) when TargetNode =:= node() ->
    get_local_node_id();
node_id(TargetNode) ->
    case safe_node_call(TargetNode, get_local_node_id, [], ?NODE_RPC_TIMEOUT) of
        NodeId when is_binary(NodeId), NodeId =/= <<>> -> NodeId;
        _ -> erlang_node_id(TargetNode)
    end.

-spec decode_integer(term()) -> non_neg_integer().
decode_integer(Value) ->
    case maybe_integer(Value) of
        Integer when is_integer(Integer), Integer >= 0 -> Integer;
        _ -> 0
    end.

-spec maybe_integer(term()) -> integer() | undefined.
maybe_integer(Value) ->
    type_conv:to_integer(Value).

-spec compare_node_ids(map(), map()) -> boolean().
compare_node_ids(Left, Right) ->
    maps:get(<<"node_id">>, Left, <<>>) =< maps:get(<<"node_id">>, Right, <<>>).

-spec erlang_node_id(node()) -> binary().
erlang_node_id(TargetNode) -> atom_to_binary(TargetNode, utf8).

-spec local_hostname() -> binary() | undefined.
local_hostname() ->
    case os:getenv("HOSTNAME") of
        false -> undefined;
        "" -> undefined;
        Hostname -> trimmed_hostname(Hostname)
    end.

-spec trimmed_hostname(string()) -> binary() | undefined.
trimmed_hostname(Hostname) ->
    case string:trim(Hostname) of
        "" -> undefined;
        Trimmed -> hostname_binary(Trimmed)
    end.

-spec hostname_binary(term()) -> binary() | undefined.
hostname_binary(Value) ->
    type_conv:unicode_to_binary(Value).

-spec get_manager_count(atom()) -> non_neg_integer().
get_manager_count(Manager) ->
    try gen_server:call(Manager, get_global_count, ?MANAGER_COUNT_TIMEOUT) of
        {ok, Count} -> Count;
        _ -> 0
    catch
        _:_ -> 0
    end.

-spec fetch_node_stats(node()) -> map().
fetch_node_stats(TargetNode) ->
    case safe_node_call(TargetNode, get_local_node_stats, [], ?NODE_RPC_TIMEOUT) of
        Stats when is_map(Stats) ->
            Stats#{<<"node_id">> => node_id(TargetNode)};
        _ ->
            unavailable_node_stats(TargetNode)
    end.

-spec fetch_node_memory_stats(node(), pos_integer()) -> [map()].
fetch_node_memory_stats(TargetNode, Limit) ->
    case safe_node_call(TargetNode, get_local_memory_stats, [Limit], ?MEMORY_RPC_TIMEOUT) of
        Guilds when is_list(Guilds) ->
            TargetNodeId = node_id(TargetNode),
            [Guild#{node_id => TargetNodeId} || Guild <- Guilds, is_map(Guild)];
        _ ->
            []
    end.

-spec aggregate_node_stats([map()]) -> map().
aggregate_node_stats(NodeStats) ->
    SortedNodes = lists:sort(fun compare_node_ids/2, NodeStats),
    #{
        <<"status">> => aggregate_status(SortedNodes),
        <<"sessions">> => sum_stat(SortedNodes, <<"sessions">>),
        <<"guilds">> => sum_stat(SortedNodes, <<"guilds">>),
        <<"presences">> => sum_stat(SortedNodes, <<"presences">>),
        <<"calls">> => sum_stat(SortedNodes, <<"calls">>),
        <<"memory">> => #{
            <<"total">> => integer_to_binary(sum_memory_stat(SortedNodes, <<"total">>)),
            <<"processes">> => integer_to_binary(sum_memory_stat(SortedNodes, <<"processes">>)),
            <<"system">> => integer_to_binary(sum_memory_stat(SortedNodes, <<"system">>))
        },
        <<"process_count">> => sum_stat(SortedNodes, <<"process_count">>),
        <<"process_limit">> => sum_stat(SortedNodes, <<"process_limit">>),
        <<"uptime_seconds">> => aggregate_uptime(SortedNodes),
        <<"node_count">> => length(SortedNodes),
        <<"nodes">> => SortedNodes
    }.

-spec aggregate_status([map()]) -> binary().
aggregate_status(NodeStats) ->
    AllHealthy = lists:all(
        fun(N) -> maps:get(<<"status">>, N, <<"healthy">>) =:= <<"healthy">> end,
        NodeStats
    ),
    case AllHealthy of
        true -> <<"healthy">>;
        false -> <<"degraded">>
    end.

-spec aggregate_uptime([map()]) -> non_neg_integer().
aggregate_uptime([]) ->
    0;
aggregate_uptime(NodeStats) ->
    Uptimes = [
        U
     || N <- NodeStats,
        U <- [maybe_integer(maps:get(<<"uptime_seconds">>, N, undefined))],
        is_integer(U),
        U >= 0
    ],
    case Uptimes of
        [] -> 0;
        _ -> lists:min(Uptimes)
    end.

-spec sum_stat([map()], binary()) -> non_neg_integer().
sum_stat(NodeStats, Key) ->
    trunc(
        lists:foldl(
            fun(Node, Acc) ->
                Acc + decode_integer(maps:get(Key, Node, undefined))
            end,
            0,
            NodeStats
        )
    ).

-spec sum_memory_stat([map()], binary()) -> non_neg_integer().
sum_memory_stat(NodeStats, MemoryKey) ->
    trunc(
        lists:foldl(
            fun(Node, Acc) ->
                Acc +
                    decode_integer(
                        maps:get(MemoryKey, maps:get(<<"memory">>, Node, #{}), undefined)
                    )
            end,
            0,
            NodeStats
        )
    ).

-spec compare_guild_memory(map(), map()) -> boolean().
compare_guild_memory(LeftGuild, RightGuild) ->
    LeftMemory = decode_integer(maps:get(memory, LeftGuild, undefined)),
    RightMemory = decode_integer(maps:get(memory, RightGuild, undefined)),
    case LeftMemory =:= RightMemory of
        true ->
            maps:get(guild_id, LeftGuild, null) =< maps:get(guild_id, RightGuild, null);
        false ->
            LeftMemory > RightMemory
    end.

-spec unavailable_node_stats(node()) -> map().
unavailable_node_stats(TargetNode) ->
    #{
        <<"node_id">> => node_id(TargetNode),
        <<"status">> => <<"unavailable">>,
        <<"sessions">> => null,
        <<"guilds">> => null,
        <<"presences">> => null,
        <<"calls">> => null,
        <<"memory">> => #{
            <<"total">> => null,
            <<"processes">> => null,
            <<"system">> => null
        },
        <<"process_count">> => null,
        <<"process_limit">> => null,
        <<"uptime_seconds">> => null
    }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

normalize_nodes_includes_local_and_filters_invalid_values_test() ->
    L = node(),
    O = 'gateway_b@127.0.0.1',
    ?assertEqual(lists:sort(lists:usort([L, O])), normalize_nodes([O, bad_node, O, L, 42])).

collect_cluster_memory_stats_sorts_globally_and_applies_limit_test() ->
    L = node(),
    O = 'gateway_b@127.0.0.1',
    LId = node_id(L),
    OId = node_id(O),
    F = fun
        (N, _) when N =:= L ->
            [
                #{guild_id => <<"1">>, memory => 100, node_id => LId},
                #{guild_id => <<"2">>, memory => 300, node_id => LId}
            ];
        (N, _) when N =:= O ->
            [
                #{guild_id => <<"3">>, memory => 250, node_id => OId},
                #{guild_id => <<"4">>, memory => 200, node_id => OId}
            ]
    end,
    All = lists:flatmap(fun(N) -> F(N, 3) end, normalize_nodes([L, O])),
    Guilds = lists:sublist(lists:sort(fun compare_guild_memory/2, All), 3),
    ?assertEqual([300, 250, 200], [maps:get(memory, G) || G <- Guilds]).

aggregate_node_stats_sums_cluster_totals_test() ->
    Mem1 = #{<<"total">> => <<"100">>, <<"processes">> => <<"60">>, <<"system">> => <<"40">>},
    Mem2 = #{<<"total">> => <<"30">>, <<"processes">> => <<"10">>, <<"system">> => <<"20">>},
    N1 = #{
        <<"node_id">> => <<"gateway_a@127.0.0.1">>,
        <<"status">> => <<"healthy">>,
        <<"sessions">> => 10,
        <<"guilds">> => 20,
        <<"presences">> => 30,
        <<"calls">> => 40,
        <<"memory">> => Mem1,
        <<"process_count">> => 50,
        <<"process_limit">> => 1000,
        <<"uptime_seconds">> => 120
    },
    N2 = #{
        <<"node_id">> => <<"gateway_b@127.0.0.1">>,
        <<"status">> => <<"healthy">>,
        <<"sessions">> => 3,
        <<"guilds">> => 4,
        <<"presences">> => 5,
        <<"calls">> => 6,
        <<"memory">> => Mem2,
        <<"process_count">> => 7,
        <<"process_limit">> => 2000,
        <<"uptime_seconds">> => 90
    },
    A = aggregate_node_stats([N1, N2]),
    ?assertEqual(13, maps:get(<<"sessions">>, A)),
    ?assertEqual(24, maps:get(<<"guilds">>, A)),
    ?assertEqual(35, maps:get(<<"presences">>, A)),
    ?assertEqual(46, maps:get(<<"calls">>, A)),
    ?assertEqual(57, maps:get(<<"process_count">>, A)),
    ?assertEqual(3000, maps:get(<<"process_limit">>, A)),
    ?assertEqual(90, maps:get(<<"uptime_seconds">>, A)),
    ?assertEqual(2, maps:get(<<"node_count">>, A)),
    ?assertEqual(<<"130">>, maps:get(<<"total">>, maps:get(<<"memory">>, A))),
    ?assertEqual(<<"70">>, maps:get(<<"processes">>, maps:get(<<"memory">>, A))),
    ?assertEqual(<<"60">>, maps:get(<<"system">>, maps:get(<<"memory">>, A))),
    ?assertEqual(2, length(maps:get(<<"nodes">>, A))).

aggregate_node_stats_ignores_unknown_uptime_test() ->
    NodeA = #{<<"node_id">> => <<"a">>, <<"uptime_seconds">> => null},
    NodeB = #{<<"node_id">> => <<"b">>, <<"uptime_seconds">> => 5},
    A = aggregate_node_stats([NodeA, NodeB]),
    ?assertEqual(5, maps:get(<<"uptime_seconds">>, A)).

safe_node_call_catches_local_errors_test() ->
    ?assertEqual(error, safe_node_call(node(), definitely_missing_function, [], 100)).

collect_and_aggregate_node_stats_with_empty_node_list_reports_local_node_test() ->
    A = collect_and_aggregate_node_stats([]),
    ?assertEqual(1, maps:get(<<"node_count">>, A)),
    ?assertEqual(<<"healthy">>, maps:get(<<"status">>, A)),
    ?assertEqual(1, length(maps:get(<<"nodes">>, A))).

-endif.
