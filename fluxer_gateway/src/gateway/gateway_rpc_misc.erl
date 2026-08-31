%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_misc).
-typing([eqwalizer]).

-export([
    execute_method/2,
    get_local_node_id/0,
    get_local_node_stats/0,
    get_local_memory_stats/1,
    get_local_voice_state_counts/0,
    get_local_active_voice_rooms/0
]).

-define(DEFAULT_MEMORY_STATS_LIMIT, 100).
-define(MAX_MEMORY_STATS_LIMIT, 500).

-spec execute_method(binary(), map()) -> map().
execute_method(<<"process.memory_stats">>, Params) ->
    Limit = parse_memory_stats_limit(Params),
    ActiveNodes = gateway_node_router:active_nodes(),
    Guilds = gateway_rpc_misc_session:collect_and_limit_memory_stats(ActiveNodes, Limit),
    GuildsWithStringMemory = [
        G#{memory := integer_to_binary(maps:get(memory, G))}
     || G <- Guilds
    ],
    #{<<"guilds">> => GuildsWithStringMemory};
execute_method(<<"process.node_stats">>, _Params) ->
    ActiveNodes = gateway_node_router:active_nodes(),
    gateway_rpc_misc_session:collect_and_aggregate_node_stats(ActiveNodes);
execute_method(<<"process.voice_state_counts">>, _Params) ->
    ActiveNodes = voice_state_count_nodes(),
    gateway_rpc_misc_presence:collect_and_aggregate_voice_state_counts(ActiveNodes);
execute_method(<<"process.active_voice_rooms">>, _Params) ->
    ActiveNodes = gateway_node_router:active_nodes(),
    gateway_rpc_misc_push:collect_and_aggregate_active_voice_rooms(ActiveNodes);
execute_method(Method, _Params) ->
    gateway_rpc_error:raise(<<"Unknown method: ", Method/binary>>).

-spec get_local_node_id() -> binary().
get_local_node_id() ->
    gateway_rpc_misc_session:get_local_node_id().

-spec get_local_node_stats() -> map().
get_local_node_stats() ->
    gateway_rpc_misc_session:get_local_node_stats().

-spec get_local_memory_stats(pos_integer()) -> [map()].
get_local_memory_stats(Limit) ->
    gateway_rpc_misc_session:get_local_memory_stats(Limit).

-spec get_local_voice_state_counts() -> map().
get_local_voice_state_counts() ->
    gateway_rpc_misc_presence:get_local_voice_state_counts().

-spec get_local_active_voice_rooms() -> map().
get_local_active_voice_rooms() ->
    gateway_rpc_misc_push:get_local_active_voice_rooms().

-spec parse_memory_stats_limit(map()) -> pos_integer().
parse_memory_stats_limit(Params) ->
    Limit =
        case maps:get(<<"limit">>, Params, undefined) of
            undefined -> ?DEFAULT_MEMORY_STATS_LIMIT;
            LimitValue -> validation:snowflake_or_throw(<<"limit">>, LimitValue)
        end,
    min(Limit, ?MAX_MEMORY_STATS_LIMIT).

-spec voice_state_count_nodes() -> [node()].
voice_state_count_nodes() ->
    lists:sort(
        lists:usort(
            gateway_node_router:active_nodes(guilds) ++
                gateway_node_router:active_nodes(calls)
        )
    ).
