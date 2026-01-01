%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(gateway_rpc_misc).

-export([execute_method/2, get_local_node_stats/0]).

execute_method(<<"process.memory_stats">>, Params) ->
    Limit =
        case maps:get(<<"limit">>, Params, undefined) of
            undefined ->
                100;
            LimitValue ->
                validation:snowflake_or_throw(<<"limit">>, LimitValue)
        end,

    Guilds = process_memory_stats:get_guild_memory_stats(Limit),
    #{<<"guilds">> => Guilds};
execute_method(<<"process.node_stats">>, _Params) ->
    get_local_node_stats().

get_local_node_stats() ->
    SessionCount =
        case gen_server:call(session_manager, get_global_count, 1000) of
            {ok, SC} -> SC;
            _ -> 0
        end,

    GuildCount =
        case gen_server:call(guild_manager, get_global_count, 1000) of
            {ok, GC} -> GC;
            _ -> 0
        end,

    PresenceCount =
        case gen_server:call(presence_manager, get_global_count, 1000) of
            {ok, PC} -> PC;
            _ -> 0
        end,

    CallCount =
        case gen_server:call(call_manager, get_global_count, 1000) of
            {ok, CC} -> CC;
            _ -> 0
        end,

    MemoryInfo = erlang:memory(),
    TotalMemory = proplists:get_value(total, MemoryInfo, 0),
    ProcessMemory = proplists:get_value(processes, MemoryInfo, 0),
    SystemMemory = proplists:get_value(system, MemoryInfo, 0),

    #{
        <<"status">> => <<"healthy">>,
        <<"sessions">> => SessionCount,
        <<"guilds">> => GuildCount,
        <<"presences">> => PresenceCount,
        <<"calls">> => CallCount,
        <<"memory">> => #{
            <<"total">> => TotalMemory,
            <<"processes">> => ProcessMemory,
            <<"system">> => SystemMemory
        },
        <<"process_count">> => erlang:system_info(process_count),
        <<"process_limit">> => erlang:system_info(process_limit),
        <<"uptime_seconds">> => element(1, erlang:statistics(wall_clock)) div 1000
    }.
