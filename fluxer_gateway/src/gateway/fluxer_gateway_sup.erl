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

-module(fluxer_gateway_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SessionManager = #{
        id => session_manager,
        start => {session_manager, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    PresenceManager = #{
        id => presence_manager,
        start => {presence_manager, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    GuildManager = #{
        id => guild_manager,
        start => {guild_manager, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    Push = #{
        id => push,
        start => {push, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    CallManager = #{
        id => call_manager,
        start => {call_manager, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    PresenceBus = #{
        id => presence_bus,
        start => {presence_bus, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    PresenceCache = #{
        id => presence_cache,
        start => {presence_cache, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    GatewayMetricsCollector = #{
        id => gateway_metrics_collector,
        start => {gateway_metrics_collector, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    {ok,
        {{one_for_one, 5, 10}, [
            SessionManager,
            PresenceCache,
            PresenceBus,
            PresenceManager,
            GuildManager,
            CallManager,
            Push,
            GatewayMetricsCollector
        ]}}.
