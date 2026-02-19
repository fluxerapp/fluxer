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

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    Children = [
        child_spec(gateway_http_client, gateway_http_client),
        child_spec(gateway_nats_rpc, gateway_nats_rpc),
        child_spec(session_manager, session_manager),
        child_spec(presence_cache, presence_cache),
        child_spec(presence_bus, presence_bus),
        child_spec(presence_manager, presence_manager),
        child_spec(guild_crash_logger, guild_crash_logger),
        child_spec(guild_manager, guild_manager),
        child_spec(call_manager, call_manager),
        child_spec(push_dispatcher, push_dispatcher),
        child_spec(push, push),
        child_spec(gateway_metrics_collector, gateway_metrics_collector)
    ],
    {ok, {SupFlags, Children}}.

-spec child_spec(atom(), module()) -> supervisor:child_spec().
child_spec(Id, Module) ->
    #{
        id => Id,
        start => {Module, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    }.
