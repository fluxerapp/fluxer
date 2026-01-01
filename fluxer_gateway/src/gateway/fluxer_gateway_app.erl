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

-module(fluxer_gateway_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    fluxer_gateway_env:load(),

    WsPort = fluxer_gateway_env:get(ws_port),
    RpcPort = fluxer_gateway_env:get(rpc_port),

    Dispatch = cowboy_router:compile([
        {'_', [
            {<<"/_health">>, health_handler, []},
            {<<"/">>, gateway_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(http, [{port, WsPort}], #{
        env => #{dispatch => Dispatch},
        max_frame_size => 4096
    }),

    RpcDispatch = cowboy_router:compile([
        {'_', [
            {<<"/_rpc">>, gateway_rpc_http_handler, []},
            {<<"/_admin/reload">>, hot_reload_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(rpc_http, [{port, RpcPort}], #{
        env => #{dispatch => RpcDispatch}
    }),

    fluxer_gateway_sup:start_link().

stop(_State) ->
    ok.
