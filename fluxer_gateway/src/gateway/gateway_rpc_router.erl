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

-module(gateway_rpc_router).

-export([execute/2]).

execute(Method, Params) ->
    case Method of
        <<"guild.", _/binary>> ->
            gateway_rpc_guild:execute_method(Method, Params);
        <<"presence.", _/binary>> ->
            gateway_rpc_presence:execute_method(Method, Params);
        <<"push.", _/binary>> ->
            gateway_rpc_push:execute_method(Method, Params);
        <<"call.", _/binary>> ->
            gateway_rpc_call:execute_method(Method, Params);
        <<"process.", _/binary>> ->
            gateway_rpc_misc:execute_method(Method, Params);
        _ ->
            throw({error, <<"Unknown method: ", Method/binary>>})
    end.
