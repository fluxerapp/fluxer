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

-module(voice_pending_common).

-export([
    add_pending_connection/3,
    remove_pending_connection/2,
    get_pending_connection/2,
    confirm_pending_connection/2
]).

-type connection_id() :: binary().
-type pending_metadata() :: map().
-type pending_map() :: #{connection_id() => pending_metadata()}.

-spec add_pending_connection(connection_id(), pending_metadata(), pending_map()) -> pending_map().
add_pending_connection(ConnectionId, Metadata, PendingMap) ->
    maps:put(ConnectionId, Metadata#{joined_at => erlang:system_time(millisecond)}, PendingMap).

-spec remove_pending_connection(connection_id() | undefined, pending_map()) -> pending_map().
remove_pending_connection(undefined, PendingMap) ->
    PendingMap;
remove_pending_connection(ConnectionId, PendingMap) ->
    maps:remove(ConnectionId, PendingMap).

-spec get_pending_connection(connection_id() | undefined, pending_map()) ->
    pending_metadata() | undefined.
get_pending_connection(undefined, _PendingMap) ->
    undefined;
get_pending_connection(ConnectionId, PendingMap) ->
    maps:get(ConnectionId, PendingMap, undefined).

-spec confirm_pending_connection(connection_id() | undefined, pending_map()) ->
    {confirmed, pending_map()} | {not_found, pending_map()}.
confirm_pending_connection(undefined, PendingMap) ->
    {not_found, PendingMap};
confirm_pending_connection(ConnectionId, PendingMap) ->
    case maps:get(ConnectionId, PendingMap, undefined) of
        undefined ->
            {not_found, PendingMap};
        _Metadata ->
            {confirmed, maps:remove(ConnectionId, PendingMap)}
    end.
