%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(voice_pending_common).
-typing([eqwalizer]).

-export([
    add_pending_connection/3,
    remove_pending_connection/2,
    get_pending_connection/2,
    confirm_pending_connection/2
]).

-export_type([
    connection_id/0,
    pending_metadata/0,
    pending_map/0
]).

-type connection_id() :: binary().
-type pending_metadata() :: map().
-type pending_map() :: #{connection_id() => pending_metadata()}.

-spec add_pending_connection(connection_id(), pending_metadata(), pending_map()) ->
    pending_map().
add_pending_connection(ConnectionId, Metadata, PendingMap) ->
    PendingMap#{ConnectionId => Metadata#{joined_at => erlang:system_time(millisecond)}}.

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
