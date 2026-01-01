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

-module(push_core).

-export([handle_message_create/1]).
-export([sync_user_guild_settings/3]).
-export([sync_user_blocked_ids/2]).

handle_message_create(Params) ->
    PushEnabled = fluxer_gateway_env:get(push_enabled),
    case PushEnabled of
        true ->
            gen_server:cast(push, {handle_message_create, Params});
        false ->
            ok
    end.

sync_user_guild_settings(UserId, GuildId, UserGuildSettings) ->
    gen_server:cast(push, {sync_user_guild_settings, UserId, GuildId, UserGuildSettings}).

sync_user_blocked_ids(UserId, BlockedIds) ->
    gen_server:cast(push, {sync_user_blocked_ids, UserId, BlockedIds}).
