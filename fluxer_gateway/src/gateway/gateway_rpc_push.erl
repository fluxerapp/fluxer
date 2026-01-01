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

-module(gateway_rpc_push).

-export([execute_method/2]).

execute_method(<<"push.sync_user_guild_settings">>, #{
    <<"user_id">> := UserIdBin,
    <<"guild_id">> := GuildIdBin,
    <<"user_guild_settings">> := UserGuildSettings
}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    push:sync_user_guild_settings(UserId, GuildId, UserGuildSettings),
    true;
execute_method(<<"push.sync_user_blocked_ids">>, #{
    <<"user_id">> := UserIdBin, <<"blocked_user_ids">> := BlockedUserIds
}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    BlockedIds = validation:snowflake_list_or_throw(<<"blocked_user_ids">>, BlockedUserIds),
    push:sync_user_blocked_ids(UserId, BlockedIds),
    true;
execute_method(<<"push.invalidate_badge_count">>, #{
    <<"user_id">> := UserIdBin
}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    push:invalidate_user_badge_count(UserId),
    true.
