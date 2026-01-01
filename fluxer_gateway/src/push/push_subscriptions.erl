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

-module(push_subscriptions).

 -export([fetch_and_send_subscriptions/9]).
-export([fetch_and_cache_user_guild_settings/3]).
-export([delete_failed_subscriptions/1]).

fetch_and_send_subscriptions(
    UserIds,
    MessageData,
    GuildId,
    ChannelId,
    MessageId,
    GuildName,
    ChannelName,
    State,
    BadgeCounts
) ->
    SubscriptionsReq = #{
        <<"type">> => <<"get_push_subscriptions">>,
        <<"user_ids">> => [integer_to_binary(UserId) || UserId <- UserIds]
    },

    case rpc_client:call(SubscriptionsReq) of
        {ok, SubscriptionsData} ->
            NewState = lists:foldl(
                fun(UserId, S) ->
                    UserIdBin = integer_to_binary(UserId),
                    case maps:get(UserIdBin, SubscriptionsData, []) of
                        [] ->
                            push_cache:cache_user_subscriptions(UserId, [], S);
                        Subscriptions ->
                            BadgeCount = maps:get(UserId, BadgeCounts, 0),
                            push_sender:send_to_user_subscriptions(
                                UserId,
                                Subscriptions,
                                MessageData,
                                GuildId,
                                ChannelId,
                                MessageId,
                                GuildName,
                                ChannelName,
                                BadgeCount
                            ),
                            push_cache:cache_user_subscriptions(UserId, Subscriptions, S)
                    end
                end,
                State,
                UserIds
            ),
            NewState;
        {error, _Reason} ->
            State
    end.

fetch_and_cache_user_guild_settings(UserId, GuildId, _State) ->
    Req = #{
        <<"type">> => <<"get_user_guild_settings">>,
        <<"user_ids">> => [integer_to_binary(UserId)],
        <<"guild_id">> => integer_to_binary(GuildId)
    },

    case rpc_client:call(Req) of
        {ok, Data} ->
            UserGuildSettings = maps:get(<<"user_guild_settings">>, Data, [null]),
            [SettingsData | _] = UserGuildSettings,
            case SettingsData of
                null ->
                    null;
                Settings ->
                    gen_server:cast(
                        push, {cache_user_guild_settings, UserId, GuildId, Settings}
                    ),
                    Settings
            end;
        {error, Reason} ->
            null
    end.

delete_failed_subscriptions(FailedSubscriptions) ->
    DeleteReq = #{
        <<"type">> => <<"delete_push_subscriptions">>,
        <<"subscriptions">> => FailedSubscriptions
    },

    rpc_client:call(DeleteReq).
