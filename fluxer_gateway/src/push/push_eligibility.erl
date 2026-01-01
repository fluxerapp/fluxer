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

-module(push_eligibility).
-export([is_eligible_for_push/8]).
-export([is_user_blocked/3]).
-export([check_user_guild_settings/7]).
-export([should_allow_notification/5]).
-export([is_user_mentioned/4]).

-define(LARGE_GUILD_THRESHOLD, 250).
-define(LARGE_GUILD_OVERRIDE_FEATURE, <<"LARGE_GUILD_OVERRIDE">>).

-define(MESSAGE_NOTIFICATIONS_NULL, -1).
-define(MESSAGE_NOTIFICATIONS_ALL, 0).
-define(MESSAGE_NOTIFICATIONS_ONLY_MENTIONS, 1).
-define(MESSAGE_NOTIFICATIONS_NO_MESSAGES, 2).
-define(MESSAGE_NOTIFICATIONS_INHERIT, 3).

-define(CHANNEL_TYPE_DM, 1).
-define(CHANNEL_TYPE_GROUP_DM, 3).

is_eligible_for_push(
    UserId, UserId, _GuildId, _ChannelId, _MessageData, _GuildDefaultNotifications, _UserRoles, _State
) ->
    false;
is_eligible_for_push(
    UserId,
    AuthorId,
    GuildId,
    ChannelId,
    MessageData,
    GuildDefaultNotifications,
    UserRolesMap,
    State
) ->
    Blocked = is_user_blocked(UserId, AuthorId, State),
    SettingsOk = check_user_guild_settings(
        UserId,
        GuildId,
        ChannelId,
        MessageData,
        GuildDefaultNotifications,
        UserRolesMap,
        State
    ),
    not Blocked andalso SettingsOk.

is_user_blocked(UserId, AuthorId, State) ->
    BlockedIdsCache = maps:get(blocked_ids_cache, State, #{}),
    case maps:get({blocked, UserId}, BlockedIdsCache, undefined) of
        undefined ->
            false;
        BlockedIds ->
            Blocked = lists:member(AuthorId, BlockedIds),
            Blocked
    end.

check_user_guild_settings(
    _UserId, 0, _ChannelId, _MessageData, _GuildDefaultNotifications, _UserRolesMap, _State
) ->
    true;
check_user_guild_settings(
    UserId,
    GuildId,
    ChannelId,
    MessageData,
    GuildDefaultNotifications,
    UserRolesMap,
    State
) ->
    UserGuildSettingsCache = maps:get(user_guild_settings_cache, State, #{}),
    Settings =
        case maps:get({settings, UserId, GuildId}, UserGuildSettingsCache, undefined) of
            undefined ->
                FetchedSettings = push_subscriptions:fetch_and_cache_user_guild_settings(
                    UserId, GuildId, State
                ),
                case FetchedSettings of
                    null -> #{};
                    S -> S
                end;
            S ->
                S
        end,

    MobilePush = maps:get(mobile_push, Settings, true),
    case MobilePush of
        false ->
            false;
        true ->
            Muted = maps:get(muted, Settings, false),
            ChannelOverrides = maps:get(channel_overrides, Settings, #{}),
            ChannelKey = integer_to_binary(ChannelId),
            ChannelOverride = maps:get(ChannelKey, ChannelOverrides, #{}),
            ChannelMuted = maps:get(muted, ChannelOverride, undefined),

            ActualMuted =
                case ChannelMuted of
                    undefined -> Muted;
                    _ -> ChannelMuted
                end,

            MuteConfig = maps:get(mute_config, Settings, undefined),
            IsTempMuted =
                case MuteConfig of
                    undefined ->
                        false;
                    #{<<"end_time">> := EndTimeStr} ->
                        case push_utils:parse_timestamp(EndTimeStr) of
                            undefined ->
                                false;
                            EndTime ->
                                Now = erlang:system_time(millisecond),
                                Now < EndTime
                        end;
                    _ ->
                        false
                end,

            case ActualMuted orelse IsTempMuted of
                true ->
                    false;
                false ->
                    Level = resolve_message_notifications(
                        ChannelId,
                        Settings,
                        GuildDefaultNotifications
                    ),
                    EffectiveLevel = override_for_large_guild(GuildId, Level, State),
                    should_allow_notification(
                        EffectiveLevel,
                        MessageData,
                        UserId,
                        Settings,
                        UserRolesMap
                    )
            end
    end.

should_allow_notification(Level, MessageData, UserId, Settings, UserRolesMap) ->
    case Level of
        ?MESSAGE_NOTIFICATIONS_NO_MESSAGES ->
            false;
        ?MESSAGE_NOTIFICATIONS_ONLY_MENTIONS ->
            case is_private_channel(MessageData) of
                true ->
                    true;
                false ->
                    is_user_mentioned(UserId, MessageData, Settings, UserRolesMap)
            end;
        _ ->
            true
    end.

is_private_channel(MessageData) ->
    ChannelType = maps:get(<<"channel_type">>, MessageData, ?CHANNEL_TYPE_DM),
    ChannelType =:= ?CHANNEL_TYPE_DM orelse ChannelType =:= ?CHANNEL_TYPE_GROUP_DM.

is_user_mentioned(UserId, MessageData, Settings, UserRolesMap) ->
    MentionEveryone = maps:get(<<"mention_everyone">>, MessageData, false),
    SuppressEveryone = maps:get(suppress_everyone, Settings, false),
    SuppressRoles = maps:get(suppress_roles, Settings, false),
    case {MentionEveryone, SuppressEveryone} of
        {true, false} ->
            true;
        {true, true} ->
            false;
        _ ->
            Mentions = maps:get(<<"mentions">>, MessageData, []),
            MentionRoles = maps:get(<<"mention_roles">>, MessageData, []),
            UserRoles = maps:get(UserId, UserRolesMap, []),
            is_user_in_mentions(UserId, Mentions) orelse
                case SuppressRoles of
                    true -> false;
                    false -> has_mentioned_role(UserRoles, MentionRoles)
                end
    end.

is_user_in_mentions(UserId, Mentions) ->
    lists:any(fun(Mention) -> mention_matches_user(UserId, Mention) end, Mentions).

mention_matches_user(UserId, Mention) ->
    case maps:get(<<"id">>, Mention, undefined) of
        undefined ->
            false;
        Id when is_integer(Id) ->
            Id =:= UserId;
        Id when is_binary(Id) ->
            case validation:validate_snowflake(<<"mention.id">>, Id) of
                {ok, ParsedId} -> ParsedId =:= UserId;
                _ -> false
            end;
        _ -> false
    end.

has_mentioned_role([], _) ->
    false;
has_mentioned_role([RoleId | Rest], MentionRoles) ->
    case role_in_mentions(RoleId, MentionRoles) of
        true -> true;
        false -> has_mentioned_role(Rest, MentionRoles)
    end.

role_in_mentions(RoleId, MentionRoles) ->
    RoleBin = integer_to_binary(RoleId),
    lists:any(
        fun(MentionRole) ->
            case MentionRole of
                Value when is_integer(Value) ->
                    Value =:= RoleId;
                Value when is_binary(Value) ->
                    Value =:= RoleBin;
                _ ->
                    false
            end
        end,
        MentionRoles
    ).

resolve_message_notifications(ChannelId, Settings, GuildDefaultNotifications) ->
    ChannelOverrides = maps:get(channel_overrides, Settings, #{}),
    ChannelKey = integer_to_binary(ChannelId),
    Level =
        case maps:get(ChannelKey, ChannelOverrides, undefined) of
            undefined ->
                undefined;
            Override ->
                maps:get(message_notifications, Override, ?MESSAGE_NOTIFICATIONS_NULL)
        end,
    case Level of
        ?MESSAGE_NOTIFICATIONS_NULL -> resolve_guild_notification(Settings, GuildDefaultNotifications);
        ?MESSAGE_NOTIFICATIONS_INHERIT -> resolve_guild_notification(Settings, GuildDefaultNotifications);
        undefined -> resolve_guild_notification(Settings, GuildDefaultNotifications);
        Valid -> normalize_notification_level(Valid)
    end.

resolve_guild_notification(Settings, GuildDefaultNotifications) ->
    Level = maps:get(message_notifications, Settings, ?MESSAGE_NOTIFICATIONS_NULL),
    case Level of
        ?MESSAGE_NOTIFICATIONS_NULL -> normalize_notification_level(GuildDefaultNotifications);
        ?MESSAGE_NOTIFICATIONS_INHERIT -> normalize_notification_level(GuildDefaultNotifications);
        Valid -> normalize_notification_level(Valid)
    end.

normalize_notification_level(Level) when Level == ?MESSAGE_NOTIFICATIONS_ALL ->
    Level;
normalize_notification_level(Level) when Level == ?MESSAGE_NOTIFICATIONS_ONLY_MENTIONS ->
    Level;
normalize_notification_level(Level) when Level == ?MESSAGE_NOTIFICATIONS_NO_MESSAGES ->
    Level;
normalize_notification_level(_) ->
    ?MESSAGE_NOTIFICATIONS_ALL.

override_for_large_guild(GuildId, CurrentLevel, _State) ->
    case get_guild_large_metadata(GuildId) of
        undefined ->
            CurrentLevel;
        #{member_count := Count, features := Features} ->
            case is_large_guild(Count, Features) of
                true -> enforce_only_mentions(CurrentLevel);
                false -> CurrentLevel
            end
    end.

enforce_only_mentions(CurrentLevel) ->
    case CurrentLevel of
        0 -> 1;
        _ -> CurrentLevel
    end.

is_large_guild(Count, Features) when is_integer(Count) ->
    Count > ?LARGE_GUILD_THRESHOLD orelse has_large_guild_override(Features);
is_large_guild(_, Features) ->
    has_large_guild_override(Features).

has_large_guild_override(Features) when is_list(Features) ->
    lists:member(?LARGE_GUILD_OVERRIDE_FEATURE, Features);
has_large_guild_override(_) ->
    false.

get_guild_large_metadata(GuildId) ->
    GuildName = process_registry:build_process_name(guild, GuildId),
    try
        case whereis(GuildName) of
            undefined ->
                undefined;
            Pid when is_pid(Pid) ->
                case gen_server:call(Pid, {get_large_guild_metadata}, 500) of
                    #{member_count := Count, features := Features} ->
                        #{member_count => Count, features => Features};
                    _ ->
                        undefined
                end
        end
    catch
        _:_ -> undefined
    end.
