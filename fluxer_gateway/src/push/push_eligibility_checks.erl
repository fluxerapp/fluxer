%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_eligibility_checks).
-typing([eqwalizer]).

-export([check_muted_and_notifications/8]).
-export([check_muted_and_notifications/9]).
-export([is_private_channel/1]).
-export([is_user_in_mentions/2]).
-export([mention_matches_user/2]).
-export([has_mentioned_role/2]).
-export([role_in_mentions/2]).
-export([resolve_message_notifications/3]).
-export([resolve_guild_notification/2]).
-export([normalize_notification_level/1]).
-export([override_for_large_guild/2]).
-export([enforce_only_mentions/1]).
-export([is_large_guild/2]).
-export([large_guild_threshold/0]).
-export([has_large_guild_override/1]).
-export([get_guild_large_metadata/1]).
-export([check_temp_muted/1]).

-define(LARGE_GUILD_THRESHOLD, 2500).
-define(LARGE_GUILD_OVERRIDE_FEATURE, <<"LARGE_GUILD_OVERRIDE">>).
-define(MESSAGE_NOTIFICATIONS_NULL, -1).
-define(MESSAGE_NOTIFICATIONS_ALL, 0).
-define(MESSAGE_NOTIFICATIONS_ONLY_MENTIONS, 1).
-define(MESSAGE_NOTIFICATIONS_NO_MESSAGES, 2).
-define(MESSAGE_NOTIFICATIONS_INHERIT, 3).
-define(CHANNEL_TYPE_DM, 1).
-define(CHANNEL_TYPE_GROUP_DM, 3).
-define(LARGE_METADATA_MAILBOX_SHED_THRESHOLD, 100).
-define(LARGE_METADATA_CALL_TIMEOUT_MS, 200).

-spec check_muted_and_notifications(
    integer(), integer(), map(), integer(), map(), map(), integer(), map()
) -> boolean().
check_muted_and_notifications(
    UserId,
    ChannelId,
    MessageData,
    GuildDefaultNotifications,
    UserRolesMap,
    Settings,
    GuildId,
    ConnectedUsers
) ->
    check_muted_and_notifications(
        UserId,
        ChannelId,
        MessageData,
        GuildDefaultNotifications,
        UserRolesMap,
        Settings,
        GuildId,
        ConnectedUsers,
        get_guild_large_metadata(GuildId)
    ).

-spec check_muted_and_notifications(
    integer(), integer(), map(), integer(), map(), map(), integer(), map(), map() | undefined
) -> boolean().
check_muted_and_notifications(
    UserId,
    ChannelId,
    MessageData,
    GuildDefaultNotifications,
    UserRolesMap,
    Settings,
    _GuildId,
    ConnectedUsers,
    LargeGuildMetadata
) ->
    Muted = boolean_setting(muted, Settings, false),
    ChannelOverrides = map_setting(channel_overrides, Settings),
    ChannelOverride = channel_override(ChannelId, ChannelOverrides, #{}),
    ChannelMuted = optional_boolean_setting(muted, ChannelOverride),
    ActualMuted = resolve_actual_muted(ChannelMuted, Muted),
    MuteConfig = push_eligibility:get_setting(mute_config, Settings, undefined),
    IsTempMuted = check_temp_muted(MuteConfig),
    case ActualMuted orelse IsTempMuted of
        true ->
            false;
        false ->
            Level = resolve_message_notifications(
                ChannelId, Settings, GuildDefaultNotifications
            ),
            EffectiveLevel = override_for_large_guild_metadata(LargeGuildMetadata, Level),
            push_eligibility:should_allow_notification(
                EffectiveLevel, MessageData, UserId, Settings, UserRolesMap, ConnectedUsers
            )
    end.

-spec resolve_actual_muted(boolean() | undefined, boolean()) -> boolean().
resolve_actual_muted(undefined, Muted) -> Muted;
resolve_actual_muted(ChannelMuted, _Muted) -> ChannelMuted.

-spec check_temp_muted(term()) -> boolean().
check_temp_muted(undefined) ->
    false;
check_temp_muted(#{<<"end_time">> := EndTimeStr}) ->
    case push_utils:parse_timestamp(EndTimeStr) of
        undefined ->
            false;
        EndTime ->
            Now = erlang:system_time(millisecond),
            Now < EndTime
    end;
check_temp_muted(_) ->
    false.

-spec is_private_channel(map()) -> boolean().
is_private_channel(MessageData) ->
    ChannelType = maps:get(<<"channel_type">>, MessageData, 0),
    ChannelType =:= ?CHANNEL_TYPE_DM orelse ChannelType =:= ?CHANNEL_TYPE_GROUP_DM.

-spec is_user_in_mentions(integer(), list()) -> boolean().
is_user_in_mentions(UserId, Mentions) ->
    lists:any(fun(Mention) -> mention_matches_user(UserId, Mention) end, Mentions).

-spec mention_matches_user(integer(), map()) -> boolean().
mention_matches_user(UserId, Mention) ->
    case maps:get(<<"id">>, Mention, undefined) of
        undefined -> false;
        Id when is_integer(Id) -> Id =:= UserId;
        Id -> snowflake_id:equal(UserId, Id)
    end.

-spec has_mentioned_role([integer()], list()) -> boolean().
has_mentioned_role([], _) ->
    false;
has_mentioned_role([RoleId | Rest], MentionRoles) ->
    case role_in_mentions(RoleId, MentionRoles) of
        true -> true;
        false -> has_mentioned_role(Rest, MentionRoles)
    end.

-spec role_in_mentions(integer(), list()) -> boolean().
role_in_mentions(RoleId, MentionRoles) ->
    snowflake_id:member(RoleId, MentionRoles).

-spec resolve_message_notifications(integer(), map(), integer()) -> integer().
resolve_message_notifications(ChannelId, Settings, GuildDefaultNotifications) ->
    ChannelOverrides = map_setting(channel_overrides, Settings),
    Level = extract_channel_level(ChannelId, ChannelOverrides),
    resolve_level_or_guild(Level, Settings, GuildDefaultNotifications).

-spec extract_channel_level(integer(), map()) -> integer() | undefined.
extract_channel_level(ChannelId, ChannelOverrides) ->
    case channel_override(ChannelId, ChannelOverrides, undefined) of
        undefined -> undefined;
        Override -> notification_level_setting(Override, ?MESSAGE_NOTIFICATIONS_NULL)
    end.

-spec channel_override(integer(), map(), term()) -> term().
channel_override(ChannelId, ChannelOverrides, Default) ->
    snowflake_id:get(ChannelId, ChannelOverrides, Default).

-spec resolve_level_or_guild(term(), map(), integer()) -> integer().
resolve_level_or_guild(?MESSAGE_NOTIFICATIONS_NULL, Settings, GuildDefault) ->
    resolve_guild_notification(Settings, GuildDefault);
resolve_level_or_guild(?MESSAGE_NOTIFICATIONS_INHERIT, Settings, GuildDefault) ->
    resolve_guild_notification(Settings, GuildDefault);
resolve_level_or_guild(undefined, Settings, GuildDefault) ->
    resolve_guild_notification(Settings, GuildDefault);
resolve_level_or_guild(Valid, _Settings, _GuildDefault) ->
    normalize_notification_level(Valid).

-spec resolve_guild_notification(map(), integer()) -> integer().
resolve_guild_notification(Settings, GuildDefaultNotifications) ->
    Level = notification_level_setting(Settings, ?MESSAGE_NOTIFICATIONS_NULL),
    case Level of
        ?MESSAGE_NOTIFICATIONS_NULL ->
            normalize_notification_level(GuildDefaultNotifications);
        ?MESSAGE_NOTIFICATIONS_INHERIT ->
            normalize_notification_level(GuildDefaultNotifications);
        Valid ->
            normalize_notification_level(Valid)
    end.

-spec normalize_notification_level(term()) -> integer().
normalize_notification_level(?MESSAGE_NOTIFICATIONS_ALL) ->
    ?MESSAGE_NOTIFICATIONS_ALL;
normalize_notification_level(?MESSAGE_NOTIFICATIONS_ONLY_MENTIONS) ->
    ?MESSAGE_NOTIFICATIONS_ONLY_MENTIONS;
normalize_notification_level(?MESSAGE_NOTIFICATIONS_NO_MESSAGES) ->
    ?MESSAGE_NOTIFICATIONS_NO_MESSAGES;
normalize_notification_level(_) ->
    ?MESSAGE_NOTIFICATIONS_ALL.

-spec override_for_large_guild(integer(), integer()) -> integer().
override_for_large_guild(GuildId, CurrentLevel) ->
    override_for_large_guild_metadata(get_guild_large_metadata(GuildId), CurrentLevel).

-spec override_for_large_guild_metadata(map() | undefined, integer()) -> integer().
override_for_large_guild_metadata(undefined, CurrentLevel) ->
    CurrentLevel;
override_for_large_guild_metadata(
    #{member_count := Count, features := Features}, CurrentLevel
) ->
    apply_large_guild_override(Count, Features, CurrentLevel);
override_for_large_guild_metadata(_Metadata, CurrentLevel) ->
    CurrentLevel.

-spec apply_large_guild_override(integer() | term(), list(), integer()) -> integer().
apply_large_guild_override(Count, Features, CurrentLevel) ->
    case is_large_guild(Count, Features) of
        true -> enforce_only_mentions(CurrentLevel);
        false -> CurrentLevel
    end.

-spec enforce_only_mentions(integer()) -> integer().
enforce_only_mentions(0) -> 1;
enforce_only_mentions(CurrentLevel) -> CurrentLevel.

-spec is_large_guild(integer() | term(), list()) -> boolean().
is_large_guild(Count, Features) when is_integer(Count) ->
    Count > large_guild_threshold() orelse has_large_guild_override(Features);
is_large_guild(_, Features) ->
    has_large_guild_override(Features).

-spec large_guild_threshold() -> pos_integer().
large_guild_threshold() ->
    ?LARGE_GUILD_THRESHOLD.

-spec has_large_guild_override(list() | term()) -> boolean().
has_large_guild_override(Features) when is_list(Features) ->
    lists:member(?LARGE_GUILD_OVERRIDE_FEATURE, Features);
has_large_guild_override(_) ->
    false.

-spec get_guild_large_metadata(integer()) -> map() | undefined.
get_guild_large_metadata(GuildId) ->
    GuildKey = process_registry:build_process_key(guild, GuildId),
    try
        lookup_guild_metadata(GuildKey)
    catch
        _:_ -> undefined
    end.

-spec lookup_guild_metadata(process_registry:process_key()) -> map() | undefined.
lookup_guild_metadata(GuildKey) ->
    case process_registry:registry_whereis(GuildKey) of
        undefined ->
            undefined;
        Pid when is_pid(Pid) ->
            query_guild_pid(Pid)
    end.

-spec query_guild_pid(pid()) -> map() | undefined.
query_guild_pid(Pid) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, Q} when Q >= ?LARGE_METADATA_MAILBOX_SHED_THRESHOLD ->
            undefined;
        _ ->
            call_guild_metadata(Pid)
    end.

-spec call_guild_metadata(pid()) -> map() | undefined.
call_guild_metadata(Pid) ->
    case gen_server:call(Pid, {get_large_guild_metadata}, ?LARGE_METADATA_CALL_TIMEOUT_MS) of
        #{member_count := Count, features := Features} ->
            #{member_count => Count, features => Features};
        _ ->
            undefined
    end.

-spec map_setting(atom(), term()) -> map().
map_setting(Key, Settings) ->
    case push_eligibility:get_setting(Key, Settings, #{}) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    end.

-spec boolean_setting(atom(), term(), boolean()) -> boolean().
boolean_setting(Key, Settings, Default) ->
    case push_eligibility:get_setting(Key, Settings, Default) of
        true -> true;
        false -> false;
        _ -> Default
    end.

-spec optional_boolean_setting(atom(), term()) -> boolean() | undefined.
optional_boolean_setting(Key, Settings) ->
    case push_eligibility:get_setting(Key, Settings, undefined) of
        true -> true;
        false -> false;
        _ -> undefined
    end.

-spec notification_level_setting(term(), integer()) -> integer().
notification_level_setting(Settings, Default) ->
    case push_eligibility:get_setting(message_notifications, Settings, Default) of
        Level when is_integer(Level) -> Level;
        _ -> Default
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_private_channel_test() ->
    ?assertEqual(true, is_private_channel(#{<<"channel_type">> => 1})),
    ?assertEqual(true, is_private_channel(#{<<"channel_type">> => 3})),
    ?assertEqual(false, is_private_channel(#{<<"channel_type">> => 0})),
    ?assertEqual(false, is_private_channel(#{})).

muted_channel_suppresses_push_test() ->
    UserId = 100,
    ChannelId = 200,
    MessageData = #{<<"channel_type">> => 0},
    GuildDefaultNotifications = 0,
    UserRolesMap = #{},
    ConnectedUsers = #{},
    GuildId = 1,
    Settings = #{
        channel_overrides => #{
            <<"200">> => #{muted => true}
        }
    },
    ?assertEqual(
        false,
        check_muted_and_notifications(
            UserId,
            ChannelId,
            MessageData,
            GuildDefaultNotifications,
            UserRolesMap,
            Settings,
            GuildId,
            ConnectedUsers
        )
    ).

guild_muted_suppresses_push_test() ->
    ?assertEqual(false, check_with_settings(#{muted => true})).

temp_muted_suppresses_push_test() ->
    FutureMs = integer_to_binary(erlang:system_time(millisecond) + 60000),
    ?assertEqual(false, check_with_settings(#{mute_config => #{<<"end_time">> => FutureMs}})).

expired_temp_mute_allows_push_test() ->
    PastMs = integer_to_binary(erlang:system_time(millisecond) - 60000),
    ?assertEqual(true, check_with_settings(#{mute_config => #{<<"end_time">> => PastMs}})).

check_with_settings(Settings) ->
    check_muted_and_notifications(
        100,
        200,
        #{<<"channel_type">> => 0},
        0,
        #{},
        Settings,
        1,
        #{}
    ).

is_user_in_mentions_test() ->
    Mentions = [#{<<"id">> => <<"123">>}, #{<<"id">> => <<"456">>}],
    ?assertEqual(true, is_user_in_mentions(123, Mentions)),
    ?assertEqual(true, is_user_in_mentions(456, Mentions)),
    ?assertEqual(false, is_user_in_mentions(789, Mentions)).

mention_matches_user_test() ->
    ?assertEqual(true, mention_matches_user(123, #{<<"id">> => 123})),
    ?assertEqual(true, mention_matches_user(123, #{<<"id">> => <<"123">>})),
    ?assertEqual(false, mention_matches_user(123, #{<<"id">> => <<"456">>})),
    ?assertEqual(false, mention_matches_user(123, #{})).

has_mentioned_role_test() ->
    ?assertEqual(true, has_mentioned_role([1, 2, 3], [2, 4])),
    ?assertEqual(true, has_mentioned_role([1, 2, 3], [<<"2">>])),
    ?assertEqual(false, has_mentioned_role([1, 2, 3], [4, 5])),
    ?assertEqual(false, has_mentioned_role([], [1, 2])).

normalize_notification_level_test() ->
    ?assertEqual(0, normalize_notification_level(0)),
    ?assertEqual(1, normalize_notification_level(1)),
    ?assertEqual(2, normalize_notification_level(2)),
    ?assertEqual(0, normalize_notification_level(99)).

enforce_only_mentions_test() ->
    ?assertEqual(1, enforce_only_mentions(0)),
    ?assertEqual(1, enforce_only_mentions(1)),
    ?assertEqual(2, enforce_only_mentions(2)).

is_large_guild_test() ->
    ?assertEqual(true, is_large_guild(3000, [])),
    ?assertEqual(false, is_large_guild(300, [])),
    ?assertEqual(true, is_large_guild(?LARGE_GUILD_THRESHOLD + 1, [])),
    ?assertEqual(false, is_large_guild(?LARGE_GUILD_THRESHOLD, [])),
    ?assertEqual(false, is_large_guild(100, [])),
    ?assertEqual(true, is_large_guild(100, [<<"LARGE_GUILD_OVERRIDE">>])),
    ?assertEqual(true, is_large_guild(300, [<<"LARGE_GUILD_OVERRIDE">>])),
    ?assertEqual(true, is_large_guild(undefined, [<<"LARGE_GUILD_OVERRIDE">>])).

large_guild_threshold_is_the_production_value_test() ->
    ?assertEqual(2500, ?LARGE_GUILD_THRESHOLD),
    ?assertEqual(?LARGE_GUILD_THRESHOLD, large_guild_threshold()).

mid_sized_guilds_keep_all_notifications_test() ->
    MessageData = #{<<"channel_type">> => 0},
    Metadata = #{member_count => 1000, features => []},
    ?assertEqual(
        true,
        check_muted_and_notifications(100, 200, MessageData, 0, #{}, #{}, 1, #{}, Metadata)
    ).

has_large_guild_override_test() ->
    ?assertEqual(true, has_large_guild_override([<<"LARGE_GUILD_OVERRIDE">>])),
    ?assertEqual(false, has_large_guild_override([<<"OTHER">>])),
    ?assertEqual(false, has_large_guild_override(not_a_list)).

cached_large_guild_metadata_overrides_all_notifications_test() ->
    MessageData = #{<<"channel_type">> => 0},
    LargeMetadata = #{member_count => 3000, features => []},
    ?assertEqual(
        false,
        check_muted_and_notifications(100, 200, MessageData, 0, #{}, #{}, 1, #{}, LargeMetadata)
    ).

cached_large_guild_metadata_keeps_mentions_allowed_test() ->
    MessageData = #{
        <<"channel_type">> => 0,
        <<"mentions">> => [#{<<"id">> => <<"100">>}]
    },
    LargeMetadata = #{member_count => 3000, features => []},
    ?assertEqual(
        true,
        check_muted_and_notifications(100, 200, MessageData, 0, #{}, #{}, 1, #{}, LargeMetadata)
    ).

undefined_large_guild_metadata_preserves_all_notifications_test() ->
    MessageData = #{<<"channel_type">> => 0},
    ?assertEqual(
        true,
        check_muted_and_notifications(100, 200, MessageData, 0, #{}, #{}, 1, #{}, undefined)
    ).

-endif.
