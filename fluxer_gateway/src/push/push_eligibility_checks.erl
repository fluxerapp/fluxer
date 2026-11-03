%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_eligibility_checks).
-typing([eqwalizer]).

-export([check_muted_and_notifications/9]).
-export([is_private_channel/1]).
-export([is_user_in_mentions/2]).
-export([mention_matches_user/2]).
-export([has_mentioned_role/2]).
-export([role_in_mentions/2]).
-export([resolve_message_notifications/3]).
-export([resolve_guild_notification/2]).
-export([normalize_notification_level/1]).
-export([enforce_only_mentions/1]).
-export([has_large_guild_override/1]).
-export([get_guild_large_metadata/1]).
-export([strip_thread_eligibility/1]).

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
-define(THREAD_PUSH_KEY, <<"__thread_push">>).
-define(THREAD_MEMBER_ALL_MESSAGES, 16#2).
-define(THREAD_MEMBER_ONLY_MENTIONS, 16#4).
-define(THREAD_MEMBER_NO_MESSAGES, 16#8).
-define(NEW_FORUM_THREADS_OFF, 16#2000).

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
    case maps:get(?THREAD_PUSH_KEY, MessageData, undefined) of
        #{<<"parent_id">> := ParentId, <<"forum_thread_created">> := true} = Thread when
            is_integer(ParentId)
        ->
            check_forum_thread_created(
                Thread, GuildDefaultNotifications, Settings, LargeGuildMetadata
            );
        #{<<"parent_id">> := ParentId} = Thread when is_integer(ParentId) ->
            check_thread_muted_and_notifications(
                UserId,
                Thread,
                MessageData,
                GuildDefaultNotifications,
                UserRolesMap,
                Settings,
                ConnectedUsers,
                LargeGuildMetadata
            );
        _ ->
            check_channel_muted_and_notifications(
                UserId,
                ChannelId,
                MessageData,
                GuildDefaultNotifications,
                UserRolesMap,
                Settings,
                ConnectedUsers,
                LargeGuildMetadata
            )
    end.

-spec check_channel_muted_and_notifications(
    integer(), integer(), map(), integer(), map(), map(), map(), map() | undefined
) -> boolean().
check_channel_muted_and_notifications(
    UserId,
    ChannelId,
    MessageData,
    GuildDefaultNotifications,
    UserRolesMap,
    Settings,
    ConnectedUsers,
    LargeGuildMetadata
) ->
    ChannelOverrides = map_setting(channel_overrides, Settings),
    ChannelOverride = channel_override(ChannelId, ChannelOverrides, #{}),
    case is_mute_active(Settings) orelse is_mute_active(ChannelOverride) of
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

-spec check_thread_muted_and_notifications(
    integer(), map(), map(), integer(), map(), map(), map(), map() | undefined
) -> boolean().
check_thread_muted_and_notifications(
    UserId,
    Thread,
    MessageData,
    GuildDefaultNotifications,
    UserRolesMap,
    Settings,
    ConnectedUsers,
    LargeGuildMetadata
) ->
    #{<<"parent_id">> := ParentId} = Thread,
    ChannelOverrides = map_setting(channel_overrides, Settings),
    ThreadMember = thread_member(UserId, Thread),
    CategoryId = maps:get(<<"category_id">>, Thread, undefined),
    Muted =
        is_mute_active(Settings) orelse
            is_override_muted(CategoryId, ChannelOverrides) orelse
            is_override_muted(ParentId, ChannelOverrides) orelse
            is_mute_active(ThreadMember),
    case Muted of
        true ->
            false;
        false ->
            Level = thread_notification_level(
                ThreadMember,
                ParentId,
                Settings,
                GuildDefaultNotifications,
                LargeGuildMetadata
            ),
            push_eligibility:should_allow_notification(
                Level,
                thread_mention_scope(ThreadMember, MessageData),
                UserId,
                Settings,
                UserRolesMap,
                ConnectedUsers
            )
    end.

-spec check_forum_thread_created(map(), integer(), map(), map() | undefined) -> boolean().
check_forum_thread_created(Thread, GuildDefaultNotifications, Settings, LargeGuildMetadata) ->
    #{<<"parent_id">> := ForumId} = Thread,
    ChannelOverrides = map_setting(channel_overrides, Settings),
    ForumOverride = channel_override(ForumId, ChannelOverrides, #{}),
    CategoryId = maps:get(<<"category_id">>, Thread, undefined),
    Suppressed =
        is_mute_active(Settings) orelse
            is_override_muted(CategoryId, ChannelOverrides) orelse
            is_mute_active(ForumOverride) orelse
            new_forum_threads_flag(ForumOverride, ?NEW_FORUM_THREADS_OFF),
    not Suppressed andalso
        inherited_thread_level(ForumId, Settings, GuildDefaultNotifications, LargeGuildMetadata) =:=
            ?MESSAGE_NOTIFICATIONS_ALL.

-spec new_forum_threads_flag(term(), integer()) -> boolean().
new_forum_threads_flag(Override, Flag) ->
    case push_eligibility:get_setting(flags, Override, 0) of
        Flags when is_integer(Flags) -> Flags band Flag =/= 0;
        _ -> false
    end.

-spec thread_member(integer(), map()) -> map() | undefined.
thread_member(UserId, Thread) ->
    case maps:get(<<"members">>, Thread, #{}) of
        #{UserId := Member} when is_map(Member) -> Member;
        _ -> undefined
    end.

-spec is_override_muted(term(), map()) -> boolean().
is_override_muted(ChannelId, ChannelOverrides) when is_integer(ChannelId) ->
    is_mute_active(channel_override(ChannelId, ChannelOverrides, #{}));
is_override_muted(_ChannelId, _ChannelOverrides) ->
    false.

-spec thread_notification_level(
    map() | undefined, integer(), map(), integer(), map() | undefined
) -> integer().
thread_notification_level(undefined, ParentId, Settings, GuildDefault, LargeGuildMetadata) ->
    Inherited = inherited_thread_level(ParentId, Settings, GuildDefault, LargeGuildMetadata),
    max(Inherited, ?MESSAGE_NOTIFICATIONS_ONLY_MENTIONS);
thread_notification_level(ThreadMember, ParentId, Settings, GuildDefault, LargeGuildMetadata) ->
    case explicit_thread_level(push_eligibility:get_setting(flags, ThreadMember, 0)) of
        undefined ->
            inherited_thread_level(ParentId, Settings, GuildDefault, LargeGuildMetadata);
        Level ->
            Level
    end.

-spec inherited_thread_level(integer(), map(), integer(), map() | undefined) -> integer().
inherited_thread_level(ParentId, Settings, GuildDefault, LargeGuildMetadata) ->
    override_for_large_guild_metadata(
        LargeGuildMetadata, resolve_message_notifications(ParentId, Settings, GuildDefault)
    ).

-spec explicit_thread_level(term()) -> integer() | undefined.
explicit_thread_level(Flags) when
    is_integer(Flags), Flags band ?THREAD_MEMBER_NO_MESSAGES =/= 0
->
    ?MESSAGE_NOTIFICATIONS_NO_MESSAGES;
explicit_thread_level(Flags) when
    is_integer(Flags), Flags band ?THREAD_MEMBER_ONLY_MENTIONS =/= 0
->
    ?MESSAGE_NOTIFICATIONS_ONLY_MENTIONS;
explicit_thread_level(Flags) when
    is_integer(Flags), Flags band ?THREAD_MEMBER_ALL_MESSAGES =/= 0
->
    ?MESSAGE_NOTIFICATIONS_ALL;
explicit_thread_level(_Flags) ->
    undefined.

-spec thread_mention_scope(map() | undefined, map()) -> map().
thread_mention_scope(undefined, MessageData) ->
    MessageData#{<<"mention_everyone">> => false, <<"mention_here">> => false};
thread_mention_scope(_ThreadMember, MessageData) ->
    MessageData.

-spec strip_thread_eligibility(map()) -> map().
strip_thread_eligibility(#{?THREAD_PUSH_KEY := Thread} = MessageData) when is_map(Thread) ->
    MessageData#{
        ?THREAD_PUSH_KEY => maps:with(
            [<<"parent_id">>, <<"parent_name">>, <<"forum_thread_created">>], Thread
        )
    };
strip_thread_eligibility(MessageData) ->
    MessageData.

-spec is_mute_active(term()) -> boolean().
is_mute_active(Config) ->
    boolean_setting(muted, Config, false) andalso
        mute_unexpired(push_eligibility:get_setting(mute_config, Config, undefined)).

-spec mute_unexpired(term()) -> boolean().
mute_unexpired(MuteConfig) when is_map(MuteConfig) ->
    case mute_end_ms(push_eligibility:get_setting(end_time, MuteConfig, undefined)) of
        undefined -> true;
        EndMs -> erlang:system_time(millisecond) < EndMs
    end;
mute_unexpired(_MuteConfig) ->
    true.

-spec mute_end_ms(term()) -> integer() | undefined.
mute_end_ms(EndTime) when is_binary(EndTime) ->
    try calendar:rfc3339_to_system_time(binary_to_list(EndTime), [{unit, millisecond}]) of
        EndMs -> EndMs
    catch
        _:_ -> undefined
    end;
mute_end_ms(_EndTime) ->
    undefined.

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

-spec notification_level_setting(term(), integer()) -> integer().
notification_level_setting(Settings, Default) ->
    case push_eligibility:get_setting(message_notifications, Settings, Default) of
        Level when is_integer(Level) -> Level;
        _ -> Default
    end.
