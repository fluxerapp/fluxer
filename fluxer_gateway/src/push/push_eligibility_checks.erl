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
-export([is_large_guild/2]).
-export([large_guild_threshold/0]).
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
    Muted =
        is_mute_active(Settings) orelse
            is_override_muted(maps:get(<<"category_id">>, Thread, undefined), ChannelOverrides) orelse
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
    Suppressed =
        is_mute_active(Settings) orelse
            is_override_muted(maps:get(<<"category_id">>, Thread, undefined), ChannelOverrides) orelse
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
            ConnectedUsers,
            undefined
        )
    ).

guild_muted_suppresses_push_test() ->
    ?assertEqual(false, check_with_settings(#{muted => true})).

temp_muted_suppresses_push_test() ->
    MuteConfig = #{<<"end_time">> => rfc3339_in_ms(60000)},
    ?assertEqual(false, check_with_settings(#{muted => true, mute_config => MuteConfig})).

expired_temp_mute_allows_push_test() ->
    MuteConfig = #{<<"end_time">> => rfc3339_in_ms(-60000)},
    ?assertEqual(true, check_with_settings(#{muted => true, mute_config => MuteConfig})).

rfc3339_in_ms(OffsetMs) ->
    Ms = erlang:system_time(millisecond) + OffsetMs,
    list_to_binary(calendar:system_time_to_rfc3339(Ms, [{unit, millisecond}, {offset, "Z"}])).

check_with_settings(Settings) ->
    check_muted_and_notifications(
        100,
        200,
        #{<<"channel_type">> => 0},
        0,
        #{},
        Settings,
        1,
        #{},
        undefined
    ).

thread_message(Members) ->
    thread_message(Members, #{}).

thread_message(Members, Extra) ->
    maps:merge(
        #{
            <<"channel_type">> => 11,
            <<"__thread_push">> => #{
                <<"parent_id">> => 200,
                <<"parent_name">> => <<"general">>,
                <<"category_id">> => 50,
                <<"members">> => Members
            }
        },
        Extra
    ).

check_thread(MessageData, Settings, Metadata) ->
    check_muted_and_notifications(100, 500, MessageData, 0, #{}, Settings, 1, #{}, Metadata).

thread_member_inherits_the_parent_level_test() ->
    Member = #{100 => #{<<"flags">> => 1}},
    ?assert(check_thread(thread_message(Member), #{}, undefined)),
    ParentMentions = #{channel_overrides => #{<<"200">> => #{message_notifications => 1}}},
    ?assertNot(check_thread(thread_message(Member), ParentMentions, undefined)),
    ThreadOverride = #{channel_overrides => #{<<"500">> => #{message_notifications => 1}}},
    ?assert(check_thread(thread_message(Member), ThreadOverride, undefined)).

explicit_thread_level_beats_the_parent_level_test() ->
    ParentMentions = #{channel_overrides => #{<<"200">> => #{message_notifications => 1}}},
    ?assert(
        check_thread(thread_message(#{100 => #{<<"flags">> => 2}}), ParentMentions, undefined)
    ),
    ?assertNot(check_thread(thread_message(#{100 => #{<<"flags">> => 8}}), #{}, undefined)),
    Mentioned = thread_message(
        #{100 => #{<<"flags">> => 4}}, #{<<"mentions">> => [#{<<"id">> => <<"100">>}]}
    ),
    ?assert(check_thread(Mentioned, #{}, undefined)),
    ?assertNot(check_thread(thread_message(#{100 => #{<<"flags">> => 4}}), #{}, undefined)).

large_guild_clamp_applies_to_inherited_thread_levels_only_test() ->
    Large = #{member_count => 3000, features => []},
    ?assertNot(check_thread(thread_message(#{100 => #{<<"flags">> => 0}}), #{}, Large)),
    ?assert(check_thread(thread_message(#{100 => #{<<"flags">> => 2}}), #{}, Large)).

thread_mutes_apply_at_every_layer_test() ->
    Member = #{100 => #{<<"flags">> => 2}},
    Muted = #{muted => true},
    ?assertNot(check_thread(thread_message(Member), Muted, undefined)),
    ?assertNot(
        check_thread(
            thread_message(Member), #{channel_overrides => #{<<"50">> => Muted}}, undefined
        )
    ),
    ?assertNot(
        check_thread(
            thread_message(Member), #{channel_overrides => #{<<"200">> => Muted}}, undefined
        )
    ),
    ?assertNot(
        check_thread(
            thread_message(#{100 => #{<<"flags">> => 2, <<"muted">> => true}}), #{}, undefined
        )
    ),
    Expired = #{<<"end_time">> => rfc3339_in_ms(-60000)},
    ?assert(
        check_thread(
            thread_message(#{
                100 => #{<<"flags">> => 2, <<"muted">> => true, <<"mute_config">> => Expired}
            }),
            #{},
            undefined
        )
    ).

thread_non_members_need_a_direct_or_role_mention_test() ->
    ?assertNot(check_thread(thread_message(#{}), #{}, undefined)),
    Everyone = thread_message(#{}, #{<<"mention_everyone">> => true}),
    ?assertNot(check_thread(Everyone, #{}, undefined)),
    Direct = thread_message(#{}, #{<<"mentions">> => [#{<<"id">> => <<"100">>}]}),
    ?assert(check_thread(Direct, #{}, undefined)),
    ?assert(
        check_thread(
            thread_message(#{100 => #{<<"flags">> => 0}}, #{<<"mention_everyone">> => true}),
            #{},
            undefined
        )
    ).

forum_message() ->
    thread_message(#{}, #{
        <<"__thread_push">> => #{
            <<"parent_id">> => 200,
            <<"parent_name">> => <<"ideas">>,
            <<"category_id">> => 50,
            <<"members">> => #{},
            <<"forum_thread_created">> => true
        }
    }).

forum_thread_created_needs_all_messages_on_the_forum_test() ->
    ?assert(check_thread(forum_message(), #{}, undefined)),
    Mentions = #{channel_overrides => #{<<"200">> => #{message_notifications => 1}}},
    ?assertNot(check_thread(forum_message(), Mentions, undefined)),
    ?assertNot(check_thread(forum_message(), #{message_notifications => 1}, undefined)),
    ?assertNot(
        check_thread(forum_message(), #{}, #{member_count => 3000, features => []})
    ),
    ExplicitAll = #{
        message_notifications => 1,
        channel_overrides => #{<<"200">> => #{message_notifications => 0}}
    },
    ?assert(check_thread(forum_message(), ExplicitAll, undefined)).

forum_thread_created_honours_mutes_and_new_forum_threads_off_test() ->
    Muted = #{muted => true},
    ?assertNot(check_thread(forum_message(), Muted, undefined)),
    ?assertNot(
        check_thread(forum_message(), #{channel_overrides => #{<<"50">> => Muted}}, undefined)
    ),
    ?assertNot(
        check_thread(forum_message(), #{channel_overrides => #{<<"200">> => Muted}}, undefined)
    ),
    Off = #{channel_overrides => #{<<"200">> => #{<<"flags">> => 16#2000}}},
    ?assertNot(check_thread(forum_message(), Off, undefined)),
    On = #{channel_overrides => #{<<"200">> => #{<<"flags">> => 16#4000}}},
    ?assert(check_thread(forum_message(), On, undefined)).

forum_thread_created_new_forum_threads_on_does_not_opt_in_below_all_test() ->
    OnMentions = #{
        channel_overrides => #{
            <<"200">> => #{<<"flags">> => 16#4000, message_notifications => 1}
        }
    },
    ?assertNot(check_thread(forum_message(), OnMentions, undefined)),
    ?assertNot(
        check_thread(
            forum_message(),
            #{channel_overrides => #{<<"200">> => #{<<"flags">> => 16#4000}}},
            #{member_count => 3000, features => []}
        )
    ),
    MutedOn = #{
        channel_overrides => #{
            <<"200">> => #{<<"flags">> => 16#4000, muted => true}
        }
    },
    ?assertNot(check_thread(forum_message(), MutedOn, undefined)),
    BothFlags = #{channel_overrides => #{<<"200">> => #{<<"flags">> => 16#6000}}},
    ?assertNot(check_thread(forum_message(), BothFlags, undefined)).

strip_thread_eligibility_keeps_delivery_fields_test() ->
    ?assertEqual(
        #{
            <<"parent_id">> => 200,
            <<"parent_name">> => <<"ideas">>,
            <<"forum_thread_created">> => true
        },
        maps:get(<<"__thread_push">>, strip_thread_eligibility(forum_message()))
    ),
    ?assertEqual(
        #{
            <<"channel_type">> => 11,
            <<"__thread_push">> => #{<<"parent_id">> => 200, <<"parent_name">> => <<"general">>}
        },
        strip_thread_eligibility(thread_message(#{100 => #{}}))
    ),
    ?assertEqual(#{<<"id">> => 1}, strip_thread_eligibility(#{<<"id">> => 1})).

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
