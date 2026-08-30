%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_eligibility).
-typing([eqwalizer]).

-export([is_user_blocked/2]).
-export([check_user_guild_settings/7]).
-export([check_user_guild_settings/8]).
-export([prefetch_user_guild_settings/3]).
-export([should_allow_notification/6]).
-export([is_user_mentioned/5]).
-export([is_eligible_for_push/8]).
-export([is_eligible_for_push/9]).
-export([get_setting/3]).

-define(MESSAGE_NOTIFICATIONS_NO_MESSAGES, 2).
-define(MESSAGE_NOTIFICATIONS_ONLY_MENTIONS, 1).
-define(SETTINGS_PREFETCH_CHUNK_SIZE, 200).

-spec is_eligible_for_push(
    integer(), integer(), integer(), integer(), map(), integer(), map(), map()
) -> boolean().
is_eligible_for_push(
    UserId,
    UserId,
    _GuildId,
    _ChannelId,
    _MessageData,
    _GuildDefaultNotifications,
    _UserRoles,
    _ConnectedUsers
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
    ConnectedUsers
) ->
    is_eligible_for_push(
        UserId,
        AuthorId,
        GuildId,
        ChannelId,
        MessageData,
        GuildDefaultNotifications,
        UserRolesMap,
        ConnectedUsers,
        push_eligibility_checks:get_guild_large_metadata(GuildId)
    ).

-spec is_eligible_for_push(
    integer(),
    integer(),
    integer(),
    integer(),
    map(),
    integer(),
    map(),
    map(),
    map() | undefined
) -> boolean().
is_eligible_for_push(
    UserId,
    UserId,
    _GuildId,
    _ChannelId,
    _MessageData,
    _GuildDefaultNotifications,
    _UserRoles,
    _ConnectedUsers,
    _LargeGuildMetadata
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
    ConnectedUsers,
    LargeGuildMetadata
) ->
    Blocked = is_user_blocked(UserId, AuthorId),
    SettingsOk = check_user_guild_settings(
        UserId,
        GuildId,
        ChannelId,
        MessageData,
        GuildDefaultNotifications,
        UserRolesMap,
        ConnectedUsers,
        LargeGuildMetadata
    ),
    Eligible = not Blocked andalso SettingsOk,
    log_ineligible(Eligible, UserId, AuthorId, GuildId, ChannelId, Blocked, SettingsOk),
    Eligible.

-spec log_ineligible(
    boolean(), integer(), integer(), integer(), integer(), boolean(), boolean()
) -> ok.
log_ineligible(false, UserId, AuthorId, GuildId, ChannelId, Blocked, SettingsOk) ->
    logger:debug(
        "Push: user not eligible",
        #{
            user_id => UserId,
            author_id => AuthorId,
            guild_id => GuildId,
            channel_id => ChannelId,
            blocked => Blocked,
            settings_ok => SettingsOk
        }
    );
log_ineligible(true, _UserId, _AuthorId, _GuildId, _ChannelId, _Blocked, _SettingsOk) ->
    ok.

-spec is_user_blocked(integer(), integer()) -> boolean().
is_user_blocked(UserId, AuthorId) ->
    case push_ets_cache:get_blocked_ids(UserId) of
        undefined -> false;
        BlockedIds -> lists:member(AuthorId, BlockedIds)
    end.

-spec check_user_guild_settings(
    integer(), integer(), integer(), map(), integer(), map(), map()
) -> boolean().
check_user_guild_settings(
    _UserId,
    0,
    _ChannelId,
    _MessageData,
    _GuildDefaultNotifications,
    _UserRolesMap,
    _ConnectedUsers
) ->
    true;
check_user_guild_settings(
    UserId,
    GuildId,
    ChannelId,
    MessageData,
    GuildDefaultNotifications,
    UserRolesMap,
    ConnectedUsers
) ->
    Settings = fetch_settings(UserId, GuildId),
    MobilePush = get_boolean_setting(mobile_push, Settings, true),
    case MobilePush of
        false ->
            false;
        true ->
            push_eligibility_checks:check_muted_and_notifications(
                UserId,
                ChannelId,
                MessageData,
                GuildDefaultNotifications,
                UserRolesMap,
                Settings,
                GuildId,
                ConnectedUsers
            )
    end.

-spec check_user_guild_settings(
    integer(), integer(), integer(), map(), integer(), map(), map(), map() | undefined
) -> boolean().
check_user_guild_settings(
    _UserId,
    0,
    _ChannelId,
    _MessageData,
    _GuildDefaultNotifications,
    _UserRolesMap,
    _ConnectedUsers,
    _LargeGuildMetadata
) ->
    true;
check_user_guild_settings(
    UserId,
    GuildId,
    ChannelId,
    MessageData,
    GuildDefaultNotifications,
    UserRolesMap,
    ConnectedUsers,
    LargeGuildMetadata
) ->
    Settings = fetch_settings(UserId, GuildId),
    MobilePush = get_boolean_setting(mobile_push, Settings, true),
    case MobilePush of
        false ->
            false;
        true ->
            push_eligibility_checks:check_muted_and_notifications(
                UserId,
                ChannelId,
                MessageData,
                GuildDefaultNotifications,
                UserRolesMap,
                Settings,
                GuildId,
                ConnectedUsers,
                LargeGuildMetadata
            )
    end.

-spec fetch_settings(integer(), integer()) -> map().
fetch_settings(UserId, GuildId) ->
    case push_ets_cache:get_user_guild_settings(UserId, GuildId) of
        undefined -> fetch_settings_rpc(UserId, GuildId);
        S -> S
    end.

-spec fetch_settings_rpc(integer(), integer()) -> map().
fetch_settings_rpc(UserId, GuildId) ->
    try push_subscriptions:fetch_and_cache_user_guild_settings(UserId, GuildId) of
        S0 when is_map(S0) -> S0;
        _ -> #{}
    catch
        throw:_ -> #{};
        error:_ -> #{};
        exit:_ -> #{}
    end.

-spec prefetch_user_guild_settings([integer()], integer(), integer()) -> ok.
prefetch_user_guild_settings(_UserIds, _AuthorId, 0) ->
    ok;
prefetch_user_guild_settings(UserIds, AuthorId, GuildId) ->
    prefetch_settings_chunks(uncached_settings_user_ids(UserIds, AuthorId, GuildId), GuildId).

-spec uncached_settings_user_ids([integer()], integer(), integer()) -> [integer()].
uncached_settings_user_ids(UserIds, AuthorId, GuildId) ->
    lists:usort(
        lists:filter(
            fun(UserId) -> is_settings_uncached(UserId, AuthorId, GuildId) end,
            UserIds
        )
    ).

-spec is_settings_uncached(integer(), integer(), integer()) -> boolean().
is_settings_uncached(AuthorId, AuthorId, _GuildId) ->
    false;
is_settings_uncached(UserId, _AuthorId, GuildId) ->
    push_ets_cache:get_user_guild_settings(UserId, GuildId) =:= undefined.

-spec prefetch_settings_chunks([integer()], integer()) -> ok.
prefetch_settings_chunks([], _GuildId) ->
    ok;
prefetch_settings_chunks(UserIds, GuildId) ->
    {Chunk, Rest} = take_settings_chunk(UserIds, ?SETTINGS_PREFETCH_CHUNK_SIZE, []),
    prefetch_settings_chunk(Chunk, GuildId),
    prefetch_settings_chunks(Rest, GuildId).

-spec take_settings_chunk([integer()], non_neg_integer(), [integer()]) ->
    {[integer()], [integer()]}.
take_settings_chunk(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
take_settings_chunk([], _Remaining, Acc) ->
    {lists:reverse(Acc), []};
take_settings_chunk([UserId | Rest], Remaining, Acc) ->
    take_settings_chunk(Rest, Remaining - 1, [UserId | Acc]).

-spec prefetch_settings_chunk([integer()], integer()) -> ok.
prefetch_settings_chunk(UserIds, GuildId) ->
    try prefetch_settings_chunk_rpc(UserIds, GuildId) of
        ok -> ok
    catch
        throw:_ -> ok;
        error:_ -> ok;
        exit:_ -> ok
    end.

-spec prefetch_settings_chunk_rpc([integer()], integer()) -> ok.
prefetch_settings_chunk_rpc(UserIds, GuildId) ->
    Req = #{
        <<"type">> => <<"get_user_guild_settings">>,
        <<"user_ids">> => [integer_to_binary(UserId) || UserId <- UserIds],
        <<"guild_id">> => integer_to_binary(GuildId)
    },
    logger:debug(
        "Push: prefetching user guild settings via RPC",
        #{user_count => length(UserIds), guild_id => GuildId}
    ),
    case rpc_client:call(Req) of
        {ok, Data} ->
            cache_prefetched_settings(UserIds, GuildId, settings_list(Data));
        {error, Reason} ->
            logger:debug(
                "Push: RPC failed to prefetch user guild settings",
                #{user_count => length(UserIds), guild_id => GuildId, reason => Reason}
            ),
            ok
    end.

-spec settings_list(map()) -> [term()].
settings_list(Data) ->
    case maps:get(<<"user_guild_settings">>, Data, []) of
        Settings when is_list(Settings) -> Settings;
        _ -> []
    end.

-spec cache_prefetched_settings([integer()], integer(), [term()]) -> ok.
cache_prefetched_settings(UserIds, GuildId, Settings) when
    length(UserIds) =:= length(Settings)
->
    lists:foreach(
        fun({UserId, UserSettings}) ->
            push_ets_cache:put_user_guild_settings(
                UserId, GuildId, settings_map(UserSettings)
            )
        end,
        lists:zip(UserIds, Settings)
    );
cache_prefetched_settings(UserIds, GuildId, Settings) ->
    logger:debug(
        "Push: prefetched user guild settings did not match requested users",
        #{
            user_count => length(UserIds),
            guild_id => GuildId,
            settings_count => length(Settings)
        }
    ),
    ok.

-spec settings_map(term()) -> map().
settings_map(Settings) when is_map(Settings) ->
    Settings;
settings_map(_Settings) ->
    #{}.

-spec should_allow_notification(
    integer(), map(), integer(), map(), map(), map()
) -> boolean().
should_allow_notification(
    ?MESSAGE_NOTIFICATIONS_NO_MESSAGES,
    _MessageData,
    _UserId,
    _Settings,
    _UserRolesMap,
    _ConnectedUsers
) ->
    false;
should_allow_notification(
    ?MESSAGE_NOTIFICATIONS_ONLY_MENTIONS,
    MessageData,
    UserId,
    Settings,
    UserRolesMap,
    ConnectedUsers
) ->
    case push_eligibility_checks:is_private_channel(MessageData) of
        true -> true;
        false -> is_user_mentioned(UserId, MessageData, Settings, UserRolesMap, ConnectedUsers)
    end;
should_allow_notification(
    _, _MessageData, _UserId, _Settings, _UserRolesMap, _ConnectedUsers
) ->
    true.

-spec is_user_mentioned(integer(), map(), map(), map(), map()) -> boolean().
is_user_mentioned(UserId, MessageData, Settings, UserRolesMap, ConnectedUsers) ->
    {EffectiveEveryoneMention, SuppressEveryone, SuppressRoles} =
        extract_mention_flags(UserId, MessageData, Settings, ConnectedUsers),
    evaluate_mentions(
        EffectiveEveryoneMention,
        SuppressEveryone,
        SuppressRoles,
        UserId,
        MessageData,
        UserRolesMap
    ).

-spec extract_mention_flags(integer(), map(), map(), map()) ->
    {boolean(), boolean(), boolean()}.
extract_mention_flags(UserId, MessageData, Settings, ConnectedUsers) ->
    RawMentionEveryone = maps:get(<<"mention_everyone">>, MessageData, false),
    MentionHere = maps:get(<<"mention_here">>, MessageData, false),
    MentionEveryone = RawMentionEveryone andalso not MentionHere,
    EffectiveEveryoneMention =
        MentionEveryone orelse (MentionHere andalso maps:is_key(UserId, ConnectedUsers)),
    SuppressEveryone = get_boolean_setting(suppress_everyone, Settings, false),
    SuppressRoles = get_boolean_setting(suppress_roles, Settings, false),
    {EffectiveEveryoneMention, SuppressEveryone, SuppressRoles}.

-spec evaluate_mentions(
    boolean(), boolean(), boolean(), integer(), map(), map()
) -> boolean().
evaluate_mentions(true, false, _SuppressRoles, _UserId, _MessageData, _UserRolesMap) ->
    true;
evaluate_mentions(true, true, _SuppressRoles, _UserId, _MessageData, _UserRolesMap) ->
    false;
evaluate_mentions(_EvMention, _SuppEv, SuppressRoles, UserId, MessageData, UserRolesMap) ->
    Mentions = maps:get(<<"mentions">>, MessageData, []),
    MentionRoles = maps:get(<<"mention_roles">>, MessageData, []),
    UserRoles = maps:get(UserId, UserRolesMap, []),
    InMentions = push_eligibility_checks:is_user_in_mentions(UserId, Mentions),
    HasRole = push_eligibility_checks:has_mentioned_role(UserRoles, MentionRoles),
    InMentions orelse (not SuppressRoles andalso HasRole).

-spec get_setting(atom(), term(), term()) -> term().
get_setting(Key, Settings, Default) when is_atom(Key), is_map(Settings) ->
    case Settings of
        #{Key := null} -> Default;
        #{Key := V} -> V;
        _ -> get_setting_binary(Key, Settings, Default)
    end;
get_setting(_Key, _Settings, Default) ->
    Default.

-spec get_boolean_setting(atom(), term(), boolean()) -> boolean().
get_boolean_setting(Key, Settings, Default) ->
    case get_setting(Key, Settings, Default) of
        true -> true;
        false -> false;
        _ -> Default
    end.

-spec get_setting_binary(atom(), map(), term()) -> term().
get_setting_binary(Key, Settings, Default) ->
    BinKey = atom_to_binary(Key, utf8),
    case Settings of
        #{BinKey := null} -> Default;
        #{BinKey := V} -> V;
        _ -> Default
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_setting_atom_test() ->
    ?assertEqual(true, get_setting(mobile_push, #{mobile_push => true}, false)).

get_setting_binary_test() ->
    ?assertEqual(true, get_setting(mobile_push, #{<<"mobile_push">> => true}, false)).

get_setting_default_test() ->
    ?assertEqual(default, get_setting(mobile_push, #{}, default)),
    ?assertEqual(default, get_setting(mobile_push, not_a_map, default)).

is_eligible_same_user_test() ->
    ?assertEqual(false, is_eligible_for_push(123, 123, 0, 0, #{}, 0, #{}, #{})).

is_user_blocked_test() ->
    push_ets_cache:init(),
    push_ets_cache:put_blocked_ids(123, [456, 789]),
    ?assertEqual(true, is_user_blocked(123, 456)),
    ?assertEqual(false, is_user_blocked(123, 999)),
    ?assertEqual(false, is_user_blocked(999, 456)),
    try
        ets:delete(push_blocked_ids)
    catch
        error:badarg -> ok
    end.

get_setting_json_null_treated_as_missing_test() ->
    ?assertEqual(true, get_setting(mobile_push, #{<<"mobile_push">> => null}, true)),
    NullOverrides = #{<<"channel_overrides">> => null},
    ?assertEqual(default, get_setting(channel_overrides, NullOverrides, default)),
    ?assertEqual(default, get_setting(mobile_push, #{mobile_push => null}, default)).

mention_here_requires_connected_user_test() ->
    MessageData = #{<<"mention_everyone">> => true, <<"mention_here">> => true},
    ?assertEqual(false, is_user_mentioned(123, MessageData, #{}, #{}, #{})),
    ?assertEqual(true, is_user_mentioned(123, MessageData, #{}, #{}, #{123 => true})).

mention_here_respects_suppress_everyone_test() ->
    MessageData = #{<<"mention_everyone">> => true, <<"mention_here">> => true},
    Settings = #{suppress_everyone => true},
    ?assertEqual(false, is_user_mentioned(123, MessageData, Settings, #{}, #{123 => true})).

-endif.
