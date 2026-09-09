%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_passive).
-typing([eqwalizer]).

-export([
    is_passive/2,
    is_small_guild/1,
    set_active/2,
    set_passive/2,
    should_receive_event/5,
    get_user_roles_for_guild/2,
    should_receive_typing/2,
    set_typing_override/3,
    get_typing_override/2,
    is_user_mentioned/2,
    is_guild_synced/2,
    mark_guild_synced/2,
    clear_guild_synced/2,
    is_message_event/1,
    is_lazy_guild_event/1,
    extract_role_ids/1,
    mention_role_set/1
]).

-export_type([guild_id/0, user_id/0, event/0, session_data/0, guild_state/0]).

-define(MAX_MENTION_ROLES, 100).

-type guild_id() :: session:guild_id().
-type user_id() :: session:user_id().
-type event() :: atom().
-type session_data() :: map().
-type guild_state() :: map().

-spec is_passive(guild_id(), session_data()) -> boolean().
is_passive(GuildId, SessionData) ->
    case maps:get(bot, SessionData, false) of
        true ->
            false;
        false ->
            ActiveGuilds = maps:get(active_guilds, SessionData, sets:new()),
            not sets:is_element(GuildId, ActiveGuilds)
    end.

-spec set_active(guild_id(), session_data()) -> session_data().
set_active(GuildId, SessionData) ->
    ActiveGuilds = maps:get(active_guilds, SessionData, sets:new()),
    NewActiveGuilds = sets:add_element(GuildId, ActiveGuilds),
    SessionData#{active_guilds => NewActiveGuilds}.

-spec set_passive(guild_id(), session_data()) -> session_data().
set_passive(GuildId, SessionData) ->
    ActiveGuilds = maps:get(active_guilds, SessionData, sets:new()),
    NewActiveGuilds = sets:del_element(GuildId, ActiveGuilds),
    SessionData#{active_guilds => NewActiveGuilds}.

-spec should_receive_event(event(), map(), guild_id(), session_data(), guild_state()) ->
    boolean().
should_receive_event(typing_start, _EventData, GuildId, SessionData, State) ->
    should_receive_typing(GuildId, SessionData, State);
should_receive_event(Event, EventData, GuildId, SessionData, State) ->
    case bypasses_passive_filter(Event, EventData, SessionData) of
        true ->
            true;
        false ->
            should_receive_regular_event(Event, EventData, GuildId, SessionData, State)
    end.

-spec bypasses_passive_filter(event(), map(), session_data()) -> boolean().
bypasses_passive_filter(guild_update, _EventData, _SessionData) ->
    true;
bypasses_passive_filter(guild_role_update, _EventData, _SessionData) ->
    true;
bypasses_passive_filter(guild_role_update_bulk, _EventData, _SessionData) ->
    true;
bypasses_passive_filter(channel_create, _EventData, _SessionData) ->
    true;
bypasses_passive_filter(channel_update, _EventData, _SessionData) ->
    true;
bypasses_passive_filter(channel_update_bulk, _EventData, _SessionData) ->
    true;
bypasses_passive_filter(channel_delete, _EventData, _SessionData) ->
    true;
bypasses_passive_filter(guild_member_update, EventData, SessionData) ->
    is_session_user_event(EventData, SessionData);
bypasses_passive_filter(_, _, _) ->
    false.

-spec should_receive_regular_event(event(), map(), guild_id(), session_data(), guild_state()) ->
    boolean().
should_receive_regular_event(Event, EventData, GuildId, SessionData, State) ->
    case maps:get(bot, SessionData, false) of
        true ->
            true;
        false ->
            should_receive_non_bot(Event, EventData, GuildId, SessionData, State)
    end.

-spec should_receive_non_bot(event(), map(), guild_id(), session_data(), guild_state()) ->
    boolean().
should_receive_non_bot(Event, EventData, GuildId, SessionData, State) ->
    case is_effectively_active(GuildId, SessionData, State) of
        true -> true;
        false -> should_passive_receive(Event, EventData, SessionData)
    end.

-spec is_small_guild(guild_state()) -> boolean().
is_small_guild(State) ->
    MemberCount = maps:get(member_count, State, undefined),
    case MemberCount of
        undefined -> false;
        Count when is_integer(Count) -> Count =< 250
    end.

-spec is_effectively_active(guild_id(), session_data(), guild_state()) -> boolean().
is_effectively_active(GuildId, SessionData, State) ->
    (not is_passive(GuildId, SessionData)) orelse is_small_guild(State).

-spec is_message_event(event()) -> boolean().
is_message_event(message_create) -> true;
is_message_event(message_update) -> true;
is_message_event(message_delete) -> true;
is_message_event(message_delete_bulk) -> true;
is_message_event(_) -> false.

-spec is_lazy_guild_event(event()) -> boolean().
is_lazy_guild_event(Event) ->
    is_message_event(Event) orelse Event =:= voice_state_update.

-spec should_passive_receive(event(), map(), session_data()) -> boolean().
should_passive_receive(channel_create, _EventData, _SessionData) ->
    true;
should_passive_receive(channel_update, _EventData, _SessionData) ->
    true;
should_passive_receive(channel_update_bulk, _EventData, _SessionData) ->
    true;
should_passive_receive(channel_delete, _EventData, _SessionData) ->
    true;
should_passive_receive(message_create, EventData, SessionData) ->
    Mentioned = is_user_mentioned(EventData, SessionData),
    case Mentioned of
        true ->
            true;
        false ->
            false
    end;
should_passive_receive(message_update, EventData, SessionData) ->
    is_user_mentioned(EventData, SessionData);
should_passive_receive(guild_delete, _EventData, _SessionData) ->
    true;
should_passive_receive(guild_member_update, EventData, SessionData) ->
    is_session_user_event(EventData, SessionData);
should_passive_receive(guild_member_remove, EventData, SessionData) ->
    is_session_user_event(EventData, SessionData);
should_passive_receive(guild_audit_log_entry_create, _EventData, _SessionData) ->
    true;
should_passive_receive(passive_updates, _EventData, _SessionData) ->
    true;
should_passive_receive(_, _, _) ->
    false.

-spec is_session_user_event(map(), session_data()) -> boolean().
is_session_user_event(EventData, SessionData) ->
    UserId = maps:get(user_id, SessionData),
    UserId =:= event_user_id(EventData).

-spec event_user_id(map()) -> user_id() | undefined.
event_user_id(EventData) ->
    user_id(maps:get(<<"user">>, EventData, #{})).

-spec user_id(term()) -> user_id() | undefined.
user_id(User) when is_map(User) ->
    snowflake_id:parse_maybe(maps:get(<<"id">>, User, undefined));
user_id(_) ->
    undefined.

-spec is_user_mentioned(map(), session_data()) -> boolean().
is_user_mentioned(EventData, SessionData) ->
    UserId = maps:get(user_id, SessionData),
    RawMentionEveryone = maps:get(<<"mention_everyone">>, EventData, false),
    MentionHere = maps:get(<<"mention_here">>, EventData, false),
    MentionEveryone = RawMentionEveryone andalso not MentionHere,
    Mentions = maps:get(<<"mentions">>, EventData, []),
    MentionRoles = maps:get(<<"mention_roles">>, EventData, []),
    UserRoles = maps:get(user_roles, SessionData, []),
    MentionEveryone orelse
        MentionHere orelse
        is_user_in_mentions(UserId, Mentions) orelse
        has_mentioned_role(UserRoles, MentionRoles).

-spec is_user_in_mentions(user_id(), [map()]) -> boolean().
is_user_in_mentions(_UserId, []) ->
    false;
is_user_in_mentions(UserId, [Mention | Rest]) ->
    user_id(Mention) =:= UserId orelse is_user_in_mentions(UserId, Rest).

-spec has_mentioned_role([integer()], [binary() | integer()]) -> boolean().
has_mentioned_role([], _MentionRoles) ->
    false;
has_mentioned_role(_UserRoles, []) ->
    false;
has_mentioned_role(UserRoles, MentionRoles) ->
    MentionSet = mention_role_set(MentionRoles),
    lists:any(fun(RoleId) -> maps:is_key(RoleId, MentionSet) end, UserRoles).

-spec mention_role_set([binary() | integer()]) -> #{integer() => true}.
mention_role_set(MentionRoles) when length(MentionRoles) > ?MAX_MENTION_ROLES ->
    mention_role_set(lists:sublist(MentionRoles, ?MAX_MENTION_ROLES));
mention_role_set(MentionRoles) ->
    lists:foldl(fun add_mention_role/2, #{}, MentionRoles).

-spec add_mention_role(term(), #{integer() => true}) -> #{integer() => true}.
add_mention_role(R, Acc) when is_integer(R), R > 0 ->
    Acc#{R => true};
add_mention_role(R, Acc) when is_binary(R) ->
    case snowflake_id:parse_maybe(R) of
        Id when is_integer(Id), Id > 0 -> Acc#{Id => true};
        _ -> Acc
    end;
add_mention_role(_, Acc) ->
    Acc.

-spec get_user_roles_for_guild(user_id(), guild_state()) -> [integer()].
get_user_roles_for_guild(UserId, GuildState) ->
    case guild_permissions:find_member_by_user_id(UserId, GuildState) of
        undefined -> [];
        Member -> extract_role_ids(maps:get(<<"roles">>, Member, []))
    end.

-spec extract_role_ids([binary() | integer()]) -> [integer()].
extract_role_ids(Roles) ->
    lists:filtermap(fun parse_role_id/1, Roles).

-spec parse_role_id(term()) -> {true, integer()} | false.
parse_role_id(Role) when is_binary(Role) ->
    case validation:validate_snowflake(<<"role">>, Role) of
        {ok, RoleId} -> {true, RoleId};
        {error, _, _} -> false
    end;
parse_role_id(Role) when is_integer(Role) ->
    {true, Role};
parse_role_id(_) ->
    false.

-spec should_receive_typing(guild_id(), session_data()) -> boolean().
should_receive_typing(GuildId, SessionData) ->
    case get_typing_override(GuildId, SessionData) of
        undefined ->
            not is_passive(GuildId, SessionData);
        TypingFlag ->
            TypingFlag
    end.

-spec should_receive_typing(guild_id(), session_data(), guild_state()) -> boolean().
should_receive_typing(GuildId, SessionData, State) ->
    case get_typing_override(GuildId, SessionData) of
        undefined ->
            is_effectively_active(GuildId, SessionData, State);
        TypingFlag ->
            TypingFlag
    end.

-spec set_typing_override(guild_id(), boolean(), session_data()) -> session_data().
set_typing_override(GuildId, TypingFlag, SessionData) ->
    TypingOverrides = maps:get(typing_overrides, SessionData, #{}),
    NewTypingOverrides = TypingOverrides#{GuildId => TypingFlag},
    SessionData#{typing_overrides => NewTypingOverrides}.

-spec get_typing_override(guild_id(), session_data()) -> boolean() | undefined.
get_typing_override(GuildId, SessionData) ->
    TypingOverrides = maps:get(typing_overrides, SessionData, #{}),
    maps:get(GuildId, TypingOverrides, undefined).

-spec is_guild_synced(guild_id(), session_data()) -> boolean().
is_guild_synced(GuildId, SessionData) ->
    SyncedGuilds = maps:get(synced_guilds, SessionData, sets:new()),
    sets:is_element(GuildId, SyncedGuilds).

-spec mark_guild_synced(guild_id(), session_data()) -> session_data().
mark_guild_synced(GuildId, SessionData) ->
    SyncedGuilds = maps:get(synced_guilds, SessionData, sets:new()),
    NewSyncedGuilds = sets:add_element(GuildId, SyncedGuilds),
    SessionData#{synced_guilds => NewSyncedGuilds}.

-spec clear_guild_synced(guild_id(), session_data()) -> session_data().
clear_guild_synced(GuildId, SessionData) ->
    SyncedGuilds = maps:get(synced_guilds, SessionData, sets:new()),
    NewSyncedGuilds = sets:del_element(GuildId, SyncedGuilds),
    SessionData#{synced_guilds => NewSyncedGuilds}.
