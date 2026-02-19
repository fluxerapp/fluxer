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

-module(session_passive).

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
    is_guild_synced/2,
    mark_guild_synced/2,
    clear_guild_synced/2
]).

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
    maps:put(active_guilds, NewActiveGuilds, SessionData).

-spec set_passive(guild_id(), session_data()) -> session_data().
set_passive(GuildId, SessionData) ->
    ActiveGuilds = maps:get(active_guilds, SessionData, sets:new()),
    NewActiveGuilds = sets:del_element(GuildId, ActiveGuilds),
    maps:put(active_guilds, NewActiveGuilds, SessionData).

-spec should_receive_event(event(), map(), guild_id(), session_data(), guild_state()) -> boolean().
should_receive_event(Event, EventData, GuildId, SessionData, State) ->
    case Event of
        typing_start ->
            should_receive_typing(GuildId, SessionData);
        _ ->
            case maps:get(bot, SessionData, false) of
                true ->
                    true;
                false ->
                    case is_lazy_guild_event(Event) of
                        true ->
                            case is_small_guild(State) of
                                true ->
                                    true;
                                false ->
                                    case is_passive(GuildId, SessionData) of
                                        false ->
                                            true;
                                        true ->
                                            should_passive_receive(Event, EventData, SessionData)
                                    end
                            end;
                        false ->
                            case is_passive(GuildId, SessionData) of
                                false -> true;
                                true -> should_passive_receive(Event, EventData, SessionData)
                            end
                    end
            end
    end.

-spec is_small_guild(guild_state()) -> boolean().
is_small_guild(State) ->
    MemberCount = maps:get(member_count, State, undefined),
    case MemberCount of
        undefined -> false;
        Count when is_integer(Count) -> Count =< 250
    end.

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
should_passive_receive(message_create, EventData, SessionData) ->
    Mentioned = is_user_mentioned(EventData, SessionData),
    case Mentioned of
        true ->
            true;
        false ->
            false
    end;
should_passive_receive(guild_delete, _EventData, _SessionData) ->
    true;
should_passive_receive(guild_member_update, EventData, SessionData) ->
    UserId = maps:get(user_id, SessionData),
    MemberUser = maps:get(<<"user">>, EventData, #{}),
    MemberUserId = map_utils:get_integer(MemberUser, <<"id">>, undefined),
    UserId =:= MemberUserId;
should_passive_receive(guild_member_remove, EventData, SessionData) ->
    UserId = maps:get(user_id, SessionData),
    MemberUser = maps:get(<<"user">>, EventData, #{}),
    MemberUserId = map_utils:get_integer(MemberUser, <<"id">>, undefined),
    UserId =:= MemberUserId;
should_passive_receive(passive_updates, _EventData, _SessionData) ->
    true;
should_passive_receive(_, _, _) ->
    false.

-spec is_user_mentioned(map(), session_data()) -> boolean().
is_user_mentioned(EventData, SessionData) ->
    UserId = maps:get(user_id, SessionData),
    MentionEveryone = maps:get(<<"mention_everyone">>, EventData, false),
    Mentions = maps:get(<<"mentions">>, EventData, []),
    MentionRoles = maps:get(<<"mention_roles">>, EventData, []),
    UserRoles = maps:get(user_roles, SessionData, []),
    MentionEveryone orelse
        is_user_in_mentions(UserId, Mentions) orelse
        has_mentioned_role(UserRoles, MentionRoles).

-spec is_user_in_mentions(user_id(), [map()]) -> boolean().
is_user_in_mentions(_UserId, []) ->
    false;
is_user_in_mentions(UserId, [#{<<"id">> := Id} | Rest]) when is_binary(Id) ->
    case validation:validate_snowflake(<<"id">>, Id) of
        {ok, ParsedId} ->
            UserId =:= ParsedId orelse is_user_in_mentions(UserId, Rest);
        {error, _, _} ->
            is_user_in_mentions(UserId, Rest)
    end;
is_user_in_mentions(UserId, [_ | Rest]) ->
    is_user_in_mentions(UserId, Rest).

-spec has_mentioned_role([integer()], [binary() | integer()]) -> boolean().
has_mentioned_role([], _MentionRoles) ->
    false;
has_mentioned_role([RoleId | Rest], MentionRoles) ->
    RoleIdBin = integer_to_binary(RoleId),
    lists:member(RoleIdBin, MentionRoles) orelse
        lists:member(RoleId, MentionRoles) orelse
        has_mentioned_role(Rest, MentionRoles).

-spec get_user_roles_for_guild(user_id(), guild_state()) -> [integer()].
get_user_roles_for_guild(UserId, GuildState) ->
    case guild_permissions:find_member_by_user_id(UserId, GuildState) of
        undefined -> [];
        Member -> extract_role_ids(maps:get(<<"roles">>, Member, []))
    end.

-spec extract_role_ids([binary() | integer()]) -> [integer()].
extract_role_ids(Roles) ->
    lists:filtermap(
        fun
            (Role) when is_binary(Role) ->
                case validation:validate_snowflake(<<"role">>, Role) of
                    {ok, RoleId} -> {true, RoleId};
                    {error, _, _} -> false
                end;
            (Role) when is_integer(Role) ->
                {true, Role};
            (_) ->
                false
        end,
        Roles
    ).

-spec should_receive_typing(guild_id(), session_data()) -> boolean().
should_receive_typing(GuildId, SessionData) ->
    case get_typing_override(GuildId, SessionData) of
        undefined ->
            not is_passive(GuildId, SessionData);
        TypingFlag ->
            TypingFlag
    end.

-spec set_typing_override(guild_id(), boolean(), session_data()) -> session_data().
set_typing_override(GuildId, TypingFlag, SessionData) ->
    TypingOverrides = maps:get(typing_overrides, SessionData, #{}),
    NewTypingOverrides = maps:put(GuildId, TypingFlag, TypingOverrides),
    maps:put(typing_overrides, NewTypingOverrides, SessionData).

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
    maps:put(synced_guilds, NewSyncedGuilds, SessionData).

-spec clear_guild_synced(guild_id(), session_data()) -> session_data().
clear_guild_synced(GuildId, SessionData) ->
    SyncedGuilds = maps:get(synced_guilds, SessionData, sets:new()),
    NewSyncedGuilds = sets:del_element(GuildId, SyncedGuilds),
    maps:put(synced_guilds, NewSyncedGuilds, SessionData).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_passive_test() ->
    SessionData = #{active_guilds => sets:from_list([123, 456])},
    ?assertEqual(false, is_passive(123, SessionData)),
    ?assertEqual(false, is_passive(456, SessionData)),
    ?assertEqual(true, is_passive(789, SessionData)),
    ?assertEqual(true, is_passive(123, #{})),
    ok.

set_active_test() ->
    SessionData = #{active_guilds => sets:from_list([123])},
    NewSessionData = set_active(456, SessionData),
    ?assertEqual(false, is_passive(456, NewSessionData)),
    ?assertEqual(false, is_passive(123, NewSessionData)),
    ok.

set_passive_test() ->
    SessionData = #{active_guilds => sets:from_list([123, 456])},
    NewSessionData = set_passive(123, SessionData),
    ?assertEqual(true, is_passive(123, NewSessionData)),
    ?assertEqual(false, is_passive(456, NewSessionData)),
    ok.

should_receive_event_active_session_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:from_list([123])},
    State = #{member_count => 100},
    ?assertEqual(true, should_receive_event(message_create, #{}, 123, SessionData, State)),
    ?assertEqual(true, should_receive_event(typing_start, #{}, 123, SessionData, State)),
    ok.

should_receive_event_passive_guild_delete_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 100},
    ?assertEqual(true, should_receive_event(guild_delete, #{}, 123, SessionData, State)),
    ok.

should_receive_event_passive_channel_create_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 100},
    ?assertEqual(false, should_receive_event(channel_create, #{}, 123, SessionData, State)),
    ok.

should_receive_event_passive_channel_delete_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 100},
    ?assertEqual(false, should_receive_event(channel_delete, #{}, 123, SessionData, State)),
    ok.

should_receive_event_passive_passive_updates_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 100},
    ?assertEqual(true, should_receive_event(passive_updates, #{}, 123, SessionData, State)),
    ok.

should_receive_event_passive_message_not_mentioned_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), user_roles => []},
    EventData = #{<<"mentions">> => [], <<"mention_roles">> => [], <<"mention_everyone">> => false},
    State = #{member_count => 300},
    ?assertEqual(false, should_receive_event(message_create, EventData, 123, SessionData, State)),
    ok.

should_receive_event_passive_message_user_mentioned_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), user_roles => []},
    EventData = #{
        <<"mentions">> => [#{<<"id">> => <<"1">>}],
        <<"mention_roles">> => [],
        <<"mention_everyone">> => false
    },
    State = #{member_count => 300},
    ?assertEqual(true, should_receive_event(message_create, EventData, 123, SessionData, State)),
    ok.

should_receive_event_passive_message_mention_everyone_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), user_roles => []},
    EventData = #{<<"mentions">> => [], <<"mention_roles">> => [], <<"mention_everyone">> => true},
    State = #{member_count => 300},
    ?assertEqual(true, should_receive_event(message_create, EventData, 123, SessionData, State)),
    ok.

should_receive_event_passive_message_role_mentioned_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), user_roles => [100]},
    EventData = #{
        <<"mentions">> => [], <<"mention_roles">> => [<<"100">>], <<"mention_everyone">> => false
    },
    State = #{member_count => 300},
    ?assertEqual(true, should_receive_event(message_create, EventData, 123, SessionData, State)),
    ok.

should_receive_event_passive_other_event_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 300},
    ?assertEqual(false, should_receive_event(typing_start, #{}, 123, SessionData, State)),
    ?assertEqual(false, should_receive_event(message_update, #{}, 123, SessionData, State)),
    ok.

should_receive_event_small_guild_all_sessions_receive_messages_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 100},
    ?assertEqual(true, should_receive_event(message_create, #{}, 123, SessionData, State)),
    ?assertEqual(true, should_receive_event(message_update, #{}, 123, SessionData, State)),
    ?assertEqual(true, should_receive_event(message_delete, #{}, 123, SessionData, State)),
    ok.

should_receive_event_small_guild_voice_state_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 100},
    EventData = #{<<"user_id">> => <<"2">>},
    ?assertEqual(
        true, should_receive_event(voice_state_update, EventData, 123, SessionData, State)
    ),
    ok.

should_receive_event_passive_voice_state_blocked_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 300},
    EventData = #{<<"user_id">> => <<"2">>},
    ?assertEqual(
        false, should_receive_event(voice_state_update, EventData, 123, SessionData, State)
    ),
    ok.

is_passive_bot_always_active_test() ->
    BotSessionData = #{user_id => 1, active_guilds => sets:new(), bot => true},
    ?assertEqual(false, is_passive(123, BotSessionData)),
    ?assertEqual(false, is_passive(456, BotSessionData)),
    ?assertEqual(false, is_passive(789, BotSessionData)),
    ok.

should_receive_event_bot_always_receives_test() ->
    BotSessionData = #{user_id => 1, active_guilds => sets:new(), bot => true},
    State = #{member_count => 300},
    ?assertEqual(true, should_receive_event(message_create, #{}, 123, BotSessionData, State)),
    ?assertEqual(true, should_receive_event(typing_start, #{}, 123, BotSessionData, State)),
    ?assertEqual(true, should_receive_event(message_update, #{}, 123, BotSessionData, State)),
    ?assertEqual(true, should_receive_event(guild_delete, #{}, 123, BotSessionData, State)),
    ok.

is_small_guild_test() ->
    ?assertEqual(true, is_small_guild(#{member_count => 100})),
    ?assertEqual(true, is_small_guild(#{member_count => 250})),
    ?assertEqual(false, is_small_guild(#{member_count => 251})),
    ?assertEqual(false, is_small_guild(#{member_count => 1000})),
    ?assertEqual(false, is_small_guild(#{})),
    ok.

is_message_event_test() ->
    ?assertEqual(true, is_message_event(message_create)),
    ?assertEqual(true, is_message_event(message_update)),
    ?assertEqual(true, is_message_event(message_delete)),
    ?assertEqual(true, is_message_event(message_delete_bulk)),
    ?assertEqual(false, is_message_event(typing_start)),
    ?assertEqual(false, is_message_event(guild_create)),
    ok.

is_lazy_guild_event_test() ->
    ?assertEqual(true, is_lazy_guild_event(message_create)),
    ?assertEqual(true, is_lazy_guild_event(voice_state_update)),
    ?assertEqual(false, is_lazy_guild_event(typing_start)),
    ?assertEqual(false, is_lazy_guild_event(channel_create)),
    ok.

extract_role_ids_test() ->
    ?assertEqual([123], extract_role_ids([<<"123">>])),
    ?assertEqual([456], extract_role_ids([456])),
    ?assertEqual([123, 456], extract_role_ids([<<"123">>, 456])),
    ?assertEqual([], extract_role_ids([<<"invalid">>])),
    ok.

-endif.
