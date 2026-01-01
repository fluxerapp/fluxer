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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

is_passive(GuildId, SessionData) ->
    case maps:get(bot, SessionData, false) of
        true ->
            false;
        false ->
            ActiveGuilds = maps:get(active_guilds, SessionData, sets:new()),
            not sets:is_element(GuildId, ActiveGuilds)
    end.

set_active(GuildId, SessionData) ->
    ActiveGuilds = maps:get(active_guilds, SessionData, sets:new()),
    NewActiveGuilds = sets:add_element(GuildId, ActiveGuilds),
    maps:put(active_guilds, NewActiveGuilds, SessionData).

set_passive(GuildId, SessionData) ->
    ActiveGuilds = maps:get(active_guilds, SessionData, sets:new()),
    NewActiveGuilds = sets:del_element(GuildId, ActiveGuilds),
    maps:put(active_guilds, NewActiveGuilds, SessionData).

should_receive_event(Event, EventData, GuildId, SessionData, State) ->
    case Event of
        typing_start ->
            should_receive_typing(GuildId, SessionData);
        _ ->
            case maps:get(bot, SessionData, false) of
                true ->
                    true;
                false ->
                    case is_message_event(Event) of
                        true ->
                            case is_small_guild(State) of
                                true ->
                                    true;
                                false ->
                                    case is_passive(GuildId, SessionData) of
                                        false -> true;
                                        true -> should_passive_receive(Event, EventData, SessionData)
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

is_small_guild(State) ->
    MemberCount = maps:get(member_count, State, undefined),
    case MemberCount of
        undefined -> false;  %% Conservative: treat as large
        Count when is_integer(Count) -> Count =< 250
    end.

is_message_event(message_create) -> true;
is_message_event(message_update) -> true;
is_message_event(message_delete) -> true;
is_message_event(message_delete_bulk) -> true;
is_message_event(_) -> false.

should_passive_receive(message_create, EventData, SessionData) ->
    is_user_mentioned(EventData, SessionData);
should_passive_receive(guild_delete, _EventData, _SessionData) ->
    true;
should_passive_receive(channel_create, _EventData, _SessionData) ->
    true;
should_passive_receive(channel_delete, _EventData, _SessionData) ->
    true;
should_passive_receive(passive_updates, _EventData, _SessionData) ->
    true;
should_passive_receive(guild_update, _EventData, _SessionData) ->
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
should_passive_receive(voice_state_update, EventData, SessionData) ->
    UserId = maps:get(user_id, SessionData),
    EventUserId = map_utils:get_integer(EventData, <<"user_id">>, undefined),
    UserId =:= EventUserId;
should_passive_receive(voice_server_update, _EventData, _SessionData) ->
    true;
should_passive_receive(_, _, _) ->
    false.

is_user_mentioned(EventData, SessionData) ->
    UserId = maps:get(user_id, SessionData),
    MentionEveryone = maps:get(<<"mention_everyone">>, EventData, false),
    Mentions = maps:get(<<"mentions">>, EventData, []),
    MentionRoles = maps:get(<<"mention_roles">>, EventData, []),
    UserRoles = maps:get(user_roles, SessionData, []),

    MentionEveryone orelse
        is_user_in_mentions(UserId, Mentions) orelse
        has_mentioned_role(UserRoles, MentionRoles).

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

has_mentioned_role([], _MentionRoles) ->
    false;
has_mentioned_role([RoleId | Rest], MentionRoles) ->
    RoleIdBin = integer_to_binary(RoleId),
    lists:member(RoleIdBin, MentionRoles) orelse
        lists:member(RoleId, MentionRoles) orelse
        has_mentioned_role(Rest, MentionRoles).

get_user_roles_for_guild(UserId, GuildState) ->
    Data = maps:get(data, GuildState, #{}),
    Members = maps:get(<<"members">>, Data, []),
    case find_member_by_user_id(UserId, Members) of
        undefined -> [];
        Member -> extract_role_ids(maps:get(<<"roles">>, Member, []))
    end.

find_member_by_user_id(_UserId, []) ->
    undefined;
find_member_by_user_id(UserId, [Member | Rest]) ->
    User = maps:get(<<"user">>, Member, #{}),
    MemberUserId = map_utils:get_integer(User, <<"id">>, undefined),
    case UserId =:= MemberUserId of
        true -> Member;
        false -> find_member_by_user_id(UserId, Rest)
    end.

extract_role_ids(Roles) ->
    lists:filtermap(
        fun(Role) when is_binary(Role) ->
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

should_receive_typing(GuildId, SessionData) ->
    case get_typing_override(GuildId, SessionData) of
        undefined ->
            not is_passive(GuildId, SessionData);
        TypingFlag ->
            TypingFlag
    end.

set_typing_override(GuildId, TypingFlag, SessionData) ->
    TypingOverrides = maps:get(typing_overrides, SessionData, #{}),
    NewTypingOverrides = maps:put(GuildId, TypingFlag, TypingOverrides),
    maps:put(typing_overrides, NewTypingOverrides, SessionData).

get_typing_override(GuildId, SessionData) ->
    TypingOverrides = maps:get(typing_overrides, SessionData, #{}),
    maps:get(GuildId, TypingOverrides, undefined).

is_guild_synced(GuildId, SessionData) ->
    SyncedGuilds = maps:get(synced_guilds, SessionData, sets:new()),
    sets:is_element(GuildId, SyncedGuilds).

mark_guild_synced(GuildId, SessionData) ->
    SyncedGuilds = maps:get(synced_guilds, SessionData, sets:new()),
    NewSyncedGuilds = sets:add_element(GuildId, SyncedGuilds),
    maps:put(synced_guilds, NewSyncedGuilds, SessionData).

clear_guild_synced(GuildId, SessionData) ->
    SyncedGuilds = maps:get(synced_guilds, SessionData, sets:new()),
    NewSyncedGuilds = sets:del_element(GuildId, SyncedGuilds),
    maps:put(synced_guilds, NewSyncedGuilds, SessionData).

-ifdef(TEST).

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
    ?assertEqual(true, should_receive_event(channel_create, #{}, 123, SessionData, State)),
    ok.

should_receive_event_passive_channel_delete_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 100},
    ?assertEqual(true, should_receive_event(channel_delete, #{}, 123, SessionData, State)),
    ok.

should_receive_event_passive_passive_updates_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 100},
    ?assertEqual(true, should_receive_event(passive_updates, #{}, 123, SessionData, State)),
    ok.

should_receive_event_passive_message_not_mentioned_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), user_roles => []},
    EventData = #{<<"mentions">> => [], <<"mention_roles">> => [], <<"mention_everyone">> => false},
    State = #{member_count => 300},  %% Large guild
    ?assertEqual(false, should_receive_event(message_create, EventData, 123, SessionData, State)),
    ok.

should_receive_event_passive_message_user_mentioned_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), user_roles => []},
    EventData = #{
        <<"mentions">> => [#{<<"id">> => <<"1">>}],
        <<"mention_roles">> => [],
        <<"mention_everyone">> => false
    },
    State = #{member_count => 300},  %% Large guild
    ?assertEqual(true, should_receive_event(message_create, EventData, 123, SessionData, State)),
    ok.

should_receive_event_passive_message_mention_everyone_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), user_roles => []},
    EventData = #{<<"mentions">> => [], <<"mention_roles">> => [], <<"mention_everyone">> => true},
    State = #{member_count => 300},  %% Large guild
    ?assertEqual(true, should_receive_event(message_create, EventData, 123, SessionData, State)),
    ok.

should_receive_event_passive_message_role_mentioned_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), user_roles => [100]},
    EventData = #{
        <<"mentions">> => [], <<"mention_roles">> => [<<"100">>], <<"mention_everyone">> => false
    },
    State = #{member_count => 300},  %% Large guild
    ?assertEqual(true, should_receive_event(message_create, EventData, 123, SessionData, State)),
    ok.

should_receive_event_passive_other_event_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 300},  %% Large guild
    ?assertEqual(false, should_receive_event(typing_start, #{}, 123, SessionData, State)),
    ?assertEqual(false, should_receive_event(message_update, #{}, 123, SessionData, State)),
    ok.

should_receive_event_small_guild_all_sessions_receive_messages_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 100},  %% Small guild
    ?assertEqual(true, should_receive_event(message_create, #{}, 123, SessionData, State)),
    ?assertEqual(true, should_receive_event(message_update, #{}, 123, SessionData, State)),
    ?assertEqual(true, should_receive_event(message_delete, #{}, 123, SessionData, State)),
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

-endif.
