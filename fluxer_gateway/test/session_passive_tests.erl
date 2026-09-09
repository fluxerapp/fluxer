%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_passive_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

is_passive_test() ->
    SessionData = #{active_guilds => sets:from_list([123, 456])},
    ?assertEqual(false, session_passive:is_passive(123, SessionData)),
    ?assertEqual(false, session_passive:is_passive(456, SessionData)),
    ?assertEqual(true, session_passive:is_passive(789, SessionData)),
    ?assertEqual(true, session_passive:is_passive(123, #{})),
    ok.

set_active_test() ->
    SessionData = #{active_guilds => sets:from_list([123])},
    NewSessionData = session_passive:set_active(456, SessionData),
    ?assertEqual(false, session_passive:is_passive(456, NewSessionData)),
    ?assertEqual(false, session_passive:is_passive(123, NewSessionData)),
    ok.

set_passive_test() ->
    SessionData = #{active_guilds => sets:from_list([123, 456])},
    NewSessionData = session_passive:set_passive(123, SessionData),
    ?assertEqual(true, session_passive:is_passive(123, NewSessionData)),
    ?assertEqual(false, session_passive:is_passive(456, NewSessionData)),
    ok.

should_receive_event_active_session_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:from_list([123])},
    State = #{member_count => 100},
    ?assertEqual(
        true, session_passive:should_receive_event(message_create, #{}, 123, SessionData, State)
    ),
    ?assertEqual(
        true, session_passive:should_receive_event(typing_start, #{}, 123, SessionData, State)
    ),
    ok.

should_receive_event_passive_guild_delete_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 100},
    ?assertEqual(
        true, session_passive:should_receive_event(guild_delete, #{}, 123, SessionData, State)
    ),
    ok.

should_receive_event_passive_guild_audit_log_entry_create_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 100},
    ?assertEqual(
        true,
        session_passive:should_receive_event(
            guild_audit_log_entry_create, #{}, 123, SessionData, State
        )
    ),
    ok.

should_receive_event_passive_channel_create_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 300},
    ?assertEqual(
        true,
        session_passive:should_receive_event(channel_create, #{}, 123, SessionData, State)
    ),
    ok.

should_receive_event_passive_channel_update_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 300},
    ?assertEqual(
        true,
        session_passive:should_receive_event(channel_update, #{}, 123, SessionData, State)
    ),
    ok.

should_receive_event_passive_channel_update_bulk_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 300},
    ?assertEqual(
        true,
        session_passive:should_receive_event(channel_update_bulk, #{}, 123, SessionData, State)
    ),
    ok.

should_receive_event_passive_channel_delete_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 300},
    ?assertEqual(
        true,
        session_passive:should_receive_event(channel_delete, #{}, 123, SessionData, State)
    ),
    ok.

should_receive_event_passive_structural_updates_bypass_passive_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), bot => false},
    State = #{member_count => 300},
    Events = [
        guild_update,
        guild_role_update,
        guild_role_update_bulk,
        channel_create,
        channel_update,
        channel_update_bulk,
        channel_delete
    ],
    lists:foreach(
        fun(Event) ->
            ?assertEqual(
                true,
                session_passive:should_receive_event(Event, #{}, 123, SessionData, State)
            )
        end,
        Events
    ),
    ok.

should_receive_event_self_guild_member_update_bypasses_passive_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), bot => false},
    State = #{member_count => 300},
    SelfUpdate = #{<<"user">> => #{<<"id">> => <<"1">>}},
    OtherUpdate = #{<<"user">> => #{<<"id">> => <<"2">>}},
    ?assertEqual(
        true,
        session_passive:should_receive_event(
            guild_member_update, SelfUpdate, 123, SessionData, State
        )
    ),
    ?assertEqual(
        false,
        session_passive:should_receive_event(
            guild_member_update, OtherUpdate, 123, SessionData, State
        )
    ),
    ok.

should_receive_event_passive_member_events_use_strict_user_id_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 300},
    ValidEventData = #{<<"user">> => #{<<"id">> => <<"1">>}},
    MalformedEventData = #{<<"user">> => #{<<"id">> => <<"001">>}},
    ?assertEqual(
        true,
        session_passive:should_receive_event(
            guild_member_update, ValidEventData, 123, SessionData, State
        )
    ),
    ?assertEqual(
        false,
        session_passive:should_receive_event(
            guild_member_update, MalformedEventData, 123, SessionData, State
        )
    ),
    ?assertEqual(
        false,
        session_passive:should_receive_event(
            guild_member_remove, MalformedEventData, 123, SessionData, State
        )
    ),
    ok.

should_receive_event_passive_passive_updates_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 100},
    ?assertEqual(
        true,
        session_passive:should_receive_event(passive_updates, #{}, 123, SessionData, State)
    ),
    ok.

should_receive_event_passive_message_not_mentioned_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), user_roles => []},
    EventData = #{
        <<"mentions">> => [], <<"mention_roles">> => [], <<"mention_everyone">> => false
    },
    State = #{member_count => 300},
    ?assertEqual(
        false,
        session_passive:should_receive_event(message_create, EventData, 123, SessionData, State)
    ),
    ok.

should_receive_event_passive_message_user_mentioned_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), user_roles => []},
    EventData = #{
        <<"mentions">> => [#{<<"id">> => <<"1">>}],
        <<"mention_roles">> => [],
        <<"mention_everyone">> => false
    },
    State = #{member_count => 300},
    ?assertEqual(
        true,
        session_passive:should_receive_event(message_create, EventData, 123, SessionData, State)
    ),
    ok.

should_receive_event_passive_message_update_user_mentioned_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), user_roles => []},
    EventData = #{
        <<"mentions">> => [#{<<"id">> => <<"1">>}],
        <<"mention_roles">> => [],
        <<"mention_everyone">> => false
    },
    State = #{member_count => 300},
    ?assertEqual(
        true,
        session_passive:should_receive_event(message_update, EventData, 123, SessionData, State)
    ),
    ok.

should_receive_event_passive_message_update_not_mentioned_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), user_roles => []},
    EventData = #{
        <<"mentions">> => [], <<"mention_roles">> => [], <<"mention_everyone">> => false
    },
    State = #{member_count => 300},
    ?assertEqual(
        false,
        session_passive:should_receive_event(message_update, EventData, 123, SessionData, State)
    ),
    ok.

should_receive_event_passive_message_mention_everyone_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), user_roles => []},
    EventData = #{
        <<"mentions">> => [], <<"mention_roles">> => [], <<"mention_everyone">> => true
    },
    State = #{member_count => 300},
    ?assertEqual(
        true,
        session_passive:should_receive_event(message_create, EventData, 123, SessionData, State)
    ),
    ok.

should_receive_event_passive_message_mention_here_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), user_roles => []},
    EventData = #{
        <<"mentions">> => [],
        <<"mention_roles">> => [],
        <<"mention_everyone">> => true,
        <<"mention_here">> => true
    },
    State = #{member_count => 300},
    ?assertEqual(
        true,
        session_passive:should_receive_event(message_create, EventData, 123, SessionData, State)
    ),
    ok.

should_receive_event_passive_message_role_mentioned_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new(), user_roles => [100]},
    EventData = #{
        <<"mentions">> => [],
        <<"mention_roles">> => [<<"100">>],
        <<"mention_everyone">> => false
    },
    State = #{member_count => 300},
    ?assertEqual(
        true,
        session_passive:should_receive_event(message_create, EventData, 123, SessionData, State)
    ),
    ok.

should_receive_event_passive_other_event_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 300},
    ?assertEqual(
        false, session_passive:should_receive_event(typing_start, #{}, 123, SessionData, State)
    ),
    ?assertEqual(
        false,
        session_passive:should_receive_event(message_update, #{}, 123, SessionData, State)
    ),
    ok.

should_receive_event_small_guild_sessions_are_effectively_active_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 100},
    ?assertEqual(
        true, session_passive:should_receive_event(message_create, #{}, 123, SessionData, State)
    ),
    ?assertEqual(
        true, session_passive:should_receive_event(message_update, #{}, 123, SessionData, State)
    ),
    ?assertEqual(
        true, session_passive:should_receive_event(message_delete, #{}, 123, SessionData, State)
    ),
    ?assertEqual(
        true, session_passive:should_receive_event(channel_create, #{}, 123, SessionData, State)
    ),
    ?assertEqual(
        true, session_passive:should_receive_event(typing_start, #{}, 123, SessionData, State)
    ),
    ok.

should_receive_event_small_guild_sessions_receive_voice_state_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 100},
    EventData = #{<<"user_id">> => <<"2">>},
    ?assertEqual(
        true,
        session_passive:should_receive_event(
            voice_state_update, EventData, 123, SessionData, State
        )
    ),
    ok.

should_receive_event_small_guild_typing_override_still_applies_test() ->
    GuildId = 123,
    State = #{member_count => 100},
    SessionData = session_passive:set_typing_override(
        GuildId, false, #{user_id => 1, active_guilds => sets:new()}
    ),
    ?assertEqual(
        false,
        session_passive:should_receive_event(typing_start, #{}, GuildId, SessionData, State)
    ),
    ok.

should_receive_event_passive_voice_state_blocked_test() ->
    SessionData = #{user_id => 1, active_guilds => sets:new()},
    State = #{member_count => 300},
    EventData = #{<<"user_id">> => <<"2">>},
    ?assertEqual(
        false,
        session_passive:should_receive_event(
            voice_state_update, EventData, 123, SessionData, State
        )
    ),
    ok.

is_passive_bot_always_active_test() ->
    BotSessionData = #{user_id => 1, active_guilds => sets:new(), bot => true},
    ?assertEqual(false, session_passive:is_passive(123, BotSessionData)),
    ?assertEqual(false, session_passive:is_passive(456, BotSessionData)),
    ?assertEqual(false, session_passive:is_passive(789, BotSessionData)),
    ok.

should_receive_event_bot_always_receives_test() ->
    BotSessionData = #{user_id => 1, active_guilds => sets:new(), bot => true},
    State = #{member_count => 300},
    ?assertEqual(
        true,
        session_passive:should_receive_event(message_create, #{}, 123, BotSessionData, State)
    ),
    ?assertEqual(
        true,
        session_passive:should_receive_event(typing_start, #{}, 123, BotSessionData, State)
    ),
    ?assertEqual(
        true,
        session_passive:should_receive_event(message_update, #{}, 123, BotSessionData, State)
    ),
    ?assertEqual(
        true,
        session_passive:should_receive_event(guild_delete, #{}, 123, BotSessionData, State)
    ),
    ok.

is_small_guild_test() ->
    ?assertEqual(true, session_passive:is_small_guild(#{member_count => 100})),
    ?assertEqual(true, session_passive:is_small_guild(#{member_count => 250})),
    ?assertEqual(false, session_passive:is_small_guild(#{member_count => 251})),
    ?assertEqual(false, session_passive:is_small_guild(#{member_count => 1000})),
    ?assertEqual(false, session_passive:is_small_guild(#{})),
    ok.

is_message_event_test() ->
    ?assertEqual(true, session_passive:is_message_event(message_create)),
    ?assertEqual(true, session_passive:is_message_event(message_update)),
    ?assertEqual(true, session_passive:is_message_event(message_delete)),
    ?assertEqual(true, session_passive:is_message_event(message_delete_bulk)),
    ?assertEqual(false, session_passive:is_message_event(typing_start)),
    ?assertEqual(false, session_passive:is_message_event(guild_create)),
    ok.

is_lazy_guild_event_test() ->
    ?assertEqual(true, session_passive:is_lazy_guild_event(message_create)),
    ?assertEqual(true, session_passive:is_lazy_guild_event(voice_state_update)),
    ?assertEqual(false, session_passive:is_lazy_guild_event(typing_start)),
    ?assertEqual(false, session_passive:is_lazy_guild_event(channel_create)),
    ok.

extract_role_ids_test() ->
    ?assertEqual([123], session_passive:extract_role_ids([<<"123">>])),
    ?assertEqual([456], session_passive:extract_role_ids([456])),
    ?assertEqual([123, 456], session_passive:extract_role_ids([<<"123">>, 456])),
    ?assertEqual([], session_passive:extract_role_ids([<<"invalid">>])),
    ok.

passive_to_active_transition_receives_all_events_test() ->
    GuildId = 999,
    SessionData0 = #{user_id => 1, active_guilds => sets:new(), bot => false},
    State = #{member_count => 300},
    ?assertEqual(
        false,
        session_passive:should_receive_event(typing_start, #{}, GuildId, SessionData0, State)
    ),
    SessionData1 = session_passive:set_active(GuildId, SessionData0),
    ?assertEqual(false, session_passive:is_passive(GuildId, SessionData1)),
    ?assertEqual(
        true,
        session_passive:should_receive_event(typing_start, #{}, GuildId, SessionData1, State)
    ),
    ?assertEqual(
        true,
        session_passive:should_receive_event(message_create, #{}, GuildId, SessionData1, State)
    ),
    ?assertEqual(
        true,
        session_passive:should_receive_event(channel_create, #{}, GuildId, SessionData1, State)
    ),
    ok.

active_to_passive_transition_filters_events_test() ->
    GuildId = 888,
    SessionData0 = #{user_id => 1, active_guilds => sets:from_list([GuildId]), bot => false},
    State = #{member_count => 300},
    ?assertEqual(false, session_passive:is_passive(GuildId, SessionData0)),
    ?assertEqual(
        true,
        session_passive:should_receive_event(channel_create, #{}, GuildId, SessionData0, State)
    ),
    SessionData1 = session_passive:set_passive(GuildId, SessionData0),
    ?assertEqual(true, session_passive:is_passive(GuildId, SessionData1)),
    ?assertEqual(
        true,
        session_passive:should_receive_event(channel_create, #{}, GuildId, SessionData1, State)
    ),
    ?assertEqual(
        false,
        session_passive:should_receive_event(typing_start, #{}, GuildId, SessionData1, State)
    ),
    ?assertEqual(
        true,
        session_passive:should_receive_event(guild_delete, #{}, GuildId, SessionData1, State)
    ),
    ok.

typing_override_survives_passive_active_toggle_test() ->
    GuildId = 777,
    SessionData0 = #{user_id => 1, active_guilds => sets:new(), bot => false},
    SessionData1 = session_passive:set_typing_override(GuildId, true, SessionData0),
    ?assertEqual(true, session_passive:should_receive_typing(GuildId, SessionData1)),
    SessionData2 = session_passive:set_active(GuildId, SessionData1),
    ?assertEqual(true, session_passive:should_receive_typing(GuildId, SessionData2)),
    SessionData3 = session_passive:set_passive(GuildId, SessionData2),
    ?assertEqual(true, session_passive:should_receive_typing(GuildId, SessionData3)),
    ok.

guild_synced_state_test() ->
    GuildId = 666,
    SessionData0 = #{},
    ?assertEqual(false, session_passive:is_guild_synced(GuildId, SessionData0)),
    SessionData1 = session_passive:mark_guild_synced(GuildId, SessionData0),
    ?assertEqual(true, session_passive:is_guild_synced(GuildId, SessionData1)),
    SessionData2 = session_passive:clear_guild_synced(GuildId, SessionData1),
    ?assertEqual(false, session_passive:is_guild_synced(GuildId, SessionData2)),
    ok.

mention_role_set_caps_at_100_test() ->
    Roles = lists:seq(1, 150),
    RoleSet = session_passive:mention_role_set(Roles),
    ?assertEqual(100, map_size(RoleSet)).

mention_role_set_under_cap_test() ->
    Roles = [1, 2, 3],
    RoleSet = session_passive:mention_role_set(Roles),
    ?assertEqual(3, map_size(RoleSet)),
    ?assert(maps:is_key(1, RoleSet)),
    ?assert(maps:is_key(2, RoleSet)),
    ?assert(maps:is_key(3, RoleSet)).
