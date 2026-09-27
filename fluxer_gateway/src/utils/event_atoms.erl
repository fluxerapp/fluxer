%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(event_atoms).
-typing([eqwalizer]).

-export([normalize/1]).

-spec normalize(binary() | atom()) -> atom() | binary().
normalize(Event) when is_atom(Event) ->
    Event;
normalize(EventBinary) when is_binary(EventBinary) ->
    normalize_binary(EventBinary).

-spec normalize_binary(binary()) -> atom() | binary().
normalize_binary(EventBinary) ->
    case maps:find(EventBinary, known_event_map()) of
        {ok, EventAtom} -> EventAtom;
        error -> normalize_existing_event_atom(EventBinary)
    end.

-spec normalize_existing_event_atom(binary()) -> atom() | binary().
normalize_existing_event_atom(EventBinary) ->
    Lowercase = string:lowercase(EventBinary),
    try
        binary_to_existing_atom(Lowercase, utf8)
    catch
        error:badarg ->
            EventBinary
    end.

-spec known_event_map() -> #{binary() => atom()}.
known_event_map() ->
    lists:foldl(
        fun maps:merge/2,
        #{},
        [
            core_event_map(),
            channel_event_map(),
            guild_event_map(),
            message_event_map(),
            user_voice_event_map()
        ]
    ).

-spec core_event_map() -> #{binary() => atom()}.
core_event_map() ->
    #{
        <<"AUTH_SESSION_CHANGE">> => auth_session_change,
        <<"ENTRANCE_SOUND_PLAY">> => entrance_sound_play,
        <<"FAVORITE_MEME_CREATE">> => favorite_meme_create,
        <<"FAVORITE_MEME_DELETE">> => favorite_meme_delete,
        <<"FAVORITE_MEME_UPDATE">> => favorite_meme_update,
        <<"INVITE_CREATE">> => invite_create,
        <<"INVITE_DELETE">> => invite_delete,
        <<"PRESENCE_UPDATE">> => presence_update,
        <<"READY">> => ready,
        <<"RECENT_MENTION_DELETE">> => recent_mention_delete,
        <<"RELATIONSHIP_ADD">> => relationship_add,
        <<"RELATIONSHIP_REMOVE">> => relationship_remove,
        <<"RELATIONSHIP_UPDATE">> => relationship_update,
        <<"RESUMED">> => resumed,
        <<"SAVED_MESSAGE_CREATE">> => saved_message_create,
        <<"SAVED_MESSAGE_DELETE">> => saved_message_delete,
        <<"SESSIONS_REPLACE">> => sessions_replace,
        <<"TYPING_START">> => typing_start
    }.

-spec channel_event_map() -> #{binary() => atom()}.
channel_event_map() ->
    #{
        <<"CHANNEL_CREATE">> => channel_create,
        <<"CHANNEL_DELETE">> => channel_delete,
        <<"CHANNEL_PINS_ACK">> => channel_pins_ack,
        <<"CHANNEL_PINS_UPDATE">> => channel_pins_update,
        <<"CHANNEL_RECIPIENT_ADD">> => channel_recipient_add,
        <<"CHANNEL_RECIPIENT_REMOVE">> => channel_recipient_remove,
        <<"CHANNEL_UPDATE">> => channel_update,
        <<"CHANNEL_UPDATE_BULK">> => channel_update_bulk
    }.

-spec guild_event_map() -> #{binary() => atom()}.
guild_event_map() ->
    #{
        <<"GUILD_AUDIT_LOG_ENTRY_CREATE">> => guild_audit_log_entry_create,
        <<"GUILD_BAN_ADD">> => guild_ban_add,
        <<"GUILD_BAN_REMOVE">> => guild_ban_remove,
        <<"GUILD_CREATE">> => guild_create,
        <<"GUILD_DELETE">> => guild_delete,
        <<"GUILD_EMOJIS_UPDATE">> => guild_emojis_update,
        <<"GUILD_MEMBER_ADD">> => guild_member_add,
        <<"GUILD_MEMBER_LIST_UPDATE">> => guild_member_list_update,
        <<"GUILD_MEMBER_REMOVE">> => guild_member_remove,
        <<"GUILD_MEMBER_UPDATE">> => guild_member_update,
        <<"GUILD_ROLE_CREATE">> => guild_role_create,
        <<"GUILD_ROLE_DELETE">> => guild_role_delete,
        <<"GUILD_ROLE_UPDATE">> => guild_role_update,
        <<"GUILD_ROLE_UPDATE_BULK">> => guild_role_update_bulk,
        <<"GUILD_STICKERS_UPDATE">> => guild_stickers_update,
        <<"GUILD_UPDATE">> => guild_update
    }.

-spec message_event_map() -> #{binary() => atom()}.
message_event_map() ->
    #{
        <<"MESSAGE_ACK">> => message_ack,
        <<"MESSAGE_CREATE">> => message_create,
        <<"MESSAGE_DELETE">> => message_delete,
        <<"MESSAGE_DELETE_BULK">> => message_delete_bulk,
        <<"MESSAGE_REACTION_ADD">> => message_reaction_add,
        <<"MESSAGE_REACTION_ADD_MANY">> => message_reaction_add_many,
        <<"MESSAGE_REACTION_REMOVE">> => message_reaction_remove,
        <<"MESSAGE_REACTION_REMOVE_ALL">> => message_reaction_remove_all,
        <<"MESSAGE_REACTION_REMOVE_EMOJI">> => message_reaction_remove_emoji,
        <<"MESSAGE_POLL_VOTE_ADD">> => message_poll_vote_add,
        <<"MESSAGE_POLL_VOTE_REMOVE">> => message_poll_vote_remove,
        <<"MESSAGE_UPDATE">> => message_update
    }.

-spec user_voice_event_map() -> #{binary() => atom()}.
user_voice_event_map() ->
    #{
        <<"USER_CONNECTIONS_UPDATE">> => user_connections_update,
        <<"USER_GUILD_SETTINGS_UPDATE">> => user_guild_settings_update,
        <<"USER_NOTE_UPDATE">> => user_note_update,
        <<"USER_PINNED_DMS_UPDATE">> => user_pinned_dms_update,
        <<"USER_SETTINGS_UPDATE">> => user_settings_update,
        <<"USER_UPDATE">> => user_update,
        <<"VOICE_SERVER_UPDATE">> => voice_server_update,
        <<"VOICE_STATE_UPDATE">> => voice_state_update,
        <<"WEBAUTHN_CREDENTIALS_UPDATE">> => webauthn_credentials_update
    }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

normalize_atom_test() ->
    ?assertEqual(test_event, normalize(test_event)),
    ?assertEqual(message_create, normalize(message_create)).

normalize_binary_existing_atom_test() ->
    _ = message_create,
    ?assertEqual(message_create, normalize(<<"MESSAGE_CREATE">>)),
    ?assertEqual(message_create, normalize(<<"message_create">>)).

normalize_known_private_event_test() ->
    ?assertEqual(user_guild_settings_update, normalize(<<"USER_GUILD_SETTINGS_UPDATE">>)),
    ?assertEqual(user_note_update, normalize(<<"USER_NOTE_UPDATE">>)).

normalize_binary_unknown_test() ->
    Result = normalize(<<"UNKNOWN_EVENT_XYZ_12345">>),
    ?assertEqual(<<"UNKNOWN_EVENT_XYZ_12345">>, Result).

-endif.
