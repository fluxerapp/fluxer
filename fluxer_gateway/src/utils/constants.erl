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

-module(constants).

-export([
    gateway_opcode/1,
    opcode_to_num/1,
    close_code_to_num/1,
    dispatch_event_atom/1,
    status_type_atom/1,
    max_payload_size/0,
    heartbeat_interval/0,
    heartbeat_timeout/0,
    random_session_bytes/0,
    view_channel_permission/0,
    administrator_permission/0,
    manage_roles_permission/0,
    manage_channels_permission/0,
    connect_permission/0,
    speak_permission/0,
    stream_permission/0,
    use_vad_permission/0,
    kick_members_permission/0,
    ban_members_permission/0
]).

gateway_opcode(0) -> dispatch;
gateway_opcode(1) -> heartbeat;
gateway_opcode(2) -> identify;
gateway_opcode(3) -> presence_update;
gateway_opcode(4) -> voice_state_update;
gateway_opcode(5) -> voice_server_ping;
gateway_opcode(6) -> resume;
gateway_opcode(7) -> reconnect;
gateway_opcode(8) -> request_guild_members;
gateway_opcode(9) -> invalid_session;
gateway_opcode(10) -> hello;
gateway_opcode(11) -> heartbeat_ack;
gateway_opcode(12) -> gateway_error;
gateway_opcode(13) -> call_connect;
gateway_opcode(14) -> lazy_request;
gateway_opcode(_) -> unknown.

opcode_to_num(dispatch) -> 0;
opcode_to_num(heartbeat) -> 1;
opcode_to_num(identify) -> 2;
opcode_to_num(presence_update) -> 3;
opcode_to_num(voice_state_update) -> 4;
opcode_to_num(voice_server_ping) -> 5;
opcode_to_num(resume) -> 6;
opcode_to_num(reconnect) -> 7;
opcode_to_num(request_guild_members) -> 8;
opcode_to_num(invalid_session) -> 9;
opcode_to_num(hello) -> 10;
opcode_to_num(heartbeat_ack) -> 11;
opcode_to_num(gateway_error) -> 12;
opcode_to_num(call_connect) -> 13;
opcode_to_num(lazy_request) -> 14.

close_code_to_num(unknown_error) -> 4000;
close_code_to_num(unknown_opcode) -> 4001;
close_code_to_num(decode_error) -> 4002;
close_code_to_num(not_authenticated) -> 4003;
close_code_to_num(authentication_failed) -> 4004;
close_code_to_num(already_authenticated) -> 4005;
close_code_to_num(invalid_seq) -> 4007;
close_code_to_num(rate_limited) -> 4008;
close_code_to_num(session_timeout) -> 4009;
close_code_to_num(invalid_shard) -> 4010;
close_code_to_num(sharding_required) -> 4011;
close_code_to_num(invalid_api_version) -> 4012.

dispatch_event_atom(<<"READY">>) ->
    ready;
dispatch_event_atom(<<"RESUMED">>) ->
    resumed;
dispatch_event_atom(<<"SESSIONS_REPLACE">>) ->
    sessions_replace;
dispatch_event_atom(<<"USER_UPDATE">>) ->
    user_update;
dispatch_event_atom(<<"USER_SETTINGS_UPDATE">>) ->
    user_settings_update;
dispatch_event_atom(<<"USER_GUILD_SETTINGS_UPDATE">>) ->
    user_guild_settings_update;
dispatch_event_atom(<<"USER_PINNED_DMS_UPDATE">>) ->
    user_pinned_dms_update;
dispatch_event_atom(<<"USER_NOTE_UPDATE">>) ->
    user_note_update;
dispatch_event_atom(<<"RECENT_MENTION_DELETE">>) ->
    recent_mention_delete;
dispatch_event_atom(<<"SAVED_MESSAGE_CREATE">>) ->
    saved_message_create;
dispatch_event_atom(<<"SAVED_MESSAGE_DELETE">>) ->
    saved_message_delete;
dispatch_event_atom(<<"AUTH_SESSION_CHANGE">>) ->
    auth_session_change;
dispatch_event_atom(<<"PRESENCE_UPDATE">>) ->
    presence_update;
dispatch_event_atom(<<"GUILD_CREATE">>) ->
    guild_create;
dispatch_event_atom(<<"GUILD_UPDATE">>) ->
    guild_update;
dispatch_event_atom(<<"GUILD_DELETE">>) ->
    guild_delete;
dispatch_event_atom(<<"GUILD_MEMBER_ADD">>) ->
    guild_member_add;
dispatch_event_atom(<<"GUILD_MEMBER_UPDATE">>) ->
    guild_member_update;
dispatch_event_atom(<<"GUILD_MEMBER_REMOVE">>) ->
    guild_member_remove;
dispatch_event_atom(<<"GUILD_ROLE_CREATE">>) ->
    guild_role_create;
dispatch_event_atom(<<"GUILD_ROLE_UPDATE">>) ->
    guild_role_update;
dispatch_event_atom(<<"GUILD_ROLE_UPDATE_BULK">>) ->
    guild_role_update_bulk;
dispatch_event_atom(<<"GUILD_ROLE_DELETE">>) ->
    guild_role_delete;
dispatch_event_atom(<<"GUILD_EMOJIS_UPDATE">>) ->
    guild_emojis_update;
dispatch_event_atom(<<"GUILD_STICKERS_UPDATE">>) ->
    guild_stickers_update;
dispatch_event_atom(<<"GUILD_BAN_ADD">>) ->
    guild_ban_add;
dispatch_event_atom(<<"GUILD_BAN_REMOVE">>) ->
    guild_ban_remove;
dispatch_event_atom(<<"GUILD_MEMBERS_CHUNK">>) ->
    guild_members_chunk;
dispatch_event_atom(<<"CHANNEL_CREATE">>) ->
    channel_create;
dispatch_event_atom(<<"CHANNEL_UPDATE">>) ->
    channel_update;
dispatch_event_atom(<<"CHANNEL_UPDATE_BULK">>) ->
    channel_update_bulk;
dispatch_event_atom(<<"PASSIVE_UPDATES">>) ->
    passive_updates;
dispatch_event_atom(<<"CHANNEL_DELETE">>) ->
    channel_delete;
dispatch_event_atom(<<"CHANNEL_RECIPIENT_ADD">>) ->
    channel_recipient_add;
dispatch_event_atom(<<"CHANNEL_RECIPIENT_REMOVE">>) ->
    channel_recipient_remove;
dispatch_event_atom(<<"CHANNEL_PINS_UPDATE">>) ->
    channel_pins_update;
dispatch_event_atom(<<"CHANNEL_PINS_ACK">>) ->
    channel_pins_ack;
dispatch_event_atom(<<"INVITE_CREATE">>) ->
    invite_create;
dispatch_event_atom(<<"INVITE_DELETE">>) ->
    invite_delete;
dispatch_event_atom(<<"MESSAGE_CREATE">>) ->
    message_create;
dispatch_event_atom(<<"MESSAGE_UPDATE">>) ->
    message_update;
dispatch_event_atom(<<"MESSAGE_DELETE">>) ->
    message_delete;
dispatch_event_atom(<<"MESSAGE_DELETE_BULK">>) ->
    message_delete_bulk;
dispatch_event_atom(<<"MESSAGE_REACTION_ADD">>) ->
    message_reaction_add;
dispatch_event_atom(<<"MESSAGE_REACTION_REMOVE">>) ->
    message_reaction_remove;
dispatch_event_atom(<<"MESSAGE_REACTION_REMOVE_ALL">>) ->
    message_reaction_remove_all;
dispatch_event_atom(<<"MESSAGE_REACTION_REMOVE_EMOJI">>) ->
    message_reaction_remove_emoji;
dispatch_event_atom(<<"MESSAGE_ACK">>) ->
    message_ack;
dispatch_event_atom(<<"TYPING_START">>) ->
    typing_start;
dispatch_event_atom(<<"WEBHOOKS_UPDATE">>) ->
    webhooks_update;
dispatch_event_atom(<<"RELATIONSHIP_ADD">>) ->
    relationship_add;
dispatch_event_atom(<<"RELATIONSHIP_UPDATE">>) ->
    relationship_update;
dispatch_event_atom(<<"RELATIONSHIP_REMOVE">>) ->
    relationship_remove;
dispatch_event_atom(<<"VOICE_STATE_UPDATE">>) ->
    voice_state_update;
dispatch_event_atom(<<"VOICE_SERVER_UPDATE">>) ->
    voice_server_update;
dispatch_event_atom(<<"FAVORITE_MEME_CREATE">>) ->
    favorite_meme_create;
dispatch_event_atom(<<"FAVORITE_MEME_UPDATE">>) ->
    favorite_meme_update;
dispatch_event_atom(<<"FAVORITE_MEME_DELETE">>) ->
    favorite_meme_delete;
dispatch_event_atom(<<"CALL_CREATE">>) ->
    call_create;
dispatch_event_atom(<<"CALL_UPDATE">>) ->
    call_update;
dispatch_event_atom(<<"CALL_DELETE">>) ->
    call_delete;
dispatch_event_atom(<<"GUILD_MEMBER_LIST_UPDATE">>) ->
    guild_member_list_update;
dispatch_event_atom(<<"GUILD_SYNC">>) ->
    guild_sync;
dispatch_event_atom(ready) ->
    <<"READY">>;
dispatch_event_atom(resumed) ->
    <<"RESUMED">>;
dispatch_event_atom(sessions_replace) ->
    <<"SESSIONS_REPLACE">>;
dispatch_event_atom(user_update) ->
    <<"USER_UPDATE">>;
dispatch_event_atom(user_settings_update) ->
    <<"USER_SETTINGS_UPDATE">>;
dispatch_event_atom(user_guild_settings_update) ->
    <<"USER_GUILD_SETTINGS_UPDATE">>;
dispatch_event_atom(user_pinned_dms_update) ->
    <<"USER_PINNED_DMS_UPDATE">>;
dispatch_event_atom(user_note_update) ->
    <<"USER_NOTE_UPDATE">>;
dispatch_event_atom(recent_mention_delete) ->
    <<"RECENT_MENTION_DELETE">>;
dispatch_event_atom(saved_message_create) ->
    <<"SAVED_MESSAGE_CREATE">>;
dispatch_event_atom(saved_message_delete) ->
    <<"SAVED_MESSAGE_DELETE">>;
dispatch_event_atom(auth_session_change) ->
    <<"AUTH_SESSION_CHANGE">>;
dispatch_event_atom(presence_update) ->
    <<"PRESENCE_UPDATE">>;
dispatch_event_atom(guild_create) ->
    <<"GUILD_CREATE">>;
dispatch_event_atom(guild_update) ->
    <<"GUILD_UPDATE">>;
dispatch_event_atom(guild_delete) ->
    <<"GUILD_DELETE">>;
dispatch_event_atom(guild_member_add) ->
    <<"GUILD_MEMBER_ADD">>;
dispatch_event_atom(guild_member_update) ->
    <<"GUILD_MEMBER_UPDATE">>;
dispatch_event_atom(guild_member_remove) ->
    <<"GUILD_MEMBER_REMOVE">>;
dispatch_event_atom(guild_role_create) ->
    <<"GUILD_ROLE_CREATE">>;
dispatch_event_atom(guild_role_update) ->
    <<"GUILD_ROLE_UPDATE">>;
dispatch_event_atom(guild_role_update_bulk) ->
    <<"GUILD_ROLE_UPDATE_BULK">>;
dispatch_event_atom(guild_role_delete) ->
    <<"GUILD_ROLE_DELETE">>;
dispatch_event_atom(guild_emojis_update) ->
    <<"GUILD_EMOJIS_UPDATE">>;
dispatch_event_atom(guild_stickers_update) ->
    <<"GUILD_STICKERS_UPDATE">>;
dispatch_event_atom(guild_ban_add) ->
    <<"GUILD_BAN_ADD">>;
dispatch_event_atom(guild_ban_remove) ->
    <<"GUILD_BAN_REMOVE">>;
dispatch_event_atom(guild_members_chunk) ->
    <<"GUILD_MEMBERS_CHUNK">>;
dispatch_event_atom(channel_create) ->
    <<"CHANNEL_CREATE">>;
dispatch_event_atom(channel_update) ->
    <<"CHANNEL_UPDATE">>;
dispatch_event_atom(channel_update_bulk) ->
    <<"CHANNEL_UPDATE_BULK">>;
dispatch_event_atom(passive_updates) ->
    <<"PASSIVE_UPDATES">>;
dispatch_event_atom(channel_delete) ->
    <<"CHANNEL_DELETE">>;
dispatch_event_atom(channel_recipient_add) ->
    <<"CHANNEL_RECIPIENT_ADD">>;
dispatch_event_atom(channel_recipient_remove) ->
    <<"CHANNEL_RECIPIENT_REMOVE">>;
dispatch_event_atom(channel_pins_update) ->
    <<"CHANNEL_PINS_UPDATE">>;
dispatch_event_atom(channel_pins_ack) ->
    <<"CHANNEL_PINS_ACK">>;
dispatch_event_atom(invite_create) ->
    <<"INVITE_CREATE">>;
dispatch_event_atom(invite_delete) ->
    <<"INVITE_DELETE">>;
dispatch_event_atom(message_create) ->
    <<"MESSAGE_CREATE">>;
dispatch_event_atom(message_update) ->
    <<"MESSAGE_UPDATE">>;
dispatch_event_atom(message_delete) ->
    <<"MESSAGE_DELETE">>;
dispatch_event_atom(message_delete_bulk) ->
    <<"MESSAGE_DELETE_BULK">>;
dispatch_event_atom(message_reaction_add) ->
    <<"MESSAGE_REACTION_ADD">>;
dispatch_event_atom(message_reaction_remove) ->
    <<"MESSAGE_REACTION_REMOVE">>;
dispatch_event_atom(message_reaction_remove_all) ->
    <<"MESSAGE_REACTION_REMOVE_ALL">>;
dispatch_event_atom(message_reaction_remove_emoji) ->
    <<"MESSAGE_REACTION_REMOVE_EMOJI">>;
dispatch_event_atom(message_ack) ->
    <<"MESSAGE_ACK">>;
dispatch_event_atom(typing_start) ->
    <<"TYPING_START">>;
dispatch_event_atom(webhooks_update) ->
    <<"WEBHOOKS_UPDATE">>;
dispatch_event_atom(relationship_add) ->
    <<"RELATIONSHIP_ADD">>;
dispatch_event_atom(relationship_update) ->
    <<"RELATIONSHIP_UPDATE">>;
dispatch_event_atom(relationship_remove) ->
    <<"RELATIONSHIP_REMOVE">>;
dispatch_event_atom(voice_state_update) ->
    <<"VOICE_STATE_UPDATE">>;
dispatch_event_atom(voice_server_update) ->
    <<"VOICE_SERVER_UPDATE">>;
dispatch_event_atom(favorite_meme_create) ->
    <<"FAVORITE_MEME_CREATE">>;
dispatch_event_atom(favorite_meme_update) ->
    <<"FAVORITE_MEME_UPDATE">>;
dispatch_event_atom(favorite_meme_delete) ->
    <<"FAVORITE_MEME_DELETE">>;
dispatch_event_atom(call_create) ->
    <<"CALL_CREATE">>;
dispatch_event_atom(call_update) ->
    <<"CALL_UPDATE">>;
dispatch_event_atom(call_delete) ->
    <<"CALL_DELETE">>;
dispatch_event_atom(guild_member_list_update) ->
    <<"GUILD_MEMBER_LIST_UPDATE">>;
dispatch_event_atom(guild_sync) ->
    <<"GUILD_SYNC">>;
dispatch_event_atom(EventBinary) when is_binary(EventBinary) -> EventBinary;
dispatch_event_atom(EventAtom) when is_atom(EventAtom) ->
    list_to_binary(string:uppercase(atom_to_list(EventAtom))).

status_type_atom(<<"online">>) -> online;
status_type_atom(<<"dnd">>) -> dnd;
status_type_atom(<<"idle">>) -> idle;
status_type_atom(<<"invisible">>) -> invisible;
status_type_atom(<<"offline">>) -> offline;
status_type_atom(online) -> <<"online">>;
status_type_atom(dnd) -> <<"dnd">>;
status_type_atom(idle) -> <<"idle">>;
status_type_atom(invisible) -> <<"invisible">>;
status_type_atom(offline) -> <<"offline">>.

max_payload_size() -> 4096.
heartbeat_interval() -> 41250.
heartbeat_timeout() -> 45000.
random_session_bytes() -> 16.
view_channel_permission() -> 1024.
administrator_permission() -> 8.
manage_roles_permission() -> 268435456.
manage_channels_permission() -> 16.
connect_permission() -> 1048576.
speak_permission() -> 2097152.
stream_permission() -> 512.
use_vad_permission() -> 33554432.
kick_members_permission() -> 2.
ban_members_permission() -> 4.
