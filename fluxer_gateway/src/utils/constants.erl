%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(constants).
-typing([eqwalizer]).

-export([
    gateway_opcode/1,
    opcode_to_num/1,
    close_code_to_num/1,
    dispatch_event_atom/1,
    status_type_atom/1,
    max_payload_size/0,
    heartbeat_interval/0,
    heartbeat_timeout/0,
    resume_timeout/0,
    random_session_bytes/0,
    view_channel_permission/0,
    view_audit_log_permission/0,
    administrator_permission/0,
    manage_roles_permission/0,
    manage_channels_permission/0,
    connect_permission/0,
    speak_permission/0,
    stream_permission/0,
    read_message_history_permission/0,
    kick_members_permission/0,
    ban_members_permission/0,
    view_channel_members_permission/0,
    voice_channel_camera_user_limit/0
]).

-spec gateway_opcode(integer()) -> atom().
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
gateway_opcode(14) -> lazy_request;
gateway_opcode(15) -> request_guild_counts;
gateway_opcode(16) -> request_channel_member_counts;
gateway_opcode(_) -> unknown.

-spec opcode_to_num(atom()) -> integer().
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
opcode_to_num(lazy_request) -> 14;
opcode_to_num(request_guild_counts) -> 15;
opcode_to_num(request_channel_member_counts) -> 16.

-spec close_code_to_num(atom()) -> integer().
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

-spec dispatch_event_atom(atom() | binary()) -> atom() | binary().
dispatch_event_atom(Event) when is_atom(Event) ->
    list_to_binary(string:uppercase(atom_to_list(Event)));
dispatch_event_atom(EventBinary) when is_binary(EventBinary) ->
    event_atoms:normalize(EventBinary).

-spec status_type_atom(binary() | atom()) -> atom() | binary().
status_type_atom(<<>>) -> online;
status_type_atom(<<"online">>) -> online;
status_type_atom(<<"dnd">>) -> dnd;
status_type_atom(<<"idle">>) -> idle;
status_type_atom(<<"invisible">>) -> invisible;
status_type_atom(<<"offline">>) -> offline;
status_type_atom(undefined) -> <<"online">>;
status_type_atom(null) -> <<"online">>;
status_type_atom(online) -> <<"online">>;
status_type_atom(dnd) -> <<"dnd">>;
status_type_atom(idle) -> <<"idle">>;
status_type_atom(invisible) -> <<"invisible">>;
status_type_atom(offline) -> <<"offline">>.

-spec max_payload_size() -> pos_integer().
max_payload_size() -> 4096.

-spec heartbeat_interval() -> pos_integer().
heartbeat_interval() -> 41250.

-spec heartbeat_timeout() -> pos_integer().
heartbeat_timeout() -> 45000.

-spec resume_timeout() -> pos_integer().
resume_timeout() -> 60000.

-spec random_session_bytes() -> pos_integer().
random_session_bytes() -> 16.

-spec view_channel_permission() -> pos_integer().
view_channel_permission() -> 1024.

-spec view_audit_log_permission() -> pos_integer().
view_audit_log_permission() -> 128.

-spec administrator_permission() -> pos_integer().
administrator_permission() -> 8.

-spec manage_roles_permission() -> pos_integer().
manage_roles_permission() -> 268435456.

-spec manage_channels_permission() -> pos_integer().
manage_channels_permission() -> 16.

-spec connect_permission() -> pos_integer().
connect_permission() -> 1048576.

-spec speak_permission() -> pos_integer().
speak_permission() -> 2097152.

-spec stream_permission() -> pos_integer().
stream_permission() -> 512.

-spec read_message_history_permission() -> pos_integer().
read_message_history_permission() -> 65536.

-spec kick_members_permission() -> pos_integer().
kick_members_permission() -> 2.

-spec ban_members_permission() -> pos_integer().
ban_members_permission() -> 4.

-spec view_channel_members_permission() -> pos_integer().
view_channel_members_permission() -> 18014398509481984.

-spec voice_channel_camera_user_limit() -> pos_integer().
voice_channel_camera_user_limit() -> 25.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

gateway_opcode_test() ->
    ?assertEqual(dispatch, gateway_opcode(0)),
    ?assertEqual(heartbeat, gateway_opcode(1)),
    ?assertEqual(identify, gateway_opcode(2)),
    ?assertEqual(unknown, gateway_opcode(999)).

opcode_to_num_test() ->
    ?assertEqual(0, opcode_to_num(dispatch)),
    ?assertEqual(1, opcode_to_num(heartbeat)),
    ?assertEqual(2, opcode_to_num(identify)).

close_code_to_num_test() ->
    ?assertEqual(4000, close_code_to_num(unknown_error)),
    ?assertEqual(4004, close_code_to_num(authentication_failed)),
    ?assertEqual(4008, close_code_to_num(rate_limited)),
    ?assertEqual(4012, close_code_to_num(invalid_api_version)),
    ?assertError(function_clause, close_code_to_num(ack_backpressure)).

status_type_atom_binary_to_atom_test() ->
    ?assertEqual(online, status_type_atom(<<>>)),
    ?assertEqual(online, status_type_atom(<<"online">>)),
    ?assertEqual(dnd, status_type_atom(<<"dnd">>)),
    ?assertEqual(idle, status_type_atom(<<"idle">>)),
    ?assertEqual(invisible, status_type_atom(<<"invisible">>)),
    ?assertEqual(offline, status_type_atom(<<"offline">>)).

status_type_atom_atom_to_binary_test() ->
    ?assertEqual(<<"online">>, status_type_atom(undefined)),
    ?assertEqual(<<"online">>, status_type_atom(null)),
    ?assertEqual(<<"online">>, status_type_atom(online)),
    ?assertEqual(<<"dnd">>, status_type_atom(dnd)),
    ?assertEqual(<<"idle">>, status_type_atom(idle)).

constants_values_test() ->
    ?assertEqual(4096, max_payload_size()),
    ?assertEqual(41250, heartbeat_interval()),
    ?assertEqual(45000, heartbeat_timeout()),
    ?assertEqual(60000, resume_timeout()),
    ?assertEqual(16, random_session_bytes()),
    ?assertEqual(1024, view_channel_permission()),
    ?assertEqual(128, view_audit_log_permission()),
    ?assertEqual(8, administrator_permission()).

-endif.
