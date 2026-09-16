%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild_voice_util).

-typing([eqwalizer]).

-export([
    handle_move_member_result/4,
    normalize_voice_rpc_error/1,
    parse_voice_update/1,
    process_voice_update/1
]).
-export_type([voice_update_payload/0]).

-define(GUILD_CALL_TIMEOUT, 4000).

-spec handle_move_member_result(term(), integer(), integer() | null, pid()) -> map().
handle_move_member_result(
    #{
        success := true,
        needs_token := true,
        session_data := SessionData,
        connections_to_move := _
    },
    GuildId,
    ChannelId,
    Pid
) when is_integer(ChannelId), is_list(SessionData) ->
    VoiceSessionData = voice_session_data(SessionData),
    send_voice_server_updates_for_move(GuildId, ChannelId, VoiceSessionData, Pid),
    #{<<"success">> => true};
handle_move_member_result(#{success := true, user_id := DisconnectedUserId}, _, null, Pid) when
    is_integer(DisconnectedUserId)
->
    cleanup_virtual_access_on_disconnect(DisconnectedUserId, Pid),
    #{<<"success">> => true};
handle_move_member_result(#{success := true}, _, _, _) ->
    #{<<"success">> => true};
handle_move_member_result({error, _Category, ErrorAtom}, _, _, _) ->
    #{<<"success">> => false, <<"error">> => normalize_voice_rpc_error(ErrorAtom)};
handle_move_member_result(#{error := Error}, _, _, _) ->
    #{<<"success">> => false, <<"error">> => normalize_voice_rpc_error(Error)};
handle_move_member_result(_, _, _, _) ->
    #{<<"success">> => false, <<"error">> => <<"move_member_error">>}.

-spec send_voice_server_updates_for_move(integer(), integer(), [map()], pid()) -> ok.
send_voice_server_updates_for_move(GuildId, ChannelId, VoiceSessionData, Pid) ->
    spawn(fun() ->
        guild_voice:send_voice_server_updates_for_move(
            GuildId, ChannelId, VoiceSessionData, Pid
        )
    end),
    ok.

-spec cleanup_virtual_access_on_disconnect(integer(), pid()) -> ok.
cleanup_virtual_access_on_disconnect(DisconnectedUserId, Pid) ->
    spawn(fun() -> guild_voice:cleanup_virtual_access_on_disconnect(DisconnectedUserId, Pid) end),
    ok.

-spec voice_session_data([term()]) -> [map()].
voice_session_data(SessionData) ->
    [Entry || Entry <- SessionData, is_map(Entry)].

-type voice_update_payload() :: #{binary() => term()}.

-spec parse_voice_update(voice_update_payload()) ->
    {integer(), integer(), boolean(), boolean(), term()}.
parse_voice_update(
    #{
        <<"guild_id">> := GuildIdBin,
        <<"user_id">> := UserIdBin,
        <<"mute">> := Mute,
        <<"deaf">> := Deaf
    } = Update
) when is_boolean(Mute), is_boolean(Deaf) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    {GuildId, UserId, Mute, Deaf, maps:get(<<"connection_id">>, Update, null)}.

-spec process_voice_update(term()) -> map().
process_voice_update({GuildId, UserId, Mute, Deaf, ConnectionId}) when
    is_integer(GuildId), is_integer(UserId), is_boolean(Mute), is_boolean(Deaf)
->
    case gateway_rpc_guild_infra:ensure_guild_pid(GuildId) of
        {ok, GuildPid} ->
            process_voice_update_for_guild(GuildId, UserId, Mute, Deaf, ConnectionId, GuildPid);
        _ ->
            voice_update_result(GuildId, UserId, false, <<"guild_not_found">>)
    end;
process_voice_update(_) ->
    #{<<"success">> => false, <<"error">> => <<"voice_update_error">>}.

-spec process_voice_update_for_guild(
    integer(), integer(), boolean(), boolean(), term(), pid()
) -> map().
process_voice_update_for_guild(GuildId, UserId, Mute, Deaf, ConnectionId, GuildPid) ->
    VoicePid = gateway_rpc_guild_infra:resolve_voice_pid(GuildId, GuildPid),
    Request = #{
        user_id => UserId,
        mute => Mute,
        deaf => Deaf,
        connection_id => ConnectionId
    },
    Result = gateway_rpc_guild_infra:safe_gen_server_call(
        VoicePid,
        {update_member_voice, Request},
        ?GUILD_CALL_TIMEOUT
    ),
    normalize_voice_update_result(GuildId, UserId, Result).

-spec normalize_voice_update_result(integer(), integer(), term()) -> map().
normalize_voice_update_result(GuildId, UserId, {ok, #{success := true}}) ->
    voice_update_result(GuildId, UserId, true, undefined);
normalize_voice_update_result(GuildId, UserId, {ok, #{error := Error}}) ->
    voice_update_result(GuildId, UserId, false, Error);
normalize_voice_update_result(GuildId, UserId, _Result) ->
    voice_update_result(GuildId, UserId, false, <<"voice_update_error">>).

-spec voice_update_result(integer(), integer(), boolean(), term()) -> map().
voice_update_result(GuildId, UserId, true, _Error) ->
    #{
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"user_id">> => integer_to_binary(UserId),
        <<"success">> => true
    };
voice_update_result(GuildId, UserId, false, Error) ->
    #{
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"user_id">> => integer_to_binary(UserId),
        <<"success">> => false,
        <<"error">> => Error
    }.

-spec normalize_voice_rpc_error(term()) -> binary().
normalize_voice_rpc_error(voice_user_not_in_voice) -> <<"user_not_in_voice">>;
normalize_voice_rpc_error(voice_channel_not_found) -> <<"channel_not_found">>;
normalize_voice_rpc_error(voice_channel_not_voice) -> <<"channel_not_voice">>;
normalize_voice_rpc_error(voice_moderator_missing_connect) -> <<"moderator_missing_connect">>;
normalize_voice_rpc_error(voice_permission_denied) -> <<"target_missing_connect">>;
normalize_voice_rpc_error(voice_connection_not_found) -> <<"connection_not_found">>;
normalize_voice_rpc_error(voice_missing_connection_id) -> <<"connection_not_found">>;
normalize_voice_rpc_error(Error) when is_binary(Error) -> normalize_binary(Error);
normalize_voice_rpc_error(Error) when is_atom(Error) -> atom_to_binary(Error, utf8);
normalize_voice_rpc_error(_) -> <<"move_member_error">>.

-spec normalize_binary(binary()) -> binary().
normalize_binary(<<"voice_user_not_in_voice">>) -> <<"user_not_in_voice">>;
normalize_binary(<<"voice_channel_not_found">>) -> <<"channel_not_found">>;
normalize_binary(<<"voice_channel_not_voice">>) -> <<"channel_not_voice">>;
normalize_binary(<<"voice_moderator_missing_connect">>) -> <<"moderator_missing_connect">>;
normalize_binary(<<"voice_permission_denied">>) -> <<"target_missing_connect">>;
normalize_binary(<<"voice_connection_not_found">>) -> <<"connection_not_found">>;
normalize_binary(<<"voice_missing_connection_id">>) -> <<"connection_not_found">>;
normalize_binary(Error) -> Error.
