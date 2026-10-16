%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild_voice_util).

-typing([eqwalizer]).

-export([handle_move_member_result/4, normalize_voice_rpc_error/1]).

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
        guild_voice_move:send_voice_server_updates_for_move(
            GuildId, ChannelId, VoiceSessionData, Pid
        )
    end),
    ok.

-spec cleanup_virtual_access_on_disconnect(integer(), pid()) -> ok.
cleanup_virtual_access_on_disconnect(DisconnectedUserId, Pid) ->
    spawn(fun() -> do_cleanup_virtual_access_on_disconnect(DisconnectedUserId, Pid) end),
    ok.

-spec voice_session_data([term()]) -> [map()].
voice_session_data(SessionData) ->
    [Entry || Entry <- SessionData, is_map(Entry)].

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

do_cleanup_virtual_access_on_disconnect(UserId, GuildPid) ->
    GuildId = resolve_guild_id_from_pid(GuildPid),
    TargetPid = guild_voice_server:resolve(GuildId, GuildPid),
    gen_server:cast(TargetPid, {cleanup_virtual_access_for_user, UserId}).

resolve_guild_id_from_pid(GuildPid) ->
    try guild_voice_server_state:guild_id_call(GuildPid, 5000) of
        GuildId when is_integer(GuildId) ->
            GuildId;
        _ ->
            undefined
    catch
        _:_ -> undefined
    end.
