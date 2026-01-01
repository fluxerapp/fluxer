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

-module(gateway_rpc_call).

-export([execute_method/2]).

execute_method(<<"call.get">>, #{<<"channel_id">> := ChannelIdBin}) ->
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    case gen_server:call(call_manager, {lookup, ChannelId}, 5000) of
        {ok, Pid} ->
            case gen_server:call(Pid, {get_state}, 5000) of
                {ok, CallData} ->
                    CallData;
                _ ->
                    throw({error, <<"Failed to get call state">>})
            end;
        {error, not_found} ->
            null;
        not_found ->
            null
    end;
execute_method(<<"call.create">>, Params) ->
    #{
        <<"channel_id">> := ChannelIdBin,
        <<"message_id">> := MessageIdBin,
        <<"region">> := Region,
        <<"ringing">> := RingingBins,
        <<"recipients">> := RecipientsBins
    } = Params,

    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    MessageId = validation:snowflake_or_throw(<<"message_id">>, MessageIdBin),
    Ringing = validation:snowflake_list_or_throw(<<"ringing">>, RingingBins),
    Recipients = validation:snowflake_list_or_throw(<<"recipients">>, RecipientsBins),

    CallData = #{
        channel_id => ChannelId,
        message_id => MessageId,
        region => Region,
        ringing => Ringing,
        recipients => Recipients
    },

    case gen_server:call(call_manager, {create, ChannelId, CallData}, 10000) of
        {ok, Pid} ->
            case gen_server:call(Pid, {get_state}, 5000) of
                {ok, CallState} ->
                    CallState;
                _ ->
                    throw({error, <<"Failed to get call state after creation">>})
            end;
        {error, already_exists} ->
            throw({error, <<"Call already exists">>});
        {error, Reason} ->
            throw({error, iolist_to_binary(io_lib:format("Failed to create call: ~p", [Reason]))})
    end;
execute_method(<<"call.update_region">>, #{
    <<"channel_id">> := ChannelIdBin, <<"region">> := Region
}) ->
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    case gen_server:call(call_manager, {lookup, ChannelId}, 5000) of
        {ok, Pid} ->
            case gen_server:call(Pid, {update_region, Region}, 5000) of
                ok ->
                    true;
                _ ->
                    throw({error, <<"Failed to update region">>})
            end;
        not_found ->
            throw({error, <<"Call not found">>})
    end;
execute_method(<<"call.ring">>, Params) ->
    #{<<"channel_id">> := ChannelIdBin, <<"recipients">> := RecipientsBin} = Params,

    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    Recipients = validation:snowflake_list_or_throw(<<"recipients">>, RecipientsBin),

    case gen_server:call(call_manager, {lookup, ChannelId}, 5000) of
        {ok, Pid} ->
            case gen_server:call(Pid, {ring_recipients, Recipients}, 5000) of
                ok ->
                    true;
                _ ->
                    throw({error, <<"Failed to ring recipients">>})
            end;
        not_found ->
            throw({error, <<"Call not found">>})
    end;
execute_method(<<"call.stop_ringing">>, Params) ->
    #{<<"channel_id">> := ChannelIdBin, <<"recipients">> := RecipientsBin} = Params,

    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    Recipients = validation:snowflake_list_or_throw(<<"recipients">>, RecipientsBin),

    case gen_server:call(call_manager, {lookup, ChannelId}, 5000) of
        {ok, Pid} ->
            case gen_server:call(Pid, {stop_ringing, Recipients}, 5000) of
                ok ->
                    true;
                _ ->
                    throw({error, <<"Failed to stop ringing">>})
            end;
        not_found ->
            throw({error, <<"Call not found">>})
    end;
execute_method(<<"call.join">>, Params) ->
    #{
        <<"channel_id">> := ChannelIdBin,
        <<"user_id">> := UserIdBin,
        <<"session_id">> := SessionIdBin,
        <<"voice_state">> := VoiceState
    } = Params,

    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    SessionId = SessionIdBin,

    case gen_server:call(session_manager, {lookup, SessionId}, 5000) of
        {ok, SessionPid} ->
            case gen_server:call(call_manager, {lookup, ChannelId}, 5000) of
                {ok, CallPid} ->
                    case
                        gen_server:call(
                            CallPid, {join, UserId, VoiceState, SessionId, SessionPid}, 5000
                        )
                    of
                        ok ->
                            true;
                        _ ->
                            throw({error, <<"Failed to join call">>})
                    end;
                not_found ->
                    throw({error, <<"Call not found">>})
            end;
        not_found ->
            throw({error, <<"Session not found">>})
    end;
execute_method(<<"call.leave">>, #{<<"channel_id">> := ChannelIdBin, <<"session_id">> := SessionId}) ->
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),

    case gen_server:call(call_manager, {lookup, ChannelId}, 5000) of
        {ok, Pid} ->
            case gen_server:call(Pid, {leave, SessionId}, 5000) of
                ok ->
                    true;
                _ ->
                    throw({error, <<"Failed to leave call">>})
            end;
        not_found ->
            throw({error, <<"Call not found">>})
    end;
execute_method(<<"call.delete">>, #{<<"channel_id">> := ChannelIdBin}) ->
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    case gen_server:call(call_manager, {terminate_call, ChannelId}, 5000) of
        ok ->
            true;
        {error, not_found} ->
            throw({error, <<"Call not found">>});
        _ ->
            throw({error, <<"Failed to delete call">>})
    end;
execute_method(<<"call.confirm_connection">>, Params) ->
    #{<<"channel_id">> := ChannelIdBin, <<"connection_id">> := ConnectionId} = Params,
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    logger:debug(
        "[gateway_rpc_call] call.confirm_connection channel_id=~p connection_id=~p",
        [ChannelId, ConnectionId]
    ),
    case gen_server:call(call_manager, {lookup, ChannelId}, 5000) of
        {ok, Pid} ->
            gen_server:call(Pid, {confirm_connection, ConnectionId}, 5000);
        {error, not_found} ->
            logger:debug(
                "[gateway_rpc_call] call.confirm_connection call not found for channel_id=~p", [
                    ChannelId
                ]
            ),
            #{success => true, call_not_found => true};
        not_found ->
            logger:debug(
                "[gateway_rpc_call] call.confirm_connection call manager returned not_found for channel_id=~p",
                [ChannelId]
            ),
            #{success => true, call_not_found => true}
    end;
execute_method(<<"call.disconnect_user_if_in_channel">>, Params) ->
    #{<<"channel_id">> := ChannelIdBin, <<"user_id">> := UserIdBin} = Params,
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    ConnectionId = maps:get(<<"connection_id">>, Params, undefined),
    case gen_server:call(call_manager, {lookup, ChannelId}, 5000) of
        {ok, Pid} ->
            gen_server:call(
                Pid, {disconnect_user_if_in_channel, UserId, ChannelId, ConnectionId}, 5000
            );
        {error, not_found} ->
            #{success => true, call_not_found => true};
        not_found ->
            #{success => true, call_not_found => true}
    end.
