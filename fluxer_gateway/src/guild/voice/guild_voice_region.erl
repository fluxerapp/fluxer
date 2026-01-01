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

-module(guild_voice_region).

-export([switch_voice_region_handler/2]).
-export([switch_voice_region/3]).

switch_voice_region_handler(Request, State) ->
    #{channel_id := ChannelId} = Request,

    Channel = guild_voice_member:find_channel_by_id(ChannelId, State),
    case Channel of
        undefined ->
            {reply, gateway_errors:error(voice_channel_not_found), State};
        _ ->
            ChannelType = maps:get(<<"type">>, Channel, 0),
            case ChannelType of
                2 ->
                    {reply, #{success => true}, State};
                _ ->
                    {reply, gateway_errors:error(voice_channel_not_voice), State}
            end
    end.

switch_voice_region(GuildId, ChannelId, GuildPid) ->
    case gen_server:call(GuildPid, {get_sessions}, 10000) of
        State when is_map(State) ->
            VoiceStates = voice_state_utils:voice_states(State),

            UsersInChannel = maps:fold(
                fun(ConnectionId, VoiceState, Acc) ->
                    case voice_state_utils:voice_state_channel_id(VoiceState) of
                        ChannelId ->
                            case voice_state_utils:voice_state_user_id(VoiceState) of
                                undefined ->
                                    logger:warning(
                                        "[guild_voice_region] Missing user_id for connection ~p",
                                        [ConnectionId]
                                    ),
                                    Acc;
                                UserId ->
                                    SessionId = maps:get(<<"session_id">>, VoiceState, undefined),
                                    [{UserId, SessionId, VoiceState} | Acc]
                            end;
                        _ ->
                            Acc
                    end
                end,
                [],
                VoiceStates
            ),

            lists:foreach(
                fun({UserId, SessionId, VoiceState}) ->
                    case SessionId of
                        undefined ->
                            ok;
                        _ ->
                            send_voice_server_update_for_region_switch(
                                GuildId, ChannelId, UserId, SessionId, VoiceState, GuildPid
                            )
                    end
                end,
                UsersInChannel
            );
        _ ->
            ok
    end.

send_voice_server_update_for_region_switch(
    GuildId, ChannelId, UserId, SessionId, ExistingVoiceState, GuildPid
) ->
    case gen_server:call(GuildPid, {get_sessions}, 10000) of
        State when is_map(State) ->
            VoicePermissions = voice_utils:compute_voice_permissions(UserId, ChannelId, State),
            case
                guild_voice_connection:request_voice_token(
                    GuildId, ChannelId, UserId, VoicePermissions
                )
            of
                {ok, TokenData} ->
                    Token = maps:get(token, TokenData),
                    Endpoint = maps:get(endpoint, TokenData),
                    ConnectionId = maps:get(connection_id, TokenData),

                    PendingMetadata = #{
                        user_id => UserId,
                        guild_id => GuildId,
                        channel_id => ChannelId,
                        session_id => SessionId,
                        self_mute => maps:get(<<"self_mute">>, ExistingVoiceState, false),
                        self_deaf => maps:get(<<"self_deaf">>, ExistingVoiceState, false),
                        self_video => maps:get(<<"self_video">>, ExistingVoiceState, false),
                        self_stream => maps:get(<<"self_stream">>, ExistingVoiceState, false),
                        is_mobile => maps:get(<<"is_mobile">>, ExistingVoiceState, false),
                        server_mute => maps:get(<<"mute">>, ExistingVoiceState, false),
                        server_deaf => maps:get(<<"deaf">>, ExistingVoiceState, false),
                        member => maps:get(<<"member">>, ExistingVoiceState, #{})
                    },
                    gen_server:cast(
                        GuildPid, {store_pending_connection, ConnectionId, PendingMetadata}
                    ),

                    guild_voice_broadcast:broadcast_voice_server_update_to_session(
                        GuildId, SessionId, Token, Endpoint, ConnectionId, State
                    );
                {error, _Reason} ->
                    ok
            end;
        _ ->
            ok
    end.
