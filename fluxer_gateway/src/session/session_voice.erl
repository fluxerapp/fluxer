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

-module(session_voice).

-export([
    handle_voice_state_update/2,
    handle_voice_disconnect/1,
    handle_voice_token_request/8
]).

handle_voice_state_update(Data, State) ->
    GuildIdRaw = maps:get(<<"guild_id">>, Data, null),
    ChannelIdRaw = maps:get(<<"channel_id">>, Data, null),
    ConnectionId = maps:get(<<"connection_id">>, Data, null),
    SelfMute = maps:get(<<"self_mute">>, Data, false),
    SelfDeaf = maps:get(<<"self_deaf">>, Data, false),
    SelfVideo = maps:get(<<"self_video">>, Data, false),
    SelfStream = maps:get(<<"self_stream">>, Data, false),
    ViewerStreamKey = maps:get(<<"viewer_stream_key">>, Data, undefined),
    IsMobile = maps:get(<<"is_mobile">>, Data, false),
    Latitude = maps:get(<<"latitude">>, Data, null),
    Longitude = maps:get(<<"longitude">>, Data, null),

    SessionId = maps:get(id, State),
    UserId = maps:get(user_id, State),
    Guilds = maps:get(guilds, State),

    GuildIdResult = validation:validate_optional_snowflake(GuildIdRaw),
    ChannelIdResult = validation:validate_optional_snowflake(ChannelIdRaw),

    case {GuildIdResult, ChannelIdResult} of
        {{ok, GuildId}, {ok, ChannelId}} ->
            handle_validated_voice_state_update(
                GuildId,
                ChannelId,
                ConnectionId,
                SelfMute,
                SelfDeaf,
                SelfVideo,
                SelfStream,
                ViewerStreamKey,
                IsMobile,
                Latitude,
                Longitude,
                SessionId,
                UserId,
                Guilds,
                State
            );
        {Error = {error, _, _}, _} ->
            {reply, Error, State};
        {_, Error = {error, _, _}} ->
            {reply, Error, State}
    end.

handle_validated_voice_state_update(
    null,
    null,
    null,
    _SelfMute,
    _SelfDeaf,
    _SelfVideo,
    _SelfStream,
    _ViewerStreamKey,
    _IsMobile,
    _Latitude,
    _Longitude,
    _SessionId,
    _UserId,
    _Guilds,
    State
) ->
    handle_voice_disconnect(State);
handle_validated_voice_state_update(
    null,
    null,
    ConnectionId,
    _SelfMute,
    _SelfDeaf,
    _SelfVideo,
    _SelfStream,
    _ViewerStreamKey,
    _IsMobile,
    _Latitude,
    _Longitude,
    SessionId,
    UserId,
    _Guilds,
    State
) when is_binary(ConnectionId) ->
    Request = #{
        user_id => UserId,
        channel_id => null,
        session_id => SessionId,
        connection_id => ConnectionId,
        self_mute => false,
        self_deaf => false,
        self_video => false,
        self_stream => false,
        viewer_stream_key => null,
        is_mobile => false,
        latitude => null,
        longitude => null
    },

    StateWithSessionPid = maps:put(session_pid, self(), State),
    case dm_voice:voice_state_update(Request, StateWithSessionPid) of
        {reply, #{success := true}, NewState} ->
            CleanState = maps:remove(session_pid, NewState),
            {reply, ok, CleanState};
        {reply, {error, Category, ErrorAtom}, _StateWithPid} ->
            {reply, {error, Category, ErrorAtom}, State}
    end;
handle_validated_voice_state_update(
    null,
    ChannelId,
    ConnectionId,
    SelfMute,
    SelfDeaf,
    SelfVideo,
    SelfStream,
    ViewerStreamKey,
    IsMobile,
    Latitude,
    Longitude,
    SessionId,
    UserId,
    _Guilds,
    State
) when is_integer(ChannelId), (is_binary(ConnectionId) orelse ConnectionId =:= null) ->
    Request = #{
        user_id => UserId,
        channel_id => ChannelId,
        session_id => SessionId,
        connection_id => ConnectionId,
        self_mute => SelfMute,
        self_deaf => SelfDeaf,
        self_video => SelfVideo,
        self_stream => SelfStream,
        viewer_stream_key => ViewerStreamKey,
        is_mobile => IsMobile,
        latitude => Latitude,
        longitude => Longitude
    },

    StateWithSessionPid = maps:put(session_pid, self(), State),
    case dm_voice:voice_state_update(Request, StateWithSessionPid) of
        {reply, #{success := true, needs_token := true}, NewState} ->
            SessionPid = self(),
            spawn(fun() ->
                dm_voice:get_voice_token(
                    ChannelId, UserId, SessionId, SessionPid, Latitude, Longitude
                )
            end),
            CleanState = maps:remove(session_pid, NewState),
            {reply, ok, CleanState};
        {reply, #{success := true}, NewState} ->
            CleanState = maps:remove(session_pid, NewState),
            {reply, ok, CleanState};
        {reply, {error, Category, ErrorAtom}, _StateWithPid} ->
            {reply, {error, Category, ErrorAtom}, State}
    end;
handle_validated_voice_state_update(
    GuildId,
    ChannelId,
    ConnectionId,
    SelfMute,
    SelfDeaf,
    SelfVideo,
    SelfStream,
    ViewerStreamKey,
    IsMobile,
    Latitude,
    Longitude,
    SessionId,
    UserId,
    Guilds,
    State
) when is_integer(GuildId) ->
    case maps:get(GuildId, Guilds, undefined) of
        undefined ->
            logger:warning("[session_voice] Guild not found in session: ~p", [GuildId]),
            {reply, gateway_errors:error(voice_guild_not_found), State};
        {GuildPid, _Ref} when is_pid(GuildPid) ->
            Request = #{
                user_id => UserId,
                channel_id => ChannelId,
                session_id => SessionId,
                connection_id => ConnectionId,
                self_mute => SelfMute,
                self_deaf => SelfDeaf,
                self_video => SelfVideo,
                self_stream => SelfStream,
                viewer_stream_key => ViewerStreamKey,
                is_mobile => IsMobile,
                latitude => Latitude,
                longitude => Longitude
            },
            logger:debug(
                "[session_voice] Calling guild process for voice state update: GuildId=~p, ChannelId=~p, ConnectionId=~p",
                [GuildId, ChannelId, ConnectionId]
            ),
            case guild_client:voice_state_update(GuildPid, Request, 12000) of
                {ok, #{needs_token := true}} ->
                    logger:debug("[session_voice] Voice state update succeeded, needs token"),
                    SessionPid = self(),
                    spawn(fun() ->
                        handle_voice_token_request(
                            GuildId,
                            ChannelId,
                            UserId,
                            ConnectionId,
                            SessionId,
                            SessionPid,
                            Latitude,
                            Longitude
                        )
                    end),
                    {reply, ok, State};
                {ok, _} ->
                    logger:debug("[session_voice] Voice state update succeeded"),
                    {reply, ok, State};
                {error, timeout} ->
                    logger:error(
                        "[session_voice] Voice state update timed out (>12s) for GuildId=~p, ChannelId=~p",
                        [GuildId, ChannelId]
                    ),
                    {reply, gateway_errors:error(timeout), State};
                {error, noproc} ->
                    logger:error(
                        "[session_voice] Guild process not running for GuildId=~p",
                        [GuildId]
                    ),
                    {reply, gateway_errors:error(internal_error), State};
                {error, Category, ErrorAtom} ->
                    logger:warning("[session_voice] Voice state update failed: ~p", [ErrorAtom]),
                    {reply, {error, Category, ErrorAtom}, State}
            end;
        _ ->
            logger:warning("[session_voice] Invalid guild pid in session"),
            {reply, gateway_errors:error(internal_error), State}
    end;
handle_validated_voice_state_update(
    GuildId,
    ChannelId,
    ConnectionId,
    _SelfMute,
    _SelfDeaf,
    _SelfVideo,
    _SelfStream,
    _ViewerStreamKey,
    _IsMobile,
    _Latitude,
    _Longitude,
    _SessionId,
    _UserId,
    _Guilds,
    State
) ->
    logger:warning(
        "[session_voice] Invalid voice state update parameters: GuildId=~p, ChannelId=~p, ConnectionId=~p",
        [GuildId, ChannelId, ConnectionId]
    ),
    {reply, gateway_errors:error(validation_invalid_params), State}.

handle_voice_disconnect(State) ->
    Guilds = maps:get(guilds, State),
    UserId = maps:get(user_id, State),
    SessionId = maps:get(id, State),
    ConnectionId = maps:get(connection_id, State),

    lists:foreach(
        fun
            ({_GuildId, {GuildPid, _Ref}}) when is_pid(GuildPid) ->
                Request = #{
                    user_id => UserId,
                    channel_id => null,
                    session_id => SessionId,
                    connection_id => ConnectionId,
                    self_mute => false,
                    self_deaf => false,
                    self_video => false,
                    self_stream => false,
                    viewer_stream_key => null
                },
                _ = guild_client:voice_state_update(GuildPid, Request, 10000);
            (_) ->
                ok
        end,
        maps:to_list(Guilds)
    ),

    {reply, #{success := true}, NewState} = dm_voice:disconnect_voice_user(UserId, State),
    {reply, ok, NewState}.

handle_voice_token_request(
    GuildId, ChannelId, UserId, ConnectionId, _SessionId, SessionPid, Latitude, Longitude
) ->
    Req = voice_utils:build_voice_token_rpc_request(
        GuildId, ChannelId, UserId, ConnectionId, Latitude, Longitude
    ),

    case rpc_client:call(Req) of
        {ok, Data} ->
            Token = maps:get(<<"token">>, Data),
            Endpoint = maps:get(<<"endpoint">>, Data),

            VoiceServerUpdate = #{
                <<"token">> => Token,
                <<"endpoint">> => Endpoint,
                <<"guild_id">> => integer_to_binary(GuildId),
                <<"connection_id">> => ConnectionId
            },

            gen_server:cast(SessionPid, {dispatch, voice_server_update, VoiceServerUpdate});
        {error, _Reason} ->
            ok
    end.
