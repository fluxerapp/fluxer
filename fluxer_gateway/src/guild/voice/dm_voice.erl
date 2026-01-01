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

-module(dm_voice).

-export([voice_state_update/2]).
-export([get_voice_state/2]).
-export([get_voice_token/6]).
-export([disconnect_voice_user/2]).
-export([broadcast_voice_state_update/3]).
-export([join_or_create_call/5, join_or_create_call/6]).

voice_state_update(Request, State) ->
    #{
        user_id := UserId,
        channel_id := ChannelId
    } = Request,

    ConnectionId = maps:get(connection_id, Request, undefined),
    VoiceStates = maps:get(dm_voice_states, State, #{}),

    case ChannelId of
        null ->
            handle_dm_disconnect(ConnectionId, UserId, VoiceStates, State);
        ChannelIdValue ->
            Channels = maps:get(channels, State, #{}),
            UserId = maps:get(user_id, State),
            logger:info(
                "[dm_voice] Looking up channel ~p for user ~p, channels map has ~p entries",
                [ChannelIdValue, UserId, maps:size(Channels)]
            ),
            case maps:get(ChannelIdValue, Channels, undefined) of
                undefined ->
                    logger:info(
                        "[dm_voice] Channel ~p not found locally for user ~p, trying RPC fallback",
                        [ChannelIdValue, UserId]
                    ),
                    case fetch_dm_channel_via_rpc(ChannelIdValue, UserId) of
                        {ok, Channel} ->
                            NewChannels = maps:put(ChannelIdValue, Channel, Channels),
                            NewState = maps:put(channels, NewChannels, State),
                            logger:info(
                                "[dm_voice] RPC fallback found channel ~p for user ~p, added to local map",
                                [ChannelIdValue, UserId]
                            ),
                            handle_dm_voice_with_channel(
                                Channel, ChannelIdValue, UserId, Request, NewState
                            );
                        {error, Reason} ->
                            logger:warning(
                                "[dm_voice] Channel ~p not found for user ~p via RPC: ~p",
                                [ChannelIdValue, UserId, Reason]
                            ),
                            {reply, gateway_errors:error(dm_channel_not_found), State}
                    end;
                Channel ->
                    logger:info(
                        "[dm_voice] Found channel ~p for user ~p, type: ~p",
                        [ChannelIdValue, UserId, maps:get(<<"type">>, Channel, 0)]
                    ),
                    handle_dm_voice_with_channel(Channel, ChannelIdValue, UserId, Request, State)
            end
    end.

handle_dm_voice_with_channel(Channel, ChannelIdValue, UserId, Request, State) ->
    #{
        session_id := SessionId,
        self_mute := SelfMute,
        self_deaf := SelfDeaf,
        self_video := SelfVideo
    } = Request,
    SelfStream = maps:get(self_stream, Request, false),
    ConnectionId = maps:get(connection_id, Request, undefined),
    IsMobile = maps:get(is_mobile, Request, false),
    ViewerStreamKey = maps:get(viewer_stream_key, Request, undefined),
    Latitude = maps:get(latitude, Request, null),
    Longitude = maps:get(longitude, Request, null),
    VoiceStates = maps:get(dm_voice_states, State, #{}),

    ChannelType = maps:get(<<"type">>, Channel, 0),
    case is_dm_channel_type(ChannelType) of
        false ->
            {reply, gateway_errors:error(dm_invalid_channel_type), State};
        true ->
            case check_recipient(UserId, ChannelIdValue, State) of
                false ->
                    {reply, gateway_errors:error(dm_not_recipient), State};
                true ->
                    handle_dm_connect_or_update(
                        ConnectionId,
                        ChannelIdValue,
                        UserId,
                        SessionId,
                        SelfMute,
                        SelfDeaf,
                        SelfVideo,
                        SelfStream,
                        ViewerStreamKey,
                        IsMobile,
                        Latitude,
                        Longitude,
                        VoiceStates,
                        State
                    )
            end
    end.

handle_dm_disconnect(undefined, _UserId, _VoiceStates, State) ->
    {reply, gateway_errors:error(voice_missing_connection_id), State};
handle_dm_disconnect(ConnectionId, _UserId, VoiceStates, State) ->
    case maps:get(ConnectionId, VoiceStates, undefined) of
        undefined ->
            {reply, #{success => true}, State};
        OldVoiceState ->
            NewVoiceStates = maps:remove(ConnectionId, VoiceStates),
            NewState = maps:put(dm_voice_states, NewVoiceStates, State),

            OldChannelId = maps:get(<<"channel_id">>, OldVoiceState, null),
            DisconnectVoiceState = maps:put(
                <<"channel_id">>, null, maps:put(<<"connection_id">>, ConnectionId, OldVoiceState)
            ),
            SessionId = maps:get(id, State),

            case OldChannelId of
                null ->
                    ok;
                ChannelIdValue ->
                    SessionPid = maps:get(session_pid, State),
                    gen_server:cast(SessionPid, {call_unmonitor, ChannelIdValue}),
                    spawn(fun() ->
                        try
                            case gen_server:call(call_manager, {lookup, ChannelIdValue}, 5000) of
                                {ok, CallPid} ->
                                    gen_server:call(CallPid, {leave, SessionId}, 5000);
                                _ ->
                                    ok
                            end
                        catch
                            _:_ -> ok
                        end
                    end)
            end,

            case OldChannelId of
                null ->
                    ok;
                _ ->
                    case validation:validate_snowflake(<<"channel_id">>, OldChannelId) of
                        {ok, OldChannelIdInt} ->
                            broadcast_voice_state_update(
                                OldChannelIdInt, DisconnectVoiceState, NewState
                            );
                        {error, _, Reason} ->
                            logger:warning("[dm_voice] Invalid channel_id: ~p", [Reason]),
                            ok
                    end
            end,

            {reply, #{success => true}, NewState}
    end.

handle_dm_connect_or_update(
    ConnectionId,
    ChannelIdValue,
    UserId,
    SessionId,
    SelfMute,
    SelfDeaf,
    SelfVideo,
    SelfStream,
    ViewerStreamKey,
    IsMobile,
    Latitude,
    Longitude,
    _VoiceStates,
    State
) when ConnectionId =:= undefined; ConnectionId =:= null ->
    VoiceStates = maps:get(dm_voice_states, State, #{}),
    case validate_dm_viewer_stream_key(ViewerStreamKey, ChannelIdValue, VoiceStates) of
        {error, ErrorAtom} ->
            {reply, gateway_errors:error(ErrorAtom), State};
        {ok, ParsedViewerKey} ->
            get_dm_voice_token_and_create_state(
                UserId,
                ChannelIdValue,
                SessionId,
                SelfMute,
                SelfDeaf,
                SelfVideo,
                SelfStream,
                ParsedViewerKey,
                IsMobile,
                Latitude,
                Longitude,
                State
            )
    end;
handle_dm_connect_or_update(
    ConnectionId,
    ChannelIdValue,
    UserId,
    SessionId,
    SelfMute,
    SelfDeaf,
    SelfVideo,
    SelfStream,
    ViewerStreamKey,
    IsMobile,
    _Latitude,
    _Longitude,
    VoiceStates,
    State
) ->
    case maps:get(ConnectionId, VoiceStates, undefined) of
        undefined ->
            {reply, gateway_errors:error(voice_connection_not_found), State};
        ExistingVoiceState ->
            ExistingSessionId = maps:get(<<"session_id">>, ExistingVoiceState, undefined),
            EffectiveSessionId = resolve_effective_session_id(ExistingSessionId, SessionId),
            ValidViewerKey = validate_dm_viewer_stream_key(
                ViewerStreamKey, ChannelIdValue, VoiceStates
            ),
            case ValidViewerKey of
                {error, ErrorAtom} ->
                    {reply, gateway_errors:error(ErrorAtom), State};
                {ok, ParsedViewerKey} ->
                    UpdatedVoiceState = ExistingVoiceState#{
                        <<"channel_id">> => integer_to_binary(ChannelIdValue),
                        <<"session_id">> => EffectiveSessionId,
                        <<"self_mute">> => SelfMute,
                        <<"self_deaf">> => SelfDeaf,
                        <<"self_video">> => SelfVideo,
                        <<"self_stream">> => SelfStream,
                        <<"is_mobile">> => IsMobile,
                        <<"viewer_stream_key">> => ParsedViewerKey
                    },

                    NewVoiceStates = maps:put(ConnectionId, UpdatedVoiceState, VoiceStates),
                    NewState = maps:put(dm_voice_states, NewVoiceStates, State),

                    broadcast_voice_state_update(ChannelIdValue, UpdatedVoiceState, NewState),

                    OldChannelId = maps:get(<<"channel_id">>, ExistingVoiceState, null),
                    NewChannelIdBin = integer_to_binary(ChannelIdValue),
                    NeedsToken = OldChannelId =/= NewChannelIdBin,

                    maybe_spawn_join_call(
                        NeedsToken, ChannelIdValue, UserId, UpdatedVoiceState, SessionId
                    ),

                    {reply, #{success => true, needs_token => NeedsToken}, NewState}
            end
    end.

normalize_session_id(undefined) ->
    undefined;
normalize_session_id(SessionId) when is_binary(SessionId) ->
    SessionId;
normalize_session_id(SessionId) when is_integer(SessionId) ->
    integer_to_binary(SessionId);
normalize_session_id(SessionId) when is_list(SessionId) ->
    list_to_binary(SessionId);
normalize_session_id(SessionId) ->
    try
        erlang:iolist_to_binary(SessionId)
    catch
        _:_ -> SessionId
    end.

validate_dm_viewer_stream_key(RawKey, ChannelIdValue, VoiceStates) ->
    case RawKey of
        undefined ->
            {ok, null};
        null ->
            {ok, null};
        _ when not is_binary(RawKey) -> {error, voice_invalid_state};
        _ ->
            case voice_state_utils:parse_stream_key(RawKey) of
                {ok, #{scope := dm, channel_id := ParsedChannelId, connection_id := ConnId}} when
                    ParsedChannelId =:= ChannelIdValue
                ->
                    case maps:get(ConnId, VoiceStates, undefined) of
                        undefined ->
                            {error, voice_connection_not_found};
                        StreamVS ->
                            case map_utils:get_integer(StreamVS, <<"channel_id">>, undefined) of
                                ChannelIdValue -> {ok, RawKey};
                                _ -> {error, voice_invalid_state}
                            end
                    end;
                _ ->
                    {error, voice_invalid_state}
            end
    end.

resolve_effective_session_id(ExistingSessionId, RequestSessionId) ->
    ExistingNormalized = normalize_session_id(ExistingSessionId),
    RequestNormalized = normalize_session_id(RequestSessionId),
    case ExistingNormalized of
        undefined -> RequestNormalized;
        RequestNormalized -> RequestNormalized;
        _ -> ExistingNormalized
    end.

maybe_spawn_join_call(false, _ChannelId, _UserId, _VoiceState, _SessionId) ->
    ok;
maybe_spawn_join_call(true, ChannelId, UserId, VoiceState, SessionId) ->
    spawn(fun() ->
        try
            join_or_create_call(ChannelId, UserId, VoiceState, SessionId, self())
        catch
            _:_ -> ok
        end
    end).

get_voice_token(ChannelId, UserId, _SessionId, SessionPid, Latitude, Longitude) ->
    Req = voice_utils:build_voice_token_rpc_request(
        null, ChannelId, UserId, null, Latitude, Longitude
    ),

    case rpc_client:call(Req) of
        {ok, Data} ->
            Token = maps:get(<<"token">>, Data),
            Endpoint = maps:get(<<"endpoint">>, Data),
            ConnectionId = maps:get(<<"connectionId">>, Data),

            SessionPid !
                {voice_server_update, #{
                    channel_id => integer_to_binary(ChannelId),
                    endpoint => Endpoint,
                    token => Token,
                    connection_id => ConnectionId
                }},
            ok;
        {error, _Reason} ->
            error
    end.

get_dm_voice_token_and_create_state(
    UserId,
    ChannelId,
    SessionId,
    SelfMute,
    SelfDeaf,
    SelfVideo,
    SelfStream,
    ViewerStreamKey,
    IsMobile,
    Latitude,
    Longitude,
    State
) ->
    Req = voice_utils:build_voice_token_rpc_request(
        null, ChannelId, UserId, null, Latitude, Longitude
    ),

    case rpc_client:call(Req) of
        {ok, Data} ->
            handle_dm_token_success(
                Data,
                UserId,
                ChannelId,
                SessionId,
                SelfMute,
                SelfDeaf,
                SelfVideo,
                SelfStream,
                ViewerStreamKey,
                IsMobile,
                State
            );
        {error, _Reason} ->
            {reply, gateway_errors:error(voice_token_failed), State}
    end.

handle_dm_token_success(
    Data,
    UserId,
    ChannelId,
    SessionId,
    SelfMute,
    SelfDeaf,
    SelfVideo,
    SelfStream,
    ViewerStreamKey,
    IsMobile,
    State
) ->
    Token = maps:get(<<"token">>, Data),
    Endpoint = maps:get(<<"endpoint">>, Data),
    ConnectionId = maps:get(<<"connectionId">>, Data),

    VoiceState = #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"connection_id">> => ConnectionId,
        <<"is_mobile">> => IsMobile,
        <<"session_id">> => SessionId,
        <<"self_mute">> => SelfMute,
        <<"self_deaf">> => SelfDeaf,
        <<"self_video">> => SelfVideo,
        <<"self_stream">> => SelfStream,
        <<"viewer_stream_key">> => ViewerStreamKey
    },

    VoiceStates = maps:get(dm_voice_states, State, #{}),
    NewVoiceStates = maps:put(ConnectionId, VoiceState, VoiceStates),
    NewState = maps:put(dm_voice_states, NewVoiceStates, State),

    broadcast_voice_state_update(ChannelId, VoiceState, NewState),

    SessionPid = maps:get(session_pid, State),
    VoiceServerUpdate = #{
        <<"token">> => Token,
        <<"endpoint">> => Endpoint,
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"connection_id">> => ConnectionId
    },
    gen_server:cast(SessionPid, {dispatch, voice_server_update, VoiceServerUpdate}),

    GatewaySessionId = maps:get(id, State),
    spawn(fun() ->
        try
            join_or_create_call(ChannelId, UserId, VoiceState, GatewaySessionId, SessionPid)
        catch
            _:_ -> ok
        end
    end),

    {reply, #{success => true, needs_token => false, connection_id => ConnectionId}, NewState}.

get_voice_state(ConnectionId, State) ->
    VoiceStates = maps:get(dm_voice_states, State, #{}),
    maps:get(ConnectionId, VoiceStates, undefined).

disconnect_voice_user(UserId, State) ->
    VoiceStates = maps:get(dm_voice_states, State, #{}),

    UserVoiceStates = maps:filter(
        fun(_ConnectionId, VoiceState) ->
            maps:get(<<"user_id">>, VoiceState) =:= integer_to_binary(UserId)
        end,
        VoiceStates
    ),

    case maps:size(UserVoiceStates) of
        0 ->
            {reply, #{success => true}, State};
        _ ->
            NewVoiceStates = maps:fold(
                fun(ConnectionId, _VoiceState, Acc) ->
                    maps:remove(ConnectionId, Acc)
                end,
                VoiceStates,
                UserVoiceStates
            ),
            NewState = maps:put(dm_voice_states, NewVoiceStates, State),

            maps:foreach(
                fun(_ConnectionId, VoiceState) ->
                    ChannelId = maps:get(<<"channel_id">>, VoiceState, null),
                    DisconnectVoiceState = maps:put(
                        <<"channel_id">>,
                        null,
                        maps:put(<<"connection_id">>, _ConnectionId, VoiceState)
                    ),
                    case ChannelId of
                        null ->
                            ok;
                        _ ->
                            case validation:validate_snowflake(<<"channel_id">>, ChannelId) of
                                {ok, ChannelIdInt} ->
                                    broadcast_voice_state_update(
                                        ChannelIdInt, DisconnectVoiceState, NewState
                                    );
                                {error, _, Reason} ->
                                    logger:warning(
                                        "[dm_voice] Invalid channel_id in voice state: ~p", [Reason]
                                    ),
                                    ok
                            end
                    end
                end,
                UserVoiceStates
            ),

            {reply, #{success => true}, NewState}
    end.

broadcast_voice_state_update(ChannelId, VoiceState, State) ->
    Channels = maps:get(channels, State, #{}),

    case maps:get(ChannelId, Channels, undefined) of
        undefined ->
            ok;
        Channel ->
            Recipients = maps:get(<<"recipient_ids">>, Channel, []),
            UserId = maps:get(user_id, State),

            AllRecipients = lists:usort([UserId | Recipients]),

            Event = voice_state_update,

            lists:foreach(
                fun(RecipientId) ->
                    presence_manager:dispatch_to_user(RecipientId, Event, VoiceState)
                end,
                AllRecipients
            )
    end.

check_recipient(UserId, ChannelId, State) ->
    Channels = maps:get(channels, State, #{}),
    case maps:get(ChannelId, Channels, undefined) of
        undefined ->
            false;
        Channel ->
            ChannelType = maps:get(<<"type">>, Channel, 0),
            is_dm_channel_type(ChannelType) andalso is_channel_recipient(UserId, Channel, State)
    end.

is_dm_channel_type(1) -> true;
is_dm_channel_type(3) -> true;
is_dm_channel_type(_) -> false.

is_channel_recipient(UserId, Channel, State) ->
    Recipients = maps:get(<<"recipient_ids">>, Channel, []),
    CurrentUserId = maps:get(user_id, State),
    lists:member(UserId, [CurrentUserId | Recipients]).

join_or_create_call(ChannelId, UserId, VoiceState, SessionId, SessionPid) ->
    join_or_create_call(ChannelId, UserId, VoiceState, SessionId, SessionPid, 10).

join_or_create_call(_ChannelId, UserId, _VoiceState, _SessionId, _SessionPid, 0) ->
    logger:warning("[dm_voice] Failed to join call after retries, user ~p could not join", [UserId]),
    ok;
join_or_create_call(ChannelId, UserId, VoiceState, SessionId, SessionPid, Retries) ->
    ConnectionId = maps:get(<<"connection_id">>, VoiceState, undefined),
    case gen_server:call(call_manager, {lookup, ChannelId}, 5000) of
        {ok, CallPid} ->
            JoinMsg =
                case ConnectionId of
                    undefined ->
                        {join, UserId, VoiceState, SessionId, SessionPid};
                    _ ->
                        {join, UserId, VoiceState, SessionId, SessionPid, ConnectionId}
                end,
            case gen_server:call(CallPid, JoinMsg, 5000) of
                ok ->
                    gen_server:cast(SessionPid, {call_monitor, ChannelId, CallPid}),
                    ok;
                Error ->
                    Error
            end;
        {error, not_found} ->
            timer:sleep(300),
            join_or_create_call(ChannelId, UserId, VoiceState, SessionId, SessionPid, Retries - 1);
        not_found ->
            timer:sleep(300),
            join_or_create_call(ChannelId, UserId, VoiceState, SessionId, SessionPid, Retries - 1)
    end.

fetch_dm_channel_via_rpc(ChannelId, UserId) ->
    Req = #{
        <<"type">> => <<"get_dm_channel">>,
        <<"channel_id">> => ChannelId,
        <<"user_id">> => UserId
    },
    case rpc_client:call(Req) of
        {ok, #{<<"channel">> := null}} ->
            {error, not_found};
        {ok, #{<<"channel">> := Channel}} when is_map(Channel) ->
            {ok, convert_api_channel_to_gateway_format(Channel, UserId)};
        {ok, _} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

convert_api_channel_to_gateway_format(Channel, CurrentUserId) ->
    ChannelType = maps:get(<<"type">>, Channel, 0),
    Recipients = maps:get(<<"recipients">>, Channel, []),
    RecipientIds = lists:filtermap(
        fun(R) -> extract_recipient_id(R, CurrentUserId) end,
        Recipients
    ),
    #{
        <<"id">> => maps:get(<<"id">>, Channel),
        <<"type">> => ChannelType,
        <<"recipient_ids">> => RecipientIds
    }.

extract_recipient_id(Recipient, CurrentUserId) when is_map(Recipient) ->
    case maps:get(<<"id">>, Recipient, undefined) of
        undefined -> false;
        Id -> filter_recipient_id(parse_id(Id), CurrentUserId)
    end;
extract_recipient_id(Id, CurrentUserId) ->
    filter_recipient_id(parse_id(Id), CurrentUserId).

parse_id(Id) when is_integer(Id) -> Id;
parse_id(Id) when is_binary(Id) ->
    case validation:validate_snowflake(<<"id">>, Id) of
        {ok, IntId} -> IntId;
        {error, _, _} -> null
    end;
parse_id(_) ->
    null.

filter_recipient_id(null, _CurrentUserId) -> false;
filter_recipient_id(Id, Id) -> false;
filter_recipient_id(Id, _CurrentUserId) -> {true, Id}.
