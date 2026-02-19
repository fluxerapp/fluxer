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

-import(guild_voice_unclaimed_account_utils, [parse_unclaimed_error/1]).

-export([voice_state_update/2]).
-export([get_voice_state/2]).
-export([get_voice_token/6]).
-export([disconnect_voice_user/2]).
-export([broadcast_voice_state_update/3]).
-export([join_or_create_call/5, join_or_create_call/6]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type dm_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.

-spec voice_state_update(map(), dm_state()) ->
    {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
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
            case maps:get(ChannelIdValue, Channels, undefined) of
                undefined ->
                    case fetch_dm_channel_via_rpc(ChannelIdValue, UserId) of
                        {ok, Channel} ->
                            NewChannels = maps:put(ChannelIdValue, Channel, Channels),
                            NewState = maps:put(channels, NewChannels, State),
                            handle_dm_voice_with_channel(
                                Channel, ChannelIdValue, UserId, Request, NewState
                            );
                        {error, _Reason} ->
                            {reply, gateway_errors:error(dm_channel_not_found), State}
                    end;
                Channel ->
                    handle_dm_voice_with_channel(Channel, ChannelIdValue, UserId, Request, State)
            end
    end.

-spec handle_dm_voice_with_channel(map(), integer(), integer(), map(), dm_state()) ->
    {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
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
    ViewerStreamKeys = maps:get(viewer_stream_keys, Request, undefined),
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
                        ViewerStreamKeys,
                        IsMobile,
                        Latitude,
                        Longitude,
                        VoiceStates,
                        State
                    )
            end
    end.

-spec handle_dm_disconnect(binary() | undefined, integer(), voice_state_map(), dm_state()) ->
    {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
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
                        {error, _, _Reason} ->
                            ok
                    end
            end,
            {reply, #{success => true}, NewState}
    end.

-spec handle_dm_connect_or_update(
    binary() | undefined | null,
    integer(),
    integer(),
    binary(),
    boolean(),
    boolean(),
    boolean(),
    boolean(),
    term(),
    boolean(),
    term(),
    term(),
    voice_state_map(),
    dm_state()
) -> {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
handle_dm_connect_or_update(
    ConnectionId,
    ChannelIdValue,
    UserId,
    SessionId,
    SelfMute,
    SelfDeaf,
    SelfVideo,
    SelfStream,
    ViewerStreamKeys,
    IsMobile,
    Latitude,
    Longitude,
    _VoiceStates,
    State
) when ConnectionId =:= undefined; ConnectionId =:= null ->
    VoiceStates = maps:get(dm_voice_states, State, #{}),
    case validate_dm_viewer_stream_keys(ViewerStreamKeys, ChannelIdValue, VoiceStates) of
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
    ViewerStreamKeys,
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
            case guild_voice_state:user_matches_voice_state(ExistingVoiceState, UserId) of
                false ->
                    {reply, gateway_errors:error(voice_user_mismatch), State};
                true ->
                    ExistingSessionId = maps:get(<<"session_id">>, ExistingVoiceState, undefined),
                    EffectiveSessionId = resolve_effective_session_id(ExistingSessionId, SessionId),
                    ValidViewerKey = validate_dm_viewer_stream_keys(
                        ViewerStreamKeys, ChannelIdValue, VoiceStates
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
                                <<"viewer_stream_keys">> => ParsedViewerKey
                            },
                            NewVoiceStates = maps:put(ConnectionId, UpdatedVoiceState, VoiceStates),
                            NewState = maps:put(dm_voice_states, NewVoiceStates, State),
                            broadcast_voice_state_update(ChannelIdValue, UpdatedVoiceState, NewState),
                            OldChannelId = maps:get(<<"channel_id">>, ExistingVoiceState, null),
                            NewChannelIdBin = integer_to_binary(ChannelIdValue),
                            NeedsToken = OldChannelId =/= NewChannelIdBin,
                            maybe_spawn_join_call(
                                NeedsToken,
                                ChannelIdValue,
                                UserId,
                                UpdatedVoiceState,
                                EffectiveSessionId,
                                State
                            ),
                            {reply, #{success => true, needs_token => NeedsToken}, NewState}
                    end
            end
    end.

-spec normalize_session_id(term()) -> binary() | undefined.
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

-spec validate_dm_viewer_stream_keys(term(), integer(), voice_state_map()) ->
    {ok, list()} | {error, atom()}.
validate_dm_viewer_stream_keys(RawKeys, _ChannelIdValue, _VoiceStates) when RawKeys =:= undefined; RawKeys =:= null ->
    {ok, []};
validate_dm_viewer_stream_keys(RawKeys, _ChannelIdValue, _VoiceStates) when not is_list(RawKeys) ->
    {error, voice_invalid_state};
validate_dm_viewer_stream_keys(Keys, ChannelIdValue, VoiceStates) ->
    validate_dm_viewer_stream_keys_list(Keys, ChannelIdValue, VoiceStates, []).

-spec validate_dm_viewer_stream_keys_list(list(), integer(), voice_state_map(), list()) ->
    {ok, list()} | {error, atom()}.
validate_dm_viewer_stream_keys_list([], _ChannelIdValue, _VoiceStates, Acc) ->
    {ok, lists:reverse(Acc)};
validate_dm_viewer_stream_keys_list([Key | Rest], ChannelIdValue, VoiceStates, Acc) ->
    case validate_single_dm_viewer_stream_key(Key, ChannelIdValue, VoiceStates) of
        {ok, ValidKey} ->
            validate_dm_viewer_stream_keys_list(Rest, ChannelIdValue, VoiceStates, [ValidKey | Acc]);
        {error, _} = Error ->
            Error
    end.

-spec validate_single_dm_viewer_stream_key(term(), integer(), voice_state_map()) ->
    {ok, binary()} | {error, atom()}.
validate_single_dm_viewer_stream_key(RawKey, _ChannelIdValue, _VoiceStates) when not is_binary(RawKey) ->
    {error, voice_invalid_state};
validate_single_dm_viewer_stream_key(RawKey, ChannelIdValue, VoiceStates) ->
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
    end.

-spec resolve_effective_session_id(term(), term()) -> binary() | undefined.
resolve_effective_session_id(ExistingSessionId, RequestSessionId) ->
    ExistingNormalized = normalize_session_id(ExistingSessionId),
    RequestNormalized = normalize_session_id(RequestSessionId),
    case ExistingNormalized of
        undefined -> RequestNormalized;
        RequestNormalized -> RequestNormalized;
        _ -> ExistingNormalized
    end.

-spec maybe_spawn_join_call(
    boolean(), integer(), integer(), voice_state(), binary() | undefined, dm_state()
) -> ok.
maybe_spawn_join_call(false, _ChannelId, _UserId, _VoiceState, _SessionId, _State) ->
    ok;
maybe_spawn_join_call(true, ChannelId, UserId, VoiceState, SessionId, State) when
    is_binary(SessionId)
->
    SessionPid = maps:get(session_pid, State, undefined),
    case SessionPid of
        Pid when is_pid(Pid) ->
            spawn(fun() ->
                try
                    join_or_create_call(ChannelId, UserId, VoiceState, SessionId, Pid)
                catch
                    _:_ -> ok
                end
            end),
            ok;
        _ ->
            ok
    end;
maybe_spawn_join_call(true, _ChannelId, _UserId, _VoiceState, _SessionId, _State) ->
    ok.

-spec get_voice_token(integer(), integer(), binary(), pid(), term(), term()) -> ok | error.
get_voice_token(ChannelId, UserId, _SessionId, SessionPid, Latitude, Longitude) ->
    Req = voice_utils:build_voice_token_rpc_request(
        null, ChannelId, UserId, null, Latitude, Longitude
    ),
    Region = resolve_call_region(ChannelId),
    ReqWithRegion = voice_utils:add_rtc_region_to_request(Req, Region),
    case rpc_client:call(ReqWithRegion) of
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
        {error, {rpc_error, _Status, Body}} ->
            case parse_unclaimed_error(Body) of
                true -> SessionPid ! {voice_error, voice_unclaimed_account};
                false -> SessionPid ! {voice_error, voice_token_failed}
            end,
            error;
        {error, _Reason} ->
            SessionPid ! {voice_error, voice_token_failed},
            error
    end.

-spec get_dm_voice_token_and_create_state(
    integer(),
    integer(),
    binary(),
    boolean(),
    boolean(),
    boolean(),
    boolean(),
    term(),
    boolean(),
    term(),
    term(),
    dm_state()
) -> {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
get_dm_voice_token_and_create_state(
    UserId,
    ChannelId,
    SessionId,
    SelfMute,
    SelfDeaf,
    SelfVideo,
    SelfStream,
    ViewerStreamKeys,
    IsMobile,
    Latitude,
    Longitude,
    State
) ->
    Req = voice_utils:build_voice_token_rpc_request(
        null, ChannelId, UserId, null, Latitude, Longitude
    ),
    Region = resolve_call_region(ChannelId, State),
    ReqWithRegion = voice_utils:add_rtc_region_to_request(Req, Region),
    case rpc_client:call(ReqWithRegion) of
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
                ViewerStreamKeys,
                IsMobile,
                State
            );
        {error, {rpc_error, _Status, Body}} ->
            case parse_unclaimed_error(Body) of
                true -> {reply, gateway_errors:error(voice_unclaimed_account), State};
                false -> {reply, gateway_errors:error(voice_token_failed), State}
            end;
        {error, _Reason} ->
            {reply, gateway_errors:error(voice_token_failed), State}
    end.

-spec resolve_call_region(integer()) -> binary() | null.
resolve_call_region(ChannelId) ->
    case gen_server:call(call_manager, {lookup, ChannelId}, 5000) of
        {ok, CallPid} ->
            case gen_server:call(CallPid, {get_state}, 5000) of
                {ok, CallData} -> maps:get(region, CallData, null);
                _ -> null
            end;
        _ ->
            null
    end.

-spec resolve_call_region(integer(), dm_state()) -> binary() | null.
resolve_call_region(ChannelId, State) ->
    Calls = maps:get(calls, State, #{}),
    case maps:get(ChannelId, Calls, undefined) of
        {CallPid, _Ref} when is_pid(CallPid) ->
            try
                case gen_server:call(CallPid, {get_state}, 250) of
                    {ok, CallData} -> maps:get(region, CallData, null);
                    _ -> null
                end
            catch
                _:_ -> null
            end;
        _ ->
            null
    end.

-spec handle_dm_token_success(
    map(),
    integer(),
    integer(),
    binary(),
    boolean(),
    boolean(),
    boolean(),
    boolean(),
    term(),
    boolean(),
    dm_state()
) -> {reply, map(), dm_state()}.
handle_dm_token_success(
    Data,
    UserId,
    ChannelId,
    SessionId,
    SelfMute,
    SelfDeaf,
    SelfVideo,
    SelfStream,
    ViewerStreamKeys,
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
        <<"viewer_stream_keys">> => ViewerStreamKeys
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

-spec get_voice_state(binary(), dm_state()) -> voice_state() | undefined.
get_voice_state(ConnectionId, State) ->
    VoiceStates = maps:get(dm_voice_states, State, #{}),
    maps:get(ConnectionId, VoiceStates, undefined).

-spec disconnect_voice_user(integer(), dm_state()) -> {reply, map(), dm_state()}.
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
            spawn(fun() ->
                maps:foreach(
                    fun(ConnId, VoiceState) ->
                        ChannelId = maps:get(<<"channel_id">>, VoiceState, null),
                        DisconnectVoiceState = maps:put(
                            <<"channel_id">>,
                            null,
                            maps:put(<<"connection_id">>, ConnId, VoiceState)
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
                                    {error, _, _Reason} ->
                                        ok
                                end
                        end
                    end,
                    UserVoiceStates
                )
            end),
            {reply, #{success => true}, NewState}
    end.

-spec broadcast_voice_state_update(integer(), voice_state(), dm_state()) -> ok.
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
            spawn(fun() ->
                lists:foreach(
                    fun(RecipientId) ->
                        presence_manager:dispatch_to_user(RecipientId, Event, VoiceState)
                    end,
                    AllRecipients
                )
            end),
            ok
    end.

-spec check_recipient(integer(), integer(), dm_state()) -> boolean().
check_recipient(UserId, ChannelId, State) ->
    Channels = maps:get(channels, State, #{}),
    case maps:get(ChannelId, Channels, undefined) of
        undefined ->
            false;
        Channel ->
            ChannelType = maps:get(<<"type">>, Channel, 0),
            is_dm_channel_type(ChannelType) andalso is_channel_recipient(UserId, Channel, State)
    end.

-spec is_dm_channel_type(integer()) -> boolean().
is_dm_channel_type(1) -> true;
is_dm_channel_type(3) -> true;
is_dm_channel_type(_) -> false.

-spec is_channel_recipient(integer(), map(), dm_state()) -> boolean().
is_channel_recipient(UserId, Channel, State) ->
    Recipients = maps:get(<<"recipient_ids">>, Channel, []),
    CurrentUserId = maps:get(user_id, State),
    lists:member(UserId, [CurrentUserId | Recipients]).

-spec join_or_create_call(integer(), integer(), voice_state(), binary(), pid()) -> ok.
join_or_create_call(ChannelId, UserId, VoiceState, SessionId, SessionPid) ->
    join_or_create_call(ChannelId, UserId, VoiceState, SessionId, SessionPid, 10).

-spec join_or_create_call(integer(), integer(), voice_state(), binary(), pid(), non_neg_integer()) ->
    ok.
join_or_create_call(_ChannelId, _UserId, _VoiceState, _SessionId, _SessionPid, 0) ->
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

-spec fetch_dm_channel_via_rpc(integer(), integer()) -> {ok, map()} | {error, term()}.
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

-spec convert_api_channel_to_gateway_format(map(), integer()) -> map().
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

-spec extract_recipient_id(term(), integer()) -> {true, integer()} | false.
extract_recipient_id(Recipient, CurrentUserId) when is_map(Recipient) ->
    case maps:get(<<"id">>, Recipient, undefined) of
        undefined -> false;
        Id -> filter_recipient_id(parse_id(Id), CurrentUserId)
    end;
extract_recipient_id(Id, CurrentUserId) ->
    filter_recipient_id(parse_id(Id), CurrentUserId).

-spec parse_id(term()) -> integer() | null.
parse_id(Id) when is_integer(Id) -> Id;
parse_id(Id) when is_binary(Id) ->
    case validation:validate_snowflake(<<"id">>, Id) of
        {ok, IntId} -> IntId;
        {error, _, _} -> null
    end;
parse_id(_) ->
    null.

-spec filter_recipient_id(integer() | null, integer()) -> {true, integer()} | false.
filter_recipient_id(null, _CurrentUserId) -> false;
filter_recipient_id(Id, Id) -> false;
filter_recipient_id(Id, _CurrentUserId) -> {true, Id}.

-ifdef(TEST).

is_dm_channel_type_test() ->
    ?assert(is_dm_channel_type(1)),
    ?assert(is_dm_channel_type(3)),
    ?assertNot(is_dm_channel_type(0)),
    ?assertNot(is_dm_channel_type(2)),
    ?assertNot(is_dm_channel_type(4)).

normalize_session_id_test() ->
    ?assertEqual(undefined, normalize_session_id(undefined)),
    ?assertEqual(<<"abc">>, normalize_session_id(<<"abc">>)),
    ?assertEqual(<<"123">>, normalize_session_id(123)),
    ?assertEqual(<<"test">>, normalize_session_id("test")).

validate_dm_viewer_stream_keys_null_test() ->
    ?assertEqual({ok, []}, validate_dm_viewer_stream_keys(undefined, 123, #{})),
    ?assertEqual({ok, []}, validate_dm_viewer_stream_keys(null, 123, #{})).

validate_dm_viewer_stream_keys_invalid_type_test() ->
    ?assertEqual({error, voice_invalid_state}, validate_dm_viewer_stream_keys(123, 456, #{})).

resolve_effective_session_id_test() ->
    ?assertEqual(<<"req">>, resolve_effective_session_id(undefined, <<"req">>)),
    ?assertEqual(<<"existing">>, resolve_effective_session_id(<<"existing">>, <<"req">>)),
    ?assertEqual(<<"same">>, resolve_effective_session_id(<<"same">>, <<"same">>)).

filter_recipient_id_test() ->
    ?assertEqual(false, filter_recipient_id(null, 1)),
    ?assertEqual(false, filter_recipient_id(1, 1)),
    ?assertEqual({true, 2}, filter_recipient_id(2, 1)).

parse_id_test() ->
    ?assertEqual(123, parse_id(123)),
    ?assertEqual(null, parse_id(invalid)).

handle_dm_connect_or_update_user_mismatch_test() ->
    VoiceStates = #{
        <<"conn-1">> => #{
            <<"channel_id">> => <<"100">>,
            <<"user_id">> => <<"20">>,
            <<"session_id">> => <<"sess">>
        }
    },
    State = #{dm_voice_states => VoiceStates},
    {reply, {error, validation_error, voice_user_mismatch}, _} =
        handle_dm_connect_or_update(
            <<"conn-1">>,
            100,
            10,
            <<"sess">>,
            false,
            false,
            false,
            false,
            undefined,
            false,
            null,
            null,
            VoiceStates,
            State
        ).

handle_dm_connect_or_update_owner_match_proceeds_test() ->
    VoiceStates = #{
        <<"conn-1">> => #{
            <<"channel_id">> => <<"100">>,
            <<"user_id">> => <<"10">>,
            <<"session_id">> => <<"sess">>
        }
    },
    State = #{
        dm_voice_states => VoiceStates,
        channels => #{100 => #{<<"type">> => 1, <<"recipient_ids">> => [10]}},
        user_id => 10,
        id => <<"sess">>,
        session_pid => self()
    },
    case
        handle_dm_connect_or_update(
            <<"conn-1">>,
            100,
            10,
            <<"sess">>,
            false,
            false,
            false,
            false,
            undefined,
            false,
            null,
            null,
            VoiceStates,
            State
        )
    of
        {reply, {error, validation_error, voice_user_mismatch}, _} ->
            error(should_not_get_user_mismatch);
        {reply, _, _} ->
            ok
    end.

-endif.
