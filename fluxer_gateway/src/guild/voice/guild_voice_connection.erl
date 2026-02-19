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

-module(guild_voice_connection).

-import(guild_voice_unclaimed_account_utils, [parse_unclaimed_error/1]).

-include_lib("fluxer_gateway/include/voice_state.hrl").

-export([voice_state_update/2]).
-export([confirm_voice_connection_from_livekit/2]).
-export([request_voice_token/4]).
-export([request_voice_token/5]).
-export([request_voice_token/6]).
-export([sweep_expired_pending_joins/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type guild_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.
-type pending_voice_connections() :: #{binary() => map()}.
-type context() :: #{
    user_id := integer() | undefined,
    channel_id := integer() | null | undefined,
    session_id := term(),
    connection_id := binary() | undefined,
    raw_connection_id := term(),
    self_mute := boolean(),
    self_deaf := boolean(),
    self_video := boolean(),
    self_stream := boolean(),
    is_mobile := boolean(),
    viewer_stream_keys := term()
}.

-spec voice_state_update(map(), guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
voice_state_update(Request, State) ->
    Context = build_context(Request),
    case maps:get(user_id, Context) of
        undefined ->
            {reply, gateway_errors:error(voice_invalid_user_id), State};
        UserId ->
            VoiceStates = voice_state_utils:voice_states(State),
            case guild_voice_member:find_member_by_user_id(UserId, State) of
                undefined ->
                    {reply, gateway_errors:error(voice_member_not_found), State};
                Member ->
                    handle_member_voice(Context, Member, VoiceStates, State)
            end
    end.

-spec handle_member_voice(context(), map(), voice_state_map(), guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
handle_member_voice(Context, Member, VoiceStates, State) ->
    case maps:get(channel_id, Context) of
        undefined ->
            {reply, gateway_errors:error(voice_invalid_channel_id), State};
        null ->
            handle_disconnect(Context, VoiceStates, State);
        ChannelIdValue ->
            handle_voice_connect_or_update(Context, ChannelIdValue, Member, VoiceStates, State)
    end.

-spec handle_disconnect(context(), voice_state_map(), guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
handle_disconnect(Context, VoiceStates, State) ->
    guild_voice_disconnect:handle_voice_disconnect(
        maps:get(raw_connection_id, Context),
        maps:get(session_id, Context),
        maps:get(user_id, Context),
        VoiceStates,
        State
    ).

-spec handle_voice_connect_or_update(
    context(), integer(), map(), voice_state_map(), guild_state()
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
handle_voice_connect_or_update(Context, ChannelIdValue, Member, VoiceStates, State) ->
    ConnectionId = maps:get(connection_id, Context),
    Channel = guild_voice_member:find_channel_by_id(ChannelIdValue, State),
    case Channel of
        undefined ->
            {reply, gateway_errors:error(voice_channel_not_found), State};
        _ ->
            case ConnectionId of
                undefined ->
                    handle_new_connection(Context, Member, Channel, VoiceStates, State);
                _ ->
                    handle_update_connection(
                        Context,
                        ChannelIdValue,
                        Member,
                        Channel,
                        VoiceStates,
                        State
                    )
            end
    end.

-spec handle_update_connection(
    context(), integer(), map(), map(), voice_state_map(), guild_state()
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
handle_update_connection(Context, ChannelIdValue, Member, Channel, VoiceStates, State) ->
    ConnectionId = maps:get(connection_id, Context),
    UserId = maps:get(user_id, Context),
    case maps:get(ConnectionId, VoiceStates, undefined) of
        undefined ->
            case
                maybe_restore_pending_connection(
                    ConnectionId, ChannelIdValue, UserId, VoiceStates, State
                )
            of
                {ok, UpdatedVoiceStates, UpdatedState} ->
                    logger:debug(
                        "Restored pending voice connection during update",
                        #{
                            connection_id => ConnectionId,
                            user_id => UserId,
                            channel_id => ChannelIdValue
                        }
                    ),
                    ExistingVoiceState = maps:get(ConnectionId, UpdatedVoiceStates),
                    ExistingChannelIdBin = maps:get(<<"channel_id">>, ExistingVoiceState, null),
                    NewChannelIdBin = integer_to_binary(ChannelIdValue),
                    IsChannelChange = ExistingChannelIdBin =/= NewChannelIdBin,
                    GuildId = map_utils:get_integer(UpdatedState, id, undefined),
                    ViewerKeyResult =
                        resolve_viewer_stream_keys(
                            Context,
                            GuildId,
                            ChannelIdValue,
                            UpdatedVoiceStates,
                            ExistingVoiceState
                        ),
                    normal_update_connection(
                        Context,
                        ChannelIdValue,
                        Member,
                        Channel,
                        UpdatedVoiceStates,
                        UpdatedState,
                        IsChannelChange,
                        ViewerKeyResult
                    );
                {error, ErrorAtom} ->
                    logger:debug(
                        "Failed to restore pending voice connection during update",
                        #{
                            connection_id => ConnectionId,
                            user_id => UserId,
                            channel_id => ChannelIdValue,
                            error => ErrorAtom
                        }
                    ),
                    {reply, gateway_errors:error(ErrorAtom), State}
            end;
        ExistingVoiceState ->
            case guild_voice_state:user_matches_voice_state(ExistingVoiceState, UserId) of
                false ->
                    {reply, gateway_errors:error(voice_user_mismatch), State};
                true ->
                    ExistingChannelIdBin = maps:get(<<"channel_id">>, ExistingVoiceState, null),
                    NewChannelIdBin = integer_to_binary(ChannelIdValue),
                    IsChannelChange = ExistingChannelIdBin =/= NewChannelIdBin,
                    GuildId = map_utils:get_integer(State, id, undefined),
                    ViewerKeyResult =
                        resolve_viewer_stream_keys(
                            Context, GuildId, ChannelIdValue, VoiceStates, ExistingVoiceState
                        ),
                    normal_update_connection(
                        Context,
                        ChannelIdValue,
                        Member,
                        Channel,
                        VoiceStates,
                        State,
                        IsChannelChange,
                        ViewerKeyResult
                    )
            end
    end.

-spec maybe_restore_pending_connection(
    binary(),
    integer(),
    integer(),
    voice_state_map(),
    guild_state()
) -> {ok, voice_state_map(), guild_state()} | {error, atom()}.
maybe_restore_pending_connection(ConnectionId, ChannelIdValue, UserId, VoiceStates, State) ->
    PendingConnections = pending_voice_connections(State),
    case maps:get(ConnectionId, PendingConnections, undefined) of
        undefined ->
            {error, voice_connection_not_found};
        PendingData ->
            PendingUserId = pending_get_integer(PendingData, user_id),
            PendingChannelId = pending_get_integer(PendingData, channel_id),
            logger:debug(
                "Checking pending voice connection for restore",
                #{
                    connection_id => ConnectionId,
                    user_id => UserId,
                    channel_id => ChannelIdValue,
                    pending_user_id => PendingUserId,
                    pending_channel_id => PendingChannelId
                }
            ),
            case {PendingUserId, PendingChannelId} of
                {UserId, ChannelIdValue} ->
                    case pending_get_integer(PendingData, expires_at) of
                        ExpiresAt when is_integer(ExpiresAt) ->
                            Now = erlang:system_time(millisecond),
                            case Now >= ExpiresAt of
                                true ->
                                    {error, voice_pending_expired};
                                false ->
                                    restore_pending_connection(
                                        ConnectionId,
                                        PendingConnections,
                                        PendingData,
                                        VoiceStates,
                                        State
                                    )
                            end;
                        _ ->
                            restore_pending_connection(
                                ConnectionId,
                                PendingConnections,
                                PendingData,
                                VoiceStates,
                                State
                            )
                    end;
                _ ->
                    {error, voice_connection_not_found}
            end
    end.

-spec restore_pending_connection(
    binary(),
    pending_voice_connections(),
    map(),
    voice_state_map(),
    guild_state()
) -> {ok, voice_state_map(), guild_state()} | {error, atom()}.
restore_pending_connection(ConnectionId, PendingConnections, PendingData, VoiceStates, State) ->
    VoiceState = resolve_voice_state_from_pending(ConnectionId, PendingData, State, VoiceStates),
    case VoiceState of
        undefined ->
            {error, voice_connection_not_found};
        _ ->
            NewPendingConnections = maps:remove(ConnectionId, PendingConnections),
            StateWithoutPending = maps:put(pending_voice_connections, NewPendingConnections, State),
            UpdatedVoiceStates = maps:put(ConnectionId, VoiceState, VoiceStates),
            StateWithVoiceStates = maps:put(voice_states, UpdatedVoiceStates, StateWithoutPending),
            StateCleared = clear_virtual_access_flags_from_voice_state(
                VoiceState, StateWithVoiceStates
            ),
            ChannelIdBin = maps:get(<<"channel_id">>, VoiceState, null),
            guild_voice_broadcast:broadcast_voice_state_update(VoiceState, StateCleared, ChannelIdBin),
            {ok, UpdatedVoiceStates, StateCleared}
    end.

-spec normal_update_connection(
    context(),
    integer(),
    map(),
    map(),
    voice_state_map(),
    guild_state(),
    boolean(),
    {ok, term()} | {error, atom()}
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
normal_update_connection(
    Context,
    ChannelIdValue,
    Member,
    Channel,
    VoiceStates,
    State,
    IsChannelChange,
    ViewerKeyResult
) ->
    ConnectionId = maps:get(connection_id, Context),
    UserId = maps:get(user_id, Context),
    PermCheck =
        case IsChannelChange of
            true ->
                guild_voice_permissions:check_voice_permissions_and_limits(
                    UserId, ChannelIdValue, Channel, VoiceStates, State, false
                );
            false ->
                {ok, allowed}
        end,
    case PermCheck of
        {error, _Category, ErrorAtom} ->
            {reply, gateway_errors:error(ErrorAtom), State};
        {ok, allowed} ->
            case check_camera_user_limit(Context, ChannelIdValue, VoiceStates) of
                {error, CameraErrorAtom} ->
                    {reply, gateway_errors:error(CameraErrorAtom), State};
                ok ->
                    case ViewerKeyResult of
                        {error, ErrorAtom} ->
                            {reply, gateway_errors:error(ErrorAtom), State};
                        {ok, ParsedViewerKey} ->
                            case IsChannelChange of
                                true ->
                                    handle_client_channel_move(
                                        Context, ChannelIdValue, Member, ConnectionId,
                                        VoiceStates, State, ParsedViewerKey
                                    );
                                false ->
                                    Flags = voice_state_utils:voice_flags_from_context(Context),
                                    guild_voice_state:update_voice_state_data(
                                        ConnectionId,
                                        integer_to_binary(ChannelIdValue),
                                        Flags,
                                        Member,
                                        maps:get(ConnectionId, VoiceStates),
                                        VoiceStates,
                                        State,
                                        false,
                                        ParsedViewerKey
                                    )
                            end
                    end
            end
    end.

-spec handle_client_channel_move(
    context(), integer(), map(), binary(), voice_state_map(), guild_state(), term()
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
handle_client_channel_move(
    Context, ChannelIdValue, Member, ConnectionId, VoiceStates, State, _ParsedViewerKey
) ->
    UserId = maps:get(user_id, Context),
    SessionId = maps:get(session_id, Context),
    ExistingVoiceState = maps:get(ConnectionId, VoiceStates),
    OldChannelIdBin = maps:get(<<"channel_id">>, ExistingVoiceState, null),
    State0 = guild_virtual_channel_access:mark_pending_join(UserId, ChannelIdValue, State),
    State1 = guild_virtual_channel_access:mark_preserve(UserId, ChannelIdValue, State0),
    State2 = guild_virtual_channel_access:mark_move_pending(UserId, ChannelIdValue, State1),
    NewVoiceStates = maps:remove(ConnectionId, VoiceStates),
    State3 = maps:put(voice_states, NewVoiceStates, State2),
    DisconnectVoiceState = maps:put(<<"channel_id">>, null, ExistingVoiceState),
    guild_voice_broadcast:broadcast_voice_state_update(
        DisconnectVoiceState, State3, OldChannelIdBin
    ),
    case resolve_guild_identity(State3) of
        {error, ErrorAtom} ->
            {reply, gateway_errors:error(ErrorAtom), State3};
        {ok, GuildId, _GuildIdBin} ->
            VoicePermissions = voice_utils:compute_voice_permissions(
                UserId, ChannelIdValue, State3
            ),
            TokenNonce = voice_utils:generate_token_nonce(),
            case request_voice_token(GuildId, ChannelIdValue, UserId, ConnectionId, VoicePermissions, TokenNonce) of
                {ok, TokenData} ->
                    Token = maps:get(token, TokenData),
                    Endpoint = maps:get(endpoint, TokenData),
                    NewConnectionId = maps:get(connection_id, TokenData),
                    SessionIdBin = normalize_session_id(SessionId),
                    ServerMute = maps:get(<<"mute">>, Member, false),
                    ServerDeaf = maps:get(<<"deaf">>, Member, false),
                    Flags = voice_state_utils:voice_flags_from_context(Context),
                    #{
                        self_mute := SelfMuteFlag,
                        self_deaf := SelfDeafFlag,
                        self_video := SelfVideoFlag,
                        self_stream := SelfStreamFlag,
                        is_mobile := IsMobileFlag
                    } = Flags,
                    ChannelIdBin = integer_to_binary(ChannelIdValue),
                    UserIdBin = integer_to_binary(UserId),
                    GuildIdBin2 = integer_to_binary(GuildId),
                    VoiceState0 = guild_voice_state:create_voice_state(
                        GuildIdBin2,
                        ChannelIdBin,
                        UserIdBin,
                        NewConnectionId,
                        ServerMute,
                        ServerDeaf,
                        Flags,
                        []
                    ),
                    VoiceState1 = maybe_attach_session_id(VoiceState0, SessionIdBin),
                    VoiceState = maybe_attach_member(VoiceState1, Member),
                    Now = erlang:system_time(millisecond),
                    PendingMetadata = #{
                        user_id => UserId,
                        guild_id => GuildId,
                        channel_id => ChannelIdValue,
                        session_id => SessionIdBin,
                        self_mute => SelfMuteFlag,
                        self_deaf => SelfDeafFlag,
                        self_video => SelfVideoFlag,
                        self_stream => SelfStreamFlag,
                        is_mobile => IsMobileFlag,
                        server_mute => ServerMute,
                        server_deaf => ServerDeaf,
                        member => Member,
                        viewer_stream_keys => [],
                        voice_state => VoiceState,
                        token_nonce => TokenNonce,
                        created_at => Now,
                        expires_at => Now + 30000
                    },
                    PendingConnections = pending_voice_connections(State3),
                    NewPendingConnections = maps:put(
                        NewConnectionId, PendingMetadata, PendingConnections
                    ),
                    State4 = maps:put(
                        pending_voice_connections, NewPendingConnections, State3
                    ),
                    {reply,
                        #{
                            success => true,
                            needs_token => true,
                            token => Token,
                            endpoint => Endpoint,
                            connection_id => NewConnectionId,
                            voice_state => VoiceState
                        },
                        State4};
                {error, _Reason} ->
                    {reply, gateway_errors:error(voice_token_failed), State3}
            end
    end.

-spec handle_new_connection(context(), map(), map(), voice_state_map(), guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
handle_new_connection(Context, Member, Channel, VoiceStates, State) ->
    ChannelIdValue = maps:get(channel_id, Context),
    GuildId = map_utils:get_integer(State, id, undefined),
    ViewerKeyResult = resolve_viewer_stream_keys(Context, GuildId, ChannelIdValue, VoiceStates, #{}),
    normal_new_connection(Context, Member, Channel, VoiceStates, State, ViewerKeyResult).

-spec normal_new_connection(
    context(), map(), map(), voice_state_map(), guild_state(), {ok, term()} | {error, atom()}
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
normal_new_connection(Context, Member, Channel, VoiceStates, State, ViewerKeyResult) ->
    UserId = maps:get(user_id, Context),
    ChannelIdValue = maps:get(channel_id, Context),
    PermCheck = guild_voice_permissions:check_voice_permissions_and_limits(
        UserId, ChannelIdValue, Channel, VoiceStates, State, false
    ),
    case PermCheck of
        {error, _Category, ErrorAtom} ->
            {reply, gateway_errors:error(ErrorAtom), State};
        {ok, allowed} ->
            case check_camera_user_limit(Context, ChannelIdValue, VoiceStates) of
                {error, CameraErrorAtom} ->
                    {reply, gateway_errors:error(CameraErrorAtom), State};
                ok ->
                    case ViewerKeyResult of
                        {error, ErrorAtom} ->
                            {reply, gateway_errors:error(ErrorAtom), State};
                        {ok, ParsedViewerKey} ->
                            get_voice_token_and_create_state(Context, Member, ParsedViewerKey, State)
                    end
            end
    end.

-spec get_voice_token_and_create_state(context(), map(), term(), guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
get_voice_token_and_create_state(Context, Member, ParsedViewerStreamKey, State) ->
    ChannelIdValue = maps:get(channel_id, Context),
    UserId = maps:get(user_id, Context),
    State0 = guild_virtual_channel_access:clear_pending_join(UserId, ChannelIdValue, State),
    State1 = guild_virtual_channel_access:clear_preserve(UserId, ChannelIdValue, State0),
    State2 = guild_virtual_channel_access:clear_move_pending(UserId, ChannelIdValue, State1),
    case resolve_guild_identity(State2) of
        {error, ErrorAtom} ->
            {reply, gateway_errors:error(ErrorAtom), State2};
        {ok, GuildId, GuildIdBin} ->
            VoicePermissions = voice_utils:compute_voice_permissions(
                UserId, ChannelIdValue, State2
            ),
            TokenNonce = voice_utils:generate_token_nonce(),
            case request_voice_token(GuildId, ChannelIdValue, UserId, null, VoicePermissions, TokenNonce) of
                {ok, TokenData} ->
                    Token = maps:get(token, TokenData),
                    Endpoint = maps:get(endpoint, TokenData),
                    ConnectionId = maps:get(connection_id, TokenData),
                    ChannelIdBin = integer_to_binary(ChannelIdValue),
                    UserIdBin = integer_to_binary(UserId),
                    ServerMute = maps:get(<<"mute">>, Member, false),
                    ServerDeaf = maps:get(<<"deaf">>, Member, false),
                    Flags = voice_state_utils:voice_flags_from_context(Context),
                    #{
                        self_mute := SelfMuteFlag,
                        self_deaf := SelfDeafFlag,
                        self_video := SelfVideoFlag,
                        self_stream := SelfStreamFlag,
                        is_mobile := IsMobileFlag
                    } = Flags,
                    SessionIdValue = maps:get(session_id, Context, undefined),
                    SessionIdBin = normalize_session_id(SessionIdValue),
                    VoiceState0 = guild_voice_state:create_voice_state(
                        GuildIdBin,
                        ChannelIdBin,
                        UserIdBin,
                        ConnectionId,
                        ServerMute,
                        ServerDeaf,
                        Flags,
                        ParsedViewerStreamKey
                    ),
                    VoiceState1 = maybe_attach_session_id(VoiceState0, SessionIdBin),
                    VoiceState = maybe_attach_member(VoiceState1, Member),
                    Now = erlang:system_time(millisecond),
                    PendingMetadata = #{
                        user_id => UserId,
                        guild_id => GuildId,
                        channel_id => ChannelIdValue,
                        session_id => SessionIdBin,
                        self_mute => SelfMuteFlag,
                        self_deaf => SelfDeafFlag,
                        self_video => SelfVideoFlag,
                        self_stream => SelfStreamFlag,
                        is_mobile => IsMobileFlag,
                        server_mute => ServerMute,
                        server_deaf => ServerDeaf,
                        member => Member,
                        viewer_stream_keys => ParsedViewerStreamKey,
                        voice_state => VoiceState,
                        token_nonce => TokenNonce,
                        created_at => Now,
                        expires_at => Now + 30000
                    },
                    PendingConnections = pending_voice_connections(State2),
                    NewPendingConnections = maps:put(
                        ConnectionId,
                        PendingMetadata,
                        PendingConnections
                    ),
                    NewState = maps:put(
                        pending_voice_connections, NewPendingConnections, State2
                    ),
                    {reply,
                        #{
                            success => true,
                            token => Token,
                            endpoint => Endpoint,
                            connection_id => ConnectionId,
                            voice_state => VoiceState
                        },
                        NewState};
                {error, _Reason} ->
                    {reply, gateway_errors:error(voice_token_failed), State}
            end
    end.

-spec build_context(map()) -> context().
build_context(Request0) ->
    Request = map_utils:ensure_map(Request0),
    RawConnectionId = maps:get(connection_id, Request, undefined),
    #{
        user_id => normalize_user_id(maps:get(user_id, Request, undefined)),
        channel_id => normalize_channel_id_value(maps:get(channel_id, Request, null)),
        session_id => maps:get(session_id, Request, undefined),
        connection_id => normalize_connection_id(RawConnectionId),
        raw_connection_id => RawConnectionId,
        self_mute => normalize_boolean(maps:get(self_mute, Request, false)),
        self_deaf => normalize_boolean(maps:get(self_deaf, Request, false)),
        self_video => normalize_boolean(maps:get(self_video, Request, false)),
        self_stream => normalize_boolean(maps:get(self_stream, Request, false)),
        is_mobile => normalize_boolean(maps:get(is_mobile, Request, false)),
        viewer_stream_keys => maps:get(viewer_stream_keys, Request, undefined)
    }.

-spec normalize_connection_id(term()) -> binary() | undefined.
normalize_connection_id(undefined) ->
    undefined;
normalize_connection_id(null) ->
    undefined;
normalize_connection_id(ConnectionId) ->
    ConnectionId.

-spec normalize_user_id(term()) -> integer() | undefined.
normalize_user_id(Value) ->
    type_conv:to_integer(Value).

-spec normalize_channel_id_value(term()) -> integer() | null | undefined.
normalize_channel_id_value(null) ->
    null;
normalize_channel_id_value(Value) ->
    type_conv:to_integer(Value).

-spec normalize_boolean(term()) -> boolean().
normalize_boolean(true) -> true;
normalize_boolean(<<"true">>) -> true;
normalize_boolean(false) -> false;
normalize_boolean(<<"false">>) -> false;
normalize_boolean(_) -> false.

-spec maybe_attach_session_id(voice_state(), binary() | undefined) -> voice_state().
maybe_attach_session_id(VoiceState, undefined) ->
    VoiceState;
maybe_attach_session_id(VoiceState, SessionId) when is_binary(SessionId) ->
    maps:put(<<"session_id">>, SessionId, VoiceState).

-spec maybe_attach_member(voice_state(), map()) -> voice_state().
maybe_attach_member(VoiceState, Member) when is_map(Member) ->
    case maps:size(Member) of
        0 -> VoiceState;
        _ -> maps:put(<<"member">>, Member, VoiceState)
    end.

-spec normalize_session_id(term()) -> binary() | undefined.
normalize_session_id(undefined) ->
    undefined;
normalize_session_id(null) ->
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
        _:_ -> undefined
    end.

-spec resolve_viewer_stream_keys(
    context(), integer() | undefined, integer(), voice_state_map(), voice_state()
) ->
    {ok, list()} | {error, atom()}.
resolve_viewer_stream_keys(Context, GuildId, ChannelIdValue, VoiceStates, ExistingVoiceState) ->
    RawKeys = maps:get(viewer_stream_keys, Context, undefined),
    case RawKeys of
        undefined ->
            {ok, maps:get(<<"viewer_stream_keys">>, ExistingVoiceState, [])};
        null ->
            {ok, []};
        Keys when is_list(Keys) ->
            validate_viewer_stream_keys(Keys, GuildId, ChannelIdValue, VoiceStates, []);
        _ ->
            {error, voice_invalid_state}
    end.

-spec validate_viewer_stream_keys(
    list(), integer() | undefined, integer(), voice_state_map(), list()
) ->
    {ok, list()} | {error, atom()}.
validate_viewer_stream_keys([], _GuildId, _ChannelIdValue, _VoiceStates, Acc) ->
    {ok, lists:reverse(Acc)};
validate_viewer_stream_keys([Key | Rest], GuildId, ChannelIdValue, VoiceStates, Acc) ->
    case validate_single_viewer_stream_key(Key, GuildId, ChannelIdValue, VoiceStates) of
        {ok, ValidKey} ->
            validate_viewer_stream_keys(Rest, GuildId, ChannelIdValue, VoiceStates, [ValidKey | Acc]);
        {error, _} = Error ->
            Error
    end.

-spec validate_single_viewer_stream_key(
    term(), integer() | undefined, integer(), voice_state_map()
) ->
    {ok, binary()} | {error, atom()}.
validate_single_viewer_stream_key(RawKey, _GuildId, _ChannelIdValue, _VoiceStates) when not is_binary(RawKey) ->
    {error, voice_invalid_state};
validate_single_viewer_stream_key(RawKey, GuildId, ChannelIdValue, VoiceStates) ->
    case voice_state_utils:parse_stream_key(RawKey) of
        {error, _} ->
            {error, voice_invalid_state};
        {ok, #{
            scope := guild,
            guild_id := ParsedGuildId,
            channel_id := ParsedChannelId,
            connection_id := StreamConnId
        }} when
            is_integer(ChannelIdValue), ParsedChannelId =:= ChannelIdValue
        ->
            GuildScopeCheck =
                case GuildId of
                    undefined -> ok;
                    ParsedGuildId -> ok;
                    _ -> error
                end,
            case GuildScopeCheck of
                ok ->
                    case maps:get(StreamConnId, VoiceStates, undefined) of
                        undefined ->
                            {error, voice_connection_not_found};
                        StreamVS ->
                            case
                                map_utils:get_integer(StreamVS, <<"channel_id">>, undefined)
                            of
                                ChannelIdValue -> {ok, RawKey};
                                _ -> {error, voice_invalid_state}
                            end
                    end;
                error ->
                    {error, voice_invalid_state}
            end;
        {ok, #{scope := dm, channel_id := ParsedChannelId}} when
            is_integer(ChannelIdValue), ParsedChannelId =:= ChannelIdValue
        ->
            {ok, RawKey};
        _ ->
            {error, voice_invalid_state}
    end.

-spec check_camera_user_limit(context(), integer(), voice_state_map()) -> ok | {error, atom()}.
check_camera_user_limit(Context, ChannelIdValue, VoiceStates) ->
    SelfVideo = maps:get(self_video, Context, false),
    case SelfVideo of
        false ->
            ok;
        true ->
            UserIds = lists:foldl(
                fun({_ConnId, VS}, Acc) ->
                    case map_utils:get_integer(VS, <<"channel_id">>, undefined) of
                        ChannelIdValue ->
                            UserId = map_utils:get_integer(VS, <<"user_id">>, undefined),
                            case UserId of
                                undefined -> Acc;
                                _ -> sets:add_element(UserId, Acc)
                            end;
                        _ ->
                            Acc
                    end
                end,
                sets:new(),
                maps:to_list(VoiceStates)
            ),
            case sets:size(UserIds) > 25 of
                true -> {error, voice_camera_user_limit};
                false -> ok
            end
    end.

-spec resolve_voice_state_from_pending(binary(), map(), guild_state(), voice_state_map()) ->
    voice_state() | undefined.
resolve_voice_state_from_pending(ConnectionId, PendingData, State, VoiceStates) ->
    case maps:get(ConnectionId, VoiceStates, undefined) of
        VoiceState when is_map(VoiceState) ->
            VoiceState;
        _ ->
            case maps:get(voice_state, PendingData, undefined) of
                VoiceState when is_map(VoiceState) ->
                    VoiceState;
                _ ->
                    build_voice_state_from_pending(PendingData, ConnectionId, State)
            end
    end.

-spec build_voice_state_from_pending(map(), binary(), guild_state()) -> voice_state() | undefined.
build_voice_state_from_pending(PendingData, ConnectionId, State) ->
    GuildIdState = map_utils:get_integer(State, id, undefined),
    GuildId0 = pending_get_integer(PendingData, guild_id),
    GuildId =
        case GuildId0 of
            undefined -> GuildIdState;
            _ -> GuildId0
        end,
    ChannelId = pending_get_integer(PendingData, channel_id),
    UserId = pending_get_integer(PendingData, user_id),
    case {GuildId, ChannelId, UserId} of
        {undefined, _, _} ->
            undefined;
        {_, undefined, _} ->
            undefined;
        {_, _, undefined} ->
            undefined;
        {GId, ChId, UId} ->
            GuildIdBin = integer_to_binary(GId),
            ChannelIdBin = integer_to_binary(ChId),
            UserIdBin = integer_to_binary(UId),
            Flags = #{
                self_mute => pending_get_boolean(PendingData, self_mute),
                self_deaf => pending_get_boolean(PendingData, self_deaf),
                self_video => pending_get_boolean(PendingData, self_video),
                self_stream => pending_get_boolean(PendingData, self_stream),
                is_mobile => pending_get_boolean(PendingData, is_mobile)
            },
            ServerMute = pending_get_boolean(PendingData, server_mute),
            ServerDeaf = pending_get_boolean(PendingData, server_deaf),
            ViewerStreamKeys = pending_get_value(PendingData, viewer_stream_keys),
            VoiceState0 = guild_voice_state:create_voice_state(
                GuildIdBin,
                ChannelIdBin,
                UserIdBin,
                ConnectionId,
                ServerMute,
                ServerDeaf,
                Flags,
                ViewerStreamKeys
            ),
            SessionId = pending_get_binary(PendingData, session_id),
            Member = pending_get_map(PendingData, member),
            VoiceState1 = maybe_attach_session_id(VoiceState0, SessionId),
            maybe_attach_member(VoiceState1, Member)
    end.

-spec pending_get_value(map(), atom()) -> term().
pending_get_value(PendingData, Key) ->
    case maps:get(Key, PendingData, undefined) of
        undefined ->
            BinKey = atom_to_binary(Key, utf8),
            maps:get(BinKey, PendingData, undefined);
        Value ->
            Value
    end.

-spec pending_get_integer(map(), atom()) -> integer() | undefined.
pending_get_integer(PendingData, Key) ->
    case pending_get_value(PendingData, Key) of
        undefined -> undefined;
        Value -> type_conv:to_integer(Value)
    end.

-spec pending_get_boolean(map(), atom()) -> boolean().
pending_get_boolean(PendingData, Key) ->
    case pending_get_value(PendingData, Key) of
        true -> true;
        false -> false;
        _ -> false
    end.

-spec pending_get_binary(map(), atom()) -> binary() | undefined.
pending_get_binary(PendingData, Key) ->
    case pending_get_value(PendingData, Key) of
        undefined -> undefined;
        Value -> type_conv:to_binary(Value)
    end.

-spec pending_get_map(map(), atom()) -> map().
pending_get_map(PendingData, Key) ->
    case pending_get_value(PendingData, Key) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    end.

-spec resolve_guild_identity(guild_state()) ->
    {ok, integer(), binary()} | {error, atom()}.
resolve_guild_identity(State) ->
    Data = guild_data(State),
    DataGuildIdBin = maps:get(<<"id">>, Data, undefined),
    StateGuildId = map_utils:get_integer(State, id, undefined),
    GuildMeta = map_utils:ensure_map(maps:get(<<"guild">>, Data, #{})),
    GuildMetaIdBin = maps:get(<<"id">>, GuildMeta, undefined),
    resolve_guild_id_priority([
        {DataGuildIdBin, fun normalize_guild_id/1},
        {StateGuildId, fun normalize_guild_id/1},
        {GuildMetaIdBin, fun normalize_guild_id/1}
    ]).

-spec resolve_guild_id_priority([
    {term(), fun((term()) -> {ok, integer(), binary()} | {error, atom()})}
]) ->
    {ok, integer(), binary()} | {error, atom()}.
resolve_guild_id_priority([]) ->
    {error, voice_guild_id_missing};
resolve_guild_id_priority([{undefined, _} | Rest]) ->
    resolve_guild_id_priority(Rest);
resolve_guild_id_priority([{Value, NormalizeFun} | _]) ->
    NormalizeFun(Value).

-spec normalize_guild_id(term()) -> {ok, integer(), binary()} | {error, atom()}.
normalize_guild_id(Value) ->
    case type_conv:to_integer(Value) of
        undefined ->
            {error, voice_invalid_guild_id};
        Int ->
            {ok, Int, guild_id_binary(Value, Int)}
    end.

-spec guild_id_binary(term(), integer()) -> binary().
guild_id_binary(Value, Int) ->
    case type_conv:to_binary(Value) of
        undefined -> integer_to_binary(Int);
        Bin -> Bin
    end.

-spec guild_data(guild_state()) -> map().
guild_data(State) ->
    map_utils:ensure_map(maps:get(data, State, #{})).

-spec confirm_voice_connection_from_livekit(map(), guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
confirm_voice_connection_from_livekit(Request, State) ->
    ConnectionId = maps:get(connection_id, Request, undefined),
    TokenNonce = maps:get(token_nonce, Request, undefined),
    case ConnectionId of
        undefined ->
            {reply, gateway_errors:error(voice_missing_connection_id), State};
        _ ->
            logger:debug(
                "Confirming voice connection from LiveKit",
                #{connection_id => ConnectionId, token_nonce => TokenNonce}
            ),
            PendingConnections = pending_voice_connections(State),
            case maps:get(ConnectionId, PendingConnections, undefined) of
                undefined ->
                    logger:debug(
                        "No pending voice connection found for LiveKit confirm",
                        #{connection_id => ConnectionId}
                    ),
                    VoiceStates = voice_state_utils:voice_states(State),
                    case maps:get(ConnectionId, VoiceStates, undefined) of
                        VoiceState when is_map(VoiceState) ->
                            {reply, #{success => true}, State};
                        _ ->
                            try_restore_from_recently_disconnected(ConnectionId, State)
                    end;
                PendingData ->
                    logger:debug(
                        "Found pending voice connection for LiveKit confirm",
                        #{
                            connection_id => ConnectionId,
                            pending_user_id => pending_get_integer(PendingData, user_id),
                            pending_channel_id => pending_get_integer(PendingData, channel_id)
                        }
                    ),
                    case validate_pending_nonce_and_expiry(TokenNonce, PendingData) of
                        {error, ErrorAtom} ->
                            logger:debug(
                                "LiveKit confirm rejected due to pending validation",
                                #{connection_id => ConnectionId, error => ErrorAtom}
                            ),
                            {reply, gateway_errors:error(ErrorAtom), State};
                        ok ->
                            VoiceStates = voice_state_utils:voice_states(State),
                            VoiceState = resolve_voice_state_from_pending(
                                ConnectionId, PendingData, State, VoiceStates
                            ),
                            NewPendingConnections = maps:remove(ConnectionId, PendingConnections),
                            StateWithoutPending = maps:put(
                                pending_voice_connections, NewPendingConnections, State
                            ),
                            case VoiceState of
                                undefined ->
                                    {reply, #{success => true}, StateWithoutPending};
                                _ ->
                                    UpdatedVoiceStates = maps:put(ConnectionId, VoiceState, VoiceStates),
                                    StateWithVoiceStates = maps:put(
                                        voice_states, UpdatedVoiceStates, StateWithoutPending
                                    ),
                                    StateCleared = clear_virtual_access_flags_from_voice_state(
                                        VoiceState, StateWithVoiceStates
                                    ),
                                    ChannelIdBin = maps:get(<<"channel_id">>, VoiceState, null),
                                    guild_voice_broadcast:broadcast_voice_state_update(
                                        VoiceState, StateCleared, ChannelIdBin
                                    ),
                                    {reply, #{success => true}, StateCleared}
                            end
                    end
            end
    end.

-spec validate_pending_nonce_and_expiry(binary() | undefined, map()) -> ok | {error, atom()}.
validate_pending_nonce_and_expiry(TokenNonce, PendingData) ->
    ExpiresAt = maps:get(expires_at, PendingData, undefined),
    Now = erlang:system_time(millisecond),
    case ExpiresAt of
        ExpiresAtVal when is_integer(ExpiresAtVal), Now >= ExpiresAtVal ->
            {error, voice_pending_expired};
        _ ->
            PendingNonce = maps:get(token_nonce, PendingData, undefined),
            case TokenNonce of
                undefined ->
                    ok;
                PendingNonce ->
                    ok;
                _ ->
                    {error, voice_nonce_mismatch}
            end
    end.

-spec try_restore_from_recently_disconnected(binary(), guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
try_restore_from_recently_disconnected(ConnectionId, State) ->
    Cache = guild_voice_disconnect:recently_disconnected_voice_states(State),
    Now = erlang:system_time(millisecond),
    case maps:get(ConnectionId, Cache, undefined) of
        #{voice_state := VoiceState, disconnected_at := DisconnectedAt} when
            (Now - DisconnectedAt) < 60000
        ->
            restore_recently_disconnected(ConnectionId, VoiceState, Cache, State);
        _ ->
            {reply, gateway_errors:error(voice_connection_not_found), State}
    end.

-spec restore_recently_disconnected(binary(), voice_state(), map(), guild_state()) ->
    {reply, map(), guild_state()}.
restore_recently_disconnected(ConnectionId, VoiceState, Cache, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    UpdatedVoiceStates = maps:put(ConnectionId, VoiceState, VoiceStates),
    NewCache = maps:remove(ConnectionId, Cache),
    State0 = maps:put(voice_states, UpdatedVoiceStates, State),
    State1 = maps:put(recently_disconnected_voice_states, NewCache, State0),
    ChannelIdBin = maps:get(<<"channel_id">>, VoiceState, null),
    guild_voice_broadcast:broadcast_voice_state_update(VoiceState, State1, ChannelIdBin),
    {reply, #{success => true}, State1}.

-spec clear_virtual_access_flags_from_voice_state(voice_state(), guild_state()) -> guild_state().
clear_virtual_access_flags_from_voice_state(VoiceState, State) when is_map(VoiceState) ->
    UserId = map_utils:get_integer(VoiceState, <<"user_id">>, undefined),
    ChannelId = map_utils:get_integer(VoiceState, <<"channel_id">>, undefined),
    case is_integer(UserId) andalso is_integer(ChannelId) of
        true ->
            State0 = guild_virtual_channel_access:clear_pending_join(UserId, ChannelId, State),
            State1 = guild_virtual_channel_access:clear_preserve(UserId, ChannelId, State0),
            guild_virtual_channel_access:clear_move_pending(UserId, ChannelId, State1);
        false ->
            State
    end.

-spec request_voice_token(integer(), integer(), integer(), map()) ->
    {ok, map()} | {error, term()}.
request_voice_token(GuildId, ChannelId, UserId, VoicePermissions) ->
    request_voice_token(GuildId, ChannelId, UserId, null, VoicePermissions).

-spec request_voice_token(integer(), integer(), integer(), binary() | null, map()) ->
    {ok, map()} | {error, term()}.
request_voice_token(GuildId, ChannelId, UserId, ConnectionId, VoicePermissions) ->
    request_voice_token(GuildId, ChannelId, UserId, ConnectionId, VoicePermissions, null).

-spec request_voice_token(integer(), integer(), integer(), binary() | null, map(), binary() | null) ->
    {ok, map()} | {error, term()}.
request_voice_token(GuildId, ChannelId, UserId, ConnectionId, VoicePermissions, TokenNonce) ->
    Req = voice_utils:build_voice_token_rpc_request(
        GuildId, ChannelId, UserId, ConnectionId, null, null, VoicePermissions, TokenNonce
    ),
    case rpc_client:call(Req) of
        {ok, Data} ->
            {ok, #{
                token => maps:get(<<"token">>, Data),
                endpoint => maps:get(<<"endpoint">>, Data),
                connection_id => maps:get(<<"connectionId">>, Data)
            }};
        {error, {rpc_error, _Status, Body}} ->
            case parse_unclaimed_error(Body) of
                true ->
                    {error, voice_unclaimed_account};
                false ->
                    {error, voice_token_failed}
            end;
        {error, _Reason} ->
            {error, voice_token_failed}
    end.

-spec pending_voice_connections(guild_state()) -> pending_voice_connections().
pending_voice_connections(State) ->
    case maps:get(pending_voice_connections, State, undefined) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    end.

-spec sweep_expired_pending_joins(guild_state()) -> guild_state().
sweep_expired_pending_joins(State) ->
    Now = erlang:system_time(millisecond),
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    {Expired, Remaining} = maps:fold(
        fun(ConnId, Metadata, {ExpAcc, RemAcc}) ->
            ExpiresAt = maps:get(expires_at, Metadata, Now + 999999),
            case Now >= ExpiresAt of
                true -> {[{ConnId, Metadata} | ExpAcc], RemAcc};
                false -> {ExpAcc, maps:put(ConnId, Metadata, RemAcc)}
            end
        end,
        {[], #{}},
        PendingConnections
    ),
    lists:foreach(
        fun({ConnId, Metadata}) ->
            UserId = maps:get(user_id, Metadata, undefined),
            GuildId = maps:get(guild_id, Metadata, undefined),
            ChannelId = maps:get(channel_id, Metadata, undefined),
            case {GuildId, ChannelId, UserId} of
                {GId, CId, UId} when is_integer(GId), is_integer(CId), is_integer(UId) ->
                    spawn(fun() ->
                        guild_voice_disconnect:force_disconnect_participant(GId, CId, UId, ConnId)
                    end);
                _ ->
                    ok
            end
        end,
        Expired
    ),
    StateCleared = lists:foldl(
        fun({_ConnId, Metadata}, AccState) ->
            ExpUserId = maps:get(user_id, Metadata, undefined),
            ExpChannelId = maps:get(channel_id, Metadata, undefined),
            case is_integer(ExpUserId) andalso is_integer(ExpChannelId) of
                true ->
                    S1 = guild_virtual_channel_access:clear_pending_join(
                        ExpUserId, ExpChannelId, AccState
                    ),
                    S2 = guild_virtual_channel_access:clear_preserve(
                        ExpUserId, ExpChannelId, S1
                    ),
                    guild_virtual_channel_access:clear_move_pending(
                        ExpUserId, ExpChannelId, S2
                    );
                false ->
                    AccState
            end
        end,
        State,
        Expired
    ),
    maps:put(pending_voice_connections, Remaining, StateCleared).

-ifdef(TEST).

required_voice_perms() ->
    constants:view_channel_permission() bor constants:connect_permission().

base_test_member(UserId) ->
    #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}}.

base_test_channel(ChannelId) ->
    #{
        <<"id">> => integer_to_binary(ChannelId),
        <<"type">> => 2,
        <<"user_limit">> => 0
    }.

base_test_state() ->
    #{
        id => 999,
        data => #{
            <<"channels">> => [base_test_channel(100)],
            <<"members">> => [base_test_member(10)]
        },
        voice_states => #{},
        test_perm_fun => fun(_) -> required_voice_perms() end
    }.

replace_channels(State, Channels) ->
    Data0 = maps:get(data, State, #{}),
    Data1 = maps:put(<<"channels">>, Channels, Data0),
    maps:put(data, Data1, State).

replace_guild_id(State, GuildIdValue) ->
    Data0 = maps:get(data, State, #{}),
    Data1 = maps:put(<<"id">>, GuildIdValue, Data0),
    maps:put(data, Data1, State).

replace_guild_meta_id(State, GuildIdValue) ->
    Data0 = maps:get(data, State, #{}),
    Guild0 = maps:get(<<"guild">>, Data0, #{}),
    Guild1 = maps:put(<<"id">>, GuildIdValue, Guild0),
    Data1 = maps:put(<<"guild">>, Guild1, Data0),
    maps:put(data, Data1, State).

build_context_normalizes_fields_test() ->
    Request = #{
        user_id => <<"42">>,
        channel_id => <<"99">>,
        connection_id => <<"conn">>,
        self_mute => true,
        self_deaf => <<"nope">>,
        self_video => true,
        self_stream => false,
        is_mobile => <<"yes">>
    },
    Context = build_context(Request),
    ?assertEqual(42, maps:get(user_id, Context)),
    ?assertEqual(99, maps:get(channel_id, Context)),
    ?assertEqual(<<"conn">>, maps:get(connection_id, Context)),
    ?assertEqual(true, maps:get(self_mute, Context)),
    ?assertEqual(false, maps:get(self_deaf, Context)),
    ?assertEqual(true, maps:get(self_video, Context)),
    ?assertEqual(false, maps:get(self_stream, Context)),
    ?assertEqual(false, maps:get(is_mobile, Context)).

resolve_guild_identity_prefers_data_test() ->
    State = #{
        id => 7,
        data => #{
            <<"id">> => <<"555">>,
            <<"guild">> => #{<<"id">> => <<"111">>}
        }
    },
    ?assertMatch({ok, 555, <<"555">>}, resolve_guild_identity(State)).

normalize_guild_id_invalid_test() ->
    ?assertMatch({error, voice_invalid_guild_id}, normalize_guild_id(foo)).

voice_state_update_invalid_user_id_test() ->
    {reply, {error, validation_error, voice_invalid_user_id}, _} =
        voice_state_update(#{channel_id => null}, #{}).

voice_state_update_member_not_found_test() ->
    State = base_test_state(),
    {reply, {error, not_found, voice_member_not_found}, _} =
        voice_state_update(#{user_id => 99, channel_id => null}, State).

voice_state_update_invalid_channel_id_test() ->
    State = base_test_state(),
    {reply, {error, validation_error, voice_invalid_channel_id}, _} =
        voice_state_update(#{user_id => 10, channel_id => undefined}, State).

voice_state_update_channel_not_found_test() ->
    State = replace_channels(base_test_state(), []),
    {reply, {error, not_found, voice_channel_not_found}, _} =
        voice_state_update(#{user_id => 10, channel_id => 999}, State).

voice_state_update_connection_not_found_test() ->
    State = base_test_state(),
    Request = #{
        user_id => 10,
        channel_id => 100,
        connection_id => <<"missing-conn">>
    },
    {reply, {error, not_found, voice_connection_not_found}, _} =
        voice_state_update(Request, State).

voice_state_update_invalid_viewer_stream_keys_test() ->
    VoiceStates = #{
        <<"conn-1">> => #{
            <<"channel_id">> => <<"100">>,
            <<"user_id">> => <<"10">>
        }
    },
    State = maps:put(voice_states, VoiceStates, base_test_state()),
    Request = #{
        user_id => 10,
        channel_id => 100,
        connection_id => <<"conn-1">>,
        viewer_stream_keys => 123
    },
    {reply, {error, validation_error, voice_invalid_state}, _} =
        voice_state_update(Request, State).

voice_state_update_viewer_stream_keys_missing_connection_test() ->
    VoiceStates = #{
        <<"conn-1">> => #{
            <<"channel_id">> => <<"100">>,
            <<"user_id">> => <<"10">>
        }
    },
    State = maps:put(voice_states, VoiceStates, base_test_state()),
    Request = #{
        user_id => 10,
        channel_id => 100,
        connection_id => <<"conn-1">>,
        viewer_stream_keys => [<<"999:100:missing-conn">>]
    },
    {reply, {error, not_found, voice_connection_not_found}, _} =
        voice_state_update(Request, State).

voice_state_update_guild_id_missing_test() ->
    State0 = base_test_state(),
    State1 = replace_guild_id(State0, undefined),
    State2 = replace_guild_meta_id(State1, undefined),
    State = maps:remove(id, State2),
    Request = #{
        user_id => 10,
        channel_id => 100
    },
    {reply, {error, validation_error, voice_guild_id_missing}, _} =
        voice_state_update(Request, State).

voice_state_update_invalid_guild_id_test() ->
    State0 = base_test_state(),
    State1 = replace_guild_id(State0, <<"nope">>),
    State2 = replace_guild_meta_id(State1, <<"nope">>),
    State = maps:put(id, undefined, State2),
    Request = #{
        user_id => 10,
        channel_id => 100
    },
    {reply, {error, validation_error, voice_invalid_guild_id}, _} =
        voice_state_update(Request, State).

confirm_voice_connection_missing_id_test() ->
    State = base_test_state(),
    {reply, {error, validation_error, voice_missing_connection_id}, _} =
        confirm_voice_connection_from_livekit(#{}, State).

confirm_voice_connection_moves_pending_to_voice_states_test() ->
    VoiceState = #{
        <<"user_id">> => <<"5">>,
        <<"guild_id">> => <<"999">>,
        <<"channel_id">> => <<"100">>
    },
    PendingData = #{
        user_id => 5,
        guild_id => 999,
        channel_id => 100,
        voice_state => VoiceState
    },
    PendingConnections = #{<<"conn1">> => PendingData},
    State = maps:merge(base_test_state(), #{
        pending_voice_connections => PendingConnections,
        voice_states => #{}
    }),
    {reply, #{success := true}, NewState} =
        confirm_voice_connection_from_livekit(#{connection_id => <<"conn1">>}, State),
    NewVoiceStates = maps:get(voice_states, NewState),
    NewPending = maps:get(pending_voice_connections, NewState, #{}),
    ?assert(maps:is_key(<<"conn1">>, NewVoiceStates)),
    ?assertNot(maps:is_key(<<"conn1">>, NewPending)).

confirm_voice_connection_clears_pending_even_without_voice_state_test() ->
    PendingData = #{
        user_id => 5,
        channel_id => 100
    },
    PendingConnections = #{<<"conn1">> => PendingData},
    State = maps:merge(base_test_state(), #{
        pending_voice_connections => PendingConnections,
        voice_states => #{}
    }),
    {reply, #{success := true}, NewState} =
        confirm_voice_connection_from_livekit(#{connection_id => <<"conn1">>}, State),
    NewPending = maps:get(pending_voice_connections, NewState, #{}),
    ?assertNot(maps:is_key(<<"conn1">>, NewPending)).

confirm_voice_connection_not_found_in_pending_test() ->
    State = maps:merge(base_test_state(), #{
        pending_voice_connections => #{}
    }),
    {reply, {error, not_found, voice_connection_not_found}, _} =
        confirm_voice_connection_from_livekit(#{connection_id => <<"missing">>}, State).

confirm_voice_connection_found_in_voice_states_test() ->
    VoiceState = #{
        <<"user_id">> => <<"5">>,
        <<"guild_id">> => <<"999">>,
        <<"channel_id">> => <<"200">>
    },
    State = maps:merge(base_test_state(), #{
        pending_voice_connections => #{},
        voice_states => #{<<"conn1">> => VoiceState}
    }),
    {reply, #{success := true}, NewState} =
        confirm_voice_connection_from_livekit(#{connection_id => <<"conn1">>}, State),
    NewVoiceStates = maps:get(voice_states, NewState),
    ?assert(maps:is_key(<<"conn1">>, NewVoiceStates)),
    ?assertEqual(VoiceState, maps:get(<<"conn1">>, NewVoiceStates)).

resolve_voice_state_from_pending_uses_stored_voice_state_test() ->
    VoiceState = #{
        <<"user_id">> => <<"5">>,
        <<"guild_id">> => <<"999">>,
        <<"channel_id">> => <<"100">>
    },
    PendingData = #{
        user_id => 5,
        guild_id => 999,
        channel_id => 100,
        voice_state => VoiceState
    },
    State = base_test_state(),
    Result = resolve_voice_state_from_pending(<<"conn1">>, PendingData, State, #{}),
    ?assertEqual(VoiceState, Result).

resolve_voice_state_from_pending_prefers_existing_voice_state_test() ->
    ExistingVoiceState = #{
        <<"user_id">> => <<"5">>,
        <<"guild_id">> => <<"999">>,
        <<"channel_id">> => <<"100">>,
        <<"existing">> => true
    },
    PendingVoiceState = #{
        <<"user_id">> => <<"5">>,
        <<"guild_id">> => <<"999">>,
        <<"channel_id">> => <<"100">>,
        <<"existing">> => false
    },
    PendingData = #{
        user_id => 5,
        voice_state => PendingVoiceState
    },
    VoiceStates = #{<<"conn1">> => ExistingVoiceState},
    State = base_test_state(),
    Result = resolve_voice_state_from_pending(<<"conn1">>, PendingData, State, VoiceStates),
    ?assertEqual(ExistingVoiceState, Result).

normalize_boolean_test() ->
    ?assertEqual(true, normalize_boolean(true)),
    ?assertEqual(true, normalize_boolean(<<"true">>)),
    ?assertEqual(false, normalize_boolean(false)),
    ?assertEqual(false, normalize_boolean(<<"false">>)),
    ?assertEqual(false, normalize_boolean(<<"other">>)),
    ?assertEqual(false, normalize_boolean(123)).

normalize_session_id_test() ->
    ?assertEqual(undefined, normalize_session_id(undefined)),
    ?assertEqual(undefined, normalize_session_id(null)),
    ?assertEqual(<<"abc">>, normalize_session_id(<<"abc">>)),
    ?assertEqual(<<"123">>, normalize_session_id(123)),
    ?assertEqual(<<"test">>, normalize_session_id("test")).

pending_get_value_test() ->
    Data = #{key1 => value1, <<"key2">> => value2},
    ?assertEqual(value1, pending_get_value(Data, key1)),
    ?assertEqual(value2, pending_get_value(Data, key2)),
    ?assertEqual(undefined, pending_get_value(Data, missing)).

try_restore_from_recently_disconnected_restores_test() ->
    VS = #{
        <<"user_id">> => <<"5">>,
        <<"guild_id">> => <<"10">>,
        <<"channel_id">> => <<"20">>,
        <<"connection_id">> => <<"conn">>
    },
    Now = erlang:system_time(millisecond),
    Cache = #{<<"conn">> => #{voice_state => VS, disconnected_at => Now - 5000}},
    State = #{
        voice_states => #{},
        recently_disconnected_voice_states => Cache,
        sessions => #{},
        data => #{},
        id => 10
    },
    {reply, #{success := true}, NewState} =
        try_restore_from_recently_disconnected(<<"conn">>, State),
    NewVoiceStates = maps:get(voice_states, NewState),
    ?assert(maps:is_key(<<"conn">>, NewVoiceStates)),
    NewCache = maps:get(recently_disconnected_voice_states, NewState),
    ?assertNot(maps:is_key(<<"conn">>, NewCache)).

try_restore_from_recently_disconnected_expired_test() ->
    VS = #{
        <<"user_id">> => <<"5">>,
        <<"guild_id">> => <<"10">>,
        <<"channel_id">> => <<"20">>,
        <<"connection_id">> => <<"conn">>
    },
    Now = erlang:system_time(millisecond),
    Cache = #{<<"conn">> => #{voice_state => VS, disconnected_at => Now - 70000}},
    State = #{
        voice_states => #{},
        recently_disconnected_voice_states => Cache,
        data => #{}
    },
    {reply, {error, not_found, voice_connection_not_found}, _} =
        try_restore_from_recently_disconnected(<<"conn">>, State).

try_restore_from_recently_disconnected_not_found_test() ->
    State = #{voice_states => #{}, data => #{}},
    {reply, {error, not_found, voice_connection_not_found}, _} =
        try_restore_from_recently_disconnected(<<"conn">>, State).

voice_state_update_connection_user_mismatch_test() ->
    VoiceStates = #{
        <<"conn-1">> => #{
            <<"channel_id">> => <<"100">>,
            <<"user_id">> => <<"20">>
        }
    },
    State = maps:put(voice_states, VoiceStates, base_test_state()),
    Request = #{
        user_id => 10,
        channel_id => 100,
        connection_id => <<"conn-1">>
    },
    {reply, {error, validation_error, voice_user_mismatch}, _} =
        voice_state_update(Request, State).

voice_state_update_connection_owner_match_proceeds_test() ->
    VoiceStates = #{
        <<"conn-1">> => #{
            <<"channel_id">> => <<"100">>,
            <<"user_id">> => <<"10">>
        }
    },
    State = maps:put(voice_states, VoiceStates, base_test_state()),
    Request = #{
        user_id => 10,
        channel_id => 100,
        connection_id => <<"conn-1">>
    },
    case voice_state_update(Request, State) of
        {reply, {error, validation_error, voice_user_mismatch}, _} ->
            error(should_not_get_user_mismatch);
        {reply, _, _} ->
            ok
    end.

validate_pending_nonce_valid_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = #{
        token_nonce => <<"abc123">>,
        created_at => Now - 5000,
        expires_at => Now + 25000
    },
    ?assertEqual(ok, validate_pending_nonce_and_expiry(<<"abc123">>, PendingData)).

validate_pending_nonce_mismatch_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = #{
        token_nonce => <<"abc123">>,
        created_at => Now - 5000,
        expires_at => Now + 25000
    },
    ?assertEqual(
        {error, voice_nonce_mismatch},
        validate_pending_nonce_and_expiry(<<"wrong-nonce">>, PendingData)
    ).

validate_pending_nonce_expired_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = #{
        token_nonce => <<"abc123">>,
        created_at => Now - 35000,
        expires_at => Now - 5000
    },
    ?assertEqual(
        {error, voice_pending_expired},
        validate_pending_nonce_and_expiry(<<"abc123">>, PendingData)
    ).

validate_pending_nonce_undefined_backwards_compat_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = #{
        created_at => Now - 5000,
        expires_at => Now + 25000
    },
    ?assertEqual(ok, validate_pending_nonce_and_expiry(undefined, PendingData)).

validate_pending_nonce_missing_expires_at_test() ->
    PendingData = #{
        token_nonce => <<"abc123">>
    },
    ?assertEqual(ok, validate_pending_nonce_and_expiry(<<"abc123">>, PendingData)).

sweep_expired_pending_joins_removes_expired_test() ->
    Now = erlang:system_time(millisecond),
    ExpiredMetadata = #{
        user_id => 10,
        guild_id => 999,
        channel_id => 100,
        expires_at => Now - 1000
    },
    ValidMetadata = #{
        user_id => 11,
        guild_id => 999,
        channel_id => 101,
        expires_at => Now + 25000
    },
    PendingConnections = #{
        <<"expired-conn">> => ExpiredMetadata,
        <<"valid-conn">> => ValidMetadata
    },
    State = maps:put(pending_voice_connections, PendingConnections, base_test_state()),
    NewState = sweep_expired_pending_joins(State),
    NewPending = maps:get(pending_voice_connections, NewState, #{}),
    ?assertNot(maps:is_key(<<"expired-conn">>, NewPending)),
    ?assert(maps:is_key(<<"valid-conn">>, NewPending)).

sweep_expired_pending_joins_keeps_valid_test() ->
    Now = erlang:system_time(millisecond),
    ValidMetadata1 = #{
        user_id => 10,
        guild_id => 999,
        channel_id => 100,
        expires_at => Now + 25000
    },
    ValidMetadata2 = #{
        user_id => 11,
        guild_id => 999,
        channel_id => 101,
        expires_at => Now + 30000
    },
    PendingConnections = #{
        <<"conn-1">> => ValidMetadata1,
        <<"conn-2">> => ValidMetadata2
    },
    State = maps:put(pending_voice_connections, PendingConnections, base_test_state()),
    NewState = sweep_expired_pending_joins(State),
    NewPending = maps:get(pending_voice_connections, NewState, #{}),
    ?assertEqual(2, maps:size(NewPending)),
    ?assert(maps:is_key(<<"conn-1">>, NewPending)),
    ?assert(maps:is_key(<<"conn-2">>, NewPending)).

sweep_expired_pending_joins_clears_virtual_access_test() ->
    Now = erlang:system_time(millisecond),
    ExpiredMetadata = #{
        user_id => 10,
        guild_id => 999,
        channel_id => 100,
        expires_at => Now - 1000
    },
    PendingConnections = #{<<"expired-conn">> => ExpiredMetadata},
    State = maps:put(pending_voice_connections, PendingConnections, base_test_state()),
    State1 = guild_virtual_channel_access:mark_pending_join(10, 100, State),
    State2 = guild_virtual_channel_access:mark_preserve(10, 100, State1),
    State3 = guild_virtual_channel_access:mark_move_pending(10, 100, State2),
    ?assert(guild_virtual_channel_access:is_pending_join(10, 100, State3)),
    ?assert(guild_virtual_channel_access:has_preserve(10, 100, State3)),
    ?assert(guild_virtual_channel_access:is_move_pending(10, 100, State3)),
    NewState = sweep_expired_pending_joins(State3),
    ?assertNot(guild_virtual_channel_access:is_pending_join(10, 100, NewState)),
    ?assertNot(guild_virtual_channel_access:has_preserve(10, 100, NewState)),
    ?assertNot(guild_virtual_channel_access:is_move_pending(10, 100, NewState)).

sweep_expired_pending_joins_empty_map_test() ->
    State = maps:put(pending_voice_connections, #{}, base_test_state()),
    NewState = sweep_expired_pending_joins(State),
    NewPending = maps:get(pending_voice_connections, NewState, #{}),
    ?assertEqual(0, maps:size(NewPending)).

sweep_expired_pending_joins_missing_user_id_test() ->
    Now = erlang:system_time(millisecond),
    InvalidMetadata = #{
        guild_id => 999,
        channel_id => 100,
        expires_at => Now - 1000
    },
    PendingConnections = #{<<"invalid-conn">> => InvalidMetadata},
    State = maps:put(pending_voice_connections, PendingConnections, base_test_state()),
    NewState = sweep_expired_pending_joins(State),
    NewPending = maps:get(pending_voice_connections, NewState, #{}),
    ?assertNot(maps:is_key(<<"invalid-conn">>, NewPending)).

confirm_voice_connection_validates_nonce_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = #{
        user_id => 5,
        guild_id => 999,
        channel_id => 100,
        token_nonce => <<"valid-nonce">>,
        created_at => Now - 5000,
        expires_at => Now + 25000,
        voice_state => #{
            <<"user_id">> => <<"5">>,
            <<"guild_id">> => <<"999">>,
            <<"channel_id">> => <<"100">>
        }
    },
    PendingConnections = #{<<"conn1">> => PendingData},
    State = maps:merge(base_test_state(), #{
        pending_voice_connections => PendingConnections,
        voice_states => #{}
    }),
    {reply, {error, validation_error, voice_nonce_mismatch}, _} =
        confirm_voice_connection_from_livekit(
            #{connection_id => <<"conn1">>, token_nonce => <<"wrong-nonce">>},
            State
        ).

confirm_voice_connection_validates_expiry_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = #{
        user_id => 5,
        guild_id => 999,
        channel_id => 100,
        token_nonce => <<"valid-nonce">>,
        created_at => Now - 35000,
        expires_at => Now - 5000,
        voice_state => #{
            <<"user_id">> => <<"5">>,
            <<"guild_id">> => <<"999">>,
            <<"channel_id">> => <<"100">>
        }
    },
    PendingConnections = #{<<"conn1">> => PendingData},
    State = maps:merge(base_test_state(), #{
        pending_voice_connections => PendingConnections,
        voice_states => #{}
    }),
    {reply, {error, validation_error, voice_pending_expired}, _} =
        confirm_voice_connection_from_livekit(
            #{connection_id => <<"conn1">>, token_nonce => <<"valid-nonce">>},
            State
        ).

confirm_voice_connection_accepts_valid_nonce_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = #{
        user_id => 5,
        guild_id => 999,
        channel_id => 100,
        token_nonce => <<"valid-nonce">>,
        created_at => Now - 5000,
        expires_at => Now + 25000,
        voice_state => #{
            <<"user_id">> => <<"5">>,
            <<"guild_id">> => <<"999">>,
            <<"channel_id">> => <<"100">>
        }
    },
    PendingConnections = #{<<"conn1">> => PendingData},
    State = maps:merge(base_test_state(), #{
        pending_voice_connections => PendingConnections,
        voice_states => #{}
    }),
    {reply, #{success := true}, NewState} =
        confirm_voice_connection_from_livekit(
            #{connection_id => <<"conn1">>, token_nonce => <<"valid-nonce">>},
            State
        ),
    NewVoiceStates = maps:get(voice_states, NewState),
    ?assert(maps:is_key(<<"conn1">>, NewVoiceStates)).

confirm_voice_connection_accepts_undefined_nonce_backwards_compat_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = #{
        user_id => 5,
        guild_id => 999,
        channel_id => 100,
        created_at => Now - 5000,
        expires_at => Now + 25000,
        voice_state => #{
            <<"user_id">> => <<"5">>,
            <<"guild_id">> => <<"999">>,
            <<"channel_id">> => <<"100">>
        }
    },
    PendingConnections = #{<<"conn1">> => PendingData},
    State = maps:merge(base_test_state(), #{
        pending_voice_connections => PendingConnections,
        voice_states => #{}
    }),
    {reply, #{success := true}, NewState} =
        confirm_voice_connection_from_livekit(#{connection_id => <<"conn1">>}, State),
    NewVoiceStates = maps:get(voice_states, NewState),
    ?assert(maps:is_key(<<"conn1">>, NewVoiceStates)).

-endif.
