%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_connection_confirm).

-typing([eqwalizer]).

-export([confirm_voice_connection_from_livekit/2]).

-export_type([
    guild_state/0,
    voice_state/0
]).

-type guild_state() :: map().
-type voice_state() :: map().

-spec confirm_voice_connection_from_livekit(map(), guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
confirm_voice_connection_from_livekit(Request, State) ->
    ConnectionId = maps:get(connection_id, Request, undefined),
    TokenNonce = maps:get(token_nonce, Request, undefined),
    case ConnectionId of
        undefined ->
            {reply, gateway_errors:error(voice_missing_connection_id), State};
        _ ->
            do_confirm(ConnectionId, TokenNonce, State)
    end.

-spec do_confirm(binary(), binary() | undefined, guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
do_confirm(ConnectionId, TokenNonce, State) ->
    logger:debug("Confirming voice connection from LiveKit", #{
        connection_id => ConnectionId, token_nonce => TokenNonce
    }),
    PendingConnections = guild_voice_connection_pending:pending_voice_connections(State),
    case maps:get(ConnectionId, PendingConnections, undefined) of
        undefined ->
            handle_no_pending(ConnectionId, State);
        PendingData ->
            handle_pending_found(
                ConnectionId, TokenNonce, PendingData, PendingConnections, State
            )
    end.

-spec handle_no_pending(binary(), guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
handle_no_pending(ConnectionId, State) ->
    logger:debug("No pending voice connection found for LiveKit confirm", #{
        connection_id => ConnectionId
    }),
    VoiceStates = voice_state_utils:voice_states(State),
    case maps:get(ConnectionId, VoiceStates, undefined) of
        VoiceState when is_map(VoiceState) ->
            {reply, #{success => true}, State};
        _ ->
            try_restore_from_recently_disconnected(ConnectionId, State)
    end.

-spec handle_pending_found(binary(), binary() | undefined, map(), map(), guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
handle_pending_found(ConnectionId, TokenNonce, PendingData, PendingConnections, State) ->
    log_pending_found(ConnectionId, PendingData),
    case
        guild_voice_connection_pending:validate_pending_nonce_and_expiry(
            TokenNonce, PendingData
        )
    of
        {error, ErrorAtom} ->
            logger:debug("LiveKit confirm rejected", #{
                connection_id => ConnectionId, error => ErrorAtom
            }),
            {reply, gateway_errors:error(ErrorAtom), State};
        ok ->
            activate_pending(ConnectionId, PendingData, PendingConnections, State)
    end.

-spec log_pending_found(binary(), map()) -> ok.
log_pending_found(ConnectionId, PendingData) ->
    logger:debug(
        "Found pending voice connection for LiveKit confirm",
        #{
            connection_id => ConnectionId,
            pending_user_id => maps:get(user_id, PendingData, undefined),
            pending_channel_id => maps:get(channel_id, PendingData, undefined)
        }
    ).

-spec activate_pending(binary(), map(), map(), guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
activate_pending(ConnectionId, PendingData, PendingConnections, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    VoiceState = guild_voice_connection_pending:resolve_voice_state_from_pending(
        ConnectionId, PendingData, State, VoiceStates
    ),
    NewPendingConnections = maps:remove(ConnectionId, PendingConnections),
    StateWithoutPending = State#{pending_voice_connections => NewPendingConnections},
    case VoiceState of
        undefined ->
            {reply, #{success => true}, StateWithoutPending};
        _ ->
            finalize_activation(ConnectionId, VoiceState, VoiceStates, StateWithoutPending)
    end.

-spec finalize_activation(binary(), voice_state(), map(), guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
finalize_activation(ConnectionId, VoiceState, VoiceStates, State) ->
    UpdatedVoiceStates = VoiceStates#{ConnectionId => VoiceState},
    case channel_invariant_violation(VoiceState, UpdatedVoiceStates) of
        ok ->
            S1 = State#{voice_states => UpdatedVoiceStates},
            S2 = guild_voice_connection_util:clear_virtual_access_flags(VoiceState, S1),
            ChannelIdBin = maps:get(<<"channel_id">>, VoiceState, null),
            guild_voice_broadcast:broadcast_voice_state_update(VoiceState, S2, ChannelIdBin),
            {reply, #{success => true}, S2};
        {error, ErrorAtom} ->
            {reply, gateway_errors:error(ErrorAtom), State}
    end.

-spec channel_invariant_violation(voice_state(), map()) -> ok | {error, atom()}.
channel_invariant_violation(VoiceState, VoiceStates) ->
    voice_p2p:invariant_violation(
        maps:values(
            voice_state_utils:channel_voice_states(
                voice_state_utils:voice_state_channel_id(VoiceState), VoiceStates
            )
        )
    ).

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
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
restore_recently_disconnected(ConnectionId, VoiceState, Cache, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    UpdatedVoiceStates = VoiceStates#{ConnectionId => VoiceState},
    case channel_invariant_violation(VoiceState, UpdatedVoiceStates) of
        ok ->
            finalize_restore(ConnectionId, VoiceState, UpdatedVoiceStates, Cache, State);
        {error, ErrorAtom} ->
            {reply, gateway_errors:error(ErrorAtom), State}
    end.

-spec finalize_restore(binary(), voice_state(), map(), map(), guild_state()) ->
    {reply, map(), guild_state()}.
finalize_restore(ConnectionId, VoiceState, UpdatedVoiceStates, Cache, State) ->
    NewCache = maps:remove(ConnectionId, Cache),
    S0 = State#{voice_states => UpdatedVoiceStates},
    S1 = S0#{recently_disconnected_voice_states => NewCache},
    ChannelIdBin = maps:get(<<"channel_id">>, VoiceState, null),
    guild_voice_broadcast:broadcast_voice_state_update(VoiceState, S1, ChannelIdBin),
    {reply, #{success => true}, S1}.
