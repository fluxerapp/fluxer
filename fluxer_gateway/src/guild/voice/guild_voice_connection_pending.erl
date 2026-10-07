%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_connection_pending).

-typing([eqwalizer]).

-export([
    pending_voice_connections/1,
    store_pending/3,
    maybe_restore_pending_connection/5,
    resolve_voice_state_from_pending/4,
    sweep_expired_pending_joins/1,
    clear_e2ee_room_key_if_channel_idle/3,
    validate_pending_nonce_and_expiry/2
]).

-export_type([
    guild_state/0,
    voice_state/0,
    voice_state_map/0,
    pending_voice_connections/0
]).

-type guild_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.
-type pending_voice_connections() :: #{binary() => map()}.
-spec pending_voice_connections(guild_state()) -> pending_voice_connections().
pending_voice_connections(State) ->
    case maps:find(pending_voice_connections, State) of
        {ok, Map} when is_map(Map) -> Map;
        _ -> #{}
    end.

-spec store_pending(binary(), map(), guild_state()) -> guild_state().
store_pending(ConnectionId, PendingMetadata, State) ->
    PendingConnections = pending_voice_connections(State),
    State#{pending_voice_connections => PendingConnections#{ConnectionId => PendingMetadata}}.

-spec maybe_restore_pending_connection(
    binary(), integer(), integer(), voice_state_map(), guild_state()
) ->
    {ok, voice_state_map(), guild_state()} | {error, atom()}.
maybe_restore_pending_connection(ConnectionId, ChannelIdValue, UserId, VoiceStates, State) ->
    PendingConnections = pending_voice_connections(State),
    case maps:find(ConnectionId, PendingConnections) of
        error ->
            {error, voice_connection_not_found};
        {ok, PendingData} ->
            check_pending_match(
                ConnectionId,
                ChannelIdValue,
                UserId,
                PendingData,
                PendingConnections,
                VoiceStates,
                State
            )
    end.

-spec resolve_voice_state_from_pending(binary(), map(), guild_state(), voice_state_map()) ->
    voice_state() | undefined.
resolve_voice_state_from_pending(ConnectionId, PendingData, State, VoiceStates) ->
    case maps:find(ConnectionId, VoiceStates) of
        {ok, VoiceState} when is_map(VoiceState) ->
            VoiceState;
        _ ->
            resolve_from_pending_data(ConnectionId, PendingData, State)
    end.

-spec sweep_expired_pending_joins(guild_state()) -> guild_state().
sweep_expired_pending_joins(State) ->
    Now = erlang:system_time(millisecond),
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    {Expired, Remaining} = partition_expired(Now, PendingConnections),
    spawn_force_disconnects(Expired),
    StateCleared = clear_expired_virtual_flags(Expired, State),
    StateWithPending = StateCleared#{pending_voice_connections => Remaining},
    clear_expired_e2ee_keys(Expired, StateWithPending, Remaining).

-spec clear_e2ee_room_key_if_channel_idle(
    integer() | undefined, voice_state_map(), guild_state()
) -> guild_state().
clear_e2ee_room_key_if_channel_idle(ChannelId, VoiceStates, State) when is_integer(ChannelId) ->
    PendingConnections = pending_voice_connections(State),
    guild_voice_e2ee:forget_room_key_if_channel_idle_guild(
        ChannelId, VoiceStates, PendingConnections, State
    );
clear_e2ee_room_key_if_channel_idle(_, _VoiceStates, State) ->
    State.

-spec validate_pending_nonce_and_expiry(binary() | undefined, map()) -> ok | {error, atom()}.
validate_pending_nonce_and_expiry(TokenNonce, PendingData) ->
    ExpiresAt = maps:get(expires_at, PendingData, undefined),
    Now = erlang:system_time(millisecond),
    case ExpiresAt of
        ExpiresAtVal when is_integer(ExpiresAtVal), Now >= ExpiresAtVal ->
            {error, voice_pending_expired};
        _ ->
            check_nonce_match(TokenNonce, PendingData)
    end.

-spec check_nonce_match(binary() | undefined, map()) -> ok | {error, atom()}.
check_nonce_match(TokenNonce, PendingData) ->
    PendingNonce = maps:get(token_nonce, PendingData, undefined),
    case TokenNonce of
        PendingNonce -> ok;
        _ -> {error, voice_nonce_mismatch}
    end.

-spec check_pending_match(
    binary(),
    integer(),
    integer(),
    map(),
    pending_voice_connections(),
    voice_state_map(),
    guild_state()
) -> {ok, voice_state_map(), guild_state()} | {error, atom()}.
check_pending_match(
    ConnectionId, ChannelIdValue, UserId, PendingData, PendingConnections, VoiceStates, State
) ->
    PendingUserId = pending_get_snowflake(PendingData, user_id),
    PendingChannelId = pending_get_snowflake(PendingData, channel_id),
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
            check_pending_expiry(
                ConnectionId, PendingData, PendingConnections, VoiceStates, State
            );
        _ ->
            {error, voice_connection_not_found}
    end.

-spec check_pending_expiry(
    binary(), map(), pending_voice_connections(), voice_state_map(), guild_state()
) -> {ok, voice_state_map(), guild_state()} | {error, atom()}.
check_pending_expiry(ConnectionId, PendingData, PendingConnections, VoiceStates, State) ->
    case pending_get_integer(PendingData, expires_at) of
        ExpiresAt when is_integer(ExpiresAt) ->
            restore_unexpired_pending(
                ExpiresAt, ConnectionId, PendingData, PendingConnections, VoiceStates, State
            );
        _ ->
            infer_expiry_from_created_at(
                ConnectionId, PendingData, PendingConnections, VoiceStates, State
            )
    end.

-define(INFERRED_PENDING_TTL_MS, 300000).

-spec infer_expiry_from_created_at(
    binary(), map(), pending_voice_connections(), voice_state_map(), guild_state()
) -> {ok, voice_state_map(), guild_state()} | {error, atom()}.
infer_expiry_from_created_at(ConnectionId, PendingData, PendingConnections, VoiceStates, State) ->
    CreatedAt = pending_get_integer(PendingData, created_at),
    JoinedAt = pending_get_integer(PendingData, joined_at),
    Timestamp = first_valid_timestamp([CreatedAt, JoinedAt]),
    case Timestamp of
        undefined ->
            {error, voice_pending_expired};
        Ts ->
            InferredExpiry = Ts + ?INFERRED_PENDING_TTL_MS,
            restore_unexpired_pending(
                InferredExpiry,
                ConnectionId,
                PendingData,
                PendingConnections,
                VoiceStates,
                State
            )
    end.

-spec first_valid_timestamp([integer() | undefined]) -> integer() | undefined.
first_valid_timestamp([]) -> undefined;
first_valid_timestamp([V | _]) when is_integer(V) -> V;
first_valid_timestamp([_ | Rest]) -> first_valid_timestamp(Rest).

-spec restore_unexpired_pending(
    integer(), binary(), map(), pending_voice_connections(), voice_state_map(), guild_state()
) -> {ok, voice_state_map(), guild_state()} | {error, atom()}.
restore_unexpired_pending(
    ExpiresAt, ConnectionId, PendingData, PendingConnections, VoiceStates, State
) ->
    case erlang:system_time(millisecond) >= ExpiresAt of
        true ->
            {error, voice_pending_expired};
        false ->
            restore_pending(ConnectionId, PendingConnections, PendingData, VoiceStates, State)
    end.

-spec restore_pending(
    binary(), pending_voice_connections(), map(), voice_state_map(), guild_state()
) -> {ok, voice_state_map(), guild_state()} | {error, atom()}.
restore_pending(ConnectionId, PendingConnections, PendingData, VoiceStates, State) ->
    VoiceState = resolve_voice_state_from_pending(
        ConnectionId, PendingData, State, VoiceStates
    ),
    case VoiceState of
        undefined ->
            {error, voice_connection_not_found};
        _ ->
            finalize_restore(ConnectionId, VoiceState, PendingConnections, VoiceStates, State)
    end.

-spec finalize_restore(
    binary(), voice_state(), pending_voice_connections(), voice_state_map(), guild_state()
) -> {ok, voice_state_map(), guild_state()}.
finalize_restore(ConnectionId, VoiceState, PendingConnections, VoiceStates, State) ->
    NewPending = maps:remove(ConnectionId, PendingConnections),
    S1 = State#{pending_voice_connections => NewPending},
    UpdatedVS = VoiceStates#{ConnectionId => VoiceState},
    S2 = S1#{voice_states => UpdatedVS},
    S3 = guild_voice_connection_util:clear_virtual_access_flags(VoiceState, S2),
    ChannelIdBin = maps:get(<<"channel_id">>, VoiceState, null),
    guild_voice_broadcast:broadcast_voice_state_update(VoiceState, S3, ChannelIdBin),
    {ok, UpdatedVS, S3}.

-spec resolve_from_pending_data(binary(), map(), guild_state()) -> voice_state() | undefined.
resolve_from_pending_data(ConnectionId, PendingData, State) ->
    case maps:find(voice_state, PendingData) of
        {ok, VoiceState} when is_map(VoiceState) -> VoiceState;
        _ -> build_voice_state_from_pending(PendingData, ConnectionId, State)
    end.

-spec build_voice_state_from_pending(map(), binary(), guild_state()) ->
    voice_state() | undefined.
build_voice_state_from_pending(PendingData, ConnectionId, State) ->
    GuildId = resolve_pending_guild_id(PendingData, State),
    ChannelId = pending_get_snowflake(PendingData, channel_id),
    UserId = pending_get_snowflake(PendingData, user_id),
    case {GuildId, ChannelId, UserId} of
        {GId, ChId, UId} when is_integer(GId), is_integer(ChId), is_integer(UId) ->
            assemble_pending_voice_state(PendingData, ConnectionId, GId, ChId, UId);
        _ ->
            undefined
    end.

-spec resolve_pending_guild_id(map(), guild_state()) -> integer() | undefined.
resolve_pending_guild_id(PendingData, State) ->
    case pending_get_snowflake(PendingData, guild_id) of
        undefined ->
            guild_voice_connection_normalize:normalize_positive_snowflake(
                maps:get(id, State, undefined)
            );
        GuildId ->
            GuildId
    end.

-spec assemble_pending_voice_state(map(), binary(), integer(), integer(), integer()) ->
    voice_state().
assemble_pending_voice_state(PendingData, ConnectionId, GId, ChId, UId) ->
    Flags = pending_flags(PendingData),
    VS0 = guild_voice_state:create_voice_state(
        pending_create_params(PendingData, ConnectionId, GId, ChId, UId),
        Flags
    ),
    decorate_pending_voice_state(VS0, PendingData).

-spec pending_flags(map()) -> map().
pending_flags(PendingData) ->
    #{
        self_mute => pending_get_boolean(PendingData, self_mute),
        self_deaf => pending_get_boolean(PendingData, self_deaf),
        self_video => pending_get_boolean(PendingData, self_video),
        self_stream => pending_get_boolean(PendingData, self_stream),
        is_mobile => pending_get_boolean(PendingData, is_mobile),
        suppress => pending_get_boolean(PendingData, suppress)
    }.

-spec pending_create_params(map(), binary(), integer(), integer(), integer()) -> map().
pending_create_params(PendingData, ConnectionId, GId, ChId, UId) ->
    #{
        guild_id => integer_to_binary(GId),
        channel_id => integer_to_binary(ChId),
        user_id => integer_to_binary(UId),
        connection_id => ConnectionId,
        server_mute => pending_get_boolean(PendingData, server_mute),
        server_deaf => pending_get_boolean(PendingData, server_deaf),
        viewer_stream_keys => pending_get_list(PendingData, viewer_stream_keys),
        e2ee_capable => pending_get_boolean(PendingData, e2ee_capable)
    }.

-spec decorate_pending_voice_state(voice_state(), map()) -> voice_state().
decorate_pending_voice_state(VS0, PendingData) ->
    Lat = pending_get_binary(PendingData, latitude),
    Lng = pending_get_binary(PendingData, longitude),
    VS1 = guild_voice_connection_util:maybe_attach_geolocation(VS0, Lat, Lng),
    SessId = pending_get_binary(PendingData, session_id),
    VS2 = guild_voice_connection_util:maybe_attach_session_id(VS1, SessId),
    Member = pending_get_map(PendingData, member),
    VS3 = guild_voice_connection_util:maybe_attach_member(VS2, Member),
    RegionId = pending_get_binary(PendingData, region_id),
    ServerId = pending_get_binary(PendingData, server_id),
    guild_voice_connection_util:maybe_attach_voice_routing_metadata(
        VS3, RegionId, ServerId
    ).

-spec partition_expired(integer(), pending_voice_connections()) ->
    {[{binary(), map()}], pending_voice_connections()}.
partition_expired(Now, PendingConnections) ->
    maps:fold(
        fun(ConnId, Metadata, {ExpAcc, RemAcc}) ->
            partition_pending_connection(ConnId, Metadata, Now, ExpAcc, RemAcc)
        end,
        {[], #{}},
        PendingConnections
    ).

-spec partition_pending_connection(
    binary(), map(), integer(), [{binary(), map()}], pending_voice_connections()
) -> {[{binary(), map()}], pending_voice_connections()}.
partition_pending_connection(ConnId, Metadata, Now, ExpAcc, RemAcc) ->
    ExpiresAt = maps:get(expires_at, Metadata, Now + 999999),
    case Now >= ExpiresAt of
        true -> {[{ConnId, Metadata} | ExpAcc], RemAcc};
        false -> {ExpAcc, RemAcc#{ConnId => Metadata}}
    end.

-spec spawn_force_disconnects([{binary(), map()}]) -> ok.
spawn_force_disconnects(Expired) ->
    lists:foreach(fun spawn_single_disconnect/1, Expired).

-spec spawn_single_disconnect({binary(), map()}) -> ok.
spawn_single_disconnect({ConnId, Metadata}) ->
    UserId = pending_get_snowflake(Metadata, user_id),
    GuildId = pending_get_snowflake(Metadata, guild_id),
    ChannelId = pending_get_snowflake(Metadata, channel_id),
    case {GuildId, ChannelId, UserId} of
        {GId, CId, UId} when
            is_integer(GId), GId > 0, is_integer(CId), CId > 0, is_integer(UId), UId > 0
        ->
            spawn_pending_disconnect(GId, CId, UId, ConnId),
            ok;
        _ ->
            ok
    end.

-spec spawn_pending_disconnect(integer(), integer(), integer(), binary()) -> pid().
spawn_pending_disconnect(GId, CId, UId, ConnId) ->
    spawn(fun() ->
        force_disconnect_with_retry(GId, CId, UId, ConnId, 3)
    end).

-spec force_disconnect_with_retry(
    integer(), integer(), integer(), binary(), non_neg_integer()
) -> ok.
force_disconnect_with_retry(GId, CId, UId, ConnId, 0) ->
    logger:warning(
        "voice_pending_force_disconnect_exhausted: guild_id=~p channel_id=~p "
        "user_id=~p connection_id=~p",
        [GId, CId, UId, ConnId]
    ),
    ok;
force_disconnect_with_retry(GId, CId, UId, ConnId, Retries) ->
    Result = shard_utils:safe_apply(
        fun() -> guild_voice_disconnect:force_disconnect_participant(GId, CId, UId, ConnId) end,
        {error, crashed}
    ),
    case Result of
        {ok, _Data} ->
            ok;
        {error, Reason} ->
            logger:warning(
                "voice_pending_force_disconnect_failed: guild_id=~p channel_id=~p "
                "user_id=~p connection_id=~p reason=~p retries_left=~p",
                [GId, CId, UId, ConnId, Reason, Retries]
            ),
            ok = gateway_retry_timer:wait(500),
            force_disconnect_with_retry(GId, CId, UId, ConnId, Retries - 1)
    end.

-spec clear_expired_virtual_flags([{binary(), map()}], guild_state()) -> guild_state().
clear_expired_virtual_flags(Expired, State) ->
    lists:foldl(fun clear_single_virtual_flags/2, State, Expired).

-spec clear_single_virtual_flags({binary(), map()}, guild_state()) -> guild_state().
clear_single_virtual_flags({_ConnId, Metadata}, AccState) ->
    ExpUserId = pending_get_snowflake(Metadata, user_id),
    ExpChannelId = pending_get_snowflake(Metadata, channel_id),
    case
        is_integer(ExpUserId) andalso ExpUserId > 0 andalso is_integer(ExpChannelId) andalso
            ExpChannelId > 0
    of
        true ->
            guild_virtual_channel_access:clear_transition_flags(
                ExpUserId, ExpChannelId, AccState
            );
        false ->
            AccState
    end.

-spec clear_expired_e2ee_keys([{binary(), map()}], guild_state(), pending_voice_connections()) ->
    guild_state().
clear_expired_e2ee_keys(Expired, State, Remaining) ->
    VoiceStates = voice_state_utils:voice_states(State),
    lists:foldl(
        fun({_ConnId, Metadata}, AccState) ->
            clear_expired_e2ee_key(Metadata, VoiceStates, Remaining, AccState)
        end,
        State,
        Expired
    ).

-spec clear_expired_e2ee_key(
    map(), voice_state_map(), pending_voice_connections(), guild_state()
) -> guild_state().
clear_expired_e2ee_key(Metadata, VoiceStates, Remaining, AccState) ->
    case pending_get_snowflake(Metadata, channel_id) of
        ChannelId when is_integer(ChannelId), ChannelId > 0 ->
            guild_voice_e2ee:forget_room_key_if_channel_idle_guild(
                ChannelId, VoiceStates, Remaining, AccState
            );
        _ ->
            AccState
    end.

-spec pending_get_value(map(), atom()) -> term().
pending_get_value(PendingData, Key) ->
    case maps:find(Key, PendingData) of
        error ->
            BinKey = atom_to_binary(Key, utf8),
            maps:get(BinKey, PendingData, undefined);
        {ok, Value} ->
            Value
    end.

-spec pending_get_snowflake(map(), atom()) -> integer() | undefined.
pending_get_snowflake(PendingData, Key) ->
    guild_voice_connection_normalize:normalize_positive_snowflake(
        pending_get_value(PendingData, Key)
    ).

-spec pending_get_integer(map(), atom()) -> integer() | undefined.
pending_get_integer(PendingData, Key) ->
    case pending_get_value(PendingData, Key) of
        undefined -> undefined;
        Value -> pending_value_to_integer(Value)
    end.

-spec pending_get_boolean(map(), atom()) -> boolean().
pending_get_boolean(PendingData, Key) ->
    case pending_get_value(PendingData, Key) of
        true -> true;
        _ -> false
    end.

-spec pending_get_binary(map(), atom()) -> binary() | undefined.
pending_get_binary(PendingData, Key) ->
    case pending_get_value(PendingData, Key) of
        undefined -> undefined;
        Value -> pending_value_to_binary(Value)
    end.

-spec pending_get_map(map(), atom()) -> map().
pending_get_map(PendingData, Key) ->
    case pending_get_value(PendingData, Key) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    end.

-spec pending_get_list(map(), atom()) -> list().
pending_get_list(PendingData, Key) ->
    case pending_get_value(PendingData, Key) of
        List when is_list(List) -> List;
        _ -> []
    end.

-spec pending_value_to_integer(term()) -> integer() | undefined.
pending_value_to_integer(Value) when
    is_integer(Value); is_binary(Value); is_list(Value); is_atom(Value)
->
    type_conv:to_integer(Value);
pending_value_to_integer(_) ->
    undefined.

-spec pending_value_to_binary(term()) -> binary() | undefined.
pending_value_to_binary(Value) when
    is_binary(Value); is_integer(Value); is_list(Value); is_atom(Value)
->
    type_conv:to_binary(Value);
pending_value_to_binary(_) ->
    undefined.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

assemble_pending_voice_state_preserves_voice_routing_test() ->
    PendingData = #{
        guild_id => 20,
        channel_id => 30,
        user_id => 10,
        session_id => <<"sess1">>,
        region_id => <<"us-east">>,
        server_id => <<"voice-ewr-1">>
    },
    VoiceState = assemble_pending_voice_state(PendingData, <<"conn1">>, 20, 30, 10),
    ?assertEqual(<<"us-east">>, maps:get(<<"region_id">>, VoiceState)),
    ?assertEqual(<<"voice-ewr-1">>, maps:get(<<"server_id">>, VoiceState)).

-endif.
