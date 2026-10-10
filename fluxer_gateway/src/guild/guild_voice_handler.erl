%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_handler).
-typing([eqwalizer]).

-export([
    handle_call/3,
    handle_cast/2
]).

-type guild_state() :: map().

-export_type([guild_state/0]).

-spec handle_call(term(), gen_server:from(), guild_state()) ->
    {reply, term(), guild_state()}
    | {noreply, guild_state()}.
handle_call({Op, Request}, From, State) when is_atom(Op), is_map(Request) ->
    handle_voice_call(Op, Request, From, State);
handle_call({add_virtual_channel_access, UserId, ChannelId}, _From, State) when
    is_integer(UserId), is_integer(ChannelId)
->
    handle_add_virtual_access(UserId, ChannelId, State);
handle_call({store_pending_connection, ConnId, Meta}, _From, State) ->
    handle_store_pending(ConnId, Meta, State);
handle_call({get_voice_states_for_channel, ChIdBin}, _From, State) when
    is_binary(ChIdBin)
->
    VS = filter_voice_states_for_channel(ChIdBin, State),
    {reply, #{voice_states => VS}, State};
handle_call({get_pending_joins_for_channel, ChIdBin}, _From, State) when
    is_binary(ChIdBin)
->
    PJ = filter_pending_joins(ChIdBin, State),
    {reply, #{pending_joins => PJ}, State}.

-spec handle_voice_call(atom(), map(), gen_server:from(), guild_state()) ->
    {reply, term(), guild_state()} | {noreply, guild_state()}.
handle_voice_call(voice_state_update, Req, _From, State) ->
    guild_voice:voice_state_update(Req, State);
handle_voice_call(get_voice_state, Req, _From, State) ->
    guild_voice:get_voice_state(Req, State);
handle_voice_call(update_member_voice, Req, _From, State) ->
    guild_voice:update_member_voice(Req, State);
handle_voice_call(disconnect_voice_user, Req, _From, State) ->
    guild_voice:disconnect_voice_user(Req, State);
handle_voice_call(disconnect_voice_user_if_in_channel, Req, _From, State) ->
    guild_voice:disconnect_voice_user_if_in_channel(Req, State);
handle_voice_call(disconnect_all_voice_users_in_channel, Req, _From, State) ->
    guild_voice:disconnect_all_voice_users_in_channel(Req, State);
handle_voice_call(confirm_voice_connection_from_livekit, Req, _From, State) ->
    guild_voice:confirm_voice_connection_from_livekit(Req, State);
handle_voice_call(move_member, Req, _From, State) ->
    guild_voice:move_member(Req, State);
handle_voice_call(switch_voice_region, Req, _From, State) ->
    guild_voice:switch_voice_region_handler(Req, State).

-spec handle_add_virtual_access(integer(), integer(), guild_state()) ->
    {reply, ok, guild_state()}.
handle_add_virtual_access(UserId, ChannelId, State) ->
    NewState = guild_virtual_channel_access:add_virtual_access(UserId, ChannelId, State),
    guild_virtual_channel_access:dispatch_channel_visibility_change(
        UserId, ChannelId, add, NewState
    ),
    {reply, ok, NewState}.

-spec handle_store_pending(term(), term(), guild_state()) ->
    {reply, ok, guild_state()}.
handle_store_pending(ConnectionId, Metadata, State) ->
    Pending = maps:get(pending_voice_connections, State, #{}),
    NewState = State#{pending_voice_connections => Pending#{ConnectionId => Metadata}},
    {reply, ok, NewState}.

-spec filter_voice_states_for_channel(binary(), guild_state()) -> [map()].
filter_voice_states_for_channel(ChannelIdBin, State) ->
    VoiceStates = maps:get(voice_states, State, #{}),
    maps:fold(
        fun(ConnId, VS, Acc) -> append_voice_state(ChannelIdBin, ConnId, VS, Acc) end,
        [],
        VoiceStates
    ).

-spec append_voice_state(binary(), term(), term(), [map()]) -> [map()].
append_voice_state(ChannelIdBin, ConnId, VS, Acc) when is_map(VS) ->
    VsChId = maps:get(<<"channel_id">>, VS, null),
    append_matching_voice_state(VsChId, ChannelIdBin, ConnId, VS, Acc);
append_voice_state(_ChannelIdBin, _ConnId, _VS, Acc) ->
    Acc.

-spec append_matching_voice_state(term(), binary(), term(), map(), [map()]) -> [map()].
append_matching_voice_state(ChannelIdBin, ChannelIdBin, ConnId, VS, Acc) ->
    UserId = maps:get(<<"user_id">>, VS, null),
    Entry = #{connection_id => ConnId, user_id => UserId, channel_id => ChannelIdBin},
    [Entry | Acc];
append_matching_voice_state(_OtherChannelId, _ChannelIdBin, _ConnId, _VS, Acc) ->
    Acc.

-spec filter_pending_joins(binary(), guild_state()) -> [map()].
filter_pending_joins(ChannelIdBin, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    case snowflake_id:parse_optional(ChannelIdBin) of
        undefined ->
            [];
        ChannelIdInt when is_integer(ChannelIdInt) ->
            collect_pending_joins(ChannelIdInt, PendingConnections)
    end.

-spec collect_pending_joins(integer(), map()) -> [map()].
collect_pending_joins(ChannelIdInt, PendingConnections) ->
    maps:fold(
        fun(ConnId, Meta, Acc) ->
            append_pending_join(ChannelIdInt, ConnId, Meta, Acc)
        end,
        [],
        PendingConnections
    ).

-spec append_pending_join(integer(), term(), term(), [map()]) -> [map()].
append_pending_join(ChannelIdInt, ConnId, Metadata, Acc) when is_map(Metadata) ->
    MetaChId = metadata_integer(Metadata, channel_id),
    MetaUserId = metadata_integer(Metadata, user_id),
    append_matching_pending_join(
        MetaChId, ChannelIdInt, MetaUserId, ConnId, Metadata, Acc
    );
append_pending_join(_ChannelIdInt, _ConnId, _Metadata, Acc) ->
    Acc.

-spec append_matching_pending_join(
    integer() | undefined,
    integer(),
    integer() | undefined,
    term(),
    map(),
    [map()]
) -> [map()].
append_matching_pending_join(ChannelIdInt, ChannelIdInt, UserId, ConnId, Metadata, Acc) when
    ChannelIdInt > 0, is_integer(UserId), UserId > 0
->
    [
        #{
            connection_id => ConnId,
            user_id => integer_to_binary(UserId),
            token_nonce => maps:get(token_nonce, Metadata, null),
            expires_at => maps:get(expires_at, Metadata, 0)
        }
        | Acc
    ];
append_matching_pending_join(
    _MetadataChannelId, _ChannelIdInt, _UserId, _ConnId, _Metadata, Acc
) ->
    Acc.

-spec metadata_integer(map(), atom()) -> integer() | undefined.
metadata_integer(Metadata, Key) ->
    metadata_integer_or_fallback(map_integer(Metadata, Key), Metadata, Key).

-spec metadata_integer_or_fallback(integer() | undefined, map(), atom()) ->
    integer() | undefined.
metadata_integer_or_fallback(undefined, Metadata, Key) ->
    map_integer(Metadata, atom_to_binary(Key, utf8));
metadata_integer_or_fallback(Value, _Metadata, _Key) ->
    Value.

-spec map_integer(map(), atom() | binary()) -> integer() | undefined.
map_integer(Metadata, Key) ->
    case map_utils:get_integer(Metadata, Key, undefined) of
        Value when is_integer(Value) -> Value;
        _ -> undefined
    end.

-spec handle_cast(term(), guild_state()) -> {noreply, guild_state()}.
handle_cast({relay_voice_state_update, VS, OldChId}, State) when
    is_map(VS), is_binary(OldChId)
->
    {noreply, relay_upsert_voice_state(VS, State)};
handle_cast({relay_voice_state_update, VS, null}, State) when is_map(VS) ->
    {noreply, relay_upsert_voice_state(VS, State)};
handle_cast({relay_voice_server_update, GId, ChId, SId, Tok, Ep, CId}, State) when
    is_integer(GId),
    is_integer(ChId),
    is_binary(SId),
    is_binary(Tok),
    is_binary(Ep),
    is_binary(CId)
->
    cast_relay_voice_server(GId, ChId, SId, Tok, Ep, CId, State);
handle_cast({store_pending_connection, ConnId, Meta}, State) ->
    {noreply, cast_store_pending(ConnId, Meta, State)};
handle_cast({add_virtual_channel_access, UId, ChId}, State) when
    is_integer(UId), is_integer(ChId)
->
    {noreply, cast_add_virtual_access(UId, ChId, State)};
handle_cast({remove_virtual_channel_access, UId, ChId}, State) when
    is_integer(UId), is_integer(ChId)
->
    {noreply, cast_remove_virtual_access(UId, ChId, State)};
handle_cast({cleanup_virtual_access_for_user, UId}, State) when
    is_integer(UId)
->
    NewState = guild_voice_disconnect:cleanup_virtual_channel_access_for_user(UId, State),
    {noreply, NewState};
handle_cast({voice_signal, Request}, State) when is_map(Request) ->
    ok = relay_voice_signal(Request, State),
    {noreply, State}.

-spec relay_voice_signal(map(), guild_state()) -> ok.
relay_voice_signal(
    #{session_id := SessionId, channel_id := ChannelId, to := To, data := Data}, State
) ->
    ChannelVoiceStates = maps:values(
        voice_state_utils:channel_voice_states(ChannelId, voice_state_utils:voice_states(State))
    ),
    case voice_p2p:signal_route(ChannelVoiceStates, SessionId, To) of
        {ok, Sender, Target} ->
            GuildId = maps:get(id, State),
            dispatch_voice_signal(
                maps:get(<<"session_id">>, Target, undefined),
                voice_p2p:signal_payload(GuildId, ChannelId, Sender, Data),
                GuildId,
                State
            );
        error ->
            ok
    end.

-spec dispatch_voice_signal(term(), map(), integer(), guild_state()) -> ok.
dispatch_voice_signal(SessionId, Payload, GuildId, State) ->
    case maps:get(SessionId, maps:get(sessions, State, #{}), undefined) of
        #{pid := Pid} when is_pid(Pid) ->
            gateway_dispatch_relay:dispatch(Pid, voice_signal, Payload, GuildId);
        _ ->
            ok
    end.

-spec cast_relay_voice_server(
    integer(), integer(), binary(), binary(), binary(), binary(), guild_state()
) -> {noreply, guild_state()}.
cast_relay_voice_server(GId, ChId, SId, Tok, Ep, CId, State) ->
    _ = guild_voice_broadcast:broadcast_voice_server_update_to_session(
        GId, ChId, SId, Tok, Ep, CId, State
    ),
    {noreply, State}.

-spec cast_store_pending(term(), term(), guild_state()) -> guild_state().
cast_store_pending(ConnId, Meta, State) ->
    Pending = maps:get(pending_voice_connections, State, #{}),
    State#{pending_voice_connections => Pending#{ConnId => Meta}}.

-spec cast_add_virtual_access(integer(), integer(), guild_state()) -> guild_state().
cast_add_virtual_access(UId, ChId, State) ->
    NewState = guild_virtual_channel_access:add_virtual_access(UId, ChId, State),
    guild_virtual_channel_access:dispatch_channel_visibility_change(UId, ChId, add, NewState),
    NewState.

-spec cast_remove_virtual_access(integer(), integer(), guild_state()) -> guild_state().
cast_remove_virtual_access(UId, ChId, State) ->
    guild_virtual_channel_access:dispatch_channel_visibility_change(UId, ChId, remove, State),
    guild_virtual_channel_access:remove_virtual_access(UId, ChId, State).

-spec relay_upsert_voice_state(map(), guild_state()) -> guild_state().
relay_upsert_voice_state(VoiceState, State) ->
    ConnectionId = maps:get(<<"connection_id">>, VoiceState, undefined),
    case ConnectionId of
        undefined ->
            State;
        _ ->
            do_upsert_voice_state(ConnectionId, VoiceState, State)
    end.

-spec do_upsert_voice_state(term(), map(), guild_state()) -> guild_state().
do_upsert_voice_state(ConnectionId, VoiceState, State) ->
    VoiceStates0 = maps:get(voice_states, State, #{}),
    ChannelId = maps:get(<<"channel_id">>, VoiceState, null),
    VoiceStates =
        case ChannelId of
            null -> maps:remove(ConnectionId, VoiceStates0);
            _ -> VoiceStates0#{ConnectionId => VoiceState}
        end,
    State#{voice_states => VoiceStates}.
