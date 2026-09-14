%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_disconnect_user).
-typing([eqwalizer]).

-export([
    handle_voice_disconnect/5,
    disconnect_voice_user/2,
    disconnect_voice_user_if_in_channel/2,
    force_disconnect_participant/4,
    cleanup_virtual_channel_access_for_user/2
]).

-export_type([
    guild_state/0,
    voice_state/0,
    voice_state_map/0,
    voice_reply/0
]).

-type guild_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.
-type voice_reply() :: {reply, map() | {error, atom(), atom()}, guild_state()}.

-spec handle_voice_disconnect(
    binary() | undefined, term(), integer(), voice_state_map() | term(), guild_state()
) -> voice_reply().
handle_voice_disconnect(undefined, _SessionId, _UserId, _VoiceStates, State) ->
    {reply, gateway_errors:error(voice_missing_connection_id), State};
handle_voice_disconnect(ConnectionId, _SessionId, UserId, VoiceStates0, State) ->
    VoiceStates = voice_state_utils:ensure_voice_states(VoiceStates0),
    case maps:get(ConnectionId, VoiceStates, undefined) of
        undefined ->
            clear_missing_connection_success(ConnectionId, State);
        OldVoiceState ->
            handle_existing_voice_state(ConnectionId, UserId, OldVoiceState, VoiceStates, State)
    end.

-spec clear_missing_connection_success(binary(), guild_state()) -> voice_reply().
clear_missing_connection_success(ConnectionId, State) ->
    State1 = guild_voice_disconnect_broadcast:clear_pending_voice_connection(
        ConnectionId, State
    ),
    {reply, #{success => true}, State1}.

-spec handle_existing_voice_state(
    binary(), integer(), voice_state(), voice_state_map(), guild_state()
) -> voice_reply().
handle_existing_voice_state(ConnectionId, UserId, OldVoiceState, VoiceStates, State) ->
    case guild_voice_state:user_matches_voice_state(OldVoiceState, UserId) of
        false ->
            {reply, gateway_errors:error(voice_user_mismatch), State};
        true ->
            GuildId = voice_state_utils:voice_state_guild_id(OldVoiceState),
            ChannelId = voice_state_utils:voice_state_channel_id(OldVoiceState),
            handle_valid_voice_state_ids(
                GuildId, ChannelId, UserId, ConnectionId, OldVoiceState, VoiceStates, State
            )
    end.

-spec handle_valid_voice_state_ids(
    integer() | undefined,
    integer() | undefined,
    integer(),
    binary(),
    voice_state(),
    voice_state_map(),
    guild_state()
) -> voice_reply().
handle_valid_voice_state_ids(undefined, _ChannelId, _UserId, _ConnId, _OldVS, _VS, State) ->
    {reply, gateway_errors:error(voice_invalid_state), State};
handle_valid_voice_state_ids(_GuildId, undefined, _UserId, _ConnId, _OldVS, _VS, State) ->
    {reply, gateway_errors:error(voice_invalid_state), State};
handle_valid_voice_state_ids(GId, CId, UserId, ConnectionId, OldVoiceState, VoiceStates, State) ->
    do_voice_disconnect(GId, CId, UserId, ConnectionId, OldVoiceState, VoiceStates, State).

-spec do_voice_disconnect(
    integer(), integer(), integer(), binary(), voice_state(), voice_state_map(), guild_state()
) -> voice_reply().
do_voice_disconnect(
    GuildId, ChannelId, UserId, ConnectionId, OldVoiceState, VoiceStates, State
) ->
    _ = maybe_force_disconnect(GuildId, ChannelId, UserId, ConnectionId, State),
    ok = guild_voice_disconnect_broadcast:purge_count_cache([ConnectionId]),
    NewVoiceStates = maps:remove(ConnectionId, VoiceStates),
    NewState0 = State#{voice_states => NewVoiceStates},
    NewState1 = guild_voice_disconnect_broadcast:clear_recently_disconnected(
        ConnectionId, NewState0
    ),
    NewState = guild_voice_disconnect_broadcast:clear_e2ee_room_key_if_channel_idle(
        ChannelId, NewVoiceStates, NewState1
    ),
    voice_state_utils:broadcast_disconnects(#{ConnectionId => OldVoiceState}, NewState),
    FinalState = maybe_cleanup_after_disconnect(UserId, ChannelId, NewState),
    {reply, #{success => true}, FinalState}.

-spec disconnect_voice_user(map(), guild_state()) -> voice_reply().
disconnect_voice_user(#{user_id := UserId} = Request, State) ->
    ConnectionId = maps:get(connection_id, Request, null),
    RequestSessionId = normalize_session_id(maps:get(session_id, Request, undefined)),
    VoiceStates = voice_state_utils:voice_states(State),
    case ConnectionId of
        null ->
            disconnect_all_user_connections(UserId, RequestSessionId, VoiceStates, State);
        SpecificConnection ->
            disconnect_specific_connection(UserId, SpecificConnection, VoiceStates, State)
    end.

-spec disconnect_all_user_connections(
    integer(), binary() | undefined, voice_state_map(), guild_state()
) -> voice_reply().
disconnect_all_user_connections(UserId, RequestSessionId, VoiceStates, State) ->
    UserVoiceStates = matching_user_voice_states(UserId, RequestSessionId, VoiceStates),
    case maps:size(UserVoiceStates) of
        0 ->
            State1 = guild_voice_disconnect_broadcast:clear_pending_voice_connections_for_user(
                UserId, RequestSessionId, State
            ),
            {reply, #{success => true}, State1};
        _ ->
            maybe_force_disconnect_voice_states(UserVoiceStates, State),
            ok = guild_voice_disconnect_broadcast:purge_count_cache(maps:keys(UserVoiceStates)),
            NewVoiceStates = voice_state_utils:drop_voice_states(UserVoiceStates, VoiceStates),
            NewState0 = State#{voice_states => NewVoiceStates},
            NewState1 = guild_voice_disconnect_broadcast:clear_e2ee_room_keys_for_removed(
                UserVoiceStates, NewVoiceStates, NewState0
            ),
            NewState = clear_recently_disconnected_connections(UserVoiceStates, NewState1),
            voice_state_utils:broadcast_disconnects(UserVoiceStates, NewState),
            FinalState = maybe_cleanup_virtual_channel_access(UserId, NewVoiceStates, NewState),
            {reply, #{success => true}, FinalState}
    end.

-spec clear_recently_disconnected_connections(voice_state_map(), guild_state()) ->
    guild_state().
clear_recently_disconnected_connections(UserVoiceStates, State) ->
    maps:fold(
        fun(ConnId, _, AccState) ->
            guild_voice_disconnect_broadcast:clear_recently_disconnected(ConnId, AccState)
        end,
        State,
        UserVoiceStates
    ).

-spec disconnect_specific_connection(
    integer(), binary(), voice_state_map(), guild_state()
) -> voice_reply().
disconnect_specific_connection(UserId, ConnId, VoiceStates, State) ->
    case maps:get(ConnId, VoiceStates, undefined) of
        undefined ->
            clear_missing_connection_success(ConnId, State);
        VoiceState ->
            handle_specific_disconnect(UserId, ConnId, VoiceState, VoiceStates, State)
    end.

-spec handle_specific_disconnect(
    integer(), binary(), voice_state(), voice_state_map(), guild_state()
) -> voice_reply().
handle_specific_disconnect(UserId, ConnId, VoiceState, VoiceStates, State) ->
    case voice_state_utils:voice_state_user_id(VoiceState) of
        undefined ->
            {reply, gateway_errors:error(voice_invalid_state), State};
        VoiceStateUserId when VoiceStateUserId =:= UserId ->
            maybe_force_disconnect_voice_state(ConnId, VoiceState, State),
            ok = guild_voice_disconnect_broadcast:purge_count_cache([ConnId]),
            NewVoiceStates = maps:remove(ConnId, VoiceStates),
            NewState0 = State#{voice_states => NewVoiceStates},
            NewState1 = guild_voice_disconnect_broadcast:clear_recently_disconnected(
                ConnId, NewState0
            ),
            NewState = guild_voice_disconnect_broadcast:clear_e2ee_room_key_if_channel_idle(
                voice_state_utils:voice_state_channel_id(VoiceState), NewVoiceStates, NewState1
            ),
            voice_state_utils:broadcast_disconnects(#{ConnId => VoiceState}, NewState),
            FinalState = maybe_cleanup_virtual_channel_access(UserId, NewVoiceStates, NewState),
            {reply, #{success => true}, FinalState};
        _ ->
            {reply, gateway_errors:error(voice_user_mismatch), State}
    end.

-spec disconnect_voice_user_if_in_channel(map(), guild_state()) -> voice_reply().
disconnect_voice_user_if_in_channel(
    #{user_id := UserId, expected_channel_id := ExpectedChannelId} = Request, State
) ->
    ConnectionId = maps:get(connection_id, Request, undefined),
    VoiceStates = voice_state_utils:voice_states(State),
    case ConnectionId of
        undefined ->
            guild_voice_disconnect_channel:disconnect_user_from_expected_channel(
                UserId, ExpectedChannelId, VoiceStates, State
            );
        ConnId ->
            guild_voice_disconnect_channel:disconnect_connection_from_expected_channel(
                UserId, ExpectedChannelId, ConnId, VoiceStates, State
            )
    end.

-spec force_disconnect_participant(integer(), integer(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
force_disconnect_participant(GuildId, ChannelId, UserId, ConnectionId) ->
    Req = voice_utils:build_force_disconnect_rpc_request(
        GuildId, ChannelId, UserId, ConnectionId
    ),
    case rpc_client:call(Req) of
        {ok, _Data} -> {ok, #{success => true}};
        {error, Reason} -> {error, Reason}
    end.

-spec cleanup_virtual_channel_access_for_user(integer(), guild_state()) -> guild_state().
cleanup_virtual_channel_access_for_user(UserId, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    VirtualChannels = guild_virtual_channel_access:get_virtual_channels_for_user(UserId, State),
    lists:foldl(
        fun(ChannelId, AccState) ->
            cleanup_virtual_channel_for_user(UserId, ChannelId, VoiceStates, AccState)
        end,
        State,
        VirtualChannels
    ).

-spec cleanup_virtual_channel_for_user(
    integer(), integer(), voice_state_map(), guild_state()
) -> guild_state().
cleanup_virtual_channel_for_user(UserId, ChannelId, VoiceStates, AccState) ->
    case user_has_voice_connection_in_channel(UserId, ChannelId, VoiceStates) of
        true ->
            AccState;
        false ->
            cleanup_virtual_channel_without_connection(UserId, ChannelId, AccState)
    end.

-spec cleanup_virtual_channel_without_connection(
    integer(), integer(), guild_state()
) -> guild_state().
cleanup_virtual_channel_without_connection(UserId, ChannelId, AccState) ->
    case has_pending_or_preserve(UserId, ChannelId, AccState) of
        true ->
            AccState;
        false ->
            ok = maybe_dispatch_visibility_remove(UserId, ChannelId, AccState),
            guild_virtual_channel_access:remove_virtual_access(UserId, ChannelId, AccState)
    end.

-spec has_pending_or_preserve(integer(), integer(), guild_state()) -> boolean().
has_pending_or_preserve(UserId, ChannelId, State) ->
    guild_virtual_channel_access:is_pending_join(UserId, ChannelId, State) orelse
        guild_virtual_channel_access:has_preserve(UserId, ChannelId, State) orelse
        guild_virtual_channel_access:is_move_pending(UserId, ChannelId, State).

-spec matching_user_voice_states(integer(), binary() | undefined, voice_state_map()) ->
    voice_state_map().
matching_user_voice_states(UserId, RequestSessionId, VoiceStates) ->
    voice_state_utils:filter_voice_states(VoiceStates, fun(_, V) ->
        voice_state_utils:voice_state_user_id(V) =:= UserId andalso
            voice_state_session_matches(V, RequestSessionId)
    end).

-spec voice_state_session_matches(voice_state(), binary() | undefined) -> boolean().
voice_state_session_matches(_VoiceState, undefined) ->
    true;
voice_state_session_matches(VoiceState, RequestSessionId) ->
    normalize_session_id(maps:get(<<"session_id">>, VoiceState, undefined)) =:=
        RequestSessionId.

-spec normalize_session_id(term()) -> binary() | undefined.
normalize_session_id(Value) -> voice_state_utils:normalize_session_id(Value).

-spec maybe_cleanup_after_disconnect(integer(), integer(), guild_state()) -> guild_state().
maybe_cleanup_after_disconnect(UserId, ChannelId, State) ->
    case has_pending_or_preserve(UserId, ChannelId, State) of
        true -> State;
        false -> cleanup_virtual_channel_access_for_user(UserId, State)
    end.

-spec maybe_cleanup_virtual_channel_access(integer(), voice_state_map(), guild_state()) ->
    guild_state().
maybe_cleanup_virtual_channel_access(UserId, RemainingVoiceStates, State) ->
    case user_has_any_voice_state(UserId, RemainingVoiceStates) of
        true -> State;
        false -> cleanup_virtual_channel_access_for_user(UserId, State)
    end.

-spec user_has_any_voice_state(integer(), voice_state_map()) -> boolean().
user_has_any_voice_state(UserId, VoiceStates) ->
    maps:fold(
        fun(_, V, Acc) -> Acc orelse voice_state_utils:voice_state_user_id(V) =:= UserId end,
        false,
        VoiceStates
    ).

-spec user_has_voice_connection_in_channel(integer(), integer(), voice_state_map()) ->
    boolean().
user_has_voice_connection_in_channel(UserId, ChannelId, VoiceStates) ->
    ChannelIdBin = integer_to_binary(ChannelId),
    maps:fold(
        fun(_ConnId, VoiceState, Acc) ->
            Acc orelse voice_connection_matches(VoiceState, UserId, ChannelIdBin)
        end,
        false,
        VoiceStates
    ).

-spec voice_connection_matches(voice_state(), integer(), binary()) -> boolean().
voice_connection_matches(VoiceState, UserId, ChannelIdBin) ->
    voice_state_utils:voice_state_user_id(VoiceState) =:= UserId andalso
        maps:get(<<"channel_id">>, VoiceState, null) =:= ChannelIdBin.

-spec maybe_dispatch_visibility_remove(integer(), integer(), guild_state()) -> ok.
maybe_dispatch_visibility_remove(UserId, ChannelId, State) ->
    case guild_virtual_channel_access:has_virtual_access(UserId, ChannelId, State) of
        true ->
            guild_virtual_channel_access:dispatch_channel_visibility_change(
                UserId, ChannelId, remove, State
            );
        false ->
            ok
    end.

-spec maybe_force_disconnect(integer(), integer(), integer(), binary(), guild_state()) ->
    {ok, map()} | {error, term()}.
maybe_force_disconnect(GuildId, ChannelId, UserId, ConnectionId, State) ->
    case maps:get(test_force_disconnect_fun, State, undefined) of
        Fun when is_function(Fun, 4) -> Fun(GuildId, ChannelId, UserId, ConnectionId);
        _ -> force_disconnect_participant(GuildId, ChannelId, UserId, ConnectionId)
    end.

-spec maybe_force_disconnect_voice_states(voice_state_map(), guild_state()) -> ok.
maybe_force_disconnect_voice_states(VoiceStates, State) ->
    maps:foreach(
        fun(ConnId, VoiceState) ->
            maybe_force_disconnect_voice_state(ConnId, VoiceState, State)
        end,
        VoiceStates
    ),
    ok.

-spec maybe_force_disconnect_voice_state(binary(), voice_state(), guild_state()) -> ok.
maybe_force_disconnect_voice_state(ConnectionId, VoiceState, State) ->
    UserId = voice_state_utils:voice_state_user_id(VoiceState),
    ChannelId = voice_state_utils:voice_state_channel_id(VoiceState),
    GuildId = resolve_guild_id(VoiceState, State),
    case {GuildId, ChannelId, UserId} of
        {GId, CId, UId} when
            is_integer(GId), GId > 0, is_integer(CId), CId > 0, is_integer(UId), UId > 0
        ->
            _ = maybe_force_disconnect(GId, CId, UId, ConnectionId, State),
            ok;
        _ ->
            ok
    end.

-spec resolve_guild_id(voice_state(), guild_state()) -> integer() | undefined.
resolve_guild_id(VoiceState, State) ->
    case voice_state_utils:voice_state_guild_id(VoiceState) of
        undefined ->
            guild_voice_connection_normalize:normalize_positive_snowflake(
                maps:get(id, State, undefined)
            );
        GuildId ->
            GuildId
    end.
