%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_disconnect_channel).
-typing([eqwalizer]).

-export([
    disconnect_all_voice_users_in_channel/2,
    disconnect_user_from_expected_channel/4,
    disconnect_connection_from_expected_channel/5
]).

-export_type([
    guild_state/0,
    voice_state/0,
    voice_state_map/0
]).

-type guild_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.

-spec disconnect_all_voice_users_in_channel(map(), guild_state()) ->
    {reply, map(), guild_state()}.
disconnect_all_voice_users_in_channel(#{channel_id := ChannelId}, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    ChannelVoiceStates = voice_state_utils:filter_voice_states(VoiceStates, fun(_, V) ->
        voice_state_utils:voice_state_channel_id(V) =:= ChannelId
    end),
    State1 = guild_voice_disconnect_broadcast:clear_pending_voice_connections_for_channel(
        ChannelId, State
    ),
    case maps:size(ChannelVoiceStates) of
        0 ->
            State2 = guild_voice_disconnect_broadcast:clear_e2ee_room_key_if_channel_idle(
                ChannelId, voice_state_utils:voice_states(State1), State1
            ),
            {reply, #{success => true, disconnected_count => 0}, State2};
        Count ->
            do_disconnect_channel(ChannelId, ChannelVoiceStates, VoiceStates, Count, State1)
    end.

-spec do_disconnect_channel(
    integer(), voice_state_map(), voice_state_map(), non_neg_integer(), guild_state()
) -> {reply, map(), guild_state()}.
do_disconnect_channel(ChannelId, ChannelVoiceStates, VoiceStates, Count, State) ->
    maybe_force_disconnect_voice_states(ChannelVoiceStates, State),
    ok = guild_voice_disconnect_broadcast:purge_count_cache(maps:keys(ChannelVoiceStates)),
    NewVoiceStates = voice_state_utils:drop_voice_states(ChannelVoiceStates, VoiceStates),
    NewState0 = State#{voice_states => NewVoiceStates},
    NewState1 = guild_voice_disconnect_broadcast:clear_recently_disconnected_for_channel(
        ChannelId, NewState0
    ),
    NewState = guild_voice_disconnect_broadcast:clear_e2ee_room_key_if_channel_idle(
        ChannelId, NewVoiceStates, NewState1
    ),
    voice_state_utils:broadcast_disconnects(ChannelVoiceStates, NewState),
    {reply, #{success => true, disconnected_count => Count}, NewState}.

-spec disconnect_user_from_expected_channel(
    integer(), integer(), voice_state_map(), guild_state()
) -> {reply, map(), guild_state()}.
disconnect_user_from_expected_channel(UserId, ExpectedChannelId, VoiceStates, State) ->
    UserVoiceStates = voice_state_utils:filter_voice_states(VoiceStates, fun(_, V) ->
        voice_state_utils:voice_state_user_id(V) =:= UserId andalso
            voice_state_utils:voice_state_channel_id(V) =:= ExpectedChannelId
    end),
    case maps:size(UserVoiceStates) of
        0 ->
            State0 =
                guild_voice_disconnect_broadcast:clear_pending_voice_connections_for_user_channel(
                    UserId, ExpectedChannelId, State
                ),
            State1 = guild_voice_disconnect_broadcast:clear_e2ee_room_key_if_channel_idle(
                ExpectedChannelId, VoiceStates, State0
            ),
            {reply,
                #{success => true, ignored => true, reason => <<"not_in_expected_channel">>},
                State1};
        _ ->
            ok = guild_voice_disconnect_broadcast:purge_count_cache(maps:keys(UserVoiceStates)),
            NewVoiceStates = voice_state_utils:drop_voice_states(UserVoiceStates, VoiceStates),
            NewState0 = State#{voice_states => NewVoiceStates},
            NewState1 = guild_voice_disconnect_broadcast:clear_e2ee_room_keys_for_removed(
                UserVoiceStates, NewVoiceStates, NewState0
            ),
            NewState = guild_voice_disconnect_broadcast:cache_recently_disconnected(
                UserVoiceStates, NewState1
            ),
            voice_state_utils:broadcast_disconnects(UserVoiceStates, NewState),
            {reply, #{success => true}, NewState}
    end.

-spec disconnect_connection_from_expected_channel(
    integer(), integer(), binary(), voice_state_map(), guild_state()
) -> {reply, map(), guild_state()}.
disconnect_connection_from_expected_channel(
    UserId, ExpectedChannelId, ConnId, VoiceStates, State
) ->
    case maps:get(ConnId, VoiceStates, undefined) of
        undefined ->
            State1 = clear_pending_in_channel(ConnId, ExpectedChannelId, State),
            {reply, #{success => true, ignored => true, reason => <<"connection_not_found">>},
                State1};
        VoiceState ->
            check_and_disconnect_connection(
                UserId, ExpectedChannelId, ConnId, VoiceState, VoiceStates, State
            )
    end.

-spec clear_pending_in_channel(binary(), integer(), guild_state()) -> guild_state().
clear_pending_in_channel(ConnId, ChannelId, State) ->
    Pending = maps:get(pending_voice_connections, State, #{}),
    case maps:get(ConnId, Pending, undefined) of
        #{channel_id := ChannelId} ->
            guild_voice_disconnect_broadcast:clear_pending_voice_connection(ConnId, State);
        _ ->
            State
    end.

-spec check_and_disconnect_connection(
    integer(), integer(), binary(), voice_state(), voice_state_map(), guild_state()
) -> {reply, map(), guild_state()}.
check_and_disconnect_connection(
    UserId, ExpectedChannelId, ConnId, VoiceState, VoiceStates, State
) ->
    case
        {
            voice_state_utils:voice_state_user_id(VoiceState),
            voice_state_utils:voice_state_channel_id(VoiceState)
        }
    of
        {UserId, ExpectedChannelId} ->
            ok = guild_voice_disconnect_broadcast:purge_count_cache([ConnId]),
            NewVoiceStates = maps:remove(ConnId, VoiceStates),
            NewState0 = State#{voice_states => NewVoiceStates},
            NewState1 = guild_voice_disconnect_broadcast:clear_e2ee_room_key_if_channel_idle(
                ExpectedChannelId, NewVoiceStates, NewState0
            ),
            NewState = guild_voice_disconnect_broadcast:cache_recently_disconnected(
                #{ConnId => VoiceState}, NewState1
            ),
            voice_state_utils:broadcast_disconnects(#{ConnId => VoiceState}, NewState),
            {reply, #{success => true}, NewState};
        _ ->
            MismatchReply = #{
                success => true,
                ignored => true,
                reason => <<"user_or_channel_mismatch">>
            },
            {reply, MismatchReply, State}
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
    GuildId =
        case voice_state_utils:voice_state_guild_id(VoiceState) of
            undefined -> map_utils:get_integer(State, id, undefined);
            GId -> GId
        end,
    case {GuildId, ChannelId, UserId} of
        {GId2, CId, UId} when
            is_integer(GId2), GId2 > 0, is_integer(CId), CId > 0, is_integer(UId), UId > 0
        ->
            _ = maybe_force_disconnect(GId2, CId, UId, ConnectionId, State),
            ok;
        _ ->
            ok
    end.

-spec maybe_force_disconnect(integer(), integer(), integer(), binary(), guild_state()) ->
    {ok, map()} | {error, term()}.
maybe_force_disconnect(GuildId, ChannelId, UserId, ConnectionId, State) ->
    case maps:get(test_force_disconnect_fun, State, undefined) of
        Fun when is_function(Fun, 4) -> Fun(GuildId, ChannelId, UserId, ConnectionId);
        _ ->
            guild_voice_disconnect_user:force_disconnect_participant(
                GuildId, ChannelId, UserId, ConnectionId
            )
    end.
