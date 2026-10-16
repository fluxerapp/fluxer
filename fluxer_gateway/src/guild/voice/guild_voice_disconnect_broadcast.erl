%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_disconnect_broadcast).
-typing([eqwalizer]).

-export([
    recently_disconnected_voice_states/1,
    cache_recently_disconnected/2,
    clear_recently_disconnected/2,
    clear_recently_disconnected_for_channel/2,
    clear_pending_voice_connection/2,
    clear_pending_voice_connections_for_user/3,
    clear_pending_voice_connections_for_user_channel/3,
    clear_pending_voice_connections_for_channel/2,
    clear_e2ee_room_key_if_channel_idle/3,
    clear_e2ee_room_keys_for_removed/3,
    purge_count_cache/1
]).

-export_type([
    guild_state/0,
    voice_state_map/0
]).

-define(RECENTLY_DISCONNECTED_TTL_MS, 60000).

-type guild_state() :: map().
-type voice_state_map() :: #{binary() => map()}.

-spec recently_disconnected_voice_states(guild_state()) -> map().
recently_disconnected_voice_states(State) ->
    case maps:get(recently_disconnected_voice_states, State, undefined) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    end.

-spec cache_recently_disconnected(voice_state_map(), guild_state()) -> guild_state().
cache_recently_disconnected(VoiceStatesToCache, State) ->
    Now = erlang:system_time(millisecond),
    Existing = recently_disconnected_voice_states(State),
    Swept = sweep_expired_recently_disconnected(Existing, Now),
    NewEntries = maps:fold(
        fun(ConnId, VoiceState, Acc) ->
            Acc#{ConnId => #{voice_state => VoiceState, disconnected_at => Now}}
        end,
        Swept,
        VoiceStatesToCache
    ),
    State#{recently_disconnected_voice_states => NewEntries}.

-spec sweep_expired_recently_disconnected(map(), integer()) -> map().
sweep_expired_recently_disconnected(Cache, Now) ->
    maps:filter(
        fun
            (_ConnId, #{disconnected_at := DisconnectedAt}) ->
                (Now - DisconnectedAt) < ?RECENTLY_DISCONNECTED_TTL_MS;
            (_ConnId, _) ->
                false
        end,
        Cache
    ).

-spec clear_recently_disconnected(binary(), guild_state()) -> guild_state().
clear_recently_disconnected(ConnectionId, State) ->
    Cache = recently_disconnected_voice_states(State),
    State#{recently_disconnected_voice_states => maps:remove(ConnectionId, Cache)}.

-spec clear_recently_disconnected_for_channel(integer(), guild_state()) -> guild_state().
clear_recently_disconnected_for_channel(ChannelId, State) ->
    Cache = recently_disconnected_voice_states(State),
    NewCache = maps:filter(
        fun
            (_ConnId, #{voice_state := VS}) ->
                voice_state_utils:voice_state_channel_id(VS) =/= ChannelId;
            (_ConnId, _) ->
                false
        end,
        Cache
    ),
    State#{recently_disconnected_voice_states => NewCache}.

-spec clear_pending_voice_connection(binary(), guild_state()) -> guild_state().
clear_pending_voice_connection(ConnectionId, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    case maps:is_key(ConnectionId, PendingConnections) of
        false ->
            State;
        true ->
            PendingData = maps:get(ConnectionId, PendingConnections, #{}),
            NewPending = maps:remove(ConnectionId, PendingConnections),
            NewState = State#{pending_voice_connections => NewPending},
            clear_e2ee_room_keys_for_removed_pending(
                #{ConnectionId => PendingData},
                voice_state_utils:voice_states(State),
                NewPending,
                NewState
            )
    end.

-spec clear_pending_voice_connections_for_user(integer(), binary() | undefined, guild_state()) ->
    guild_state().
clear_pending_voice_connections_for_user(UserId, RequestSessionId, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    RemovedPending = maps:filter(
        fun(_ConnId, PendingData) ->
            maps:get(user_id, PendingData, undefined) =:= UserId andalso
                pending_session_matches(PendingData, RequestSessionId)
        end,
        PendingConnections
    ),
    FilteredPending = maps:filter(
        fun(_ConnId, PendingData) ->
            PendingUserId = maps:get(user_id, PendingData, undefined),
            PendingUserId =/= UserId orelse
                not pending_session_matches(PendingData, RequestSessionId)
        end,
        PendingConnections
    ),
    NewState = State#{pending_voice_connections => FilteredPending},
    clear_e2ee_room_keys_for_removed_pending(
        RemovedPending, voice_state_utils:voice_states(State), FilteredPending, NewState
    ).

-spec pending_session_matches(map(), binary() | undefined) -> boolean().
pending_session_matches(_PendingData, undefined) ->
    true;
pending_session_matches(PendingData, RequestSessionId) ->
    guild_voice_connection_normalize:normalize_session_id(
        maps:get(session_id, PendingData, undefined)
    ) =:= RequestSessionId.

-spec clear_pending_voice_connections_for_user_channel(integer(), integer(), guild_state()) ->
    guild_state().
clear_pending_voice_connections_for_user_channel(UserId, ChannelId, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    RemovedPending = maps:filter(
        fun(_ConnId, PendingData) ->
            pending_user_channel_matches(PendingData, UserId, ChannelId)
        end,
        PendingConnections
    ),
    FilteredPending = maps:filter(
        fun(_ConnId, PendingData) ->
            not pending_user_channel_matches(PendingData, UserId, ChannelId)
        end,
        PendingConnections
    ),
    NewState = State#{pending_voice_connections => FilteredPending},
    clear_e2ee_room_keys_for_removed_pending(
        RemovedPending, voice_state_utils:voice_states(State), FilteredPending, NewState
    ).

-spec pending_user_channel_matches(map(), integer(), integer()) -> boolean().
pending_user_channel_matches(PendingData, UserId, ChannelId) ->
    maps:get(user_id, PendingData, undefined) =:= UserId andalso
        maps:get(channel_id, PendingData, undefined) =:= ChannelId.

-spec clear_pending_voice_connections_for_channel(integer(), guild_state()) -> guild_state().
clear_pending_voice_connections_for_channel(ChannelId, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    FilteredPending = maps:filter(
        fun(_ConnId, PendingData) ->
            maps:get(channel_id, PendingData, undefined) =/= ChannelId
        end,
        PendingConnections
    ),
    NewState = State#{pending_voice_connections => FilteredPending},
    clear_e2ee_room_key_if_channel_idle(
        ChannelId, voice_state_utils:voice_states(State), NewState
    ).

-spec clear_e2ee_room_key_if_channel_idle(
    integer() | undefined, voice_state_map(), guild_state()
) -> guild_state().
clear_e2ee_room_key_if_channel_idle(ChannelId, VoiceStates, State) when is_integer(ChannelId) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    guild_voice_e2ee:forget_room_key_if_channel_idle_guild(
        ChannelId, VoiceStates, PendingConnections, State
    );
clear_e2ee_room_key_if_channel_idle(_, _VoiceStates, State) ->
    State.

-spec clear_e2ee_room_keys_for_removed(voice_state_map(), voice_state_map(), guild_state()) ->
    guild_state().
clear_e2ee_room_keys_for_removed(RemovedVoiceStates, NewVoiceStates, State) ->
    maps:fold(
        fun(_ConnId, VoiceState, AccState) ->
            clear_e2ee_room_key_if_channel_idle(
                voice_state_utils:voice_state_channel_id(VoiceState), NewVoiceStates, AccState
            )
        end,
        State,
        RemovedVoiceStates
    ).

-spec clear_e2ee_room_keys_for_removed_pending(
    map(), voice_state_map(), map(), guild_state()
) -> guild_state().
clear_e2ee_room_keys_for_removed_pending(Removed, VS, Remaining, State) ->
    maps:fold(
        fun(_ConnId, PendingData, AccState) ->
            clear_e2ee_room_key_for_removed_pending(
                PendingData, VS, Remaining, AccState
            )
        end,
        State,
        Removed
    ).

-spec clear_e2ee_room_key_for_removed_pending(map(), voice_state_map(), map(), guild_state()) ->
    guild_state().
clear_e2ee_room_key_for_removed_pending(PendingData, VoiceStates, RemainingPending, AccState) ->
    case maps:get(channel_id, PendingData, undefined) of
        ChannelId when is_integer(ChannelId) ->
            guild_voice_e2ee:forget_room_key_if_channel_idle_guild(
                ChannelId, VoiceStates, RemainingPending, AccState
            );
        _ ->
            AccState
    end.

-spec purge_count_cache([binary()]) -> ok.
purge_count_cache(ConnectionIds) ->
    lists:foreach(fun voice_state_counts_cache:remove_connection/1, ConnectionIds),
    ok.
