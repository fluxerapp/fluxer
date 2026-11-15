%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_server_caps).
-typing([eqwalizer]).

-export([bounded_put/4, sweep/1]).

-define(MAX_RECENT_DISCONNECTS, 500).
-define(MAX_E2EE_KEYS, 1000).
-define(RECENT_DISCONNECT_TTL_MS, 120000).

-spec bounded_put(term(), term(), map(), pos_integer()) -> map().
bounded_put(Key, Value, Map, MaxSize) ->
    case maps:is_key(Key, Map) of
        true ->
            Map#{Key => Value};
        false ->
            bounded_put_new(Key, Value, Map, MaxSize)
    end.

-spec bounded_put_new(term(), term(), map(), pos_integer()) -> map().
bounded_put_new(Key, Value, Map, MaxSize) ->
    case maps:size(Map) >= MaxSize of
        true ->
            logger:warning("Voice server bounded map at capacity", #{max_size => MaxSize}),
            Map;
        false ->
            Map#{Key => Value}
    end.

-spec sweep(map()) -> map().
sweep(State) ->
    sweep_e2ee_room_keys(sweep_recently_disconnected(State)).

-spec sweep_recently_disconnected(map()) -> map().
sweep_recently_disconnected(State) ->
    Cache = maps:get(recently_disconnected_voice_states, State, #{}),
    case maps:size(Cache) of
        0 ->
            State;
        _ ->
            Capped = evict_stale_disconnects(Cache),
            State#{recently_disconnected_voice_states => Capped}
    end.

-spec evict_stale_disconnects(map()) -> map().
evict_stale_disconnects(Cache) ->
    Now = erlang:system_time(millisecond),
    IsAlive = fun
        (_ConnId, #{disconnected_at := At}) -> (Now - At) < ?RECENT_DISCONNECT_TTL_MS;
        (_ConnId, _) -> false
    end,
    enforce_map_cap(maps:filter(IsAlive, Cache), ?MAX_RECENT_DISCONNECTS).

-spec sweep_e2ee_room_keys(map()) -> map().
sweep_e2ee_room_keys(State) ->
    Keys = maps:get(e2ee_room_keys, State, #{}),
    case maps:size(Keys) of
        0 ->
            State;
        Size when Size =< ?MAX_E2EE_KEYS ->
            State;
        _ ->
            State#{e2ee_room_keys => enforce_map_cap(Keys, ?MAX_E2EE_KEYS)}
    end.

-spec enforce_map_cap(map(), pos_integer()) -> map().
enforce_map_cap(Map, MaxSize) ->
    case maps:size(Map) =< MaxSize of
        true ->
            Map;
        false ->
            {Keep, _} = lists:split(MaxSize, maps:to_list(Map)),
            maps:from_list(Keep)
    end.
