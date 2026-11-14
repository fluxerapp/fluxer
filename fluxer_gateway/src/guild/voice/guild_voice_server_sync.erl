%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_server_sync).
-typing([eqwalizer]).

-export([
    sync_voice_state_count_diff/2,
    sync_voice_state_counts/1,
    cleanup_voice_state_counts/1,
    ensure_registry/0
]).

-export_type([
    voice_state_map/0,
    server_state/0
]).

-define(REGISTRY_TABLE, guild_voice_registry).

-type voice_state_map() :: #{binary() => map()}.
-type server_state() :: map().

-spec sync_voice_state_count_diff(server_state(), server_state()) -> ok.
sync_voice_state_count_diff(OldState, NewState) ->
    OldVoiceStates = maps:get(voice_states, OldState, #{}),
    NewVoiceStates = maps:get(voice_states, NewState, #{}),
    case OldVoiceStates =:= NewVoiceStates of
        true -> ok;
        false -> sync_replaced_voice_states(OldVoiceStates, NewVoiceStates)
    end.

-spec sync_voice_state_counts(voice_state_map()) -> ok.
sync_voice_state_counts(VoiceStates) ->
    maps:foreach(
        fun(_ConnectionId, VoiceState) ->
            _ = voice_state_counts_cache:upsert_voice_state(VoiceState),
            ok
        end,
        VoiceStates
    ),
    ok.

-spec sync_replaced_voice_states(voice_state_map(), voice_state_map()) -> ok.
sync_replaced_voice_states(OldVoiceStates, NewVoiceStates) ->
    maps:foreach(
        fun(ConnectionId, _VoiceState) ->
            remove_missing_replaced_voice_state(ConnectionId, NewVoiceStates)
        end,
        OldVoiceStates
    ),
    sync_voice_state_counts(NewVoiceStates).

-spec remove_missing_replaced_voice_state(binary(), voice_state_map()) -> ok.
remove_missing_replaced_voice_state(ConnectionId, NewVoiceStates) ->
    case maps:is_key(ConnectionId, NewVoiceStates) of
        true ->
            ok;
        false ->
            _ = voice_state_counts_cache:remove_connection(ConnectionId),
            ok
    end.

-spec cleanup_voice_state_counts(voice_state_map()) -> ok.
cleanup_voice_state_counts(VoiceStates) ->
    maps:foreach(
        fun(ConnectionId, _VoiceState) ->
            _ = voice_state_counts_cache:remove_connection(ConnectionId),
            ok
        end,
        VoiceStates
    ),
    ok.

-spec ensure_registry() -> ok.
ensure_registry() ->
    guild_ets_utils:ensure_table(?REGISTRY_TABLE, [
        named_table,
        public,
        set,
        {read_concurrency, true},
        {write_concurrency, true}
    ]).
