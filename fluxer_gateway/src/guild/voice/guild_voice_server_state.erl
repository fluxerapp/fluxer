%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_server_state).
-typing([eqwalizer]).

-export([
    build_guild_state/1,
    apply_guild_state/2,
    merge_guild_state/2,
    local_voice_states_for_channel/2,
    parse_voice_channel_id/1,
    fetch_guild_data/1,
    guild_state_call/2,
    guild_id_call/2
]).

-export_type([
    voice_state/0,
    voice_state_map/0,
    server_state/0
]).

-define(GUILD_CALL_TIMEOUT, 10000).

-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.
-type server_state() :: map().

-spec build_guild_state(server_state()) -> map().
build_guild_state(#{guild_pid := GuildPid} = State) ->
    GuildData = fetch_guild_data(GuildPid),
    GuildData#{
        guild_pid => GuildPid,
        voice_states => maps:get(voice_states, State, #{}),
        pending_voice_connections => maps:get(pending_voice_connections, State, #{}),
        recently_disconnected_voice_states =>
            maps:get(recently_disconnected_voice_states, State, #{}),
        e2ee_room_keys => maps:get(e2ee_room_keys, State, #{})
    }.

-spec apply_guild_state(map(), server_state()) -> server_state().
apply_guild_state(GuildState, State) ->
    NewState = merge_guild_state(GuildState, State),
    _ = guild_voice_server_sync:sync_voice_state_count_diff(State, NewState),
    NewState.

-spec merge_guild_state(map(), server_state()) -> server_state().
merge_guild_state(GuildState, State) ->
    State#{
        voice_states => maps:get(voice_states, GuildState, maps:get(voice_states, State, #{})),
        pending_voice_connections =>
            maps:get(
                pending_voice_connections,
                GuildState,
                maps:get(pending_voice_connections, State, #{})
            ),
        recently_disconnected_voice_states =>
            maps:get(
                recently_disconnected_voice_states,
                GuildState,
                maps:get(recently_disconnected_voice_states, State, #{})
            ),
        e2ee_room_keys =>
            maps:get(e2ee_room_keys, GuildState, maps:get(e2ee_room_keys, State, #{}))
    }.

-spec local_voice_states_for_channel(binary(), server_state()) -> map().
local_voice_states_for_channel(ChannelIdBin, State) ->
    VoiceStates = voice_state_utils:ensure_voice_states(maps:get(voice_states, State, #{})),
    Filtered = maps:fold(
        fun(ConnId, VS, Acc) ->
            prepend_defined(voice_state_rpc_entry_for_channel(ConnId, VS, ChannelIdBin), Acc)
        end,
        [],
        VoiceStates
    ),
    #{voice_states => Filtered}.

-spec parse_voice_channel_id(term()) -> {ok, integer(), binary()} | error.
parse_voice_channel_id(ChannelId) ->
    case validation:validate_snowflake(<<"channel_id">>, ChannelId) of
        {ok, ChannelIdInt} when ChannelIdInt > 0 ->
            {ok, ChannelIdInt, integer_to_binary(ChannelIdInt)};
        _ ->
            error
    end.

-spec voice_state_rpc_entry(map()) -> false | {true, map()}.
voice_state_rpc_entry(VS) when is_map(VS) ->
    Entry0 = #{
        connection_id => normalize_rpc_id(
            maps:get(<<"connection_id">>, VS, maps:get(connection_id, VS, null))
        ),
        user_id => normalize_rpc_id(maps:get(<<"user_id">>, VS, maps:get(user_id, VS, null))),
        channel_id => normalize_rpc_id(
            maps:get(<<"channel_id">>, VS, maps:get(channel_id, VS, null))
        )
    },
    case lists:member(null, maps:values(Entry0)) of
        true -> false;
        false -> {true, maybe_attach_voice_routing_metadata(Entry0, VS)}
    end.

-spec maybe_attach_voice_routing_metadata(map(), map()) -> map().
maybe_attach_voice_routing_metadata(Entry, VS) ->
    WithRegion = maybe_put_normalized_rpc_id(
        Entry, region_id, maps:get(<<"region_id">>, VS, maps:get(region_id, VS, null))
    ),
    maybe_put_normalized_rpc_id(
        WithRegion, server_id, maps:get(<<"server_id">>, VS, maps:get(server_id, VS, null))
    ).

-spec maybe_put_normalized_rpc_id(map(), atom(), term()) -> map().
maybe_put_normalized_rpc_id(Entry, Key, Value) ->
    case normalize_rpc_id(Value) of
        null -> Entry;
        Normalized -> Entry#{Key => Normalized}
    end.

-spec voice_state_rpc_entry_for_channel(term(), term(), binary()) -> map() | undefined.
voice_state_rpc_entry_for_channel(ConnId, VS, ChannelIdBin) when is_map(VS) ->
    RawChId = maps:get(<<"channel_id">>, VS, maps:get(channel_id, VS, null)),
    case normalize_rpc_id(RawChId) of
        ChannelIdBin ->
            Decorated = VS#{connection_id => ConnId, channel_id => ChannelIdBin},
            voice_state_rpc_entry_or_undefined(Decorated);
        _ ->
            undefined
    end;
voice_state_rpc_entry_for_channel(_, _, _) ->
    undefined.

-spec voice_state_rpc_entry_or_undefined(map()) -> map() | undefined.
voice_state_rpc_entry_or_undefined(VS) ->
    case voice_state_rpc_entry(VS) of
        {true, Entry} -> Entry;
        false -> undefined
    end.

-spec guild_state_call(pid(), timeout()) -> term().
guild_state_call(GuildPid, Timeout) ->
    case gen_server:call(GuildPid, {get_voice_guild_state}, Timeout) of
        GuildState when is_map(GuildState) -> GuildState;
        _ -> full_guild_state_call(GuildPid, Timeout)
    end.

-spec full_guild_state_call(pid(), timeout()) -> term().
full_guild_state_call(GuildPid, Timeout) ->
    gen_server:call(GuildPid, {get_sessions}, Timeout).

-spec guild_id_call(pid(), timeout()) -> integer() | undefined.
guild_id_call(GuildPid, Timeout) ->
    case gen_server:call(GuildPid, {get_guild_id}, Timeout) of
        GuildId when is_integer(GuildId) -> GuildId;
        _ -> guild_state_id(full_guild_state_call(GuildPid, Timeout))
    end.

-spec guild_state_id(term()) -> integer() | undefined.
guild_state_id(GuildState) when is_map(GuildState) ->
    case maps:get(id, GuildState, undefined) of
        GuildId when is_integer(GuildId) -> GuildId;
        _ -> undefined
    end;
guild_state_id(_) ->
    undefined.

-spec fetch_guild_data(pid()) -> map().
fetch_guild_data(GuildPid) ->
    try guild_state_call(GuildPid, ?GUILD_CALL_TIMEOUT) of
        GuildState when is_map(GuildState) -> GuildState;
        _ -> #{}
    catch
        exit:{timeout, _} ->
            logger:warning("Voice server timed out fetching guild state", #{}),
            #{};
        exit:{noproc, _} ->
            #{};
        exit:{normal, _} ->
            #{}
    end.

-spec prepend_defined(undefined | map(), [map()]) -> [map()].
prepend_defined(undefined, Acc) -> Acc;
prepend_defined(Entry, Acc) -> [Entry | Acc].

-spec normalize_rpc_id(term()) -> binary() | null.
normalize_rpc_id(Value) ->
    case normalize_rpc_integer(Value) of
        undefined -> normalize_rpc_binary(Value);
        Integer -> integer_to_binary(Integer)
    end.

-spec normalize_rpc_integer(term()) -> integer() | undefined.
normalize_rpc_integer(Value) when is_integer(Value), Value > 0 -> Value;
normalize_rpc_integer(Value) when is_binary(Value), byte_size(Value) > 0 ->
    case validation:validate_snowflake(<<"id">>, Value) of
        {ok, Id} when Id > 0 -> Id;
        _ -> undefined
    end;
normalize_rpc_integer(Value) when is_list(Value), Value =/= [] ->
    normalize_rpc_integer_from_list(Value);
normalize_rpc_integer(_) ->
    undefined.

-spec normalize_rpc_integer_from_list(list()) -> integer() | undefined.
normalize_rpc_integer_from_list(Value) ->
    case guild_voice_connection_normalize:normalize_optional_binary(Value) of
        Binary when is_binary(Binary) -> normalize_rpc_integer(Binary);
        undefined -> undefined
    end.

-spec normalize_rpc_binary(term()) -> binary() | null.
normalize_rpc_binary(Value) when is_binary(Value), byte_size(Value) > 0 -> Value;
normalize_rpc_binary(Value) when is_list(Value), Value =/= [] ->
    case guild_voice_connection_normalize:normalize_optional_binary(Value) of
        Binary when is_binary(Binary), byte_size(Binary) > 0 -> Binary;
        _ -> null
    end;
normalize_rpc_binary(_) ->
    null.
