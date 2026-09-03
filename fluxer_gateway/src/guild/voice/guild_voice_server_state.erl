%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_server_state).
-typing([eqwalizer]).

-export([
    build_guild_state/1,
    apply_guild_state/2,
    merge_guild_state/2,
    local_voice_states_for_channel/2,
    local_pending_joins_for_channel/2,
    parse_voice_channel_id/1,
    repair_voice_state_from_guild_cache/2,
    voice_state_rpc_entries/1,
    pending_join_rpc_entries/1,
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

-spec local_pending_joins_for_channel(binary(), server_state()) -> map().
local_pending_joins_for_channel(ChannelIdBin, State) ->
    case parse_voice_channel_id(ChannelIdBin) of
        {ok, ChannelIdInt, _NormalizedBin} ->
            do_local_pending_joins(ChannelIdInt, State);
        error ->
            #{pending_joins => []}
    end.

-spec do_local_pending_joins(integer(), server_state()) -> map().
do_local_pending_joins(ChannelIdInt, State) ->
    PendingConns = ensure_map(maps:get(pending_voice_connections, State, #{})),
    Filtered = maps:fold(
        fun(ConnId, Metadata, Acc) ->
            prepend_pending_join_for_channel(ConnId, Metadata, ChannelIdInt, Acc)
        end,
        [],
        PendingConns
    ),
    #{pending_joins => Filtered}.

-spec prepend_pending_join_for_channel(term(), term(), integer(), [map()]) -> [map()].
prepend_pending_join_for_channel(ConnId, Metadata, ChannelIdInt, Acc) ->
    case pending_join_channel_id(Metadata) of
        ChannelIdInt -> prepend_defined(pending_join_rpc_entry(ConnId, Metadata), Acc);
        _ -> Acc
    end.

-spec parse_voice_channel_id(term()) -> {ok, integer(), binary()} | error.
parse_voice_channel_id(ChannelId) ->
    case validation:validate_snowflake(<<"channel_id">>, ChannelId) of
        {ok, ChannelIdInt} when ChannelIdInt > 0 ->
            {ok, ChannelIdInt, integer_to_binary(ChannelIdInt)};
        _ ->
            error
    end.

-spec repair_voice_state_from_guild_cache(map(), server_state()) -> {map(), server_state()}.
repair_voice_state_from_guild_cache(Request, State) ->
    ConnectionId = maps:get(connection_id, Request, undefined),
    UserId = maps:get(user_id, Request, undefined),
    ChannelId = maps:get(channel_id, Request, undefined),
    case {ConnectionId, UserId, ChannelId} of
        {Conn, UId, CId} when is_binary(Conn), is_integer(UId), is_integer(CId) ->
            do_repair(Conn, UId, CId, State);
        _ ->
            {#{success => false, error => voice_invalid_state}, State}
    end.

-spec do_repair(binary(), integer(), integer(), server_state()) -> {map(), server_state()}.
do_repair(ConnectionId, UserId, ChannelId, State) ->
    VoiceStates = maps:get(voice_states, State, #{}),
    case maps:get(ConnectionId, VoiceStates, undefined) of
        ExistingVoiceState when is_map(ExistingVoiceState) ->
            repair_existing_voice_state(
                ExistingVoiceState, ConnectionId, UserId, ChannelId, State
            );
        _ ->
            repair_from_cached(ConnectionId, UserId, ChannelId, VoiceStates, State)
    end.

-spec repair_existing_voice_state(
    voice_state(), binary(), integer(), integer(), server_state()
) ->
    {map(), server_state()}.
repair_existing_voice_state(ExistingVoiceState, ConnectionId, UserId, ChannelId, State) ->
    case voice_state_matches(ExistingVoiceState, ConnectionId, UserId, ChannelId) of
        true -> {#{success => true, repaired => false}, State};
        false -> {#{success => false, error => voice_state_mismatch}, State}
    end.

-spec repair_from_cached(binary(), integer(), integer(), voice_state_map(), server_state()) ->
    {map(), server_state()}.
repair_from_cached(ConnectionId, UserId, ChannelId, VoiceStates, State) ->
    case fetch_cached_voice_state(ConnectionId, State) of
        {ok, CachedVoiceState} ->
            repair_with_cached(
                ConnectionId, UserId, ChannelId, CachedVoiceState, VoiceStates, State
            );
        {error, _Reason} ->
            {#{success => false, error => voice_connection_not_found}, State}
    end.

-spec repair_with_cached(
    binary(), integer(), integer(), voice_state(), voice_state_map(), server_state()
) -> {map(), server_state()}.
repair_with_cached(ConnectionId, UserId, ChannelId, CachedVoiceState, VoiceStates, State) ->
    case voice_state_matches(CachedVoiceState, ConnectionId, UserId, ChannelId) of
        false ->
            {#{success => false, error => voice_state_mismatch}, State};
        true ->
            OldVoiceStates = maps:get(voice_states, State, #{}),
            NewVoiceStates = VoiceStates#{ConnectionId => CachedVoiceState},
            _ = guild_voice_server_sync:sync_replaced_voice_states(
                OldVoiceStates, NewVoiceStates
            ),
            PendingConns = maps:remove(
                ConnectionId, maps:get(pending_voice_connections, State, #{})
            ),
            RecentDisc = maps:remove(
                ConnectionId,
                maps:get(recently_disconnected_voice_states, State, #{})
            ),
            NewState0 = State#{
                voice_states => NewVoiceStates,
                pending_voice_connections => PendingConns,
                recently_disconnected_voice_states => RecentDisc
            },
            GuildState = build_guild_state(NewState0),
            ChannelIdBin = maps:get(<<"channel_id">>, CachedVoiceState, null),
            guild_voice_broadcast:broadcast_voice_state_update(
                CachedVoiceState, GuildState, ChannelIdBin
            ),
            logger:warning(
                voice_state_repaired_log_message(),
                [maps:get(guild_id, State), ChannelId, UserId, ConnectionId]
            ),
            {#{success => true, repaired => true}, NewState0}
    end.

-spec voice_state_repaired_log_message() -> string().
voice_state_repaired_log_message() ->
    "guild_voice_state_repaired_from_guild_cache: guild_id=~p "
    "channel_id=~p user_id=~p connection_id=~p".

-spec fetch_cached_voice_state(binary(), server_state()) ->
    {ok, voice_state()} | {error, not_found}.
fetch_cached_voice_state(ConnectionId, #{guild_pid := GuildPid}) when is_pid(GuildPid) ->
    try gen_server:call(GuildPid, {get_cached_voice_state_by_connection, ConnectionId}, 1000) of
        {ok, VoiceState} when is_map(VoiceState) -> {ok, VoiceState};
        _ -> {error, not_found}
    catch
        throw:_ -> {error, not_found};
        error:_ -> {error, not_found};
        exit:_ -> {error, not_found}
    end;
fetch_cached_voice_state(_ConnectionId, _State) ->
    {error, not_found}.

-spec voice_state_matches(voice_state(), binary(), integer(), integer()) -> boolean().
voice_state_matches(VoiceState, ConnectionId, UserId, ChannelId) ->
    maps:get(<<"connection_id">>, VoiceState, ConnectionId) =:= ConnectionId andalso
        voice_state_utils:voice_state_user_id(VoiceState) =:= UserId andalso
        voice_state_utils:voice_state_channel_id(VoiceState) =:= ChannelId.

-spec voice_state_rpc_entries(term()) -> [map()].
voice_state_rpc_entries(VoiceStates) when is_list(VoiceStates) ->
    lists:filtermap(fun voice_state_rpc_entry/1, VoiceStates);
voice_state_rpc_entries(_) ->
    [].

-spec voice_state_rpc_entry(term()) -> false | {true, map()}.
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
    end;
voice_state_rpc_entry(_) ->
    false.

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

-spec pending_join_rpc_entries(term()) -> [map()].
pending_join_rpc_entries(Pending) when is_list(Pending) ->
    lists:filtermap(fun pending_join_rpc_entry_from_map/1, Pending);
pending_join_rpc_entries(_) ->
    [].

-spec pending_join_rpc_entry_from_map(term()) -> false | {true, map()}.
pending_join_rpc_entry_from_map(PendingJoin) when is_map(PendingJoin) ->
    ConnId = maps:get(
        connection_id, PendingJoin, maps:get(<<"connection_id">>, PendingJoin, null)
    ),
    case pending_join_rpc_entry(ConnId, PendingJoin) of
        undefined -> false;
        Entry -> {true, Entry}
    end;
pending_join_rpc_entry_from_map(_) ->
    false.

-spec pending_join_rpc_entry(term(), term()) -> map() | undefined.
pending_join_rpc_entry(ConnId, Metadata) when is_map(Metadata) ->
    NormalizedConnId = normalize_rpc_id(ConnId),
    UserId = normalize_rpc_id(
        maps:get(user_id, Metadata, maps:get(<<"user_id">>, Metadata, null))
    ),
    case {NormalizedConnId, UserId} of
        {null, _} ->
            undefined;
        {_, null} ->
            undefined;
        _ ->
            #{
                connection_id => NormalizedConnId,
                user_id => UserId,
                token_nonce => normalize_rpc_binary(
                    maps:get(token_nonce, Metadata, maps:get(<<"token_nonce">>, Metadata, null))
                ),
                expires_at => normalize_rpc_millisecond(
                    maps:get(expires_at, Metadata, maps:get(<<"expires_at">>, Metadata, 0))
                )
            }
    end;
pending_join_rpc_entry(_, _) ->
    undefined.

-spec pending_join_channel_id(term()) -> integer() | undefined.
pending_join_channel_id(Metadata) when is_map(Metadata) ->
    normalize_rpc_integer(
        maps:get(channel_id, Metadata, maps:get(<<"channel_id">>, Metadata, undefined))
    );
pending_join_channel_id(_) ->
    undefined.

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

-spec ensure_map(term()) -> map().
ensure_map(Map) when is_map(Map) -> Map;
ensure_map(_) -> #{}.

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

-spec normalize_rpc_millisecond(term()) -> integer().
normalize_rpc_millisecond(Value) when is_integer(Value), Value >= 0 -> Value;
normalize_rpc_millisecond(Value) when is_binary(Value), byte_size(Value) > 0 ->
    try binary_to_integer(Value) of
        Integer when Integer >= 0 -> Integer;
        _ -> 0
    catch
        throw:_ -> 0;
        error:_ -> 0;
        exit:_ -> 0
    end;
normalize_rpc_millisecond(_) ->
    0.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

guild_state_call_uses_projection_test() ->
    with_fake_guild(fun new_guild_handler/1, fun(Pid) ->
        ?assertEqual(projected_guild_state(), guild_state_call(Pid, 1000)),
        ?assertEqual([{get_voice_guild_state}], drain_requests())
    end).

guild_state_call_retries_get_sessions_against_old_peer_test() ->
    with_fake_guild(fun old_guild_handler/1, fun(Pid) ->
        ?assertEqual(full_guild_state(), guild_state_call(Pid, 1000)),
        ?assertEqual([{get_voice_guild_state}, {get_sessions}], drain_requests())
    end).

fetch_guild_data_retries_get_sessions_against_old_peer_test() ->
    with_fake_guild(fun old_guild_handler/1, fun(Pid) ->
        ?assertEqual(full_guild_state(), fetch_guild_data(Pid)),
        ?assertEqual([{get_voice_guild_state}, {get_sessions}], drain_requests())
    end).

guild_id_call_uses_get_guild_id_test() ->
    with_fake_guild(fun new_guild_handler/1, fun(Pid) ->
        ?assertEqual(42, guild_id_call(Pid, 1000)),
        ?assertEqual([{get_guild_id}], drain_requests())
    end).

guild_id_call_returns_real_id_against_old_peer_test() ->
    with_fake_guild(fun old_guild_handler/1, fun(Pid) ->
        ?assertEqual(42, guild_id_call(Pid, 1000)),
        ?assertEqual([{get_guild_id}, {get_sessions}], drain_requests())
    end).

guild_id_call_returns_undefined_when_peer_has_no_id_test() ->
    with_fake_guild(fun(_) -> ok end, fun(Pid) ->
        ?assertEqual(undefined, guild_id_call(Pid, 1000)),
        ?assertEqual([{get_guild_id}, {get_sessions}], drain_requests())
    end).

new_guild_handler({get_voice_guild_state}) -> projected_guild_state();
new_guild_handler({get_guild_id}) -> 42;
new_guild_handler({get_sessions}) -> full_guild_state();
new_guild_handler(_) -> ok.

old_guild_handler({get_sessions}) -> full_guild_state();
old_guild_handler(_) -> ok.

full_guild_state() ->
    #{
        id => 42,
        sessions => #{},
        voice_states => #{},
        data => #{<<"id">> => <<"42">>, <<"members">> => #{}}
    }.

projected_guild_state() ->
    #{id => 42, sessions => #{}, voice_states => #{}, data => #{<<"id">> => <<"42">>}}.

with_fake_guild(Handler, Fun) ->
    _ = drain_requests(),
    Owner = self(),
    Pid = spawn(fun() -> fake_guild_loop(Owner, Handler) end),
    try
        Fun(Pid)
    after
        Pid ! stop
    end.

fake_guild_loop(Owner, Handler) ->
    receive
        {'$gen_call', From, Request} ->
            Owner ! {fake_guild_request, Request},
            gen_server:reply(From, Handler(Request)),
            fake_guild_loop(Owner, Handler);
        stop ->
            ok
    end.

drain_requests() ->
    receive
        {fake_guild_request, Request} -> [Request | drain_requests()]
    after 0 -> []
    end.

-endif.
