%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(voice_state_counts_cache).
-typing([eqwalizer]).

-export([
    ensure_tables/0,
    upsert_voice_state/1,
    remove_connection/1,
    get_local_counts/0,
    rebuild_from_live/0,
    rebuild_from_live/1
]).

-export_type([
    voice_state/0
]).

-type voice_state() :: map().

-define(REBUILD_CALL_TIMEOUT_MS, 500).
-define(CONNECTION_TABLE, voice_state_count_connections).
-define(REGION_TABLE, voice_state_count_regions).
-define(SERVER_TABLE, voice_state_count_servers).
-define(TABLE_OPTS, [
    named_table,
    public,
    set,
    {read_concurrency, true},
    {write_concurrency, true}
]).

-spec ensure_tables() -> ok.
ensure_tables() ->
    guild_ets_utils:ensure_table(?CONNECTION_TABLE, ?TABLE_OPTS),
    guild_ets_utils:ensure_table(?REGION_TABLE, ?TABLE_OPTS),
    guild_ets_utils:ensure_table(?SERVER_TABLE, ?TABLE_OPTS).

-spec upsert_voice_state(voice_state()) -> ok.
upsert_voice_state(VoiceState) when is_map(VoiceState) ->
    ensure_tables(),
    ConnectionId = guild_voice_connection_normalize:normalize_optional_binary(
        get_optional_field(VoiceState, <<"connection_id">>, connection_id)
    ),
    upsert_by_connection(ConnectionId, VoiceState);
upsert_voice_state(_) ->
    ok.

-spec upsert_by_connection(binary() | undefined, voice_state()) -> ok.
upsert_by_connection(undefined, _VoiceState) ->
    ok;
upsert_by_connection(ConnectionId, VoiceState) ->
    case
        guild_voice_connection_normalize:normalize_optional_binary(
            get_optional_field(VoiceState, <<"channel_id">>, channel_id, null)
        )
    of
        undefined ->
            remove_connection(ConnectionId);
        _ChannelId ->
            RegionId = guild_voice_connection_normalize:normalize_optional_binary(
                get_optional_field(VoiceState, <<"region_id">>, region_id)
            ),
            ServerId = guild_voice_connection_normalize:normalize_optional_binary(
                get_optional_field(VoiceState, <<"server_id">>, server_id)
            ),
            upsert_connection_metadata(ConnectionId, RegionId, ServerId)
    end.

-spec remove_connection(binary() | term()) -> ok.
remove_connection(ConnectionId) ->
    ensure_tables(),
    case guild_voice_connection_normalize:normalize_optional_binary(ConnectionId) of
        undefined ->
            ok;
        NormalizedConnectionId ->
            do_remove_connection(NormalizedConnectionId)
    end.

-spec do_remove_connection(binary()) -> ok.
do_remove_connection(ConnId) ->
    case ets:lookup(?CONNECTION_TABLE, ConnId) of
        [{ConnId, OldRegionId, OldServerId}] ->
            maybe_decrement_counter(?REGION_TABLE, OldRegionId),
            maybe_decrement_counter(?SERVER_TABLE, OldServerId),
            ets:delete(?CONNECTION_TABLE, ConnId),
            ok;
        _ ->
            ok
    end.

-spec get_local_counts() -> map().
get_local_counts() ->
    ensure_tables(),
    TotalVoiceStates = table_size(?CONNECTION_TABLE),
    Regions = build_count_entries(?REGION_TABLE, <<"region_id">>),
    Servers = build_count_entries(?SERVER_TABLE, <<"server_id">>),
    #{
        <<"total_voice_states">> => TotalVoiceStates,
        <<"regions">> => Regions,
        <<"servers">> => Servers
    }.

-spec rebuild_from_live() -> #{removed := non_neg_integer(), upserted := non_neg_integer()}.
rebuild_from_live() ->
    rebuild_from_live(?REBUILD_CALL_TIMEOUT_MS).

-spec rebuild_from_live(timeout()) ->
    #{removed := non_neg_integer(), upserted := non_neg_integer()}.
rebuild_from_live(CallTimeout) ->
    ensure_tables(),
    LiveStates = voice_state_counts_cache_sync:collect_live_voice_states(CallTimeout),
    LiveKeys = sets:from_list(maps:keys(LiveStates)),
    Removed = remove_stale_connections(LiveKeys),
    Upserted = upsert_live_states(LiveStates),
    #{removed => Removed, upserted => Upserted}.

-spec remove_stale_connections(sets:set(binary())) -> non_neg_integer().
remove_stale_connections(LiveKeys) ->
    ets:foldl(
        fun
            ({ConnId, _Region, _Server}, Acc) ->
                remove_stale_connection(ConnId, LiveKeys, Acc);
            (_Row, Acc) ->
                Acc
        end,
        0,
        ?CONNECTION_TABLE
    ).

-spec remove_stale_connection(binary(), sets:set(binary()), non_neg_integer()) ->
    non_neg_integer().
remove_stale_connection(ConnId, LiveKeys, Acc) ->
    case sets:is_element(ConnId, LiveKeys) of
        true ->
            Acc;
        false ->
            ok = remove_connection(ConnId),
            Acc + 1
    end.

-spec upsert_live_states(#{binary() => voice_state()}) -> non_neg_integer().
upsert_live_states(LiveStates) ->
    maps:fold(
        fun(_ConnId, VoiceState, Acc) ->
            ok = upsert_voice_state(VoiceState),
            Acc + 1
        end,
        0,
        LiveStates
    ).

-spec upsert_connection_metadata(binary(), binary() | undefined, binary() | undefined) -> ok.
upsert_connection_metadata(ConnectionId, RegionId, ServerId) ->
    case ets:lookup(?CONNECTION_TABLE, ConnectionId) of
        [{ConnectionId, RegionId, ServerId}] ->
            ok;
        [{ConnectionId, OldRegionId, OldServerId}] ->
            maybe_decrement_counter(?REGION_TABLE, OldRegionId),
            maybe_decrement_counter(?SERVER_TABLE, OldServerId),
            maybe_increment_counter(?REGION_TABLE, RegionId),
            maybe_increment_counter(?SERVER_TABLE, ServerId),
            ets:insert(?CONNECTION_TABLE, {ConnectionId, RegionId, ServerId}),
            ok;
        _ ->
            maybe_increment_counter(?REGION_TABLE, RegionId),
            maybe_increment_counter(?SERVER_TABLE, ServerId),
            ets:insert(?CONNECTION_TABLE, {ConnectionId, RegionId, ServerId}),
            ok
    end.

-spec maybe_increment_counter(atom(), binary() | undefined) -> ok.
maybe_increment_counter(_Table, undefined) ->
    ok;
maybe_increment_counter(Table, Key) ->
    _ = ets:update_counter(Table, Key, {2, 1}, {Key, 0}),
    ok.

-spec maybe_decrement_counter(atom(), binary() | undefined) -> ok.
maybe_decrement_counter(_Table, undefined) ->
    ok;
maybe_decrement_counter(Table, Key) ->
    case ets:lookup(Table, Key) of
        [{Key, Count}] when is_integer(Count), Count =< 1 ->
            ets:delete(Table, Key),
            ok;
        [{Key, _Count}] ->
            _ = ets:update_counter(Table, Key, {2, -1}),
            ok;
        _ ->
            ok
    end.

-spec table_size(atom()) -> non_neg_integer().
table_size(Table) ->
    case ets:info(Table, size) of
        undefined -> 0;
        Size when is_integer(Size), Size >= 0 -> Size;
        _ -> 0
    end.

-spec build_count_entries(atom(), binary()) -> [map()].
build_count_entries(Table, IdKey) ->
    SortedRows = lists:sort(fun compare_count_rows/2, ets:tab2list(Table)),
    [
        #{IdKey => Id, <<"voice_state_count">> => Count}
     || {Id, Count} <- SortedRows, is_binary(Id), is_integer(Count), Count > 0
    ].

-spec compare_count_rows({binary(), integer()}, {binary(), integer()}) -> boolean().
compare_count_rows({LeftId, LeftCount}, {RightId, RightCount}) ->
    case LeftCount =:= RightCount of
        true -> LeftId =< RightId;
        false -> LeftCount > RightCount
    end.

-spec get_optional_field(map(), term(), term()) -> term().
get_optional_field(Map, PrimaryKey, FallbackKey) ->
    get_optional_field(Map, PrimaryKey, FallbackKey, undefined).

-spec get_optional_field(map(), term(), term(), term()) -> term().
get_optional_field(Map, PrimaryKey, FallbackKey, Default) ->
    maps:get(PrimaryKey, Map, maps:get(FallbackKey, Map, Default)).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec reset_tables() -> ok.
reset_tables() ->
    try
        ets:delete(?CONNECTION_TABLE)
    catch
        error:badarg -> ok
    end,
    try
        ets:delete(?REGION_TABLE)
    catch
        error:badarg -> ok
    end,
    try
        ets:delete(?SERVER_TABLE)
    catch
        error:badarg -> ok
    end,
    ok.

upsert_voice_state_tracks_region_and_server_counts_test() ->
    ok = reset_tables(),
    VoiceState = #{
        <<"connection_id">> => <<"conn-1">>,
        <<"channel_id">> => <<"123">>,
        <<"region_id">> => <<"us-east">>,
        <<"server_id">> => <<"us-east-1">>
    },
    ok = upsert_voice_state(VoiceState),
    Counts = get_local_counts(),
    ?assertEqual(1, maps:get(<<"total_voice_states">>, Counts)),
    ?assertEqual(
        [#{<<"region_id">> => <<"us-east">>, <<"voice_state_count">> => 1}],
        maps:get(<<"regions">>, Counts)
    ),
    ?assertEqual(
        [#{<<"server_id">> => <<"us-east-1">>, <<"voice_state_count">> => 1}],
        maps:get(<<"servers">>, Counts)
    ).

upsert_voice_state_replaces_existing_metadata_test() ->
    ok = reset_tables(),
    ok = upsert_voice_state(#{
        <<"connection_id">> => <<"conn-1">>,
        <<"channel_id">> => <<"123">>,
        <<"region_id">> => <<"us-east">>,
        <<"server_id">> => <<"us-east-1">>
    }),
    ok = upsert_voice_state(#{
        <<"connection_id">> => <<"conn-1">>,
        <<"channel_id">> => <<"456">>,
        <<"region_id">> => <<"eu-west">>,
        <<"server_id">> => <<"eu-west-2">>
    }),
    Counts = get_local_counts(),
    ?assertEqual(1, maps:get(<<"total_voice_states">>, Counts)),
    ?assertEqual(
        [#{<<"region_id">> => <<"eu-west">>, <<"voice_state_count">> => 1}],
        maps:get(<<"regions">>, Counts)
    ),
    ?assertEqual(
        [#{<<"server_id">> => <<"eu-west-2">>, <<"voice_state_count">> => 1}],
        maps:get(<<"servers">>, Counts)
    ).

remove_connection_decrements_counters_test() ->
    ok = reset_tables(),
    ok = upsert_voice_state(#{
        <<"connection_id">> => <<"conn-1">>,
        <<"channel_id">> => <<"123">>,
        <<"region_id">> => <<"us-east">>,
        <<"server_id">> => <<"us-east-1">>
    }),
    ok = remove_connection(<<"conn-1">>),
    Counts = get_local_counts(),
    ?assertEqual(0, maps:get(<<"total_voice_states">>, Counts)),
    ?assertEqual([], maps:get(<<"regions">>, Counts)),
    ?assertEqual([], maps:get(<<"servers">>, Counts)).

upsert_voice_state_disconnect_removes_connection_test() ->
    ok = reset_tables(),
    ok = upsert_voice_state(#{
        <<"connection_id">> => <<"conn-1">>,
        <<"channel_id">> => <<"123">>,
        <<"region_id">> => <<"us-east">>,
        <<"server_id">> => <<"us-east-1">>
    }),
    ok = upsert_voice_state(#{
        <<"connection_id">> => <<"conn-1">>,
        <<"channel_id">> => null
    }),
    Counts = get_local_counts(),
    ?assertEqual(0, maps:get(<<"total_voice_states">>, Counts)),
    ?assertEqual([], maps:get(<<"regions">>, Counts)),
    ?assertEqual([], maps:get(<<"servers">>, Counts)).

upsert_voice_state_accepts_atom_keys_test() ->
    ok = reset_tables(),
    ok = upsert_voice_state(#{
        connection_id => <<"conn-atom">>,
        channel_id => 123,
        region_id => <<"atom-region">>,
        server_id => <<"atom-server">>
    }),
    Counts = get_local_counts(),
    ?assertEqual(1, maps:get(<<"total_voice_states">>, Counts)),
    ?assertEqual(
        [#{<<"region_id">> => <<"atom-region">>, <<"voice_state_count">> => 1}],
        maps:get(<<"regions">>, Counts)
    ),
    ?assertEqual(
        [#{<<"server_id">> => <<"atom-server">>, <<"voice_state_count">> => 1}],
        maps:get(<<"servers">>, Counts)
    ).

-endif.
