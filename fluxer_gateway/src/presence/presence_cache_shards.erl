%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_cache_shards).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    determine_count/1,
    start_all/1,
    stop_all/1,
    handle_down_by_ref/2,
    handle_down_by_pid/2,
    forward_call/3,
    forward_put/3,
    forward_delete/2,
    forward_bulk_get/2,
    forward_bulk_get_map/2,
    local_snapshot/1,
    local_snapshot_chunks/2,
    content_digest/1,
    memory_stats/1,
    find_by_ref/2
]).

-export_type([shard/0, state/0]).

-type shard() :: #{pid := pid(), ref := reference()}.
-type state() :: #{
    shards := #{non_neg_integer() => shard()}, shard_count := pos_integer(), _ => _
}.

-spec determine_count(atom()) -> {pos_integer(), configured | auto}.
determine_count(ConfigKey) ->
    case clustered_ets_cache:determine_shard_count([ConfigKey]) of
        {Count, ConfigKey} -> {Count, configured};
        {Count, auto} -> {Count, auto};
        {Count, _Source} -> {Count, configured}
    end.

-spec start_all(pos_integer()) -> #{non_neg_integer() => shard()}.
start_all(Count) ->
    lists:foldl(fun start_and_accumulate/2, #{}, lists:seq(0, Count - 1)).

-spec start_and_accumulate(non_neg_integer(), #{non_neg_integer() => shard()}) ->
    #{non_neg_integer() => shard()}.
start_and_accumulate(Index, MapAcc) ->
    case start(Index) of
        {ok, Shard} -> MapAcc#{Index => Shard};
        {error, _Reason} -> MapAcc
    end.

-spec stop_all(state()) -> ok.
stop_all(State) ->
    Shards = maps:get(shards, State),
    lists:foreach(fun(#{pid := Pid}) -> safe_stop(Pid) end, maps:values(Shards)).

-spec handle_down_by_ref(reference(), state()) -> state().
handle_down_by_ref(Ref, State) ->
    Shards = maps:get(shards, State),
    case find_by_ref(Ref, Shards) of
        {ok, Index} -> restart_state(Index, State);
        not_found -> State
    end.

-spec handle_down_by_pid(pid(), state()) -> state().
handle_down_by_pid(Pid, State) ->
    Shards = maps:get(shards, State),
    case shard_utils:find_shard_by_pid(Pid, Shards) of
        {ok, Index} -> restart_state(Index, State);
        not_found -> State
    end.

-spec forward_call(term(), term(), state()) -> {term(), state()}.
forward_call(Key, Request, State) ->
    {Index, State1} = ensure_shard(Key, State),
    call_shard(Index, Request, State1).

-spec forward_put(integer(), map(), state()) -> {ok, state()}.
forward_put(UserId, Presence, State) ->
    {Index, State1} = ensure_shard(UserId, State),
    {presence_cache_shard:write_put(Index, UserId, Presence), State1}.

-spec forward_delete(integer(), state()) -> {ok, state()}.
forward_delete(UserId, State) ->
    {Index, State1} = ensure_shard(UserId, State),
    {presence_cache_shard:write_delete(Index, UserId), State1}.

-spec forward_bulk_get([integer()], state()) -> {[map()], state()}.
forward_bulk_get(UserIds, State) ->
    {PresenceMap, NewState} = forward_bulk_get_map(UserIds, State),
    {presence_cache_bulk:presence_values(PresenceMap), NewState}.

-spec forward_bulk_get_map([integer()], state()) -> {#{integer() => map()}, state()}.
forward_bulk_get_map(UserIds, State) ->
    Count = maps:get(shard_count, State),
    Unique = presence_cache_bulk:normalize_user_ids(UserIds),
    Groups = normalize_bulk_groups(rendezvous_router:group_keys(Unique, Count)),
    lists:foldl(fun merge_bulk_group/2, {#{}, State}, Groups).

-spec local_snapshot(state()) -> #{integer() => map()}.
local_snapshot(State) ->
    Count = maps:get(shard_count, State),
    {SnapshotMap, _FinalState} = lists:foldl(
        fun merge_snapshot_index/2,
        {#{}, State},
        lists:seq(0, Count - 1)
    ),
    SnapshotMap.

-spec local_snapshot_chunks(state(), pos_integer()) -> [#{integer() => map()}].
local_snapshot_chunks(State, ChunkSize) when is_integer(ChunkSize), ChunkSize > 0 ->
    Count = maps:get(shard_count, State),
    lists:foldl(
        fun(Index, Acc) ->
            TableName = presence_cache_shard:table_name(Index),
            collect_table_chunks(TableName, ChunkSize, Acc)
        end,
        [],
        lists:seq(0, Count - 1)
    ).

-spec content_digest(state()) -> binary().
content_digest(State) ->
    Count = maps:get(shard_count, State),
    Combined = lists:foldl(
        fun(Index, Acc) ->
            TableName = presence_cache_shard:table_name(Index),
            Acc bxor table_digest(TableName)
        end,
        0,
        lists:seq(0, Count - 1)
    ),
    <<Combined:64/unsigned-integer>>.

-spec table_digest(atom()) -> non_neg_integer().
table_digest(TableName) ->
    try
        ets:foldl(
            fun(Entry, Acc) -> Acc bxor erlang:phash2(Entry, 16#100000000) end,
            0,
            TableName
        )
    catch
        error:badarg -> 0
    end.

-spec collect_table_chunks(atom(), pos_integer(), [#{integer() => map()}]) ->
    [#{integer() => map()}].
collect_table_chunks(TableName, ChunkSize, Acc) ->
    try ets:select(TableName, [{{'$1', '$2'}, [], [{{'$1', '$2'}}]}], ChunkSize) of
        '$end_of_table' -> Acc;
        {Matches, Continuation} -> drain_select(Continuation, [chunk_map(Matches) | Acc])
    catch
        error:badarg -> Acc
    end.

-spec drain_select(term(), [#{integer() => map()}]) -> [#{integer() => map()}].
drain_select(Continuation, Acc) ->
    case ets:select(Continuation) of
        '$end_of_table' ->
            Acc;
        {Matches, NextContinuation} ->
            drain_select(NextContinuation, [chunk_map(Matches) | Acc])
    end.

-spec chunk_map([{integer(), map()}]) -> #{integer() => map()}.
chunk_map(Matches) ->
    lists:foldl(
        fun
            ({UserId, Presence}, Acc) when is_integer(UserId), is_map(Presence) ->
                Acc#{UserId => Presence};
            (_, Acc) ->
                Acc
        end,
        #{},
        Matches
    ).

-spec memory_stats(state()) -> {ok, map()}.
memory_stats(State) ->
    Count = maps:get(shard_count, State),
    WordSize = erlang:system_info(wordsize),
    {TotalMemory, TotalEntries} = lists:foldl(
        fun(Index, {MemAcc, EntryAcc}) ->
            TableName = presence_cache_shard:table_name(Index),
            Mem = safe_ets_info(TableName, memory, 0),
            Size = safe_ets_info(TableName, size, 0),
            {MemAcc + (Mem * WordSize), EntryAcc + Size}
        end,
        {0, 0},
        lists:seq(0, Count - 1)
    ),
    {ok, #{memory_bytes => TotalMemory, entry_count => TotalEntries}}.

-spec find_by_ref(reference(), #{non_neg_integer() => shard()}) ->
    {ok, non_neg_integer()} | not_found.
find_by_ref(Ref, Shards) ->
    shard_utils:find_shard_by_ref(Ref, Shards).

-spec start(non_neg_integer()) -> {ok, shard()} | {error, term()}.
start(Index) ->
    case presence_cache_shard:start_link(Index) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            {ok, #{pid => Pid, ref => Ref}};
        Error ->
            Error
    end.

-spec restart_state(non_neg_integer(), state()) -> state().
restart_state(Index, State) ->
    {_Shard, NewState} = restart(Index, State),
    NewState.

-spec restart(non_neg_integer(), state()) -> {shard(), state()}.
restart(Index, State) ->
    case start(Index) of
        {ok, Shard} ->
            Shards = maps:get(shards, State),
            {Shard, State#{shards := Shards#{Index => Shard}}};
        {error, _Reason} ->
            Dummy = #{pid => spawn(fun dummy_shard/0), ref => make_ref()},
            {Dummy, State}
    end.

-spec dummy_shard() -> ok.
dummy_shard() ->
    receive
        stop -> ok
    after infinity ->
        ok
    end.

-spec ensure_shard(term(), state()) -> {non_neg_integer(), state()}.
ensure_shard(Key, State) ->
    Count = maps:get(shard_count, State),
    Index = presence_cache_bulk:select_shard(Key, Count),
    ensure_shard_for_index(Index, State).

-spec ensure_shard_for_index(non_neg_integer(), state()) -> {non_neg_integer(), state()}.
ensure_shard_for_index(Index, State) ->
    Shards = maps:get(shards, State),
    case maps:get(Index, Shards, undefined) of
        undefined -> restart_missing(Index, State);
        #{pid := Pid} -> ensure_live(Index, Pid, State)
    end.

-spec restart_missing(non_neg_integer(), state()) -> {non_neg_integer(), state()}.
restart_missing(Index, State) ->
    {_Shard, NewState} = restart(Index, State),
    {Index, NewState}.

-spec ensure_live(non_neg_integer(), pid(), state()) -> {non_neg_integer(), state()}.
ensure_live(Index, Pid, State) ->
    case process_liveness:is_alive(Pid) of
        true -> {Index, State};
        false -> restart_missing(Index, State)
    end.

-spec call_shard(non_neg_integer(), term(), state()) -> {term(), state()}.
call_shard(Index, Request, State) ->
    #{pid := Pid} = maps:get(Index, maps:get(shards, State)),
    Reply = safe_call_shard(Pid, Request),
    {Reply, State}.

-spec safe_call_shard(pid(), term()) -> term().
safe_call_shard(Pid, Request) ->
    try gen_server:call(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        Reply -> Reply
    catch
        error:_ -> error_reply(Request);
        exit:_ -> error_reply(Request)
    end.

-spec merge_bulk_group({non_neg_integer(), [integer()]}, {#{integer() => map()}, state()}) ->
    {#{integer() => map()}, state()}.
merge_bulk_group({Index, Ids}, {AccMap, AccState}) ->
    {SafeIndex, State1} = ensure_shard_for_index(Index, AccState),
    {Reply, State2} = call_shard(SafeIndex, {bulk_get_map, Ids}, State1),
    {merge_bulk_reply(Reply, AccMap), State2}.

-spec merge_snapshot_index(non_neg_integer(), {#{integer() => map()}, state()}) ->
    {#{integer() => map()}, state()}.
merge_snapshot_index(Index, {AccMap, AccState}) ->
    {SafeIndex, State1} = ensure_shard_for_index(Index, AccState),
    {Reply, State2} = call_shard(SafeIndex, snapshot, State1),
    {merge_snapshot_reply(Reply, AccMap), State2}.

-spec merge_bulk_reply(term(), #{integer() => map()}) -> #{integer() => map()}.
merge_bulk_reply(ReplyMap, AccMap) when is_map(ReplyMap) ->
    maps:merge(AccMap, presence_cache_bulk:sanitize_presence_map(ReplyMap));
merge_bulk_reply(_, AccMap) ->
    AccMap.

-spec merge_snapshot_reply(term(), #{integer() => map()}) -> #{integer() => map()}.
merge_snapshot_reply(Snapshot, AccMap) when is_map(Snapshot) ->
    maps:merge(AccMap, Snapshot);
merge_snapshot_reply(_, AccMap) ->
    AccMap.

-spec error_reply(term()) -> term().
error_reply({put, _, _}) -> {error, unavailable};
error_reply({delete, _}) -> {error, unavailable};
error_reply({get, _}) -> not_found;
error_reply(_) -> {error, unavailable}.

-spec normalize_bulk_groups([{non_neg_integer(), [term()]}]) ->
    [{non_neg_integer(), [integer()]}].
normalize_bulk_groups(Groups) ->
    [
        {Index, [UserId || UserId <- Ids, is_integer(UserId)]}
     || {Index, Ids} <- Groups
    ].

-spec safe_ets_info(atom(), memory | size, non_neg_integer()) -> non_neg_integer().
safe_ets_info(TableName, InfoKey, Default) ->
    case ets:info(TableName, InfoKey) of
        undefined -> Default;
        Value -> Value
    end.

-spec safe_stop(pid()) -> ok.
safe_stop(Pid) ->
    try gen_server:stop(Pid, shutdown, 5000) of
        _ -> ok
    catch
        error:_ -> ok;
        exit:_ -> ok
    end.
