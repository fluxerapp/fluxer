%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager_shards).
-typing([eqwalizer]).

-export([
    determine_shard_count/0,
    default_shard_count/0,
    start_shards/1,
    start_shard/1,
    restart_shard/2,
    ensure_shard/2,
    ensure_shard_for_index/2,
    select_shard/2,
    group_ids_by_shard/2,
    find_shard_by_ref/2,
    find_shard_by_pid/2
]).

-type guild_id() :: integer().
-type shard_map() :: #{pid := pid(), ref := reference()}.
-type state() :: #{shards := #{non_neg_integer() => shard_map()}, shard_count := pos_integer()}.

-export_type([guild_id/0, shard_map/0, state/0]).

-spec determine_shard_count() -> {pos_integer(), configured | auto}.
determine_shard_count() ->
    shard_utils:determine_shard_count(guild_shards).

-spec default_shard_count() -> pos_integer().
default_shard_count() ->
    clustered_ets_cache:default_shard_count().

-spec start_shards(pos_integer()) -> #{non_neg_integer() => shard_map()}.
start_shards(Count) ->
    lists:foldl(fun start_shard_into_map/2, #{}, lists:seq(0, Count - 1)).

-spec start_shard(non_neg_integer()) -> {ok, shard_map()} | {error, term()}.
start_shard(Index) ->
    case guild_manager_shard:start_link(Index) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            guild_manager_cache:put_shard_pid(Index, Pid),
            {ok, #{pid => Pid, ref => Ref}};
        {error, _Reason} = Error ->
            Error;
        ignore ->
            {error, ignore}
    end.

-spec restart_shard(non_neg_integer(), state()) ->
    {ok, shard_map(), state()} | {error, state()}.
restart_shard(Index, State) ->
    Shards = maps:get(shards, State),
    case start_shard(Index) of
        {ok, Shard} ->
            Updated = State#{shards => Shards#{Index => Shard}},
            guild_manager_cache:sync_shard_table(Updated),
            {ok, Shard, Updated};
        {error, _Reason} ->
            guild_manager_cache:clear_shard_pid(Index),
            {error, State#{shards => maps:remove(Index, Shards)}}
    end.

-spec ensure_shard(guild_id(), state()) -> {non_neg_integer(), state()}.
ensure_shard(GuildId, State) ->
    Count = maps:get(shard_count, State),
    Index = select_shard(GuildId, Count),
    ensure_shard_for_index(Index, State).

-spec ensure_shard_for_index(non_neg_integer(), state()) -> {non_neg_integer(), state()}.
ensure_shard_for_index(Index, State) ->
    Shards = maps:get(shards, State),
    case maps:get(Index, Shards, undefined) of
        #{pid := Pid} when is_pid(Pid) ->
            ensure_live_shard(Index, Pid, State);
        _ ->
            restart_shard_or_fail(Index, State)
    end.

-spec restart_shard_or_fail(non_neg_integer(), state()) -> {non_neg_integer(), state()}.
restart_shard_or_fail(Index, State) ->
    case restart_shard(Index, State) of
        {ok, _Shard, NewState} -> {Index, NewState};
        {error, NewState} -> error({shard_start_failed, Index, NewState})
    end.

-spec select_shard(guild_id(), pos_integer()) -> non_neg_integer().
select_shard(GuildId, Count) when Count > 0 ->
    rendezvous_router:select(GuildId, Count).

-spec group_ids_by_shard([guild_id()], pos_integer()) -> [{non_neg_integer(), [guild_id()]}].
group_ids_by_shard(GuildIds, ShardCount) ->
    normalize_grouped_ids(rendezvous_router:group_keys(GuildIds, ShardCount)).

-spec normalize_grouped_ids([{non_neg_integer(), [term()]}]) ->
    [{non_neg_integer(), [guild_id()]}].
normalize_grouped_ids(Groups) ->
    [{Index, [GuildId || GuildId <- Ids, is_integer(GuildId)]} || {Index, Ids} <- Groups].

-spec find_shard_by_ref(reference(), #{non_neg_integer() => shard_map()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_ref(Ref, Shards) ->
    shard_utils:find_shard_by_ref(Ref, Shards).

-spec find_shard_by_pid(pid(), #{non_neg_integer() => shard_map()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_pid(Pid, Shards) ->
    shard_utils:find_shard_by_pid(Pid, Shards).

-spec start_shard_into_map(non_neg_integer(), #{non_neg_integer() => shard_map()}) ->
    #{non_neg_integer() => shard_map()}.
start_shard_into_map(Index, Acc) ->
    case start_shard(Index) of
        {ok, Shard} -> Acc#{Index => Shard};
        {error, _Reason} -> Acc
    end.

-spec ensure_live_shard(non_neg_integer(), pid(), state()) -> {non_neg_integer(), state()}.
ensure_live_shard(Index, Pid, State) ->
    case process_liveness:is_alive(Pid) of
        true ->
            {Index, State};
        false ->
            restart_shard_or_fail(Index, State)
    end.
