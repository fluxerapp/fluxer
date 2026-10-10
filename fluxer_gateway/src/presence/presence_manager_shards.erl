%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_manager_shards).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    determine_count/0,
    start/1,
    restart/2,
    forward_call/3,
    aggregate_counts/2,
    select/2,
    ensure_table/0,
    delete_table/0,
    sync_table/1,
    put_pid/2,
    clear_pid/1,
    pid_from_table/1,
    find_by_ref/2,
    find_by_pid/2,
    default_count/0
]).

-export_type([user_id/0, shard/0, state/0]).

-define(SHARD_TABLE, presence_manager_shard_table).
-define(FORWARD_CALL_MAX_ATTEMPTS, 3).

-type user_id() :: integer().
-type shard() :: #{pid := pid(), ref := reference()}.
-type state() :: #{
    shards := #{non_neg_integer() => shard()}, shard_count := pos_integer(), _ => _
}.

-spec determine_count() -> {pos_integer(), configured | auto}.
determine_count() ->
    shard_utils:determine_shard_count(presence_shards).

-spec start(non_neg_integer()) -> {ok, shard()} | {error, term()}.
start(Index) ->
    case presence_manager_shard:start_link(Index) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            put_pid(Index, Pid),
            {ok, #{pid => Pid, ref => Ref}};
        Error ->
            Error
    end.

-spec restart(non_neg_integer(), state()) -> {shard(), state()}.
restart(Index, State) ->
    case start(Index) of
        {ok, Shard} ->
            Shards = maps:get(shards, State),
            Updated = State#{shards := Shards#{Index => Shard}},
            sync_table(Updated),
            {Shard, Updated};
        {error, _Reason} ->
            clear_pid(Index),
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

-spec forward_call(user_id(), term(), state()) -> {term(), state()}.
forward_call(Key, Request, State) ->
    forward_call(Key, Request, State, ?FORWARD_CALL_MAX_ATTEMPTS).

-spec forward_call(user_id(), term(), state(), non_neg_integer()) -> {term(), state()}.
forward_call(_Key, _Request, State, 0) ->
    {{error, unavailable}, State};
forward_call(Key, Request, State, Attempts) ->
    {ShardIndex, State1} = ensure_shard(Key, State),
    #{pid := Pid} = maps:get(ShardIndex, maps:get(shards, State1)),
    case shard_utils:safe_gen_call_wrapped(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {exit, _} ->
            {_ShardEntry, State2} = restart(ShardIndex, State1),
            forward_call(Key, Request, State2, Attempts - 1);
        {ok, Reply} ->
            {Reply, State1}
    end.

-spec aggregate_counts(term(), state()) -> {non_neg_integer(), state()}.
aggregate_counts(Request, State) ->
    Shards = maps:get(shards, State),
    Results = [count_shard(Shard, Request) || Shard <- maps:values(Shards)],
    {lists:sum(Results), State}.

-spec select(user_id(), pos_integer()) -> non_neg_integer().
select(Key, Count) when Count > 0 ->
    rendezvous_router:select(Key, Count).

-spec ensure_table() -> ok.
ensure_table() ->
    case ets:whereis(?SHARD_TABLE) of
        undefined ->
            _ = ets:new(?SHARD_TABLE, [named_table, public, set, {read_concurrency, true}]),
            ok;
        _ ->
            ok
    end.

-spec delete_table() -> ok.
delete_table() ->
    try ets:delete(?SHARD_TABLE) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec sync_table(state()) -> ok.
sync_table(State) ->
    ensure_table(),
    _ = ets:delete_all_objects(?SHARD_TABLE),
    ShardCount = maps:get(shard_count, State),
    ets:insert(?SHARD_TABLE, {shard_count, ShardCount}),
    maps:foreach(
        fun(Index, #{pid := Pid}) -> put_pid(Index, Pid) end,
        maps:get(shards, State)
    ),
    ok.

-spec put_pid(non_neg_integer(), pid()) -> ok.
put_pid(Index, Pid) ->
    ensure_table(),
    ets:insert(?SHARD_TABLE, {{shard_pid, Index}, Pid}),
    ok.

-spec clear_pid(non_neg_integer()) -> ok.
clear_pid(Index) ->
    try ets:delete(?SHARD_TABLE, {shard_pid, Index}) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec pid_from_table(user_id()) -> {ok, pid()} | error.
pid_from_table(Key) ->
    try lookup_shard_pid(Key) of
        Reply -> Reply
    catch
        error:badarg -> error
    end.

-spec find_by_ref(reference(), #{non_neg_integer() => shard()}) ->
    {ok, non_neg_integer()} | not_found.
find_by_ref(Ref, Shards) ->
    shard_utils:find_shard_by_ref(Ref, Shards).

-spec find_by_pid(pid(), #{non_neg_integer() => shard()}) ->
    {ok, non_neg_integer()} | not_found.
find_by_pid(Pid, Shards) ->
    shard_utils:find_shard_by_pid(Pid, Shards).

-spec default_count() -> pos_integer().
default_count() ->
    clustered_ets_cache:default_shard_count().

-spec ensure_shard(user_id(), state()) -> {non_neg_integer(), state()}.
ensure_shard(Key, State) ->
    Count = maps:get(shard_count, State),
    Shards = maps:get(shards, State),
    Index = select(Key, Count),
    case maps:get(Index, Shards, undefined) of
        undefined -> restart_missing_shard(Index, State);
        #{pid := Pid} -> ensure_live_shard(Index, Pid, State)
    end.

-spec restart_missing_shard(non_neg_integer(), state()) -> {non_neg_integer(), state()}.
restart_missing_shard(Index, State) ->
    {_ShardEntry, NewState} = restart(Index, State),
    {Index, NewState}.

-spec ensure_live_shard(non_neg_integer(), pid(), state()) -> {non_neg_integer(), state()}.
ensure_live_shard(Index, Pid, State) ->
    case process_liveness:is_alive(Pid) of
        true -> {Index, State};
        false -> restart_missing_shard(Index, State)
    end.

-spec count_shard(shard(), term()) -> non_neg_integer().
count_shard(#{pid := Pid}, Request) ->
    case shard_utils:safe_gen_call_wrapped(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {ok, {ok, Count}} when is_integer(Count), Count >= 0 -> Count;
        _ -> 0
    end.

-spec lookup_shard_pid(user_id()) -> {ok, pid()} | error.
lookup_shard_pid(Key) ->
    case ets:lookup(?SHARD_TABLE, shard_count) of
        [{shard_count, ShardCount}] when is_integer(ShardCount), ShardCount > 0 ->
            lookup_shard_pid(Key, ShardCount);
        _ ->
            error
    end.

-spec lookup_shard_pid(user_id(), pos_integer()) -> {ok, pid()} | error.
lookup_shard_pid(Key, ShardCount) ->
    Index = select(Key, ShardCount),
    case ets:lookup(?SHARD_TABLE, {shard_pid, Index}) of
        [{{shard_pid, Index}, Pid}] when is_pid(Pid) -> live_pid_reply(Pid);
        _ -> error
    end.

-spec live_pid_reply(pid()) -> {ok, pid()} | error.
live_pid_reply(Pid) ->
    case process_liveness:is_alive(Pid) of
        true -> {ok, Pid};
        false -> error
    end.
