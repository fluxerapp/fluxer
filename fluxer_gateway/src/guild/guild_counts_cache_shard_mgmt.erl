%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_counts_cache_shard_mgmt).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    start_shards/2,
    start_shard/1,
    restart_shard/2,
    find_shard_by_ref/2,
    find_shard_by_pid/2,
    ensure_shard/2,
    ensure_shard_for_index/2,
    select_shard/2,
    forward_call/3,
    call_shard/3,
    shard_pid/2,
    determine_shard_count/0
]).

-export([
    rebalance_retry_timer_ref/1,
    set_rebalance_retry_timer/2,
    refresh_rebalance_retry_timer/1,
    ensure_rebalance_retry_timer/1,
    cancel_rebalance_retry_timer/1,
    ensure_state_fields/1
]).

-type shard() :: #{pid := pid(), ref := reference()}.

-export_type([shard/0]).

-define(PENDING_HANDOFF_RETRY_MS, 1000).

-spec determine_shard_count() -> {pos_integer(), atom() | auto}.
determine_shard_count() ->
    clustered_ets_cache:determine_shard_count([guild_counts_cache_shards, guild_shards]).

-spec start_shards(pos_integer(), #{}) -> #{non_neg_integer() => shard()}.
start_shards(Count, Acc) ->
    lists:foldl(
        fun start_shard_into/2,
        Acc,
        lists:seq(0, Count - 1)
    ).

-spec start_shard_into(non_neg_integer(), #{non_neg_integer() => shard()}) ->
    #{non_neg_integer() => shard()}.
start_shard_into(Index, MapAcc) ->
    case start_shard(Index) of
        {ok, Shard} -> MapAcc#{Index => Shard};
        {error, _Reason} -> MapAcc
    end.

-spec start_shard(non_neg_integer()) -> {ok, shard()} | {error, term()}.
start_shard(Index) ->
    case guild_counts_cache_shard:start_link(Index) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            {ok, #{pid => Pid, ref => Ref}};
        ignore ->
            {error, ignore};
        Error ->
            Error
    end.

-spec restart_shard(non_neg_integer(), map()) -> {shard(), map()}.
restart_shard(Index, State) ->
    case start_shard(Index) of
        {ok, Shard} ->
            store_shard(Index, Shard, State);
        {error, _Reason} ->
            Dummy = make_dummy_shard(),
            store_shard(Index, Dummy, State)
    end.

-spec store_shard(non_neg_integer(), shard(), map()) -> {shard(), map()}.
store_shard(Index, Shard, State) ->
    Shards = maps:get(shards, State),
    {Shard, State#{shards := Shards#{Index => Shard}}}.

-spec make_dummy_shard() -> shard().
make_dummy_shard() ->
    DummyPid = spawn(fun() -> ok = gateway_retry_timer:wait(50) end),
    #{pid => DummyPid, ref => make_ref()}.

-spec forward_call(term(), term(), map()) -> {term(), map()}.
forward_call(Key, Request, State) ->
    {Index, State1} = ensure_shard(Key, State),
    call_shard(Index, Request, State1).

-spec call_shard(non_neg_integer(), term(), map()) -> {term(), map()}.
call_shard(Index, Request, State) ->
    Pid = shard_pid(Index, State),
    try gen_server:call(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        Reply -> {Reply, State}
    catch
        exit:_ ->
            {_Shard, State1} = restart_shard(Index, State),
            call_shard_after_restart(Index, Request, State1)
    end.

-spec call_shard_after_restart(non_neg_integer(), term(), map()) -> {term(), map()}.
call_shard_after_restart(Index, Request, State) ->
    Pid = shard_pid(Index, State),
    try gen_server:call(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        Reply -> {Reply, State}
    catch
        exit:_ -> {error, State}
    end.

-spec shard_pid(non_neg_integer(), map()) -> pid().
shard_pid(Index, State) ->
    Shards = maps:get(shards, State),
    Shard = maps:get(Index, Shards),
    maps:get(pid, Shard).

-spec ensure_shard(term(), map()) -> {non_neg_integer(), map()}.
ensure_shard(Key, State) ->
    Count = maps:get(shard_count, State),
    Index = select_shard(Key, Count),
    ensure_shard_for_index(Index, State).

-spec ensure_shard_for_index(non_neg_integer(), map()) -> {non_neg_integer(), map()}.
ensure_shard_for_index(Index, State) ->
    Shards = maps:get(shards, State),
    case maps:get(Index, Shards, undefined) of
        undefined ->
            {_Shard, NewState} = restart_shard(Index, State),
            {Index, NewState};
        #{pid := Pid} when is_pid(Pid) ->
            handle_shard_liveness(Index, Pid, State)
    end.

-spec handle_shard_liveness(non_neg_integer(), pid(), map()) ->
    {non_neg_integer(), map()}.
handle_shard_liveness(Index, Pid, State) ->
    case process_liveness:is_alive(Pid) of
        true ->
            {Index, State};
        false ->
            {_Shard, NewState} = restart_shard(Index, State),
            {Index, NewState}
    end.

-spec select_shard(term(), pos_integer()) -> non_neg_integer().
select_shard(Key, Count) when Count > 0 ->
    clustered_ets_cache:select_shard(Key, Count).

-spec find_shard_by_ref(reference(), #{non_neg_integer() => shard()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_ref(Ref, Shards) ->
    shard_utils:find_shard_by_ref(Ref, Shards).

-spec find_shard_by_pid(pid(), #{non_neg_integer() => shard()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_pid(Pid, Shards) ->
    shard_utils:find_shard_by_pid(Pid, Shards).

-spec rebalance_retry_timer_ref(map()) -> reference() | undefined.
rebalance_retry_timer_ref(State) ->
    case maps:get(rebalance_retry_timer, State, undefined) of
        TimerRef when is_reference(TimerRef) -> TimerRef;
        _ -> undefined
    end.

-spec set_rebalance_retry_timer(reference() | undefined, map()) -> map().
set_rebalance_retry_timer(TimerRef, State) ->
    State#{rebalance_retry_timer => TimerRef}.

-spec ensure_rebalance_retry_timer(map()) -> map().
ensure_rebalance_retry_timer(State) ->
    case rebalance_retry_timer_ref(State) of
        TimerRef when is_reference(TimerRef) -> State;
        undefined ->
            NewRef = erlang:start_timer(?PENDING_HANDOFF_RETRY_MS, self(), rebalance_retry),
            set_rebalance_retry_timer(NewRef, State)
    end.

-spec cancel_rebalance_retry_timer(map()) -> map().
cancel_rebalance_retry_timer(State) ->
    case rebalance_retry_timer_ref(State) of
        TimerRef when is_reference(TimerRef) ->
            _ = erlang:cancel_timer(TimerRef),
            set_rebalance_retry_timer(undefined, State);
        undefined ->
            set_rebalance_retry_timer(undefined, State)
    end.

-spec refresh_rebalance_retry_timer(map()) -> map().
refresh_rebalance_retry_timer(State) ->
    case guild_counts_cache_remote:pending_handoff_count_from_state(State) of
        0 -> cancel_rebalance_retry_timer(State);
        _Count -> ensure_rebalance_retry_timer(State)
    end.

-spec ensure_state_fields(map()) -> map().
ensure_state_fields(State) when is_map(State) ->
    Pending = guild_counts_cache_remote:pending_handoffs(State),
    TimerRef = rebalance_retry_timer_ref(State),
    State1 = guild_counts_cache_remote:set_pending_handoffs(Pending, State),
    set_rebalance_retry_timer(TimerRef, State1).
