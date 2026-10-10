%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_counts_cache).
-typing([eqwalizer]).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-compile({no_auto_import, [get/1]}).

-export([
    start_link/0,
    update/3,
    get/1,
    bulk_get/1,
    delete/1,
    rebalance/0,
    rebalance_async/0,
    pending_handoff_count/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type guild_id() :: integer().
-type counts() :: {non_neg_integer(), non_neg_integer()}.
-type shard() :: #{pid := pid(), ref := reference()}.
-type pending_operation() :: {upsert, counts()} | delete.
-type handoff_success_action() :: keep_local | delete_local | delete_local_if_remote_owner.
-type pending_handoffs() :: #{guild_id() => pending_operation()}.
-type state() :: #{
    shards := #{non_neg_integer() => shard()},
    shard_count := pos_integer(),
    pending_handoffs := pending_handoffs(),
    rebalance_retry_timer := reference() | undefined
}.

-spec start_link() -> {ok, pid()} | {error, term()} | ignore.
start_link() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            {ok, Pid};
        undefined ->
            start_new()
    end.

-spec update(guild_id(), non_neg_integer(), non_neg_integer()) -> ok.
update(GuildId, MemberCount, OnlineCount) when
    is_integer(GuildId),
    is_integer(MemberCount),
    MemberCount >= 0,
    is_integer(OnlineCount),
    OnlineCount >= 0
->
    _ = guild_counts_cache_query:safe_owner_call(
        GuildId, {update, GuildId, MemberCount, OnlineCount}, ok
    ),
    ok.

-spec get(guild_id()) -> {ok, non_neg_integer(), non_neg_integer()} | miss.
get(GuildId) when is_integer(GuildId) ->
    guild_counts_cache_remote:normalize_get_reply(
        guild_counts_cache_query:safe_owner_call(GuildId, {get, GuildId}, miss)
    ).

-spec bulk_get([guild_id()]) -> #{guild_id() => counts()}.
bulk_get(GuildIds) when is_list(GuildIds) ->
    guild_counts_cache_query:safe_bulk_get(GuildIds).

-spec delete(guild_id()) -> ok.
delete(GuildId) when is_integer(GuildId) ->
    _ = guild_counts_cache_query:safe_owner_call(GuildId, {delete, GuildId}, ok),
    ok.

-spec rebalance_async() -> ok.
rebalance_async() ->
    _ = guild_counts_cache_query:safe_local_cast(rebalance),
    ok.

-spec rebalance() -> ok.
rebalance() ->
    _ = guild_counts_cache_query:safe_local_call(rebalance, ok),
    ok.

-spec pending_handoff_count() -> non_neg_integer().
pending_handoff_count() ->
    case guild_counts_cache_query:safe_local_call(pending_handoff_count, 0) of
        Count when is_integer(Count), Count >= 0 -> Count;
        _ -> 0
    end.

-spec init([]) -> {ok, state(), hibernate}.
init([]) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 0),
    {ShardCount, _Source} = guild_counts_cache_shard_mgmt:determine_shard_count(),
    Shards = guild_counts_cache_shard_mgmt:start_shards(ShardCount, #{}),
    {ok,
        #{
            shards => Shards,
            shard_count => ShardCount,
            pending_handoffs => #{},
            rebalance_retry_timer => undefined
        },
        hibernate}.

-spec handle_call(term(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({update, GuildId, MC, OC}, _From, State) ->
    handle_update(require_guild_id(GuildId), require_count(MC), require_count(OC), State);
handle_call({update_local, GuildId, MC, OC}, _From, State) ->
    handle_update_local(require_guild_id(GuildId), require_count(MC), require_count(OC), State);
handle_call({delete, GuildId}, _From, State) ->
    handle_delete(require_guild_id(GuildId), State);
handle_call({delete_local, GuildId}, _From, State) ->
    handle_delete_local(require_guild_id(GuildId), State);
handle_call({get, GuildId}, From, State) ->
    handle_get(require_guild_id(GuildId), From, State);
handle_call({get_local, GuildId}, _From, State) ->
    {Reply, NewState} = guild_counts_cache_query:get_local(require_guild_id(GuildId), State),
    {reply, Reply, NewState};
handle_call({bulk_get, GuildIds}, From, State) ->
    guild_counts_cache_query:handle_bulk_get_call(require_guild_ids(GuildIds), From, State);
handle_call({bulk_get_local, GuildIds}, _From, State) ->
    {Reply, NewState} = guild_counts_cache_query:bulk_get_local(
        require_guild_ids(GuildIds), State
    ),
    {reply, Reply, NewState};
handle_call(pending_handoff_count, _From, State) ->
    {reply, guild_counts_cache_remote:pending_handoff_count_from_state(State), State};
handle_call(rebalance, _From, State) ->
    {reply, ok, guild_counts_cache_remote:rebalance_ownership(State)};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({remote_handoff_result, GuildId, Op, Result}, State) ->
    State1 = guild_counts_cache_remote:handle_remote_handoff_result(
        require_guild_id(GuildId), require_pending_operation(Op), keep_local, Result, State
    ),
    {noreply, State1};
handle_cast({remote_handoff_result, GuildId, Op, Action, Result}, State) ->
    State1 = guild_counts_cache_remote:handle_remote_handoff_result(
        require_guild_id(GuildId),
        require_pending_operation(Op),
        require_handoff_success_action(Action),
        Result,
        State
    ),
    {noreply, State1};
handle_cast(rebalance, State) ->
    {noreply, guild_counts_cache_remote:rebalance_ownership(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) when is_reference(Ref) ->
    Shards = maps:get(shards, State),
    case guild_counts_cache_shard_mgmt:find_shard_by_ref(Ref, Shards) of
        {ok, Index} -> restart_shard(Index, State);
        not_found -> {noreply, State}
    end;
handle_info({'EXIT', Pid, _Reason}, State) when is_pid(Pid) ->
    Shards = maps:get(shards, State),
    case guild_counts_cache_shard_mgmt:find_shard_by_pid(Pid, Shards) of
        {ok, Index} -> restart_shard(Index, State);
        not_found -> {noreply, State}
    end;
handle_info({timeout, TimerRef, rebalance_retry}, State) ->
    case guild_counts_cache_shard_mgmt:rebalance_retry_timer_ref(State) of
        TimerRef ->
            State1 = guild_counts_cache_shard_mgmt:set_rebalance_retry_timer(undefined, State),
            {noreply, guild_counts_cache_remote:rebalance_ownership(State1)};
        _ ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    State1 = guild_counts_cache_shard_mgmt:cancel_rebalance_retry_timer(State),
    Shards = maps:get(shards, State1),
    lists:foreach(
        fun stop_shard/1,
        maps:values(Shards)
    ),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    Upgraded = guild_counts_cache_shard_mgmt:ensure_state_fields(State),
    {ok, guild_counts_cache_shard_mgmt:refresh_rebalance_retry_timer(Upgraded)}.

-spec start_new() -> {ok, pid()} | {error, term()} | ignore.
start_new() ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {error, {already_started, Pid}} when is_pid(Pid) -> {ok, Pid};
        Result -> Result
    end.

-spec stop_shard(shard()) -> ok.
stop_shard(Shard) ->
    Pid = maps:get(pid, Shard),
    try
        gen_server:stop(Pid, shutdown, 5000)
    catch
        exit:_ -> ok
    end.

-spec require_guild_id(term()) -> guild_id().
require_guild_id(GuildId) when is_integer(GuildId) ->
    GuildId;
require_guild_id(_) ->
    error(badarg).

-spec require_guild_ids(term()) -> [guild_id()].
require_guild_ids(GuildIds) when is_list(GuildIds) ->
    [require_guild_id(GuildId) || GuildId <- GuildIds];
require_guild_ids(_) ->
    error(badarg).

-spec require_count(term()) -> non_neg_integer().
require_count(Count) when is_integer(Count), Count >= 0 ->
    Count;
require_count(_) ->
    error(badarg).

-spec require_pending_operation(term()) -> pending_operation().
require_pending_operation(Operation) ->
    case guild_counts_cache_remote:normalize_pending_operation(Operation) of
        invalid -> error(badarg);
        Normalized -> Normalized
    end.

-spec require_handoff_success_action(term()) -> handoff_success_action().
require_handoff_success_action(keep_local) ->
    keep_local;
require_handoff_success_action(delete_local) ->
    delete_local;
require_handoff_success_action(delete_local_if_remote_owner) ->
    delete_local_if_remote_owner;
require_handoff_success_action(_) ->
    error(badarg).

-spec restart_shard(non_neg_integer(), state()) -> {noreply, state()}.
restart_shard(Index, State) ->
    {_Shard, NewState} = guild_counts_cache_shard_mgmt:restart_shard(Index, State),
    {noreply, NewState}.

-spec handle_update(guild_id(), non_neg_integer(), non_neg_integer(), state()) ->
    {reply, ok, state()}.
handle_update(GuildId, MC, OC, State) ->
    OwnerNode = guild_counts_cache_query:resolve_owner_node(GuildId),
    case OwnerNode of
        LocalNode when LocalNode =:= node() ->
            {ok, S1} = guild_counts_cache_query:update_local(GuildId, MC, OC, State),
            S2 = guild_counts_cache_remote:clear_pending_handoff(GuildId, S1),
            {reply, ok, guild_counts_cache_shard_mgmt:refresh_rebalance_retry_timer(S2)};
        unavailable ->
            S1 = guild_counts_cache_remote:enqueue_pending_upsert(GuildId, MC, OC, State),
            {reply, ok, guild_counts_cache_shard_mgmt:refresh_rebalance_retry_timer(S1)};
        RemoteNode when is_atom(RemoteNode) ->
            Op = {upsert, {MC, OC}},
            S1 = guild_counts_cache_remote:enqueue_pending_upsert(GuildId, MC, OC, State),
            guild_counts_cache_remote:start_remote_pending_handoff(
                RemoteNode, GuildId, Op, {update_local, GuildId, MC, OC}
            ),
            {reply, ok, guild_counts_cache_shard_mgmt:refresh_rebalance_retry_timer(S1)}
    end.

-spec handle_update_local(guild_id(), non_neg_integer(), non_neg_integer(), state()) ->
    {reply, ok, state()}.
handle_update_local(GuildId, MC, OC, State) ->
    {ok, S1} = guild_counts_cache_query:update_local(GuildId, MC, OC, State),
    S2 = guild_counts_cache_remote:clear_pending_handoff(GuildId, S1),
    {reply, ok, guild_counts_cache_shard_mgmt:refresh_rebalance_retry_timer(S2)}.

-spec handle_delete(guild_id(), state()) -> {reply, ok, state()}.
handle_delete(GuildId, State) ->
    OwnerNode = guild_counts_cache_query:resolve_owner_node(GuildId),
    case OwnerNode of
        LocalNode when LocalNode =:= node() ->
            {ok, S1} = guild_counts_cache_query:delete_local(GuildId, State),
            S2 = guild_counts_cache_remote:clear_pending_handoff(GuildId, S1),
            {reply, ok, guild_counts_cache_shard_mgmt:refresh_rebalance_retry_timer(S2)};
        unavailable ->
            S1 = guild_counts_cache_remote:enqueue_pending_delete(GuildId, State),
            {reply, ok, guild_counts_cache_shard_mgmt:refresh_rebalance_retry_timer(S1)};
        RemoteNode when is_atom(RemoteNode) ->
            S1 = guild_counts_cache_remote:enqueue_pending_delete(GuildId, State),
            guild_counts_cache_remote:start_remote_pending_handoff(
                RemoteNode, GuildId, delete, {delete_local, GuildId}
            ),
            {reply, ok, guild_counts_cache_shard_mgmt:refresh_rebalance_retry_timer(S1)}
    end.

-spec handle_delete_local(guild_id(), state()) -> {reply, ok, state()}.
handle_delete_local(GuildId, State) ->
    {ok, S1} = guild_counts_cache_query:delete_local(GuildId, State),
    S2 = guild_counts_cache_remote:clear_pending_handoff(GuildId, S1),
    {reply, ok, guild_counts_cache_shard_mgmt:refresh_rebalance_retry_timer(S2)}.

-spec handle_get(guild_id(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_get(GuildId, From, State) ->
    OwnerNode = guild_counts_cache_query:resolve_owner_node(GuildId),
    case OwnerNode of
        LocalNode when LocalNode =:= node() ->
            {Reply, NewState} = guild_counts_cache_query:get_local(GuildId, State),
            {reply, Reply, NewState};
        unavailable ->
            {reply, miss, State};
        RemoteNode when is_atom(RemoteNode) ->
            guild_counts_cache_remote:start_remote_get_reply(From, RemoteNode, GuildId),
            {noreply, State}
    end.
