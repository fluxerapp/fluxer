%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager).
-typing([eqwalizer]).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    start_link/0,
    start_or_lookup/1,
    start_or_lookup/2,
    lookup/1,
    lookup/2,
    local_guild_count/0,
    handoff_to_topology/1,
    call_via_manager/2,
    call_via_manager_local/2
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type guild_id() :: integer().
-type shard_map() :: #{pid := pid(), ref := reference()}.
-type handoff_result() :: #{attempted := non_neg_integer(), handed_off := non_neg_integer()}.
-type state() :: #{
    shards := #{non_neg_integer() => shard_map()},
    shard_count := pos_integer()
}.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start_or_lookup(guild_id()) -> {ok, pid()} | {error, term()}.
start_or_lookup(GuildId) ->
    start_or_lookup(GuildId, ?DEFAULT_GEN_SERVER_TIMEOUT).

-spec start_or_lookup(guild_id(), pos_integer()) -> {ok, pid()} | {error, term()}.
start_or_lookup(GuildId, Timeout) ->
    normalize_pid_reply(call_via_manager({start_or_lookup, GuildId}, Timeout)).

-spec lookup(guild_id()) -> {ok, pid()} | {error, term()}.
lookup(GuildId) ->
    lookup(GuildId, ?DEFAULT_GEN_SERVER_TIMEOUT).

-spec lookup(guild_id(), pos_integer()) -> {ok, pid()} | {error, term()}.
lookup(GuildId, Timeout) ->
    normalize_pid_reply(call_via_manager({lookup, GuildId}, Timeout)).

-spec local_guild_count() -> non_neg_integer().
local_guild_count() ->
    case call_via_manager_local(get_local_count, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {ok, Count} when is_integer(Count), Count >= 0 -> Count;
        Count when is_integer(Count), Count >= 0 -> Count;
        {error, Reason} -> error({guild_count_unavailable, Reason});
        Other -> error({unexpected_guild_count_reply, Other})
    end.

-spec handoff_to_topology([node()]) -> handoff_result() | {error, term()}.
handoff_to_topology(TargetNodes) ->
    Request = {handoff_to_topology, TargetNodes},
    normalize_handoff_result(call_via_manager_local(Request, ?DEFAULT_GEN_SERVER_TIMEOUT)).

-spec call_via_manager(term(), pos_integer()) -> term().
call_via_manager(Request, Timeout) ->
    guild_manager_router:call_via_manager(Request, Timeout).

-spec call_via_manager_local(term(), pos_integer()) -> term().
call_via_manager_local(Request, Timeout) ->
    guild_manager_router:call_via_manager_local(Request, Timeout).

-spec init(list()) -> {ok, state(), hibernate}.
init([]) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 0),
    process_registry:init(),
    guild_manager_cache:ensure_tables(),
    {ShardCount, _Source} = guild_manager_shards:determine_shard_count(),
    ShardMap = guild_manager_shards:start_shards(ShardCount),
    State = #{shards => ShardMap, shard_count => ShardCount},
    guild_manager_cache:sync_shard_table(State),
    {ok, State, hibernate}.

-spec handle_call(term(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({with_timeout, Request, Timeout}, From, State) ->
    handle_timed_call(Request, require_timeout(Timeout), From, State);
handle_call(Request, From, State) ->
    handle_plain_call(Request, From, State).

-spec handle_plain_call(term(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_plain_call({start_or_lookup, GuildId}, From, State) ->
    forward_guild_call(start_or_lookup, GuildId, From, State);
handle_plain_call({lookup, GuildId}, From, State) ->
    forward_guild_call(lookup, GuildId, From, State);
handle_plain_call({start_transferred, GuildId, TransferState}, From, State) ->
    forward_start_transferred(GuildId, TransferState, From, State);
handle_plain_call({stop_guild, GuildId}, From, State) ->
    forward_guild_call(stop_guild, GuildId, From, State);
handle_plain_call({reload_guild, GuildId}, From, State) ->
    forward_guild_call(reload_guild, GuildId, From, State);
handle_plain_call({shutdown_guild, GuildId}, From, State) ->
    forward_guild_call(shutdown_guild, GuildId, From, State);
handle_plain_call(Request, _From, State) ->
    handle_manager_call(Request, State).

-spec handle_manager_call(term(), state()) -> {reply, term(), state()}.
handle_manager_call({reload_all_guilds, GuildIds}, State) ->
    {Reply, NewState} = guild_manager_router:handle_reload_all(
        require_guild_ids(GuildIds), State
    ),
    {reply, Reply, NewState};
handle_manager_call(get_local_count, State) ->
    {Reply, NewState} = guild_manager_router:aggregate_counts(get_local_count, State),
    {reply, Reply, NewState};
handle_manager_call(get_global_count, State) ->
    {Reply, NewState} = guild_manager_router:aggregate_counts(get_global_count, State),
    {reply, Reply, NewState};
handle_manager_call({handoff_to_topology, TargetNodes}, State) ->
    {Result, NewState} = guild_manager_handoff:perform_handoff_to_topology(
        require_nodes(TargetNodes), State
    ),
    {reply, Result, NewState};
handle_manager_call(_Request, State) ->
    {reply, ok, State}.

-spec handle_timed_call(term(), pos_integer(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_timed_call(Request, Timeout, From, State) ->
    handle_timed_forward_call(Request, Timeout, From, State).

-spec handle_timed_forward_call(term(), pos_integer(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_timed_forward_call({start_or_lookup, GuildId}, Timeout, From, State) ->
    forward_timed_guild_call(start_or_lookup, GuildId, Timeout, From, State);
handle_timed_forward_call({lookup, GuildId}, Timeout, From, State) ->
    forward_timed_guild_call(lookup, GuildId, Timeout, From, State);
handle_timed_forward_call({start_transferred, GuildId, TransferState}, Timeout, From, State) ->
    forward_timed_start_transferred(GuildId, TransferState, Timeout, From, State);
handle_timed_forward_call({stop_guild, GuildId}, Timeout, From, State) ->
    forward_timed_guild_call(stop_guild, GuildId, Timeout, From, State);
handle_timed_forward_call({reload_guild, GuildId}, Timeout, From, State) ->
    forward_timed_guild_call(reload_guild, GuildId, Timeout, From, State);
handle_timed_forward_call({shutdown_guild, GuildId}, Timeout, From, State) ->
    forward_timed_guild_call(shutdown_guild, GuildId, Timeout, From, State);
handle_timed_forward_call(Request, Timeout, From, State) ->
    handle_timed_manager_call(Request, Timeout, From, State).

-spec handle_timed_manager_call(term(), pos_integer(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_timed_manager_call({reload_all_guilds, GuildIds}, _Timeout, _From, State) ->
    {Reply, NewState} = guild_manager_router:handle_reload_all(
        require_guild_ids(GuildIds), State
    ),
    {reply, Reply, NewState};
handle_timed_manager_call(get_local_count, _Timeout, _From, State) ->
    {Reply, NewState} = guild_manager_router:aggregate_counts(get_local_count, State),
    {reply, Reply, NewState};
handle_timed_manager_call(get_global_count, _Timeout, _From, State) ->
    {Reply, NewState} = guild_manager_router:aggregate_counts(get_global_count, State),
    {reply, Reply, NewState};
handle_timed_manager_call({handoff_to_topology, TargetNodes}, _Timeout, From, State) ->
    Targets = require_nodes(TargetNodes),
    async_handoff_reply(
        self(),
        From,
        fun() -> guild_manager_handoff:perform_handoff_to_topology(Targets, State) end
    ),
    {noreply, State};
handle_timed_manager_call(Request, _Timeout, _From, State) ->
    {reply, {error, {unsupported_timed_call, Request}}, State}.

-spec async_handoff_reply(pid(), gen_server:from(), fun(() -> {term(), state()})) -> ok.
async_handoff_reply(Manager, From, Fun) ->
    proc_lib:spawn(fun() ->
        erlang:process_flag(fullsweep_after, 0),
        Reply = run_async_handoff(Manager, Fun),
        gen_server:reply(From, Reply)
    end),
    ok.

-spec run_async_handoff(pid(), fun(() -> {term(), state()})) -> term().
run_async_handoff(Manager, Fun) ->
    try
        {Result, NewState} = Fun(),
        sync_handoff_shards(Manager, NewState),
        Result
    catch
        Class:Reason:Stacktrace ->
            logger:error(
                "guild_manager_handoff_failed: class=~p reason=~p stacktrace=~p",
                [Class, Reason, Stacktrace]
            ),
            {error, {handoff_failed, Reason}}
    end.

-spec sync_handoff_shards(pid(), state()) -> ok.
sync_handoff_shards(Manager, NewState) ->
    case NewState of
        #{shards := Shards} when is_map(Shards) ->
            gen_server:cast(Manager, {handoff_shards_sync, Shards}),
            ok;
        _ ->
            ok
    end.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({handoff_shards_sync, WorkerShards}, State) when is_map(WorkerShards) ->
    {noreply, reconcile_handoff_shards(eqwalizer:dynamic_cast(WorkerShards), State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec reconcile_handoff_shards(#{non_neg_integer() => shard_map()}, state()) -> state().
reconcile_handoff_shards(WorkerShards, State) ->
    Current = maps:get(shards, State),
    Reconciled = maps:fold(fun adopt_worker_shard/3, Current, WorkerShards),
    NewState = State#{shards => Reconciled},
    guild_manager_cache:sync_shard_table(NewState),
    NewState.

-spec adopt_worker_shard(
    non_neg_integer(), shard_map(), #{non_neg_integer() => shard_map()}
) -> #{non_neg_integer() => shard_map()}.
adopt_worker_shard(Index, WorkerShard, Acc) ->
    case maps:get(Index, Acc, undefined) of
        #{pid := Pid} when is_pid(Pid) ->
            adopt_worker_shard_for_pid(Index, WorkerShard, Acc, Pid);
        _ ->
            Acc#{Index => WorkerShard}
    end.

-spec adopt_worker_shard_for_pid(
    non_neg_integer(), shard_map(), #{non_neg_integer() => shard_map()}, pid()
) -> #{non_neg_integer() => shard_map()}.
adopt_worker_shard_for_pid(Index, WorkerShard, Acc, Pid) ->
    case process_liveness:is_alive(Pid) of
        true -> Acc;
        false -> Acc#{Index => WorkerShard}
    end.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) when is_reference(Ref), is_pid(Pid) ->
    Shards = maps:get(shards, State),
    case guild_manager_shards:find_shard_by_ref(Ref, Shards) of
        {ok, Index} ->
            restart_manager_shard(Index, State);
        not_found ->
            guild_manager_cache:cleanup_guild_from_cache(Pid),
            {noreply, State}
    end;
handle_info({'EXIT', Pid, _Reason}, State) when is_pid(Pid) ->
    Shards = maps:get(shards, State),
    case guild_manager_shards:find_shard_by_pid(Pid, Shards) of
        {ok, Index} ->
            restart_manager_shard(Index, State);
        not_found ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    guild_manager_cache:stop_shards(State),
    guild_manager_cache:delete_tables(),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:process_flag(fullsweep_after, 0),
    erlang:garbage_collect(),
    {ok, State}.

-spec restart_manager_shard(non_neg_integer(), state()) -> {noreply, state()}.
restart_manager_shard(Index, State) ->
    case guild_manager_shards:restart_shard(Index, State) of
        {ok, _Shard, NewState} -> {noreply, NewState};
        {error, NewState} -> {noreply, NewState}
    end.

-spec normalize_pid_reply(term()) -> {ok, pid()} | {error, term()}.
normalize_pid_reply({ok, Pid}) when is_pid(Pid) ->
    {ok, Pid};
normalize_pid_reply({error, _Reason} = Error) ->
    Error;
normalize_pid_reply(Other) ->
    {error, {unexpected_reply, Other}}.

-spec forward_guild_call(atom(), term(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
forward_guild_call(Operation, GuildId0, From, State) ->
    GuildId = require_guild_id(GuildId0),
    guild_manager_router:forward_call(GuildId, {Operation, GuildId}, From, State).

-spec forward_start_transferred(term(), term(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
forward_start_transferred(GuildId0, TransferState, From, State) ->
    GuildId = require_guild_id(GuildId0),
    Request = {start_transferred, GuildId, require_map(TransferState)},
    guild_manager_router:forward_call(GuildId, Request, From, State).

-spec forward_timed_guild_call(atom(), term(), pos_integer(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
forward_timed_guild_call(Operation, GuildId0, Timeout, From, State) ->
    GuildId = require_guild_id(GuildId0),
    guild_manager_router:forward_call(GuildId, {Operation, GuildId}, Timeout, From, State).

-spec forward_timed_start_transferred(
    term(), term(), pos_integer(), gen_server:from(), state()
) ->
    {reply, term(), state()} | {noreply, state()}.
forward_timed_start_transferred(GuildId0, TransferState, Timeout, From, State) ->
    GuildId = require_guild_id(GuildId0),
    Request = {start_transferred, GuildId, require_map(TransferState)},
    guild_manager_router:forward_call(GuildId, Request, Timeout, From, State).

-spec normalize_handoff_result(term()) -> handoff_result() | {error, term()}.
normalize_handoff_result(#{attempted := Attempted, handed_off := HandedOff}) when
    is_integer(Attempted), Attempted >= 0, is_integer(HandedOff), HandedOff >= 0
->
    #{attempted => Attempted, handed_off => HandedOff};
normalize_handoff_result({error, _Reason} = Error) ->
    Error;
normalize_handoff_result(Other) ->
    {error, {unexpected_reply, Other}}.

-spec require_guild_id(term()) -> guild_id().
require_guild_id(GuildId) when is_integer(GuildId) ->
    GuildId;
require_guild_id(GuildId) ->
    erlang:error({bad_guild_id, GuildId}).

-spec require_guild_ids(term()) -> [guild_id()].
require_guild_ids(GuildIds) when is_list(GuildIds) ->
    [require_guild_id(GuildId) || GuildId <- GuildIds];
require_guild_ids(GuildIds) ->
    erlang:error({bad_guild_ids, GuildIds}).

-spec require_timeout(term()) -> pos_integer().
require_timeout(Timeout) when is_integer(Timeout), Timeout > 0 ->
    Timeout;
require_timeout(Timeout) ->
    erlang:error({bad_timeout, Timeout}).

-spec require_node(term()) -> node().
require_node(Node) when is_atom(Node) ->
    Node;
require_node(Node) ->
    erlang:error({bad_node, Node}).

-spec require_nodes(term()) -> [node()].
require_nodes(Nodes) when is_list(Nodes) ->
    [require_node(Node) || Node <- Nodes];
require_nodes(Nodes) ->
    erlang:error({bad_nodes, Nodes}).

-spec require_map(term()) -> map().
require_map(Value) when is_map(Value) ->
    Value;
require_map(Value) ->
    erlang:error({bad_map, Value}).
