%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_cache).
-typing([eqwalizer]).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-compile({no_auto_import, [get/1, put/2]}).

-export([
    start_link/0,
    put/2,
    delete/1,
    get/1,
    bulk_get/1,
    get_memory_stats/0,
    pending_handoff_count/0,
    get_pending_handoff_count/0,
    rebalance/0,
    rebalance_async/0,
    handle_nodedown/1,
    handle_nodeup/1,
    trigger_anti_entropy/0,
    generation/0,
    pending_operations_count/0,
    handoff_to_target/1
]).

-export([
    put_local/3,
    delete_local/2,
    local_snapshot/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PENDING_HANDOFF_RETRY_MSG, pending_handoff_retry).
-define(ANTI_ENTROPY_MSG, anti_entropy_tick).

-type state() :: map().

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    normalize_start_link(gen_server:start_link({local, ?MODULE}, ?MODULE, [], [])).

-spec put(integer(), map()) -> ok.
put(UserId, Presence) when is_integer(UserId), is_map(Presence) ->
    case persistent_term:get(presence_noop, false) of
        true -> ok;
        false -> presence_cache_api:cast_owner(UserId, {put, UserId, Presence})
    end.

-spec delete(integer()) -> ok.
delete(UserId) when is_integer(UserId) ->
    case persistent_term:get(presence_noop, false) of
        true -> ok;
        false -> presence_cache_api:cast_owner(UserId, {delete, UserId})
    end.

-spec get(integer()) -> {ok, map()} | not_found.
get(UserId) when is_integer(UserId) ->
    case persistent_term:get(presence_noop, false) of
        true -> not_found;
        false -> presence_cache_bulk:get_from_cluster(UserId)
    end.

-spec bulk_get([integer()]) -> [map()].
bulk_get(UserIds) when is_list(UserIds) ->
    case persistent_term:get(presence_noop, false) of
        true -> [];
        false -> presence_cache_bulk:bulk_get_inner(UserIds)
    end.

-spec get_memory_stats() -> {ok, map()} | {error, term()}.
get_memory_stats() ->
    case presence_cache_api:safe_call_if_enabled(get_memory_stats, {error, not_available}) of
        {ok, Stats} when is_map(Stats) -> {ok, Stats};
        {error, Reason} -> {error, Reason};
        _ -> {error, not_available}
    end.

-spec get_pending_handoff_count() -> non_neg_integer().
get_pending_handoff_count() ->
    case presence_cache_api:safe_call_if_enabled(get_pending_handoff_count, 0) of
        Count when is_integer(Count), Count >= 0 -> Count;
        _ -> 0
    end.

-spec pending_handoff_count() -> non_neg_integer().
pending_handoff_count() ->
    get_pending_handoff_count().

-spec rebalance_async() -> ok.
rebalance_async() ->
    presence_cache_api:safe_cast_if_enabled(rebalance).

-spec rebalance() -> ok.
rebalance() ->
    presence_cache_api:rebalance().

-spec handle_nodedown(node()) -> ok.
handle_nodedown(Node) when is_atom(Node) ->
    presence_cache_api:safe_cast_if_enabled({nodedown_grace, Node}).

-spec handle_nodeup(node()) -> ok.
handle_nodeup(Node) when is_atom(Node) ->
    presence_cache_api:safe_cast_if_enabled({nodeup_cancel_grace, Node}).

-spec trigger_anti_entropy() -> ok.
trigger_anti_entropy() ->
    presence_cache_api:safe_cast_if_enabled(anti_entropy_sync).

-spec generation() -> non_neg_integer().
generation() ->
    presence_cache_api:generation().

-spec pending_operations_count() -> non_neg_integer().
pending_operations_count() ->
    get_pending_handoff_count().

-spec handoff_to_target(node()) -> ok.
handoff_to_target(TargetNode) ->
    presence_cache_api:handoff_to_target(TargetNode).

-spec put_local(integer(), map(), state()) -> {ok, state()}.
put_local(UserId, Presence, State) ->
    {_Reply, NewState} = presence_cache_shards:forward_put(UserId, Presence, State),
    Tracked = presence_cache_anti_entropy:record_put(UserId, Presence, NewState),
    {ok, presence_cache_rebalance:increment_generation(Tracked)}.

-spec delete_local(integer(), state()) -> {ok, state()}.
delete_local(UserId, State) ->
    NewState = presence_cache_rebalance:evict_local(UserId, State),
    {ok, presence_cache_anti_entropy:record_delete(UserId, NewState)}.

-spec local_snapshot(state()) -> #{integer() => map()}.
local_snapshot(State) ->
    presence_cache_shards:local_snapshot(State).

-spec init(list()) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 10),
    {ShardCount, _Source} = presence_cache_shards:determine_count(presence_cache_shards),
    Shards = presence_cache_shards:start_all(ShardCount),
    {ok, #{
        shards => Shards,
        shard_count => ShardCount,
        pending_operations => #{},
        pending_retry_timer => undefined,
        pending_nodedown_cleanups => #{},
        delete_tombstones => #{},
        delete_tombstone_order => queue:new(),
        generation => 0,
        anti_entropy_timer => presence_cache_rebalance:schedule_anti_entropy()
    }}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    presence_cache_rebalance:cancel_pending_retry_timer(State),
    presence_cache_rebalance:cancel_all_grace_timers(State),
    presence_cache_rebalance:cancel_anti_entropy_timer(State),
    presence_cache_shards:stop_all(State),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, presence_cache_rebalance:ensure_pending_state(State)}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({put, UserId, Presence}, _From, State) when is_integer(UserId), is_map(Presence) ->
    {reply, ok, presence_cache_ops:handle_put(UserId, Presence, State)};
handle_call({put_local, UserId, Presence}, _From, State) when
    is_integer(UserId), is_map(Presence)
->
    {Reply, NewState} = put_local(UserId, Presence, State),
    {reply, Reply, NewState};
handle_call({delete, UserId}, _From, State) when is_integer(UserId) ->
    {reply, ok, presence_cache_ops:handle_delete(UserId, State)};
handle_call({delete_local, UserId}, _From, State) when is_integer(UserId) ->
    {Reply, NewState} = delete_local(UserId, State),
    {reply, Reply, NewState};
handle_call({get_local, UserId}, _From, State) when is_integer(UserId) ->
    {Reply, NewState} = presence_cache_ops:get_local(UserId, State),
    {reply, Reply, NewState};
handle_call(Request, From, State) ->
    handle_call_extended(Request, From, State).

-spec handle_call_extended(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call_extended({bulk_get_local, UserIds}, _From, State) when is_list(UserIds) ->
    {Reply, NewState} = presence_cache_shards:forward_bulk_get(
        presence_cache_bulk:normalize_user_ids(UserIds), State
    ),
    {reply, Reply, NewState};
handle_call_extended({bulk_get_local_map, UserIds}, _From, State) when is_list(UserIds) ->
    {Reply, NewState} = presence_cache_shards:forward_bulk_get_map(
        presence_cache_bulk:normalize_user_ids(UserIds), State
    ),
    {reply, Reply, NewState};
handle_call_extended(rebalance, _From, State) ->
    {reply, ok, presence_cache_rebalance:rebalance_ownership(State)};
handle_call_extended({handoff_to_target, TargetNode}, _From, State) when is_atom(TargetNode) ->
    {reply, ok, presence_cache_rebalance:handoff_all_to_target(TargetNode, State)};
handle_call_extended(get_pending_handoff_count, _From, State) ->
    {reply, presence_cache_rebalance:count_pending_operations(State), State};
handle_call_extended(get_memory_stats, _From, State) ->
    {reply, presence_cache_shards:memory_stats(State), State};
handle_call_extended(get_generation, _From, State) ->
    {reply, maps:get(generation, State, 0), State};
handle_call_extended(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(rebalance, State) ->
    {noreply, presence_cache_rebalance:rebalance_ownership(State)};
handle_cast({nodedown_grace, Node}, State) when is_atom(Node) ->
    {noreply, presence_cache_rebalance:start_nodedown_grace(Node, State)};
handle_cast({nodeup_cancel_grace, Node}, State) when is_atom(Node) ->
    {noreply, presence_cache_rebalance:cancel_nodedown_grace(Node, State)};
handle_cast(anti_entropy_sync, State) ->
    {noreply, presence_cache_rebalance:perform_anti_entropy(State)};
handle_cast({anti_entropy_request, FromNode, RemoteGeneration}, State) when
    is_atom(FromNode), is_integer(RemoteGeneration), RemoteGeneration >= 0
->
    presence_cache_rebalance:handle_anti_entropy_request(FromNode, RemoteGeneration, State);
handle_cast({anti_entropy_digest_request, FromNode, RemoteDigest}, State) when
    is_atom(FromNode), is_binary(RemoteDigest)
->
    presence_cache_rebalance:handle_anti_entropy_digest_request(FromNode, RemoteDigest, State);
handle_cast({anti_entropy_response, Entries}, State) when is_map(Entries) ->
    {noreply,
        presence_cache_rebalance:merge_anti_entropy_entries(
            presence_cache_bulk:sanitize_presence_map(Entries), State
        )};
handle_cast({put, UserId, Presence}, State) when is_integer(UserId), is_map(Presence) ->
    {noreply, presence_cache_ops:handle_put(UserId, Presence, State)};
handle_cast({delete, UserId}, State) when is_integer(UserId) ->
    {noreply, presence_cache_ops:handle_delete(UserId, State)};
handle_cast({put_local, UserId, Presence}, State) when is_integer(UserId), is_map(Presence) ->
    {_Reply, NewState} = put_local(UserId, Presence, State),
    {noreply, NewState};
handle_cast({delete_local, UserId}, State) when is_integer(UserId) ->
    {_Reply, NewState} = delete_local(UserId, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) when is_reference(Ref) ->
    State1 = presence_cache_rebalance:ensure_pending_state(State),
    {noreply, presence_cache_shards:handle_down_by_ref(Ref, State1)};
handle_info(?PENDING_HANDOFF_RETRY_MSG, State) ->
    State1 = (presence_cache_rebalance:ensure_pending_state(State))#{
        pending_retry_timer => undefined
    },
    {noreply, presence_cache_rebalance:rebalance_ownership(State1)};
handle_info({'EXIT', Pid, _Reason}, State) when is_pid(Pid) ->
    State1 = presence_cache_rebalance:ensure_pending_state(State),
    {noreply, presence_cache_shards:handle_down_by_pid(Pid, State1)};
handle_info({nodedown_grace_expired, Node}, State) when is_atom(Node) ->
    {noreply, presence_cache_rebalance:process_nodedown_grace_expiry(Node, State)};
handle_info(?ANTI_ENTROPY_MSG, State) ->
    State1 = presence_cache_rebalance:perform_anti_entropy(State),
    {noreply, State1#{anti_entropy_timer => presence_cache_rebalance:schedule_anti_entropy()}};
handle_info({'ETS-TRANSFER', _Table, _FromPid, _HeirData}, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, presence_cache_rebalance:ensure_pending_state(State)}.

-spec normalize_start_link(gen_server:start_ret()) -> {ok, pid()} | {error, term()}.
normalize_start_link({ok, Pid}) ->
    {ok, Pid};
normalize_start_link({error, Reason}) ->
    {error, Reason};
normalize_start_link(ignore) ->
    {error, ignore}.
