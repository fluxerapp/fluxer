%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_manager).
-typing([eqwalizer]).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    start_link/0,
    start/2,
    lookup/1,
    lookup_or_rehydrate/3,
    reconnect_drain/0,
    handoff_to_topology/1,
    session_count/0,
    call_shard/3
]).

-export([
    ensure_shard/2, restart_shard/2
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(START_TIMEOUT, 135000).
-define(LOOKUP_TIMEOUT, 5000).

-type session_id() :: binary().
-type shard() :: #{pid := pid(), ref := reference()}.
-type state() :: #{shards := #{non_neg_integer() => shard()}, shard_count := pos_integer()}.
-type handoff_result() :: #{
    attempted := non_neg_integer(),
    handed_off := non_neg_integer()
}.

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start(map(), pid()) -> term().
start(Request, SocketPid) ->
    SessionId = maps:get(session_id, Request),
    session_manager_routing:call_owner_manager(
        SessionId, {start, Request, SocketPid}, ?START_TIMEOUT
    ).

-spec lookup(session_id()) -> {ok, pid()} | {error, not_found}.
lookup(SessionId) ->
    Reply = session_manager_routing:call_owner_manager(
        SessionId, {lookup, SessionId}, ?LOOKUP_TIMEOUT
    ),
    case Reply of
        {ok, Pid} when is_pid(Pid) -> {ok, Pid};
        _ -> {error, not_found}
    end.

-spec lookup_or_rehydrate(session_id(), binary(), pid()) ->
    {ok, pid()} | {error, not_found} | {error, invalid_token}.
lookup_or_rehydrate(SessionId, Token, SocketPid) ->
    Reply = session_manager_routing:call_owner_manager(
        SessionId, {lookup_or_rehydrate, SessionId, Token, SocketPid}, ?LOOKUP_TIMEOUT
    ),
    case Reply of
        {ok, Pid} when is_pid(Pid) -> {ok, Pid};
        {error, invalid_token} -> {error, invalid_token};
        _ -> {error, not_found}
    end.

-spec reconnect_drain() -> {ok, non_neg_integer()} | {error, timeout | unavailable}.
reconnect_drain() ->
    safe_gen_call(reconnect_drain).

-spec handoff_to_topology([node()]) -> {ok, handoff_result()} | {error, timeout | unavailable}.
handoff_to_topology(TargetNodes) ->
    safe_handoff_call({handoff_to_topology, TargetNodes}).

-spec session_count() -> non_neg_integer().
session_count() ->
    try gen_server:call(?MODULE, get_local_count, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {ok, Count} when is_integer(Count), Count >= 0 -> Count;
        Count when is_integer(Count), Count >= 0 -> Count;
        _ -> 0
    catch
        error:_ -> 0;
        exit:_ -> 0
    end.

-spec call_shard(session_id(), term(), pos_integer()) -> term().
call_shard(SessionId, Request, Timeout) ->
    session_manager_routing:call_shard(SessionId, Request, Timeout).

-spec init([]) -> {ok, state(), hibernate}.
init([]) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 0),
    _ = fluxer_gateway_env:load(),
    session_manager_shards:ensure_shard_table(),
    {ShardCount, _Source} = shard_utils:determine_shard_count(session_shards),
    Shards = session_manager_shards:start_shards(ShardCount),
    State = #{shards => Shards, shard_count => ShardCount},
    session_manager_shards:sync_shard_table(State),
    {ok, State, hibernate}.

-spec handle_call(term(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({proxy_call, SessionId, Request, Timeout}, _From, State) when
    is_binary(SessionId), is_integer(Timeout), Timeout > 0
->
    handle_proxy_call(SessionId, Request, Timeout, State);
handle_call({owner_call, SessionId, Request, Timeout}, _From, State) when
    is_binary(SessionId), is_integer(Timeout), Timeout > 0
->
    handle_owner_request(SessionId, Request, Timeout, State);
handle_call({start, Request, SocketPid} = OwnerRequest, From, State) when
    is_map(Request), is_pid(SocketPid)
->
    handle_start_owner_request(Request, SocketPid, OwnerRequest, From, State);
handle_call({lookup, SessionId} = OwnerRequest, From, State) when is_binary(SessionId) ->
    handle_lookup_owner_request(SessionId, OwnerRequest, From, State);
handle_call({lookup_or_rehydrate, SessionId, Token, SocketPid} = OwnerRequest, From, State) when
    is_binary(SessionId), is_binary(Token), is_pid(SocketPid)
->
    handle_lookup_owner_request(SessionId, OwnerRequest, From, State);
handle_call(get_local_count, _From, State) ->
    handle_aggregate_count(get_local_count, State);
handle_call(get_global_count, _From, State) ->
    handle_aggregate_count(get_global_count, State);
handle_call(reconnect_drain, _From, State) ->
    handle_aggregate_count(reconnect_drain, State);
handle_call({handoff_to_topology, TargetNodes}, _From, State) ->
    handle_handoff_to_topology_request(TargetNodes, State);
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_handoff_to_topology_request(term(), state()) ->
    {reply, {ok, handoff_result()} | {error, unavailable}, state()}.
handle_handoff_to_topology_request(TargetNodes, State) ->
    case atom_list(TargetNodes) of
        {ok, Nodes} -> handle_handoff_to_topology(Nodes, State);
        error -> {reply, {error, unavailable}, State}
    end.

-spec handle_handoff_to_topology([node()], state()) ->
    {reply, {ok, handoff_result()}, state()}.
handle_handoff_to_topology(TargetNodes, State) ->
    {Result, NewState} = session_manager_transfer:aggregate_handoff_to_topology(
        TargetNodes, State
    ),
    {reply, {ok, Result}, NewState}.

-spec handle_proxy_call(session_id(), term(), pos_integer(), state()) ->
    {reply, term(), state()}.
handle_proxy_call(SessionId, Request, Timeout, State) ->
    {Reply, NewState} = session_manager_routing:forward_call(
        SessionId, Request, Timeout, State
    ),
    {reply, Reply, NewState}.

-spec handle_owner_request(session_id(), term(), pos_integer(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_owner_request(SessionId, Request, Timeout, State) ->
    case session_manager_routing:owner_scope(SessionId) of
        local -> reply_owner_request(SessionId, Request, Timeout, State);
        {remote, OwnerNode} -> {reply, {error, {not_owner, OwnerNode}}, State};
        unavailable -> {reply, {error, unavailable}, State}
    end.

-spec reply_owner_request(session_id(), term(), pos_integer(), state()) ->
    {reply, term(), state()}.
reply_owner_request(SessionId, Request, Timeout, State) ->
    {Reply, NewState} = session_manager_routing:execute_owner_request(
        SessionId, Request, Timeout, State
    ),
    {reply, Reply, NewState}.

-spec handle_start_owner_request(map(), pid(), term(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_start_owner_request(Request, _SocketPid, OwnerRequest, From, State) ->
    case maps:get(session_id, Request, undefined) of
        SessionId when is_binary(SessionId) ->
            session_manager_routing:handle_owner_call(
                SessionId, OwnerRequest, ?START_TIMEOUT, From, State
            );
        _ ->
            {reply, {error, invalid_session}, State}
    end.

-spec handle_lookup_owner_request(session_id(), term(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_lookup_owner_request(SessionId, OwnerRequest, From, State) ->
    session_manager_routing:handle_owner_call(
        SessionId, OwnerRequest, ?LOOKUP_TIMEOUT, From, State
    ).

-spec handle_aggregate_count(term(), state()) -> {reply, {ok, non_neg_integer()}, state()}.
handle_aggregate_count(Request, State) ->
    {Count, NewState} = session_manager_transfer:aggregate_counts(Request, State),
    {reply, {ok, Count}, NewState}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) when is_reference(Ref), is_pid(Pid) ->
    Shards = maps:get(shards, State),
    case session_manager_transfer:find_shard_by_ref(Ref, Shards) of
        {ok, Index} -> restart_shard_noreply(Index, State);
        not_found -> handle_down_by_pid(Pid, State)
    end;
handle_info({'EXIT', Pid, _Reason}, State) when is_pid(Pid) ->
    handle_down_by_pid(Pid, State);
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    Shards = maps:get(shards, State),
    lists:foreach(fun stop_shard/1, maps:values(Shards)),
    session_manager_shards:delete_shard_table(),
    ok.

-spec stop_shard(shard()) -> ok.
stop_shard(#{pid := Pid}) ->
    try
        gen_server:stop(Pid, shutdown, 5000)
    catch
        error:_ -> ok;
        exit:_ -> ok
    end.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:process_flag(fullsweep_after, 0),
    erlang:garbage_collect(),
    {ok, State}.

-spec ensure_shard(session_id(), state()) -> {non_neg_integer(), state()}.
ensure_shard(SessionId, State) ->
    session_manager_shards:ensure_shard(SessionId, State).

-spec restart_shard(non_neg_integer(), state()) -> {shard() | {error, term()}, state()}.
restart_shard(Index, State) ->
    session_manager_shards:restart_shard(Index, State).

-spec safe_gen_call(term()) -> {ok, non_neg_integer()} | {error, timeout | unavailable}.
safe_gen_call(Request) ->
    try gen_server:call(?MODULE, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {ok, Count} when is_integer(Count), Count >= 0 ->
            {ok, Count};
        Count when is_integer(Count), Count >= 0 ->
            {ok, Count};
        _ ->
            {error, unavailable}
    catch
        exit:{timeout, _} ->
            {error, timeout};
        exit:_ ->
            {error, unavailable};
        error:_ ->
            {error, unavailable}
    end.

-spec safe_handoff_call(term()) -> {ok, handoff_result()} | {error, timeout | unavailable}.
safe_handoff_call(Request) ->
    try gen_server:call(?MODULE, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {ok, #{attempted := Attempted, handed_off := HandedOff} = Result} when
            is_integer(Attempted),
            Attempted >= 0,
            is_integer(HandedOff),
            HandedOff >= 0
        ->
            {ok, Result};
        #{attempted := Attempted, handed_off := HandedOff} = Result when
            is_integer(Attempted),
            Attempted >= 0,
            is_integer(HandedOff),
            HandedOff >= 0
        ->
            {ok, Result};
        _ ->
            {error, unavailable}
    catch
        exit:{timeout, _} ->
            {error, timeout};
        exit:_ ->
            {error, unavailable};
        error:_ ->
            {error, unavailable}
    end.

-spec restart_shard_noreply(non_neg_integer(), state()) -> {noreply, state()}.
restart_shard_noreply(Index, State) ->
    {_Shard, NewState} = restart_shard(Index, State),
    {noreply, NewState}.

-spec handle_down_by_pid(pid(), state()) -> {noreply, state()}.
handle_down_by_pid(Pid, State) ->
    Shards = maps:get(shards, State),
    case session_manager_transfer:find_shard_by_pid(Pid, Shards) of
        {ok, Index} -> restart_shard_noreply(Index, State);
        not_found -> {noreply, State}
    end.

-spec atom_list(term()) -> {ok, [atom()]} | error.
atom_list(Value) when is_list(Value) ->
    atom_list(Value, []);
atom_list(_) ->
    error.

-spec atom_list([term()], [atom()]) -> {ok, [atom()]} | error.
atom_list([], Acc) ->
    {ok, lists:reverse(Acc)};
atom_list([Value | Rest], Acc) when is_atom(Value) ->
    atom_list(Rest, [Value | Acc]);
atom_list(_, _) ->
    error.
