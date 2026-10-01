%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_manager_shard).
-typing([eqwalizer]).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export_type([session_data/0, user_id/0]).

-type session_id() :: binary().
-type user_id() :: integer().
-type session_ref() :: {pid(), reference()}.
-type status() :: online | offline | idle | dnd | invisible.
-type identify_timestamp() :: integer().

-type identify_request() :: #{
    session_id := session_id(),
    identify_data := map(),
    version := non_neg_integer(),
    peer_ip := term(),
    token := binary()
}.

-type session_data() :: #{
    id := session_id(),
    user_id := user_id(),
    user_data := map(),
    version := non_neg_integer(),
    token_hash := binary(),
    auth_session_id_hash := binary(),
    properties := map(),
    status := status(),
    afk := boolean(),
    mobile := boolean(),
    socket_pid := pid(),
    guilds := [integer()],
    ready := map(),
    ignored_events := [binary()]
}.

-type state() :: #{
    sessions := #{session_id() => session_ref()},
    identify_attempts := [identify_timestamp()],
    pending_identifies := #{session_id() => pending_identify()},
    identify_workers := #{reference() => {session_id(), pid()}},
    shard_index := non_neg_integer(),
    _ => _
}.

-type pending_identify() :: #{
    request := identify_request(),
    socket_pid := pid(),
    froms := [gen_server:from()],
    worker_ref := reference(),
    worker_token := reference(),
    timeout_ref := reference(),
    slot_held => boolean()
}.

-type start_reply() ::
    {success, pid()}
    | {error, draining}
    | {error, invalid_token}
    | {error, rate_limited}
    | {error, identify_rate_limited}
    | {error, not_eligible}
    | {error, {server_error, non_neg_integer()}}
    | {error, {retries_exhausted, term()}}
    | {error, {rpc_error, non_neg_integer(), binary()}}
    | {error, {network_error, term()}}
    | {error, registration_failed}
    | {error, term()}.

-type lookup_reply() :: {ok, pid()} | {error, not_found}.
-type rehydrate_lookup_reply() :: {ok, pid()} | {error, not_found} | {error, invalid_token}.

-spec start_link(non_neg_integer()) -> {ok, pid()} | ignore | {error, term()}.
start_link(ShardIndex) ->
    gen_server:start_link(?MODULE, #{shard_index => ShardIndex}, []).

-spec init(map()) -> {ok, state(), hibernate}.
init(Args) ->
    _ = fluxer_gateway_env:load(),
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 0),
    ShardIndex = maps:get(shard_index, Args),
    {ok,
        #{
            sessions => #{},
            session_refs => #{},
            identify_attempts => [],
            pending_identifies => #{},
            identify_workers => #{},
            shard_index => ShardIndex
        },
        hibernate}.

-spec handle_call(Request, From, State) -> Result when
    Request ::
        {start, identify_request(), pid()}
        | {lookup, session_id()}
        | {lookup_or_rehydrate, session_id(), binary(), pid()}
        | get_local_count
        | get_global_count
        | reconnect_drain
        | {handoff_to_topology, [node()]}
        | term(),
    From :: gen_server:from(),
    State :: state(),
    Result :: {reply, Reply, state()} | {noreply, state()},
    Reply ::
        start_reply()
        | lookup_reply()
        | rehydrate_lookup_reply()
        | {ok, non_neg_integer()}
        | {ok, #{attempted := non_neg_integer(), handed_off := non_neg_integer()}}
        | ok.
handle_call({start, Request, SocketPid}, From, State) when is_map(Request), is_pid(SocketPid) ->
    session_manager_shard_lifecycle:handle_start_call(
        gateway_node_router:is_draining(), Request, SocketPid, From, State
    );
handle_call({lookup, SessionId}, _From, State) when is_binary(SessionId) ->
    {Reply, NewState} = session_manager_shard_lookup:lookup_session_anywhere(SessionId, State),
    {reply, Reply, NewState};
handle_call({lookup, _InvalidSessionId}, _From, State) ->
    {reply, {error, not_found}, State};
handle_call({lookup_or_rehydrate, SessionId, Token, SocketPid}, _From, State) when
    is_binary(SessionId), is_binary(Token), is_pid(SocketPid)
->
    handle_lookup_or_rehydrate(SessionId, Token, SocketPid, State);
handle_call({lookup_or_rehydrate, _InvalidSessionId, _Token, _SocketPid}, _From, State) ->
    {reply, {error, not_found}, State};
handle_call(get_local_count, _From, State) ->
    handle_session_count(State);
handle_call(get_global_count, _From, State) ->
    handle_session_count(State);
handle_call(reconnect_drain, _From, State) ->
    DrainCount = session_manager_shard_drain:broadcast_reconnect_drain(State),
    {reply, {ok, DrainCount}, State};
handle_call({transfer_to, TargetNode}, _From, State) when is_atom(TargetNode) ->
    TransferCount = session_manager_shard_drain:broadcast_transfer_to(TargetNode, State),
    {reply, {ok, TransferCount}, State};
handle_call({transfer_to_topology, TargetNodes}, _From, State) ->
    handle_transfer_topology(TargetNodes, State);
handle_call({handoff_to_topology, TargetNodes}, _From, State) ->
    handle_handoff_topology(TargetNodes, State);
handle_call(_, _From, State) ->
    {reply, ok, State}.

-spec handle_lookup_or_rehydrate(session_id(), binary(), pid(), state()) ->
    {reply, rehydrate_lookup_reply(), state()}.
handle_lookup_or_rehydrate(SessionId, Token, SocketPid, State) ->
    {Reply, NewState} = session_manager_shard_lookup:lookup_or_rehydrate(
        SessionId, Token, SocketPid, State
    ),
    {reply, Reply, NewState}.

-spec handle_session_count(state()) -> {reply, {ok, non_neg_integer()}, state()}.
handle_session_count(State) ->
    Sessions = maps:get(sessions, State),
    {reply, {ok, maps:size(Sessions)}, State}.

-spec handle_transfer_topology(term(), state()) ->
    {reply, {ok, non_neg_integer()}, state()}.
handle_transfer_topology(TargetNodes, State) ->
    case atom_list(TargetNodes) of
        {ok, Nodes} ->
            Count = session_manager_shard_drain:broadcast_transfer_to_topology(Nodes, State),
            {reply, {ok, Count}, State};
        error ->
            {reply, {ok, 0}, State}
    end.

-spec handle_handoff_topology(term(), state()) ->
    {reply, {ok, #{attempted := non_neg_integer(), handed_off := non_neg_integer()}}, state()}.
handle_handoff_topology(TargetNodes, State) ->
    case atom_list(TargetNodes) of
        {ok, Nodes} ->
            Result = session_manager_shard_drain:handoff_to_topology(Nodes, State),
            {reply, {ok, Result}, State};
        error ->
            {reply, {ok, #{attempted => 0, handed_off => 0}}, State}
    end.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({broadcast_dispatch, Event, {pre_encoded, Encoded} = Data}, State) when
    is_atom(Event), is_binary(Encoded)
->
    session_manager_shard_drain:broadcast_dispatch(Event, Data, State),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_info(Info, State) -> {noreply, state()} when
    Info :: {'DOWN', reference(), process, pid(), term()} | term(),
    State :: state().
handle_info({identify_slot_acquired, SessionId, WorkerToken}, State) when
    is_binary(SessionId), is_reference(WorkerToken)
->
    session_manager_shard_lifecycle:note_identify_slot_acquired(
        SessionId, WorkerToken, State
    );
handle_info({identify_fetch_result, SessionId, WorkerToken, FetchResult}, State) when
    is_binary(SessionId), is_reference(WorkerToken)
->
    session_manager_shard_lifecycle:complete_identify_fetch(
        SessionId, WorkerToken, FetchResult, State
    );
handle_info({identify_fetch_result, SessionId, FetchResult}, State) when is_binary(SessionId) ->
    session_manager_shard_lifecycle:complete_identify_fetch(SessionId, FetchResult, State);
handle_info({identify_fetch_timeout, SessionId, WorkerRef}, State) when
    is_binary(SessionId), is_reference(WorkerRef)
->
    session_manager_shard_lifecycle:handle_identify_fetch_timeout(SessionId, WorkerRef, State);
handle_info({'DOWN', Ref, process, Pid, Reason}, State) when is_reference(Ref), is_pid(Pid) ->
    IdentifyWorkers = maps:get(identify_workers, State),
    case maps:take(Ref, IdentifyWorkers) of
        {{SessionId, _WorkerPid}, RemainingWorkers} ->
            StateWithoutWorker = State#{identify_workers := RemainingWorkers},
            session_manager_shard_lifecycle:maybe_fail_pending_identify(
                SessionId, Ref, Reason, StateWithoutWorker
            );
        error ->
            {noreply, session_manager_shard_session_index:cleanup_down(Ref, Pid, State)}
    end;
handle_info({'EXIT', Pid, _Reason}, State) when is_pid(Pid) ->
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

-spec terminate(Reason, State) -> ok when
    Reason :: term(),
    State :: state().
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:process_flag(fullsweep_after, 0),
    erlang:garbage_collect(),
    {ok, State}.

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
