%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager_shard_lookup).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    do_start_or_lookup/3,
    do_lookup/2,
    lookup_or_fetch/3,
    start_fetch/3,
    add_pending_request/3,
    handle_guild_data_fetched/3,
    handle_guild_data_fetched/4,
    handle_fetch_worker_down/3,
    reply_to_all/2,
    collect_active_guild_ids/1,
    ensure_local_owner/1,
    fetch_guild_data/1
]).

-type guild_id() :: integer().
-type state() :: map().
-type fetch_result() :: {ok, map()} | {error, term()}.
-type fetch_token() :: reference() | undefined.

-export_type([guild_id/0, state/0, fetch_result/0, fetch_token/0]).

-spec do_start_or_lookup(guild_id(), gen_server:from(), state()) ->
    {reply, {ok, pid()} | {error, term()}, state()} | {noreply, state()}.
do_start_or_lookup(GuildId, From, State) ->
    case ensure_local_owner(GuildId) of
        ok ->
            do_start_or_lookup_owned(GuildId, From, State);
        {error, _Reason} = Error ->
            {reply, Error, State}
    end.

-spec do_start_or_lookup_owned(guild_id(), gen_server:from(), state()) ->
    {reply, {ok, pid()} | {error, term()}, state()} | {noreply, state()}.
do_start_or_lookup_owned(GuildId, From, State) ->
    Guilds = maps:get(guilds, State),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, Ref} ->
            handle_tracked_start_or_lookup(GuildId, Pid, Ref, From, Guilds, State);
        loading ->
            add_pending_request(GuildId, From, State);
        undefined ->
            lookup_or_fetch(GuildId, From, State)
    end.

-spec handle_tracked_start_or_lookup(
    guild_id(), pid(), reference(), gen_server:from(), map(), state()
) -> {reply, {ok, pid()} | {error, term()}, state()} | {noreply, state()}.
handle_tracked_start_or_lookup(GuildId, Pid, Ref, From, Guilds, State) ->
    case process_liveness:is_alive(Pid) of
        true ->
            {reply, {ok, Pid}, State};
        false ->
            cleanup_dead_guild(GuildId, Ref, Guilds),
            CleanState = State#{guilds => maps:remove(GuildId, Guilds)},
            lookup_or_fetch(GuildId, From, CleanState)
    end.

-spec do_lookup(guild_id(), state()) -> {reply, {ok, pid()} | {error, not_found}, state()}.
do_lookup(GuildId, State) ->
    Guilds = maps:get(guilds, State),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, Ref} ->
            handle_tracked_lookup(GuildId, Pid, Ref, Guilds, State);
        loading ->
            {reply, {error, not_found}, State};
        undefined ->
            lookup_from_registry(GuildId, Guilds, State)
    end.

-spec handle_tracked_lookup(guild_id(), pid(), reference(), map(), state()) ->
    {reply, {ok, pid()} | {error, not_found}, state()}.
handle_tracked_lookup(GuildId, Pid, Ref, Guilds, State) ->
    case process_liveness:is_alive(Pid) of
        true ->
            {reply, {ok, Pid}, State};
        false ->
            cleanup_dead_guild(GuildId, Ref, Guilds),
            do_lookup(GuildId, State#{guilds => maps:remove(GuildId, Guilds)})
    end.

-spec lookup_from_registry(guild_id(), map(), state()) ->
    {reply, {ok, pid()} | {error, not_found}, state()}.
lookup_from_registry(GuildId, Guilds, State) ->
    GuildKey = process_registry:build_process_key(guild, GuildId),
    case process_registry:lookup_or_monitor(GuildKey, GuildId, Guilds) of
        {ok, Pid, _Ref, NewGuilds} -> {reply, {ok, Pid}, State#{guilds => NewGuilds}};
        {error, not_found} -> {reply, {error, not_found}, State}
    end.

-spec monitor_existing(process_registry:process_key(), guild_id(), map(), state()) ->
    {reply, {ok, pid()} | {error, term()}, state()}.
monitor_existing(GuildKey, GuildId, Guilds, State) ->
    case process_registry:lookup_or_monitor(GuildKey, GuildId, Guilds) of
        {ok, Pid, _Ref, NewGuilds} -> {reply, {ok, Pid}, State#{guilds => NewGuilds}};
        {error, not_found} -> {reply, {error, process_died}, State}
    end.

-spec lookup_or_fetch(guild_id(), gen_server:from(), state()) ->
    {reply, {ok, pid()} | {error, term()}, state()} | {noreply, state()}.
lookup_or_fetch(GuildId, From, State) ->
    GuildKey = process_registry:build_process_key(guild, GuildId),
    case process_registry:registry_whereis(GuildKey) of
        undefined ->
            lookup_or_fetch_new(GuildId, GuildKey, From, State);
        _ExistingPid ->
            Guilds = maps:get(guilds, State),
            monitor_existing(GuildKey, GuildId, Guilds, State)
    end.

-spec lookup_or_fetch_new(
    guild_id(), process_registry:process_key(), gen_server:from(), state()
) ->
    {reply, {ok, pid()} | {error, term()}, state()} | {noreply, state()}.
lookup_or_fetch_new(GuildId, GuildKey, From, State) ->
    GuildIdBin = require_binary(type_conv:to_binary(GuildId)),
    maybe
        true ?= gateway_rollout_config:is_guild_eligible(GuildIdBin),
        {ok, RemotePid} ?= check_remote_guild_exists(GuildId, GuildKey),
        Guilds = maps:get(guilds, State),
        Ref = erlang:monitor(process, RemotePid),
        NewGuilds = Guilds#{GuildId => {RemotePid, Ref}},
        ets:insert_new(process_registry_table, {GuildKey, RemotePid}),
        {reply, {ok, RemotePid}, State#{guilds => NewGuilds}}
    else
        false -> {reply, {error, not_eligible}, State};
        not_found -> start_fetch(GuildId, From, State)
    end.

-spec start_fetch(guild_id(), gen_server:from(), state()) -> {noreply, state()}.
start_fetch(GuildId, From, State) ->
    Guilds = maps:get(guilds, State),
    Pending = maps:get(pending_requests, State),
    NewState = State#{
        guilds => Guilds#{GuildId => loading},
        pending_requests => Pending#{GuildId => [From]}
    },
    {noreply, start_fetch_worker(GuildId, NewState)}.

-spec start_fetch_worker(guild_id(), state()) -> state().
start_fetch_worker(GuildId, State) ->
    FetchToken = make_ref(),
    {WorkerPid, WorkerRef} = spawn_fetch(GuildId, FetchToken),
    FetchWorkers = maps:get(fetch_workers, State, #{}),
    State#{fetch_workers => FetchWorkers#{WorkerRef => {GuildId, WorkerPid, FetchToken}}}.

-spec spawn_fetch(guild_id(), reference()) -> {pid(), reference()}.
spawn_fetch(GuildId, FetchToken) ->
    Manager = self(),
    spawn_monitor(fun() ->
        fetch_worker(Manager, GuildId, FetchToken)
    end).

-spec fetch_worker(pid(), guild_id(), reference()) -> ok.
fetch_worker(Manager, GuildId, FetchToken) ->
    erlang:process_flag(fullsweep_after, 0),
    Result = safe_fetch_guild_data(GuildId),
    gen_server:cast(Manager, {guild_data_fetched, GuildId, FetchToken, Result}).

-spec safe_fetch_guild_data(guild_id()) -> fetch_result().
safe_fetch_guild_data(GuildId) ->
    try
        fetch_guild_data(GuildId)
    catch
        throw:Reason -> {error, {fetch_threw, Reason}};
        error:Reason -> {error, {fetch_failed, Reason}};
        exit:Reason -> {error, {fetch_exited, Reason}}
    end.

-spec add_pending_request(guild_id(), gen_server:from(), state()) -> {noreply, state()}.
add_pending_request(GuildId, From, State) ->
    Pending = maps:get(pending_requests, State),
    Requests = maps:get(GuildId, Pending, []),
    {noreply, State#{pending_requests => Pending#{GuildId => [From | Requests]}}}.

-spec handle_guild_data_fetched(guild_id(), fetch_result(), state()) -> {noreply, state()}.
handle_guild_data_fetched(GuildId, Result, State) ->
    handle_guild_data_fetched(GuildId, undefined, Result, State).

-spec handle_guild_data_fetched(guild_id(), fetch_token(), fetch_result(), state()) ->
    {noreply, state()}.
handle_guild_data_fetched(GuildId, FetchToken, Result, State) ->
    case fetch_token_matches(GuildId, FetchToken, State) of
        true ->
            State1 = cleanup_fetch_worker(GuildId, FetchToken, State),
            complete_guild_data_fetch(GuildId, Result, State1);
        false ->
            {noreply, State}
    end.

-spec complete_guild_data_fetch(guild_id(), fetch_result(), state()) -> {noreply, state()}.
complete_guild_data_fetch(GuildId, {ok, Data}, State) ->
    complete_fetch_success(GuildId, Data, State);
complete_guild_data_fetch(GuildId, {error, Reason}, State) ->
    complete_fetch_error(GuildId, {error, Reason}, State).

-spec complete_fetch_success(guild_id(), map(), state()) -> {noreply, state()}.
complete_fetch_success(GuildId, Data, State) ->
    case superseding_guild_pid(GuildId, State) of
        {ok, Pid} -> reply_with_superseding_guild(GuildId, Pid, State);
        none -> start_fetched_guild(GuildId, Data, State)
    end.

-spec superseding_guild_pid(guild_id(), state()) -> {ok, pid()} | none.
superseding_guild_pid(GuildId, State) ->
    Guilds = maps:get(guilds, State),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, _Ref} when is_pid(Pid) ->
            superseding_guild_pid_for_pid(Pid);
        _ ->
            none
    end.

-spec superseding_guild_pid_for_pid(pid()) -> {ok, pid()} | none.
superseding_guild_pid_for_pid(Pid) ->
    case process_liveness:is_alive(Pid) of
        true -> {ok, Pid};
        false -> none
    end.

-spec reply_with_superseding_guild(guild_id(), pid(), state()) -> {noreply, state()}.
reply_with_superseding_guild(GuildId, Pid, State) ->
    Pending = maps:get(pending_requests, State),
    Requests = maps:get(GuildId, Pending, []),
    reply_to_all(Requests, {ok, Pid}),
    {noreply, State#{pending_requests => maps:remove(GuildId, Pending)}}.

-spec start_fetched_guild(guild_id(), map(), state()) -> {noreply, state()}.
start_fetched_guild(GuildId, Data, State) ->
    Pending = maps:get(pending_requests, State),
    Requests = maps:get(GuildId, Pending, []),
    case guild_manager_shard_lifecycle:start_guild(GuildId, Data, State) of
        {ok, Pid, NewState} ->
            reply_to_all(Requests, {ok, Pid}),
            {noreply, NewState#{pending_requests => maps:remove(GuildId, Pending)}};
        {error, Reason} ->
            complete_fetch_error(GuildId, {error, Reason}, State)
    end.

-spec complete_fetch_error(guild_id(), {error, term()}, state()) -> {noreply, state()}.
complete_fetch_error(GuildId, Reply, State) ->
    Pending = maps:get(pending_requests, State),
    Requests = maps:get(GuildId, Pending, []),
    Guilds = maps:get(guilds, State),
    reply_to_all(Requests, Reply),
    NewGuilds = maps:remove(GuildId, Guilds),
    CleanPending = maps:remove(GuildId, Pending),
    {noreply, State#{guilds => NewGuilds, pending_requests => CleanPending}}.

-spec handle_fetch_worker_down(reference(), term(), state()) ->
    {fetch_worker, state()} | not_fetch_worker.
handle_fetch_worker_down(WorkerRef, Reason, State) ->
    FetchWorkers = maps:get(fetch_workers, State, #{}),
    case maps:take(WorkerRef, FetchWorkers) of
        {{_GuildId, _WorkerPid, _FetchToken}, RemainingWorkers} when
            Reason =:= normal; Reason =:= shutdown
        ->
            {fetch_worker, State#{fetch_workers => RemainingWorkers}};
        {{GuildId, _WorkerPid, _FetchToken}, RemainingWorkers} ->
            State1 = State#{fetch_workers => RemainingWorkers},
            {noreply, State2} = complete_guild_data_fetch(
                GuildId, {error, {fetch_worker_down, Reason}}, State1
            ),
            {fetch_worker, State2};
        error ->
            not_fetch_worker
    end.

-spec fetch_token_matches(guild_id(), fetch_token(), state()) -> boolean().
fetch_token_matches(GuildId, undefined, State) ->
    active_fetch_worker_for_guild(GuildId, State) =:= undefined;
fetch_token_matches(GuildId, FetchToken, State) when is_reference(FetchToken) ->
    active_fetch_worker_for_guild(GuildId, State) =:= FetchToken.

-spec active_fetch_worker_for_guild(guild_id(), state()) -> fetch_token().
active_fetch_worker_for_guild(GuildId, State) ->
    maps:fold(
        fun
            (_WorkerRef, {WorkerGuildId, _WorkerPid, FetchToken}, _Acc) when
                WorkerGuildId =:= GuildId
            ->
                FetchToken;
            (_WorkerRef, _WorkerInfo, Acc) ->
                Acc
        end,
        undefined,
        maps:get(fetch_workers, State, #{})
    ).

-spec cleanup_fetch_worker(guild_id(), fetch_token(), state()) -> state().
cleanup_fetch_worker(GuildId, FetchToken, State) ->
    FetchWorkers = maps:get(fetch_workers, State, #{}),
    RemainingWorkers = maps:fold(
        fun
            (WorkerRef, {WorkerGuildId, _WorkerPid, WorkerFetchToken}, Acc) when
                WorkerGuildId =:= GuildId, WorkerFetchToken =:= FetchToken
            ->
                demonitor(WorkerRef, [flush]),
                Acc;
            (WorkerRef, WorkerInfo, Acc) ->
                Acc#{WorkerRef => WorkerInfo}
        end,
        #{},
        FetchWorkers
    ),
    State#{fetch_workers => RemainingWorkers}.

-spec reply_to_all([gen_server:from()], term()) -> ok.
reply_to_all(Requests, Reply) ->
    lists:foreach(fun(From) -> gen_server:reply(From, Reply) end, Requests).

-spec collect_active_guild_ids(map()) -> [guild_id()].
collect_active_guild_ids(Guilds) ->
    lists:sort(maps:fold(fun collect_active_id/3, [], Guilds)).

-spec collect_active_id(term(), term(), [guild_id()]) -> [guild_id()].
collect_active_id(GuildId, {Pid, _Ref}, Acc) when is_integer(GuildId), is_pid(Pid) ->
    case process_liveness:is_alive(Pid) of
        true -> [GuildId | Acc];
        false -> Acc
    end;
collect_active_id(_GuildId, _Value, Acc) ->
    Acc.

-spec ensure_local_owner(guild_id()) -> ok | {error, term()}.
ensure_local_owner(GuildId) ->
    case safe_owner_node(GuildId) of
        {ok, LocalNode} when LocalNode =:= node() -> ok;
        {ok, OwnerNode} when is_atom(OwnerNode) -> {error, {not_owner, OwnerNode}};
        error -> {error, cluster_unavailable}
    end.

-spec check_remote_guild_exists(guild_id(), process_registry:process_key()) ->
    {ok, pid()} | not_found.
check_remote_guild_exists(GuildId, GuildKey) ->
    case gateway_node_router:owner_node_result(GuildId, guilds) of
        {ok, OwnerNode} when OwnerNode =:= node() -> not_found;
        {ok, OwnerNode} when is_atom(OwnerNode) -> check_nodes_for_guild(GuildKey, [OwnerNode]);
        {error, _Reason} -> not_found
    end.

-spec require_binary(term()) -> binary().
require_binary(Value) when is_binary(Value) ->
    Value;
require_binary(_) ->
    error(badarg).

-spec check_nodes_for_guild(process_registry:process_key(), [node()]) ->
    {ok, pid()} | not_found.
check_nodes_for_guild(_GuildKey, []) ->
    not_found;
check_nodes_for_guild(GuildKey, [Node | Rest]) ->
    case check_node_for_guild(GuildKey, Node) of
        {ok, Pid} -> {ok, Pid};
        not_found -> check_nodes_for_guild(GuildKey, Rest)
    end.

-spec check_node_for_guild(process_registry:process_key(), node()) -> {ok, pid()} | not_found.
check_node_for_guild(GuildKey, Node) ->
    case rpc:call(Node, ets, lookup, [process_registry_table, GuildKey], 2000) of
        [{_, Pid}] when is_pid(Pid) ->
            verify_remote_pid(Node, Pid);
        _ ->
            not_found
    end.

-spec verify_remote_pid(node(), pid()) -> {ok, pid()} | not_found.
verify_remote_pid(Node, Pid) ->
    case rpc:call(Node, erlang, is_process_alive, [Pid], 1000) of
        true -> {ok, Pid};
        _ -> not_found
    end.

-spec cleanup_dead_guild(guild_id(), reference(), map()) -> ok.
cleanup_dead_guild(GuildId, Ref, _Guilds) ->
    demonitor(Ref, [flush]),
    GuildKey = process_registry:build_process_key(guild, GuildId),
    process_registry:safe_unregister(GuildKey),
    ok.

-spec fetch_guild_data(guild_id()) -> fetch_result().
fetch_guild_data(GuildId) ->
    guild_manager_shard_fetch:fetch_guild_data(GuildId).

-spec safe_owner_node(guild_id()) -> {ok, node()} | error.
safe_owner_node(GuildId) ->
    try gateway_node_router:owner_node_result(GuildId, guilds) of
        {ok, OwnerNode} when is_atom(OwnerNode) -> {ok, OwnerNode};
        {error, _Reason} -> error
    catch
        error:badarg -> error;
        error:undef -> error;
        exit:{noproc, _Call} -> error;
        exit:{nodedown, _Node} -> error
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

fetch_worker_down_replies_and_clears_loading_test() ->
    GuildId = 1427764882469228556,
    ReplyRef = make_ref(),
    WorkerRef = make_ref(),
    FetchToken = make_ref(),
    State0 = #{
        guilds => #{GuildId => loading},
        pending_requests => #{GuildId => [{self(), ReplyRef}]},
        fetch_workers => #{WorkerRef => {GuildId, self(), FetchToken}},
        shard_index => 0
    },
    {fetch_worker, State1} = handle_fetch_worker_down(WorkerRef, crashed, State0),
    ?assertEqual(#{}, maps:get(guilds, State1)),
    ?assertEqual(#{}, maps:get(pending_requests, State1)),
    ?assertEqual(#{}, maps:get(fetch_workers, State1)),
    receive
        {ReplyRef, {error, {fetch_worker_down, crashed}}} -> ok
    after 0 ->
        ?assert(false)
    end.

stale_fetch_result_does_not_complete_current_loading_test() ->
    GuildId = 1427764882469228556,
    ReplyRef = make_ref(),
    WorkerRef = make_ref(),
    CurrentToken = make_ref(),
    StaleToken = make_ref(),
    State0 = #{
        guilds => #{GuildId => loading},
        pending_requests => #{GuildId => [{self(), ReplyRef}]},
        fetch_workers => #{WorkerRef => {GuildId, self(), CurrentToken}},
        shard_index => 0
    },
    {noreply, State1} = handle_guild_data_fetched(
        GuildId,
        StaleToken,
        {error, not_found},
        State0
    ),
    ?assertEqual(State0, State1),
    receive
        {ReplyRef, _Reply} -> ?assert(false)
    after 0 ->
        ok
    end.

legacy_fetch_result_completes_when_no_fetch_worker_is_tracked_test() ->
    GuildId = 1427764882469228556,
    ReplyRef = make_ref(),
    State0 = #{
        guilds => #{GuildId => loading},
        pending_requests => #{GuildId => [{self(), ReplyRef}]},
        fetch_workers => #{},
        shard_index => 0
    },
    {noreply, State1} = handle_guild_data_fetched(GuildId, {error, not_found}, State0),
    ?assertEqual(#{}, maps:get(guilds, State1)),
    ?assertEqual(#{}, maps:get(pending_requests, State1)),
    receive
        {ReplyRef, {error, not_found}} -> ok
    after 0 ->
        ?assert(false)
    end.

-endif.
