%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_manager_routing).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    call_shard/3,
    call_owner_manager/3,
    owner_scope/1,
    select_shard/2,
    shard_pid_from_table/1,
    handle_owner_call/5,
    forward_call/4,
    execute_owner_request/4
]).

-export_type([session_id/0, state/0]).

-define(SHARD_TABLE, session_manager_shard_table).
-define(START_TIMEOUT, 135000).
-define(LOOKUP_TIMEOUT, 5000).
-define(OWNER_REDIRECT_LIMIT, 3).

-type session_id() :: binary().
-type shard() :: #{pid := pid(), ref := reference()}.
-type safe_call_result() :: {ok, term()} | {error, term()} | {exit, term()}.
-type state() :: #{shards := #{non_neg_integer() => shard()}, shard_count := pos_integer()}.

-spec call_shard(session_id(), term(), pos_integer()) -> term().
call_shard(SessionId, Request, Timeout) ->
    case shard_pid_from_table(SessionId) of
        {ok, Pid} ->
            call_shard_pid(Pid, SessionId, Request, Timeout);
        error ->
            call_shard_fallback(SessionId, Request, Timeout)
    end.

-spec call_shard_pid(pid(), session_id(), term(), pos_integer()) -> term().
call_shard_pid(Pid, SessionId, Request, Timeout) ->
    case safe_gen_call(Pid, Request, Timeout) of
        {ok, Reply} -> Reply;
        {exit, {timeout, _}} -> {error, timeout};
        _ -> call_via_manager(SessionId, Request, Timeout)
    end.

-spec call_shard_fallback(session_id(), term(), pos_integer()) -> term().
call_shard_fallback(SessionId, Request, Timeout) ->
    case fluxer_gateway_sup:role_enabled(sessions) of
        true ->
            call_via_manager(SessionId, Request, Timeout);
        false ->
            call_shard_fallback_remote(SessionId, Request, Timeout)
    end.

-spec call_shard_fallback_remote(session_id(), term(), pos_integer()) -> term().
call_shard_fallback_remote(SessionId, Request, Timeout) ->
    call_via_manager_remote(SessionId, Request, Timeout).

-spec call_owner_manager(session_id(), term(), pos_integer()) -> term().
call_owner_manager(SessionId, Request, Timeout) ->
    case owner_scope(SessionId) of
        local ->
            call_shard(SessionId, Request, Timeout);
        {remote, OwnerNode} ->
            call_remote_shard(SessionId, OwnerNode, Request, Timeout);
        unavailable ->
            {error, unavailable}
    end.

-spec call_remote_shard(session_id(), node(), term(), pos_integer()) -> term().
call_remote_shard(SessionId, OwnerNode, Request, Timeout) ->
    Result =
        try
            rpc:call(
                OwnerNode,
                session_manager,
                call_shard,
                [SessionId, Request, Timeout],
                Timeout + 1000
            )
        catch
            error:_ -> {badrpc, unavailable};
            exit:_ -> {badrpc, unavailable}
        end,
    case Result of
        {badrpc, timeout} ->
            {error, timeout};
        {badrpc, nodedown} ->
            {error, unavailable};
        {badrpc, _} ->
            {error, unavailable};
        Reply ->
            Reply
    end.

-spec handle_owner_call(session_id(), term(), pos_integer(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_owner_call(SessionId, Request, Timeout, From, State) ->
    case owner_scope(SessionId) of
        local ->
            {Reply, NewState} = execute_owner_request(SessionId, Request, Timeout, State),
            {reply, Reply, NewState};
        {remote, OwnerNode} ->
            spawn_owner_reply_worker(SessionId, OwnerNode, Request, Timeout, From),
            {noreply, State};
        unavailable ->
            {reply, {error, unavailable}, State}
    end.

-spec spawn_owner_reply_worker(session_id(), node(), term(), pos_integer(), gen_server:from()) ->
    pid().
spawn_owner_reply_worker(SessionId, OwnerNode, Request, Timeout, From) ->
    spawn(fun() ->
        erlang:process_flag(fullsweep_after, 0),
        Reply = call_owner_manager_node(
            SessionId, OwnerNode, Request, Timeout, [node()], ?OWNER_REDIRECT_LIMIT
        ),
        gen_server:reply(From, Reply)
    end).

-spec execute_owner_request(session_id(), term(), pos_integer(), state()) -> {term(), state()}.
execute_owner_request(_SessionId, {start, Request, SocketPid}, _Timeout, State) when
    is_map(Request), is_pid(SocketPid)
->
    start_call_with_drain_guard(gateway_node_router:is_draining(), Request, SocketPid, State);
execute_owner_request(SessionId, {lookup, SessionId}, _Timeout, State) ->
    forward_call(SessionId, {lookup, SessionId}, ?LOOKUP_TIMEOUT, State);
execute_owner_request(
    SessionId, {lookup_or_rehydrate, SessionId, Token, SocketPid}, _Timeout, State
) ->
    Request = {lookup_or_rehydrate, SessionId, Token, SocketPid},
    forward_call(SessionId, Request, ?LOOKUP_TIMEOUT, State);
execute_owner_request(SessionId, Request, Timeout, State) ->
    forward_call(SessionId, Request, Timeout, State).

-spec owner_scope(session_id()) -> local | {remote, node()} | unavailable.
owner_scope(SessionId) ->
    LocalNode = node(),
    try gateway_node_router:owner_node_result(SessionId, sessions) of
        {ok, OwnerNode} when OwnerNode =:= LocalNode ->
            local;
        {ok, OwnerNode} when is_atom(OwnerNode) ->
            {remote, OwnerNode};
        {error, _Reason} ->
            unavailable
    catch
        error:_ -> unavailable;
        exit:_ -> unavailable
    end.

-spec call_owner_manager_node(
    session_id(), node(), term(), pos_integer(), [node()], non_neg_integer()
) -> term().
call_owner_manager_node(_SessionId, TargetNode, _Request, _Timeout, SeenNodes, 0) ->
    {error, {owner_redirect_loop, TargetNode, lists:reverse(SeenNodes)}};
call_owner_manager_node(SessionId, TargetNode, Request, Timeout, SeenNodes, _RedirectsLeft) when
    TargetNode =:= node()
->
    case lists:member(TargetNode, SeenNodes) of
        true ->
            call_shard(SessionId, Request, Timeout);
        false ->
            call_owner_manager(SessionId, Request, Timeout)
    end;
call_owner_manager_node(SessionId, TargetNode, Request, Timeout, SeenNodes, RedirectsLeft) ->
    case safe_gen_call(session_manager_server_ref(TargetNode), Request, Timeout + 1000) of
        {ok, {error, {not_owner, NextNode}}} when is_atom(NextNode) ->
            handle_redirect(SessionId, NextNode, Request, Timeout, SeenNodes, RedirectsLeft);
        {ok, Reply} ->
            Reply;
        {exit, {timeout, _}} ->
            {error, timeout};
        {exit, {nodedown, _}} ->
            {error, unavailable};
        {exit, {noproc, _}} ->
            {error, unavailable};
        _ ->
            {error, unavailable}
    end.

-spec handle_redirect(session_id(), node(), term(), pos_integer(), [node()], non_neg_integer()) ->
    term().
handle_redirect(SessionId, NextNode, Request, Timeout, SeenNodes, RedirectsLeft) ->
    case lists:member(NextNode, SeenNodes) of
        true ->
            {error, {owner_redirect_loop, NextNode, lists:reverse(SeenNodes)}};
        false ->
            call_owner_manager_node(
                SessionId,
                NextNode,
                Request,
                Timeout,
                [NextNode | SeenNodes],
                RedirectsLeft - 1
            )
    end.

-spec session_manager_server_ref(node()) -> atom() | {atom(), node()}.
session_manager_server_ref(TargetNode) when TargetNode =:= node() ->
    session_manager;
session_manager_server_ref(TargetNode) ->
    {session_manager, TargetNode}.

-spec start_call_with_drain_guard(boolean(), map(), pid(), state()) -> {term(), state()}.
start_call_with_drain_guard(true, _Request, _SocketPid, State) ->
    {{error, draining}, State};
start_call_with_drain_guard(false, Request, SocketPid, State) ->
    SessionId = maps:get(session_id, Request),
    forward_call(SessionId, {start, Request, SocketPid}, ?START_TIMEOUT, State).

-spec call_via_manager(session_id(), term(), pos_integer()) -> term().
call_via_manager(SessionId, Request, Timeout) ->
    case fluxer_gateway_sup:role_enabled(sessions) of
        true ->
            call_via_manager_local(SessionId, Request, Timeout);
        false ->
            call_via_manager_remote(SessionId, Request, Timeout)
    end.

-spec call_via_manager_local(session_id(), term(), pos_integer()) -> term().
call_via_manager_local(SessionId, Request, Timeout) ->
    ProxyRequest = {proxy_call, SessionId, Request, Timeout},
    case safe_gen_call(session_manager, ProxyRequest, Timeout + 1000) of
        {ok, Reply} -> Reply;
        {exit, {timeout, _}} -> {error, timeout};
        _ -> {error, unavailable}
    end.

-spec call_via_manager_remote(session_id(), term(), pos_integer()) -> term().
call_via_manager_remote(SessionId, Request, Timeout) ->
    case owner_scope(SessionId) of
        {remote, OwnerNode} ->
            call_remote_shard(SessionId, OwnerNode, Request, Timeout);
        _ ->
            {error, unavailable}
    end.

-spec forward_call(session_id(), term(), pos_integer(), state()) -> {term(), state()}.
forward_call(SessionId, Request, Timeout, State) ->
    {Index, State1} = session_manager:ensure_shard(SessionId, State),
    Shards = maps:get(shards, State1),
    case maps:get(Index, Shards, undefined) of
        #{pid := Pid} ->
            forward_call_to_pid(Pid, Index, Request, Timeout, State1);
        _ ->
            {{error, unavailable}, State1}
    end.

-spec forward_call_to_pid(pid(), non_neg_integer(), term(), pos_integer(), state()) ->
    {term(), state()}.
forward_call_to_pid(Pid, Index, Request, Timeout, State) ->
    case safe_gen_call(Pid, Request, Timeout) of
        {ok, Reply} -> {Reply, State};
        _ -> retry_forward_call(Index, Request, Timeout, State)
    end.

-spec retry_forward_call(non_neg_integer(), term(), pos_integer(), state()) ->
    {term(), state()}.
retry_forward_call(Index, Request, Timeout, State) ->
    {_Result, State2} = session_manager:restart_shard(Index, State),
    Shards2 = maps:get(shards, State2),
    case maps:get(Index, Shards2, undefined) of
        #{pid := RetryPid} ->
            retry_forward_call_to_pid(RetryPid, Request, Timeout, State2);
        _ ->
            {{error, unavailable}, State2}
    end.

-spec retry_forward_call_to_pid(pid(), term(), pos_integer(), state()) -> {term(), state()}.
retry_forward_call_to_pid(RetryPid, Request, Timeout, State) ->
    case safe_gen_call(RetryPid, Request, Timeout) of
        {ok, Reply} -> {Reply, State};
        _ -> {{error, unavailable}, State}
    end.

-spec safe_gen_call(gen_server:server_ref(), term(), pos_integer()) -> safe_call_result().
safe_gen_call(Server, Request, Timeout) ->
    try gen_server:call(Server, Request, Timeout) of
        Reply -> {ok, Reply}
    catch
        error:Reason -> {error, Reason};
        exit:Reason -> {exit, Reason}
    end.

-spec select_shard(session_id(), pos_integer()) -> non_neg_integer().
select_shard(SessionId, Count) when Count > 0 ->
    rendezvous_router:select(SessionId, Count).

-spec shard_pid_from_table(session_id()) -> {ok, pid()} | error.
shard_pid_from_table(SessionId) ->
    try
        case ets:lookup(?SHARD_TABLE, shard_count) of
            [{shard_count, ShardCount}] when is_integer(ShardCount), ShardCount > 0 ->
                lookup_shard_pid(SessionId, ShardCount);
            _ ->
                error
        end
    catch
        error:badarg ->
            error
    end.

-spec lookup_shard_pid(session_id(), pos_integer()) -> {ok, pid()} | error.
lookup_shard_pid(SessionId, ShardCount) ->
    Index = select_shard(SessionId, ShardCount),
    case ets:lookup(?SHARD_TABLE, {shard_pid, Index}) of
        [{{shard_pid, Index}, Pid}] when is_pid(Pid) ->
            live_shard_pid(Pid);
        _ ->
            error
    end.

-spec live_shard_pid(pid()) -> {ok, pid()} | error.
live_shard_pid(Pid) ->
    case process_liveness:is_alive(Pid) of
        true -> {ok, Pid};
        false -> error
    end.
