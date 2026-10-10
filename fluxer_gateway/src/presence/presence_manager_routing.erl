%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_manager_routing).
-typing([eqwalizer]).

-export([
    call_owner_manager/3,
    call_via_manager/2,
    call_via_manager_local/2,
    resolve_owner_node/1
]).

-export_type([user_id/0, owner_route/0]).

-type user_id() :: integer().

-type owner_route() :: {ok, node()} | unavailable.

-spec call_owner_manager(user_id(), term(), pos_integer()) -> term().
call_owner_manager(UserId, Request, Timeout) ->
    case resolve_owner_node(UserId) of
        {ok, OwnerNode} when OwnerNode =:= node() -> call_shard(UserId, Request, Timeout);
        {ok, OwnerNode} -> call_presence_manager_node(OwnerNode, Request, Timeout);
        unavailable -> {error, unavailable}
    end.

-spec call_via_manager(term(), pos_integer()) -> term().
call_via_manager(Request, Timeout) ->
    case fluxer_gateway_sup:role_enabled(presence) of
        true -> call_via_manager_local(Request, Timeout);
        false -> call_via_owner(Request, Timeout)
    end.

-spec call_via_manager_local(term(), pos_integer()) -> term().
call_via_manager_local(Request, Timeout) ->
    case shard_utils:safe_gen_call_wrapped(presence_manager, Request, Timeout + 1000) of
        {exit, {timeout, _}} -> {error, timeout};
        {exit, _} -> {error, unavailable};
        {ok, Reply} -> Reply
    end.

-spec resolve_owner_node(user_id()) -> owner_route().
resolve_owner_node(UserId) ->
    case gateway_node_router:owner_node_result(UserId, presence) of
        {ok, OwnerNode} when is_atom(OwnerNode) -> {ok, OwnerNode};
        {error, _Reason} -> unavailable
    end.

-spec call_presence_manager_node(node(), term(), pos_integer()) -> term().
call_presence_manager_node(TargetNode, Request, Timeout) ->
    case
        shard_utils:safe_gen_call_wrapped(
            presence_manager_server_ref(TargetNode), Request, Timeout
        )
    of
        {exit, {timeout, _}} -> {error, timeout};
        {exit, {nodedown, _}} -> {error, unavailable};
        {exit, {noproc, _}} -> {error, unavailable};
        {exit, _} -> {error, unavailable};
        {ok, Reply} -> Reply
    end.

-spec presence_manager_server_ref(node()) -> atom() | {atom(), node()}.
presence_manager_server_ref(TargetNode) when TargetNode =:= node() ->
    presence_manager;
presence_manager_server_ref(TargetNode) ->
    {presence_manager, TargetNode}.

-spec call_shard(user_id(), term(), pos_integer()) -> term().
call_shard(Key, Request, Timeout) ->
    case presence_manager_shards:pid_from_table(Key) of
        {ok, Pid} -> call_shard_pid(Pid, Request, Timeout);
        error -> call_via_manager(Request, Timeout)
    end.

-spec call_shard_pid(pid(), term(), pos_integer()) -> term().
call_shard_pid(Pid, Request, Timeout) ->
    case shard_utils:safe_gen_call_wrapped(Pid, Request, Timeout) of
        {exit, {timeout, _}} -> {error, timeout};
        {exit, _} -> call_via_manager(Request, Timeout);
        {ok, Reply} -> Reply
    end.

-spec call_via_owner(term(), pos_integer()) -> term().
call_via_owner(Request, Timeout) ->
    case extract_user_id_from_request(Request) of
        undefined -> call_via_manager_local(Request, Timeout);
        UserId -> call_request_owner(UserId, Request, Timeout)
    end.

-spec call_request_owner(user_id(), term(), pos_integer()) -> term().
call_request_owner(UserId, Request, Timeout) ->
    case safe_owner_node(UserId) of
        {ok, OwnerNode} when OwnerNode =:= node() -> call_via_manager_local(Request, Timeout);
        {ok, OwnerNode} -> call_remote_manager(OwnerNode, Request, Timeout);
        unavailable -> {error, unavailable}
    end.

-spec safe_owner_node(user_id()) -> owner_route().
safe_owner_node(UserId) ->
    try gateway_node_router:owner_node_result(UserId, presence) of
        {ok, OwnerNode} when is_atom(OwnerNode) -> {ok, OwnerNode};
        {error, _Reason} -> unavailable
    catch
        error:_ -> unavailable;
        exit:_ -> unavailable
    end.

-spec call_remote_manager(node(), term(), pos_integer()) -> term().
call_remote_manager(OwnerNode, Request, Timeout) ->
    case
        rpc:call(
            OwnerNode,
            presence_manager,
            call_via_manager_local,
            [Request, Timeout],
            Timeout + 1000
        )
    of
        {badrpc, _} -> {error, unavailable};
        Reply -> Reply
    end.

-spec extract_user_id_from_request(term()) -> user_id() | undefined.
extract_user_id_from_request({lookup, UserId}) when is_integer(UserId) ->
    UserId;
extract_user_id_from_request({dispatch, UserId, _, _}) when is_integer(UserId) ->
    UserId;
extract_user_id_from_request({start_or_lookup, #{user_id := UserId}}) when is_integer(UserId) ->
    UserId;
extract_user_id_from_request(_) ->
    undefined.
