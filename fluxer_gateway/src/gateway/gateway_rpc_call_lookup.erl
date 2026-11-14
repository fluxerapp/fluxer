%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_call_lookup).
-typing([eqwalizer]).

-export([call_owner_call_manager/3, lookup_call/1, safe_gen_server_call/3, with_call/2]).

-define(CALL_LOOKUP_TIMEOUT, 2000).

-spec lookup_call(integer()) -> {ok, pid()} | not_found | {error, term()}.
lookup_call(ChannelId) ->
    case resolve_owner_node(ChannelId) of
        {ok, OwnerNode} ->
            normalize_lookup_call_result(lookup_call_on_node(OwnerNode, ChannelId));
        {error, unavailable} ->
            {error, unavailable}
    end.

-spec normalize_lookup_call_result({ok, pid()} | {error, term()}) ->
    {ok, pid()} | not_found | {error, term()}.
normalize_lookup_call_result({ok, Pid}) when is_pid(Pid) ->
    {ok, Pid};
normalize_lookup_call_result({error, not_found}) ->
    not_found;
normalize_lookup_call_result({error, _Reason} = Error) ->
    Error.

-spec with_call(integer(), fun((pid()) -> T)) -> T when T :: term().
with_call(ChannelId, Fun) ->
    case lookup_call(ChannelId) of
        {ok, Pid} ->
            Fun(Pid);
        not_found ->
            gateway_rpc_error:raise(<<"call_not_found">>);
        {error, _Reason} ->
            gateway_rpc_error:raise(<<"call_lookup_error">>)
    end.

-spec call_owner_call_manager(integer(), term(), timeout()) -> term().
call_owner_call_manager(ChannelId, Request, Timeout) ->
    case resolve_owner_node(ChannelId) of
        {ok, OwnerNode} -> call_call_manager_node(OwnerNode, Request, Timeout);
        {error, unavailable} -> {error, unavailable}
    end.

-spec safe_gen_server_call(pid(), term(), timeout()) ->
    {ok, term()} | {error, not_found | timeout | unavailable}.
safe_gen_server_call(Pid, Request, Timeout) ->
    try gen_server:call(Pid, Request, Timeout) of
        Reply ->
            {ok, Reply}
    catch
        exit:{timeout, _} ->
            {error, timeout};
        exit:{nodedown, _} ->
            {error, unavailable};
        exit:{noproc, _} ->
            {error, not_found};
        exit:_Reason ->
            {error, unavailable}
    end.

-spec lookup_call_on_node(node(), integer()) ->
    {ok, pid()} | {error, not_found | timeout | unavailable | term()}.
lookup_call_on_node(TargetNode, ChannelId) ->
    case call_call_manager_node(TargetNode, {lookup, ChannelId}, ?CALL_LOOKUP_TIMEOUT) of
        {ok, Pid} when is_pid(Pid) ->
            {ok, Pid};
        {error, not_found} ->
            {error, not_found};
        {error, _Reason} = Error ->
            Error;
        not_found ->
            {error, not_found};
        _ ->
            {error, invalid_reply}
    end.

-spec call_manager_server_ref(node()) -> atom() | {atom(), node()}.
call_manager_server_ref(TargetNode) when TargetNode =:= node() ->
    call_manager;
call_manager_server_ref(TargetNode) ->
    {call_manager, TargetNode}.

-spec resolve_owner_node(integer()) -> {ok, node()} | {error, unavailable}.
resolve_owner_node(ChannelId) ->
    resolve_owner_node(ChannelId, fun owner_node_result_for_call/1).

-spec owner_node_result_for_call(integer()) -> {ok, node()} | unavailable.
owner_node_result_for_call(ChannelId) ->
    case gateway_node_router:owner_node_result(ChannelId, calls) of
        {ok, Node} when is_atom(Node) -> {ok, Node};
        _ -> unavailable
    end.

-spec resolve_owner_node(integer(), fun((integer()) -> term())) ->
    {ok, node()} | {error, unavailable}.
resolve_owner_node(ChannelId, OwnerResolver) ->
    try OwnerResolver(ChannelId) of
        {ok, OwnerNode} when is_atom(OwnerNode) ->
            maybe_valid_node(OwnerNode);
        _ ->
            {error, unavailable}
    catch
        error:_Reason ->
            {error, unavailable};
        exit:_Reason ->
            {error, unavailable};
        throw:_Reason ->
            {error, unavailable}
    end.

-spec maybe_valid_node(node()) -> {ok, node()} | {error, unavailable}.
maybe_valid_node(OwnerNode) ->
    case lists:member($@, atom_to_list(OwnerNode)) of
        true ->
            {ok, OwnerNode};
        false ->
            {error, unavailable}
    end.

call_call_manager_node(TargetNode, Request, Timeout) ->
    try gen_server:call(call_manager_server_ref(TargetNode), Request, Timeout) of
        Reply ->
            Reply
    catch
        exit:{timeout, _} ->
            {error, timeout};
        exit:{nodedown, _} ->
            {error, unavailable};
        exit:{noproc, _} ->
            {error, unavailable};
        exit:_Reason ->
            {error, unavailable}
    end.
