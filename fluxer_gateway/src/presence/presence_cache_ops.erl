%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_cache_ops).
-typing([eqwalizer]).

-export([
    handle_put/3,
    handle_delete/2,
    get_local/2
]).

-export_type([state/0]).

-type state() :: map().
-type pending_operation() :: {put, map()} | delete.

-spec handle_put(integer(), map(), state()) -> state().
handle_put(UserId, Presence, State) ->
    OwnerNodes = presence_cache_bulk:resolve_owner_nodes(UserId),
    {Replicated, State1} = replicate_to_nodes(UserId, {put, Presence}, OwnerNodes, State),
    case Replicated of
        true ->
            presence_cache_pending:clear_pending_operation(UserId, State1);
        false ->
            presence_cache_pending:queue_pending_operation(UserId, {put, Presence}, State1)
    end.

-spec handle_delete(integer(), state()) -> state().
handle_delete(UserId, State) ->
    OwnerNodes = presence_cache_bulk:resolve_owner_nodes(UserId),
    {Replicated, State1} = replicate_to_nodes(UserId, delete, OwnerNodes, State),
    case Replicated of
        true -> presence_cache_pending:clear_pending_operation(UserId, State1);
        false -> presence_cache_pending:queue_pending_operation(UserId, delete, State1)
    end.

-spec get_local(integer(), state()) -> {{ok, map()} | not_found, state()}.
get_local(UserId, State) ->
    {Reply, NewState} = presence_cache_shards:forward_call(UserId, {get, UserId}, State),
    {presence_cache_bulk:normalize_get_reply(Reply), NewState}.

-spec replicate_to_nodes(integer(), pending_operation(), [node()], state()) ->
    {boolean(), state()}.
replicate_to_nodes(_UserId, _Operation, [], State) ->
    {false, State};
replicate_to_nodes(UserId, Operation, OwnerNodes, State) ->
    TargetNodes = lists:usort(OwnerNodes),
    lists:foldl(
        fun(TargetNode, {AllReplicated, AccState}) ->
            {Result, NextState} = apply_op(TargetNode, UserId, Operation, AccState),
            {AllReplicated andalso Result =:= ok, NextState}
        end,
        {true, State},
        TargetNodes
    ).

-spec apply_op(node(), integer(), pending_operation(), state()) -> {ok | error, state()}.
apply_op(TargetNode, UserId, {put, Presence}, State) ->
    case TargetNode =:= node() of
        true ->
            {_Reply, NewState} = presence_cache:put_local(UserId, Presence, State),
            {ok, NewState};
        false ->
            remote_apply_op(TargetNode, {put_local, UserId, Presence}, State)
    end;
apply_op(TargetNode, UserId, delete, State) ->
    case TargetNode =:= node() of
        true ->
            {_Reply, NewState} = presence_cache:delete_local(UserId, State),
            {ok, NewState};
        false ->
            remote_apply_op(TargetNode, {delete_local, UserId}, State)
    end.

-spec remote_apply_op(node(), term(), state()) -> {ok | error, state()}.
remote_apply_op(TargetNode, Request, State) ->
    case presence_cache_bulk:safe_remote_call(TargetNode, Request, {error, unavailable}) of
        ok -> {ok, State};
        _ -> {error, State}
    end.
