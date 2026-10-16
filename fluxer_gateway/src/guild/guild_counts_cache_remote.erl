%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_counts_cache_remote).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    safe_remote_call/3, safe_remote_call/4,
    start_remote_pending_handoff/4, start_remote_pending_handoff/5,
    start_remote_get_reply/3,
    start_remote_bulk_get_reply/3,
    handle_remote_handoff_result/5,
    normalize_get_reply/1,
    fetch_remote_bulk_counts/2,
    fetch_remote_bulk_groups/1,
    pending_handoffs/1,
    set_pending_handoffs/2,
    enqueue_pending_upsert/4,
    enqueue_pending_delete/2,
    clear_pending_handoff/2,
    pending_handoff_count_from_state/1,
    sanitize_pending_handoffs/1,
    normalize_pending_operation/1,
    cap_pending_handoffs/1,
    rebalance_ownership/1,
    pending_operation_request/2,
    pending_operation_success_action/1,
    merge_pending_handoffs_with_snapshot/2
]).

-type guild_id() :: integer().
-type counts() :: {non_neg_integer(), non_neg_integer()}.
-type pending_operation() :: {upsert, counts()} | delete.
-type handoff_success_action() :: keep_local | delete_local | delete_local_if_remote_owner.
-type pending_handoffs() :: #{guild_id() => pending_operation()}.

-export_type([
    guild_id/0,
    counts/0,
    pending_operation/0,
    handoff_success_action/0,
    pending_handoffs/0
]).

-define(REMOTE_CALL_TIMEOUT_MS, 1000).
-define(PENDING_HANDOFFS_CAP, 10000).
-define(PENDING_HANDOFFS_WARN_THRESHOLD, 1000).

-spec safe_remote_call(node(), term(), term()) -> term().
safe_remote_call(TargetNode, Request, Fallback) ->
    safe_remote_call(TargetNode, Request, Fallback, ?REMOTE_CALL_TIMEOUT_MS).

-spec safe_remote_call(node(), term(), term(), pos_integer()) -> term().
safe_remote_call(TargetNode, Request, Fallback, Timeout) when
    is_integer(Timeout), Timeout > 0
->
    try
        gen_server:call({guild_counts_cache, TargetNode}, Request, Timeout)
    catch
        error:_ -> Fallback;
        exit:_ -> Fallback
    end.

-spec start_remote_pending_handoff(node(), guild_id(), pending_operation(), term()) -> ok.
start_remote_pending_handoff(OwnerNode, GuildId, Operation, Request) ->
    start_remote_pending_handoff(OwnerNode, GuildId, Operation, keep_local, Request).

-spec start_remote_pending_handoff(
    node(), guild_id(), pending_operation(), handoff_success_action(), term()
) -> ok.
start_remote_pending_handoff(Node, GId, Op, Action, Req) ->
    _ = spawn(fun() ->
        Result = safe_remote_call(Node, Req, {error, unavailable}),
        gen_server:cast(guild_counts_cache, {remote_handoff_result, GId, Op, Action, Result})
    end),
    ok.

-spec start_remote_get_reply(gen_server:from(), node(), guild_id()) -> ok.
start_remote_get_reply(From, OwnerNode, GuildId) ->
    _ = spawn(fun() ->
        Reply = safe_remote_call(OwnerNode, {get_local, GuildId}, {error, unavailable}),
        gen_server:reply(From, normalize_get_reply(Reply))
    end),
    ok.

-spec normalize_get_reply(term()) -> {ok, non_neg_integer(), non_neg_integer()} | miss.
normalize_get_reply({ok, MC, OC}) when
    is_integer(MC), MC >= 0, is_integer(OC), OC >= 0
->
    {ok, MC, OC};
normalize_get_reply(_) ->
    miss.

-spec start_remote_bulk_get_reply(
    gen_server:from(), #{guild_id() => counts()}, [{node(), [guild_id()]}]
) -> ok.
start_remote_bulk_get_reply(From, LocalMap, RemoteGroups) ->
    _ = spawn(fun() ->
        RemoteMap = fetch_remote_bulk_groups(RemoteGroups),
        gen_server:reply(From, maps:merge(LocalMap, RemoteMap))
    end),
    ok.

-spec fetch_remote_bulk_counts(node(), [guild_id()]) -> #{guild_id() => counts()}.
fetch_remote_bulk_counts(OwnerNode, GuildIds) ->
    case safe_remote_call(OwnerNode, {bulk_get_local, GuildIds}, #{}) of
        Reply when is_map(Reply) -> normalize_counts_map(Reply);
        _ -> #{}
    end.

-spec normalize_counts_map(map()) -> #{guild_id() => counts()}.
normalize_counts_map(Reply) ->
    maps:fold(
        fun
            (GuildId, {MC, OC}, Acc) when
                is_integer(GuildId), is_integer(MC), MC >= 0, is_integer(OC), OC >= 0
            ->
                Acc#{GuildId => {MC, OC}};
            (_GuildId, _Counts, Acc) ->
                Acc
        end,
        #{},
        Reply
    ).

-spec fetch_remote_bulk_groups([{node(), [guild_id()]}]) -> #{guild_id() => counts()}.
fetch_remote_bulk_groups(RemoteGroups) ->
    lists:foldl(
        fun({OwnerNode, OwnerGuildIds}, AccMap) ->
            maps:merge(AccMap, fetch_remote_bulk_counts(OwnerNode, OwnerGuildIds))
        end,
        #{},
        RemoteGroups
    ).

-spec handle_remote_handoff_result(
    guild_id(), pending_operation(), handoff_success_action(), term(), map()
) -> map().
handle_remote_handoff_result(GuildId, Operation, SuccessAction, ok, State) ->
    case maps:get(GuildId, pending_handoffs(State), undefined) of
        Operation ->
            State1 = apply_success(GuildId, Operation, SuccessAction, State),
            guild_counts_cache_shard_mgmt:refresh_rebalance_retry_timer(
                clear_pending_handoff(GuildId, State1)
            );
        _Other ->
            guild_counts_cache_shard_mgmt:refresh_rebalance_retry_timer(State)
    end;
handle_remote_handoff_result(_GuildId, _Op, _Action, _Result, State) ->
    guild_counts_cache_shard_mgmt:refresh_rebalance_retry_timer(State).

-spec apply_success(guild_id(), pending_operation(), handoff_success_action(), map()) -> map().
apply_success(_GuildId, _Operation, keep_local, State) ->
    State;
apply_success(GuildId, _Operation, delete_local, State) ->
    {_Reply, State1} = guild_counts_cache_query:delete_local(GuildId, State),
    State1;
apply_success(GuildId, {upsert, _}, delete_local_if_remote_owner, State) ->
    case guild_counts_cache_query:resolve_owner_node_safe(GuildId) of
        LocalNode when LocalNode =:= node() -> State;
        unavailable ->
            State;
        RemoteNode when is_atom(RemoteNode) ->
            {_Reply, S1} = guild_counts_cache_query:delete_local(GuildId, State),
            S1
    end;
apply_success(_GuildId, delete, delete_local_if_remote_owner, State) ->
    State.

-spec pending_handoffs(map()) -> pending_handoffs().
pending_handoffs(State) ->
    sanitize_pending_handoffs(maps:get(pending_handoffs, State, #{})).

-spec sanitize_pending_handoffs(term()) -> pending_handoffs().
sanitize_pending_handoffs(Pending) when is_map(Pending) ->
    maps:fold(
        fun sanitize_entry/3,
        #{},
        Pending
    );
sanitize_pending_handoffs(_) ->
    #{}.

-spec sanitize_entry(term(), term(), pending_handoffs()) -> pending_handoffs().
sanitize_entry(GId, Op, Acc) when is_integer(GId), GId > 0 ->
    case normalize_pending_operation(Op) of
        invalid -> Acc;
        Normalized -> Acc#{GId => Normalized}
    end;
sanitize_entry(_GId, _Op, Acc) ->
    Acc.

-spec normalize_pending_operation(term()) -> pending_operation() | invalid.
normalize_pending_operation({upsert, {MC, OC}}) when
    is_integer(MC), MC >= 0, is_integer(OC), OC >= 0
->
    {upsert, {MC, OC}};
normalize_pending_operation(delete) ->
    delete;
normalize_pending_operation(_) ->
    invalid.

-spec set_pending_handoffs(pending_handoffs(), map()) -> map().
set_pending_handoffs(Pending, State) ->
    State#{pending_handoffs => sanitize_pending_handoffs(Pending)}.

-spec enqueue_pending_upsert(guild_id(), non_neg_integer(), non_neg_integer(), map()) -> map().
enqueue_pending_upsert(GuildId, MC, OC, State) ->
    Pending = pending_handoffs(State),
    Updated = Pending#{GuildId => {upsert, {MC, OC}}},
    set_pending_handoffs(cap_pending_handoffs(Updated), State).

-spec enqueue_pending_delete(guild_id(), map()) -> map().
enqueue_pending_delete(GuildId, State) ->
    Pending = pending_handoffs(State),
    set_pending_handoffs(cap_pending_handoffs(Pending#{GuildId => delete}), State).

-spec cap_pending_handoffs(pending_handoffs()) -> pending_handoffs().
cap_pending_handoffs(Pending) ->
    Size = maps:size(Pending),
    case Size > ?PENDING_HANDOFFS_WARN_THRESHOLD of
        true when Size > ?PENDING_HANDOFFS_CAP ->
            Keys = lists:sort(maps:keys(Pending)),
            KeepKeys = lists:nthtail(Size - ?PENDING_HANDOFFS_CAP, Keys),
            maps:with(KeepKeys, Pending);
        true ->
            Pending;
        false ->
            Pending
    end.

-spec clear_pending_handoff(guild_id(), map()) -> map().
clear_pending_handoff(GuildId, State) ->
    set_pending_handoffs(maps:remove(GuildId, pending_handoffs(State)), State).

-spec pending_handoff_count_from_state(map()) -> non_neg_integer().
pending_handoff_count_from_state(State) ->
    maps:size(pending_handoffs(State)).

-spec rebalance_ownership(map()) -> map().
rebalance_ownership(State) ->
    Snapshot = guild_counts_cache_query:local_snapshot(State),
    Pending1 = merge_pending_handoffs_with_snapshot(Snapshot, pending_handoffs(State)),
    State1 = set_pending_handoffs(Pending1, State),
    {Remaining, State2} = start_pending_handoff_attempts(Pending1, State1),
    guild_counts_cache_shard_mgmt:refresh_rebalance_retry_timer(
        set_pending_handoffs(Remaining, State2)
    ).

-spec start_pending_handoff_attempts(pending_handoffs(), map()) -> {pending_handoffs(), map()}.
start_pending_handoff_attempts(Pending, State) ->
    Resolver = fun guild_counts_cache_query:resolve_owner_node/1,
    maps:fold(
        fun(GId, Op, {AccP, AccS}) ->
            attempt_single_handoff(GId, Op, Resolver, AccP, AccS)
        end,
        {#{}, State},
        sanitize_pending_handoffs(Pending)
    ).

-spec attempt_single_handoff(
    guild_id(),
    pending_operation(),
    fun((guild_id()) -> term()),
    pending_handoffs(),
    map()
) -> {pending_handoffs(), map()}.
attempt_single_handoff(GId, Op, Resolver, AccP, AccS) ->
    case guild_counts_cache_query:resolve_owner_node_safe(GId, Resolver) of
        LocalNode when LocalNode =:= node() ->
            {AccP, AccS};
        unavailable ->
            {AccP#{GId => Op}, AccS};
        OwnerNode when is_atom(OwnerNode) ->
            Req = pending_operation_request(GId, Op),
            Action = pending_operation_success_action(Op),
            start_remote_pending_handoff(OwnerNode, GId, Op, Action, Req),
            {AccP#{GId => Op}, AccS}
    end.

-spec pending_operation_request(guild_id(), pending_operation()) -> term().
pending_operation_request(GId, {upsert, {MC, OC}}) -> {update_local, GId, MC, OC};
pending_operation_request(GId, delete) -> {delete_local, GId}.

-spec pending_operation_success_action(pending_operation()) -> handoff_success_action().
pending_operation_success_action({upsert, _}) -> delete_local_if_remote_owner;
pending_operation_success_action(delete) -> keep_local.

-spec merge_pending_handoffs_with_snapshot(
    #{guild_id() => counts()}, pending_handoffs()
) -> pending_handoffs().
merge_pending_handoffs_with_snapshot(Snapshot, Pending) ->
    maps:fold(
        fun
            (GId, {MC, OC}, AccP) when
                is_integer(GId),
                GId > 0,
                is_integer(MC),
                MC >= 0,
                is_integer(OC),
                OC >= 0
            ->
                merge_single_entry(GId, MC, OC, AccP);
            (_GId, _Counts, AccP) ->
                AccP
        end,
        sanitize_pending_handoffs(Pending),
        Snapshot
    ).

-spec merge_single_entry(guild_id(), non_neg_integer(), non_neg_integer(), pending_handoffs()) ->
    pending_handoffs().
merge_single_entry(GId, MC, OC, AccP) ->
    case guild_counts_cache_query:resolve_owner_node_safe(GId) of
        LocalNode when LocalNode =:= node() -> maps:remove(GId, AccP);
        _OwnerNode -> AccP#{GId => {upsert, {MC, OC}}}
    end.
