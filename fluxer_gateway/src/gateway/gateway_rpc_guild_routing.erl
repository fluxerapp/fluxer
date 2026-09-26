%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild_routing).

-typing([eqwalizer]).

-export([
    guild_owner_scope/1, guild_owner_scope/3,
    resolve_owner_node/3,
    group_guild_ids_by_owner/1, group_guild_ids_by_owner/2,
    owner_groups_for_reload_all/1, owner_groups_for_reload_all/2,
    call_owner_guild_manager/3,
    safe_guild_counts_get/1,
    process_batch/3,
    validate_batch_size/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(MAX_BATCH_SIZE, 100).

-spec guild_owner_scope(integer()) ->
    local | {remote, node()} | unavailable.
guild_owner_scope(GuildId) ->
    Resolver = fun(Gid) ->
        gateway_node_router:owner_node_result(Gid, guilds)
    end,
    guild_owner_scope(GuildId, node(), Resolver).

-spec guild_owner_scope(
    integer(), node(), fun((integer()) -> term())
) -> local | {remote, node()} | unavailable.
guild_owner_scope(GuildId, LocalNode, OwnerResolver) ->
    case resolve_owner_node(GuildId, LocalNode, OwnerResolver) of
        {ok, OwnerNode} when OwnerNode =:= LocalNode -> local;
        {ok, OwnerNode} -> {remote, OwnerNode};
        unavailable -> unavailable
    end.

-spec resolve_owner_node(
    integer(), node(), fun((integer()) -> term())
) -> {ok, node()} | unavailable.
resolve_owner_node(GuildId, _LocalNode, OwnerResolver) ->
    try OwnerResolver(GuildId) of
        {ok, OwnerNode} when is_atom(OwnerNode) ->
            validate_node(OwnerNode);
        {error, _Reason} ->
            unavailable;
        OwnerNode when is_atom(OwnerNode) ->
            validate_node(OwnerNode);
        _ ->
            unavailable
    catch
        throw:_Reason -> unavailable;
        error:_Reason -> unavailable;
        exit:_Reason -> unavailable
    end.

-spec validate_node(node()) -> {ok, node()} | unavailable.
validate_node(OwnerNode) ->
    case lists:member($@, atom_to_list(OwnerNode)) of
        true -> {ok, OwnerNode};
        false -> unavailable
    end.

-spec group_guild_ids_by_owner([integer()]) ->
    [{node(), [integer()]}].
group_guild_ids_by_owner(GuildIds) ->
    Resolver = fun(Gid) ->
        gateway_node_router:owner_node_result(Gid, guilds)
    end,
    group_guild_ids_by_owner(GuildIds, Resolver).

-spec group_guild_ids_by_owner(
    [integer()], fun((integer()) -> term())
) -> [{node(), [integer()]}].
group_guild_ids_by_owner(GuildIds, OwnerResolver) ->
    GroupedMap = lists:foldl(
        fun(GuildId, Acc) ->
            fold_guild_owner(GuildId, OwnerResolver, Acc)
        end,
        #{},
        GuildIds
    ),
    lists:sort([
        {Node, lists:reverse(Ids)}
     || {Node, Ids} <- maps:to_list(GroupedMap)
    ]).

-spec fold_guild_owner(integer(), fun((integer()) -> term()), #{node() => [integer()]}) ->
    #{node() => [integer()]}.
fold_guild_owner(GuildId, OwnerResolver, Acc) ->
    case resolve_owner_node(GuildId, node(), OwnerResolver) of
        {ok, OwnerNode} ->
            Existing = maps:get(OwnerNode, Acc, []),
            Acc#{OwnerNode => [GuildId | Existing]};
        unavailable ->
            Acc
    end.

-spec owner_groups_for_reload_all([integer()]) ->
    [{node(), [integer()]}].
owner_groups_for_reload_all(GuildIds) ->
    Provider = fun() ->
        gateway_node_router:active_nodes(guilds)
    end,
    owner_groups_for_reload_all(GuildIds, Provider).

-spec owner_groups_for_reload_all(
    [integer()], fun(() -> [node()])
) -> [{node(), [integer()]}].
owner_groups_for_reload_all([], ActiveNodesProvider) ->
    [{N, []} || N <- lists:usort(ActiveNodesProvider())];
owner_groups_for_reload_all(GuildIds, _ActiveNodesProvider) ->
    group_guild_ids_by_owner(GuildIds).

-spec call_owner_guild_manager(
    integer(), term(), pos_integer()
) -> term().
call_owner_guild_manager(GuildId, Request, Timeout) ->
    case guild_owner_scope(GuildId) of
        local ->
            call_guild_manager_node(node(), Request, Timeout);
        {remote, OwnerNode} ->
            call_guild_manager_node(OwnerNode, Request, Timeout);
        unavailable ->
            {error, unavailable}
    end.

-spec call_guild_manager_node(
    node(), term(), pos_integer()
) -> term().
call_guild_manager_node(TargetNode, Request, Timeout) ->
    Ref = guild_manager_ref(TargetNode),
    try gen_server:call(Ref, Request, Timeout) of
        Reply -> Reply
    catch
        exit:{timeout, _} -> {error, timeout};
        exit:{nodedown, _} -> {error, unavailable};
        exit:{noproc, _} -> {error, unavailable};
        exit:_ -> {error, unavailable}
    end.

-spec guild_manager_ref(node()) -> guild_manager | {guild_manager, node()}.
guild_manager_ref(TargetNode) ->
    case TargetNode =:= node() of
        true -> guild_manager;
        false -> {guild_manager, TargetNode}
    end.

-spec safe_guild_counts_get(integer()) ->
    {ok, non_neg_integer(), non_neg_integer()} | miss.
safe_guild_counts_get(GuildId) ->
    try guild_counts_cache:get(GuildId) of
        Result -> Result
    catch
        throw:_Reason -> miss;
        error:_Reason -> miss;
        exit:_Reason -> miss
    end.

-spec validate_batch_size(non_neg_integer()) -> ok.
validate_batch_size(Size) when Size > ?MAX_BATCH_SIZE ->
    gateway_rpc_error:raise(<<"batch_too_large">>);
validate_batch_size(_) ->
    ok.

-spec process_batch(
    [term()], fun((term()) -> term()), pos_integer()
) -> [term()].
process_batch(Items, HandlerFun, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    Workers = [
        spawn_monitor(fun() ->
            batch_worker(Parent, Ref, HandlerFun, Item)
        end)
     || Item <- Items
    ],
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    collect_results(Ref, length(Workers), Deadline, []).

-spec batch_worker(pid(), reference(), fun((term()) -> term()), term()) -> term().
batch_worker(Parent, Ref, HandlerFun, Item) ->
    try
        Parent ! {Ref, {ok, HandlerFun(Item)}}
    catch
        throw:_Reason -> Parent ! {Ref, error};
        error:_Reason -> Parent ! {Ref, error};
        exit:_Reason -> Parent ! {Ref, error}
    end.

-spec collect_results(
    reference(), non_neg_integer(), integer(), [T]
) -> [T] when T :: term().
collect_results(_, 0, _, Acc) ->
    lists:reverse(Acc);
collect_results(Ref, Remaining, Deadline, Acc) ->
    TimeoutMs = erlang:max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Ref, {ok, Result}} ->
            collect_results(Ref, Remaining - 1, Deadline, [Result | Acc]);
        {Ref, error} ->
            collect_results(Ref, Remaining - 1, Deadline, Acc);
        {'DOWN', _, process, _, _} ->
            collect_results(Ref, Remaining, Deadline, Acc)
    after TimeoutMs -> lists:reverse(Acc)
    end.

-ifdef(TEST).

guild_owner_scope_local_test() ->
    LocalNode = 'gateway_a@127.0.0.1',
    ?assertEqual(
        local,
        guild_owner_scope(123, LocalNode, fun(_) -> LocalNode end)
    ).

guild_owner_scope_remote_test() ->
    LocalNode = 'gateway_a@127.0.0.1',
    RemoteNode = 'gateway_b@127.0.0.1',
    ?assertEqual(
        {remote, RemoteNode},
        guild_owner_scope(123, LocalNode, fun(_) -> RemoteNode end)
    ).

resolve_owner_node_rejects_invalid_owner_atom_test() ->
    LocalNode = 'gateway_a@127.0.0.1',
    ?assertEqual(
        unavailable,
        resolve_owner_node(123, LocalNode, fun(_) -> bad_owner end)
    ),
    ?assertEqual(
        unavailable,
        resolve_owner_node(123, LocalNode, fun(_) -> {bad_owner} end)
    ).

group_guild_ids_by_owner_groups_ids_test() ->
    NodeA = 'gateway_a@127.0.0.1',
    NodeB = 'gateway_b@127.0.0.1',
    Groups = group_guild_ids_by_owner([10, 20, 30, 40, 20], fun
        (10) -> NodeA;
        (20) -> NodeB;
        (30) -> NodeA;
        (40) -> NodeB
    end),
    ?assertEqual(
        [{NodeA, [10, 30]}, {NodeB, [20, 40, 20]}],
        Groups
    ).

owner_groups_for_reload_all_empty_ids_uses_active_nodes_test() ->
    NodeA = 'gateway_a@127.0.0.1',
    NodeB = 'gateway_b@127.0.0.1',
    ?assertEqual(
        [{NodeA, []}, {NodeB, []}],
        owner_groups_for_reload_all([], fun() -> [NodeB, NodeA, NodeB] end)
    ).

validate_batch_size_test() ->
    ?assertEqual(ok, validate_batch_size(50)),
    ?assertEqual(ok, validate_batch_size(100)),
    ?assertError({gateway_rpc_error, <<"batch_too_large">>}, validate_batch_size(101)).

process_batch_collects_successful_results_test() ->
    Results = process_batch([1, 2, 3], fun(N) -> eqwalizer:dynamic_cast(N) * 2 end, 1000),
    ?assertEqual([2, 4, 6], lists:sort(Results)).

process_batch_drops_failed_workers_test() ->
    Results = process_batch(
        [1, 2, 3],
        fun
            (2) -> erlang:error(boom);
            (N) -> N
        end,
        1000
    ),
    ?assertEqual([1, 3], lists:sort(Results)).

process_batch_bounded_by_deadline_under_crashes_test() ->
    Timeout = 200,
    Start = erlang:monotonic_time(millisecond),
    _ = process_batch(
        lists:seq(1, 5),
        fun(_) -> erlang:error(boom) end,
        Timeout
    ),
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    ?assert(Elapsed < Timeout * 3).

-endif.
