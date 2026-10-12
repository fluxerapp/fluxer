%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(rendezvous_router).
-typing([eqwalizer]).

-export([select/2, select_node/2, group_keys/2]).

-define(HASH_LIMIT, 16#FFFFFFFF).

-spec select_node(term(), [node()]) -> node() | undefined.
select_node(_Key, []) ->
    undefined;
select_node(_Key, [Node]) ->
    Node;
select_node(Key, Nodes) when is_list(Nodes) ->
    {Best, _Weight} =
        lists:foldl(
            fun(Node, Acc) -> pick_better_node(Key, Node, Acc) end,
            {undefined, -1},
            Nodes
        ),
    Best.

-spec pick_better_node(term(), node(), {node() | undefined, integer()}) ->
    {node() | undefined, integer()}.
pick_better_node(Key, Node, {BestNode, BestWeight}) ->
    Weight = node_weight(Key, Node),
    IsBetter =
        (Weight > BestWeight) orelse
            (Weight =:= BestWeight andalso is_lower_node(Node, BestNode)),
    case IsBetter of
        true -> {Node, Weight};
        false -> {BestNode, BestWeight}
    end.

-spec is_lower_node(node(), node() | undefined) -> boolean().
is_lower_node(_Node, undefined) ->
    true;
is_lower_node(Node, BestNode) ->
    Node < BestNode.

-spec node_weight(term(), node()) -> non_neg_integer().
node_weight(Key, Node) ->
    erlang:phash2({Key, Node}, ?HASH_LIMIT).

-spec select(term(), pos_integer()) -> non_neg_integer().
select(Key, ShardCount) when ShardCount > 0 ->
    Indices = lists:seq(0, ShardCount - 1),
    {Index, _Weight} =
        lists:foldl(
            fun(CurrentIndex, Best) ->
                pick_better(Key, CurrentIndex, Best)
            end,
            {0, -1},
            Indices
        ),
    Index;
select(_Key, _ShardCount) ->
    0.

-spec group_keys([term()], pos_integer()) -> [{non_neg_integer(), [term()]}].
group_keys(Keys, ShardCount) when is_list(Keys), ShardCount > 0 ->
    Grouped =
        lists:foldl(
            fun(Key, Acc) ->
                Index = select(Key, ShardCount),
                Existing = maps:get(Index, Acc, []),
                Acc#{Index => [Key | Existing]}
            end,
            #{},
            Keys
        ),
    Sorted = lists:sort(
        fun({IdxA, _}, {IdxB, _}) -> IdxA =< IdxB end,
        [{Index, lists:usort(Group)} || {Index, Group} <- maps:to_list(Grouped)]
    ),
    Sorted;
group_keys(_Keys, _ShardCount) ->
    [].

-spec pick_better(term(), non_neg_integer(), {non_neg_integer(), integer()}) ->
    {non_neg_integer(), integer()}.
pick_better(Key, CurrentIndex, {BestIndex, BestWeight}) ->
    Weight = weight(Key, CurrentIndex),
    IsBetter =
        (Weight > BestWeight) orelse
            (Weight =:= BestWeight andalso CurrentIndex < BestIndex),
    case IsBetter of
        true -> {CurrentIndex, Weight};
        false -> {BestIndex, BestWeight}
    end.

-spec weight(term(), non_neg_integer()) -> non_neg_integer().
weight(Key, Index) ->
    erlang:phash2({Key, Index}, ?HASH_LIMIT).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

select_distribution_test() ->
    Keys = lists:seq(1, 1000),
    ShardCount = 10,
    Distribution = count_distribution(Keys, ShardCount),
    Counts = maps:values(Distribution),
    ?assertEqual(ShardCount, maps:size(Distribution)),
    lists:foreach(fun(Count) -> ?assert(Count > 0) end, Counts).

count_distribution(Keys, ShardCount) ->
    lists:foldl(
        fun(Key, Acc) ->
            count_distribution_fold(Key, ShardCount, Acc)
        end,
        #{},
        Keys
    ).

count_distribution_fold(Key, ShardCount, Acc) ->
    Index = select(Key, ShardCount),
    maps:update_with(Index, fun(V) -> V + 1 end, 1, Acc).

group_keys_deduplicates_test() ->
    Keys = [1, 2, 3, 1, 2],
    Groups = group_keys(Keys, 2),
    lists:foreach(
        fun({_Index, GroupKeys}) ->
            ?assertEqual(GroupKeys, lists:usort(GroupKeys))
        end,
        Groups
    ).

group_keys_sorted_indices_test() ->
    Keys = lists:seq(1, 100),
    Groups = group_keys(Keys, 5),
    Indices = [I || {I, _} <- Groups],
    ?assertEqual(Indices, lists:sort(Indices)).

group_keys_all_keys_present_test() ->
    Keys = [a, b, c, d, e],
    Groups = group_keys(Keys, 3),
    AllGroupedKeys = lists:flatten([K || {_, K} <- Groups]),
    ?assertEqual(lists:sort(Keys), lists:sort(AllGroupedKeys)).

select_node_independent_of_order_test() ->
    Nodes1 = ['n@a', 'n@b', 'n@c'],
    Nodes2 = ['n@c', 'n@a', 'n@b'],
    ?assertEqual(select_node(<<"k">>, Nodes1), select_node(<<"k">>, Nodes2)).

select_node_member_of_candidates_test() ->
    Nodes = ['n@a', 'n@b', 'n@c'],
    lists:foreach(
        fun(I) ->
            Owner = select_node(<<"key-", (integer_to_binary(I))/binary>>, Nodes),
            ?assert(lists:member(Owner, Nodes))
        end,
        lists:seq(1, 100)
    ).

select_node_minimal_disruption_on_leave_test() ->
    Nodes = ['n@a', 'n@b', 'n@c', 'n@d'],
    Remaining = ['n@a', 'n@b', 'n@c'],
    Keys = [<<"key-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 400)],
    Moved = length([
        K
     || K <- Keys,
        select_node(K, Nodes) =/= select_node(K, Remaining),
        lists:member(select_node(K, Nodes), Remaining)
    ]),
    ?assertEqual(0, Moved).

-endif.
