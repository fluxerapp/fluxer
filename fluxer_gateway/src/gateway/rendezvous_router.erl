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
