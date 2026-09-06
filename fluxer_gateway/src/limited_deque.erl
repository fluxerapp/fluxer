%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(limited_deque).
-typing([eqwalizer]).
-compile({no_auto_import, [size/1]}).

-export([
    new/2,
    push/2,
    push/3,
    push_trimmed/3,
    push_front/2,
    pop/1,
    pop_front/1,
    to_list/1,
    from_list/3,
    size/1,
    bytes/1,
    is_empty/1,
    filter/2,
    drop_while_front/2,
    recompute_bytes/2,
    entry_bytes/1
]).

-export_type([deque/0]).

-define(WORD_SIZE_KEY, {?MODULE, word_size}).

-opaque deque() :: #{
    front := [term()],
    rear := [term()],
    count := non_neg_integer(),
    max_count := pos_integer(),
    bytes := non_neg_integer(),
    max_bytes := non_neg_integer()
}.

-spec new(pos_integer(), non_neg_integer()) -> deque().
new(MaxCount, MaxBytes) ->
    #{
        front => [],
        rear => [],
        count => 0,
        max_count => MaxCount,
        bytes => 0,
        max_bytes => MaxBytes
    }.

-spec push(term(), deque()) -> deque().
push(Item, D) ->
    push(Item, entry_bytes(Item), D).

-spec push(term(), non_neg_integer(), deque()) -> deque().
push(Item, ItemBytes, D) ->
    {D1, _Dropped} = push_trimmed(Item, ItemBytes, D),
    D1.

-spec push_trimmed(term(), non_neg_integer(), deque()) -> {deque(), [term()]}.
push_trimmed(Item, ItemBytes, #{rear := Rear, count := Count, bytes := Bytes} = D) ->
    D1 = D#{rear := [Item | Rear], count := Count + 1, bytes := Bytes + ItemBytes},
    {D2, Dropped} = trim_front_collect(D1, []),
    {D2, lists:reverse(Dropped)}.

-spec push_front(term(), deque()) -> deque().
push_front(Item, #{front := Front, count := Count, bytes := Bytes} = D) ->
    ItemBytes = entry_bytes(Item),
    D1 = D#{front := [Item | Front], count := Count + 1, bytes := Bytes + ItemBytes},
    trim_rear(D1).

-spec pop(deque()) -> {term(), deque()} | empty.
pop(#{rear := [H | T], count := Count, bytes := Bytes} = D) ->
    {H, D#{rear := T, count := Count - 1, bytes := max(0, Bytes - entry_bytes(H))}};
pop(#{rear := [], front := []}) ->
    empty;
pop(#{rear := [], front := Front, count := Count, bytes := Bytes} = D) ->
    [H | T] = lists:reverse(Front),
    {H, D#{
        front := [],
        rear := T,
        count := Count - 1,
        bytes := max(0, Bytes - entry_bytes(H))
    }}.

-spec pop_front(deque()) -> {term(), deque()} | empty.
pop_front(#{front := [H | T], count := Count, bytes := Bytes} = D) ->
    {H, D#{front := T, count := Count - 1, bytes := max(0, Bytes - entry_bytes(H))}};
pop_front(#{front := [], rear := []}) ->
    empty;
pop_front(#{front := [], rear := Rear, count := Count, bytes := Bytes} = D) ->
    [H | T] = lists:reverse(Rear),
    {H, D#{
        rear := [],
        front := T,
        count := Count - 1,
        bytes := max(0, Bytes - entry_bytes(H))
    }}.

-spec to_list(deque()) -> [term()].
to_list(#{front := Front, rear := Rear}) ->
    Front ++ lists:reverse(Rear).

-spec from_list([term()], pos_integer(), non_neg_integer()) -> deque().
from_list(List, MaxCount, MaxBytes) ->
    TotalBytes = lists:foldl(fun(I, Acc) -> Acc + entry_bytes(I) end, 0, List),
    D = #{
        front => List,
        rear => [],
        count => length(List),
        max_count => MaxCount,
        bytes => TotalBytes,
        max_bytes => MaxBytes
    },
    trim_front(D).

-spec size(deque()) -> non_neg_integer().
size(#{count := Count}) -> Count.

-spec bytes(deque()) -> non_neg_integer().
bytes(#{bytes := Bytes}) -> Bytes.

-spec is_empty(deque()) -> boolean().
is_empty(#{count := 0}) -> true;
is_empty(_) -> false.

-spec filter(fun((term()) -> boolean()), deque()) -> deque().
filter(Pred, #{max_count := MC, max_bytes := MB} = D) ->
    List = to_list(D),
    from_list(lists:filter(Pred, List), MC, MB).

-spec drop_while_front(fun((term()) -> boolean()), deque()) -> deque().
drop_while_front(Pred, D) ->
    case pop_front(D) of
        empty ->
            D;
        {Item, D2} ->
            continue_drop_while_front(Pred(Item), Pred, Item, D2)
    end.

-spec continue_drop_while_front(boolean(), fun((term()) -> boolean()), term(), deque()) ->
    deque().
continue_drop_while_front(true, Pred, _Item, D) ->
    drop_while_front(Pred, D);
continue_drop_while_front(false, _Pred, Item, D) ->
    push_front(Item, D).

-spec recompute_bytes(fun((term()) -> non_neg_integer()), deque()) -> deque().
recompute_bytes(ByteFun, #{front := Front, rear := Rear} = D) ->
    FrontBytes = lists:foldl(fun(I, Acc) -> Acc + ByteFun(I) end, 0, Front),
    RearBytes = lists:foldl(fun(I, Acc) -> Acc + ByteFun(I) end, 0, Rear),
    D#{bytes := FrontBytes + RearBytes}.

-spec trim_front(deque()) -> deque().
trim_front(
    #{count := Count, max_count := MaxCount, max_bytes := MaxBytes} = D
) when
    Count =< MaxCount, MaxBytes =:= 0
->
    D;
trim_front(
    #{count := Count, max_count := MaxCount, bytes := Bytes, max_bytes := MaxBytes} = D
) when
    Count =< MaxCount, Bytes =< MaxBytes
->
    D;
trim_front(D) ->
    case pop_front(D) of
        empty -> D;
        {_, D2} -> trim_front(D2)
    end.

-spec trim_front_collect(deque(), [term()]) -> {deque(), [term()]}.
trim_front_collect(
    #{count := Count, max_count := MaxCount, max_bytes := MaxBytes} = D, Acc
) when
    Count =< MaxCount, MaxBytes =:= 0
->
    {D, Acc};
trim_front_collect(
    #{count := Count, max_count := MaxCount, bytes := Bytes, max_bytes := MaxBytes} = D, Acc
) when
    Count =< MaxCount, Bytes =< MaxBytes
->
    {D, Acc};
trim_front_collect(D, Acc) ->
    case pop_front(D) of
        empty -> {D, Acc};
        {Item, D2} -> trim_front_collect(D2, [Item | Acc])
    end.

-spec trim_rear(deque()) -> deque().
trim_rear(
    #{count := Count, max_count := MaxCount, max_bytes := MaxBytes} = D
) when
    Count =< MaxCount, MaxBytes =:= 0
->
    D;
trim_rear(
    #{count := Count, max_count := MaxCount, bytes := Bytes, max_bytes := MaxBytes} = D
) when
    Count =< MaxCount, Bytes =< MaxBytes
->
    D;
trim_rear(D) ->
    case pop(D) of
        empty -> D;
        {_, D2} -> trim_rear(D2)
    end.

-spec entry_bytes(term()) -> non_neg_integer().
entry_bytes(Term) ->
    erts_debug:flat_size(Term) * word_size().

-spec word_size() -> 4 | 8.
word_size() ->
    case persistent_term:get(?WORD_SIZE_KEY, undefined) of
        4 -> 4;
        8 -> 8;
        _Other -> cache_word_size()
    end.

-spec cache_word_size() -> 4 | 8.
cache_word_size() ->
    WordSize = erlang:system_info(wordsize),
    persistent_term:put(?WORD_SIZE_KEY, WordSize),
    WordSize.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_empty_test() ->
    D = new(10, 0),
    ?assertEqual(0, size(D)),
    ?assertEqual(true, is_empty(D)),
    ?assertEqual([], to_list(D)).

push_and_to_list_test() ->
    D0 = new(10, 0),
    D1 = push(a, push(b, push(c, D0))),
    ?assertEqual([c, b, a], to_list(D1)),
    ?assertEqual(3, size(D1)).

push_trims_at_bound_test() ->
    D0 = new(3, 0),
    D1 = push(d, push(c, push(b, push(a, D0)))),
    ?assertEqual(3, size(D1)),
    List = to_list(D1),
    ?assertEqual([b, c, d], List).

push_trimmed_returns_dropped_items_test() ->
    D0 = new(3, 0),
    D1 = lists:foldl(fun push/2, D0, [a, b, c]),
    {D2, Dropped} = push_trimmed(d, entry_bytes(d), D1),
    ?assertEqual([a], Dropped),
    ?assertEqual([b, c, d], to_list(D2)).

push_trimmed_returns_no_dropped_items_below_bound_test() ->
    {D1, Dropped} = push_trimmed(a, entry_bytes(a), new(3, 0)),
    ?assertEqual([], Dropped),
    ?assertEqual([a], to_list(D1)).

push_trimmed_returns_dropped_items_oldest_first_test() ->
    Big = lists:seq(1, 100),
    Smalls = [[1], [2], [3]],
    D0 = new(1000, entry_bytes(Big)),
    D1 = lists:foldl(fun push/2, D0, Smalls),
    {D2, Dropped} = push_trimmed(Big, entry_bytes(Big), D1),
    ?assertEqual(Smalls, Dropped),
    ?assertEqual([Big], to_list(D2)).

pop_front_test() ->
    assert_pop_sequence(fun pop_front/1, [1, 2, 3]).

pop_rear_test() ->
    assert_pop_sequence(fun pop/1, [3, 2, 1]).

assert_pop_sequence(PopFun, Items) ->
    D1 = lists:foldl(
        fun(Expected, D) ->
            {Expected, NextD} = PopFun(D),
            NextD
        end,
        from_list([1, 2, 3], 10, 0),
        Items
    ),
    ?assertEqual(empty, PopFun(D1)).

filter_test() ->
    D0 = default_test_deque(),
    D1 = filter(fun(X) -> X > 3 end, D0),
    ?assertEqual([4, 5], to_list(D1)),
    ?assertEqual(2, size(D1)).

from_list_trims_test() ->
    D = bounded_test_deque(3),
    ?assertEqual(3, size(D)),
    ?assertEqual([3, 4, 5], to_list(D)).

default_test_deque() ->
    bounded_test_deque(10).

bounded_test_deque(MaxCount) ->
    from_list([1, 2, 3, 4, 5], MaxCount, 0).

size_is_o1_test() ->
    D0 = new(1000, 0),
    D1 = lists:foldl(fun push/2, D0, lists:seq(1, 1000)),
    ?assertEqual(1000, size(D1)).

push_with_precomputed_bytes_matches_push_test() ->
    Item = #{event => presence_update, data => #{<<"status">> => <<"online">>}, seq => 7},
    D0 = new(10, 1048576),
    ?assertEqual(push(Item, D0), push(Item, entry_bytes(Item), D0)),
    ?assertEqual(entry_bytes(Item), bytes(push(Item, D0))).

push_with_precomputed_bytes_skips_recomputation_test() ->
    Item = #{event => presence_update, data => #{<<"status">> => <<"online">>}, seq => 7},
    D = push(Item, 1234, new(10, 1048576)),
    ?assertEqual(1, size(D)),
    ?assertEqual(1234, bytes(D)).

push_with_precomputed_bytes_matches_push_at_byte_bound_test() ->
    Item = lists:seq(1, 256),
    ItemBytes = entry_bytes(Item),
    D0 = new(10, ItemBytes * 2),
    Items = [Item, Item, Item],
    D1 = lists:foldl(fun push/2, D0, Items),
    D2 = lists:foldl(fun(I, D) -> push(I, entry_bytes(I), D) end, D0, Items),
    ?assertEqual(2, size(D1)),
    ?assertEqual(ItemBytes * 2, bytes(D1)),
    ?assertEqual(D1, D2).

push_with_precomputed_bytes_matches_push_at_count_bound_test() ->
    D0 = new(3, 0),
    Items = [a, b, c, d],
    D1 = lists:foldl(fun push/2, D0, Items),
    D2 = lists:foldl(fun(I, D) -> push(I, entry_bytes(I), D) end, D0, Items),
    ?assertEqual([b, c, d], to_list(D1)),
    ?assertEqual(D1, D2).

entry_bytes_uses_word_size_test() ->
    ?assertEqual(
        erts_debug:flat_size({a, b, c}) * erlang:system_info(wordsize),
        entry_bytes({a, b, c})
    ).

-endif.
