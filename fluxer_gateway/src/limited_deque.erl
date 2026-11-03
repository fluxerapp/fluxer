%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(limited_deque).
-typing([eqwalizer]).

-export([
    push_trimmed/3,
    push_front/2,
    pop/1,
    pop_front/1,
    to_list/1,
    from_list/3,
    bytes/1,
    drop_while_front/2,
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

-spec bytes(deque()) -> non_neg_integer().
bytes(#{bytes := Bytes}) -> Bytes.

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
