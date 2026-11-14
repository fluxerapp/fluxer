%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_sync).
-typing([eqwalizer]).

-export([
    is_subset_of_ranges/2,
    compute_range_delta/2
]).

-type range() :: {non_neg_integer(), non_neg_integer()}.
-type session_id() :: binary().
-type list_id() :: binary().
-type list_item() :: map().

-export_type([range/0, session_id/0, list_id/0, list_item/0]).

-spec is_subset_of_ranges([range()], [range()]) -> boolean().
is_subset_of_ranges([], _Outer) ->
    true;
is_subset_of_ranges(_Inner, []) ->
    false;
is_subset_of_ranges(Inner, Outer) ->
    lists:all(fun(Range) -> range_is_subset(Range, Outer) end, Inner).

-spec range_is_subset(range(), [range()]) -> boolean().
range_is_subset({InStart, InEnd}, Outer) ->
    lists:any(
        fun({OutStart, OutEnd}) ->
            OutStart =< InStart andalso OutEnd >= InEnd
        end,
        Outer
    ).

-spec compute_range_delta([range()], [range()]) -> [range()].
compute_range_delta(NewRanges, OldRanges) ->
    Subtracted = lists:foldl(
        fun subtract_range_from_list/2,
        NewRanges,
        OldRanges
    ),
    guild_member_list:normalize_ranges(Subtracted).

-spec subtract_range_from_list(range(), [range()]) -> [range()].
subtract_range_from_list(_SubRange, []) ->
    [];
subtract_range_from_list({SubStart, SubEnd}, Ranges) ->
    lists:flatmap(
        fun({RStart, REnd}) ->
            subtract_one_range(RStart, REnd, SubStart, SubEnd)
        end,
        Ranges
    ).

-spec subtract_one_range(
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer()
) -> [range()].
subtract_one_range(RStart, REnd, SubStart, SubEnd) when
    REnd < SubStart; RStart > SubEnd
->
    [{RStart, REnd}];
subtract_one_range(RStart, REnd, SubStart, SubEnd) ->
    Left =
        case RStart < SubStart of
            true -> [{RStart, SubStart - 1}];
            false -> []
        end,
    Right =
        case REnd > SubEnd of
            true -> [{SubEnd + 1, REnd}];
            false -> []
        end,
    Left ++ Right.
