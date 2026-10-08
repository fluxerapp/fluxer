%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_sync).
-typing([eqwalizer]).

-export([
    slice_items/3,
    update_subscriptions/4,
    remove_session_from_subscriptions/2,
    is_subset_of_ranges/2,
    compute_range_delta/2
]).

-type range() :: {non_neg_integer(), non_neg_integer()}.
-type session_id() :: binary().
-type list_id() :: binary().
-type list_item() :: map().

-export_type([range/0, session_id/0, list_id/0, list_item/0]).

-spec slice_items([list_item()], non_neg_integer(), non_neg_integer()) -> [list_item()].
slice_items(Items, Start, End) ->
    SafeEnd = min(End, length(Items) - 1),
    case Start > SafeEnd of
        true -> [];
        false -> lists:sublist(Items, Start + 1, SafeEnd - Start + 1)
    end.

-spec update_subscriptions(session_id(), list_id(), [range()], map()) ->
    {map(), [range()], boolean()}.
update_subscriptions(SessionId, ListId, NormalizedRanges, Subscriptions) ->
    case valid_list_id(ListId) of
        true ->
            ListSubs0 = maps:get(ListId, Subscriptions, #{}),
            OldRanges = maps:get(SessionId, ListSubs0, []),
            NewSubscriptions = apply_subscription_change(
                SessionId, ListId, NormalizedRanges, ListSubs0, Subscriptions
            ),
            ShouldSync = NormalizedRanges =/= [] andalso NormalizedRanges =/= OldRanges,
            {NewSubscriptions, OldRanges, ShouldSync};
        false ->
            {Subscriptions, [], false}
    end.

-spec apply_subscription_change(session_id(), list_id(), [range()], map(), map()) -> map().
apply_subscription_change(SessionId, ListId, [], ListSubs0, Subscriptions) ->
    Trimmed = maps:remove(SessionId, ListSubs0),
    case map_size(Trimmed) of
        0 -> maps:remove(ListId, Subscriptions);
        _ -> Subscriptions#{ListId => Trimmed}
    end;
apply_subscription_change(SessionId, ListId, NormalizedRanges, ListSubs0, Subscriptions) ->
    Updated = ListSubs0#{SessionId => NormalizedRanges},
    Subscriptions#{ListId => Updated}.

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

-spec remove_session_from_subscriptions(session_id(), map()) -> map().
remove_session_from_subscriptions(SessionId, Subscriptions) ->
    maps:fold(
        fun(ListId, ListSubs, Acc) ->
            remove_session_from_list(SessionId, ListId, ListSubs, Acc)
        end,
        #{},
        Subscriptions
    ).

-spec remove_session_from_list(session_id(), list_id(), map(), map()) -> map().
remove_session_from_list(SessionId, ListId, ListSubs, Acc) ->
    Trimmed = maps:remove(SessionId, ListSubs),
    case map_size(Trimmed) of
        0 -> Acc;
        _ -> Acc#{ListId => Trimmed}
    end.

-spec valid_list_id(list_id()) -> boolean().
valid_list_id(<<"0">>) ->
    true;
valid_list_id(ListId) when is_binary(ListId) ->
    case snowflake_id:parse_maybe(ListId) of
        Id when is_integer(Id), Id > 0 -> true;
        _ -> false
    end;
valid_list_id(_) ->
    false.
