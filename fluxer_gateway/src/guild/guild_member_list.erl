%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list).
-typing([eqwalizer]).

-export([calculate_list_id/2, normalize_ranges/1]).

-type guild_state() :: map().
-type list_id() :: binary().
-type range() :: {non_neg_integer(), non_neg_integer()}.
-type channel_id() :: integer().

-export_type([guild_state/0, list_id/0, range/0, channel_id/0]).

-define(MAX_RANGE_END, 100000).
-define(MAX_RANGE_SPAN, 99).

-spec calculate_list_id(channel_id(), guild_state()) -> list_id() | undefined.
calculate_list_id(ChannelId, State) when is_integer(ChannelId), ChannelId > 0 ->
    Data = maps:get(data, State, #{}),
    ChannelIndex = guild_data_index_channels:channel_index(Data),
    case maps:is_key(ChannelId, ChannelIndex) of
        true -> integer_to_binary(ChannelId);
        false -> undefined
    end;
calculate_list_id(_, _) ->
    undefined.

-spec normalize_ranges([range()]) -> [range()].
normalize_ranges(Ranges) ->
    ValidRanges = lists:filtermap(
        fun valid_range_item/1,
        Ranges
    ),
    MergedRanges = merge_overlapping_ranges(lists:sort(ValidRanges)),
    split_ranges_by_max_span(MergedRanges).

-spec valid_range_item(term()) -> {true, range()} | false.
valid_range_item(Range) ->
    case validate_range(Range) of
        invalid -> false;
        Valid -> {true, Valid}
    end.

-spec validate_range(term()) -> range() | invalid.
validate_range({Start, End}) when
    is_integer(Start),
    is_integer(End),
    Start >= 0,
    End >= 0,
    Start =< End,
    End =< ?MAX_RANGE_END,
    End - Start =< ?MAX_RANGE_SPAN
->
    {Start, End};
validate_range(_) ->
    invalid.

-spec merge_overlapping_ranges([range()]) -> [range()].
merge_overlapping_ranges([]) ->
    [];
merge_overlapping_ranges([Single]) ->
    [Single];
merge_overlapping_ranges([{S1, E1}, {S2, E2} | Rest]) when S2 =< E1 + 1 ->
    merge_overlapping_ranges([{S1, max(E1, E2)} | Rest]);
merge_overlapping_ranges([Range | Rest]) ->
    [Range | merge_overlapping_ranges(Rest)].

-spec split_ranges_by_max_span([range()]) -> [range()].
split_ranges_by_max_span(Ranges) ->
    lists:append([split_range_by_max_span(Range) || Range <- Ranges]).

-spec split_range_by_max_span(range()) -> [range()].
split_range_by_max_span({Start, End}) when End - Start =< ?MAX_RANGE_SPAN ->
    [{Start, End}];
split_range_by_max_span({Start, End}) ->
    ChunkEnd = Start + ?MAX_RANGE_SPAN,
    [{Start, ChunkEnd} | split_range_by_max_span({ChunkEnd + 1, End})].
