%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list).
-typing([eqwalizer]).

-export([
    calculate_list_id/2,
    get_counts/2,
    subscribe_ranges/4,
    unsubscribe_session/2,
    build_sync_response/4,
    member_list_snapshot/2,
    get_online_count/1,
    broadcast_member_list_updates/3,
    broadcast_member_list_updates/5,
    broadcast_all_member_list_updates/1,
    broadcast_member_list_updates_for_channel/2,
    flush_pending_member_list_syncs/1,
    normalize_ranges/1,
    get_members_cursor/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
    ChannelIndex = guild_data_index:channel_index(Data),
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

-spec get_counts(list_id(), guild_state()) -> {non_neg_integer(), non_neg_integer()}.
get_counts(ListId, State) ->
    guild_member_list_read:get_counts(ListId, State).

-spec subscribe_ranges(binary(), list_id(), [range()], guild_state()) ->
    {guild_state(), boolean(), [range()]}.
subscribe_ranges(SessionId, ListId, Ranges, State) ->
    guild_member_list_subscribe:subscribe_ranges(SessionId, ListId, Ranges, State).

-spec unsubscribe_session(binary(), guild_state()) -> guild_state().
unsubscribe_session(SessionId, State) ->
    guild_member_list_subscribe:unsubscribe_session(SessionId, State).

-spec build_sync_response(integer(), list_id(), [range()], guild_state()) -> map().
build_sync_response(GuildId, ListId, Ranges, State) ->
    guild_member_list_read:build_sync_response(GuildId, ListId, Ranges, State).

-spec member_list_snapshot(list_id(), guild_state()) ->
    {non_neg_integer(), non_neg_integer(), [map()], [map()]}.
member_list_snapshot(ListId, State) ->
    guild_member_list_read:member_list_snapshot(ListId, State).

-spec get_online_count(guild_state()) -> non_neg_integer().
get_online_count(State) ->
    guild_member_list_read:get_online_count(State).

-spec broadcast_member_list_updates(integer() | undefined, guild_state(), guild_state()) ->
    {ok, guild_state()}.
broadcast_member_list_updates(UserId, OldState, UpdatedState) ->
    guild_member_list_write:broadcast_member_list_updates(UserId, OldState, UpdatedState).

-spec broadcast_member_list_updates(
    integer() | undefined,
    guild_state(),
    guild_state(),
    map() | undefined,
    map() | undefined
) -> {ok, guild_state()}.
broadcast_member_list_updates(
    UserId,
    OldState,
    UpdatedState,
    OldPresence,
    NewPresence
) ->
    guild_member_list_write:broadcast_member_list_updates(
        UserId,
        OldState,
        UpdatedState,
        OldPresence,
        NewPresence
    ).

-spec broadcast_all_member_list_updates(guild_state()) -> {ok, guild_state()}.
broadcast_all_member_list_updates(State) ->
    guild_member_list_write:broadcast_all_member_list_updates(State).

-spec broadcast_member_list_updates_for_channel(channel_id(), guild_state()) ->
    {ok, guild_state()}.
broadcast_member_list_updates_for_channel(ChannelId, State) ->
    guild_member_list_write:broadcast_member_list_updates_for_channel(ChannelId, State).

-spec flush_pending_member_list_syncs(guild_state()) -> guild_state().
flush_pending_member_list_syncs(State) ->
    guild_member_list_write:flush_pending_member_list_syncs(State).

-spec get_members_cursor(map(), guild_state()) -> {reply, map(), guild_state()}.
get_members_cursor(Request, State) ->
    guild_member_list_read:get_members_cursor(Request, State).

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

-ifdef(TEST).

calculate_list_id_returns_channel_id_test() ->
    State = #{
        id => 100,
        data => #{<<"channels">> => [#{<<"id">> => <<"500">>}]}
    },
    ?assertEqual(<<"500">>, calculate_list_id(500, State)).

calculate_list_id_missing_channel_test() ->
    State = #{id => 100, data => #{}},
    ?assertEqual(undefined, calculate_list_id(123, State)).

normalize_ranges_empty_test() ->
    ?assertEqual([], normalize_ranges([])).

normalize_ranges_single_test() ->
    ?assertEqual([{0, 99}], normalize_ranges([{0, 99}])).

normalize_ranges_merges_overlapping_test() ->
    ?assertEqual([{0, 99}], normalize_ranges([{0, 60}, {50, 99}])).

normalize_ranges_merges_adjacent_test() ->
    ?assertEqual([{0, 99}], normalize_ranges([{0, 49}, {50, 99}])).

normalize_ranges_keeps_separate_test() ->
    ?assertEqual([{0, 50}, {100, 150}], normalize_ranges([{0, 50}, {100, 150}])).

normalize_ranges_filters_invalid_test() ->
    ?assertEqual([{0, 50}], normalize_ranges([{100, 50}, {0, 50}, {-1, 10}])).

normalize_ranges_sorts_and_merges_test() ->
    ?assertEqual([{0, 99}], normalize_ranges([{50, 99}, {0, 49}, {30, 70}])).

normalize_ranges_duplicate_ranges_test() ->
    ?assertEqual([{0, 99}], normalize_ranges([{0, 99}, {0, 99}])).

normalize_ranges_three_separate_ranges_test() ->
    ThreeRanges = [{0, 10}, {50, 60}, {100, 110}],
    ?assertEqual(ThreeRanges, normalize_ranges(ThreeRanges)).

normalize_ranges_all_invalid_test() ->
    ?assertEqual([], normalize_ranges([{10, 5}, {-1, -1}, {200000, 300000}])).

normalize_ranges_touching_ranges_merge_test() ->
    ?assertEqual([{0, 20}], normalize_ranges([{0, 10}, {11, 20}])).

normalize_ranges_gap_of_one_no_merge_test() ->
    ?assertEqual([{0, 10}, {12, 20}], normalize_ranges([{0, 10}, {12, 20}])).

-endif.
