%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_sync_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

is_subset_of_ranges_empty_inner_test() ->
    ?assertEqual(true, guild_member_list_sync:is_subset_of_ranges([], [{0, 99}])).

is_subset_of_ranges_empty_outer_test() ->
    ?assertEqual(false, guild_member_list_sync:is_subset_of_ranges([{0, 99}], [])).

is_subset_of_ranges_exact_match_test() ->
    ?assertEqual(true, guild_member_list_sync:is_subset_of_ranges([{0, 99}], [{0, 99}])).

is_subset_of_ranges_true_subset_test() ->
    ?assertEqual(true, guild_member_list_sync:is_subset_of_ranges([{10, 50}], [{0, 99}])).

is_subset_of_ranges_not_subset_test() ->
    ?assertEqual(false, guild_member_list_sync:is_subset_of_ranges([{0, 150}], [{0, 99}])).

is_subset_of_ranges_partial_overlap_not_subset_test() ->
    ?assertEqual(false, guild_member_list_sync:is_subset_of_ranges([{50, 150}], [{0, 99}])).

is_subset_of_ranges_multiple_inner_covered_test() ->
    ?assertEqual(
        true, guild_member_list_sync:is_subset_of_ranges([{10, 20}, {30, 40}], [{0, 99}])
    ).

is_subset_of_ranges_multiple_outer_test() ->
    ?assertEqual(
        true,
        guild_member_list_sync:is_subset_of_ranges([{10, 20}, {110, 120}], [{0, 50}, {100, 150}])
    ).

is_subset_of_ranges_multiple_outer_not_covered_test() ->
    ?assertEqual(
        false,
        guild_member_list_sync:is_subset_of_ranges([{10, 20}, {60, 80}], [{0, 50}, {100, 150}])
    ).

compute_range_delta_no_overlap_test() ->
    ?assertEqual(
        [{100, 199}], guild_member_list_sync:compute_range_delta([{100, 199}], [{0, 50}])
    ).

compute_range_delta_full_overlap_test() ->
    ?assertEqual([], guild_member_list_sync:compute_range_delta([{10, 50}], [{0, 99}])).

compute_range_delta_partial_overlap_right_test() ->
    ?assertEqual(
        [{100, 199}], guild_member_list_sync:compute_range_delta([{0, 199}], [{0, 99}])
    ).

compute_range_delta_partial_overlap_left_test() ->
    ?assertEqual(
        [{0, 49}], guild_member_list_sync:compute_range_delta([{0, 150}], [{50, 200}])
    ).

compute_range_delta_middle_gap_test() ->
    ?assertEqual(
        [{51, 99}],
        guild_member_list_sync:compute_range_delta([{0, 150}], [{0, 50}, {100, 200}])
    ).

compute_range_delta_empty_old_test() ->
    ?assertEqual([{0, 99}], guild_member_list_sync:compute_range_delta([{0, 99}], [])).

compute_range_delta_empty_new_test() ->
    ?assertEqual([], guild_member_list_sync:compute_range_delta([], [{0, 99}])).
