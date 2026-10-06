%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(backoff_utils).
-typing([eqwalizer]).

-export([
    calculate/1,
    calculate/2,
    calculate_with_jitter/1,
    calculate_with_jitter/2
]).

-define(MAX_BACKOFF_EXPONENT, 32).
-define(BASE_BACKOFF_MS, 1000).
-define(DEFAULT_MAX_BACKOFF_MS, 30000).

-spec calculate(non_neg_integer()) -> non_neg_integer().
calculate(Attempt) ->
    calculate(Attempt, ?DEFAULT_MAX_BACKOFF_MS).

-spec calculate(non_neg_integer(), pos_integer()) -> non_neg_integer().
calculate(Attempt, MaxMs) ->
    Exponent = cap_exponent(Attempt),
    BackoffMs = ?BASE_BACKOFF_MS bsl Exponent,
    min(BackoffMs, MaxMs).

-spec cap_exponent(non_neg_integer()) -> non_neg_integer().
cap_exponent(Attempt) when is_integer(Attempt), Attempt >= 0 ->
    min(Attempt, ?MAX_BACKOFF_EXPONENT);
cap_exponent(_) ->
    0.

-spec calculate_with_jitter(non_neg_integer()) -> non_neg_integer().
calculate_with_jitter(Attempt) ->
    calculate_with_jitter(Attempt, ?DEFAULT_MAX_BACKOFF_MS).

-spec calculate_with_jitter(non_neg_integer(), pos_integer()) -> non_neg_integer().
calculate_with_jitter(Attempt, MaxMs) ->
    Base = calculate(Attempt, MaxMs),
    JitterRange = max(1, Base div 2),
    Jitter = rand:uniform(JitterRange + 1) - 1,
    Offset = JitterRange div 2,
    max(1, min(MaxMs, Base - Offset + Jitter)).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

calculate_default_max_test() ->
    ?assertEqual(1000, calculate(0)),
    ?assertEqual(2000, calculate(1)),
    ?assertEqual(4000, calculate(2)),
    ?assertEqual(8000, calculate(3)),
    ?assertEqual(16000, calculate(4)),
    ?assertEqual(30000, calculate(5)),
    ?assertEqual(30000, calculate(10)).

calculate_custom_max_test() ->
    ?assertEqual(1000, calculate(0, 5000)),
    ?assertEqual(2000, calculate(1, 5000)),
    ?assertEqual(4000, calculate(2, 5000)),
    ?assertEqual(5000, calculate(3, 5000)),
    ?assertEqual(5000, calculate(10, 5000)).

calculate_small_max_test() ->
    ?assertEqual(1000, calculate(0, 1000)),
    ?assertEqual(1000, calculate(1, 1000)),
    ?assertEqual(1000, calculate(5, 1000)).

calculate_huge_attempt_does_not_overflow_test() ->
    ?assertEqual(30000, calculate(1000000000)),
    ?assertEqual(5000, calculate(1000000000, 5000)).

calculate_negative_attempt_is_safe_test() ->
    ?assertEqual(1000, calculate(-5, 30000)).

calculate_with_jitter_stays_in_range_test() ->
    lists:foreach(
        fun(_) ->
            V0 = calculate_with_jitter(0),
            ?assert(V0 >= 750),
            ?assert(V0 =< 1250),
            V2 = calculate_with_jitter(2),
            ?assert(V2 >= 3000),
            ?assert(V2 =< 5000),
            V10 = calculate_with_jitter(10),
            ?assert(V10 >= 22500),
            ?assert(V10 =< 30000)
        end,
        lists:seq(1, 20)
    ).

calculate_with_jitter_respects_max_test() ->
    lists:foreach(
        fun(_) ->
            V = calculate_with_jitter(10, 5000),
            ?assert(V >= 1),
            ?assert(V =< 5000)
        end,
        lists:seq(1, 20)
    ).

calculate_with_jitter_positive_test() ->
    lists:foreach(
        fun(_) ->
            V = calculate_with_jitter(0, 1),
            ?assert(V >= 1)
        end,
        lists:seq(1, 20)
    ).

-endif.
