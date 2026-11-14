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
