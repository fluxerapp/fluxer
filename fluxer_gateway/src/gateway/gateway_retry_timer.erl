%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_retry_timer).
-typing([eqwalizer]).

-export([wait/1, wait_until/2]).

-define(MAX_RETRY_WAIT_MS, 60000).
-define(TIMER_GUARD_SLACK_MS, 5000).
-define(RETRY_WAIT, retry_wait).

-spec wait(term()) -> ok | {error, invalid_delay}.
wait(DelayMs) when is_integer(DelayMs), DelayMs >= 0 ->
    wait_non_neg(DelayMs);
wait(_DelayMs) ->
    {error, invalid_delay}.

-spec wait_non_neg(non_neg_integer()) -> ok.
wait_non_neg(DelayMs) when DelayMs =< ?MAX_RETRY_WAIT_MS ->
    wait_for_timer(erlang:start_timer(DelayMs, self(), ?RETRY_WAIT));
wait_non_neg(_DelayMs) ->
    wait_for_timer(erlang:start_timer(?MAX_RETRY_WAIT_MS, self(), ?RETRY_WAIT)).

-spec wait_until(term(), integer()) -> ok | expired | {error, invalid_delay}.
wait_until(DelayMs, DeadlineMs) when is_integer(DelayMs), DelayMs >= 0 ->
    NowMs = gateway_clock:monotonic_ms(),
    case DeadlineMs =< NowMs of
        true ->
            expired;
        false ->
            wait_non_neg(min(DelayMs, DeadlineMs - NowMs))
    end;
wait_until(_DelayMs, _DeadlineMs) ->
    {error, invalid_delay}.

-spec wait_for_timer(reference()) -> ok.
wait_for_timer(Ref) ->
    receive
        {timeout, Ref, ?RETRY_WAIT} -> ok
    after ?MAX_RETRY_WAIT_MS + ?TIMER_GUARD_SLACK_MS ->
        _ = erlang:cancel_timer(Ref),
        receive
            {timeout, Ref, ?RETRY_WAIT} -> ok
        after 0 ->
            ok
        end
    end.
