%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_worker_pool).
-typing([eqwalizer]).

-export([init_counter/0, maybe_spawn/1, active_count/0, utilization_pct/0]).

-define(MAX_PUSH_WORKERS, 256).
-define(PUSH_WORKER_COUNTER, push_worker_counter).
-define(UTILIZATION_WARN_THRESHOLD, 80).

-spec init_counter() -> ok.
init_counter() ->
    case ets:info(?PUSH_WORKER_COUNTER) of
        undefined ->
            _ = ets:new(?PUSH_WORKER_COUNTER, [
                named_table, public, set, {write_concurrency, true}
            ]),
            ets:insert(?PUSH_WORKER_COUNTER, {active, 0}),
            ok;
        _ ->
            ensure_active_counter()
    end.

-spec ensure_active_counter() -> ok.
ensure_active_counter() ->
    try
        _ = ets:insert_new(?PUSH_WORKER_COUNTER, {active, 0}),
        ok
    catch
        error:badarg -> ok
    end.

-spec maybe_spawn(fun(() -> term())) -> ok | dropped.
maybe_spawn(Fun) ->
    maybe_spawn(Fun, false).

-spec maybe_spawn(fun(() -> term()), boolean()) -> ok | dropped.
maybe_spawn(Fun, RetryAfterInit) ->
    try ets:update_counter(?PUSH_WORKER_COUNTER, active, {2, 1}) of
        N when N > ?MAX_PUSH_WORKERS ->
            _ = ets:update_counter(?PUSH_WORKER_COUNTER, active, {2, -1}),
            dropped;
        N ->
            maybe_warn_high_utilization(N),
            spawn_tracked_worker(Fun),
            ok
    catch
        error:badarg ->
            maybe_spawn_after_missing_counter(Fun, RetryAfterInit)
    end.

-spec active_count() -> non_neg_integer().
active_count() ->
    try ets:lookup(?PUSH_WORKER_COUNTER, active) of
        [{active, N}] when is_integer(N), N >= 0 -> N;
        _ -> 0
    catch
        error:badarg -> 0
    end.

-spec utilization_pct() -> non_neg_integer().
utilization_pct() ->
    (active_count() * 100) div ?MAX_PUSH_WORKERS.

-spec maybe_warn_high_utilization(non_neg_integer()) -> ok.
maybe_warn_high_utilization(Active) ->
    Pct = (Active * 100) div ?MAX_PUSH_WORKERS,
    case Pct >= ?UTILIZATION_WARN_THRESHOLD of
        true ->
            throttled_utilization_warning(Active, Pct);
        false ->
            ok
    end.

-spec throttled_utilization_warning(non_neg_integer(), non_neg_integer()) -> ok.
throttled_utilization_warning(Active, Pct) ->
    Now = erlang:monotonic_time(second),
    Last =
        case persistent_term:get({?MODULE, last_util_warn}, undefined) of
            undefined -> 0;
            V -> V
        end,
    case Now - Last >= 10 of
        true ->
            persistent_term:put({?MODULE, last_util_warn}, Now),
            logger:warning("Push worker pool high utilisation", #{
                utilisation_pct => Pct, active => Active, max => ?MAX_PUSH_WORKERS
            }),
            ok;
        false ->
            ok
    end.

-spec maybe_spawn_after_missing_counter(fun(() -> term()), boolean()) -> ok | dropped.
maybe_spawn_after_missing_counter(Fun, false) ->
    ok = init_counter(),
    maybe_spawn(Fun, true);
maybe_spawn_after_missing_counter(_Fun, true) ->
    dropped.

-spec spawn_tracked_worker(fun(() -> term())) -> pid().
spawn_tracked_worker(Fun) ->
    spawn(fun() -> run_tracked_worker(Fun) end).

-spec run_tracked_worker(fun(() -> term())) -> term() | ok.
run_tracked_worker(Fun) ->
    try Fun() of
        Result -> Result
    catch
        throw:Reason:Stack ->
            log_worker_crash(throw, Reason, Stack);
        error:Reason:Stack ->
            log_worker_crash(error, Reason, Stack);
        exit:Reason:Stack ->
            log_worker_crash(exit, Reason, Stack)
    after
        ets:update_counter(?PUSH_WORKER_COUNTER, active, {2, -1})
    end.

-spec log_worker_crash(throw | error | exit, term(), list()) -> ok.
log_worker_crash(Class, Reason, Stack) ->
    logger:error("Push worker crash", #{class => Class, reason => Reason, stacktrace => Stack}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

maybe_spawn_initializes_missing_counter_and_tracks_worker_test() ->
    run_maybe_spawn_counter_test(fun() -> ok end).

maybe_spawn_repairs_missing_active_counter_test() ->
    run_maybe_spawn_counter_test(fun() ->
        _ = ets:new(?PUSH_WORKER_COUNTER, [named_table, public, set]),
        ok
    end).

run_maybe_spawn_counter_test(Setup) ->
    cleanup_test_counter(),
    Self = self(),
    ok = Setup(),
    try
        ?assertEqual(ok, maybe_spawn(fun() -> Self ! worker_started end)),
        ?assertEqual(ok, receive_worker_started()),
        ok = gateway_retry_timer:wait(50),
        ?assertEqual(0, active_count())
    after
        cleanup_test_counter()
    end.

maybe_spawn_drops_when_full_test() ->
    cleanup_test_counter(),
    init_counter(),
    ets:insert(?PUSH_WORKER_COUNTER, {active, ?MAX_PUSH_WORKERS}),
    ?assertEqual(dropped, maybe_spawn(fun() -> ok end)),
    ?assertEqual(?MAX_PUSH_WORKERS, active_count()),
    cleanup_test_counter().

worker_crash_decrements_counter_test() ->
    cleanup_test_counter(),
    init_counter(),
    ok = maybe_spawn(fun() -> error(deliberate_crash) end),
    ok = gateway_retry_timer:wait(100),
    ?assertEqual(0, active_count()),
    cleanup_test_counter().

cleanup_test_counter() ->
    try ets:delete(?PUSH_WORKER_COUNTER) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

receive_worker_started() ->
    receive
        worker_started -> ok
    after 100 ->
        timeout
    end.

-endif.
