%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_worker_pool).
-typing([eqwalizer]).

-export([init_counter/0, maybe_spawn/1, active_count/0]).
-export([bump_counter/2, read_counter/1, counter_table_status/0]).

-define(MAX_PUSH_WORKERS, 256).
-define(PUSH_WORKER_COUNTER, push_worker_counter).
-define(UTILIZATION_WARN_THRESHOLD, 80).
-define(UTILIZATION_WARNING_INTERVAL_SECONDS, 10).

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
    try ets:update_counter(?PUSH_WORKER_COUNTER, active, {2, 1}) of
        N -> spawn_with_capacity(Fun, N)
    catch
        error:badarg ->
            ok = init_counter(),
            retry_spawn(Fun)
    end.

-spec spawn_with_capacity(fun(() -> term()), integer()) -> ok | dropped.
spawn_with_capacity(_Fun, N) when N > ?MAX_PUSH_WORKERS ->
    _ = ets:update_counter(?PUSH_WORKER_COUNTER, active, {2, -1}),
    dropped;
spawn_with_capacity(Fun, N) ->
    maybe_warn_high_utilization(N),
    spawn_tracked_worker(Fun),
    ok.

-spec active_count() -> non_neg_integer().
active_count() ->
    try ets:lookup(?PUSH_WORKER_COUNTER, active) of
        [{active, N}] when is_integer(N), N >= 0 -> N;
        _ -> 0
    catch
        error:badarg -> 0
    end.

-spec maybe_warn_high_utilization(non_neg_integer()) -> ok.
maybe_warn_high_utilization(Active) ->
    case (Active * 100) div ?MAX_PUSH_WORKERS of
        Pct when Pct >= ?UTILIZATION_WARN_THRESHOLD ->
            throttled_utilization_warning(Active, Pct);
        _ ->
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
    case Now - Last of
        Elapsed when Elapsed >= ?UTILIZATION_WARNING_INTERVAL_SECONDS ->
            persistent_term:put({?MODULE, last_util_warn}, Now),
            logger:warning("Push worker pool high utilisation", #{
                utilisation_pct => Pct, active => Active, max => ?MAX_PUSH_WORKERS
            }),
            ok;
        _ ->
            ok
    end.

-spec retry_spawn(fun(() -> term())) -> ok | dropped.
retry_spawn(Fun) ->
    try ets:update_counter(?PUSH_WORKER_COUNTER, active, {2, 1}) of
        N -> spawn_with_capacity(Fun, N)
    catch
        error:badarg -> dropped
    end.

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

-spec counter_table_status() -> live | unavailable.
counter_table_status() ->
    case ets:info(?PUSH_WORKER_COUNTER, size) of
        Size when is_integer(Size) -> live;
        _ -> unavailable
    end.

-spec read_counter(atom()) -> non_neg_integer() | unavailable.
read_counter(Key) ->
    try ets:lookup(?PUSH_WORKER_COUNTER, Key) of
        [{Key, Value}] when is_integer(Value), Value >= 0 -> Value;
        _ -> unavailable
    catch
        error:badarg -> unavailable
    end.

-spec bump_counter(atom(), non_neg_integer()) -> ok.
bump_counter(Key, Increment) ->
    try ets:update_counter(?PUSH_WORKER_COUNTER, Key, {2, Increment}) of
        _Value -> ok
    catch
        error:badarg -> insert_missing_counter(Key, Increment)
    end.

-spec insert_missing_counter(atom(), non_neg_integer()) -> ok.
insert_missing_counter(Key, Increment) ->
    try ets:insert_new(?PUSH_WORKER_COUNTER, {Key, Increment}) of
        true -> ok;
        false -> retry_bump_counter(Key, Increment)
    catch
        error:badarg -> ok
    end.

-spec retry_bump_counter(atom(), non_neg_integer()) -> ok.
retry_bump_counter(Key, Increment) ->
    try ets:update_counter(?PUSH_WORKER_COUNTER, Key, {2, Increment}) of
        _Value -> ok
    catch
        error:badarg -> ok
    end.
