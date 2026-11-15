%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_push_metrics).
-typing([eqwalizer]).

-export([count/1]).

-define(PUSH_COUNTERS, guild_push_counters).
-define(PUSH_COUNTER_KEYS, [
    worker_started,
    worker_completed,
    worker_failed,
    queued,
    dropped_at_limit,
    spawn_failed,
    slot_reclaimed,
    slot_deduped,
    members_table_missing,
    scan_table_unavailable
]).

-spec count(atom()) -> ok.
count(Key) ->
    try ets:update_counter(?PUSH_COUNTERS, Key, {2, 1}) of
        _ -> ok
    catch
        error:badarg ->
            ok = ensure_push_counters(),
            retry_push_counter(Key)
    end.

-spec retry_push_counter(atom()) -> ok.
retry_push_counter(Key) ->
    try ets:update_counter(?PUSH_COUNTERS, Key, {2, 1}) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec ensure_push_counters() -> ok.
ensure_push_counters() ->
    case ets:whereis(?PUSH_COUNTERS) of
        undefined -> create_push_counters();
        _ -> ensure_push_counter_keys()
    end.

-spec create_push_counters() -> ok.
create_push_counters() ->
    ok = guild_ets_utils:ensure_table(?PUSH_COUNTERS, push_counter_table_options()),
    ensure_push_counter_keys().

-spec push_counter_table_options() -> [term()].
push_counter_table_options() ->
    [named_table, public, set, {read_concurrency, true}, {write_concurrency, true}].

-spec ensure_push_counter_keys() -> ok.
ensure_push_counter_keys() ->
    lists:foreach(fun ensure_push_counter_key/1, ?PUSH_COUNTER_KEYS).

-spec ensure_push_counter_key(atom()) -> ok.
ensure_push_counter_key(Key) ->
    try
        _ = ets:insert_new(?PUSH_COUNTERS, {Key, 0}),
        ok
    catch
        error:badarg -> ok
    end.
