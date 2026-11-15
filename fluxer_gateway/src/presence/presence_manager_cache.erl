%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_manager_cache).
-typing([eqwalizer]).

-export([
    ensure_table/0,
    delete_table/0,
    lookup/1,
    put_if_local/2,
    clean/2,
    invalidate/1
]).

-define(PID_CACHE_TABLE, presence_pid_cache).
-define(CACHE_TTL_MS, 300000).

-spec ensure_table() -> ok.
ensure_table() ->
    case ets:whereis(?PID_CACHE_TABLE) of
        undefined ->
            _ = ets:new(?PID_CACHE_TABLE, [
                named_table, public, set, {read_concurrency, true}, {write_concurrency, true}
            ]),
            ok;
        _ ->
            ok
    end.

-spec delete_table() -> ok.
delete_table() ->
    try ets:delete(?PID_CACHE_TABLE) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec lookup(integer()) -> {hit, pid()} | miss.
lookup(UserId) ->
    try ets:lookup(?PID_CACHE_TABLE, UserId) of
        [{UserId, Pid, Timestamp}] -> validate_cache_entry(UserId, Pid, Timestamp);
        [] -> miss
    catch
        error:badarg -> miss
    end.

-spec put_if_local(integer(), pid()) -> ok.
put_if_local(UserId, Pid) ->
    case node(Pid) of
        LocalNode when LocalNode =:= node() ->
            ensure_table(),
            ets:insert(?PID_CACHE_TABLE, {UserId, Pid, erlang:monotonic_time(millisecond)}),
            ok;
        _ ->
            ok
    end.

-spec invalidate(integer()) -> ok.
invalidate(UserId) ->
    try
        ets:delete(?PID_CACHE_TABLE, UserId),
        ok
    catch
        error:badarg -> ok
    end.

-spec clean(integer(), pid()) -> ok.
clean(UserId, Pid) ->
    try
        ets:match_delete(?PID_CACHE_TABLE, {UserId, Pid, '_'}),
        ok
    catch
        error:badarg -> ok
    end.

-spec validate_cache_entry(integer(), pid(), integer()) -> {hit, pid()} | miss.
validate_cache_entry(UserId, Pid, Timestamp) ->
    IsFresh = erlang:monotonic_time(millisecond) - Timestamp < ?CACHE_TTL_MS,
    IsAlive = is_cached_pid_alive(Pid),
    case {IsFresh, IsAlive} of
        {true, true} ->
            {hit, Pid};
        _ ->
            ets:delete(?PID_CACHE_TABLE, UserId),
            miss
    end.

-spec is_cached_pid_alive(pid()) -> boolean().
is_cached_pid_alive(Pid) ->
    case node(Pid) of
        LocalNode when LocalNode =:= node() -> process_liveness:is_alive(Pid);
        _ -> false
    end.
