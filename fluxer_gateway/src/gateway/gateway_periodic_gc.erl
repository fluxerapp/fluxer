%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_periodic_gc).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(GC_INTERVAL_MS, 10_000).
-define(HEALTH_INTERVAL_MS, 30_000).
-define(NATS_MQL_THRESHOLD, 5000).
-define(STUCK_PROC_MQL_THRESHOLD, 100).
-define(STUCK_PROC_MAX_REDUCTIONS, 1_000_000).
-define(LARGE_BROADCASTER_BYTES, 64 * 1024 * 1024).
-define(LARGE_GUILD_BYTES, 256 * 1024 * 1024).
-define(MAX_PROCESS_SCAN, 5000).
-define(NATS_SUSTAINED_CYCLES, 3).
-define(STUCK_SUSTAINED_CYCLES, 3).
-define(GRACEFUL_SHUTDOWN_WAIT_MS, 2000).

-type nats_backlog() :: #{pid() => non_neg_integer()}.
-type stuck_progress() :: #{pid() => {non_neg_integer(), non_neg_integer()}}.
-type state() :: #{
    nats_backlog := nats_backlog(),
    stuck_progress := stuck_progress()
}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} -> {ok, Pid};
        ignore -> {error, ignore};
        {error, E} -> {error, E}
    end.

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:process_flag(fullsweep_after, 50),
    schedule_gc(),
    schedule_health_check(),
    {ok, #{nats_backlog => #{}, stuck_progress => #{}}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, ok, state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(run_gc, State) ->
    gc_large_idle_processes(),
    NatsBacklog = check_nats_health(maps:get(nats_backlog, State, #{})),
    schedule_gc(),
    {noreply, State#{nats_backlog => NatsBacklog}};
handle_info(run_health_check, State) ->
    StuckProgress = reap_stuck_spawns(maps:get(stuck_progress, State, #{})),
    reset_drifted_counters(),
    schedule_health_check(),
    {noreply, State#{stuck_progress => StuckProgress}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec gc_large_idle_processes() -> ok.
gc_large_idle_processes() ->
    lists:foreach(fun gc_if_large_and_idle/1, process_scan_pids()).

-spec gc_if_large_and_idle(pid()) -> ok.
gc_if_large_and_idle(Pid) ->
    try erlang:process_info(Pid, [memory, message_queue_len]) of
        [{memory, Mem}, {message_queue_len, 0}] when Mem >= ?LARGE_BROADCASTER_BYTES ->
            gc_if_known_type(Pid, Mem);
        _ ->
            ok
    catch
        error:badarg -> ok
    end.

-spec gc_if_known_type(pid(), non_neg_integer()) -> ok.
gc_if_known_type(Pid, Memory) ->
    try erlang:process_info(Pid, {dictionary, '$initial_call'}) of
        {{dictionary, '$initial_call'}, InitialCall} ->
            force_gc_by_type(Pid, Memory, InitialCall);
        _ ->
            ok
    catch
        error:badarg -> ok
    end.

-spec force_gc_by_type(pid(), non_neg_integer(), term()) -> ok.
force_gc_by_type(Pid, Mem, {guild_broadcaster, init, 1}) when Mem >= ?LARGE_BROADCASTER_BYTES ->
    erlang:garbage_collect(Pid, [{type, major}]),
    ok;
force_gc_by_type(Pid, Mem, {guild, init, 1}) when Mem >= ?LARGE_GUILD_BYTES ->
    erlang:garbage_collect(Pid, [{type, major}]),
    ok;
force_gc_by_type(_Pid, _Mem, _Type) ->
    ok.

-spec check_nats_health(nats_backlog()) -> nats_backlog().
check_nats_health(Backlog) ->
    case get_nats_pool_slots() of
        {ok, Slots} ->
            Conns = [Conn || {_Idx, Conn} <- maps:to_list(Slots), is_pid(Conn)],
            track_backlogged_conns(Conns, Backlog, #{});
        error ->
            Backlog
    end.

-spec track_backlogged_conns([pid()], nats_backlog(), nats_backlog()) -> nats_backlog().
track_backlogged_conns([], _PrevBacklog, Acc) ->
    Acc;
track_backlogged_conns([Conn | Rest], PrevBacklog, Acc) ->
    NextAcc = track_backlogged_conn(Conn, PrevBacklog, Acc),
    track_backlogged_conns(Rest, PrevBacklog, NextAcc).

-spec track_backlogged_conn(pid(), nats_backlog(), nats_backlog()) -> nats_backlog().
track_backlogged_conn(Conn, PrevBacklog, Acc) ->
    case current_mql(Conn) of
        {ok, MQL} when MQL > ?NATS_MQL_THRESHOLD ->
            Count = maps:get(Conn, PrevBacklog, 0) + 1,
            maybe_kill_sustained_conn(Conn, MQL, Count, Acc);
        _ ->
            Acc
    end.

-spec current_mql(pid()) -> {ok, non_neg_integer()} | error.
current_mql(Pid) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, MQL} -> {ok, MQL};
        undefined -> error
    end.

-spec maybe_kill_sustained_conn(pid(), non_neg_integer(), non_neg_integer(), nats_backlog()) ->
    nats_backlog().
maybe_kill_sustained_conn(Conn, MQL, Count, Acc) when Count >= ?NATS_SUSTAINED_CYCLES ->
    logger:warning("Shutting down sustained backlogged NATS connection", #{
        message_queue_len => MQL, pid => Conn, cycles => Count
    }),
    graceful_stop(Conn),
    Acc;
maybe_kill_sustained_conn(Conn, MQL, Count, Acc) ->
    logger:warning("NATS connection backlogged, awaiting sustained confirmation", #{
        message_queue_len => MQL, pid => Conn, cycles => Count
    }),
    Acc#{Conn => Count}.

-spec graceful_stop(pid()) -> ok.
graceful_stop(Pid) ->
    MRef = monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    after ?GRACEFUL_SHUTDOWN_WAIT_MS ->
        exit(Pid, kill),
        receive
            {'DOWN', MRef, process, Pid, _} -> ok
        after ?GRACEFUL_SHUTDOWN_WAIT_MS ->
            demonitor(MRef, [flush]),
            ok
        end
    end.

-spec get_nats_pool_slots() -> {ok, map()} | error.
get_nats_pool_slots() ->
    try sys:get_state(gateway_nats_pool, 1000) of
        PoolState when is_map(PoolState) ->
            extract_slots(PoolState);
        _ ->
            error
    catch
        _:_ -> error
    end.

-spec extract_slots(map()) -> {ok, map()} | error.
extract_slots(PoolState) ->
    case maps:get(slots, PoolState, #{}) of
        Slots when is_map(Slots) -> {ok, Slots};
        _ -> error
    end.

-spec reap_stuck_spawns(stuck_progress()) -> stuck_progress().
reap_stuck_spawns(Progress) ->
    lists:foldl(
        fun(Pid, Acc) -> reap_if_stuck(Pid, Progress, Acc) end,
        #{},
        process_scan_pids()
    ).

-spec process_scan_pids() -> [pid()].
process_scan_pids() ->
    Pids = erlang:processes(),
    case length(Pids) =< ?MAX_PROCESS_SCAN of
        true ->
            Pids;
        false ->
            Shuffled = [P || {_, P} <- lists:sort([{rand:uniform(), P} || P <- Pids])],
            lists:sublist(Shuffled, ?MAX_PROCESS_SCAN)
    end.

-spec reap_if_stuck(pid(), stuck_progress(), stuck_progress()) -> stuck_progress().
reap_if_stuck(Pid, PrevProgress, Acc) ->
    InfoItems = [initial_call, current_function, message_queue_len, reductions],
    try erlang:process_info(Pid, InfoItems) of
        [
            {initial_call, {erlang, apply, 2}},
            {current_function, CF},
            {message_queue_len, MQL},
            {reductions, Reds}
        ] when
            MQL > ?STUCK_PROC_MQL_THRESHOLD, Reds < ?STUCK_PROC_MAX_REDUCTIONS
        ->
            track_stuck_if_defined(Pid, MQL, Reds, CF, PrevProgress, Acc);
        _ ->
            Acc
    catch
        error:badarg -> Acc
    end.

-spec track_stuck_if_defined(
    pid(),
    non_neg_integer(),
    non_neg_integer(),
    {atom(), atom(), non_neg_integer()} | undefined,
    stuck_progress(),
    stuck_progress()
) -> stuck_progress().
track_stuck_if_defined(_Pid, _MQL, _Reds, undefined, _PrevProgress, Acc) ->
    Acc;
track_stuck_if_defined(Pid, MQL, Reds, {M, F, A}, PrevProgress, Acc) ->
    case is_known_stuck_function({M, F, A}) of
        true ->
            track_stuck(Pid, MQL, Reds, {M, F, A}, PrevProgress, Acc);
        false ->
            Acc
    end.

-spec track_stuck(
    pid(),
    non_neg_integer(),
    non_neg_integer(),
    {atom(), atom(), non_neg_integer()},
    stuck_progress(),
    stuck_progress()
) -> stuck_progress().
track_stuck(Pid, MQL, Reds, CF, PrevProgress, Acc) ->
    case maps:get(Pid, PrevProgress, undefined) of
        {Count, PrevReds} when Reds =:= PrevReds ->
            maybe_reap_sustained(Pid, MQL, Reds, CF, Count + 1, Acc);
        _ ->
            Acc#{Pid => {1, Reds}}
    end.

-spec maybe_reap_sustained(
    pid(),
    non_neg_integer(),
    non_neg_integer(),
    {atom(), atom(), non_neg_integer()},
    non_neg_integer(),
    stuck_progress()
) -> stuck_progress().
maybe_reap_sustained(Pid, MQL, _Reds, CF, Count, Acc) when Count >= ?STUCK_SUSTAINED_CYCLES ->
    logger:warning("Reaping sustained stuck spawned process", #{
        pid => Pid, message_queue_len => MQL, current_function => CF, cycles => Count
    }),
    graceful_stop(Pid),
    Acc;
maybe_reap_sustained(Pid, MQL, Reds, CF, Count, Acc) ->
    logger:warning(
        "Stuck spawned process with no progress, awaiting sustained confirmation", #{
            pid => Pid, message_queue_len => MQL, current_function => CF, cycles => Count
        }
    ),
    Acc#{Pid => {Count, Reds}}.

-spec is_known_stuck_function({atom(), atom(), non_neg_integer()}) -> boolean().
is_known_stuck_function({gen, do_call, _}) -> true;
is_known_stuck_function({prim_inet, recv0, _}) -> true;
is_known_stuck_function({gen_statem, call_clean, _}) -> true;
is_known_stuck_function(_) -> false.

-spec reset_drifted_counters() -> ok.
reset_drifted_counters() ->
    try persistent_term:get(gateway_concurrency_counters) of
        Ref ->
            SessionMax = gateway_rollout_config:max_concurrent_session_starts(),
            GuildMax = gateway_rollout_config:max_concurrent_guild_starts(),
            check_counter(Ref, 1, SessionMax, "session"),
            check_counter(Ref, 2, GuildMax, "guild")
    catch
        error:badarg -> ok
    end.

-spec check_counter(counters:counters_ref(), pos_integer(), pos_integer(), string()) -> ok.
check_counter(Ref, Idx, Max, Label) ->
    Value = counters:get(Ref, Idx),
    case Value < 0 orelse Value > Max * 2 of
        true ->
            logger:warning("Resetting drifted counter", #{label => Label, value => Value}),
            counters:put(Ref, Idx, 0);
        false ->
            ok
    end.

-spec schedule_gc() -> reference().
schedule_gc() ->
    erlang:send_after(?GC_INTERVAL_MS, self(), run_gc).

-spec schedule_health_check() -> reference().
schedule_health_check() ->
    erlang:send_after(?HEALTH_INTERVAL_MS, self(), run_health_check).
