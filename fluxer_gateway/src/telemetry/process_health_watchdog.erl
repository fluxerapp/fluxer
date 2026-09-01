%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(process_health_watchdog).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/0, kill_threshold/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CHECK_INTERVAL_MS, 10_000).
-define(WARNING_THRESHOLD, 1000).
-define(CRITICAL_THRESHOLD, 10000).
-define(KILL_THRESHOLD, 50000).
-define(STUCK_CONSECUTIVE_GROWTH, 3).
-define(MAX_GUILD_WATCHDOG_PIDS, 5000).

-type queue_history() :: #{pid() => [non_neg_integer()]}.
-type state() :: #{history := queue_history()}.

-spec kill_threshold() -> pos_integer().
kill_threshold() ->
    ?KILL_THRESHOLD.

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
    schedule_check(),
    {ok, #{history => #{}}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, ok, state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(run_check, State) ->
    NewState = safe_run_watchdog_check(State),
    schedule_check(),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec safe_run_watchdog_check(state()) -> state().
safe_run_watchdog_check(State) ->
    try
        run_watchdog_check(State)
    catch
        Class:Reason:Stack ->
            logger:error("Watchdog check failed", #{
                class => Class, reason => Reason, stacktrace => Stack
            }),
            State
    end.

-spec run_watchdog_check(state()) -> state().
run_watchdog_check(#{history := History}) ->
    Pids = collect_monitored_pids(),
    NewHistory = check_all_pids(Pids, History),
    PrunedHistory = prune_dead(NewHistory, Pids),
    #{history => PrunedHistory}.

-spec collect_monitored_pids() -> [{pid(), binary()}].
collect_monitored_pids() ->
    guild_pids() ++ singleton_pids() ++ dispatch_relay_worker_pids().

-spec dispatch_relay_worker_pids() -> [{pid(), binary()}].
dispatch_relay_worker_pids() ->
    Workers = gateway_dispatch_relay_batch:current_workers(),
    [
        {Pid, iolist_to_binary(["dispatch_relay_worker:", integer_to_list(Index)])}
     || {Index, Pid} <- lists:enumerate(0, Workers), is_pid(Pid)
    ].

-spec guild_pids() -> [{pid(), binary()}].
guild_pids() ->
    try guild_pid_rows() of
        Rows ->
            [
                {Pid, iolist_to_binary(["guild:", integer_to_list(GuildId)])}
             || {GuildId, Pid} <- Rows, is_integer(GuildId), is_pid(Pid), node(Pid) =:= node()
            ]
    catch
        error:badarg -> []
    end.

-spec guild_pid_rows() -> [term()].
guild_pid_rows() ->
    MatchSpec = [{{'$1', '$2'}, [], [{{'$1', '$2'}}]}],
    case ets:select(guild_pid_cache, MatchSpec, ?MAX_GUILD_WATCHDOG_PIDS) of
        {Rows, _Continuation} -> Rows;
        '$end_of_table' -> []
    end.

-spec singleton_pids() -> [{pid(), binary()}].
singleton_pids() ->
    Names = [
        session_manager,
        presence_manager,
        guild_manager,
        call_manager,
        push_dispatcher,
        push,
        gateway_nats_rpc,
        gateway_nats_pool,
        gateway_dispatch_relay,
        gateway_rollout_config
    ],
    lists:filtermap(fun resolve_singleton/1, Names).

-spec resolve_singleton(atom()) -> {true, {pid(), binary()}} | false.
resolve_singleton(Name) ->
    case whereis(Name) of
        undefined -> false;
        Pid when is_pid(Pid) -> {true, {Pid, atom_to_binary(Name, utf8)}}
    end.

-spec check_all_pids([{pid(), binary()}], queue_history()) -> queue_history().
check_all_pids(Pids, History) ->
    lists:foldl(fun({Pid, Label}, Acc) -> check_pid(Pid, Label, Acc) end, History, Pids).

-spec check_pid(pid(), binary(), queue_history()) -> queue_history().
check_pid(Pid, Label, History) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, Len} ->
            handle_queue_len(Pid, Label, Len, History);
        undefined ->
            maps:remove(Pid, History)
    end.

-spec handle_queue_len(pid(), binary(), non_neg_integer(), queue_history()) -> queue_history().
handle_queue_len(Pid, Label, Len, History) ->
    PrevSamples = maps:get(Pid, History, []),
    NewSamples = update_samples(Len, PrevSamples),
    apply_thresholds(Pid, Label, Len, NewSamples),
    maybe_act_on_stuck(Pid, Label, NewSamples),
    History#{Pid => NewSamples}.

-spec update_samples(non_neg_integer(), [non_neg_integer()]) -> [non_neg_integer()].
update_samples(Len, PrevSamples) ->
    lists:sublist([Len | PrevSamples], ?STUCK_CONSECUTIVE_GROWTH).

-spec apply_thresholds(pid(), binary(), non_neg_integer(), [non_neg_integer()]) -> ok.
apply_thresholds(Pid, Label, Len, Samples) when Len > ?KILL_THRESHOLD ->
    maybe_kill_over_threshold(Pid, Label, Len, Samples);
apply_thresholds(Pid, Label, Len, _Samples) when Len > ?CRITICAL_THRESHOLD ->
    logger:critical("Critical mailbox size, forcing GC", #{
        label => Label, pid => Pid, message_queue_len => Len
    }),
    erlang:garbage_collect(Pid, [{type, major}]),
    ok;
apply_thresholds(Pid, Label, Len, _Samples) when Len > ?WARNING_THRESHOLD ->
    logger:warning("High mailbox size", #{
        label => Label, pid => Pid, message_queue_len => Len
    }),
    ok;
apply_thresholds(_Pid, _Label, _Len, _Samples) ->
    ok.

-spec maybe_kill_over_threshold(pid(), binary(), non_neg_integer(), [non_neg_integer()]) -> ok.
maybe_kill_over_threshold(Pid, Label, Len, Samples) ->
    case is_sustained_over_kill_threshold(Samples) of
        true ->
            logger:critical("Killing process over sustained kill threshold", #{
                label => Label,
                pid => Pid,
                message_queue_len => Len,
                kill_threshold => ?KILL_THRESHOLD,
                samples => Samples
            }),
            exit(Pid, kill),
            ok;
        false ->
            logger:critical(
                "Mailbox over kill threshold, forcing GC pending sustained growth", #{
                    label => Label,
                    pid => Pid,
                    message_queue_len => Len,
                    kill_threshold => ?KILL_THRESHOLD,
                    samples => Samples
                }
            ),
            erlang:garbage_collect(Pid, [{type, major}]),
            ok
    end.

-spec is_sustained_over_kill_threshold([non_neg_integer()]) -> boolean().
is_sustained_over_kill_threshold(Samples) when length(Samples) < ?STUCK_CONSECUTIVE_GROWTH ->
    false;
is_sustained_over_kill_threshold(Samples) ->
    lists:all(fun(S) -> S > ?KILL_THRESHOLD end, Samples) andalso
        is_strictly_decreasing(Samples).

-spec maybe_act_on_stuck(pid(), binary(), [non_neg_integer()]) -> ok.
maybe_act_on_stuck(Pid, Label, Samples) ->
    case is_monotonically_growing(Samples) of
        true ->
            logger:warning("Stuck process detected, monotonically growing mailbox", #{
                label => Label,
                pid => Pid,
                samples => Samples,
                consecutive_checks => length(Samples)
            });
        false ->
            ok
    end.

-spec is_monotonically_growing([non_neg_integer()]) -> boolean().
is_monotonically_growing(Samples) when length(Samples) < ?STUCK_CONSECUTIVE_GROWTH ->
    false;
is_monotonically_growing(Samples) ->
    Oldest = lists:last(Samples),
    Oldest > ?WARNING_THRESHOLD andalso is_strictly_decreasing(Samples).

-dialyzer({no_match, is_strictly_decreasing/1}).
-spec is_strictly_decreasing([non_neg_integer()]) -> boolean().
is_strictly_decreasing([]) ->
    false;
is_strictly_decreasing([_]) ->
    true;
is_strictly_decreasing([A, B | Rest]) ->
    A > B andalso is_strictly_decreasing([B | Rest]).

-spec prune_dead(queue_history(), [{pid(), binary()}]) -> queue_history().
prune_dead(History, LivePids) ->
    LiveSet = sets:from_list([Pid || {Pid, _} <- LivePids], [{version, 2}]),
    maps:filter(fun(Pid, _) -> sets:is_element(Pid, LiveSet) end, History).

-spec schedule_check() -> reference().
schedule_check() ->
    erlang:send_after(?CHECK_INTERVAL_MS, self(), run_check).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

update_samples_keeps_bounded_test() ->
    S1 = update_samples(100, []),
    ?assertEqual([100], S1),
    S2 = update_samples(200, S1),
    ?assertEqual([200, 100], S2),
    S3 = update_samples(300, S2),
    ?assertEqual([300, 200, 100], S3),
    S4 = update_samples(400, S3),
    ?assertEqual([400, 300, 200], S4).

is_monotonically_growing_true_test() ->
    ?assert(is_monotonically_growing([3000, 2000, 1500])).

is_monotonically_growing_false_when_decreasing_test() ->
    ?assertNot(is_monotonically_growing([500, 2000, 1500])).

is_monotonically_growing_false_when_flat_test() ->
    ?assertNot(is_monotonically_growing([2000, 2000, 2000])).

is_monotonically_growing_false_below_threshold_test() ->
    ?assertNot(is_monotonically_growing([500, 400, 300])).

is_monotonically_growing_false_insufficient_samples_test() ->
    ?assertNot(is_monotonically_growing([5000, 4000])),
    ?assertNot(is_monotonically_growing([5000])),
    ?assertNot(is_monotonically_growing([])).

is_strictly_decreasing_test() ->
    ?assert(is_strictly_decreasing([3, 2, 1])),
    ?assertNot(is_strictly_decreasing([3, 3, 1])),
    ?assertNot(is_strictly_decreasing([3, 2, 4])),
    ?assertNot(is_strictly_decreasing([])),
    ?assert(is_strictly_decreasing([1])).

prune_dead_removes_absent_pids_test() ->
    Pid1 = self(),
    Pid2 = spawn(fun() -> ok end),
    ok = gateway_retry_timer:wait(50),
    History = #{Pid1 => [100], Pid2 => [200]},
    LivePids = [{Pid1, <<"test">>}],
    Pruned = prune_dead(History, LivePids),
    ?assertEqual(#{Pid1 => [100]}, Pruned).

resolve_singleton_missing_test() ->
    ?assertEqual(false, resolve_singleton(nonexistent_process_xyz_test)).

dispatch_relay_workers_are_monitored_test() ->
    Key = {gateway_dispatch_relay, state},
    Previous = persistent_term:get(Key, undefined),
    Worker = self(),
    persistent_term:put(Key, #{workers => {Worker}, shard_count => 1}),
    try
        Expected = {Worker, <<"dispatch_relay_worker:0">>},
        ?assertEqual([Expected], dispatch_relay_worker_pids()),
        ?assert(lists:member(Expected, collect_monitored_pids()))
    after
        restore_relay_state_test_term(Key, Previous)
    end.

check_pid_dead_process_test() ->
    Pid = spawn(fun() -> ok end),
    ok = gateway_retry_timer:wait(50),
    History = #{Pid => [100, 50]},
    Result = check_pid(Pid, <<"dead">>, History),
    ?assertNot(maps:is_key(Pid, Result)).

apply_thresholds_below_warning_test() ->
    ?assertEqual(ok, apply_thresholds(self(), <<"test">>, 500, [500])).

is_sustained_over_kill_threshold_true_test() ->
    ?assert(is_sustained_over_kill_threshold([70000, 60000, 55000])).

is_sustained_over_kill_threshold_false_single_spike_test() ->
    ?assertNot(is_sustained_over_kill_threshold([70000, 100, 100])).

is_sustained_over_kill_threshold_false_not_growing_test() ->
    ?assertNot(is_sustained_over_kill_threshold([55000, 60000, 70000])).

is_sustained_over_kill_threshold_false_insufficient_samples_test() ->
    ?assertNot(is_sustained_over_kill_threshold([70000, 60000])),
    ?assertNot(is_sustained_over_kill_threshold([70000])),
    ?assertNot(is_sustained_over_kill_threshold([])).

safe_run_watchdog_check_survives_bad_state_test() ->
    BadState = maps:remove(history, #{history => #{}}),
    ?assertEqual(BadState, safe_run_watchdog_check(eqwalizer:dynamic_cast(BadState))).

restore_relay_state_test_term(Key, undefined) ->
    _ = persistent_term:erase(Key),
    ok;
restore_relay_state_test_term(Key, Previous) ->
    persistent_term:put(Key, Previous).

-endif.
