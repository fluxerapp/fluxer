%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_handoff_freeze).
-typing([eqwalizer]).

-export([
    transfer/5,
    default_opts/0,
    is_frozen/1,
    shard_pid/2,
    ensure_absent/3
]).

-define(SHARD_TABLE, guild_manager_shard_table).
-define(GUARD_EXPRS,
    "R = erlang:monitor(process, C),"
    " C ! {freeze_guard_ready, self()},"
    " receive"
    " {freeze_guard_release, C} -> erlang:demonitor(R, [flush]), released;"
    " {'DOWN', R, process, C, _} ->"
    " try sys:resume(P, 60000) of ok -> resumed catch _:Reason -> {resume_failed, Reason} end"
    " end."
).
-define(EXPORT_EXPRS,
    "maps:merge(maps:update_with(data, fun(D) -> maps:without(K, D) end,"
    " guild_handoff:export_handoff_state(sys:get_state(P, T))), X)."
).

-type guild_id() :: integer().
-type hook() :: fun(() -> ok | {error, term()}).
-type started_hook() :: fun((pid()) -> ok | {error, term()}).
-type opts() :: #{
    suspend_timeout => pos_integer(),
    export_timeout => pos_integer(),
    start_timeout => pos_integer(),
    stop_timeout => pos_integer(),
    barrier_timeout => pos_integer(),
    rpc_timeout => pos_integer(),
    guard_timeout => pos_integer(),
    freeze_budget => pos_integer(),
    forward_rounds => non_neg_integer(),
    transfer_sessions => boolean(),
    max_heap_words => non_neg_integer(),
    measure => boolean(),
    before_start => hook(),
    after_start => started_hook(),
    on_abort => hook()
}.
-type report() :: map().
-type result() :: {ok, report()} | {error, report()}.
-type freeze() :: #{pid := pid(), guard := pid(), guard_ref := reference()}.

-export_type([opts/0, report/0, result/0]).

-spec default_opts() -> opts().
default_opts() ->
    #{
        suspend_timeout => 30000,
        export_timeout => 120000,
        start_timeout => 120000,
        stop_timeout => 60000,
        barrier_timeout => 30000,
        rpc_timeout => 10000,
        guard_timeout => 10000,
        freeze_budget => 90000,
        forward_rounds => 3,
        transfer_sessions => true,
        max_heap_words => 0,
        measure => false,
        before_start => fun() -> ok end,
        after_start => fun(_Pid) -> ok end,
        on_abort => fun() -> ok end
    }.

-spec transfer(guild_id(), pid(), pid(), node(), opts()) -> result().
transfer(GuildId, SrcPid, SrcShard, TargetNode, Opts0) ->
    Opts = maps:merge(default_opts(), Opts0),
    Parent = self(),
    Tag = make_ref(),
    {Pid, Ref} = spawn_monitor(fun() ->
        process_flag(trap_exit, true),
        ok = maybe_limit_heap(maps:get(max_heap_words, Opts)),
        Parent ! {Tag, run(GuildId, SrcPid, SrcShard, TargetNode, Opts)}
    end),
    receive
        {Tag, Result} ->
            erlang:demonitor(Ref, [flush]),
            Result;
        {'DOWN', Ref, process, Pid, Reason} ->
            {error, #{phase => crashed, reason => Reason}}
    end.

-spec is_frozen(pid()) -> boolean() | {error, term()}.
is_frozen(Pid) ->
    case rpc_call(node(Pid), erlang, process_info, [Pid, current_function], 10000) of
        {ok, {current_function, {sys, _, _}}} -> true;
        {ok, {current_function, _}} -> false;
        {ok, undefined} -> {error, noproc};
        {ok, Other} -> {error, {unexpected, Other}};
        {error, _} = Error -> Error
    end.

-spec shard_pid(guild_id(), node()) -> {ok, pid()} | {error, term()}.
shard_pid(GuildId, Node) ->
    case rpc_call(Node, ets, lookup, [?SHARD_TABLE, shard_count], 10000) of
        {ok, [{shard_count, Count}]} when is_integer(Count), Count > 0 ->
            Index = guild_manager_shards:select_shard(GuildId, Count),
            indexed_shard_pid(Node, Index);
        {ok, _} ->
            {error, shard_table_unavailable};
        {error, _} = Error ->
            Error
    end.

-spec ensure_absent(guild_id(), node(), opts()) -> ok | {error, term()}.
ensure_absent(GuildId, Node, Opts0) ->
    Opts = maps:merge(default_opts(), Opts0),
    StopTimeout = maps:get(start_timeout, Opts) + maps:get(stop_timeout, Opts),
    case shard_pid(GuildId, Node) of
        {ok, Shard} ->
            _ = safe_call(Shard, {stop_guild, GuildId}, StopTimeout),
            case safe_call(Shard, {lookup, GuildId}, maps:get(rpc_timeout, Opts)) of
                {error, not_found} -> ok;
                {ok, Pid} when is_pid(Pid) -> {error, {still_running, Pid}};
                Other -> {error, {unverified, Other}}
            end;
        {error, _} = Error ->
            Error
    end.

-spec run(guild_id(), pid(), pid(), node(), opts()) -> result().
run(GuildId, SrcPid, SrcShard, TargetNode, Opts) ->
    Ctx = #{
        guild_id => GuildId,
        src => SrcPid,
        src_shard => SrcShard,
        target => TargetNode,
        opts => Opts,
        t0 => now_ms(),
        report => #{source => SrcPid, target_node => TargetNode}
    },
    case is_frozen(SrcPid) of
        false -> run_freeze(Ctx);
        true -> fail(preflight, already_frozen, Ctx);
        {error, Reason} -> fail(preflight, Reason, Ctx)
    end.

-spec run_freeze(map()) -> result().
run_freeze(#{src := SrcPid, opts := Opts} = Ctx) ->
    T0 = now_ms(),
    case freeze(SrcPid, Opts) of
        {ok, Freeze} ->
            Ctx1 = put_ms(suspend_ms, T0, Ctx#{freeze => Freeze, frozen_at => now_ms()}),
            run_export(Ctx1);
        {error, Reason} ->
            fail(freeze, Reason, Ctx)
    end.

-spec run_export(map()) -> result().
run_export(#{src := SrcPid, opts := Opts} = Ctx) ->
    T0 = now_ms(),
    case export(SrcPid, Opts) of
        {ok, Export} ->
            Ctx1 = put_ms(export_ms, T0, Ctx),
            Ctx2 = put_report(export_measure, measure_export(Export, Opts), Ctx1),
            run_before_start(Export, Ctx2);
        {error, Reason} ->
            abort(export, Reason, Ctx)
    end.

-spec run_before_start(map(), map()) -> result().
run_before_start(Export, #{opts := Opts} = Ctx) ->
    case within_budget(Ctx) of
        true -> run_hook(before_start, maps:get(before_start, Opts), Export, Ctx);
        false -> abort(before_start, freeze_budget_exceeded, Ctx)
    end.

-spec run_hook(before_start, hook(), map(), map()) -> result().
run_hook(Phase, Hook, Export, Ctx) ->
    case safe_hook(Hook) of
        ok -> run_start(Export, Ctx#{routed => true});
        {error, Reason} -> abort(Phase, Reason, Ctx#{routed => true})
    end.

-spec run_start(map(), map()) -> result().
run_start(Export, #{guild_id := GuildId, target := Target, opts := Opts} = Ctx) ->
    T0 = now_ms(),
    Ctx1 = Ctx#{start_sent => true},
    case start_on(GuildId, Target, Export, maps:get(start_timeout, Opts)) of
        {ok, NewPid} ->
            run_after_start(NewPid, put_ms(start_ms, T0, Ctx1#{new_pid => NewPid}));
        {error, Reason} ->
            abort(start, Reason, put_ms(start_ms, T0, Ctx1))
    end.

-spec run_after_start(pid(), map()) -> result().
run_after_start(NewPid, #{opts := Opts} = Ctx) ->
    T0 = now_ms(),
    Hook = maps:get(after_start, Opts),
    case safe_started_hook(Hook, NewPid) of
        ok -> run_commit(NewPid, put_ms(route_ms, T0, Ctx));
        {error, Reason} -> abort(after_start, Reason, put_ms(route_ms, T0, Ctx))
    end.

-spec run_commit(pid(), map()) -> result().
run_commit(NewPid, #{src := SrcPid, opts := Opts} = Ctx) ->
    case forward_rounds(SrcPid, NewPid, maps:get(forward_rounds, Opts), 0, [], Opts) of
        {ok, Seen, Rounds} ->
            commit_final(NewPid, Seen, put_report(rounds, lists:reverse(Rounds), Ctx));
        {error, {capture_failed, noproc}, Rounds} ->
            source_gone(NewPid, put_report(rounds, lists:reverse(Rounds), Ctx));
        {error, Reason, Rounds} ->
            abort(forward, Reason, put_report(rounds, lists:reverse(Rounds), Ctx))
    end.

-spec commit_final(pid(), non_neg_integer(), map()) -> result().
commit_final(NewPid, Seen, #{src := SrcPid} = Ctx) ->
    case {target_exit(NewPid), is_frozen(SrcPid)} of
        {{exited, Reason}, _} ->
            abort(target_died, Reason, Ctx);
        {alive, true} ->
            stop_and_drain(NewPid, Seen, Ctx);
        {alive, {error, noproc}} ->
            source_gone(NewPid, Ctx);
        {alive, Frozen} ->
            abort(final_capture, {not_frozen, Frozen}, Ctx)
    end.

-spec target_exit(pid()) -> alive | {exited, term()}.
target_exit(NewPid) ->
    receive
        {'EXIT', NewPid, Reason} -> {exited, Reason}
    after 0 -> alive
    end.

-spec source_gone(pid(), map()) -> result().
source_gone(NewPid, #{src := SrcPid, opts := Opts} = Ctx) ->
    case {target_exit(NewPid), source_alive(SrcPid, Opts)} of
        {alive, false} -> finish(NewPid, put_report(source_gone, true, Ctx));
        Other -> abort(source_gone, {source_state, Other}, Ctx)
    end.

-spec stop_and_drain(pid(), non_neg_integer(), map()) -> result().
stop_and_drain(NewPid, Seen, #{src := SrcPid, opts := Opts} = Ctx) ->
    case capture(SrcPid, Opts) of
        {ok, Msgs} ->
            New = drop_seen(Seen, relevant(Msgs)),
            Stats = forward(New, NewPid, final),
            Parent = self(),
            Tag = make_ref(),
            {Pid, Ref} = spawn_monitor(fun() -> Parent ! {Tag, stop_source(Ctx)} end),
            Drain = #{
                stopper => {Pid, Ref, Tag},
                stop => pending,
                stats => Stats,
                captures => 1,
                t0 => now_ms()
            },
            drain(NewPid, Seen + length(New), Drain, Ctx);
        {error, noproc} ->
            source_gone(NewPid, Ctx);
        {error, Reason} ->
            abort(final_capture, Reason, Ctx)
    end.

-spec drain(pid(), non_neg_integer(), map(), map()) -> result().
drain(NewPid, Seen, Drain, #{src := SrcPid, opts := Opts} = Ctx) ->
    case capture(SrcPid, Opts) of
        {ok, Msgs} ->
            New = drop_seen(Seen, relevant(Msgs)),
            Stats = merge_counts(maps:get(stats, Drain), forward(New, NewPid, final)),
            Drain1 = Drain#{stats => Stats, captures => maps:get(captures, Drain) + 1},
            drain_alive(NewPid, Seen + length(New), poll_stopper(Drain1, 1), Ctx);
        {error, noproc} ->
            drained(NewPid, poll_stopper(Drain, maps:get(stop_timeout, Opts)), Ctx);
        {error, Reason} ->
            Polled = poll_stopper(Drain, maps:get(stop_timeout, Opts)),
            drain_unknown(NewPid, Reason, Polled, Ctx)
    end.

-spec drain_alive(pid(), non_neg_integer(), map(), map()) -> result().
drain_alive(NewPid, Seen, #{stop := pending} = Drain, Ctx) ->
    drain(NewPid, Seen, Drain, Ctx);
drain_alive(NewPid, Seen, #{stop := ok} = Drain, #{src := SrcPid, opts := Opts} = Ctx) ->
    case now_ms() - maps:get(t0, Drain) > maps:get(stop_timeout, Opts) of
        true -> exit(SrcPid, kill);
        false -> timer:sleep(1)
    end,
    drain(NewPid, Seen, Drain, Ctx);
drain_alive(_NewPid, _Seen, #{stop := Reason}, Ctx) ->
    abort(stop, Reason, Ctx).

-spec drain_unknown(pid(), term(), map(), map()) -> result().
drain_unknown(NewPid, Reason, Drain, #{src := SrcPid, opts := Opts} = Ctx) ->
    case source_alive(SrcPid, Opts) of
        false -> drained(NewPid, Drain, Ctx);
        _ -> abort(stop, {capture_failed, Reason, maps:get(stop, Drain)}, Ctx)
    end.

-spec drained(pid(), map(), map()) -> result().
drained(NewPid, Drain, Ctx) ->
    Final = (maps:get(stats, Drain))#{
        captures => maps:get(captures, Drain),
        stop => maps:get(stop, Drain)
    },
    Ctx1 = put_report(stop_ms, now_ms() - maps:get(t0, Drain), Ctx),
    finish(NewPid, put_report(final, Final, Ctx1)).

-spec poll_stopper(map(), non_neg_integer()) -> map().
poll_stopper(#{stop := pending, stopper := {Pid, Ref, Tag}} = Drain, Timeout) ->
    receive
        {Tag, Result} ->
            erlang:demonitor(Ref, [flush]),
            Drain#{stop => Result};
        {'DOWN', Ref, process, Pid, Reason} ->
            Drain#{stop => {error, {stopper_crashed, Reason}}}
    after Timeout ->
        Drain
    end;
poll_stopper(Drain, _Timeout) ->
    Drain.

-spec merge_counts(map(), map()) -> map().
merge_counts(Left, Right) ->
    maps:merge_with(fun(_Key, A, B) -> A + B end, Left, Right).

-spec finish(pid(), map()) -> result().
finish(NewPid, #{freeze := Freeze, frozen_at := FrozenAt} = Ctx) ->
    release_guard(Freeze),
    unlink(NewPid),
    Report = maps:get(report, Ctx),
    {ok, Report#{
        new_pid => NewPid,
        frozen_ms => now_ms() - FrozenAt,
        total_ms => now_ms() - maps:get(t0, Ctx)
    }}.

-spec fail(atom(), term(), map()) -> result().
fail(Phase, Reason, Ctx) ->
    Report = maps:get(report, Ctx),
    {error, Report#{phase => Phase, reason => Reason, total_ms => now_ms() - maps:get(t0, Ctx)}}.

-spec abort(atom(), term(), map()) -> result().
abort(Phase, Reason, #{opts := Opts} = Ctx) ->
    Routes = abort_routes(Ctx, maps:get(on_abort, Opts)),
    Target = abort_target(Ctx),
    Thaw = thaw(maps:get(freeze, Ctx), Opts),
    Report = maps:get(report, Ctx),
    {error, Report#{
        phase => Phase,
        reason => Reason,
        abort => #{routes => Routes, target => Target, thaw => Thaw},
        total_ms => now_ms() - maps:get(t0, Ctx)
    }}.

-spec abort_routes(map(), hook()) -> ok | {error, term()} | skipped.
abort_routes(#{routed := true}, Hook) ->
    safe_hook(Hook);
abort_routes(_Ctx, _Hook) ->
    skipped.

-spec abort_target(map()) -> ok | {error, term()} | skipped.
abort_target(#{start_sent := true, guild_id := GuildId, target := Target, opts := Opts} = Ctx) ->
    Result = ensure_absent(GuildId, Target, Opts),
    case maps:get(new_pid, Ctx, undefined) of
        Pid when is_pid(Pid) -> unlink_and_kill_if(Result, Pid);
        _ -> Result
    end;
abort_target(_Ctx) ->
    skipped.

-spec unlink_and_kill_if(ok | {error, term()}, pid()) -> ok | {error, term()}.
unlink_and_kill_if(ok, Pid) ->
    unlink(Pid),
    ok;
unlink_and_kill_if({error, _} = Error, Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    Error.

-spec freeze(pid(), opts()) -> {ok, freeze()} | {error, term()}.
freeze(SrcPid, Opts) ->
    case spawn_guard(SrcPid, maps:get(guard_timeout, Opts)) of
        {ok, Guard, GuardRef} ->
            Freeze = #{pid => SrcPid, guard => Guard, guard_ref => GuardRef},
            case suspend(SrcPid, maps:get(suspend_timeout, Opts)) of
                ok ->
                    {ok, Freeze};
                {error, Reason} ->
                    _ = thaw(Freeze, Opts),
                    {error, {suspend_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {guard_failed, Reason}}
    end.

-spec thaw(freeze(), opts()) -> ok | {error, term()}.
thaw(#{pid := SrcPid} = Freeze, Opts) ->
    Result =
        try sys:resume(SrcPid, maps:get(suspend_timeout, Opts)) of
            ok -> ok
        catch
            exit:Reason -> {error, Reason}
        end,
    release_guard(Freeze),
    Result.

-spec suspend(pid(), pos_integer()) -> ok | {error, term()}.
suspend(SrcPid, Timeout) ->
    try sys:suspend(SrcPid, Timeout) of
        ok -> ok
    catch
        exit:Reason -> {error, Reason}
    end.

-spec spawn_guard(pid(), pos_integer()) -> {ok, pid(), reference()} | {error, term()}.
spawn_guard(SrcPid, Timeout) ->
    Bindings = bindings([{'C', self()}, {'P', SrcPid}]),
    Guard = erlang:spawn(node(SrcPid), erl_eval, exprs, [parse(?GUARD_EXPRS), Bindings]),
    Ref = erlang:monitor(process, Guard),
    receive
        {freeze_guard_ready, Guard} -> {ok, Guard, Ref};
        {'DOWN', Ref, process, Guard, Reason} -> {error, Reason}
    after Timeout ->
        erlang:demonitor(Ref, [flush]),
        exit(Guard, kill),
        {error, guard_timeout}
    end.

-spec release_guard(freeze()) -> ok.
release_guard(#{guard := Guard, guard_ref := Ref}) ->
    erlang:demonitor(Ref, [flush]),
    Guard ! {freeze_guard_release, self()},
    ok.

-spec export(pid(), opts()) -> {ok, map()} | {error, term()}.
export(SrcPid, Opts) ->
    Timeout = maps:get(export_timeout, Opts),
    Bindings = bindings([
        {'P', SrcPid},
        {'T', Timeout},
        {'K', guild_handoff:derived_data_keys()},
        {'X', export_overrides(Opts)}
    ]),
    Args = [parse(?EXPORT_EXPRS), Bindings],
    case rpc_call(node(SrcPid), erl_eval, exprs, Args, Timeout + 5000) of
        {ok, {value, Export, _}} -> validate_export(Export);
        {ok, Other} -> {error, {unexpected_export, Other}};
        {error, _} = Error -> Error
    end.

-spec export_overrides(opts()) -> map().
export_overrides(#{transfer_sessions := false}) ->
    #{sessions => #{}};
export_overrides(_Opts) ->
    #{}.

-spec validate_export(term()) -> {ok, map()} | {error, term()}.
validate_export(Export) when is_map(Export) ->
    case guild_handoff:validate_handoff_state(Export) of
        ok -> {ok, Export};
        {error, Missing} -> {error, {invalid_export, Missing}}
    end;
validate_export(Other) ->
    {error, {invalid_export, Other}}.

-spec measure_export(map(), opts()) -> map() | skipped.
measure_export(Export, #{measure := true}) ->
    T0 = now_ms(),
    Bytes = erlang:external_size(Export),
    #{external_bytes => Bytes, measure_ms => now_ms() - T0};
measure_export(_Export, _Opts) ->
    skipped.

-spec start_on(guild_id(), node(), map(), pos_integer()) -> {ok, pid()} | {error, term()}.
start_on(GuildId, Target, Export, Timeout) ->
    case shard_pid(GuildId, Target) of
        {ok, Shard} ->
            case safe_call(Shard, {start_transferred, GuildId, Export}, Timeout) of
                {ok, NewPid} when is_pid(NewPid) ->
                    link(NewPid),
                    {ok, NewPid};
                {error, Reason} ->
                    {error, Reason};
                Other ->
                    {error, {unexpected_start_reply, Other}}
            end;
        {error, _} = Error ->
            Error
    end.

-spec forward_rounds(pid(), pid(), non_neg_integer(), non_neg_integer(), [map()], opts()) ->
    {ok, non_neg_integer(), [map()]} | {error, term(), [map()]}.
forward_rounds(_SrcPid, _NewPid, 0, Seen, Rounds, _Opts) ->
    {ok, Seen, Rounds};
forward_rounds(SrcPid, NewPid, Left, Seen, Rounds, Opts) ->
    case capture(SrcPid, Opts) of
        {ok, Msgs} ->
            Relevant = relevant(Msgs),
            forward_round(SrcPid, NewPid, Left, Seen, Relevant, Rounds, Opts);
        {error, Reason} ->
            {error, {capture_failed, Reason}, Rounds}
    end.

-spec forward_round(
    pid(), pid(), pos_integer(), non_neg_integer(), [term()], [map()], opts()
) -> {ok, non_neg_integer(), [map()]} | {error, term(), [map()]}.
forward_round(_SrcPid, _NewPid, _Left, Seen, Relevant, Rounds, _Opts) when
    length(Relevant) < Seen
->
    {error, mailbox_shrank, Rounds};
forward_round(_SrcPid, _NewPid, _Left, Seen, Relevant, Rounds, _Opts) when
    length(Relevant) =:= Seen
->
    {ok, Seen, Rounds};
forward_round(SrcPid, NewPid, Left, Seen, Relevant, Rounds, Opts) ->
    New = drop_seen(Seen, Relevant),
    Stats = forward(New, NewPid, rounds),
    T0 = now_ms(),
    Barrier = barrier(NewPid, maps:get(barrier_timeout, Opts)),
    Round = Stats#{captured => length(New), barrier => Barrier, barrier_ms => now_ms() - T0},
    forward_rounds(SrcPid, NewPid, Left - 1, Seen + length(New), [Round | Rounds], Opts).

-spec drop_seen(non_neg_integer(), [term()]) -> [term()].
drop_seen(Seen, Relevant) when length(Relevant) >= Seen ->
    lists:nthtail(Seen, Relevant);
drop_seen(_Seen, _Relevant) ->
    [].

-spec relevant([term()]) -> [term()].
relevant(Msgs) ->
    [Msg || Msg <- Msgs, forwardable(Msg)].

-spec forwardable(term()) -> boolean().
forwardable({'$gen_call', _From, _Request}) -> true;
forwardable({'$gen_cast', _Request}) -> true;
forwardable({presence, UserId, _Payload}) when is_integer(UserId) -> true;
forwardable({reconcile_user_presence, _UserId}) -> true;
forwardable({clear_stale_cached_voice_states, _Ids}) -> true;
forwardable(_Msg) -> false.

-spec forward([term()], pid(), rounds | final) -> map().
forward(Msgs, NewPid, Phase) ->
    lists:foldl(
        fun(Msg, Acc) ->
            Kind = forward_msg(Msg, NewPid, Phase),
            maps:update_with(Kind, fun(N) -> N + 1 end, 1, Acc)
        end,
        #{},
        Msgs
    ).

-spec forward_msg(term(), pid(), rounds | final) -> atom().
forward_msg({'$gen_call', _From, _Request} = Msg, NewPid, rounds) ->
    NewPid ! Msg,
    call;
forward_msg({'$gen_call', _From, _Request}, _NewPid, final) ->
    dropped_call;
forward_msg({'$gen_cast', {session_connect_worker_done, _, _, _, _}}, _NewPid, _Phase) ->
    dropped_worker_result;
forward_msg({'$gen_cast', _Request} = Msg, NewPid, _Phase) ->
    NewPid ! Msg,
    cast;
forward_msg({presence, UserId, _Payload}, NewPid, _Phase) ->
    NewPid ! {reconcile_user_presence, UserId},
    presence_repair;
forward_msg({reconcile_user_presence, _UserId} = Msg, NewPid, _Phase) ->
    NewPid ! Msg,
    presence_repair;
forward_msg({clear_stale_cached_voice_states, _Ids} = Msg, NewPid, _Phase) ->
    NewPid ! Msg,
    voice_cleanup.

-spec barrier(pid(), pos_integer()) -> ok | {error, term()}.
barrier(NewPid, Timeout) ->
    case safe_call(NewPid, {get_guild_id}, Timeout) of
        Id when is_integer(Id) -> ok;
        Other -> {error, Other}
    end.

-spec capture(pid(), opts()) -> {ok, [term()]} | {error, term()}.
capture(SrcPid, Opts) ->
    Args = [SrcPid, messages],
    case rpc_call(node(SrcPid), erlang, process_info, Args, maps:get(rpc_timeout, Opts)) of
        {ok, {messages, Msgs}} when is_list(Msgs) -> {ok, Msgs};
        {ok, undefined} -> {error, noproc};
        {ok, Other} -> {error, {unexpected, Other}};
        {error, _} = Error -> Error
    end.

-spec stop_source(map()) -> ok | {error, term()}.
stop_source(#{guild_id := GuildId, src_shard := Shard, opts := Opts}) ->
    Request = {stop_guild, GuildId, {shutdown, handoff}},
    case safe_call(Shard, Request, maps:get(stop_timeout, Opts)) of
        ok -> ok;
        Other -> {error, Other}
    end.

-spec source_alive(pid(), opts()) -> boolean() | {error, term()}.
source_alive(SrcPid, Opts) ->
    Args = [SrcPid],
    case rpc_call(node(SrcPid), erlang, is_process_alive, Args, maps:get(rpc_timeout, Opts)) of
        {ok, Alive} when is_boolean(Alive) -> Alive;
        {ok, Other} -> {error, {unexpected, Other}};
        {error, _} = Error -> Error
    end.

-spec within_budget(map()) -> boolean().
within_budget(#{frozen_at := FrozenAt, opts := Opts}) ->
    now_ms() - FrozenAt =< maps:get(freeze_budget, Opts).

-spec indexed_shard_pid(node(), non_neg_integer()) -> {ok, pid()} | {error, term()}.
indexed_shard_pid(Node, Index) ->
    case rpc_call(Node, ets, lookup, [?SHARD_TABLE, {shard_pid, Index}], 10000) of
        {ok, [{{shard_pid, Index}, Pid}]} when is_pid(Pid) -> {ok, Pid};
        {ok, _} -> {error, shard_unavailable};
        {error, _} = Error -> Error
    end.

-spec safe_hook(hook()) -> ok | {error, term()}.
safe_hook(Hook) ->
    try Hook() of
        ok -> ok;
        {error, _} = Error -> Error
    catch
        Class:Reason -> {error, {hook_crashed, Class, Reason}}
    end.

-spec safe_started_hook(started_hook(), pid()) -> ok | {error, term()}.
safe_started_hook(Hook, NewPid) ->
    try Hook(NewPid) of
        ok -> ok;
        {error, _} = Error -> Error
    catch
        Class:Reason -> {error, {hook_crashed, Class, Reason}}
    end.

-spec safe_call(pid(), term(), pos_integer()) -> term().
safe_call(Server, Request, Timeout) ->
    try
        gen_server:call(Server, Request, Timeout)
    catch
        exit:{timeout, _} -> {error, timeout};
        exit:Reason -> {error, {exit, Reason}}
    end.

-spec rpc_call(node(), module(), atom(), [term()], pos_integer()) ->
    {ok, term()} | {error, term()}.
rpc_call(Node, Module, Function, Args, Timeout) ->
    try erpc:call(Node, Module, Function, Args, Timeout) of
        Result -> {ok, Result}
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

-spec parse(string()) -> [erl_parse:abstract_expr()].
parse(Source) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    Exprs.

-spec bindings([{atom(), term()}]) -> erl_eval:binding_struct().
bindings(Pairs) ->
    lists:foldl(
        fun({Name, Value}, Acc) -> erl_eval:add_binding(Name, Value, Acc) end,
        erl_eval:new_bindings(),
        Pairs
    ).

-spec maybe_limit_heap(non_neg_integer()) -> ok.
maybe_limit_heap(0) ->
    ok;
maybe_limit_heap(Words) ->
    _ = process_flag(max_heap_size, #{size => Words, kill => true, error_logger => true}),
    ok.

-spec put_ms(atom(), integer(), map()) -> map().
put_ms(Key, T0, Ctx) ->
    put_report(Key, now_ms() - T0, Ctx).

-spec put_report(atom(), term(), map()) -> map().
put_report(Key, Value, #{report := Report} = Ctx) ->
    Ctx#{report => Report#{Key => Value}}.

-spec now_ms() -> integer().
now_ms() ->
    erlang:monotonic_time(millisecond).
