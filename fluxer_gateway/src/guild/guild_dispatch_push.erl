%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_dispatch_push).
-typing([eqwalizer]).

-export([maybe_send_push_notifications/4]).

-define(CONCURRENCY_LIMIT_KEY, guild_push_concurrency_limit).
-define(DEFAULT_PUSH_CONCURRENCY, 2).
-define(MAX_PUSH_CONCURRENCY, 8).
-define(QUEUE_LIMIT_KEY, guild_push_queue_limit).
-define(DEFAULT_PUSH_QUEUE_LIMIT, 64).
-define(MAX_PUSH_QUEUE_LIMIT, 1024).
-define(PUSH_WORKER_MAX_AGE_MS, 60000).
-type event_data() :: map().
-type guild_state() :: map().
-type guild_id() :: integer().
-type push_worker() :: {integer(), pid(), integer()}.
-type push_gate() :: push_worker() | undefined.
-spec maybe_send_push_notifications(atom(), map(), integer(), map()) -> ok.
maybe_send_push_notifications(message_create, FinalData, GuildId, UpdatedState) ->
    case
        maps:get(disable_push_notifications, UpdatedState, false) orelse
            push_message_params:suppresses_notifications(FinalData)
    of
        true -> ok;
        false -> maybe_spawn_push(FinalData, GuildId, UpdatedState)
    end;
maybe_send_push_notifications(_Event, _FinalData, _GuildId, _UpdatedState) ->
    ok.

-spec maybe_spawn_push(event_data(), guild_id(), guild_state()) -> ok.
maybe_spawn_push(FinalData, GuildId, UpdatedState) ->
    Limit = push_concurrency_limit(),
    QueueLimit = push_queue_limit(),
    Workers = live_push_workers(),
    put(push_inflight_workers, Workers),
    case push_admission(length(Workers), Limit, QueueLimit) of
        run ->
            spawn_push(FinalData, GuildId, UpdatedState, undefined);
        queue ->
            guild_push_metrics:count(queued),
            spawn_push(FinalData, GuildId, UpdatedState, lists:nth(Limit, Workers));
        overflow ->
            push_queue_overflow(GuildId, Limit, QueueLimit)
    end.

-spec push_admission(non_neg_integer(), pos_integer(), non_neg_integer()) ->
    run | queue | overflow.
push_admission(Tracked, Limit, _QueueLimit) when Tracked < Limit -> run;
push_admission(Tracked, Limit, QueueLimit) when Tracked - Limit < QueueLimit -> queue;
push_admission(_Tracked, _Limit, _QueueLimit) -> overflow.

-spec push_queue_overflow(guild_id(), pos_integer(), non_neg_integer()) -> ok.
push_queue_overflow(GuildId, Limit, QueueLimit) ->
    guild_push_metrics:count(dropped_at_limit),
    logger:warning(
        "guild_push_dropped_at_limit: guild_id=~p concurrency=~p queue_limit=~p",
        [GuildId, Limit, QueueLimit]
    ).

-spec push_queue_limit() -> non_neg_integer().
push_queue_limit() ->
    case application:get_env(fluxer_gateway, ?QUEUE_LIMIT_KEY, undefined) of
        Value when is_integer(Value), Value >= 0 -> min(Value, ?MAX_PUSH_QUEUE_LIMIT);
        _ -> ?DEFAULT_PUSH_QUEUE_LIMIT
    end.

-spec local_process_alive(pid()) -> boolean().
local_process_alive(Pid) ->
    node(Pid) =:= node() andalso erlang:is_process_alive(Pid).

-spec push_concurrency_limit() -> pos_integer().
push_concurrency_limit() ->
    case application:get_env(fluxer_gateway, ?CONCURRENCY_LIMIT_KEY, undefined) of
        Value when is_integer(Value), Value > 0 -> min(Value, ?MAX_PUSH_CONCURRENCY);
        _ -> ?DEFAULT_PUSH_CONCURRENCY
    end.

-spec live_push_workers() -> [push_worker()].
live_push_workers() ->
    Now = gateway_clock:monotonic_ms(),
    prune_push_workers(tracked_push_workers(Now), Now, #{}, []).

-spec tracked_push_workers(integer()) -> [term()].
tracked_push_workers(Now) ->
    merge_inflight_push_worker(get(push_inflight), tracked_worker_list(), Now).

-spec tracked_worker_list() -> [term()].
tracked_worker_list() ->
    case get(push_inflight_workers) of
        Workers when is_list(Workers) -> Workers;
        _ -> []
    end.

-spec merge_inflight_push_worker(term(), [term()], integer()) -> [term()].
merge_inflight_push_worker(Pid, Workers, Now) when is_pid(Pid) ->
    case has_push_worker(Pid, Workers) of
        true -> Workers;
        false -> Workers ++ [{0, Pid, Now}]
    end;
merge_inflight_push_worker(_Inflight, Workers, _Now) ->
    Workers.

-spec has_push_worker(pid(), [term()]) -> boolean().
has_push_worker(_Pid, []) -> false;
has_push_worker(Pid, [{_Gen, Pid, _StartedMs} | _Rest]) -> true;
has_push_worker(Pid, [_Worker | Rest]) -> has_push_worker(Pid, Rest).

-spec prune_push_workers([term()], integer(), map(), [push_worker()]) -> [push_worker()].
prune_push_workers([], _Now, _Seen, Acc) ->
    lists:reverse(Acc);
prune_push_workers([Worker | Rest], Now, Seen, Acc) ->
    case keep_push_worker(Worker, Now, Seen) of
        {keep, {_Gen, Pid, _StartedMs} = Kept} ->
            prune_push_workers(Rest, Now, Seen#{Pid => true}, [Kept | Acc]);
        drop ->
            prune_push_workers(Rest, Now, Seen, Acc)
    end.

-spec keep_push_worker(term(), integer(), map()) -> {keep, push_worker()} | drop.
keep_push_worker({Gen, Pid, StartedMs} = Worker, Now, Seen) when
    is_integer(Gen), is_pid(Pid), is_integer(StartedMs)
->
    case maps:is_key(Pid, Seen) of
        true -> drop_duplicate_push_worker();
        false -> keep_live_push_worker(Worker, Now - StartedMs)
    end;
keep_push_worker(_Worker, _Now, _Seen) ->
    drop.

-spec drop_duplicate_push_worker() -> drop.
drop_duplicate_push_worker() ->
    guild_push_metrics:count(slot_deduped),
    drop.

-spec keep_live_push_worker(push_worker(), integer()) -> {keep, push_worker()} | drop.
keep_live_push_worker({_Gen, Pid, _StartedMs} = Worker, AgeMs) ->
    case local_process_alive(Pid) of
        false ->
            drop;
        true when AgeMs < ?PUSH_WORKER_MAX_AGE_MS -> {keep, Worker};
        true ->
            guild_push_metrics:count(slot_reclaimed),
            drop
    end.

-spec note_push_worker(pid()) -> ok.
note_push_worker(Pid) ->
    Gen = erlang:unique_integer([monotonic, positive]),
    StartedMs = gateway_clock:monotonic_ms(),
    put(push_inflight_workers, [{Gen, Pid, StartedMs} | tracked_worker_list()]),
    ok.

-spec spawn_push(event_data(), guild_id(), guild_state(), push_gate()) -> ok.
spawn_push(FinalData, GuildId, UpdatedState, Gate) ->
    Data = maps:get(data, UpdatedState, #{}),
    case maps:get(members_ets, Data, undefined) of
        MembersTab when is_reference(MembersTab) ->
            CompactState = compact_push_state(
                eqwalizer:dynamic_cast(MembersTab), Data, GuildId, UpdatedState
            ),
            spawn_compact_push(FinalData, GuildId, CompactState, Gate);
        _ ->
            missing_members_table(FinalData, GuildId, UpdatedState, Gate)
    end.

-spec compact_push_state(ets:tid(), map(), guild_id(), guild_state()) -> guild_state().
compact_push_state(MembersTab, Data, GuildId, UpdatedState) ->
    Sessions = maps:get(sessions, UpdatedState, #{}),
    {SessionEligibility, Hold} = guild_push_engagement:snapshot(Sessions, UpdatedState),
    #{
        id => maps:get(id, UpdatedState, GuildId),
        data => compact_push_data(Data),
        virtual_channel_access => maps:get(virtual_channel_access, UpdatedState, #{}),
        members_ets => MembersTab,
        member_presence => maps:get(member_presence, UpdatedState, undefined),
        guild_pid => guild_push_engagement:guild_pid(),
        session_eligibility => SessionEligibility,
        grace_hold => Hold,
        member_count => maps:get(member_count, UpdatedState, undefined)
    }.

-spec compact_push_data(map()) -> map().
compact_push_data(Data) ->
    maps:with(
        [
            <<"guild">>,
            <<"roles">>,
            <<"role_index">>,
            role_perms_cache,
            <<"channels">>,
            <<"channel_index">>,
            overwrite_perms_cache,
            members_ets,
            thread_gate,
            thread_store
        ],
        Data
    ).

-spec spawn_compact_push(event_data(), guild_id(), guild_state(), push_gate()) -> ok.
spawn_compact_push(FinalData, GuildId, CompactState, Gate) ->
    spawn_push_worker(
        fun() ->
            guild_push_delivery:compact(FinalData, GuildId, CompactState)
        end,
        GuildId,
        Gate
    ).

-spec missing_members_table(event_data(), guild_id(), guild_state(), push_gate()) -> ok.
missing_members_table(FinalData, GuildId, UpdatedState, Gate) ->
    guild_push_metrics:count(members_table_missing),
    logger:warning(
        "guild_push_members_table_unavailable: guild_id=~p phase=spawn",
        [GuildId]
    ),
    spawn_legacy_push(FinalData, GuildId, UpdatedState, Gate).

-spec spawn_legacy_push(event_data(), guild_id(), guild_state(), push_gate()) -> ok.
spawn_legacy_push(FinalData, GuildId, UpdatedState, Gate) ->
    LegacyState = legacy_push_state(GuildId, UpdatedState),
    spawn_push_worker(
        fun() ->
            guild_push_delivery:legacy(FinalData, GuildId, LegacyState)
        end,
        GuildId,
        Gate
    ).

-spec legacy_push_state(guild_id(), guild_state()) -> guild_state().
legacy_push_state(GuildId, UpdatedState) ->
    #{
        id => maps:get(id, UpdatedState, GuildId),
        data => maps:get(data, UpdatedState, #{}),
        sessions => maps:get(sessions, UpdatedState, #{}),
        member_presence => maps:get(member_presence, UpdatedState, undefined),
        guild_pid => guild_push_engagement:guild_pid(),
        virtual_channel_access => maps:get(virtual_channel_access, UpdatedState, #{}),
        member_count => maps:get(member_count, UpdatedState, undefined)
    }.

-spec spawn_push_worker(fun(() -> ok), guild_id(), push_gate()) -> ok.
spawn_push_worker(Worker, GuildId, Gate) ->
    Counted = fun() ->
        ok = wait_for_push_slot(Gate),
        run_counted_push_worker(Worker)
    end,
    case try_spawn_push_worker(Counted, GuildId) of
        {ok, Pid} ->
            put(push_inflight, Pid),
            note_push_worker(Pid),
            guild_push_metrics:count(worker_started);
        error ->
            guild_push_metrics:count(spawn_failed)
    end.

-spec wait_for_push_slot(push_gate()) -> ok.
wait_for_push_slot(undefined) ->
    ok;
wait_for_push_slot({_Gen, Pid, _StartedMs}) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _Reason} -> ok
    after ?PUSH_WORKER_MAX_AGE_MS ->
        erlang:demonitor(Ref, [flush]),
        ok
    end.

-spec run_counted_push_worker(fun(() -> ok)) -> ok.
run_counted_push_worker(Worker) ->
    try Worker() of
        _ -> guild_push_metrics:count(worker_completed)
    catch
        _Class:_Reason -> guild_push_metrics:count(worker_failed)
    end.

-spec try_spawn_push_worker(fun(() -> ok), guild_id()) -> {ok, pid()} | error.
try_spawn_push_worker(Worker, GuildId) ->
    try
        {ok, spawn(fun() -> run_deprioritised_push_worker(Worker) end)}
    catch
        error:system_limit ->
            logger:warning(
                "guild_push_worker_spawn_failed: guild_id=~p reason=system_limit",
                [GuildId]
            ),
            error
    end.

-spec run_deprioritised_push_worker(fun(() -> ok)) -> ok.
run_deprioritised_push_worker(Worker) ->
    _ = erlang:process_flag(priority, low),
    Worker().
