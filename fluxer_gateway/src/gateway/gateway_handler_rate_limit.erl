%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_handler_rate_limit).
-typing([eqwalizer]).

-export([
    check_rate_limit/2,
    check_shared_ip_rate/1,
    check_shared_user_rate/1,
    rate_limits_disabled/0
]).

-export_type([state/0]).

-define(GATEWAY_RATE_LIMIT_WINDOW_MS, 60000).
-define(GATEWAY_RATE_LIMIT_MAX_EVENTS, 600).
-define(PRESENCE_RATE_LIMIT_WINDOW_MS, 20000).
-define(PRESENCE_RATE_LIMIT_MAX_EVENTS, 5).

-define(SHARED_IP_RATE_TABLE, gateway_shared_ip_rate).
-define(SHARED_USER_RATE_TABLE, gateway_shared_user_rate).

-define(SHARED_IP_RATE_WINDOW_MS, 60000).
-define(SHARED_IP_RATE_MAX_EVENTS, 6000).
-define(SHARED_USER_RATE_WINDOW_MS, 60000).
-define(SHARED_USER_RATE_MAX_EVENTS, 600).
-define(SHARED_RATE_CLEANUP_INTERVAL_MS, ?SHARED_IP_RATE_WINDOW_MS * 2).

-type state() :: gateway_handler:state().

-ifdef(TEST).
-export([prune_old_window_entries/2]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec check_rate_limit(state(), atom()) ->
    {ok, state()} | {rate_limited, state()} | {opcode_rate_limited, state()}.
check_rate_limit(State, Op) ->
    case rate_limits_disabled() of
        true -> {ok, State};
        false -> check_shared_then_connection(State, Op)
    end.

-spec check_shared_then_connection(state(), atom()) ->
    {ok, state()} | {rate_limited, state()} | {opcode_rate_limited, state()}.
check_shared_then_connection(State, Op) ->
    case check_shared_budgets(State) of
        rate_limited ->
            {rate_limited, State};
        ok ->
            check_rate_limit_limited(State, Op)
    end.

-spec check_shared_budgets(state()) -> ok | rate_limited.
check_shared_budgets(State) ->
    case check_shared_ip_rate(maps:get(peer_ip, State, undefined)) of
        {error, ip_rate_limited} ->
            rate_limited;
        ok ->
            check_shared_user_budget(maps:get(session_pid, State, undefined))
    end.

-spec check_shared_user_budget(term()) -> ok | rate_limited.
check_shared_user_budget(SessionPid) when is_pid(SessionPid) ->
    case check_shared_user_rate(SessionPid) of
        {error, user_rate_limited} -> rate_limited;
        ok -> ok
    end;
check_shared_user_budget(_) ->
    ok.

-spec check_rate_limit_limited(state(), atom()) ->
    {ok, state()} | {rate_limited, state()} | {opcode_rate_limited, state()}.
check_rate_limit_limited(#{rate_limit_state := RateLimitState} = State, Op) ->
    Now = erlang:system_time(millisecond),
    Events = maps:get(events, RateLimitState, []),
    case
        check_timestamp_window(
            Events,
            Now,
            ?GATEWAY_RATE_LIMIT_WINDOW_MS,
            ?GATEWAY_RATE_LIMIT_MAX_EVENTS
        )
    of
        rate_limited ->
            {rate_limited, State};
        {ok, NewEvents} ->
            NewRLS = RateLimitState#{events => NewEvents},
            check_opcode_rate_limit(State#{rate_limit_state => NewRLS}, Op, Now)
    end.

-spec check_opcode_rate_limit(state(), atom(), integer()) ->
    {ok, state()} | {opcode_rate_limited, state()}.
check_opcode_rate_limit(State, presence_update, Now) ->
    check_named_opcode_rate_limit(
        State,
        presence_update,
        Now,
        ?PRESENCE_RATE_LIMIT_WINDOW_MS,
        ?PRESENCE_RATE_LIMIT_MAX_EVENTS
    );
check_opcode_rate_limit(State, _Op, _Now) ->
    {ok, State}.

-spec check_named_opcode_rate_limit(state(), atom(), integer(), pos_integer(), pos_integer()) ->
    {ok, state()} | {opcode_rate_limited, state()}.
check_named_opcode_rate_limit(
    #{rate_limit_state := RateLimitState} = State,
    Op,
    Now,
    WindowMs,
    MaxEvents
) ->
    OpEvents = maps:get(op_events, RateLimitState, #{}),
    Events = maps:get(Op, OpEvents, []),
    case check_timestamp_window(Events, Now, WindowMs, MaxEvents) of
        rate_limited ->
            {opcode_rate_limited, State};
        {ok, NewEvents} ->
            NewOpEvents = OpEvents#{Op => NewEvents},
            {ok, State#{rate_limit_state => RateLimitState#{op_events => NewOpEvents}}}
    end.

-spec check_timestamp_window([integer()], integer(), pos_integer(), pos_integer()) ->
    {ok, [integer()]} | rate_limited.
check_timestamp_window(Events, Now, WindowMs, MaxEvents) ->
    EventsInWindow = [T || T <- Events, (Now - T) < WindowMs],
    case length(EventsInWindow) >= MaxEvents of
        true -> rate_limited;
        false -> {ok, [Now | EventsInWindow]}
    end.

-spec check_shared_ip_rate(term()) -> ok | {error, ip_rate_limited}.
check_shared_ip_rate(PeerIP) when is_binary(PeerIP), PeerIP =/= <<"unknown">> ->
    case rate_limits_disabled() of
        true ->
            ok;
        false ->
            ensure_window_table(?SHARED_IP_RATE_TABLE, ?SHARED_IP_RATE_WINDOW_MS),
            check_shared_ip_window(PeerIP)
    end;
check_shared_ip_rate(_) ->
    ok.

-spec check_shared_ip_window(binary()) -> ok | {error, ip_rate_limited}.
check_shared_ip_window(PeerIP) ->
    case
        check_shared_window(
            ?SHARED_IP_RATE_TABLE,
            PeerIP,
            ?SHARED_IP_RATE_WINDOW_MS,
            ?SHARED_IP_RATE_MAX_EVENTS,
            ip_rate_limited
        )
    of
        ok -> ok;
        {error, ip_rate_limited} -> {error, ip_rate_limited}
    end.

-spec check_shared_user_rate(term()) -> ok | {error, user_rate_limited}.
check_shared_user_rate(UserKey) when UserKey =/= undefined ->
    case rate_limits_disabled() of
        true ->
            ok;
        false ->
            ensure_window_table(?SHARED_USER_RATE_TABLE, ?SHARED_USER_RATE_WINDOW_MS),
            check_shared_user_window(UserKey)
    end;
check_shared_user_rate(_) ->
    ok.

-spec check_shared_user_window(term()) -> ok | {error, user_rate_limited}.
check_shared_user_window(UserKey) ->
    case
        check_shared_window(
            ?SHARED_USER_RATE_TABLE,
            UserKey,
            ?SHARED_USER_RATE_WINDOW_MS,
            ?SHARED_USER_RATE_MAX_EVENTS,
            user_rate_limited
        )
    of
        ok -> ok;
        {error, user_rate_limited} -> {error, user_rate_limited}
    end.

-spec check_shared_window(atom(), term(), pos_integer(), pos_integer(), atom()) ->
    ok | {error, atom()}.
check_shared_window(Table, Key, WindowMs, MaxEvents, LimitReason) ->
    Now = erlang:system_time(millisecond),
    Bucket = Now div WindowMs,
    BucketKey = {Key, Bucket},
    try ets:update_counter(Table, BucketKey, {2, 1}, {BucketKey, 0}) of
        Count when Count > MaxEvents -> {error, LimitReason};
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec ensure_window_table(atom(), pos_integer()) -> ok.
ensure_window_table(Table, WindowMs) ->
    case ets:whereis(Table) of
        undefined -> create_window_table(Table, WindowMs);
        _ -> ok
    end.

-spec create_window_table(atom(), pos_integer()) -> ok.
create_window_table(Table, WindowMs) ->
    case create_table(Table) of
        created -> schedule_window_cleanup(Table, WindowMs);
        exists -> ok
    end.

-spec schedule_window_cleanup(atom(), pos_integer()) -> ok.
schedule_window_cleanup(Table, WindowMs) ->
    case ets:whereis(Table) of
        undefined -> ok;
        Tid -> spawn_window_cleanup(Tid, WindowMs)
    end.

-spec spawn_window_cleanup(ets:table(), pos_integer()) -> ok.
spawn_window_cleanup(Tid, WindowMs) ->
    _ = spawn(fun() -> window_cleanup_loop(Tid, WindowMs) end),
    ok.

-spec window_cleanup_loop(ets:table(), pos_integer()) -> ok.
window_cleanup_loop(Table, WindowMs) ->
    ok = gateway_retry_timer:wait(?SHARED_RATE_CLEANUP_INTERVAL_MS),
    case prune_old_window_entries(Table, WindowMs) of
        ok -> window_cleanup_loop(Table, WindowMs);
        gone -> ok
    end.

-spec prune_old_window_entries(ets:table(), pos_integer()) -> ok | gone.
prune_old_window_entries(Table, WindowMs) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now div WindowMs - 1,
    try
        _ = ets:select_delete(Table, [
            {{{'$1', '$2'}, '_'}, [{'<', '$2', Cutoff}], [true]}
        ]),
        ok
    catch
        error:badarg -> gone
    end.

-spec create_table(atom()) -> created | exists.
create_table(Table) ->
    try
        _ = ets:new(Table, rate_table_options()),
        created
    catch
        error:badarg -> exists
    end.

-spec rate_table_options() -> list().
rate_table_options() ->
    [named_table, public, set, {write_concurrency, true}, {read_concurrency, true}] ++
        guild_ets_utils:heir_options().

-spec rate_limits_disabled() -> boolean().
rate_limits_disabled() ->
    case os:getenv("FLUXER_DISABLE_RATE_LIMITS") of
        "1" -> true;
        "true" -> true;
        "TRUE" -> true;
        _ -> false
    end.

-ifdef(TEST).

check_rate_limit_disabled_by_env_test() ->
    Now = erlang:system_time(millisecond),
    State = #{
        rate_limit_state => #{
            events => lists:duplicate(130, Now),
            op_events => #{presence_update => lists:duplicate(10, Now)}
        }
    },
    OldValue = os:getenv("FLUXER_DISABLE_RATE_LIMITS"),
    os:putenv("FLUXER_DISABLE_RATE_LIMITS", "true"),
    try
        CastState = eqwalizer:dynamic_cast(State),
        ?assertEqual({ok, CastState}, check_rate_limit(CastState, presence_update))
    after
        restore_env("FLUXER_DISABLE_RATE_LIMITS", OldValue)
    end.

restore_env(Key, false) ->
    os:unsetenv(Key);
restore_env(Key, Value) ->
    os:putenv(Key, Value).

with_rate_limits_enabled(Fun) ->
    OldValue = os:getenv("FLUXER_DISABLE_RATE_LIMITS"),
    os:unsetenv("FLUXER_DISABLE_RATE_LIMITS"),
    try
        Fun()
    after
        restore_env("FLUXER_DISABLE_RATE_LIMITS", OldValue)
    end.

shared_ip_rate_blocks_over_limit_test() ->
    with_rate_limits_enabled(fun() ->
        IP = <<"198.51.100.10">>,
        reset_shared_ip(IP),
        assert_shared_ip_rate_allows_limit(IP),
        ?assertEqual({error, ip_rate_limited}, check_shared_ip_rate(IP)),
        reset_shared_ip(IP)
    end).

shared_ip_rate_ignores_unknown_ip_test() ->
    with_rate_limits_enabled(fun() ->
        ?assertEqual(ok, check_shared_ip_rate(<<"unknown">>)),
        ?assertEqual(ok, check_shared_ip_rate(undefined))
    end).

assert_shared_ip_rate_allows_limit(IP) ->
    lists:foreach(
        fun(_) -> ?assertEqual(ok, check_shared_ip_rate(IP)) end,
        lists:seq(1, ?SHARED_IP_RATE_MAX_EVENTS)
    ).

reset_shared_ip(IP) ->
    case ets:whereis(?SHARED_IP_RATE_TABLE) of
        undefined ->
            ok;
        _ ->
            Now = erlang:system_time(millisecond),
            Bucket = Now div ?SHARED_IP_RATE_WINDOW_MS,
            ets:delete(?SHARED_IP_RATE_TABLE, {IP, Bucket}),
            ok
    end.

prune_old_window_entries_removes_old_buckets_test() ->
    with_rate_limits_enabled(fun() ->
        ok = check_shared_ip_rate(<<"198.51.100.50">>),
        CurrentBucket = erlang:system_time(millisecond) div ?SHARED_IP_RATE_WINDOW_MS,
        OldKey = {<<"198.51.100.51">>, CurrentBucket - 5},
        CurrentKey = {<<"198.51.100.52">>, CurrentBucket},
        ets:insert(?SHARED_IP_RATE_TABLE, [{OldKey, 3}, {CurrentKey, 7}]),
        prune_old_window_entries(?SHARED_IP_RATE_TABLE, ?SHARED_IP_RATE_WINDOW_MS),
        ?assertEqual([], ets:lookup(?SHARED_IP_RATE_TABLE, OldKey)),
        ?assertEqual([{CurrentKey, 7}], ets:lookup(?SHARED_IP_RATE_TABLE, CurrentKey)),
        ets:delete(?SHARED_IP_RATE_TABLE, CurrentKey)
    end).

prune_old_window_entries_keeps_recent_buckets_test() ->
    with_rate_limits_enabled(fun() ->
        ok = check_shared_user_rate(<<"user-900100">>),
        CurrentBucket = erlang:system_time(millisecond) div ?SHARED_USER_RATE_WINDOW_MS,
        RecentKey = {<<"user-900101">>, CurrentBucket - 1},
        ets:insert(?SHARED_USER_RATE_TABLE, {RecentKey, 5}),
        prune_old_window_entries(?SHARED_USER_RATE_TABLE, ?SHARED_USER_RATE_WINDOW_MS),
        ?assertNotEqual([], ets:lookup(?SHARED_USER_RATE_TABLE, RecentKey)),
        ets:delete(?SHARED_USER_RATE_TABLE, RecentKey)
    end).

shared_ip_rate_table_sweeps_stale_buckets_test() ->
    with_rate_limits_enabled(fun() -> with_fast_cleanup_timer(fun sweep_stale_ip_buckets/0) end).

with_fast_cleanup_timer(Fun) ->
    meck:new(gateway_retry_timer, [passthrough]),
    meck:expect(gateway_retry_timer, wait, fun(_) -> timer:sleep(5) end),
    try
        Fun()
    after
        meck:unload(gateway_retry_timer)
    end.

sweep_stale_ip_buckets() ->
    drop_table(?SHARED_IP_RATE_TABLE),
    IP = <<"198.51.100.40">>,
    ?assertEqual(ok, check_shared_ip_rate(IP)),
    Bucket = erlang:system_time(millisecond) div ?SHARED_IP_RATE_WINDOW_MS,
    fill_stale_buckets(?SHARED_IP_RATE_TABLE, IP, Bucket, 200),
    ?assert(await_table_size(?SHARED_IP_RATE_TABLE, 2, 150)).

drop_table(Table) ->
    case ets:whereis(Table) of
        undefined ->
            ok;
        _ ->
            ets:delete(Table),
            ok
    end.

fill_stale_buckets(Table, Key, Bucket, Count) ->
    lists:foreach(
        fun(N) -> ets:insert(Table, {{Key, Bucket - N - 1}, 1}) end,
        lists:seq(1, Count)
    ).

await_table_size(Table, Max, 0) ->
    ets:info(Table, size) =< Max;
await_table_size(Table, Max, Attempts) ->
    case ets:info(Table, size) =< Max of
        true ->
            true;
        false ->
            timer:sleep(10),
            await_table_size(Table, Max, Attempts - 1)
    end.

shared_ip_bucket_survives_creating_process_death_test() ->
    with_rate_limits_enabled(fun assert_shared_ip_bucket_survives_creator/0).

assert_shared_ip_bucket_survives_creator() ->
    drop_table(?SHARED_IP_RATE_TABLE),
    {ok, _} = guild_ets_owner:start_link(),
    try
        assert_shared_ip_bucket_outlives_creator()
    after
        gen_server:stop(guild_ets_owner)
    end.

assert_shared_ip_bucket_outlives_creator() ->
    IP = <<"198.51.100.70">>,
    stop_table_owner(start_shared_ip_bucket_creator(IP)),
    ?assertNotEqual(undefined, ets:whereis(?SHARED_IP_RATE_TABLE)),
    Key = {IP, erlang:system_time(millisecond) div ?SHARED_IP_RATE_WINDOW_MS},
    ?assertEqual([{Key, 1}], ets:lookup(?SHARED_IP_RATE_TABLE, Key)).

start_shared_ip_bucket_creator(IP) ->
    Parent = self(),
    Pid = spawn(fun() -> create_shared_ip_bucket(Parent, IP) end),
    receive
        {owner_ready, Pid} -> Pid
    after 1000 -> error(owner_start_timeout)
    end.

create_shared_ip_bucket(Parent, IP) ->
    ok = check_shared_ip_rate(IP),
    Parent ! {owner_ready, self()},
    receive
        stop -> ok
    after 30000 -> ok
    end.

window_cleanup_loops_do_not_outlive_their_table_test() ->
    with_rate_limits_enabled(fun() ->
        with_fast_cleanup_timer(fun assert_window_cleanup_loops_do_not_leak/0)
    end).

assert_window_cleanup_loops_do_not_leak() ->
    drop_table(?SHARED_IP_RATE_TABLE),
    Before = count_window_cleanup_loops(),
    lists:foreach(fun(_) -> churn_shared_ip_table_owner() end, lists:seq(1, 5)),
    Owner = start_shared_ip_table_owner(),
    try
        ?assert(await_window_cleanup_loops(Before + 1, 200))
    after
        stop_table_owner(Owner)
    end.

churn_shared_ip_table_owner() ->
    stop_table_owner(start_shared_ip_table_owner()).

start_shared_ip_table_owner() ->
    Parent = self(),
    Pid = spawn(fun() -> own_shared_ip_table(Parent) end),
    receive
        {owner_ready, Pid} -> Pid
    after 1000 -> error(owner_start_timeout)
    end.

own_shared_ip_table(Parent) ->
    ok = check_shared_ip_rate(<<"198.51.100.60">>),
    Parent ! {owner_ready, self()},
    receive
        stop -> ok
    after 30000 -> ok
    end.

stop_table_owner(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! stop,
    receive
        {'DOWN', Ref, process, Pid, _Reason} -> ok
    after 1000 -> error(owner_stop_timeout)
    end.

count_window_cleanup_loops() ->
    length([Pid || Pid <- erlang:processes(), is_window_cleanup_loop(Pid)]).

is_window_cleanup_loop(Pid) ->
    case erlang:process_info(Pid, current_stacktrace) of
        {current_stacktrace, Stack} ->
            lists:any(fun is_window_cleanup_frame/1, Stack);
        _ ->
            false
    end.

is_window_cleanup_frame({?MODULE, window_cleanup_loop, _Arity, _Location}) ->
    true;
is_window_cleanup_frame(_Frame) ->
    false.

await_window_cleanup_loops(Max, 0) ->
    count_window_cleanup_loops() =< Max;
await_window_cleanup_loops(Max, Attempts) ->
    case count_window_cleanup_loops() =< Max of
        true ->
            true;
        false ->
            timer:sleep(10),
            await_window_cleanup_loops(Max, Attempts - 1)
    end.

-endif.
