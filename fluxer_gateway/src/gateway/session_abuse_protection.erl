%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_abuse_protection).
-typing([eqwalizer]).

-export([
    ensure_tables/0,
    check_user_session_limit/1,
    increment_user_sessions/1,
    decrement_user_sessions/1,
    check_identify_rate/1
]).

-ifdef(TEST).
-export([prune_old_identify_entries/1]).
-endif.

-define(IDENTIFY_TABLE, gateway_identify_rate).
-define(SESSION_USER_COUNTS, session_user_counts).
-define(IDENTIFY_MAX_PER_IP, 300).
-define(IDENTIFY_WINDOW_SECS, 60).
-define(IDENTIFY_CLEANUP_INTERVAL_MS, ?IDENTIFY_WINDOW_SECS * 2 * 1000).
-define(MAX_SESSIONS_PER_USER, 100).

-spec ensure_tables() -> ok.
ensure_tables() ->
    ensure_identify_table(),
    ensure_session_user_counts_table(),
    ok.

-spec check_user_session_limit(term()) -> ok | {error, too_many_sessions}.
check_user_session_limit(UserId) when is_integer(UserId), UserId > 0 ->
    case gateway_handler_rate_limit:rate_limits_disabled() of
        true ->
            ok;
        false ->
            ok = ensure_session_user_counts_table(),
            do_check_user_session_limit(UserId)
    end;
check_user_session_limit(_) ->
    ok.

-spec do_check_user_session_limit(integer()) -> ok | {error, too_many_sessions}.
do_check_user_session_limit(UserId) ->
    try ets:lookup(?SESSION_USER_COUNTS, UserId) of
        [{_, Count}] when Count >= ?MAX_SESSIONS_PER_USER ->
            {error, too_many_sessions};
        _ ->
            ok
    catch
        error:badarg ->
            ok
    end.

-spec increment_user_sessions(term()) -> ok.
increment_user_sessions(UserId) when is_integer(UserId), UserId > 0 ->
    ok = ensure_session_user_counts_table(),
    try
        _ = ets:update_counter(?SESSION_USER_COUNTS, UserId, {2, 1}, {UserId, 0}),
        ok
    catch
        error:badarg -> ok
    end;
increment_user_sessions(_) ->
    ok.

-spec decrement_user_sessions(term()) -> ok.
decrement_user_sessions(UserId) when is_integer(UserId), UserId > 0 ->
    try
        case ets:update_counter(?SESSION_USER_COUNTS, UserId, {2, -1, 0, 0}) of
            0 ->
                ets:delete(?SESSION_USER_COUNTS, UserId),
                ok;
            _ ->
                ok
        end
    catch
        error:badarg -> ok
    end;
decrement_user_sessions(_) ->
    ok.

-spec check_identify_rate(term()) -> ok | {error, identify_rate_limited}.
check_identify_rate(PeerIP) when is_binary(PeerIP) ->
    case gateway_handler_rate_limit:rate_limits_disabled() of
        true ->
            ok;
        false ->
            ok = ensure_identify_table(),
            do_check_identify_rate(PeerIP)
    end;
check_identify_rate(_) ->
    ok.

-spec do_check_identify_rate(binary()) -> ok | {error, identify_rate_limited}.
do_check_identify_rate(PeerIP) ->
    Now = erlang:system_time(second),
    Bucket = Now div ?IDENTIFY_WINDOW_SECS,
    Key = {PeerIP, Bucket},
    try ets:update_counter(?IDENTIFY_TABLE, Key, {2, 1}, {Key, 0}) of
        Count when Count > ?IDENTIFY_MAX_PER_IP ->
            {error, identify_rate_limited};
        _ ->
            ok
    catch
        error:badarg ->
            ok
    end.

-spec ensure_session_user_counts_table() -> ok.
ensure_session_user_counts_table() ->
    case ets:whereis(?SESSION_USER_COUNTS) of
        undefined -> create_session_user_counts_table();
        _ -> ok
    end.

-spec create_session_user_counts_table() -> ok.
create_session_user_counts_table() ->
    _ = create_rate_table(?SESSION_USER_COUNTS),
    ok.

-spec create_rate_table(atom()) -> created | exists.
create_rate_table(Table) ->
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

-spec ensure_identify_table() -> ok.
ensure_identify_table() ->
    case ets:whereis(?IDENTIFY_TABLE) of
        undefined -> create_identify_table();
        _ -> ok
    end.

-spec create_identify_table() -> ok.
create_identify_table() ->
    case create_rate_table(?IDENTIFY_TABLE) of
        created -> schedule_identify_cleanup();
        exists -> ok
    end.

-spec schedule_identify_cleanup() -> ok.
schedule_identify_cleanup() ->
    case ets:whereis(?IDENTIFY_TABLE) of
        undefined -> ok;
        Tid -> spawn_identify_cleanup(Tid)
    end.

-spec spawn_identify_cleanup(ets:table()) -> ok.
spawn_identify_cleanup(Tid) ->
    _ = spawn(fun() -> identify_cleanup_loop(Tid) end),
    ok.

-spec identify_cleanup_loop(ets:table()) -> ok.
identify_cleanup_loop(Table) ->
    ok = gateway_retry_timer:wait(?IDENTIFY_CLEANUP_INTERVAL_MS),
    case prune_old_identify_entries(Table) of
        ok -> identify_cleanup_loop(Table);
        gone -> ok
    end.

-spec prune_old_identify_entries(ets:table()) -> ok | gone.
prune_old_identify_entries(Table) ->
    Now = erlang:system_time(second),
    Cutoff = Now div ?IDENTIFY_WINDOW_SECS - 1,
    try
        _ = ets:select_delete(Table, [
            {{{'$1', '$2'}, '_'}, [{'<', '$2', Cutoff}], [true]}
        ]),
        ok
    catch
        error:badarg -> gone
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

check_identify_rate_allows_under_limit_test() ->
    ensure_tables(),
    IP = <<"192.0.2.100">>,
    ?assertEqual(ok, check_identify_rate(IP)).

check_identify_rate_blocks_over_limit_test() ->
    with_rate_limits_enabled(fun() ->
        ensure_tables(),
        IP = <<"192.0.2.200">>,
        lists:foreach(
            fun(_) -> check_identify_rate(IP) end,
            lists:seq(1, ?IDENTIFY_MAX_PER_IP)
        ),
        ?assertEqual({error, identify_rate_limited}, check_identify_rate(IP))
    end).

check_identify_rate_disabled_by_env_test() ->
    OldValue = os:getenv("FLUXER_DISABLE_RATE_LIMITS"),
    os:putenv("FLUXER_DISABLE_RATE_LIMITS", "true"),
    try
        ensure_tables(),
        IP = <<"192.0.2.201">>,
        lists:foreach(
            fun(_) -> ?assertEqual(ok, check_identify_rate(IP)) end,
            lists:seq(1, ?IDENTIFY_MAX_PER_IP + 1)
        ),
        ?assertEqual(ok, check_identify_rate(IP))
    after
        restore_env("FLUXER_DISABLE_RATE_LIMITS", OldValue)
    end.

check_identify_rate_non_binary_returns_ok_test() ->
    ?assertEqual(ok, check_identify_rate(undefined)).

user_session_limit_allows_under_limit_test() ->
    ensure_tables(),
    UserId = 900001,
    delete_user_session_count(UserId),
    ?assertEqual(ok, check_user_session_limit(UserId)),
    lists:foreach(fun(_) -> increment_user_sessions(UserId) end, lists:seq(1, 5)),
    ?assertEqual(ok, check_user_session_limit(UserId)),
    delete_user_session_count(UserId).

user_session_limit_blocks_over_limit_test() ->
    with_rate_limits_enabled(fun() ->
        ensure_tables(),
        UserId = 900002,
        delete_user_session_count(UserId),
        lists:foreach(
            fun(_) -> increment_user_sessions(UserId) end,
            lists:seq(1, ?MAX_SESSIONS_PER_USER)
        ),
        ?assertEqual({error, too_many_sessions}, check_user_session_limit(UserId)),
        delete_user_session_count(UserId)
    end).

check_user_session_limit_disabled_by_env_test() ->
    OldValue = os:getenv("FLUXER_DISABLE_RATE_LIMITS"),
    os:putenv("FLUXER_DISABLE_RATE_LIMITS", "true"),
    try
        ensure_tables(),
        UserId = 900005,
        delete_user_session_count(UserId),
        lists:foreach(
            fun(_) -> increment_user_sessions(UserId) end,
            lists:seq(1, ?MAX_SESSIONS_PER_USER + 1)
        ),
        ?assertEqual(ok, check_user_session_limit(UserId)),
        delete_user_session_count(UserId)
    after
        restore_env("FLUXER_DISABLE_RATE_LIMITS", OldValue)
    end.

user_session_decrement_works_test() ->
    with_rate_limits_enabled(fun() ->
        ensure_tables(),
        UserId = 900003,
        delete_user_session_count(UserId),
        lists:foreach(
            fun(_) -> increment_user_sessions(UserId) end,
            lists:seq(1, ?MAX_SESSIONS_PER_USER)
        ),
        ?assertEqual({error, too_many_sessions}, check_user_session_limit(UserId)),
        decrement_user_sessions(UserId),
        ?assertEqual(ok, check_user_session_limit(UserId)),
        delete_user_session_count(UserId)
    end).

user_session_decrement_does_not_go_negative_test() ->
    ensure_tables(),
    UserId = 900004,
    delete_user_session_count(UserId),
    increment_user_sessions(UserId),
    decrement_user_sessions(UserId),
    decrement_user_sessions(UserId),
    ?assertEqual(ok, check_user_session_limit(UserId)).

user_session_limit_non_integer_returns_ok_test() ->
    ?assertEqual(ok, check_user_session_limit(undefined)),
    ?assertEqual(ok, check_user_session_limit(<<"not_an_id">>)).

prune_old_identify_entries_removes_old_buckets_test() ->
    ensure_tables(),
    Now = erlang:system_time(second),
    CurrentBucket = Now div ?IDENTIFY_WINDOW_SECS,
    OldBucket = CurrentBucket - 5,
    OldKey = {<<"10.0.0.1">>, OldBucket},
    CurrentKey = {<<"10.0.0.2">>, CurrentBucket},
    ets:insert(?IDENTIFY_TABLE, {OldKey, 3}),
    ets:insert(?IDENTIFY_TABLE, {CurrentKey, 7}),
    prune_old_identify_entries(?IDENTIFY_TABLE),
    ?assertEqual([], ets:lookup(?IDENTIFY_TABLE, OldKey)),
    ?assertEqual([{CurrentKey, 7}], ets:lookup(?IDENTIFY_TABLE, CurrentKey)),
    ets:delete(?IDENTIFY_TABLE, CurrentKey).

prune_old_identify_entries_keeps_recent_buckets_test() ->
    ensure_tables(),
    Now = erlang:system_time(second),
    CurrentBucket = Now div ?IDENTIFY_WINDOW_SECS,
    RecentKey = {<<"10.0.0.3">>, CurrentBucket - 1},
    ets:insert(?IDENTIFY_TABLE, {RecentKey, 5}),
    prune_old_identify_entries(?IDENTIFY_TABLE),
    ?assertNotEqual([], ets:lookup(?IDENTIFY_TABLE, RecentKey)),
    ets:delete(?IDENTIFY_TABLE, RecentKey).

identify_bucket_survives_creating_process_death_test() ->
    drop_identify_table(),
    {ok, _} = guild_ets_owner:start_link(),
    try
        assert_identify_bucket_outlives_creator()
    after
        gen_server:stop(guild_ets_owner)
    end.

assert_identify_bucket_outlives_creator() ->
    IP = <<"192.0.2.220">>,
    stop_table_owner(start_identify_bucket_creator(IP)),
    ?assertNotEqual(undefined, ets:whereis(?IDENTIFY_TABLE)),
    Key = {IP, erlang:system_time(second) div ?IDENTIFY_WINDOW_SECS},
    ?assertEqual([{Key, 1}], ets:lookup(?IDENTIFY_TABLE, Key)).

start_identify_bucket_creator(IP) ->
    Parent = self(),
    Pid = spawn(fun() -> create_identify_bucket(Parent, IP) end),
    receive
        {owner_ready, Pid} -> Pid
    after 1000 -> error(owner_start_timeout)
    end.

create_identify_bucket(Parent, IP) ->
    ok = check_identify_rate(IP),
    Parent ! {owner_ready, self()},
    receive
        stop -> ok
    after 30000 -> ok
    end.

identify_cleanup_loops_do_not_outlive_their_table_test() ->
    meck:new(gateway_retry_timer, [passthrough]),
    meck:expect(gateway_retry_timer, wait, fun(_) -> timer:sleep(5) end),
    try
        assert_identify_cleanup_loops_do_not_leak()
    after
        meck:unload(gateway_retry_timer)
    end.

assert_identify_cleanup_loops_do_not_leak() ->
    drop_identify_table(),
    Before = count_identify_cleanup_loops(),
    lists:foreach(fun(_) -> churn_identify_table_owner() end, lists:seq(1, 5)),
    Owner = start_identify_table_owner(),
    try
        ?assert(await_identify_cleanup_loops(Before + 1, 200))
    after
        stop_table_owner(Owner)
    end.

drop_identify_table() ->
    case ets:whereis(?IDENTIFY_TABLE) of
        undefined ->
            ok;
        _ ->
            ets:delete(?IDENTIFY_TABLE),
            ok
    end.

churn_identify_table_owner() ->
    stop_table_owner(start_identify_table_owner()).

start_identify_table_owner() ->
    Parent = self(),
    Pid = spawn(fun() -> own_identify_table(Parent) end),
    receive
        {owner_ready, Pid} -> Pid
    after 1000 -> error(owner_start_timeout)
    end.

own_identify_table(Parent) ->
    ok = check_identify_rate(<<"192.0.2.210">>),
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

count_identify_cleanup_loops() ->
    length([Pid || Pid <- erlang:processes(), is_identify_cleanup_loop(Pid)]).

is_identify_cleanup_loop(Pid) ->
    case erlang:process_info(Pid, current_stacktrace) of
        {current_stacktrace, Stack} ->
            lists:any(fun is_identify_cleanup_frame/1, Stack);
        _ ->
            false
    end.

is_identify_cleanup_frame({?MODULE, identify_cleanup_loop, _Arity, _Location}) ->
    true;
is_identify_cleanup_frame(_Frame) ->
    false.

await_identify_cleanup_loops(Max, 0) ->
    count_identify_cleanup_loops() =< Max;
await_identify_cleanup_loops(Max, Attempts) ->
    case count_identify_cleanup_loops() =< Max of
        true ->
            true;
        false ->
            timer:sleep(10),
            await_identify_cleanup_loops(Max, Attempts - 1)
    end.

delete_user_session_count(UserId) ->
    try ets:delete(?SESSION_USER_COUNTS, UserId) of
        _ -> ok
    catch
        error:badarg -> ok
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

-endif.
