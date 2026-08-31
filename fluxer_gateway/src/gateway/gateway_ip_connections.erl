%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_ip_connections).
-typing([eqwalizer]).

-export([acquire/1, release/1, note_disconnect/1]).

-define(CONNECTION_TABLE, gateway_ip_connections).
-define(OWNER_TABLE, gateway_ip_connection_owners).
-define(MAX_CONNECTIONS_PER_IP, 256).
-define(OWNER_SWEEP_INTERVAL_MS, 60000).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec acquire(term()) -> ok | {error, too_many_connections}.
acquire(PeerIP) when is_binary(PeerIP), PeerIP =/= <<"unknown">> ->
    case gateway_handler_rate_limit:rate_limits_disabled() of
        true ->
            ok;
        false ->
            ensure_tables(),
            do_acquire(PeerIP)
    end;
acquire(_) ->
    ok.

-spec release(term()) -> ok.
release(PeerIP) when is_binary(PeerIP), PeerIP =/= <<"unknown">> ->
    case take_owner({self(), PeerIP}) of
        true -> decrement(PeerIP);
        false -> ok
    end;
release(_) ->
    ok.

-spec note_disconnect(gateway_handler:state()) -> ok.
note_disconnect(State) ->
    release(maps:get(peer_ip, State, undefined)).

-spec do_acquire(binary()) -> ok | {error, too_many_connections}.
do_acquire(PeerIP) ->
    try ets:update_counter(?CONNECTION_TABLE, PeerIP, {2, 1}, {PeerIP, 0}) of
        Count when Count > ?MAX_CONNECTIONS_PER_IP ->
            _ = ets:update_counter(?CONNECTION_TABLE, PeerIP, {2, -1, 0, 0}),
            {error, too_many_connections};
        _ ->
            track_owner(PeerIP)
    catch
        error:badarg -> ok
    end.

-spec track_owner(binary()) -> ok.
track_owner(PeerIP) ->
    try
        _ = ets:insert(?OWNER_TABLE, {{self(), PeerIP}}),
        ok
    catch
        error:badarg -> ok
    end.

-spec take_owner({pid(), binary()}) -> boolean().
take_owner(Owner) ->
    try ets:take(?OWNER_TABLE, Owner) of
        [_ | _] -> true;
        [] -> false
    catch
        error:badarg -> false
    end.

-spec decrement(binary()) -> ok.
decrement(PeerIP) ->
    try update_connection_count(PeerIP) of
        ok -> ok
    catch
        error:badarg -> ok
    end.

-spec update_connection_count(binary()) -> ok.
update_connection_count(PeerIP) ->
    case ets:update_counter(?CONNECTION_TABLE, PeerIP, {2, -1, 0, 0}) of
        0 ->
            ets:delete(?CONNECTION_TABLE, PeerIP),
            ok;
        _ ->
            ok
    end.

-spec ensure_tables() -> ok.
ensure_tables() ->
    ensure_connection_table(),
    ensure_owner_table().

-spec ensure_connection_table() -> ok.
ensure_connection_table() ->
    case ets:whereis(?CONNECTION_TABLE) of
        undefined ->
            _ = create_table(?CONNECTION_TABLE),
            ok;
        _ ->
            ok
    end.

-spec ensure_owner_table() -> ok.
ensure_owner_table() ->
    case ets:whereis(?OWNER_TABLE) of
        undefined -> create_owner_table();
        _ -> ok
    end.

-spec create_owner_table() -> ok.
create_owner_table() ->
    case create_table(?OWNER_TABLE) of
        created -> schedule_owner_sweep();
        exists -> ok
    end.

-spec create_table(atom()) -> created | exists.
create_table(Table) ->
    try
        _ = ets:new(Table, table_options()),
        created
    catch
        error:badarg -> exists
    end.

-spec table_options() -> list().
table_options() ->
    [named_table, public, set, {write_concurrency, true}, {read_concurrency, true}] ++
        guild_ets_utils:heir_options().

-spec schedule_owner_sweep() -> ok.
schedule_owner_sweep() ->
    case ets:whereis(?OWNER_TABLE) of
        undefined -> ok;
        Tid -> spawn_owner_sweep(Tid)
    end.

-spec spawn_owner_sweep(ets:table()) -> ok.
spawn_owner_sweep(Tid) ->
    _ = spawn(fun() -> owner_sweep_loop(Tid) end),
    ok.

-spec owner_sweep_loop(ets:table()) -> ok.
owner_sweep_loop(Table) ->
    ok = gateway_retry_timer:wait(?OWNER_SWEEP_INTERVAL_MS),
    case reclaim_dead_owners(Table) of
        ok -> owner_sweep_loop(Table);
        gone -> ok
    end.

-spec reclaim_dead_owners(ets:table()) -> ok | gone.
reclaim_dead_owners(Table) ->
    try ets:select(Table, [{{{'$1', '$2'}}, [], [{{'$1', '$2'}}]}]) of
        Owners -> lists:foreach(fun reclaim_owner/1, Owners)
    catch
        error:badarg -> gone
    end.

-spec reclaim_owner({pid(), binary()}) -> ok.
reclaim_owner({Pid, PeerIP}) ->
    case is_process_alive(Pid) of
        true -> ok;
        false -> reclaim_dead_owner({Pid, PeerIP}, PeerIP)
    end.

-spec reclaim_dead_owner({pid(), binary()}, binary()) -> ok.
reclaim_dead_owner(Owner, PeerIP) ->
    case take_owner(Owner) of
        true -> decrement(PeerIP);
        false -> ok
    end.

-ifdef(TEST).

with_rate_limits_enabled(Fun) ->
    OldValue = os:getenv("FLUXER_DISABLE_RATE_LIMITS"),
    os:unsetenv("FLUXER_DISABLE_RATE_LIMITS"),
    try
        Fun()
    after
        restore_env("FLUXER_DISABLE_RATE_LIMITS", OldValue)
    end.

restore_env(Key, false) ->
    os:unsetenv(Key);
restore_env(Key, Value) ->
    os:putenv(Key, Value).

connection_cap_blocks_over_limit_test() ->
    with_rate_limits_enabled(fun() ->
        IP = <<"198.51.100.20">>,
        reset_connections(IP),
        assert_connection_cap_allows_limit(IP),
        ?assertEqual({error, too_many_connections}, acquire(IP)),
        ok = release(IP),
        ?assertEqual(ok, acquire(IP)),
        reset_connections(IP)
    end).

assert_connection_cap_allows_limit(IP) ->
    lists:foreach(
        fun(_) -> ?assertEqual(ok, acquire(IP)) end,
        lists:seq(1, ?MAX_CONNECTIONS_PER_IP)
    ).

connection_release_decrements_test() ->
    with_rate_limits_enabled(fun() ->
        IP = <<"198.51.100.30">>,
        reset_connections(IP),
        ok = acquire(IP),
        ok = release(IP),
        ?assertEqual([], ets:lookup(?CONNECTION_TABLE, IP))
    end).

killed_connection_owner_releases_ip_count_test() ->
    with_rate_limits_enabled(fun() ->
        with_fast_owner_sweep(fun assert_killed_connection_count_is_released/0)
    end).

assert_killed_connection_count_is_released() ->
    with_heir_owner(fun assert_killed_connection_count_drops/0).

assert_killed_connection_count_drops() ->
    IP = <<"198.51.100.80">>,
    Holder = start_connection_holder(IP),
    ?assertEqual(1, connection_count(IP)),
    kill_connection_holder(Holder),
    ?assert(await_connection_count(IP, 0, 300)).

released_connection_is_not_released_twice_test() ->
    with_rate_limits_enabled(fun() ->
        with_fast_owner_sweep(fun assert_released_connection_is_not_double_released/0)
    end).

assert_released_connection_is_not_double_released() ->
    with_heir_owner(fun assert_released_connection_keeps_peer_count/0).

assert_released_connection_keeps_peer_count() ->
    IP = <<"198.51.100.90">>,
    Holder = start_connection_holder(IP),
    Releaser = start_connection_holder(IP),
    ?assertEqual(2, connection_count(IP)),
    release_connection_holder(Releaser),
    ?assert(await_connection_count(IP, 1, 300)),
    timer:sleep(100),
    ?assertEqual(1, connection_count(IP)),
    stop_connection_holder(Holder).

with_fast_owner_sweep(Fun) ->
    meck:new(gateway_retry_timer, [passthrough]),
    meck:expect(gateway_retry_timer, wait, fun(_) -> timer:sleep(5) end),
    try
        Fun()
    after
        meck:unload(gateway_retry_timer)
    end.

with_heir_owner(Fun) ->
    drop_table(?CONNECTION_TABLE),
    drop_table(?OWNER_TABLE),
    {ok, _} = guild_ets_owner:start_link(),
    try
        Fun()
    after
        gen_server:stop(guild_ets_owner)
    end.

drop_table(Table) ->
    case ets:whereis(Table) of
        undefined ->
            ok;
        _ ->
            ets:delete(Table),
            ok
    end.

start_connection_holder(IP) ->
    Parent = self(),
    Pid = spawn(fun() -> hold_connection(Parent, IP) end),
    receive
        {connection_acquired, Pid} -> Pid
    after 1000 -> error(connection_holder_timeout)
    end.

hold_connection(Parent, IP) ->
    ok = acquire(IP),
    Parent ! {connection_acquired, self()},
    receive
        release -> release(IP);
        stop -> ok
    after 30000 -> ok
    end.

kill_connection_holder(Pid) ->
    exit(Pid, kill),
    await_connection_holder_down(Pid).

release_connection_holder(Pid) ->
    Pid ! release,
    await_connection_holder_down(Pid).

stop_connection_holder(Pid) ->
    Pid ! stop,
    await_connection_holder_down(Pid).

await_connection_holder_down(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _Reason} -> ok
    after 1000 -> error(connection_holder_down_timeout)
    end.

connection_count(IP) ->
    try ets:lookup(?CONNECTION_TABLE, IP) of
        [{_, Count}] -> Count;
        [] -> 0
    catch
        error:badarg -> 0
    end.

await_connection_count(IP, Expected, 0) ->
    connection_count(IP) =:= Expected;
await_connection_count(IP, Expected, Attempts) ->
    case connection_count(IP) =:= Expected of
        true ->
            true;
        false ->
            timer:sleep(10),
            await_connection_count(IP, Expected, Attempts - 1)
    end.

reset_connections(IP) ->
    _ = take_owner({self(), IP}),
    drop_connection_row(IP).

drop_connection_row(IP) ->
    case ets:whereis(?CONNECTION_TABLE) of
        undefined ->
            ok;
        _ ->
            ets:delete(?CONNECTION_TABLE, IP),
            ok
    end.

-endif.
