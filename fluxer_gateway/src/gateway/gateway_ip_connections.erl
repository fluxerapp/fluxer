%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_ip_connections).

-typing([eqwalizer]).

-export([acquire/1, release/1, note_disconnect/1]).

-define(CONNECTION_TABLE, gateway_ip_connections).
-define(OWNER_TABLE, gateway_ip_connection_owners).
-define(MAX_CONNECTIONS_PER_IP, 256).
-define(OWNER_SWEEP_INTERVAL_MS, 60000).

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
