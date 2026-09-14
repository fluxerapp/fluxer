%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_ets_owner).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/0, ensure_table/2, sweep_orphan_tables/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_CALL_TIMEOUT_MS, 5000).
-define(ORPHAN_SWEEP_INTERVAL_MS, 60000).

-type state() :: #{sweep_timer => reference()}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} -> {ok, Pid};
        ignore -> {error, ignore};
        {error, Reason} -> {error, Reason}
    end.

-spec ensure_table(atom(), list()) -> ok.
ensure_table(TableName, Options) ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid), Pid =/= self() ->
            call_owner(TableName, Options);
        _ ->
            ensure_table_local(TableName, Options)
    end.

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:process_flag(fullsweep_after, 0),
    ok = ensure_core_tables(),
    {ok, #{sweep_timer => schedule_orphan_sweep()}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, ok, state()}.
handle_call({ensure_table, TableName, Options}, _From, State) when
    is_atom(TableName), is_list(Options)
->
    {reply, ensure_table_local(TableName, Options), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(sweep_orphan_tables, State) ->
    _ = sweep_orphan_tables(),
    {noreply, State#{sweep_timer => schedule_orphan_sweep()}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec schedule_orphan_sweep() -> reference().
schedule_orphan_sweep() ->
    erlang:send_after(?ORPHAN_SWEEP_INTERVAL_MS, self(), sweep_orphan_tables).

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    cancel_orphan_sweep(State),
    ok.

-spec cancel_orphan_sweep(state()) -> ok.
cancel_orphan_sweep(#{sweep_timer := Ref}) when is_reference(Ref) ->
    _ = erlang:cancel_timer(Ref),
    ok;
cancel_orphan_sweep(_State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:process_flag(fullsweep_after, 0),
    _ = erlang:garbage_collect(),
    {ok, State}.

-spec call_owner(atom(), list()) -> ok.
call_owner(TableName, Options) ->
    try
        gen_server:call(?MODULE, {ensure_table, TableName, Options}, ?DEFAULT_CALL_TIMEOUT_MS)
    of
        ok -> ok;
        _ -> ensure_table_local(TableName, Options)
    catch
        exit:_Reason -> ensure_table_local(TableName, Options);
        error:_Reason -> ensure_table_local(TableName, Options)
    end.

-spec ensure_core_tables() -> ok.
ensure_core_tables() ->
    RW = rw_table_options(),
    RO = ro_table_options(),
    lists:foreach(fun({Name, Opts}) -> ensure_table_local(Name, Opts) end, [
        {guild_voice_registry, RW},
        {voice_state_count_connections, RW},
        {voice_state_count_regions, RW},
        {voice_state_count_servers, RW},
        {guild_unavailability_cache, RO},
        {guild_circuit_breaker, RW},
        {guild_permission_cache, RO},
        {voice_update_queue, RW},
        {voice_update_rate_limit, RW}
    ]).

-spec rw_table_options() -> list().
rw_table_options() ->
    [named_table, public, set, {read_concurrency, true}, {write_concurrency, true}].

-spec ro_table_options() -> list().
ro_table_options() ->
    [named_table, public, set, {read_concurrency, true}].

-spec ensure_table_local(atom(), list()) -> ok.
ensure_table_local(TableName, Options) ->
    case ets:whereis(TableName) of
        undefined -> try_create_table(TableName, Options);
        _ -> ok
    end.

-spec try_create_table(atom(), list()) -> ok.
try_create_table(TableName, Options) ->
    try ets:new(TableName, Options) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec sweep_orphan_tables() -> {ok, non_neg_integer()}.
sweep_orphan_tables() ->
    AllTables = ets:all(),
    OrphanCount = lists:foldl(fun check_table_owner/2, 0, AllTables),
    {ok, OrphanCount}.

-spec check_table_owner(ets:table(), non_neg_integer()) -> non_neg_integer().
check_table_owner(Tab, Acc) ->
    try ets:info(Tab, owner) of
        Owner when is_pid(Owner) ->
            check_owner_alive(Tab, Owner, Acc);
        _ ->
            Acc
    catch
        error:badarg ->
            Acc
    end.

-spec check_owner_alive(ets:table(), pid(), non_neg_integer()) -> non_neg_integer().
check_owner_alive(Tab, Owner, Acc) ->
    case erlang:is_process_alive(Owner) of
        true ->
            Acc;
        false ->
            Name = safe_table_name(Tab),
            Reclaimed = reclaim_orphan_table(Tab),
            logger:warning(
                "guild_ets_orphan_table: table=~p name=~p dead_owner=~p reclaimed=~p",
                [Tab, Name, Owner, Reclaimed]
            ),
            Acc + 1
    end.

-spec reclaim_orphan_table(ets:table()) -> boolean().
reclaim_orphan_table(Tab) ->
    try ets:delete(Tab) of
        _ -> true
    catch
        error:badarg -> false
    end.

-spec safe_table_name(ets:table()) -> atom() | unknown.
safe_table_name(Tab) ->
    try
        ets:info(Tab, name)
    catch
        error:badarg -> unknown
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

safe_delete_test_table(TableName) ->
    try ets:delete(TableName) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

ensure_table_creates_table_owned_by_owner_test() ->
    TableName = guild_ets_owner_test_table,
    safe_delete_test_table(TableName),
    {ok, Pid} = start_link(),
    try
        ok = ensure_table(TableName, [named_table, public, set]),
        ?assertEqual(Pid, ets:info(TableName, owner))
    after
        gen_server:stop(?MODULE),
        safe_delete_test_table(TableName)
    end.

core_voice_tables_are_owned_by_owner_on_start_test() ->
    lists:foreach(
        fun safe_delete_test_table/1,
        [
            guild_voice_registry,
            voice_state_count_connections,
            voice_state_count_regions,
            voice_state_count_servers,
            guild_unavailability_cache,
            guild_circuit_breaker,
            guild_permission_cache,
            voice_update_queue,
            voice_update_rate_limit
        ]
    ),
    {ok, Pid} = start_link(),
    try
        ?assertEqual(Pid, ets:info(guild_voice_registry, owner)),
        ?assertEqual(Pid, ets:info(voice_state_count_connections, owner)),
        ?assertEqual(Pid, ets:info(voice_state_count_regions, owner)),
        ?assertEqual(Pid, ets:info(voice_state_count_servers, owner)),
        ?assertEqual(Pid, ets:info(guild_unavailability_cache, owner)),
        ?assertEqual(Pid, ets:info(guild_circuit_breaker, owner)),
        ?assertEqual(Pid, ets:info(guild_permission_cache, owner)),
        ?assertEqual(Pid, ets:info(voice_update_queue, owner)),
        ?assertEqual(Pid, ets:info(voice_update_rate_limit, owner))
    after
        gen_server:stop(?MODULE)
    end.

sweep_orphan_tables_detects_dead_owner_test() ->
    Self = self(),
    Pid = spawn(fun() -> orphan_test_table_owner(Self) end),
    Tab =
        receive
            {tab, T} -> T
        after 1000 -> error(timeout)
        end,
    exit(Pid, kill),
    receive
        {'ETS-TRANSFER', Tab, Pid, orphan_test} -> ok
    after 1000 -> error(timeout)
    end,
    {ok, Count} = sweep_orphan_tables(),
    ?assert(Count >= 0),
    ets:delete(Tab).

orphan_sweep_is_scheduled_and_rearmed_test() ->
    {ok, Pid} = start_link(),
    try
        #{sweep_timer := Ref} = sys:get_state(Pid),
        ?assert(is_reference(Ref)),
        Pid ! sweep_orphan_tables,
        #{sweep_timer := NextRef} = sys:get_state(Pid),
        ?assert(is_reference(NextRef)),
        ?assertNotEqual(Ref, NextRef),
        ?assertEqual(Pid, ets:info(guild_voice_registry, owner))
    after
        gen_server:stop(?MODULE)
    end.

sweep_orphan_tables_returns_zero_when_clean_test() ->
    {ok, Count} = sweep_orphan_tables(),
    ?assert(is_integer(Count)),
    ?assert(Count >= 0).

orphan_test_table_owner(Parent) ->
    Tab = ets:new(orphan_test_table, [set, public, {heir, Parent, orphan_test}]),
    Parent ! {tab, Tab},
    receive
        stop -> ok
    after 30000 -> ok
    end.

-endif.
