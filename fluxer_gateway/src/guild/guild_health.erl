%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_health).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([
    start_link/0,
    is_overloaded/1,
    register_guild/1,
    put_session/2,
    remove_session/2,
    send_current/2
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TABLE, guild_health_status).
-define(INTERVAL_MS, 250).
-define(DEGRADED_MS, 2000).
-define(RECOVERED_MS, 500).
-define(REMOTE_LOOKUP_TIMEOUT_MS, 250).
-define(PROBE_TIMEOUT_MS, 5000).
-define(STALE_PROBE_MS, (?PROBE_TIMEOUT_MS + 4 * ?INTERVAL_MS)).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec is_overloaded(pid()) -> boolean().
is_overloaded(Pid) when node(Pid) =:= node() ->
    case lookup(Pid) of
        {Pid, _GuildId, _Degraded, _Targets, Pending} ->
            overloaded(Pending, erlang:monotonic_time(millisecond));
        undefined ->
            false
    end;
is_overloaded(Pid) ->
    try erpc:call(node(Pid), ?MODULE, is_overloaded, [Pid], ?REMOTE_LOOKUP_TIMEOUT_MS) of
        true -> true;
        _ -> false
    catch
        _:_ -> false
    end.

-spec register_guild(map()) -> map().
register_guild(State) ->
    case maps:get(id, State, undefined) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            State1 = ensure_targets(State),
            Targets = maps:get(guild_health_sessions, State1),
            try ets:insert_new(?TABLE, {self(), GuildId, false, Targets, undefined}) of
                _ -> gen_server:cast(?MODULE, {register, self()})
            catch
                error:badarg -> ok
            end,
            State1;
        _ ->
            State
    end.

-spec ensure_targets(map()) -> map().
ensure_targets(#{guild_health_sessions := Tab} = State) ->
    case table_owner(Tab) of
        Pid when Pid =:= self() -> State;
        _ -> ensure_targets(maps:remove(guild_health_sessions, State))
    end;
ensure_targets(State) ->
    Tab = ets:new(guild_health_sessions, [set, protected, {read_concurrency, true}]),
    State1 = State#{guild_health_sessions => Tab},
    maps:foreach(
        fun(SessionId, _) -> put_session(SessionId, State1) end, maps:get(sessions, State, #{})
    ),
    State1.

-spec table_owner(ets:table()) -> pid() | undefined.
table_owner(Tab) ->
    try ets:info(Tab, owner) of
        Owner -> Owner
    catch
        error:badarg -> undefined
    end.

-spec put_session(term(), map()) -> ok.
put_session(SessionId, #{guild_health_sessions := Tab} = State) ->
    case maps:get(SessionId, maps:get(sessions, State, #{}), undefined) of
        #{pid := Pid} when is_pid(Pid) ->
            ets:insert(Tab, {SessionId, Pid}),
            ok;
        _ ->
            ok
    end;
put_session(_, _) ->
    ok.

-spec remove_session(term(), map()) -> ok.
remove_session(SessionId, #{guild_health_sessions := Tab}) ->
    ets:delete(Tab, SessionId),
    ok;
remove_session(_, _) ->
    ok.

-spec send_current(pid(), pid()) -> ok.
send_current(GuildPid, SessionPid) ->
    gen_server:cast({?MODULE, node(GuildPid)}, {current, GuildPid, SessionPid}).

-spec init([]) -> {ok, map()}.
init([]) ->
    ok = guild_ets_owner:ensure_table(?TABLE, [
        named_table, public, set, {read_concurrency, true}
    ]),
    State = lists:foldl(
        fun({Pid, _, _, _, _}, Acc) -> track_guild(Pid, Acc) end, #{}, ets:tab2list(?TABLE)
    ),
    discover_guilds(),
    schedule_tick(),
    {ok, State}.

-spec handle_call(term(), gen_server:from(), map()) -> {reply, ok, map()}.
handle_call(_, _, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast({register, Pid}, State) when is_pid(Pid), node(Pid) =:= node() ->
    {noreply, track_guild(Pid, State)};
handle_cast({pong, Pid, Ref, HandledAt}, State) when is_pid(Pid), is_integer(HandledAt) ->
    ok = pong(Pid, Ref, HandledAt, erlang:monotonic_time(millisecond)),
    {noreply, State};
handle_cast({current, Pid, SessionPid}, State) when is_pid(Pid), is_pid(SessionPid) ->
    case lookup(Pid) of
        {Pid, GuildId, Degraded, _Targets, _Pending} ->
            notify_session(SessionPid, GuildId, Pid, Degraded);
        _ ->
            ok
    end,
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(tick, State) ->
    Now = erlang:monotonic_time(millisecond),
    maps:foreach(fun(Pid, _) -> check_guild(Pid, Now) end, State),
    schedule_tick(),
    {noreply, State};
handle_info({'DOWN', Ref, process, Pid, _}, State) ->
    case maps:get(Pid, State, undefined) of
        Ref ->
            case lookup(Pid) of
                {Pid, GuildId, _, _, _} -> guild_read_model:delete(GuildId, Pid);
                _ -> ok
            end,
            ets:delete(?TABLE, Pid),
            {noreply, maps:remove(Pid, State)};
        _ ->
            {noreply, State}
    end;
handle_info(_, State) ->
    {noreply, State}.

-spec terminate(term(), map()) -> ok.
terminate(_, _) -> ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_, State, _) -> {ok, State}.

-spec lookup(pid()) -> tuple() | undefined.
lookup(Pid) ->
    try ets:lookup(?TABLE, Pid) of
        [Entry] -> Entry;
        [] -> undefined
    catch
        error:badarg -> undefined
    end.

-spec track_guild(pid(), map()) -> map().
track_guild(Pid, State) ->
    case maps:is_key(Pid, State) orelse lookup(Pid) =:= undefined of
        true -> State;
        false -> State#{Pid => erlang:monitor(process, Pid)}
    end.

-spec check_guild(pid(), integer()) -> ok.
check_guild(Pid, Now) ->
    case lookup(Pid) of
        {Pid, GuildId, Degraded, Targets, undefined} ->
            case should_probe(Pid, Degraded) of
                true -> update({Pid, GuildId, Degraded, Targets, probe(Pid, Now, 0)}, Degraded);
                false -> ok
            end;
        {Pid, GuildId, Degraded, Targets, {_Ref, SentAt, Lag}} when
            Now - SentAt >= ?PROBE_TIMEOUT_MS
        ->
            Pending = probe(Pid, Now, max(Lag, Now - SentAt)),
            update({Pid, GuildId, Degraded, Targets, Pending}, true);
        {Pid, _, Degraded, _, {_Ref, SentAt, _Lag}} = Entry ->
            update(Entry, Degraded orelse Now - SentAt > ?DEGRADED_MS);
        _ ->
            ok
    end.

-spec pong(pid(), reference(), integer(), integer()) -> ok.
pong(Pid, Ref, HandledAt, Now) ->
    case lookup(Pid) of
        {Pid, GuildId, Degraded, Targets, {Ref, SentAt, _Lag}} ->
            Delay = max(0, HandledAt - SentAt),
            NewDegraded = next_degraded(Degraded, Delay),
            Pending = next_pending(Pid, NewDegraded, Delay, Now),
            update({Pid, GuildId, Degraded, Targets, Pending}, NewDegraded);
        _ ->
            ok
    end.

-spec next_pending(pid(), boolean(), non_neg_integer(), integer()) ->
    {reference(), integer(), non_neg_integer()} | undefined.
next_pending(Pid, true, Delay, Now) -> probe(Pid, Now, Delay);
next_pending(_Pid, false, _Delay, _Now) -> undefined.

-spec probe(pid(), integer(), non_neg_integer()) -> {reference(), integer(), non_neg_integer()}.
probe(Pid, Now, Lag) ->
    Ref = make_ref(),
    Pid ! {guild_health_probe, Ref},
    {Ref, Now, Lag}.

-spec overloaded(term(), integer()) -> boolean().
overloaded({_Ref, SentAt, Lag}, Now) when Now - SentAt =< ?STALE_PROBE_MS ->
    max(Lag, Now - SentAt) > ?DEGRADED_MS;
overloaded(_Pending, _Now) ->
    false.

-spec should_probe(pid(), boolean()) -> boolean().
should_probe(_Pid, true) ->
    true;
should_probe(Pid, false) ->
    case process_info(Pid, [message_queue_len, status]) of
        [{message_queue_len, Len}, {status, Status}] -> Len > 0 orelse Status =:= running;
        undefined -> false
    end.

-spec next_degraded(boolean(), non_neg_integer()) -> boolean().
next_degraded(_Degraded, Delay) when Delay > ?DEGRADED_MS -> true;
next_degraded(_Degraded, Delay) when Delay < ?RECOVERED_MS -> false;
next_degraded(Degraded, _Delay) -> Degraded.

-spec update(tuple(), boolean()) -> ok.
update({Pid, GuildId, Previous, Targets, Pending}, Degraded) ->
    ets:insert(?TABLE, {Pid, GuildId, Degraded, Targets, Pending}),
    case Previous =/= Degraded of
        true -> notify_targets(Targets, GuildId, Pid, Degraded);
        false -> ok
    end.

-spec notify_targets(ets:table(), integer(), pid(), boolean()) -> ok.
notify_targets(Targets, GuildId, Pid, Degraded) ->
    try ets:tab2list(Targets) of
        Rows ->
            lists:foreach(
                fun({_SessionId, SessionPid}) ->
                    notify_session(SessionPid, GuildId, Pid, Degraded)
                end,
                Rows
            )
    catch
        error:badarg -> ok
    end.

-spec notify_session(pid(), integer(), pid(), boolean()) -> ok.
notify_session(SessionPid, GuildId, GuildPid, Degraded) ->
    gen_server:cast(SessionPid, {guild_health, GuildId, GuildPid, Degraded}).

-spec discover_guilds() -> ok.
discover_guilds() ->
    try ets:tab2list(guild_pid_cache) of
        Rows ->
            lists:foreach(
                fun
                    ({_GuildId, Pid}) when is_pid(Pid), node(Pid) =:= node() ->
                        Pid ! guild_health_register;
                    (_) ->
                        ok
                end,
                Rows
            )
    catch
        error:badarg -> ok
    end.

-spec schedule_tick() -> reference().
schedule_tick() -> erlang:send_after(?INTERVAL_MS, self(), tick).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

hysteresis_test() ->
    ?assert(next_degraded(false, 2001)),
    ?assertNot(next_degraded(false, 2000)),
    ?assert(next_degraded(true, 500)),
    ?assertNot(next_degraded(true, 499)),
    ?assert(next_degraded(true, 3000)).

unreachable_remote_pid_is_unknown_test() ->
    Pid = binary_to_term(<<131, 88, 119, 12, "fake@nowhere", 1:32, 0:32, 1:32>>),
    ?assertNotEqual(node(), node(Pid)),
    {ElapsedUs, Result} = timer:tc(?MODULE, is_overloaded, [Pid]),
    ?assertNot(Result),
    ?assert(ElapsedUs < 1000000).

pending_probe_is_bounded_and_recovers_only_after_fresh_reply_test() ->
    ok = guild_ets_owner:ensure_table(?TABLE, [named_table, public, set]),
    Targets = ets:new(health_test_targets, [set]),
    ets:insert(Targets, {<<"session">>, self()}),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Entry = {Pid, 42, false, Targets, undefined},
    ets:insert(?TABLE, Entry),
    try
        Pid ! queued_work,
        check_guild(Pid, 100),
        {Pid, 42, false, Targets, {Ref, 100, 0}} = lookup(Pid),
        check_guild(Pid, 2201),
        {Pid, 42, true, Targets, {Ref, 100, 0}} = lookup(Pid),
        receive
            {'$gen_cast', {guild_health, 42, Pid, true}} -> ok
        after 100 -> ?assert(false)
        end,
        check_guild(Pid, 5000),
        ?assertEqual({message_queue_len, 2}, process_info(Pid, message_queue_len)),
        ok = pong(Pid, Ref, 5000, 5000),
        {Pid, 42, true, Targets, {Ref2, 5000, 4900}} = lookup(Pid),
        ?assertEqual({message_queue_len, 3}, process_info(Pid, message_queue_len)),
        ok = pong(Pid, Ref, 5110, 5110),
        {Pid, 42, true, Targets, {Ref2, 5000, 4900}} = lookup(Pid),
        ok = pong(Pid, Ref2, 5110, 5110),
        ?assertEqual({Pid, 42, false, Targets, undefined}, lookup(Pid)),
        receive
            {'$gen_cast', {guild_health, 42, Pid, false}} -> ok
        after 100 -> ?assert(false)
        end
    after
        Pid ! stop,
        ets:delete(?TABLE, Pid),
        ets:delete(Targets)
    end.

lost_pong_is_reprobed_and_the_guild_recovers_test() ->
    ok = guild_ets_owner:ensure_table(?TABLE, [named_table, public, set]),
    Targets = ets:new(health_lost_pong_targets, [set]),
    ets:insert(Targets, {<<"session">>, self()}),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ets:insert(?TABLE, {Pid, 44, false, Targets, undefined}),
    try
        Pid ! queued_work,
        check_guild(Pid, 0),
        {Pid, 44, false, Targets, {Lost, 0, 0}} = lookup(Pid),
        check_guild(Pid, 2001),
        receive
            {'$gen_cast', {guild_health, 44, Pid, true}} -> ok
        after 100 -> ?assert(false)
        end,
        check_guild(Pid, 4999),
        ?assertEqual({message_queue_len, 2}, process_info(Pid, message_queue_len)),
        check_guild(Pid, 5000),
        {Pid, 44, true, Targets, {Fresh, 5000, 5000} = Pending} = lookup(Pid),
        ?assertNotEqual(Lost, Fresh),
        ?assertEqual({message_queue_len, 3}, process_info(Pid, message_queue_len)),
        ?assert(overloaded(Pending, 5001)),
        ok = pong(Pid, Fresh, 5020, 5020),
        ?assertEqual({Pid, 44, false, Targets, undefined}, lookup(Pid)),
        ?assertNot(is_overloaded(Pid)),
        receive
            {'$gen_cast', {guild_health, 44, Pid, false}} -> ok
        after 100 -> ?assert(false)
        end,
        ok = pong(Pid, Lost, 5030, 5030),
        ?assertEqual({Pid, 44, false, Targets, undefined}, lookup(Pid))
    after
        Pid ! stop,
        ets:delete(?TABLE, Pid),
        ets:delete(Targets)
    end.

shedding_needs_fresh_evidence_of_lag_test() ->
    Ref = make_ref(),
    ?assertNot(overloaded(undefined, 10000)),
    ?assertNot(overloaded({Ref, 10000, 0}, 12000)),
    ?assert(overloaded({Ref, 10000, 0}, 12001)),
    ?assertNot(overloaded({Ref, 10000, 1500}, 10100)),
    ?assert(overloaded({Ref, 10000, 2500}, 10100)),
    ?assert(overloaded({Ref, 10000, 5000}, 10000 + ?STALE_PROBE_MS)),
    ?assertNot(overloaded({Ref, 10000, 5000}, 10001 + ?STALE_PROBE_MS)).

session_targets_follow_replacement_and_removal_test() ->
    Tab = ets:new(health_test_targets, [set]),
    State = #{guild_health_sessions => Tab, sessions => #{<<"s">> => #{pid => self()}}},
    ok = put_session(<<"s">>, State),
    ?assertEqual([{<<"s">>, self()}], ets:tab2list(Tab)),
    Other = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ok = put_session(<<"s">>, State#{sessions => #{<<"s">> => #{pid => Other}}}),
    ?assertEqual([{<<"s">>, Other}], ets:tab2list(Tab)),
    ok = remove_session(<<"s">>, State),
    ?assertEqual([], ets:tab2list(Tab)),
    Other ! stop,
    ets:delete(Tab).

idle_guild_is_not_probed_test() ->
    ok = guild_ets_owner:ensure_table(?TABLE, [named_table, public, set]),
    Targets = ets:new(health_idle_targets, [set]),
    Parent = self(),
    Pid = spawn(fun() ->
        Parent ! {idle, self()},
        receive
            stop -> ok
        end
    end),
    receive
        {idle, Pid} -> ok
    end,
    ets:insert(?TABLE, {Pid, 43, false, Targets, undefined}),
    try
        check_guild(Pid, 100),
        check_guild(Pid, 1000),
        ?assertEqual({message_queue_len, 0}, process_info(Pid, message_queue_len))
    after
        Pid ! stop,
        ets:delete(?TABLE, Pid),
        ets:delete(Targets)
    end.

foreign_target_table_is_rebuilt_test() ->
    Parent = self(),
    Owner = spawn(fun() ->
        Tab = ets:new(health_foreign_targets, [set, public]),
        Parent ! {foreign, Tab},
        receive
            stop -> ok
        end
    end),
    receive
        {foreign, Foreign} ->
            State = ensure_targets(#{
                guild_health_sessions => Foreign, sessions => #{<<"s">> => #{pid => self()}}
            }),
            Local = maps:get(guild_health_sessions, State),
            ?assertNotEqual(Foreign, Local),
            ?assertEqual(self(), ets:info(Local, owner)),
            ?assertEqual([{<<"s">>, self()}], ets:tab2list(Local)),
            ets:delete(Local)
    end,
    Owner ! stop.

-endif.
