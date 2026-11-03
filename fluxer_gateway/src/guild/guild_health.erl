%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_health).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([
    start_link/0,
    is_overloaded/1,
    is_degraded/1,
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

-type pending() :: {reference(), integer(), non_neg_integer()} | undefined.
-type entry() :: {pid(), integer(), boolean(), ets:table(), pending()}.
-type state() :: #{pid() => reference()}.

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

-spec is_degraded(pid()) -> boolean().
is_degraded(Pid) ->
    case lookup(Pid) of
        {Pid, _GuildId, Degraded, _Targets, _Pending} -> Degraded;
        undefined -> false
    end.

-spec register_guild(map()) -> map().
register_guild(State) ->
    case maps:get(id, State, undefined) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            State1 = ensure_targets(State),
            Targets = maps:get(guild_health_sessions, State1),
            ok = insert_entry(GuildId, Targets),
            State1;
        _ ->
            State
    end.

-spec insert_entry(integer(), ets:table()) -> ok.
insert_entry(GuildId, Targets) ->
    try ets:insert_new(?TABLE, {self(), GuildId, false, Targets, undefined}) of
        _ -> gen_server:cast(?MODULE, {register, self()})
    catch
        error:badarg -> ok
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

-spec init([]) -> {ok, state()}.
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

-spec handle_call(term(), gen_server:from(), state()) -> {reply, ok, state()}.
handle_call(_, _, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({register, Pid}, State) when is_pid(Pid), node(Pid) =:= node() ->
    {noreply, track_guild(Pid, State)};
handle_cast({pong, Pid, Ref, HandledAt}, State) when
    is_pid(Pid), is_reference(Ref), is_integer(HandledAt)
->
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

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(tick, State) ->
    Now = erlang:monotonic_time(millisecond),
    maps:foreach(fun(Pid, _) -> check_guild(Pid, Now) end, State),
    schedule_tick(),
    {noreply, State};
handle_info({'DOWN', Ref, process, Pid, _}, State) when is_pid(Pid) ->
    case maps:get(Pid, State, undefined) of
        Ref ->
            ok = forget_guild(Pid),
            {noreply, maps:remove(Pid, State)};
        _ ->
            {noreply, State}
    end;
handle_info(_, State) ->
    {noreply, State}.

-spec forget_guild(pid()) -> ok.
forget_guild(Pid) ->
    case lookup(Pid) of
        {Pid, GuildId, _, _, _} -> guild_read_model:delete(GuildId, Pid);
        _ -> ok
    end,
    ets:delete(?TABLE, Pid),
    ok.

-spec terminate(term(), state()) -> ok.
terminate(_, _) -> ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_, State, _) -> {ok, State}.

-spec lookup(pid()) -> entry() | undefined.
lookup(Pid) ->
    try ets:lookup(?TABLE, Pid) of
        [Entry] -> eqwalizer:dynamic_cast(Entry);
        [] -> undefined
    catch
        error:badarg -> undefined
    end.

-spec track_guild(pid(), state()) -> state().
track_guild(Pid, State) ->
    case maps:is_key(Pid, State) orelse lookup(Pid) =:= undefined of
        true -> State;
        false -> State#{Pid => erlang:monitor(process, Pid)}
    end.

-spec check_guild(pid(), integer()) -> ok.
check_guild(Pid, Now) ->
    case lookup(Pid) of
        {Pid, _GuildId, Degraded, _Targets, undefined} = Entry ->
            maybe_probe(should_probe(Pid, Degraded), Entry, Now);
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

-spec maybe_probe(boolean(), entry(), integer()) -> ok.
maybe_probe(true, {Pid, GuildId, Degraded, Targets, _Pending}, Now) ->
    update({Pid, GuildId, Degraded, Targets, probe(Pid, Now, 0)}, Degraded);
maybe_probe(false, _Entry, _Now) ->
    ok.

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

-spec next_pending(pid(), boolean(), non_neg_integer(), integer()) -> pending().
next_pending(Pid, true, Delay, Now) -> probe(Pid, Now, Delay);
next_pending(_Pid, false, _Delay, _Now) -> undefined.

-spec probe(pid(), integer(), non_neg_integer()) -> {reference(), integer(), non_neg_integer()}.
probe(Pid, Now, Lag) ->
    Ref = make_ref(),
    Pid ! {guild_health_probe, Ref},
    {Ref, Now, Lag}.

-spec overloaded(pending(), integer()) -> boolean().
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

-spec update(entry(), boolean()) -> ok.
update({Pid, GuildId, Previous, Targets, Pending}, Degraded) ->
    ets:insert(?TABLE, {Pid, GuildId, Degraded, Targets, Pending}),
    case Previous =/= Degraded of
        true -> notify_targets(Targets, GuildId, Pid, Degraded);
        false -> ok
    end.

-spec notify_targets(ets:table(), integer(), pid(), boolean()) -> ok.
notify_targets(Targets, GuildId, Pid, Degraded) ->
    lists:foreach(
        fun({_SessionId, SessionPid}) -> notify_session(SessionPid, GuildId, Pid, Degraded) end,
        table_rows(Targets)
    ).

-spec notify_session(pid(), integer(), pid(), boolean()) -> ok.
notify_session(SessionPid, GuildId, GuildPid, Degraded) ->
    gen_server:cast(SessionPid, {guild_health, GuildId, GuildPid, Degraded}).

-spec discover_guilds() -> ok.
discover_guilds() ->
    lists:foreach(
        fun
            ({_GuildId, Pid}) when is_pid(Pid), node(Pid) =:= node() ->
                Pid ! guild_health_register;
            (_) ->
                ok
        end,
        table_rows(guild_pid_cache)
    ).

-spec table_rows(ets:table()) -> [{term(), pid()}].
table_rows(Tab) ->
    try
        eqwalizer:dynamic_cast(ets:tab2list(Tab))
    catch
        error:badarg -> []
    end.

-spec schedule_tick() -> reference().
schedule_tick() -> erlang:send_after(?INTERVAL_MS, self(), tick).
