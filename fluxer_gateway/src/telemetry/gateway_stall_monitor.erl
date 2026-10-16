%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_stall_monitor).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/0, snapshot/0, reset/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TICK_MS, 100).
-define(LATE_TICK_MS, 250).
-define(SEVERE_MS, 1000).
-define(RECENT_MAX, 32).
-define(LOG_INTERVAL_MS, 10_000).
-define(DEFAULT_LONG_SCHEDULE_MS, 200).
-define(DEFAULT_LONG_GC_MS, 200).
-define(SNAPSHOT_TIMEOUT_MS, 5000).

-type kind() :: long_schedule | long_gc | busy_dist_port | timer_late.
-type stat() :: #{
    count := non_neg_integer(), severe := non_neg_integer(), max_ms := non_neg_integer()
}.
-type event() :: #{
    at_ms := integer(),
    kind := kind(),
    ms := non_neg_integer(),
    who := pid() | port() | undefined,
    info := term()
}.
-type monitor_status() :: installed | not_owner | disabled.
-type state() :: #{
    since_ms := integer(),
    status := monitor_status(),
    stats := #{kind() => stat()},
    recent := [event()],
    tick_due := integer(),
    last_log_ms := integer(),
    suppressed := non_neg_integer()
}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} -> {ok, Pid};
        ignore -> {error, ignore};
        {error, E} -> {error, E}
    end.

-spec snapshot() -> map().
snapshot() ->
    Raw = gen_server:call(?MODULE, snapshot, ?SNAPSHOT_TIMEOUT_MS),
    Recent = maps:get(recent, Raw, []),
    Raw#{recent => [describe(E) || E <- Recent]}.

-spec reset() -> ok.
reset() ->
    gen_server:call(?MODULE, reset, ?SNAPSHOT_TIMEOUT_MS).

-spec init([]) -> {ok, state()}.
init([]) ->
    _ = process_flag(priority, high),
    Now = now_ms(),
    {ok, #{
        since_ms => erlang:system_time(millisecond),
        status => install(),
        stats => #{},
        recent => [],
        tick_due => schedule_tick(Now),
        last_log_ms => Now - ?LOG_INTERVAL_MS,
        suppressed => 0
    }}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call(snapshot, _From, State) ->
    {reply, view(State), State};
handle_call(reset, _From, State) ->
    {reply, ok, State#{
        since_ms := erlang:system_time(millisecond), stats := #{}, recent := [], suppressed := 0
    }};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({monitor, Who, Kind, Info}, State) when
    Kind =:= long_schedule; Kind =:= long_gc; Kind =:= busy_dist_port
->
    {noreply, record(Kind, event_ms(Kind, Info), Who, compact_info(Kind, Info), State)};
handle_info(tick, #{tick_due := Due} = State) ->
    Now = now_ms(),
    State1 = State#{tick_due := schedule_tick(Now)},
    {noreply, maybe_record_late(Now - Due, State1)};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #{status := installed}) ->
    _ = release(),
    ok;
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec install() -> monitor_status().
install() ->
    case enabled() of
        false ->
            disabled;
        true ->
            case erlang:system_monitor() of
                undefined ->
                    _ = erlang:system_monitor(self(), [
                        {long_schedule,
                            threshold(
                                stall_monitor_long_schedule_ms, ?DEFAULT_LONG_SCHEDULE_MS
                            )},
                        {long_gc, threshold(stall_monitor_long_gc_ms, ?DEFAULT_LONG_GC_MS)},
                        busy_dist_port
                    ]),
                    installed;
                {Pid, _} when Pid =:= self() ->
                    installed;
                _ ->
                    not_owner
            end
    end.

-spec release() -> ok.
release() ->
    case erlang:system_monitor() of
        {Pid, _} when Pid =:= self() ->
            _ = erlang:system_monitor(undefined),
            ok;
        _ ->
            ok
    end.

-spec enabled() -> boolean().
enabled() ->
    application:get_env(fluxer_gateway, stall_monitor_enabled, true) =/= false.

-spec threshold(atom(), pos_integer()) -> pos_integer().
threshold(Key, Default) ->
    case application:get_env(fluxer_gateway, Key) of
        {ok, N} when is_integer(N), N > 0 -> N;
        _ -> Default
    end.

-spec maybe_record_late(integer(), state()) -> state().
maybe_record_late(Late, State) when Late >= ?LATE_TICK_MS ->
    record(timer_late, Late, undefined, #{late_ms => Late}, State);
maybe_record_late(_Late, State) ->
    State.

-spec record(kind(), non_neg_integer(), pid() | port() | undefined, term(), state()) -> state().
record(Kind, Ms, Who, Info, #{stats := Stats, recent := Recent} = State) ->
    Event = #{
        at_ms => erlang:system_time(millisecond),
        kind => Kind,
        ms => Ms,
        who => Who,
        info => Info
    },
    State1 = State#{
        stats := Stats#{Kind => bump(maps:get(Kind, Stats, empty_stat()), Ms)},
        recent := lists:sublist([Event | Recent], ?RECENT_MAX)
    },
    maybe_log(Event, State1).

-spec empty_stat() -> stat().
empty_stat() ->
    #{count => 0, severe => 0, max_ms => 0}.

-spec bump(stat(), non_neg_integer()) -> stat().
bump(#{count := C, severe := S, max_ms := M}, Ms) ->
    #{
        count => C + 1,
        severe => S + severe_inc(Ms),
        max_ms => max(M, Ms)
    }.

-spec severe_inc(non_neg_integer()) -> 0 | 1.
severe_inc(Ms) when Ms >= ?SEVERE_MS -> 1;
severe_inc(_Ms) -> 0.

-spec maybe_log(event(), state()) -> state().
maybe_log(Event, #{last_log_ms := Last, suppressed := Suppressed} = State) ->
    Now = now_ms(),
    case Now - Last >= ?LOG_INTERVAL_MS of
        true ->
            logger:warning("Scheduler stall detected", Event#{suppressed => Suppressed}),
            State#{last_log_ms := Now, suppressed := 0};
        false ->
            State#{suppressed := Suppressed + 1}
    end.

-spec event_ms(kind(), term()) -> non_neg_integer().
event_ms(_Kind, Info) when is_list(Info) ->
    case proplists:get_value(timeout, Info) of
        Ms when is_integer(Ms), Ms >= 0 -> Ms;
        _ -> 0
    end;
event_ms(_Kind, _Info) ->
    0.

-spec compact_info(kind(), term()) -> term().
compact_info(long_schedule, Info) when is_list(Info) ->
    maps:from_list([{K, V} || {K, V} <- Info, K =:= in orelse K =:= out]);
compact_info(long_gc, Info) when is_list(Info) ->
    maps:from_list([
        {K, V}
     || {K, V} <- Info, K =:= heap_size orelse K =:= old_heap_size orelse K =:= mbuf_size
    ]);
compact_info(_Kind, Info) when is_pid(Info); is_port(Info) ->
    #{peer => Info};
compact_info(_Kind, _Info) ->
    #{}.

-spec view(state()) -> map().
view(#{since_ms := Since, status := Status, stats := Stats, recent := Recent, suppressed := S}) ->
    #{
        since_ms => Since,
        status => Status,
        stats => Stats,
        recent => Recent,
        suppressed_logs => S
    }.

-spec describe(event()) -> map().
describe(#{who := Who} = Event) when is_pid(Who) ->
    Event#{who_info => pid_info(Who)};
describe(#{who := Who} = Event) when is_port(Who) ->
    Event#{who_info => port_name(Who)};
describe(Event) ->
    Event.

-spec pid_info(pid()) -> term().
pid_info(Pid) when node(Pid) =/= node() ->
    remote;
pid_info(Pid) ->
    Base =
        case erlang:process_info(Pid, [registered_name, {dictionary, '$initial_call'}]) of
            undefined -> #{alive => false};
            Items -> maps:from_list([{item_key(K), V} || {K, V} <- Items])
        end,
    Base#{guild_id => guild_id_of(Pid)}.

-spec item_key(term()) -> term().
item_key({dictionary, '$initial_call'}) -> initial_call;
item_key(K) -> K.

-spec guild_id_of(pid()) -> integer() | undefined.
guild_id_of(Pid) ->
    try ets:select(guild_pid_cache, [{{'$1', '$2'}, [{'=:=', '$2', Pid}], ['$1']}], 1) of
        {[GuildId | _], _} when is_integer(GuildId) -> GuildId;
        _ -> undefined
    catch
        error:badarg -> undefined
    end.

-spec port_name(port()) -> term().
port_name(Port) ->
    case erlang:port_info(Port, name) of
        {name, Name} -> Name;
        undefined -> closed
    end.

-spec schedule_tick(integer()) -> integer().
schedule_tick(Now) ->
    _ = erlang:send_after(?TICK_MS, self(), tick),
    Now + ?TICK_MS.

-spec now_ms() -> integer().
now_ms() ->
    erlang:monotonic_time(millisecond).
