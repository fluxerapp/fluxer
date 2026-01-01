%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(gateway_metrics_collector).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    inc_connections/0,
    inc_disconnections/0,
    inc_heartbeat_success/0,
    inc_heartbeat_failure/0,
    inc_resume_success/0,
    inc_resume_failure/0,
    inc_identify_rate_limited/0,
    record_rpc_latency/1,
    inc_websocket_close/1
]).

-type state() :: #{
    report_interval_ms := pos_integer(),
    timer_ref := reference() | undefined,
    connections := non_neg_integer(),
    disconnections := non_neg_integer(),
    heartbeat_success := non_neg_integer(),
    heartbeat_failure := non_neg_integer(),
    resume_success := non_neg_integer(),
    resume_failure := non_neg_integer(),
    identify_rate_limited := non_neg_integer(),
    rpc_latencies := [non_neg_integer()]
}.

-define(DEFAULT_REPORT_INTERVAL_MS, 30000).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(list()) -> {ok, state()}.
init([]) ->
    Enabled = get_enabled(),
    ReportInterval = get_report_interval(),
    BaseState = #{
        report_interval_ms => ReportInterval,
        timer_ref => undefined,
        connections => 0,
        disconnections => 0,
        heartbeat_success => 0,
        heartbeat_failure => 0,
        resume_success => 0,
        resume_failure => 0,
        identify_rate_limited => 0,
        rpc_latencies => []
    },
    case Enabled of
        true ->
            logger:info("[gateway_metrics_collector] starting with ~p ms interval", [ReportInterval]),
            TimerRef = schedule_collection(ReportInterval),
            {ok, BaseState#{timer_ref := TimerRef}};
        false ->
            logger:info("[gateway_metrics_collector] disabled"),
            {ok, BaseState}
    end.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(inc_connections, #{connections := Connections} = State) ->
    {noreply, State#{connections := Connections + 1}};
handle_cast(inc_disconnections, #{disconnections := Disconnections} = State) ->
    {noreply, State#{disconnections := Disconnections + 1}};
handle_cast(inc_heartbeat_success, #{heartbeat_success := HeartbeatSuccess} = State) ->
    {noreply, State#{heartbeat_success := HeartbeatSuccess + 1}};
handle_cast(inc_heartbeat_failure, #{heartbeat_failure := HeartbeatFailure} = State) ->
    {noreply, State#{heartbeat_failure := HeartbeatFailure + 1}};
handle_cast(inc_resume_success, #{resume_success := ResumeSuccess} = State) ->
    {noreply, State#{resume_success := ResumeSuccess + 1}};
handle_cast(inc_resume_failure, #{resume_failure := ResumeFailure} = State) ->
    {noreply, State#{resume_failure := ResumeFailure + 1}};
handle_cast(inc_identify_rate_limited, #{identify_rate_limited := IdentifyRateLimited} = State) ->
    {noreply, State#{identify_rate_limited := IdentifyRateLimited + 1}};
handle_cast({record_rpc_latency, LatencyMs}, #{rpc_latencies := Latencies} = State) ->
    MaxLatencies = 1000,
    NewLatencies = case length(Latencies) >= MaxLatencies of
        true -> [LatencyMs | lists:sublist(Latencies, MaxLatencies - 1)];
        false -> [LatencyMs | Latencies]
    end,
    {noreply, State#{rpc_latencies := NewLatencies}};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(collect_and_report, #{report_interval_ms := Interval} = State) ->
    collect_and_report_metrics(State),
    TimerRef = schedule_collection(Interval),
    ResetState = State#{
        timer_ref := TimerRef,
        connections := 0,
        disconnections := 0,
        heartbeat_success := 0,
        heartbeat_failure := 0,
        resume_success := 0,
        resume_failure := 0,
        identify_rate_limited := 0,
        rpc_latencies := []
    },
    {noreply, ResetState};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #{timer_ref := TimerRef}) ->
    case TimerRef of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

-spec code_change(term(), state() | tuple(), term()) -> {ok, state()}.
code_change(_OldVsn, {state, ReportIntervalMs, TimerRef, Connections, Disconnections,
                      HeartbeatSuccess, HeartbeatFailure, ResumeSuccess, ResumeFailure,
                      IdentifyRateLimited, RpcLatencies}, _Extra) ->
    {ok, #{
        report_interval_ms => ReportIntervalMs,
        timer_ref => TimerRef,
        connections => Connections,
        disconnections => Disconnections,
        heartbeat_success => HeartbeatSuccess,
        heartbeat_failure => HeartbeatFailure,
        resume_success => ResumeSuccess,
        resume_failure => ResumeFailure,
        identify_rate_limited => IdentifyRateLimited,
        rpc_latencies => RpcLatencies
    }};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec schedule_collection(pos_integer()) -> reference().
schedule_collection(IntervalMs) ->
    erlang:send_after(IntervalMs, self(), collect_and_report).

-spec get_enabled() -> boolean().
get_enabled() ->
    case fluxer_gateway_env:get(gateway_metrics_enabled) of
        false -> false;
        _ -> metrics_client:is_enabled()
    end.

-spec get_report_interval() -> pos_integer().
get_report_interval() ->
    case fluxer_gateway_env:get(gateway_metrics_report_interval_ms) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> ?DEFAULT_REPORT_INTERVAL_MS
    end.

-spec collect_and_report_metrics(state()) -> ok.
collect_and_report_metrics(State) ->
    Gauges = lists:flatten([
        collect_process_counts(),
        collect_mailbox_sizes(),
        collect_memory_stats(),
        collect_system_stats(),
        collect_event_metrics(State)
    ]),
    case Gauges of
        [] -> ok;
        _ -> metrics_client:batch(Gauges)
    end.

-spec collect_event_metrics(state()) -> [map()].
collect_event_metrics(State) ->
    #{
        rpc_latencies := RpcLatencies,
        connections := Connections,
        disconnections := Disconnections,
        heartbeat_success := HeartbeatSuccess,
        heartbeat_failure := HeartbeatFailure,
        resume_success := ResumeSuccess,
        resume_failure := ResumeFailure,
        identify_rate_limited := IdentifyRateLimited
    } = State,
    RpcLatencyStats = calculate_latency_stats(RpcLatencies),
    lists:flatten([
        [gauge(<<"gateway.websocket.connections">>, Connections)],
        [gauge(<<"gateway.websocket.disconnections">>, Disconnections)],
        [gauge(<<"gateway.heartbeat.success">>, HeartbeatSuccess)],
        [gauge(<<"gateway.heartbeat.failure">>, HeartbeatFailure)],
        [gauge(<<"gateway.resume.success">>, ResumeSuccess)],
        [gauge(<<"gateway.resume.failure">>, ResumeFailure)],
        [gauge(<<"gateway.identify.rate_limited">>, IdentifyRateLimited)],
        RpcLatencyStats
    ]).

-spec calculate_latency_stats([non_neg_integer()]) -> [map()].
calculate_latency_stats([]) ->
    [];
calculate_latency_stats(Latencies) ->
    Sorted = lists:sort(Latencies),
    Count = length(Sorted),
    Sum = lists:sum(Sorted),
    Avg = Sum / Count,
    Min = hd(Sorted),
    Max = lists:last(Sorted),
    P50 = percentile(Sorted, 50),
    P95 = percentile(Sorted, 95),
    P99 = percentile(Sorted, 99),
    [
        gauge(<<"gateway.rpc.latency.avg">>, Avg),
        gauge(<<"gateway.rpc.latency.min">>, Min),
        gauge(<<"gateway.rpc.latency.max">>, Max),
        gauge(<<"gateway.rpc.latency.p50">>, P50),
        gauge(<<"gateway.rpc.latency.p95">>, P95),
        gauge(<<"gateway.rpc.latency.p99">>, P99),
        gauge(<<"gateway.rpc.latency.count">>, Count)
    ].

-spec percentile([number()], number()) -> number().
percentile(SortedList, Percent) ->
    Len = length(SortedList),
    Index = max(1, min(Len, round(Len * Percent / 100))),
    lists:nth(Index, SortedList).

-spec collect_process_counts() -> [map()].
collect_process_counts() ->
    SessionCount = get_manager_count(session_manager),
    GuildCount = get_manager_count(guild_manager),
    PresenceCount = get_manager_count(presence_manager),
    CallCount = get_manager_count(call_manager),
    [
        gauge(<<"gateway.sessions.count">>, SessionCount),
        gauge(<<"gateway.guilds.count">>, GuildCount),
        gauge(<<"gateway.presences.count">>, PresenceCount),
        gauge(<<"gateway.calls.count">>, CallCount)
    ].

-spec get_manager_count(atom()) -> non_neg_integer().
get_manager_count(Manager) ->
    case catch gen_server:call(Manager, get_global_count, 1000) of
        {ok, Count} when is_integer(Count) -> Count;
        Count when is_integer(Count) -> Count;
        _ -> 0
    end.

-spec collect_mailbox_sizes() -> [map()].
collect_mailbox_sizes() ->
    Managers = [
        {session_manager, <<"gateway.mailbox.session_manager">>},
        {guild_manager, <<"gateway.mailbox.guild_manager">>},
        {presence_manager, <<"gateway.mailbox.presence_manager">>},
        {call_manager, <<"gateway.mailbox.call_manager">>},
        {push, <<"gateway.mailbox.push">>},
        {presence_cache, <<"gateway.mailbox.presence_cache">>},
        {presence_bus, <<"gateway.mailbox.presence_bus">>}
    ],
    MailboxMetrics = lists:filtermap(fun({Manager, MetricName}) ->
        case get_mailbox_size(Manager) of
            undefined -> false;
            Size -> {true, gauge(MetricName, Size)}
        end
    end, Managers),
    TotalMailbox = lists:foldl(fun({Manager, _}, Acc) ->
        case get_mailbox_size(Manager) of
            undefined -> Acc;
            Size -> Acc + Size
        end
    end, 0, Managers),
    [gauge(<<"gateway.mailbox.total">>, TotalMailbox) | MailboxMetrics].

-spec get_mailbox_size(atom()) -> non_neg_integer() | undefined.
get_mailbox_size(Manager) ->
    case whereis(Manager) of
        undefined -> undefined;
        Pid ->
            case erlang:process_info(Pid, message_queue_len) of
                {message_queue_len, Size} -> Size;
                undefined -> undefined
            end
    end.

-spec collect_memory_stats() -> [map()].
collect_memory_stats() ->
    PresenceCacheMemory = get_presence_cache_memory(),
    PushMemory = get_push_process_memory(),
    GuildMemoryStats = collect_guild_memory_stats(),
    lists:flatten([
        [gauge(<<"gateway.memory.presence_cache">>, PresenceCacheMemory)],
        [gauge(<<"gateway.memory.push">>, PushMemory)],
        GuildMemoryStats
    ]).

-spec collect_guild_memory_stats() -> [map()].
collect_guild_memory_stats() ->
    case catch process_memory_stats:get_guild_memory_stats(10000) of
        GuildStats when is_list(GuildStats), length(GuildStats) > 0 ->
            Memories = [maps:get(memory, G, 0) || G <- GuildStats],
            TotalMemory = lists:sum(Memories),
            GuildCount = length(Memories),
            AvgMemory = TotalMemory / GuildCount,
            MaxMemory = lists:max(Memories),
            MinMemory = lists:min(Memories),
            [
                gauge(<<"gateway.memory.guilds.total">>, TotalMemory),
                gauge(<<"gateway.memory.guilds.count">>, GuildCount),
                gauge(<<"gateway.memory.guilds.avg">>, AvgMemory),
                gauge(<<"gateway.memory.guilds.max">>, MaxMemory),
                gauge(<<"gateway.memory.guilds.min">>, MinMemory)
            ];
        _ ->
            []
    end.

-spec get_presence_cache_memory() -> non_neg_integer().
get_presence_cache_memory() ->
    case catch presence_cache:get_memory_stats() of
        {ok, #{memory_bytes := Bytes}} -> Bytes;
        _ -> 0
    end.

-spec get_push_process_memory() -> non_neg_integer().
get_push_process_memory() ->
    case whereis(push) of
        undefined -> 0;
        Pid ->
            case erlang:process_info(Pid, memory) of
                {memory, Bytes} -> Bytes;
                undefined -> 0
            end
    end.

-spec collect_system_stats() -> [map()].
collect_system_stats() ->
    {TotalMemory, ProcessMemory, SystemMemory} = get_memory_info(),
    ProcessCount = erlang:system_info(process_count),
    [
        gauge(<<"gateway.memory.total">>, TotalMemory),
        gauge(<<"gateway.memory.processes">>, ProcessMemory),
        gauge(<<"gateway.memory.system">>, SystemMemory),
        gauge(<<"gateway.process_count">>, ProcessCount)
    ].

-spec get_memory_info() -> {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
get_memory_info() ->
    MemData = erlang:memory(),
    Total = proplists:get_value(total, MemData, 0),
    Processes = proplists:get_value(processes, MemData, 0),
    System = proplists:get_value(system, MemData, 0),
    {Total, Processes, System}.

-spec gauge(binary(), number()) -> map().
gauge(Name, Value) ->
    #{
        type => gauge,
        name => Name,
        dimensions => #{},
        value => Value
    }.

-spec inc_connections() -> ok.
inc_connections() ->
    gen_server:cast(?MODULE, inc_connections).

-spec inc_disconnections() -> ok.
inc_disconnections() ->
    gen_server:cast(?MODULE, inc_disconnections).

-spec inc_heartbeat_success() -> ok.
inc_heartbeat_success() ->
    gen_server:cast(?MODULE, inc_heartbeat_success).

-spec inc_heartbeat_failure() -> ok.
inc_heartbeat_failure() ->
    gen_server:cast(?MODULE, inc_heartbeat_failure).

-spec inc_resume_success() -> ok.
inc_resume_success() ->
    gen_server:cast(?MODULE, inc_resume_success).

-spec inc_resume_failure() -> ok.
inc_resume_failure() ->
    gen_server:cast(?MODULE, inc_resume_failure).

-spec inc_identify_rate_limited() -> ok.
inc_identify_rate_limited() ->
    gen_server:cast(?MODULE, inc_identify_rate_limited).

-spec record_rpc_latency(non_neg_integer()) -> ok.
record_rpc_latency(LatencyMs) ->
    gen_server:cast(?MODULE, {record_rpc_latency, LatencyMs}).

-spec inc_websocket_close(atom()) -> ok.
inc_websocket_close(Reason) ->
    ReasonBin = atom_to_binary(Reason, utf8),
    metrics_client:counter(<<"gateway.websocket.close">>, #{<<"reason">> => ReasonBin}).
