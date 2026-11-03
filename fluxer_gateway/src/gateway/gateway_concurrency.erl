%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_concurrency).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([
    start_link/0,
    request_session_start/1,
    cancel_session_start/1,
    release_session_start/0,
    session_queue_length/0,
    try_acquire_guild_start/0,
    release_guild_start/0,
    session_start_count/0,
    guild_start_count/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SESSION_START_IDX, 1).
-define(GUILD_START_IDX, 2).

-type state() :: #{
    counters := counters:counters_ref(),
    session_queue => queue:queue({pid(), term()}),
    session_queued => non_neg_integer(),
    session_holders => #{pid() => pos_integer()},
    session_held => non_neg_integer(),
    monitors => #{pid() => reference()}
}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason};
        ignore -> {error, ignore}
    end.

-spec request_session_start(term()) -> ok.
request_session_start(Tag) ->
    gen_server:cast(?MODULE, {request_session_start, self(), Tag}).

-spec cancel_session_start(term()) -> ok.
cancel_session_start(Tag) ->
    gen_server:cast(?MODULE, {cancel_session_start, self(), Tag}).

-spec release_session_start() -> ok.
release_session_start() ->
    gen_server:cast(?MODULE, {release_session_start, self()}).

-spec session_queue_length() -> non_neg_integer().
session_queue_length() ->
    gen_server:call(?MODULE, session_queue_length, 1000).

-spec try_acquire_guild_start() -> ok | {error, at_capacity}.
try_acquire_guild_start() ->
    try_acquire(?GUILD_START_IDX, fun gateway_rollout_config:max_concurrent_guild_starts/0).

-spec try_acquire(pos_integer(), fun(() -> pos_integer())) -> ok | {error, at_capacity}.
try_acquire(Idx, LimitFun) ->
    try
        gen_server:call(?MODULE, {acquire, Idx, LimitFun()}, infinity)
    catch
        exit:_ -> ok
    end.

-spec do_acquire(pos_integer(), integer(), counters:counters_ref()) ->
    ok | {error, at_capacity}.
do_acquire(Idx, Limit, Ref) ->
    Count = counters:get(Ref, Idx),
    case Count >= Limit of
        true ->
            {error, at_capacity};
        false ->
            counters:add(Ref, Idx, 1),
            ok
    end.

-spec release_guild_start() -> ok.
release_guild_start() ->
    release_counter(?GUILD_START_IDX).

-spec release_counter(pos_integer()) -> ok.
release_counter(Idx) ->
    try
        Ref = persistent_term:get(gateway_concurrency_counters),
        case counters:get(Ref, Idx) of
            Value when Value > 0 ->
                counters:sub(Ref, Idx, 1);
            _ ->
                counters:put(Ref, Idx, 0)
        end,
        ok
    catch
        error:badarg -> ok
    end.

-spec session_start_count() -> non_neg_integer().
session_start_count() ->
    Ref = persistent_term:get(gateway_concurrency_counters),
    max(0, counters:get(Ref, ?SESSION_START_IDX)).

-spec guild_start_count() -> non_neg_integer().
guild_start_count() ->
    Ref = persistent_term:get(gateway_concurrency_counters),
    max(0, counters:get(Ref, ?GUILD_START_IDX)).

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:process_flag(fullsweep_after, 50),
    Ref = counters:new(2, [write_concurrency]),
    persistent_term:put(gateway_concurrency_counters, Ref),
    {ok, #{
        counters => Ref,
        session_queue => queue:new(),
        session_queued => 0,
        session_holders => #{},
        session_held => 0,
        monitors => #{}
    }}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({acquire, Idx, Limit}, _From, #{counters := Ref} = State) when
    is_integer(Idx), is_integer(Limit)
->
    {reply, do_acquire(Idx, Limit, Ref), State};
handle_call(session_queue_length, _From, State) ->
    {reply, maps:get(session_queued, State, 0), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({request_session_start, Pid, Tag}, State) when is_pid(Pid) ->
    Monitored = ensure_monitor(Pid, State),
    Queue = maps:get(session_queue, Monitored, queue:new()),
    Queued = Monitored#{
        session_queue => queue:in({Pid, Tag}, Queue),
        session_queued => maps:get(session_queued, Monitored, 0) + 1
    },
    {noreply, admit_session_starts(Queued)};
handle_cast({cancel_session_start, Pid, Tag}, State) when is_pid(Pid) ->
    {noreply, drop_queued(fun(Entry) -> Entry =/= {Pid, Tag} end, State)};
handle_cast({release_session_start, Pid}, State) when is_pid(Pid) ->
    case maps:is_key(Pid, maps:get(session_holders, State, #{})) of
        true -> {noreply, admit_session_starts(release_held(Pid, 1, State))};
        false -> {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', MonitorRef, process, Pid, _Reason}, State) when is_pid(Pid) ->
    Monitors = maps:get(monitors, State, #{}),
    case maps:get(Pid, Monitors, undefined) of
        MonitorRef ->
            Held = maps:get(Pid, maps:get(session_holders, State, #{}), 0),
            Released = release_held(Pid, Held, State#{monitors => maps:remove(Pid, Monitors)}),
            Dropped = drop_queued(fun({QueuedPid, _Tag}) -> QueuedPid =/= Pid end, Released),
            {noreply, admit_session_starts(Dropped)};
        _ ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec ensure_monitor(pid(), state()) -> state().
ensure_monitor(Pid, State) ->
    Monitors = maps:get(monitors, State, #{}),
    case maps:is_key(Pid, Monitors) of
        true -> State;
        false -> State#{monitors => Monitors#{Pid => erlang:monitor(process, Pid)}}
    end.

-spec drop_queued(fun(({pid(), term()}) -> boolean()), state()) -> state().
drop_queued(Keep, State) ->
    Remaining = queue:filter(Keep, maps:get(session_queue, State, queue:new())),
    State#{session_queue => Remaining, session_queued => queue:len(Remaining)}.

-spec release_held(pid(), non_neg_integer(), state()) -> state().
release_held(_Pid, 0, State) ->
    State;
release_held(Pid, Count, State) ->
    Holders = maps:get(session_holders, State, #{}),
    NewHolders =
        case maps:get(Pid, Holders, 0) - Count of
            Left when Left > 0 -> Holders#{Pid => Left};
            _ -> maps:remove(Pid, Holders)
        end,
    set_held(max(0, maps:get(session_held, State, 0) - Count), State#{
        session_holders => NewHolders
    }).

-spec set_held(non_neg_integer(), state()) -> state().
set_held(Held, #{counters := Ref} = State) ->
    counters:put(Ref, ?SESSION_START_IDX, Held),
    State#{session_held => Held}.

-spec admit_session_starts(state()) -> state().
admit_session_starts(State) ->
    admit_session_starts(gateway_rollout_config:max_concurrent_session_starts(), State).

-spec admit_session_starts(integer(), state()) -> state().
admit_session_starts(Limit, State) ->
    Held = maps:get(session_held, State, 0),
    case Held < Limit andalso queue:out(maps:get(session_queue, State, queue:new())) of
        {{value, {Pid, Tag}}, Rest} ->
            Pid ! {session_start_admitted, Tag},
            Holders = maps:get(session_holders, State, #{}),
            Admitted = State#{
                session_queue => Rest,
                session_queued => max(0, maps:get(session_queued, State, 0) - 1),
                session_holders => Holders#{Pid => maps:get(Pid, Holders, 0) + 1}
            },
            admit_session_starts(Limit, set_held(Held + 1, Admitted));
        _ ->
            State
    end.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.
