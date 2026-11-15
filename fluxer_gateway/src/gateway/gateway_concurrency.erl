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
    session_queue_size => non_neg_integer(),
    session_pending => #{{pid(), term()} => pos_integer()},
    session_holders => #{pid() => pos_integer()},
    session_held => non_neg_integer(),
    guild_holders => #{pid() => true},
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
    Limit = gateway_rollout_config:max_concurrent_guild_starts(),
    try
        gen_server:call(?MODULE, {acquire_guild_start, Limit}, infinity)
    catch
        exit:_ -> ok
    end.

-spec release_guild_start() -> ok.
release_guild_start() ->
    gen_server:cast(?MODULE, {release_guild_start, self()}).

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
        session_queue_size => 0,
        session_pending => #{},
        session_holders => #{},
        session_held => 0,
        guild_holders => #{},
        monitors => #{}
    }}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({acquire_guild_start, Limit}, {Pid, _Tag}, State) when
    is_integer(Limit), is_pid(Pid)
->
    Holders = maps:get(guild_holders, State, #{}),
    case map_size(Holders) >= Limit of
        true ->
            {reply, {error, at_capacity}, State};
        false ->
            Held = ensure_monitor(Pid, State#{guild_holders => Holders#{Pid => true}}),
            {reply, ok, publish_guild_held(Held)}
    end;
handle_call(session_queue_length, _From, State) ->
    {reply, maps:get(session_queued, State, 0), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({request_session_start, Pid, Tag}, State) when is_pid(Pid) ->
    Monitored = ensure_monitor(Pid, State),
    Queue = maps:get(session_queue, Monitored, queue:new()),
    Pending = maps:get(session_pending, Monitored, #{}),
    Key = {Pid, Tag},
    Queued = Monitored#{
        session_queue => queue:in(Key, Queue),
        session_queue_size => maps:get(session_queue_size, Monitored, 0) + 1,
        session_queued => maps:get(session_queued, Monitored, 0) + 1,
        session_pending => Pending#{Key => maps:get(Key, Pending, 0) + 1}
    },
    {noreply, admit_session_starts(Queued)};
handle_cast({cancel_session_start, Pid, Tag}, State) when is_pid(Pid) ->
    case maps:take({Pid, Tag}, maps:get(session_pending, State, #{})) of
        {Count, Pending} ->
            {noreply, drop_queued(Count, Pending, State)};
        error ->
            {noreply, State}
    end;
handle_cast({release_session_start, Pid}, State) when is_pid(Pid) ->
    case maps:is_key(Pid, maps:get(session_holders, State, #{})) of
        true -> {noreply, admit_session_starts(release_held(Pid, 1, State))};
        false -> {noreply, State}
    end;
handle_cast({release_guild_start, Pid}, State) when is_pid(Pid) ->
    {noreply, release_guild_held(Pid, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', MonitorRef, process, Pid, _Reason}, State) when is_pid(Pid) ->
    Monitors = maps:get(monitors, State, #{}),
    case maps:get(Pid, Monitors, undefined) of
        MonitorRef ->
            Held = maps:get(Pid, maps:get(session_holders, State, #{}), 0),
            Released = release_guild_held(
                Pid, release_held(Pid, Held, State#{monitors => maps:remove(Pid, Monitors)})
            ),
            {Gone, Kept} = split_pending(Pid, maps:get(session_pending, Released, #{})),
            Dropped = drop_queued(Gone, Kept, Released),
            {noreply, admit_session_starts(Dropped)};
        _ ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec split_pending(pid(), #{{pid(), term()} => pos_integer()}) ->
    {non_neg_integer(), #{{pid(), term()} => pos_integer()}}.
split_pending(Pid, Pending) ->
    maps:fold(
        fun
            ({QueuedPid, _Tag}, Count, {GoneAcc, KeptAcc}) when QueuedPid =:= Pid ->
                {GoneAcc + Count, KeptAcc};
            (Key, Count, {GoneAcc, KeptAcc}) ->
                {GoneAcc, KeptAcc#{Key => Count}}
        end,
        {0, #{}},
        Pending
    ).

-spec ensure_monitor(pid(), state()) -> state().
ensure_monitor(Pid, State) ->
    Monitors = maps:get(monitors, State, #{}),
    case maps:is_key(Pid, Monitors) of
        true -> State;
        false -> State#{monitors => Monitors#{Pid => erlang:monitor(process, Pid)}}
    end.

-spec drop_queued(non_neg_integer(), #{{pid(), term()} => pos_integer()}, state()) -> state().
drop_queued(Count, Pending, State) ->
    Queued = max(0, maps:get(session_queued, State, 0) - Count),
    Size = maps:get(session_queue_size, State, 0),
    Dropped = State#{session_queued => Queued, session_pending => Pending},
    case Size > 2 * Queued + 1024 of
        true -> compact_queue(Pending, Dropped);
        false -> Dropped
    end.

-spec compact_queue(#{{pid(), term()} => pos_integer()}, state()) -> state().
compact_queue(Pending, State) ->
    Remaining = queue:filter(
        fun(Key) -> maps:is_key(Key, Pending) end,
        maps:get(session_queue, State, queue:new())
    ),
    State#{session_queue => Remaining, session_queue_size => queue:len(Remaining)}.

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

-spec release_guild_held(pid(), state()) -> state().
release_guild_held(Pid, State) ->
    Holders = maps:get(guild_holders, State, #{}),
    publish_guild_held(State#{guild_holders => maps:remove(Pid, Holders)}).

-spec publish_guild_held(state()) -> state().
publish_guild_held(#{counters := Ref} = State) ->
    counters:put(Ref, ?GUILD_START_IDX, map_size(maps:get(guild_holders, State, #{}))),
    State.

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
        {{value, Key}, Rest} ->
            Count = maps:get(Key, maps:get(session_pending, State, #{}), 0),
            Popped = State#{
                session_queue => Rest,
                session_queue_size => max(0, maps:get(session_queue_size, State, 0) - 1)
            },
            admit_popped(Limit, Held, Key, Count, Popped);
        _ ->
            State
    end.

-spec admit_popped(integer(), non_neg_integer(), {pid(), term()}, non_neg_integer(), state()) ->
    state().
admit_popped(Limit, _Held, _Key, 0, State) ->
    admit_session_starts(Limit, State);
admit_popped(Limit, Held, {Pid, Tag} = Key, Count, State) ->
    Pid ! {session_start_admitted, Tag},
    Holders = maps:get(session_holders, State, #{}),
    Admitted = State#{
        session_queued => max(0, maps:get(session_queued, State, 0) - 1),
        session_pending => decrement_pending(Key, Count, maps:get(session_pending, State, #{})),
        session_holders => Holders#{Pid => maps:get(Pid, Holders, 0) + 1}
    },
    admit_session_starts(Limit, set_held(Held + 1, Admitted)).

-spec decrement_pending({pid(), term()}, pos_integer(), #{{pid(), term()} => pos_integer()}) ->
    #{{pid(), term()} => pos_integer()}.
decrement_pending(Key, 1, Pending) ->
    maps:remove(Key, Pending);
decrement_pending(Key, Count, Pending) ->
    Pending#{Key => Count - 1}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.
