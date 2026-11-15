%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(voice_permission_sync_queue).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/0, enqueue/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(RETRY_CONFIG, {4, 250, 2000, 100}).

-type request() :: map().
-type attempt() :: pos_integer().
-type state() :: #{
    connections := #{binary() => request() | none},
    monitors := #{reference() => {binary(), request(), attempt()}}
}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} -> {ok, Pid};
        ignore -> {error, ignore};
        {error, E} -> {error, E}
    end.

-spec enqueue(binary(), request()) -> ok.
enqueue(ConnectionId, Req) ->
    gen_server:cast(?MODULE, {enqueue, ConnectionId, Req}).

-spec init([]) -> {ok, state()}.
init([]) ->
    {ok, #{connections => #{}, monitors => #{}}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, ok, state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({enqueue, ConnectionId, Req}, State) ->
    #{connections := Connections} = State,
    case maps:find(ConnectionId, Connections) of
        {ok, _Pending} ->
            {noreply, State#{connections => Connections#{ConnectionId => Req}}};
        error ->
            {noreply, start_worker(ConnectionId, Req, 1, State)}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', MRef, process, _Pid, Reason}, State) ->
    #{connections := Connections, monitors := Monitors} = State,
    case maps:take(MRef, Monitors) of
        {{ConnectionId, Req, Attempt}, Monitors1} ->
            State1 = State#{monitors => Monitors1},
            {noreply, worker_exited(Reason, ConnectionId, Req, Attempt, Connections, State1)};
        error ->
            {noreply, State}
    end;
handle_info({retry, ConnectionId, Req, Attempt}, State) ->
    #{connections := Connections} = State,
    case maps:find(ConnectionId, Connections) of
        {ok, none} ->
            {noreply, start_worker(ConnectionId, Req, Attempt, State)};
        {ok, Next} ->
            {noreply, start_worker(ConnectionId, Next, 1, State)};
        error ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec worker_exited(
    term(), binary(), request(), attempt(), #{binary() => request() | none}, state()
) -> state().
worker_exited(retry, ConnectionId, Req, Attempt, Connections, State) ->
    {MaxAttempts, _BaseMs, _MaxMs, _JitterMs} = ?RETRY_CONFIG,
    case {maps:get(ConnectionId, Connections, none), Attempt < MaxAttempts} of
        {none, true} ->
            Delay = rpc_client:backoff_delay(Attempt, ?RETRY_CONFIG),
            _ = erlang:send_after(Delay, self(), {retry, ConnectionId, Req, Attempt + 1}),
            State;
        _ ->
            worker_done(ConnectionId, Connections, State)
    end;
worker_exited(_Reason, ConnectionId, _Req, _Attempt, Connections, State) ->
    worker_done(ConnectionId, Connections, State).

-spec worker_done(binary(), #{binary() => request() | none}, state()) -> state().
worker_done(ConnectionId, Connections, State) ->
    case maps:take(ConnectionId, Connections) of
        {none, Connections1} ->
            State#{connections => Connections1};
        {Next, Connections1} ->
            start_worker(ConnectionId, Next, 1, State#{connections => Connections1});
        error ->
            State
    end.

-spec start_worker(binary(), request(), attempt(), state()) -> state().
start_worker(ConnectionId, Req, Attempt, State) ->
    #{connections := Connections, monitors := Monitors} = State,
    {_Pid, MRef} = spawn_monitor(fun() -> send_request(Req) end),
    State#{
        connections => Connections#{ConnectionId => none},
        monitors => Monitors#{MRef => {ConnectionId, Req, Attempt}}
    }.

-spec send_request(request()) -> ok.
send_request(Req) ->
    case rpc_client:call(Req) of
        {ok, _Data} -> ok;
        {error, _Reason} -> exit(retry)
    end.
