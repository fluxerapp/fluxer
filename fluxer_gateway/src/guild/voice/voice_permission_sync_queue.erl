%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(voice_permission_sync_queue).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/0, enqueue/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type request() :: map().
-type state() :: #{
    connections := #{binary() => request() | none},
    monitors := #{reference() => binary()}
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
            {noreply, start_worker(ConnectionId, Req, State)}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', MRef, process, _Pid, _Reason}, State) ->
    #{connections := Connections, monitors := Monitors} = State,
    case maps:take(MRef, Monitors) of
        {ConnectionId, Monitors1} ->
            {noreply, worker_done(ConnectionId, Connections, State#{monitors => Monitors1})};
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

-spec worker_done(binary(), #{binary() => request() | none}, state()) -> state().
worker_done(ConnectionId, Connections, State) ->
    case maps:take(ConnectionId, Connections) of
        {none, Connections1} ->
            State#{connections => Connections1};
        {Next, Connections1} ->
            start_worker(ConnectionId, Next, State#{connections => Connections1});
        error ->
            State
    end.

-spec start_worker(binary(), request(), state()) -> state().
start_worker(ConnectionId, Req, State) ->
    #{connections := Connections, monitors := Monitors} = State,
    {_Pid, MRef} = spawn_monitor(fun() -> send_request(Req) end),
    State#{
        connections => Connections#{ConnectionId => none},
        monitors => Monitors#{MRef => ConnectionId}
    }.

-spec send_request(request()) -> ok.
send_request(Req) ->
    case rpc_client:call(Req) of
        {ok, _Data} -> ok;
        {error, _Reason} -> ok
    end.
