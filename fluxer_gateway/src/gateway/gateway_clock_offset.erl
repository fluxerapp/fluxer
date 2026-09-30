%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_clock_offset).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/0, start_link/1, offset/1, sample/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TABLE, gateway_clock_offset).
-define(PROBE_INTERVAL_MS, 10_000).
-define(PROBE_TIMEOUT_MS, 1_000).
-define(MAX_PROBE_RTT_MS, 100).

-type read_fun() :: fun((node()) -> integer()).
-type options() :: #{
    read_fun => read_fun(),
    nodes_fun => fun(() -> [node()]),
    interval_ms => pos_integer()
}.
-type state() :: #{
    read_fun := read_fun(),
    nodes_fun := fun(() -> [node()]),
    interval_ms := pos_integer(),
    probes := #{node() => reference()}
}.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    start_link(#{}).

-spec start_link(options()) -> gen_server:start_ret().
start_link(Options) when is_map(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

-spec offset(node()) -> integer() | undefined.
offset(Node) when Node =:= node() ->
    0;
offset(Node) ->
    try ets:lookup(?TABLE, Node) of
        [{Node, Offset}] when is_integer(Offset) -> Offset;
        _ -> undefined
    catch
        error:badarg -> undefined
    end.

-spec sample(integer(), integer(), integer()) -> {ok, integer()} | discard.
sample(RemoteMs, LocalAfterMs, RttMs) when RttMs >= 0, RttMs =< ?MAX_PROBE_RTT_MS ->
    {ok, RemoteMs - LocalAfterMs};
sample(_RemoteMs, _LocalAfterMs, _RttMs) ->
    discard.

-spec init(options()) -> {ok, state()}.
init(Options) ->
    _ = ets:new(?TABLE, [named_table, protected, set, {read_concurrency, true}]),
    _ = net_kernel:monitor_nodes(true),
    self() ! probe_all,
    {ok, #{
        read_fun => maps:get(read_fun, Options, fun read_remote_clock/1),
        nodes_fun => maps:get(nodes_fun, Options, fun erlang:nodes/0),
        interval_ms => maps:get(interval_ms, Options, ?PROBE_INTERVAL_MS),
        probes => #{}
    }}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, ok, state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(probe_all, #{nodes_fun := NodesFun, interval_ms := IntervalMs} = State) ->
    erlang:send_after(IntervalMs, self(), probe_all),
    {noreply, lists:foldl(fun spawn_probe/2, State, NodesFun())};
handle_info({nodeup, Node}, State) when is_atom(Node) ->
    {noreply, spawn_probe(Node, State)};
handle_info({nodedown, Node}, #{probes := Probes} = State) when is_atom(Node) ->
    ets:delete(?TABLE, Node),
    {noreply, State#{probes := maps:remove(Node, Probes)}};
handle_info({clock_offset, Node, Ref, Offset}, #{probes := Probes} = State) when
    is_atom(Node), is_reference(Ref), is_integer(Offset)
->
    case maps:get(Node, Probes, undefined) of
        Ref -> ets:insert(?TABLE, {Node, Offset});
        _ -> ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec spawn_probe(node(), state()) -> state().
spawn_probe(Node, #{read_fun := ReadFun, probes := Probes} = State) ->
    Server = self(),
    Ref = make_ref(),
    _ = spawn(fun() -> probe(Server, Node, Ref, ReadFun) end),
    State#{probes := Probes#{Node => Ref}}.

-spec probe(pid(), node(), reference(), read_fun()) -> ok.
probe(Server, Node, Ref, ReadFun) ->
    Start = erlang:monotonic_time(millisecond),
    try ReadFun(Node) of
        RemoteMs when is_integer(RemoteMs) ->
            End = erlang:monotonic_time(millisecond),
            report(Server, Node, Ref, sample(RemoteMs, End, End - Start))
    catch
        _:_ -> ok
    end.

-spec report(pid(), node(), reference(), {ok, integer()} | discard) -> ok.
report(Server, Node, Ref, {ok, Offset}) ->
    Server ! {clock_offset, Node, Ref, Offset},
    ok;
report(_Server, _Node, _Ref, discard) ->
    ok.

-spec read_remote_clock(node()) -> integer().
read_remote_clock(Node) ->
    erpc:call(Node, erlang, monotonic_time, [millisecond], ?PROBE_TIMEOUT_MS).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sample_retains_the_full_return_trip_uncertainty_test() ->
    ?assertEqual({ok, 4990}, sample(15000, 10010, 20)),
    ?assertEqual({ok, -5000}, sample(5000, 10000, 0)),
    ?assertEqual(discard, sample(15000, 10000, 101)),
    ?assertEqual(discard, sample(15000, 10000, -1)).

local_clock_does_not_need_a_probe_test() ->
    ?assertEqual(0, offset(node())).

-endif.
