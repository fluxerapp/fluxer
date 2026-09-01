%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_dispatch_relay).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([
    start_link/0,
    dispatch/3,
    dispatch/4,
    dispatch_many/3,
    dispatch_many/4,
    dispatch_direct/3,
    dispatch_grouped/4,
    dispatch_remote_pids/2,
    group_by_node/1,
    diagnostic_info/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STATE_KEY, {gateway_dispatch_relay, state}).
-define(WORKER_KEY(Index), {gateway_dispatch_relay_worker, Index}).

-type state() :: coordinator_state() | worker_state().
-type worker_state() :: #{
    role := worker,
    index := non_neg_integer(),
    inflight := atomics:atomics_ref() | undefined,
    delivered := non_neg_integer()
}.
-type coordinator_state() :: #{
    role := coordinator,
    workers := tuple(),
    shard_count := pos_integer()
}.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link(
        {local, ?MODULE},
        ?MODULE,
        coordinator,
        [{spawn_opt, [{message_queue_data, off_heap}]}]
    ).

-spec dispatch(pid(), atom(), term()) -> ok.
dispatch(SessionPid, Event, Payload) ->
    gateway_dispatch_relay_batch:relay_or_direct(SessionPid, Event, Payload).

-spec dispatch(pid(), atom(), term(), term()) -> ok.
dispatch(SessionPid, Event, Payload, _IgnoredPartitionKey) ->
    dispatch(SessionPid, Event, Payload).

-spec dispatch_many([pid()], atom(), term()) -> ok.
dispatch_many(SessionPids, Event, Payload) ->
    gateway_dispatch_relay_batch:relay_or_direct_many(SessionPids, Event, Payload).

-spec dispatch_many([pid()], atom(), term(), term()) -> ok.
dispatch_many(SessionPids, Event, Payload, _IgnoredPartitionKey) ->
    dispatch_many(SessionPids, Event, Payload).

-spec dispatch_direct(term(), term(), term()) -> ok.
dispatch_direct(SessionPid, Event, Payload) when is_pid(SessionPid) ->
    Msg = {dispatch, Event, Payload},
    case node(SessionPid) of
        LocalNode when LocalNode =:= node() ->
            gen_server:cast(SessionPid, Msg);
        RemoteNode ->
            remote_dispatch_cast(RemoteNode, SessionPid, Msg)
    end;
dispatch_direct(_SessionPid, _Event, _Payload) ->
    ok.

-spec remote_dispatch_cast(node(), pid(), term()) -> ok.
remote_dispatch_cast(RemoteNode, SessionPid, Msg) ->
    try erpc:cast(RemoteNode, gen_server, cast, [SessionPid, Msg]) of
        _ -> ok
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec group_by_node([term()]) -> [{node(), [pid()]}].
group_by_node([]) ->
    [];
group_by_node(Pids) ->
    LocalNode = node(),
    {Local, RemoteGrouped} = group_by_node_loop(Pids, LocalNode, [], #{}),
    case map_size(RemoteGrouped) of
        0 -> [{LocalNode, lists:reverse(Local)}];
        _ -> [{LocalNode, lists:reverse(Local)} | maps:to_list(RemoteGrouped)]
    end.

-spec group_by_node_loop([term()], node(), [pid()], #{node() => [pid()]}) ->
    {[pid()], #{node() => [pid()]}}.
group_by_node_loop([], _LocalNode, Local, RemoteGrouped) ->
    {Local, RemoteGrouped};
group_by_node_loop([Pid | Rest], LocalNode, Local, RemoteGrouped) when is_pid(Pid) ->
    case node(Pid) of
        LocalNode ->
            group_by_node_loop(Rest, LocalNode, [Pid | Local], RemoteGrouped);
        RemoteNode ->
            group_by_node_loop(
                Rest,
                LocalNode,
                Local,
                RemoteGrouped#{RemoteNode => [Pid | maps:get(RemoteNode, RemoteGrouped, [])]}
            )
    end;
group_by_node_loop([_ | Rest], LocalNode, Local, RemoteGrouped) ->
    group_by_node_loop(Rest, LocalNode, Local, RemoteGrouped).

-spec dispatch_grouped([{node(), [pid()]}], term(), term(), fun((pid(), term(), term()) -> ok)) ->
    ok.
dispatch_grouped(Grouped, Event, Payload, DispatchFun) ->
    LocalNode = node(),
    Msg = {dispatch, Event, Payload},
    lists:foreach(
        fun
            ({Node, Pids}) when Node =:= LocalNode ->
                dispatch_local_group(Pids, Event, Payload, DispatchFun);
            ({RemoteNode, Pids}) ->
                dispatch_remote_group(RemoteNode, Pids, Msg)
        end,
        Grouped
    ),
    ok.

-spec dispatch_local_group([pid()], term(), term(), fun((pid(), term(), term()) -> ok)) -> ok.
dispatch_local_group(Pids, Event, Payload, DispatchFun) ->
    lists:foreach(fun(Pid) -> DispatchFun(Pid, Event, Payload) end, Pids).

-spec dispatch_remote_group(node(), [pid()], term()) -> ok.
dispatch_remote_group(RemoteNode, Pids, Msg) ->
    try erpc:cast(RemoteNode, gateway_dispatch_relay, dispatch_remote_pids, [Pids, Msg]) of
        _ -> ok
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec dispatch_remote_pids([pid()], term()) -> ok.
dispatch_remote_pids(Pids, Msg) ->
    lists:foreach(fun(Pid) -> gen_server:cast(Pid, Msg) end, Pids).

-spec diagnostic_info() -> map().
diagnostic_info() ->
    case gateway_dispatch_relay_batch:current_workers() of
        [] ->
            #{workers => 0};
        Workers ->
            Queues = [
                gateway_dispatch_relay_batch:message_queue_len(Pid)
             || Pid <- Workers, is_pid(Pid)
            ],
            #{workers => length(Workers), queues => Queues}
    end.

-spec init(coordinator | {worker, non_neg_integer()}) -> {ok, state()}.
init(coordinator) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 50),
    ShardCount = gateway_rollout_config:gateway_dispatch_relay_shards(),
    Workers = list_to_tuple(gateway_dispatch_relay_batch:start_workers(ShardCount)),
    persistent_term:put(?STATE_KEY, #{workers => Workers, shard_count => ShardCount}),
    {ok, #{role => coordinator, workers => Workers, shard_count => ShardCount}};
init({worker, Index}) ->
    erlang:process_flag(fullsweep_after, 50),
    persistent_term:put(?WORKER_KEY(Index), self()),
    {ok, #{
        role => worker,
        index => Index,
        inflight => gateway_dispatch_relay_batch:inflight_ref(Index),
        delivered => 0
    }}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call(diagnostic_info, _From, State) ->
    {reply, diagnostic_info(), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(
    {deliver, SessionPid, Event, Payload},
    #{role := worker, inflight := Inflight, delivered := Delivered} = State
) ->
    ok = gateway_dispatch_relay_batch:release_queue_slot(Inflight),
    dispatch_direct(SessionPid, Event, Payload),
    {noreply, State#{delivered := Delivered + 1}};
handle_cast(
    {deliver_many, SessionPids, Event, Payload},
    #{role := worker, inflight := Inflight, delivered := Delivered} = State
) when is_list(SessionPids) ->
    ok = gateway_dispatch_relay_batch:release_queue_slot(Inflight),
    deliver_many_direct(SessionPids, Event, Payload),
    {noreply, State#{delivered := Delivered + length(SessionPids)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec deliver_many_direct([pid()], term(), term()) -> ok.
deliver_many_direct(SessionPids, Event, Payload) ->
    dispatch_grouped(group_by_node(SessionPids), Event, Payload, fun dispatch_direct/3).

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'EXIT', Pid, Reason}, #{role := coordinator, workers := Workers} = State) when
    is_pid(Pid)
->
    case gateway_dispatch_relay_batch:worker_index(Pid, Workers, 0) of
        undefined ->
            {noreply, State};
        Index ->
            handle_worker_exit(Index, Reason, Workers, State)
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #{role := coordinator}) ->
    persistent_term:erase(?STATE_KEY),
    ok;
terminate(_Reason, #{role := worker, index := Index}) ->
    persistent_term:erase(?WORKER_KEY(Index)),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec handle_worker_exit(non_neg_integer(), term(), tuple(), coordinator_state()) ->
    {noreply, coordinator_state()}.
handle_worker_exit(Index, Reason, Workers, State) ->
    logger:warning("Dispatch relay worker exited", #{index => Index, reason => Reason}),
    NewPid = gateway_dispatch_relay_batch:start_worker(Index),
    NewWorkers = setelement(Index + 1, Workers, NewPid),
    persistent_term:put(?STATE_KEY, #{
        workers => NewWorkers,
        shard_count => maps:get(shard_count, State)
    }),
    {noreply, State#{workers := NewWorkers}}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

dispatch_direct_local_cast_test() ->
    Payload = #{<<"k">> => <<"v">>},
    ?assertEqual(ok, dispatch_direct(self(), guild_update, Payload)),
    receive
        {'$gen_cast', {dispatch, guild_update, Payload}} -> ok
    after 1000 ->
        ?assert(false)
    end.

dispatch_direct_local_cast_under_mailbox_pressure_test() ->
    Self = self(),
    Receiver = spawn(fun() -> test_dispatch_receiver(Self, pressured) end),
    lists:foreach(fun(N) -> Receiver ! {filler, N} end, lists:seq(1, 6000)),
    {message_queue_len, QueueLen} = erlang:process_info(Receiver, message_queue_len),
    ?assert(QueueLen > 5000),
    Payload = #{<<"pressure">> => true},
    ?assertEqual(ok, dispatch_direct(Receiver, guild_update, Payload)),
    assert_dispatch_received(pressured, guild_update, Payload).

group_by_node_empty_test() ->
    ?assertEqual([], group_by_node([])).

group_by_node_local_pids_test() ->
    P1 = spawn(fun test_wait_until_stop/0),
    P2 = spawn(fun test_wait_until_stop/0),
    Grouped = group_by_node([P1, P2]),
    ?assertEqual(1, length(Grouped)),
    [{Node, Pids}] = Grouped,
    ?assertEqual(node(), Node),
    ?assertEqual(2, length(Pids)),
    P1 ! stop,
    P2 ! stop.

group_by_node_filters_non_pids_test() ->
    P1 = spawn(fun test_wait_until_stop/0),
    Grouped = group_by_node([P1, not_a_pid, 42, <<"binary">>]),
    [{_Node, Pids}] = Grouped,
    ?assertEqual([P1], Pids),
    P1 ! stop.

dispatch_grouped_empty_test() ->
    ?assertEqual(ok, dispatch_grouped([], test_event, #{}, fun dispatch_direct/3)).

dispatch_grouped_local_pids_test() ->
    Self = self(),
    Receiver1 = spawn(fun() -> test_dispatch_receiver(Self, 1) end),
    Receiver2 = spawn(fun() -> test_dispatch_receiver(Self, 2) end),
    Grouped = [{node(), [Receiver1, Receiver2]}],
    Payload = #{<<"test">> => true},
    ?assertEqual(ok, dispatch_grouped(Grouped, my_event, Payload, fun dispatch_direct/3)),
    assert_dispatch_received(1, my_event, Payload),
    assert_dispatch_received(2, my_event, Payload).

test_wait_until_stop() ->
    receive
        stop -> ok
    after 1000 ->
        ok
    end.

test_dispatch_receiver(Parent, Id) ->
    receive
        {'$gen_cast', {dispatch, Ev, Pay}} -> Parent ! {got, Id, Ev, Pay}
    after 1000 ->
        Parent ! {timeout, Id}
    end.

assert_dispatch_received(Id, Event, Payload) ->
    receive
        {got, Id, Event, Payload} -> ok;
        {timeout, Id} -> ?assert(false)
    after 1000 ->
        ?assert(false)
    end.

-endif.
