%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_nats_resubscribe_tests).
-typing([eqwalizer]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(CONFIG_KEY, {fluxer_gateway, runtime_config}).
-define(RPC_SUBJECT, <<"rpc.gateway.>">>).
-define(EXTRA_SUBJECT, <<"config.gateway.rollout">>).
-define(INFO,
    <<"INFO {\"server_id\":\"fake\",\"version\":\"2.10.0\",\"proto\":1,",
        "\"max_payload\":1048576,\"headers\":true}\r\n">>
).

subscribe_retry_delay_backs_off_and_stops_growing_test() ->
    Delays = [gateway_nats_pool_conn:subscribe_retry_delay(N) || N <- lists:seq(1, 8)],
    ?assertEqual([250, 500, 1000, 2000, 4000, 5000, 5000, 5000], Delays),
    ?assertEqual(250, gateway_nats_pool_conn:subscribe_retry_delay(0)).

closed_notice_before_connect_result_is_not_lost_test_() ->
    with_env(fun closed_notice_before_connect_result_is_not_lost/1).

closed_notice_before_connect_result_is_not_lost(Port) ->
    State0 = rpc_state(),
    Conn = failed_conn(Port),
    Attempt = dead_pid(),
    {noreply, State1} = gateway_nats_rpc:handle_info(
        {Conn, closed}, State0#{connecting_pid => Attempt}
    ),
    ?assertNot(is_process_alive(Conn)),
    {noreply, State2} = gateway_nats_rpc:handle_info(
        {nats_connect_result, Attempt, {ok, Conn}}, State1
    ),
    MRef = maps:get(monitor_ref, State2),
    Down = receive_down(MRef, Conn),
    {noreply, State3} = gateway_nats_rpc:handle_info(Down, State2),
    ?assertEqual(undefined, maps:get(conn, State3)),
    ?assertEqual(connect, receive_message(connect, 3000)).

ready_notice_before_connect_result_still_subscribes_test_() ->
    with_env(fun ready_notice_before_connect_result_still_subscribes/1).

ready_notice_before_connect_result_still_subscribes(Port) ->
    Server = start_fake_nats(Port),
    State0 = rpc_state(),
    {ok, Conn} = connect(Port),
    ready = receive_message({Conn, ready}, 5000, ready),
    Attempt = dead_pid(),
    {noreply, State1} = gateway_nats_rpc:handle_info(
        {nats_connect_result, Attempt, {ok, Conn}}, State0#{connecting_pid => Attempt}
    ),
    ?assert(maps:get(ready, State1)),
    ?assert(maps:is_key(?RPC_SUBJECT, maps:get(rpc_subs, State1))),
    ?assertEqual(ok, await_subs(?RPC_SUBJECT, 1, 5000)),
    gateway_nats_pool_conn:ignore_disconnect(Conn),
    stop_fake_nats(Server).

connection_that_never_becomes_ready_is_replaced_test_() ->
    with_env(fun connection_that_never_becomes_ready_is_replaced/1).

connection_that_never_becomes_ready_is_replaced(Port) ->
    State0 = rpc_state(),
    Conn = failed_conn(Port),
    Attempt = dead_pid(),
    Token = make_ref(),
    {noreply, State1} = gateway_nats_rpc:handle_info(
        {nats_connect_result, Attempt, {ok, Conn}},
        State0#{connecting_pid => Attempt, connecting_token => Token}
    ),
    ?assertEqual(Conn, maps:get(conn, State1)),
    ?assertNot(maps:get(ready, State1)),
    {noreply, State2} = gateway_nats_rpc:handle_info({connect_timeout, Token}, State1),
    ?assertEqual(undefined, maps:get(conn, State2)),
    ?assertNot(is_process_alive(Conn)),
    ?assertEqual(connect, receive_message(connect, 3000)).

subscribe_on_a_stuck_connection_gives_up_test_() ->
    with_env(fun subscribe_on_a_stuck_connection_gives_up/1).

subscribe_on_a_stuck_connection_gives_up(Port) ->
    Conn = failed_conn(Port),
    Result = gateway_nats_pool_conn:guarded_sub(Conn, ?RPC_SUBJECT, #{}),
    ?assertMatch({error, _}, Result),
    ?assertNot(is_process_alive(Conn)).

gateway_recovers_when_nats_starts_late_and_restarts_test_() ->
    with_env(fun gateway_recovers_when_nats_starts_late_and_restarts/1).

gateway_recovers_when_nats_starts_late_and_restarts(Port) ->
    {ok, Rpc} = gateway_nats_rpc:start_link(),
    {ok, Pool} = gateway_nats_pool:start_link(),
    timer:sleep(1500),
    ?assertEqual({error, no_connection}, gateway_nats_pool_conn:get_pool_conn()),
    ?assertNot(gateway_nats_rpc:nats_ready()),
    ?assertEqual({error, not_connected}, gateway_nats_rpc:subscribe(?EXTRA_SUBJECT, <<>>)),
    Server = start_fake_nats(Port),
    ?assertEqual(ok, await_subs(?RPC_SUBJECT, 3, 10000)),
    ?assertEqual(ok, await_subs(?EXTRA_SUBJECT, 1, 10000)),
    ?assertEqual(ok, wait_until(fun gateway_nats_rpc:nats_ready/0, 5000)),
    stop_fake_nats(Server),
    ?assertEqual(ok, wait_until(fun() -> not gateway_nats_rpc:nats_ready() end, 5000)),
    flush_subs(),
    Restarted = start_fake_nats(Port),
    ?assertEqual(ok, await_subs(?RPC_SUBJECT, 3, 10000)),
    ?assertEqual(ok, await_subs(?EXTRA_SUBJECT, 1, 10000)),
    ?assertEqual(ok, wait_until(fun gateway_nats_rpc:nats_ready/0, 5000)),
    ok = gen_server:stop(Pool),
    ok = gen_server:stop(Rpc),
    stop_fake_nats(Restarted).

with_env(Test) ->
    {setup, fun setup/0, fun cleanup/1, fun({Port, _Saved}) ->
        {timeout, 60, fun() -> run_trapping_exits(Test, Port) end}
    end}.

run_trapping_exits(Test, Port) ->
    TrapExit = process_flag(trap_exit, true),
    try
        Test(Port)
    after
        process_flag(trap_exit, TrapExit),
        flush()
    end.

setup() ->
    Saved = #{
        config => persistent_term:get(?CONFIG_KEY, undefined),
        logger => logger:get_primary_config()
    },
    ok = logger:set_primary_config(level, none),
    Port = free_port(),
    persistent_term:put(?CONFIG_KEY, #{
        nats_core_url => "nats://127.0.0.1:" ++ integer_to_list(Port),
        nats_auth_token => "",
        nats_rpc_enabled => true,
        nats_pool_size => 2,
        gateway_role => all,
        gateway_nats_rpc_max_handlers => 16
    }),
    {Port, Saved}.

cleanup({_Port, #{config := Config, logger := Logger}}) ->
    restore_config(Config),
    persistent_term:erase({gateway_nats_rpc, subscription_status}),
    ok = logger:set_primary_config(Logger).

restore_config(undefined) ->
    persistent_term:erase(?CONFIG_KEY);
restore_config(Config) ->
    persistent_term:put(?CONFIG_KEY, Config).

rpc_state() ->
    {ok, State} = gateway_nats_rpc:init([]),
    connect = receive_message(connect, 1000),
    State.

connect(Port) ->
    Server = #{scheme => <<"nats">>, host => "127.0.0.1", port => Port, family => inet},
    nats:connect(Server, #{buffer_size => 0}).

failed_conn(Port) ->
    {ok, Conn} = connect(Port),
    closed = receive_message({Conn, closed}, 5000, closed),
    flush_conn(Conn),
    ?assert(is_process_alive(Conn)),
    ?assertEqual(false, nats:is_ready(Conn)),
    Conn.

dead_pid() ->
    {Pid, MRef} = spawn_monitor(fun() -> ok end),
    receive
        {'DOWN', MRef, process, Pid, _} -> Pid
    end.

free_port() ->
    {ok, Socket} = gen_tcp:listen(0, [{ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Socket),
    ok = gen_tcp:close(Socket),
    Port.

start_fake_nats(Port) ->
    Owner = self(),
    Pid = spawn(fun() -> listen(Port, Owner) end),
    listening = receive_message({fake_nats_listening, Pid}, 5000, listening),
    Pid.

stop_fake_nats(Pid) ->
    MRef = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    end.

listen(Port, Owner) ->
    Opts = [binary, {packet, line}, {active, false}, {reuseaddr, true}, {ip, {127, 0, 0, 1}}],
    {ok, Listen} = gen_tcp:listen(Port, Opts),
    Owner ! {fake_nats_listening, self()},
    accept_loop(Listen, Owner).

accept_loop(Listen, Owner) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    Handler = spawn_link(fun() -> serve(Owner) end),
    ok = gen_tcp:controlling_process(Socket, Handler),
    Handler ! {socket, Socket},
    accept_loop(Listen, Owner).

serve(Owner) ->
    receive
        {socket, Socket} ->
            ok = gen_tcp:send(Socket, ?INFO),
            serve_loop(Socket, Owner)
    end.

serve_loop(Socket, Owner) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Line} ->
            handle_line(Socket, Owner, Line),
            serve_loop(Socket, Owner);
        {error, _Reason} ->
            ok
    end.

handle_line(Socket, _Owner, <<"PING", _/binary>>) ->
    gen_tcp:send(Socket, <<"PONG\r\n">>);
handle_line(_Socket, Owner, <<"SUB ", Rest/binary>>) ->
    [Subject | _] = binary:split(Rest, <<" ">>),
    Owner ! {fake_nats_sub, Subject},
    ok;
handle_line(_Socket, _Owner, _Line) ->
    ok.

await_subs(_Subject, 0, _Timeout) ->
    ok;
await_subs(Subject, Count, Timeout) ->
    receive
        {fake_nats_sub, Subject} -> await_subs(Subject, Count - 1, Timeout)
    after Timeout ->
        {missing, Subject, Count}
    end.

wait_until(Check, Timeout) when Timeout =< 0 ->
    {timeout, Check()};
wait_until(Check, Timeout) ->
    case Check() of
        true ->
            ok;
        false ->
            timer:sleep(50),
            wait_until(Check, Timeout - 50)
    end.

receive_down(MRef, Conn) ->
    receive
        {'DOWN', MRef, process, Conn, _} = Down -> Down
    after 3000 ->
        error(no_down_message)
    end.

receive_message(Message, Timeout) ->
    receive_message(Message, Timeout, Message).

receive_message(Message, Timeout, Result) ->
    receive
        Message -> Result
    after Timeout ->
        {timeout, Message}
    end.

flush_conn(Conn) ->
    receive
        {Conn, _} -> flush_conn(Conn)
    after 100 ->
        ok
    end.

flush_subs() ->
    receive
        {fake_nats_sub, _Subject} -> flush_subs()
    after 0 ->
        ok
    end.

flush() ->
    receive
        _ -> flush()
    after 0 ->
        ok
    end.

-endif.
