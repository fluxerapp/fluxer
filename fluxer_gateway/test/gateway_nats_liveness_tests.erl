%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_nats_liveness_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CONFIG_TERM_KEY, {fluxer_gateway, runtime_config}).
-define(RECONNECT_WAIT_MS, 30000).
-define(STEADY_WAIT_MS, 25000).

liveness_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun(Ctx) ->
            {"rpc connection reconnects when the server goes silent",
                {timeout, 60, fun() -> rpc_reconnects_after_silence(Ctx) end}}
        end,
        fun(Ctx) ->
            {"pool slot reconnects when the server goes silent",
                {timeout, 60, fun() -> pool_reconnects_after_silence(Ctx) end}}
        end,
        fun(Ctx) ->
            {"rpc connection stays up while the server answers",
                {timeout, 60, fun() -> rpc_stays_on_responsive_server(Ctx) end}}
        end
    ]}.

setup() ->
    OldConfig = persistent_term:get(?CONFIG_TERM_KEY, undefined),
    #{old_config => OldConfig}.

teardown(#{old_config := OldConfig}) ->
    stop_registered(gateway_nats_rpc),
    stop_registered(gateway_nats_pool),
    case OldConfig of
        undefined -> persistent_term:erase(?CONFIG_TERM_KEY);
        _ -> persistent_term:put(?CONFIG_TERM_KEY, OldConfig)
    end,
    ok.

rpc_reconnects_after_silence(_Ctx) ->
    Server = start_fake_server(silent),
    use_server(Server),
    {ok, _} = gateway_nats_rpc:start_link(),
    ?assert(wait_for_connections(Server, 1, ?RECONNECT_WAIT_MS)),
    ?assert(wait_for_connections(Server, 2, ?RECONNECT_WAIT_MS)).

pool_reconnects_after_silence(_Ctx) ->
    Server = start_fake_server(silent),
    use_server(Server),
    {ok, _} = gateway_nats_pool:start_link(),
    ?assert(wait_for_connections(Server, 1, ?RECONNECT_WAIT_MS)),
    ?assert(wait_for_connections(Server, 2, ?RECONNECT_WAIT_MS)).

rpc_stays_on_responsive_server(_Ctx) ->
    Server = start_fake_server(responsive),
    use_server(Server),
    {ok, _} = gateway_nats_rpc:start_link(),
    ?assert(wait_for_connections(Server, 1, ?RECONNECT_WAIT_MS)),
    ?assertNot(wait_for_connections(Server, 2, ?STEADY_WAIT_MS)).

use_server(#{port := Port}) ->
    Url = iolist_to_binary(["nats://127.0.0.1:", integer_to_binary(Port)]),
    persistent_term:put(?CONFIG_TERM_KEY, #{
        nats_core_url => Url,
        nats_rpc_enabled => false,
        nats_pool_size => 1,
        gateway_role => all
    }).

stop_registered(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            unlink(Pid),
            MRef = monitor(process, Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', MRef, process, Pid, _} -> ok
            after 5000 -> exit(Pid, kill)
            end
    end.

wait_for_connections(#{counter := Counter} = Server, Wanted, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    wait_for_connections(Server, Counter, Wanted, Deadline).

wait_for_connections(Server, Counter, Wanted, Deadline) ->
    case counters:get(Counter, 1) >= Wanted of
        true ->
            true;
        false ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true ->
                    false;
                false ->
                    timer:sleep(100),
                    wait_for_connections(Server, Counter, Wanted, Deadline)
            end
    end.

start_fake_server(Mode) ->
    {ok, Listen} = gen_tcp:listen(0, [
        binary, {ip, {127, 0, 0, 1}}, {active, false}, {packet, line}, {reuseaddr, true}
    ]),
    {ok, Port} = inet:port(Listen),
    Counter = counters:new(1, []),
    Acceptor = spawn(fun() -> accept_loop(Listen, Mode, Counter) end),
    ok = gen_tcp:controlling_process(Listen, Acceptor),
    #{port => Port, counter => Counter}.

accept_loop(Listen, Mode, Counter) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            counters:add(Counter, 1, 1),
            Handler = spawn(fun() -> serve_client(Socket, Mode) end),
            ok = gen_tcp:controlling_process(Socket, Handler),
            Handler ! go,
            accept_loop(Listen, Mode, Counter);
        {error, _} ->
            ok
    end.

serve_client(Socket, Mode) ->
    receive
        go -> ok
    end,
    Info = <<"INFO {\"server_id\":\"fake\",\"version\":\"2.10.0\",\"headers\":true,",
        "\"max_payload\":1048576}\r\n">>,
    ok = gen_tcp:send(Socket, Info),
    client_loop(Socket, Mode, #{}).

client_loop(Socket, Mode, Subs) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Line} -> client_loop(Socket, Mode, handle_line(Socket, Mode, Line, Subs));
        {error, _} -> ok
    end.

handle_line(_Socket, silent, _Line, Subs) ->
    Subs;
handle_line(Socket, responsive, Line, Subs) ->
    case binary:split(string:trim(Line), <<" ">>, [global, trim_all]) of
        [<<"PING">>] ->
            ok = gen_tcp:send(Socket, <<"PONG\r\n">>),
            Subs;
        [<<"SUB">>, Subject, Sid] ->
            Subs#{inbox_prefix(Subject) => Sid};
        [<<"PUB">>, _Subject, ReplyTo, Size] ->
            {ok, _Payload} = read_payload(Socket, binary_to_integer(Size)),
            answer_no_responders(Socket, ReplyTo, Subs),
            Subs;
        [<<"HPUB">>, _Subject, ReplyTo, _HeaderSize, TotalSize] ->
            {ok, _Payload} = read_payload(Socket, binary_to_integer(TotalSize)),
            answer_no_responders(Socket, ReplyTo, Subs),
            Subs;
        _ ->
            Subs
    end.

inbox_prefix(Subject) ->
    case binary:split(Subject, <<"*">>) of
        [Prefix, <<>>] -> Prefix;
        _ -> Subject
    end.

read_payload(Socket, Size) ->
    ok = inet:setopts(Socket, [{packet, raw}]),
    Result = gen_tcp:recv(Socket, Size + 2),
    ok = inet:setopts(Socket, [{packet, line}]),
    Result.

answer_no_responders(Socket, ReplyTo, Subs) ->
    Sids = [Sid || Prefix := Sid <- Subs, binary:longest_common_prefix([Prefix, ReplyTo]) =:= byte_size(Prefix)],
    Header = <<"NATS/1.0 503\r\n\r\n">>,
    HeaderSize = integer_to_binary(byte_size(Header)),
    lists:foreach(
        fun(Sid) ->
            ok = gen_tcp:send(Socket, [
                <<"HMSG ">>, ReplyTo, <<" ">>, Sid, <<" ">>, HeaderSize, <<" ">>, HeaderSize,
                <<"\r\n">>, Header, <<"\r\n">>
            ])
        end,
        Sids
    ).
