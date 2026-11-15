%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_slot_release_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MAX_CONCURRENCY, 4).
-define(RECV_TIMEOUT_MS, 1000).
-define(MAX_SESSIONS_PER_USER, 100).
-define(VOICE_GUILD_ID, 1539791936896897026).
-define(VOICE_TIMEOUT_MS, 500).

rpc_slots_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(Ctx) ->
        [
            {"killed caller releases its slot",
                {timeout, 30, fun() ->
                    killed_caller_releases_its_slot(Ctx)
                end}},
            {"linked exit releases the slot",
                {timeout, 30, fun() ->
                    linked_exit_releases_the_slot(Ctx)
                end}},
            {"capacity returns after callers die",
                {timeout, 30, fun() ->
                    capacity_returns_after_callers_die(Ctx)
                end}},
            {"killed worker releases its slot",
                {timeout, 30, fun() ->
                    killed_worker_releases_its_slot(Ctx)
                end}},
            {"prune frees slots of purged workers",
                {timeout, 30, fun() ->
                    prune_frees_slots_of_purged_workers(Ctx)
                end}}
        ]
    end}.

killed_caller_releases_its_slot(#{blackhole := Blackhole}) ->
    Caller = spawn(fun() -> hanging_request(Blackhole) end),
    wait_until(fun() -> inflight() =:= 1 end),
    kill_and_wait(Caller),
    wait_until(fun() -> inflight() =:= 0 end).

linked_exit_releases_the_slot(#{blackhole := Blackhole}) ->
    Owner = spawn(fun() ->
        _ = spawn_link(fun() -> hanging_request(Blackhole) end),
        receive
            close -> exit(socket_closed)
        end
    end),
    wait_until(fun() -> inflight() =:= 1 end),
    MRef = monitor(process, Owner),
    Owner ! close,
    receive
        {'DOWN', MRef, process, Owner, socket_closed} -> ok
    end,
    wait_until(fun() -> inflight() =:= 0 end).

capacity_returns_after_callers_die(#{blackhole := Blackhole, responder := Responder}) ->
    Callers = [
        spawn(fun() -> hanging_request(Blackhole) end)
     || _ <- lists:seq(1, ?MAX_CONCURRENCY)
    ],
    wait_until(fun() -> inflight() =:= ?MAX_CONCURRENCY end),
    ?assertEqual({error, overloaded}, request(Responder)),
    lists:foreach(fun kill_and_wait/1, Callers),
    wait_until(fun() -> inflight() =:= 0 end),
    ?assertMatch({ok, 200, _, _}, request(Responder)).

killed_worker_releases_its_slot(#{blackhole := Blackhole}) ->
    Caller = spawn(fun() -> hanging_request(Blackhole) end),
    wait_until(fun() -> inflight() =:= 1 end),
    [Worker] = inflight_workers(),
    kill_and_wait(Worker),
    wait_until(fun() -> inflight() =:= 0 end),
    kill_and_wait(Caller).

prune_frees_slots_of_purged_workers(#{blackhole := Blackhole}) ->
    Callers = [
        spawn(fun() -> hanging_request(Blackhole) end)
     || _ <- lists:seq(1, ?MAX_CONCURRENCY)
    ],
    wait_until(fun() -> inflight() =:= ?MAX_CONCURRENCY end),
    Workers = inflight_workers(),
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Callers ++ Workers),
    lists:foreach(fun wait_dead/1, Callers ++ Workers),
    ?assertEqual(?MAX_CONCURRENCY, inflight()),
    ok = gateway_http_client_response:prune_inflight_slots(),
    ?assertEqual(0, inflight()).

voice_slots_test_() ->
    {setup, fun voice_setup/0, fun voice_teardown/1, fun(Ctx) ->
        [
            {"killed caller releases its voice slot",
                {timeout, 30, fun() ->
                    killed_caller_releases_its_voice_slot(Ctx)
                end}},
            {"prune frees voice slots of purged workers",
                {timeout, 30, fun() ->
                    prune_frees_voice_slots_of_purged_workers(Ctx)
                end}}
        ]
    end}.

killed_caller_releases_its_voice_slot(#{voice := Voice}) ->
    Caller = spawn(fun() -> voice_request() end),
    wait_until(fun() -> voice_slots(Voice) =:= 1 end),
    kill_and_wait(Caller),
    wait_until(fun() -> voice_slots(Voice) =:= 0 end).

prune_frees_voice_slots_of_purged_workers(#{voice := Voice}) ->
    Caller = spawn(fun() -> voice_request() end),
    wait_until(fun() -> voice_slots(Voice) =:= 1 end),
    [Worker] = voice_slot_workers(Voice),
    exit(Caller, kill),
    exit(Worker, kill),
    wait_dead(Caller),
    wait_dead(Worker),
    ?assertEqual(1, voice_slots(Voice)),
    ok = guild_client_circuit_breaker:prune_dead(),
    ?assertEqual(0, voice_slots(Voice)).

voice_setup() ->
    {ok, Owner} = guild_ets_owner:start_link(),
    unlink(Owner),
    Voice = spawn(fun silent_server/0),
    true = ets:insert(guild_voice_registry, {?VOICE_GUILD_ID, Voice}),
    #{owner => Owner, voice => Voice}.

voice_teardown(#{owner := Owner, voice := Voice}) ->
    exit(Voice, kill),
    kill_and_wait(Owner).

silent_server() ->
    receive
        _ -> silent_server()
    end.

voice_request() ->
    guild_client:voice_state_update(
        spawn(fun silent_server/0), ?VOICE_GUILD_ID, #{}, ?VOICE_TIMEOUT_MS
    ).

voice_slots(Voice) ->
    length(voice_slot_workers(Voice)).

voice_slot_workers(Voice) ->
    ets:select(guild_circuit_breaker_slots, [{{{Voice, '$1'}}, [], ['$1']}]).

at_capacity_guild_connect_is_retried_test() ->
    GuildId = 1539791936896897027,
    Attempt = 2,
    WorkerRef = make_ref(),
    State = #{
        guild_connect_inflight => #{GuildId => Attempt},
        guild_connect_workers => #{WorkerRef => {GuildId, Attempt, self()}},
        guild_connect_timers => #{}
    },
    {guild_connect_worker, {noreply, State1}} =
        session_connection_guild:handle_guild_connect_worker_down(
            WorkerRef, guild_start_at_capacity, State
        ),
    ?assertEqual(#{}, maps:get(guild_connect_inflight, State1)),
    ?assertEqual(#{}, maps:get(guild_connect_workers, State1)),
    receive
        {guild_connect, GuildId, Attempt} -> ok
    after 1000 ->
        ?assert(false)
    end.

guild_start_slot_follows_its_holder_test_() ->
    {timeout, 30, fun guild_start_slot_follows_its_holder/0}.

guild_start_slot_follows_its_holder() ->
    {ok, Server} = gateway_concurrency:start_link(),
    unlink(Server),
    Parent = self(),
    Holders = [
        spawn(fun() ->
            Parent ! {acquired, self(), gateway_concurrency:try_acquire_guild_start()},
            receive
                stop -> ok
            end
        end)
     || _ <- lists:seq(1, 3)
    ],
    [
        receive
            {acquired, Pid, Result} -> ?assertEqual(ok, Result)
        end
     || Pid <- Holders
    ],
    ?assertEqual(3, gateway_concurrency:guild_start_count()),
    lists:foreach(fun kill_and_wait/1, Holders),
    wait_until(fun() -> gateway_concurrency:guild_start_count() =:= 0 end),
    kill_and_wait(Server).

killed_sessions_stop_counting_test() ->
    UserId = 1539791936896897025,
    ?assertEqual(ok, session_abuse_protection:reserve_user_session(UserId)),
    ok = session_abuse_protection:remove_user_session(UserId),
    Sessions = [spawn_session(UserId) || _ <- lists:seq(1, ?MAX_SESSIONS_PER_USER)],
    ?assertEqual(
        {error, too_many_sessions}, session_abuse_protection:reserve_user_session(UserId)
    ),
    lists:foreach(fun kill_and_wait/1, Sessions),
    ?assertEqual(ok, session_abuse_protection:reserve_user_session(UserId)),
    ok = session_abuse_protection:remove_user_session(UserId).

spawn_session(UserId) ->
    Parent = self(),
    Pid = spawn(fun() ->
        ok = session_abuse_protection:add_user_session(UserId),
        Parent ! {added, self()},
        receive
            stop -> ok
        end
    end),
    receive
        {added, Pid} -> Pid
    end.

setup() ->
    {ok, _} = application:ensure_all_started(inets),
    Blackhole = start_server(blackhole),
    Responder = start_server(responder),
    persistent_term:put({fluxer_gateway, runtime_config}, #{
        gateway_http_rpc_max_concurrency => ?MAX_CONCURRENCY
    }),
    {ok, _} = gateway_http_client:start_link(),
    unlink(whereis(gateway_http_client)),
    #{blackhole => Blackhole, responder => Responder}.

teardown(#{blackhole := Blackhole, responder := Responder}) ->
    exit(maps:get(acceptor, Blackhole), kill),
    exit(maps:get(acceptor, Responder), kill),
    persistent_term:erase({fluxer_gateway, runtime_config}),
    ok.

hanging_request(Server) ->
    request(Server).

request(Server) ->
    gateway_http_client:request(
        rpc, post, endpoint(Server), [], <<"{}">>, #{recv_timeout => ?RECV_TIMEOUT_MS}
    ).

kill_and_wait(Pid) ->
    MRef = monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    end.

inflight() ->
    ets:info(gateway_http_inflight, size).

inflight_workers() ->
    ets:select(gateway_http_inflight, [{{'$1'}, [], ['$1']}]).

wait_dead(Pid) ->
    wait_until(fun() -> not is_process_alive(Pid) end).

wait_until(Fun) ->
    wait_until(Fun, 100).

wait_until(Fun, 0) ->
    ?assert(Fun());
wait_until(Fun, Attempts) ->
    case Fun() of
        true ->
            ok;
        false ->
            timer:sleep(50),
            wait_until(Fun, Attempts - 1)
    end.

endpoint(#{port := Port}) ->
    iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port), "/internal/rpc"]).

start_server(Mode) ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(Listen),
    Acceptor = spawn(fun() -> accept_loop(Mode, Listen, []) end),
    ok = gen_tcp:controlling_process(Listen, Acceptor),
    #{port => Port, acceptor => Acceptor}.

accept_loop(Mode, Listen, Held) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            serve(Mode, Socket),
            accept_loop(Mode, Listen, [Socket | Held]);
        {error, _} ->
            ok
    end.

serve(blackhole, _Socket) ->
    ok;
serve(responder, Socket) ->
    Handler = spawn(fun() -> respond_loop(Socket) end),
    ok = gen_tcp:controlling_process(Socket, Handler).

respond_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, _Request} ->
            ok = gen_tcp:send(Socket, <<"HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\n{}">>),
            respond_loop(Socket);
        {error, _} ->
            ok
    end.
