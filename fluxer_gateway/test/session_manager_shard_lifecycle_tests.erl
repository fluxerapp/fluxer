%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_manager_shard_lifecycle_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

build_test_state(Overrides) ->
    Base = #{
        sessions => #{},
        identify_attempts => [],
        pending_identifies => #{},
        identify_workers => #{},
        shard_index => 0
    },
    maps:merge(Base, Overrides).

build_pending_identify(Overrides) ->
    Base = #{
        request => #{},
        socket_pid => self(),
        froms => [],
        worker_ref => make_ref(),
        worker_token => make_ref(),
        timeout_ref => make_ref()
    },
    maps:merge(Base, Overrides).

spawn_test_proc() -> spawn(fun test_proc_loop/0).
test_proc_loop() ->
    receive
        _ -> ok
    after 30000 -> ok
    end.

-spec test_from() -> gen_server:from().
test_from() ->
    {self(), make_ref()}.

handle_start_call_rejects_during_drain_test() ->
    State = build_test_state(#{}),
    ?assertEqual(
        {reply, {error, draining}, State},
        session_manager_shard_lifecycle:handle_start_call(true, #{}, self(), test_from(), State)
    ).

identify_fetch_timeout_replies_and_cleans_worker_test() ->
    SessionId = <<"identify-timeout-session">>,
    WorkerPid = spawn_test_proc(),
    WorkerRef = monitor(process, WorkerPid),
    ReplyRef = make_ref(),
    TimeoutRef = erlang:send_after(5000, self(), stale_timeout),
    Pending = build_pending_identify(#{
        froms => [{self(), ReplyRef}], worker_ref => WorkerRef, timeout_ref => TimeoutRef
    }),
    State0 = build_test_state(#{
        pending_identifies => #{SessionId => Pending},
        identify_workers => #{WorkerRef => {SessionId, WorkerPid}}
    }),
    {noreply, State1} = session_manager_shard_lifecycle:handle_identify_fetch_timeout(
        SessionId, WorkerRef, State0
    ),
    ?assertEqual(#{}, maps:get(pending_identifies, State1)),
    ?assertEqual(#{}, maps:get(identify_workers, State1)),
    assert_receive({ReplyRef, {error, timeout}}),
    assert_no_receive(stale_timeout).

complete_identify_fetch_cleans_pending_on_error_test() ->
    SessionId = <<"complete-error-session">>,
    ReplyRef = make_ref(),
    WorkerRef = make_ref(),
    Pending = build_pending_identify(#{
        froms => [{self(), ReplyRef}], worker_ref => WorkerRef
    }),
    Token = maps:get(worker_token, Pending),
    State0 = build_test_state(#{
        pending_identifies => #{SessionId => Pending},
        identify_workers => #{WorkerRef => {SessionId, self()}}
    }),
    {noreply, State1} = session_manager_shard_lifecycle:complete_identify_fetch(
        SessionId, Token, {error, some_rpc_error}, State0
    ),
    ?assertEqual(#{}, maps:get(pending_identifies, State1)),
    ?assertEqual(#{}, maps:get(identify_workers, State1)),
    assert_receive({ReplyRef, {error, some_rpc_error}}).

complete_identify_fetch_accepts_wrapped_timing_result_test() ->
    SessionId = <<"complete-error-with-timings-session">>,
    ReplyRef = make_ref(),
    WorkerRef = make_ref(),
    Pending = build_pending_identify(#{
        request => #{gw_timings => gateway_timings:new()},
        froms => [{self(), ReplyRef}],
        worker_ref => WorkerRef
    }),
    Token = maps:get(worker_token, Pending),
    State0 = build_test_state(#{
        pending_identifies => #{SessionId => Pending},
        identify_workers => #{WorkerRef => {SessionId, self()}}
    }),
    WrappedResult = {{error, some_rpc_error}, gateway_timings:new()},
    {noreply, State1} = session_manager_shard_lifecycle:complete_identify_fetch(
        SessionId, Token, WrappedResult, State0
    ),
    ?assertEqual(#{}, maps:get(pending_identifies, State1)),
    ?assertEqual(#{}, maps:get(identify_workers, State1)),
    assert_receive({ReplyRef, {error, some_rpc_error}}).

stale_identify_fetch_result_ignores_wrong_token_test() ->
    SessionId = <<"stale-identify-result-session">>,
    ReplyRef = make_ref(),
    WorkerRef = make_ref(),
    Pending = build_pending_identify(#{
        froms => [{self(), ReplyRef}], worker_ref => WorkerRef, worker_token => make_ref()
    }),
    State0 = build_test_state(#{
        pending_identifies => #{SessionId => Pending},
        identify_workers => #{WorkerRef => {SessionId, self()}}
    }),
    {noreply, State1} = session_manager_shard_lifecycle:complete_identify_fetch(
        SessionId, make_ref(), {error, invalid_token}, State0
    ),
    ?assertEqual(State0, State1),
    assert_no_receive({ReplyRef, '_'}).

malformed_identify_payload_returns_error_test() ->
    State0 = build_test_state(#{}),
    Expected = {reply, {error, {invalid_identify_payload, missing_user}}, State0},
    ?assertEqual(
        Expected,
        session_manager_shard_lifecycle:build_and_start_session(
            #{}, #{}, 1, self(), <<"bad">>, #{}, State0, gateway_timings:new()
        )
    ).

assert_receive(Pattern) ->
    receive
        Pattern -> ok
    after 200 -> ?assert(false)
    end.

assert_no_receive(Pattern) ->
    receive
        Pattern -> ?assert(false)
    after 100 -> ok
    end.
