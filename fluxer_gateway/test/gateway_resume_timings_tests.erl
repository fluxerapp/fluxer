%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_resume_timings_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

handle_resume_omits_gateway_timings_for_non_staff_test() ->
    drain_mailbox(),
    SessionPid = spawn(fun() -> fake_resume_session_loop(<<"resume-token">>, 12, false) end),
    meck:new(session_manager, [passthrough, no_link]),
    meck:new(rpc_client, [passthrough, no_link]),
    meck:expect(
        session_manager,
        lookup_or_rehydrate,
        fun(<<"session-non-staff">>, <<"resume-token">>, SocketPid) when is_pid(SocketPid) ->
            {ok, SessionPid}
        end
    ),
    meck:expect(rpc_client, call, fun(_Request) -> error(unexpected_resume_api_call) end),
    try
        State0 = maps:remove(peer_ip, new_json_state()),
        {ok, State1} = gateway_handler_identify:handle_resume(
            #{
                <<"token">> => <<"resume-token">>,
                <<"session_id">> => <<"session-non-staff">>,
                <<"seq">> => 7
            },
            State0
        ),
        ?assertEqual(SessionPid, maps:get(session_pid, State1)),
        assert_resumed_dispatch_without_timings(12)
    after
        SessionPid ! stop,
        meck:unload(rpc_client),
        meck:unload(session_manager)
    end.

handle_resume_missing_session_sends_invalid_session_without_api_test() ->
    drain_mailbox(),
    meck:new(session_manager, [passthrough, no_link]),
    meck:expect(
        session_manager,
        lookup_or_rehydrate,
        fun(<<"missing-session">>, <<"resume-token">>, SocketPid) when is_pid(SocketPid) ->
            {error, not_found}
        end
    ),
    try
        {Frames, _State1} = gateway_handler_identify:handle_resume(
            #{
                <<"token">> => <<"resume-token">>,
                <<"session_id">> => <<"missing-session">>,
                <<"seq">> => 7
            },
            new_json_state()
        ),
        ?assertMatch([_Frame], Frames)
    after
        meck:unload(session_manager)
    end.

handle_resume_below_replay_floor_sends_invalid_session_test() ->
    drain_mailbox(),
    SessionPid = spawn(fun() -> fake_not_resumable_session_loop(<<"resume-token">>) end),
    meck:new(session_manager, [passthrough, no_link]),
    meck:expect(
        session_manager,
        lookup_or_rehydrate,
        fun(<<"evicted-session">>, <<"resume-token">>, SocketPid) when is_pid(SocketPid) ->
            {ok, SessionPid}
        end
    ),
    try
        {Frames, _State1} = gateway_handler_identify:handle_resume(
            #{
                <<"token">> => <<"resume-token">>,
                <<"session_id">> => <<"evicted-session">>,
                <<"seq">> => 7
            },
            new_json_state()
        ),
        [{text, Payload}] = Frames,
        Message = json:decode(Payload),
        ?assertEqual(constants:opcode_to_num(invalid_session), maps:get(<<"op">>, Message)),
        ?assertEqual(false, maps:get(<<"d">>, Message))
    after
        SessionPid ! stop,
        meck:unload(session_manager)
    end.

fake_not_resumable_session_loop(Token) ->
    receive
        {'$gen_call', From, {token_verify, Candidate}} ->
            gen_server:reply(From, Candidate =:= Token),
            fake_not_resumable_session_loop(Token);
        {'$gen_call', From, {resume, _Seq, SocketPid}} when is_pid(SocketPid) ->
            gen_server:reply(From, not_resumable),
            fake_not_resumable_session_loop(Token);
        stop ->
            ok
    after 30000 ->
        ok
    end.

assert_resumed_dispatch_without_timings(ExpectedSeq) ->
    receive
        {dispatch, resumed, ResumedData, ExpectedSeq} ->
            ?assertNot(maps:is_key(<<"_timings_gw">>, ResumedData))
    after 1000 ->
        ?assert(false, resumed_not_dispatched)
    end.

fake_resume_session_loop(Token, CurrentSeq, IsStaff) ->
    receive
        {'$gen_call', From, {token_verify, Candidate}} ->
            gen_server:reply(From, Candidate =:= Token),
            fake_resume_session_loop(Token, CurrentSeq, IsStaff);
        {'$gen_call', From, {resume, _Seq, SocketPid}} when is_pid(SocketPid) ->
            gen_server:reply(From, {ok, [], CurrentSeq}),
            fake_resume_session_loop(Token, CurrentSeq, IsStaff);
        {'$gen_call', From, {is_staff}} ->
            gen_server:reply(From, IsStaff),
            fake_resume_session_loop(Token, CurrentSeq, IsStaff);
        stop ->
            ok
    after 30000 ->
        ok
    end.

drain_mailbox() ->
    receive
        _ -> drain_mailbox()
    after 0 ->
        ok
    end.

new_json_state() ->
    (gateway_handler:new_state())#{
        version => 1,
        encoding => json,
        compress_ctx => gateway_compress:new_context(none),
        peer_ip => <<"127.0.0.1">>
    }.
