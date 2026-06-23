%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_handler_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_GATEWAY_RATE_LIMIT_MAX_EVENTS, 600).
-define(TEST_IDENTIFY_MAX_PER_IP, 300).

websocket_info_session_reconnect_sends_reconnect_then_close_test() ->
    assert_reconnect_close(
        gateway_handler:websocket_info(session_reconnect, new_json_state()),
        <<"Session drain requested; reconnect to continue">>
    ).

start_session_with_drain_guard_holds_during_drain_test() ->
    Request = #{},
    assert_pending_identify(
        gateway_handler_identify:start_session_with_drain_guard(
            true, Request, self(), new_json_state()
        ),
        Request,
        self()
    ).

handle_session_start_result_holds_draining_without_context_test() ->
    assert_held_without_pending(
        gateway_handler_identify:handle_session_start_result(
            {error, draining}, new_json_state()
        )
    ).

handle_session_start_result_holds_not_eligible_without_context_test() ->
    assert_held_without_pending(
        gateway_handler_identify:handle_session_start_result(
            {error, not_eligible}, new_json_state()
        )
    ).

start_session_zero_rollout_holds_socket_test() ->
    OldConfig = gateway_rollout_config:get(),
    try
        persistent_term:put(gateway_rollout_config, OldConfig#{
            <<"session_rollout_percentage">> => 0
        }),
        Request = #{},
        assert_pending_identify(
            gateway_handler_identify:start_session(Request, self(), new_json_state()),
            Request,
            self()
        )
    after
        persistent_term:put(gateway_rollout_config, OldConfig)
    end.

handle_session_start_result_retries_exhausted_holds_without_context_test() ->
    State = (gateway_handler:new_state())#{
        version => 1, encoding => json, compress_ctx => gateway_compress:new_context(none)
    },
    assert_held_without_pending(
        gateway_handler_identify:handle_session_start_result(
            {error, {retries_exhausted, timeout}}, State
        )
    ).

handle_session_start_result_retries_exhausted_5xx_holds_without_context_test() ->
    State = (gateway_handler:new_state())#{
        version => 1, encoding => json, compress_ctx => gateway_compress:new_context(none)
    },
    assert_held_without_pending(
        gateway_handler_identify:handle_session_start_result(
            {error, {retries_exhausted, {rpc_error, 503, <<"Service unavailable">>}}}, State
        )
    ).

handle_session_start_result_server_error_closes_with_unknown_error_test() ->
    State = (gateway_handler:new_state())#{
        version => 1, encoding => json, compress_ctx => gateway_compress:new_context(none)
    },
    {[{close, CloseCode, _Reason}], _NewState} =
        gateway_handler_identify:handle_session_start_result(
            {error, {server_error, 500}}, State
        ),
    ?assertEqual(constants:close_code_to_num(unknown_error), CloseCode).

websocket_init_schedules_tokened_heartbeat_timer_test() ->
    {[{text, _Frame}], State} = gateway_handler:websocket_init(new_json_state()),
    ?assertMatch({_TimerRef, _Token}, maps:get(heartbeat_timer, State)),
    gateway_handler_heartbeat:cancel_heartbeat_timer(State).

heartbeat_check_ignores_stale_token_test() ->
    State0 = heartbeat_test_state(),
    State1 = gateway_handler_heartbeat:schedule_heartbeat_check(State0),
    {TimerRef, Token} = maps:get(heartbeat_timer, State1),
    {ok, State2} = gateway_handler:websocket_info({heartbeat_check, make_ref()}, State1),
    ?assertEqual({TimerRef, Token}, maps:get(heartbeat_timer, State2)),
    gateway_handler_heartbeat:cancel_heartbeat_timer(State2).

heartbeat_check_replaces_matched_timer_test() ->
    State0 = heartbeat_test_state(),
    State1 = gateway_handler_heartbeat:schedule_heartbeat_check(State0),
    {TimerRef, Token} = maps:get(heartbeat_timer, State1),
    {ok, State2} = gateway_handler:websocket_info({heartbeat_check, Token}, State1),
    ?assertMatch({_NewTimerRef, _NewToken}, maps:get(heartbeat_timer, State2)),
    ?assertNotEqual({TimerRef, Token}, maps:get(heartbeat_timer, State2)),
    gateway_handler_heartbeat:cancel_heartbeat_timer(State2).

legacy_heartbeat_check_ignored_when_tokened_timer_active_test() ->
    State0 = heartbeat_test_state(),
    State1 = gateway_handler_heartbeat:schedule_heartbeat_check(State0),
    {TimerRef, Token} = maps:get(heartbeat_timer, State1),
    {ok, State2} = gateway_handler:websocket_info({heartbeat_check}, State1),
    ?assertEqual({TimerRef, Token}, maps:get(heartbeat_timer, State2)),
    gateway_handler_heartbeat:cancel_heartbeat_timer(State2).

pending_identify_retry_ignores_stale_token_test() ->
    Timer = erlang:send_after(60000, self(), stale_pending_identify_retry),
    CurrentToken = make_ref(),
    State0 = (new_json_state())#{
        pending_identify => {#{token => <<"t">>}, self()},
        pending_identify_retry_timer => {Timer, CurrentToken}
    },
    {ok, State1} = gateway_handler:websocket_info(
        {retry_pending_identify, make_ref()}, State0
    ),
    ?assertEqual(State0, State1),
    cancel_pending_identify_timer(State1).

legacy_pending_identify_retry_ignored_when_tokened_timer_active_test() ->
    Timer = erlang:send_after(60000, self(), stale_pending_identify_retry),
    CurrentToken = make_ref(),
    State0 = (new_json_state())#{
        pending_identify => {#{token => <<"t">>}, self()},
        pending_identify_retry_timer => {Timer, CurrentToken}
    },
    {ok, State1} = gateway_handler:websocket_info(retry_pending_identify, State0),
    ?assertEqual(State0, State1),
    cancel_pending_identify_timer(State1).

parse_forwarded_for_ipv4_test() ->
    ?assertEqual(<<"203.0.113.7">>, gateway_handler:parse_forwarded_for(<<"203.0.113.7">>)).
parse_forwarded_for_ipv4_with_port_test() ->
    ?assertEqual(
        <<"203.0.113.7">>, gateway_handler:parse_forwarded_for(<<"203.0.113.7:8080">>)
    ).
parse_forwarded_for_ipv4_with_port_and_extra_entries_test() ->
    ?assertEqual(
        <<"203.0.113.7">>,
        gateway_handler:parse_forwarded_for(<<" 203.0.113.7:8080 , 10.0.0.1">>)
    ).
parse_forwarded_for_ipv6_test() ->
    ?assertEqual(<<"2001:db8::1">>, gateway_handler:parse_forwarded_for(<<"2001:db8::1">>)).
parse_forwarded_for_ipv6_with_brackets_test() ->
    ?assertEqual(<<"2001:db8::1">>, gateway_handler:parse_forwarded_for(<<"[2001:db8::1]">>)).
parse_forwarded_for_ipv6_with_brackets_and_port_test() ->
    ?assertEqual(
        <<"2001:db8::1">>, gateway_handler:parse_forwarded_for(<<"[2001:db8::1]:443">>)
    ).
parse_forwarded_for_ipv6_with_spaces_test() ->
    ?assertEqual(
        <<"2001:db8::1">>, gateway_handler:parse_forwarded_for(<<"  [2001:db8::1]  ">>)
    ).
parse_forwarded_for_invalid_ip_test() ->
    ?assertEqual(<<>>, gateway_handler:parse_forwarded_for(<<"not_an_ip">>)).
parse_forwarded_for_invalid_ipv4_octet_test() ->
    ?assertEqual(<<>>, gateway_handler:parse_forwarded_for(<<"203.0.113.300">>)).
parse_forwarded_for_unterminated_bracket_test() ->
    ?assertEqual(<<>>, gateway_handler:parse_forwarded_for(<<"[2001:db8::1">>)).
parse_version_test() ->
    ?assertEqual(1, gateway_handler:parse_version(<<"1">>)),
    ?assertEqual(undefined, gateway_handler:parse_version(<<"2">>)),
    ?assertEqual(undefined, gateway_handler:parse_version(undefined)).

parse_ignored_events_test() ->
    ?assertEqual({ok, []}, gateway_handler_identify:parse_ignored_events(undefined)),
    ?assertEqual({ok, []}, gateway_handler_identify:parse_ignored_events(null)),
    ?assertEqual(
        {ok, [<<"TYPING_START">>]},
        gateway_handler_identify:parse_ignored_events([<<"typing_start">>])
    ),
    ?assertEqual(
        {error, invalid_ignored_events}, gateway_handler_identify:parse_ignored_events([123])
    ),
    ?assertEqual(
        {error, invalid_ignored_events},
        gateway_handler_identify:parse_ignored_events(<<"not_a_list">>)
    ).

adjust_status_test() ->
    ?assertEqual(invisible, gateway_handler_dispatch:adjust_status(offline)),
    ?assertEqual(online, gateway_handler_dispatch:adjust_status(online)),
    ?assertEqual(idle, gateway_handler_dispatch:adjust_status(idle)).

check_rate_limit_blocks_general_flood_test() ->
    with_rate_limits_enabled(fun() ->
        Now = erlang:system_time(millisecond),
        Events = lists:duplicate(?TEST_GATEWAY_RATE_LIMIT_MAX_EVENTS, Now - 1000),
        State = (gateway_handler:new_state())#{
            rate_limit_state => #{events => Events, op_events => #{}}
        },
        ?assertMatch(
            {rate_limited, _}, gateway_handler_rate_limit:check_rate_limit(State, heartbeat)
        )
    end).

check_presence_rate_limit_soft_drops_only_presence_test() ->
    with_rate_limits_enabled(fun() ->
        Now = erlang:system_time(millisecond),
        Events = lists:duplicate(5, Now - 1000),
        State = (gateway_handler:new_state())#{
            rate_limit_state => #{
                events => [], op_events => #{presence_update => Events}
            }
        },
        {opcode_rate_limited, NewState} = gateway_handler_rate_limit:check_rate_limit(
            State, presence_update
        ),
        RateLimitState = maps:get(rate_limit_state, NewState),
        ?assertEqual(1, length(maps:get(events, RateLimitState)))
    end).

enqueue_voice_update_keeps_latest_update_for_connection_test() ->
    Queue0 = queue:new(),
    Data1 = #{
        <<"guild_id">> => <<"1">>, <<"connection_id">> => <<"conn-1">>, <<"self_mute">> => false
    },
    Data2 = Data1#{<<"self_mute">> => true},
    Queue1 = gateway_handler_voice:enqueue_voice_update(Queue0, Data1),
    Queue2 = gateway_handler_voice:enqueue_voice_update(Queue1, Data2),
    ?assertEqual([Data2], queue:to_list(Queue2)).

enqueue_voice_update_keeps_distinct_connections_test() ->
    Queue0 = queue:new(),
    Data1 = #{
        <<"guild_id">> => <<"1">>, <<"connection_id">> => <<"conn-1">>, <<"self_mute">> => false
    },
    Data2 = Data1#{<<"connection_id">> => <<"conn-2">>, <<"self_mute">> => true},
    Queue1 = gateway_handler_voice:enqueue_voice_update(Queue0, Data1),
    Queue2 = gateway_handler_voice:enqueue_voice_update(Queue1, Data2),
    ?assertEqual([Data1, Data2], queue:to_list(Queue2)).

handle_request_guild_members_queues_latest_request_while_worker_active_test() ->
    ExistingWorkerPid = self(),
    State = (gateway_handler:new_state())#{request_guild_members_pid => ExistingWorkerPid},
    Data = #{<<"guild_ids">> => [<<"1">>, <<"2">>]},
    ?assertMatch(
        {ok, #{request_guild_members_pending := Data}},
        gateway_handler_dispatch:handle_request_guild_members(Data, self(), State)
    ).

handle_request_guild_counts_drops_when_socket_worker_limit_reached_test() ->
    Ref = make_ref(),
    WorkerPid = self(),
    State = (gateway_handler:new_state())#{
        request_worker_max => 1,
        request_workers => #{
            Ref => #{pid => WorkerPid, type => lazy_request, timer => undefined}
        }
    },
    {ok, NewState} = gateway_handler_dispatch:handle_request_guild_counts(
        #{}, self(), eqwalizer:dynamic_cast(State)
    ),
    ?assertEqual(1, maps:size(maps:get(request_workers, NewState))).

request_worker_down_removes_tracked_worker_test() ->
    Ref = make_ref(),
    WorkerPid = self(),
    State = (gateway_handler:new_state())#{
        request_workers => #{
            Ref => #{pid => WorkerPid, type => lazy_request, timer => undefined}
        }
    },
    {ok, NewState} = gateway_handler_dispatch:handle_request_worker_down(
        Ref, WorkerPid, normal, State
    ),
    ?assertEqual(#{}, maps:get(request_workers, NewState)).

request_worker_timeout_removes_and_kills_worker_test() ->
    WorkerPid = spawn(fun request_worker_wait_loop/0),
    Ref = make_ref(),
    State = (gateway_handler:new_state())#{
        request_workers => #{
            Ref => #{pid => WorkerPid, type => lazy_request, timer => undefined}
        }
    },
    {ok, NewState} = gateway_handler_dispatch:handle_request_worker_timeout(
        Ref, lazy_request, State
    ),
    ok = gateway_retry_timer:wait(10),
    ?assertEqual(#{}, maps:get(request_workers, NewState)),
    ?assertNot(erlang:is_process_alive(WorkerPid)).

validate_presence_data_valid_test() ->
    Data = #{<<"status">> => <<"online">>, <<"afk">> => false, <<"mobile">> => false},
    {ok, Result} = gateway_handler_dispatch:validate_presence_data(Data),
    ?assertEqual(online, maps:get(status, Result)).

validate_presence_data_missing_status_test() ->
    ?assertEqual(
        {error, invalid_presence},
        gateway_handler_dispatch:validate_presence_data(#{<<"afk">> => false})
    ).
validate_presence_data_empty_map_test() ->
    ?assertEqual(
        {error, invalid_presence}, gateway_handler_dispatch:validate_presence_data(#{})
    ).
validate_presence_data_not_a_map_test() ->
    ?assertEqual(
        {error, invalid_presence}, gateway_handler_dispatch:validate_presence_data(not_a_map())
    ).
validate_presence_data_invalid_status_string_test() ->
    ?assertEqual(
        {error, invalid_presence},
        gateway_handler_dispatch:validate_presence_data(#{
            <<"status">> => <<"not_a_real_status">>
        })
    ).

validate_presence_data_integer_status_defaults_to_online_test() ->
    {ok, Result} = gateway_handler_dispatch:validate_presence_data(#{<<"status">> => 42}),
    ?assertEqual(online, maps:get(status, Result)).

validate_presence_data_offline_becomes_invisible_test() ->
    {ok, Result} = gateway_handler_dispatch:validate_presence_data(#{
        <<"status">> => <<"offline">>
    }),
    ?assertEqual(invisible, maps:get(status, Result)).

validate_resume_data_valid_test() ->
    ?assertEqual(
        {ok, <<"abc">>, <<"sess1">>, 5},
        gateway_handler_identify:validate_resume_data(#{
            <<"token">> => <<"abc">>, <<"session_id">> => <<"sess1">>, <<"seq">> => 5
        })
    ).
validate_resume_data_missing_token_test() ->
    ?assertEqual(
        {error, missing_required_field},
        gateway_handler_identify:validate_resume_data(#{
            <<"session_id">> => <<"sess1">>, <<"seq">> => 5
        })
    ).
validate_resume_data_empty_map_test() ->
    ?assertEqual(
        {error, missing_required_field}, gateway_handler_identify:validate_resume_data(#{})
    ).
validate_resume_data_not_a_map_test() ->
    ?assertEqual(
        {error, invalid_data}, gateway_handler_identify:validate_resume_data(not_a_map())
    ).

validate_identify_data_empty_map_test() ->
    ?assertEqual(
        {error, missing_required_field}, gateway_handler_identify:validate_identify_data(#{})
    ).
validate_identify_data_non_map_properties_test() ->
    ?assertEqual(
        {error, invalid_properties},
        gateway_handler_identify:validate_identify_data(#{
            <<"token">> => <<"abc">>, <<"properties">> => <<"not_a_map">>
        })
    ).
validate_identify_data_negative_flags_test() ->
    ?assertEqual(
        {error, invalid_properties},
        gateway_handler_identify:validate_identify_data(#{
            <<"token">> => <<"abc">>,
            <<"properties">> => #{
                <<"os">> => <<"linux">>, <<"browser">> => <<"b">>, <<"device">> => <<"d">>
            },
            <<"flags">> => -1
        })
    ).

validate_identify_data_accepts_shard_test() ->
    ?assertMatch(
        {ok, <<"abc">>, _Properties, _Presence, [], 0, undefined, {1, 4}},
        gateway_handler_identify:validate_identify_data(
            valid_identify_data(#{
                <<"shard">> => [1, 4]
            })
        )
    ).

validate_identify_data_accepts_absent_shard_test() ->
    ?assertMatch(
        {ok, <<"abc">>, _Properties, _Presence, [], 0, undefined, undefined},
        gateway_handler_identify:validate_identify_data(valid_identify_data(#{}))
    ).

validate_identify_data_rejects_invalid_shard_test() ->
    ?assertEqual(
        {error, invalid_shard},
        gateway_handler_identify:validate_identify_data(
            valid_identify_data(#{
                <<"shard">> => [2, 2]
            })
        )
    ),
    ?assertEqual(
        {error, invalid_shard},
        gateway_handler_identify:validate_identify_data(
            valid_identify_data(#{
                <<"shard">> => <<"not-a-shard">>
            })
        )
    ).

handle_identify_runs_identify_rate_check_at_zero_rollout_test() ->
    session_abuse_protection:ensure_tables(),
    OldConfig = gateway_rollout_config:get(),
    IP = unique_test_peer_ip(<<"zero-rollout-identify">>),
    try
        persistent_term:put(gateway_rollout_config, OldConfig#{
            <<"session_rollout_percentage">> => 0
        }),
        lists:foreach(
            fun(_) ->
                ?assertEqual(ok, session_abuse_protection:check_identify_rate(IP))
            end,
            lists:seq(1, ?TEST_IDENTIFY_MAX_PER_IP - 1)
        ),
        {ok, HeldState} = gateway_handler_identify:handle_identify(
            valid_identify_data(#{}), IP, new_json_state()
        ),
        cancel_pending_identify_timer(HeldState),
        ?assertEqual(
            {error, identify_rate_limited},
            session_abuse_protection:check_identify_rate(IP)
        )
    after
        persistent_term:put(gateway_rollout_config, OldConfig)
    end.

handle_session_start_result_invalid_shard_closes_4010_test() ->
    {[{close, CloseCode, _Reason}], _NewState} =
        gateway_handler_identify:handle_session_start_result(
            {error, invalid_shard}, new_json_state()
        ),
    ?assertEqual(constants:close_code_to_num(invalid_shard), CloseCode).

handle_session_start_result_sharding_required_closes_4011_test() ->
    {[{close, CloseCode, _Reason}], _NewState} =
        gateway_handler_identify:handle_session_start_result(
            {error, sharding_required}, new_json_state()
        ),
    ?assertEqual(constants:close_code_to_num(sharding_required), CloseCode).

new_json_state() ->
    (gateway_handler:new_state())#{
        version => 1, encoding => json, compress_ctx => gateway_compress:new_context(none)
    }.

heartbeat_test_state() ->
    (new_json_state())#{
        heartbeat_state => #{
            last_ack => erlang:system_time(millisecond),
            waiting_for_ack => false
        }
    }.

valid_identify_data(Extra) ->
    maps:merge(
        #{
            <<"token">> => <<"abc">>,
            <<"properties">> => #{
                <<"os">> => <<"linux">>,
                <<"browser">> => <<"browser">>,
                <<"device">> => <<"device">>
            }
        },
        Extra
    ).

assert_reconnect_close(Result, CloseReason) ->
    {[Frame, {close, CloseCode, CloseReason}], _NewState} = Result,
    ?assertEqual(constants:close_code_to_num(unknown_error), CloseCode),
    {_FrameType, EncodedFrame} = Frame,
    {ok, DecodedFrame} = gateway_codec:decode(EncodedFrame, json),
    ?assertEqual(constants:opcode_to_num(reconnect), maps:get(<<"op">>, DecodedFrame)).

assert_pending_identify(Result, Request, SocketPid) ->
    {ok, State} = Result,
    ?assertEqual({Request, SocketPid}, maps:get(pending_identify, State)),
    ?assertMatch({_TimerRef, _Token}, maps:get(pending_identify_retry_timer, State)),
    cancel_pending_identify_timer(State).

assert_held_without_pending(Result) ->
    {ok, State} = Result,
    ?assertEqual(undefined, maps:get(pending_identify, State, undefined)),
    ?assertEqual(undefined, maps:get(pending_identify_retry_timer, State, undefined)).

not_a_map() ->
    eqwalizer:dynamic_cast(not_a_map).

request_worker_wait_loop() ->
    receive
        stop -> ok
    after 60000 ->
        ok
    end.

cancel_pending_identify_timer(State) ->
    case maps:get(pending_identify_retry_timer, State, undefined) of
        {TimerRef, _Token} when is_reference(TimerRef) ->
            _ = erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
            ok;
        TimerRef when is_reference(TimerRef) ->
            _ = erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
            ok;
        _ ->
            ok
    end.

with_rate_limits_enabled(Fun) ->
    OldValue = os:getenv("FLUXER_DISABLE_RATE_LIMITS"),
    os:unsetenv("FLUXER_DISABLE_RATE_LIMITS"),
    try
        Fun()
    after
        restore_env("FLUXER_DISABLE_RATE_LIMITS", OldValue)
    end.

restore_env(Key, false) ->
    os:unsetenv(Key);
restore_env(Key, Value) ->
    os:putenv(Key, Value).

unique_test_peer_ip(Prefix) ->
    Suffix = integer_to_binary(erlang:unique_integer([positive])),
    <<Prefix/binary, "-", Suffix/binary>>.
