%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rollout_config_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

-define(PERSISTENT_TERM_KEY, gateway_rollout_config).

default_config() ->
    #{
        <<"session_rollout_percentage">> => 100,
        <<"session_rollout_mode">> => <<"modulo">>,
        <<"guild_rollout_percentage">> => 100,
        <<"rpc_request_timeout_ms">> => 10000,
        <<"max_concurrent_session_starts">> => 512,
        <<"max_concurrent_guild_starts">> => 256,
        <<"gateway_dispatch_relay_shards">> => 32,
        <<"gateway_dispatch_relay_max_queue">> => 50000,
        <<"voice_e2ee_scope">> => <<"guild_feature_only">>,
        <<"voice_reconciliation_v3_percentage">> => 100,
        <<"voice_reconciliation_v3_interval_ms">> => 2000
    }.

default_config_has_expected_keys_test() ->
    persistent_term:erase(?PERSISTENT_TERM_KEY),
    Config = gateway_rollout_config:get(),
    ?assertEqual(100, maps:get(<<"session_rollout_percentage">>, Config)),
    ?assertEqual(<<"modulo">>, maps:get(<<"session_rollout_mode">>, Config)),
    ?assertEqual(100, maps:get(<<"guild_rollout_percentage">>, Config)),
    ?assertEqual(10000, maps:get(<<"rpc_request_timeout_ms">>, Config)),
    ?assertEqual(512, maps:get(<<"max_concurrent_session_starts">>, Config)),
    ?assertEqual(256, maps:get(<<"max_concurrent_guild_starts">>, Config)),
    ?assertEqual(100, maps:get(<<"voice_reconciliation_v3_percentage">>, Config)),
    ?assertEqual(2000, maps:get(<<"voice_reconciliation_v3_interval_ms">>, Config)).

is_session_eligible_full_rollout_test() ->
    persistent_term:put(?PERSISTENT_TERM_KEY, default_config()),
    ?assert(gateway_rollout_config:is_session_eligible(<<"user123">>)),
    ?assert(gateway_rollout_config:is_session_eligible(<<"user456">>)).

is_guild_eligible_full_rollout_test() ->
    persistent_term:put(?PERSISTENT_TERM_KEY, default_config()),
    ?assert(gateway_rollout_config:is_guild_eligible(<<"guild123">>)),
    ?assert(gateway_rollout_config:is_guild_eligible(<<"guild456">>)).

is_session_eligible_zero_rollout_test() ->
    Config = (default_config())#{<<"session_rollout_percentage">> => 0},
    persistent_term:put(?PERSISTENT_TERM_KEY, Config),
    ?assertNot(gateway_rollout_config:is_session_eligible(<<"user123">>)),
    ?assertNot(gateway_rollout_config:is_session_eligible(<<"user456">>)).

is_guild_eligible_zero_rollout_test() ->
    Config = (default_config())#{<<"guild_rollout_percentage">> => 0},
    persistent_term:put(?PERSISTENT_TERM_KEY, Config),
    ?assertNot(gateway_rollout_config:is_guild_eligible(<<"guild123">>)),
    ?assertNot(gateway_rollout_config:is_guild_eligible(<<"guild456">>)).

validate_config_rejects_bad_rollout_values_test() ->
    ?assertMatch(
        {error, {invalid_field, <<"session_rollout_percentage">>, 101}},
        gateway_rollout_config_validate:validate(
            #{
                <<"session_rollout_percentage">> => 101
            },
            default_config()
        )
    ),
    ?assertMatch(
        {error, {invalid_field, <<"session_rollout_mode">>, <<"open">>}},
        gateway_rollout_config_validate:validate(
            #{
                <<"session_rollout_mode">> => <<"open">>
            },
            default_config()
        )
    ),
    ?assertMatch(
        {error, {invalid_field, <<"max_concurrent_session_starts">>, 0}},
        gateway_rollout_config_validate:validate(
            #{
                <<"max_concurrent_session_starts">> => 0
            },
            default_config()
        )
    ).

validate_config_ignores_unknown_keys_and_merges_defaults_test() ->
    {ok, Config} = gateway_rollout_config_validate:validate(
        #{
            <<"session_rollout_percentage">> => 25,
            <<"unknown">> => unsafe
        },
        default_config()
    ),
    ?assertEqual(25, maps:get(<<"session_rollout_percentage">>, Config)),
    ?assertEqual(false, maps:is_key(<<"unknown">>, Config)),
    ?assertEqual(256, maps:get(<<"max_concurrent_guild_starts">>, Config)).

session_start_rollout_decision_eligible_test() ->
    persistent_term:put(?PERSISTENT_TERM_KEY, default_config()),
    ?assertEqual(
        eligible,
        gateway_rollout_config:session_start_rollout_decision(#{<<"user_id">> => <<"123">>})
    ).

session_start_rollout_decision_missing_user_id_test() ->
    Config = (default_config())#{<<"session_rollout_percentage">> => 50},
    persistent_term:put(?PERSISTENT_TERM_KEY, Config),
    ?assertEqual(missing_user_id, gateway_rollout_config:session_start_rollout_decision(#{})).

is_clustered_false_when_flag_unset_test() ->
    persistent_term:put({fluxer_gateway, runtime_config}, #{}),
    ?assertEqual(false, gateway_rollout_config:is_clustered()).

is_clustered_false_when_flag_not_true_test() ->
    persistent_term:put({fluxer_gateway, runtime_config}, #{cluster_enabled => false}),
    ?assertEqual(false, gateway_rollout_config:is_clustered()),
    persistent_term:put({fluxer_gateway, runtime_config}, #{cluster_enabled => <<"true">>}),
    ?assertEqual(false, gateway_rollout_config:is_clustered()),
    persistent_term:put({fluxer_gateway, runtime_config}, #{cluster_enabled => 1}),
    ?assertEqual(false, gateway_rollout_config:is_clustered()).

is_clustered_false_when_flag_true_but_only_self_test() ->
    persistent_term:put({fluxer_gateway, runtime_config}, #{cluster_enabled => true}),
    persistent_term:erase({gateway_cluster_membership, members}),
    ?assertEqual(false, gateway_rollout_config:is_clustered()),
    persistent_term:put({gateway_cluster_membership, members}, [node()]),
    ?assertEqual(false, gateway_rollout_config:is_clustered()),
    persistent_term:erase({gateway_cluster_membership, members}),
    persistent_term:erase({fluxer_gateway, runtime_config}).

is_clustered_true_when_flag_and_multiple_members_test() ->
    persistent_term:put({fluxer_gateway, runtime_config}, #{cluster_enabled => true}),
    persistent_term:put(
        {gateway_cluster_membership, members},
        [node(), 'peer@host']
    ),
    ?assertEqual(true, gateway_rollout_config:is_clustered()),
    persistent_term:erase({gateway_cluster_membership, members}),
    persistent_term:erase({fluxer_gateway, runtime_config}).

rollout_percentage_update_applies_atomically_across_concurrent_reads_test() ->
    Config50 = (default_config())#{<<"session_rollout_percentage">> => 50},
    persistent_term:put(?PERSISTENT_TERM_KEY, Config50),
    Self = self(),
    NumReaders = 20,
    Pids = [
        spawn(fun() ->
            V = gateway_rollout_config:session_rollout_percentage(),
            Self ! {rollout_read, self(), V}
        end)
     || _ <- lists:seq(1, NumReaders)
    ],
    Results = [
        receive
            {rollout_read, Pid, V} -> V
        after 1000 -> timeout
        end
     || Pid <- Pids
    ],
    ?assert(lists:all(fun(V) -> V =:= 50 end, Results)),
    persistent_term:put(?PERSISTENT_TERM_KEY, default_config()).

nats_payload_updates_config_and_notifies_subscribers_test() ->
    persistent_term:put(?PERSISTENT_TERM_KEY, default_config()),
    Config = (default_config())#{<<"session_rollout_percentage">> => 42},
    Payload = iolist_to_binary(
        json:encode(#{
            <<"type">> => <<"gateway_rollout_config">>,
            <<"config">> => Config
        })
    ),
    State = #{
        subscribers => [{self(), make_ref()}],
        nats_subscription => undefined,
        nats_monitor => undefined
    },
    ?assertMatch(
        {noreply, State},
        gateway_rollout_config:handle_info(
            {nats_msg, <<"config.gateway.rollout">>, Payload, undefined},
            State
        )
    ),
    ?assertEqual(42, gateway_rollout_config:session_rollout_percentage()),
    receive
        rollout_config_changed -> ok
    after 100 ->
        ?assert(false)
    end,
    persistent_term:put(?PERSISTENT_TERM_KEY, default_config()).

nats_duplicate_payload_does_not_notify_subscribers_test() ->
    persistent_term:put(?PERSISTENT_TERM_KEY, default_config()),
    Payload = iolist_to_binary(
        json:encode(#{
            <<"type">> => <<"gateway_rollout_config">>,
            <<"config">> => default_config()
        })
    ),
    State = #{
        subscribers => [{self(), make_ref()}],
        nats_subscription => undefined,
        nats_monitor => undefined
    },
    ?assertMatch(
        {noreply, State},
        gateway_rollout_config:handle_info(
            {nats_msg, <<"config.gateway.rollout">>, Payload, undefined},
            State
        )
    ),
    receive
        rollout_config_changed -> ?assert(false)
    after 100 ->
        ok
    end.

validate_config_rejects_rpc_timeout_below_minimum_test() ->
    ?assertMatch(
        {error, {invalid_field, <<"rpc_request_timeout_ms">>, 500}},
        gateway_rollout_config_validate:validate(
            #{<<"rpc_request_timeout_ms">> => 500},
            default_config()
        )
    ).

validate_config_rejects_rpc_timeout_above_maximum_test() ->
    ?assertMatch(
        {error, {invalid_field, <<"rpc_request_timeout_ms">>, 61000}},
        gateway_rollout_config_validate:validate(
            #{<<"rpc_request_timeout_ms">> => 61000},
            default_config()
        )
    ).

validate_config_rejects_voice_reconciliation_v3_interval_below_minimum_test() ->
    ?assertMatch(
        {error, {invalid_field, <<"voice_reconciliation_v3_interval_ms">>, 499}},
        gateway_rollout_config_validate:validate(
            #{<<"voice_reconciliation_v3_interval_ms">> => 499},
            default_config()
        )
    ).

validate_config_rejects_voice_reconciliation_v3_percentage_above_maximum_test() ->
    ?assertMatch(
        {error, {invalid_field, <<"voice_reconciliation_v3_percentage">>, 101}},
        gateway_rollout_config_validate:validate(
            #{<<"voice_reconciliation_v3_percentage">> => 101},
            default_config()
        )
    ).

validate_config_rejects_relay_max_queue_zero_test() ->
    ?assertMatch(
        {error, {invalid_field, <<"gateway_dispatch_relay_max_queue">>, 0}},
        gateway_rollout_config_validate:validate(
            #{<<"gateway_dispatch_relay_max_queue">> => 0},
            default_config()
        )
    ).

validate_config_rejects_relay_max_queue_above_kill_threshold_test() ->
    Above = process_health_watchdog:kill_threshold() + 1,
    ?assertMatch(
        {error, {invalid_field, <<"gateway_dispatch_relay_max_queue">>, Above}},
        gateway_rollout_config_validate:validate(
            #{<<"gateway_dispatch_relay_max_queue">> => Above},
            default_config()
        )
    ).

validate_config_accepts_relay_max_queue_range_bounds_test() ->
    Ceiling = process_health_watchdog:kill_threshold(),
    ?assertMatch(
        {ok, #{<<"gateway_dispatch_relay_max_queue">> := 1}},
        gateway_rollout_config_validate:validate(
            #{<<"gateway_dispatch_relay_max_queue">> => 1},
            default_config()
        )
    ),
    ?assertMatch(
        {ok, #{<<"gateway_dispatch_relay_max_queue">> := Ceiling}},
        gateway_rollout_config_validate:validate(
            #{<<"gateway_dispatch_relay_max_queue">> => Ceiling},
            default_config()
        )
    ).
