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
        <<"voice_e2ee_scope">> => <<"guild_feature_only">>
    }.

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
