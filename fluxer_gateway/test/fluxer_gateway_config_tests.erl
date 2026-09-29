%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(fluxer_gateway_config_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

cluster_defaults_test() ->
    Config = fluxer_gateway_config:build_config(#{}),
    ?assertEqual(false, maps:get(cluster_enabled, Config)),
    ?assertEqual(undefined, maps:get(cluster_discovery_dns_name, Config)),
    ?assertEqual(undefined, maps:get(cluster_discovery_node_basename, Config)),
    ?assertEqual(5000, maps:get(cluster_discovery_poll_interval_ms, Config)),
    ?assertEqual([], maps:get(cluster_static_peers, Config)).

cluster_overrides_test() ->
    RawConfig = #{
        <<"services">> => #{
            <<"gateway">> => #{
                <<"cluster_enabled">> => true,
                <<"cluster_discovery_dns_name">> =>
                    <<"fluxer-gateway-headless.fluxer.svc.cluster.local">>,
                <<"cluster_discovery_node_basename">> => <<"fluxer_gateway">>,
                <<"cluster_discovery_poll_interval_ms">> => 2500,
                <<"cluster_static_peers">> =>
                    <<"fluxer_gateway_websocket_1@127.0.0.1, fluxer_gateway_sessions_1@127.0.0.1">>
            }
        }
    },
    Config = fluxer_gateway_config:build_config(RawConfig),
    ?assertEqual(true, maps:get(cluster_enabled, Config)),
    ?assertEqual(
        "fluxer-gateway-headless.fluxer.svc.cluster.local",
        maps:get(cluster_discovery_dns_name, Config)
    ),
    ?assertEqual(
        "fluxer_gateway",
        maps:get(cluster_discovery_node_basename, Config)
    ),
    ?assertEqual(2500, maps:get(cluster_discovery_poll_interval_ms, Config)),
    ?assertEqual(
        [
            list_to_atom("fluxer_gateway_websocket_1@127.0.0.1"),
            list_to_atom("fluxer_gateway_sessions_1@127.0.0.1")
        ],
        maps:get(cluster_static_peers, Config)
    ).

cluster_static_peers_rejects_invalid_node_names_test() ->
    LongPeer = list_to_binary(lists:duplicate(260, $a)),
    RawConfig = #{
        <<"services">> => #{
            <<"gateway">> => #{
                <<"cluster_static_peers">> => <<
                    "valid_peer@127.0.0.1,",
                    "invalid peer@127.0.0.2,",
                    "missing-host@,",
                    LongPeer/binary,
                    ",other-valid@node.local"
                >>
            }
        }
    },
    ?assertError(
        {invalid_cluster_static_peer, "invalid peer@127.0.0.2"},
        fluxer_gateway_config:build_config(RawConfig)
    ).

cluster_static_peers_accepts_valid_node_names_test() ->
    RawConfig = #{
        <<"services">> => #{
            <<"gateway">> => #{
                <<"cluster_static_peers">> =>
                    <<"valid_peer@127.0.0.1,other-valid@node.local">>
            }
        }
    },
    Config = fluxer_gateway_config:build_config(RawConfig),
    ?assertEqual(
        [list_to_atom("valid_peer@127.0.0.1"), list_to_atom("other-valid@node.local")],
        maps:get(cluster_static_peers, Config)
    ).

push_clear_switch_reads_the_enrolled_env_name_test() ->
    ?assertEqual(
        true, maps:get(push_enrolled_clear_notifications_enabled, fluxer_gateway_config:load())
    ),
    with_env("FLUXER_GATEWAY_PUSH_ENROLLED_CLEAR_NOTIFICATIONS_ENABLED", "false", fun() ->
        Config = fluxer_gateway_config:load(),
        ?assertEqual(false, maps:get(push_enrolled_clear_notifications_enabled, Config))
    end).

gateway_config_holds_no_direct_push_delivery_keys_test() ->
    Config = fluxer_gateway_config:load(),
    lists:foreach(
        fun(Key) -> ?assertNot(maps:is_key(Key, Config)) end,
        [
            push_clear_notifications_enabled,
            push_endpoint_guard_enabled,
            push_managed_relay_hosts,
            push_relay_consent_accepted,
            vapid_public_key,
            apns_enabled,
            fcm_enabled,
            gateway_http_push_max_concurrency
        ]
    ).

presence_push_buffer_env_defaults_test() ->
    with_env("FLUXER_GATEWAY_PRESENCE_PUSH_BUFFER_MAX_ENTRIES", "7", fun() ->
        with_env("FLUXER_GATEWAY_PRESENCE_PUSH_BUFFER_MAX_BYTES", "4096", fun() ->
            Config = fluxer_gateway_config:load(),
            ?assertEqual(7, maps:get(presence_push_buffer_max_entries, Config)),
            ?assertEqual(4096, maps:get(presence_push_buffer_max_bytes, Config))
        end)
    end).

env_only_http_runtime_config_test() ->
    with_envs(
        [
            {"FLUXER_GATEWAY_HTTP_RPC_MAX_CONCURRENCY", "42"},
            {"FLUXER_GATEWAY_HTTP_FAILURE_THRESHOLD", "9"},
            {"FLUXER_GATEWAY_HTTP_RECOVERY_TIMEOUT_MS", "6000"}
        ],
        fun() ->
            Config = fluxer_gateway_config:load(),
            ?assertEqual(42, maps:get(gateway_http_rpc_max_concurrency, Config)),
            ?assertEqual(9, maps:get(gateway_http_failure_threshold, Config)),
            ?assertEqual(6000, maps:get(gateway_http_recovery_timeout_ms, Config))
        end
    ).

rpc_concurrency_keys_are_independent_test() ->
    with_envs(
        [
            {"FLUXER_GATEWAY_HTTP_RPC_MAX_CONCURRENCY", "128"},
            {"FLUXER_GATEWAY_NATS_RPC_MAX_HANDLERS", "2048"}
        ],
        fun() ->
            Config = fluxer_gateway_config:load(),
            ?assertEqual(128, maps:get(gateway_http_rpc_max_concurrency, Config)),
            ?assertEqual(2048, maps:get(gateway_nats_rpc_max_handlers, Config))
        end
    ).

env_int_rejects_a_non_integer_value_test() ->
    with_env("FLUXER_GATEWAY_HTTP_RPC_MAX_CONCURRENCY", "abc", fun() ->
        ?assertError(
            {invalid_integer_env, "FLUXER_GATEWAY_HTTP_RPC_MAX_CONCURRENCY", "abc"},
            fluxer_gateway_config:load()
        )
    end).

env_int_falls_back_to_the_default_for_an_empty_value_test() ->
    with_env("FLUXER_GATEWAY_HTTP_RPC_MAX_CONCURRENCY", "", fun() ->
        Config = fluxer_gateway_config:load(),
        ?assertEqual(512, maps:get(gateway_http_rpc_max_concurrency, Config))
    end).

blank_env_values_fall_back_to_the_defaults_test() ->
    with_envs(
        [
            {"FLUXER_GATEWAY_HTTP_RPC_MAX_CONCURRENCY", "  "},
            {"FLUXER_CLIENT_IP_HEADER_NAME", " "},
            {"FLUXER_NATS_URL", "\t"},
            {"FLUXER_GATEWAY_API_RPC_ENDPOINT", "   "},
            {"FLUXER_GATEWAY_LOGGER_LEVEL", " "},
            {"FLUXER_GATEWAY_PUSH_ENABLED", " "}
        ],
        fun() ->
            Config = fluxer_gateway_config:load(),
            ?assertEqual(512, maps:get(gateway_http_rpc_max_concurrency, Config)),
            ?assertEqual(<<"x-forwarded-for">>, maps:get(client_ip_header, Config)),
            ?assertEqual("nats://nats:4222", maps:get(nats_core_url, Config)),
            ?assertEqual(undefined, maps:get(api_rpc_endpoint, Config)),
            ?assertEqual(info, maps:get(logger_level, Config)),
            ?assertEqual(true, maps:get(push_enabled, Config))
        end
    ).

logger_level_env_test() ->
    with_env("FLUXER_GATEWAY_LOGGER_LEVEL", "Debug", fun() ->
        ?assertEqual(debug, maps:get(logger_level, fluxer_gateway_config:load()))
    end).

rpc_concurrency_key_defaults_test() ->
    Config = fluxer_gateway_config:build_config(#{}),
    ?assertEqual(512, maps:get(gateway_nats_rpc_max_handlers, Config)),
    ?assertEqual(512, maps:get(gateway_http_rpc_max_concurrency, Config)).

optional_string_test() ->
    ?assertEqual(undefined, fluxer_gateway_config:optional_string(undefined)),
    ?assertEqual("hello", fluxer_gateway_config:optional_string(<<"hello">>)),
    ?assertEqual("", fluxer_gateway_config:optional_string(<<>>)).

public_endpoints_env_non_default_port_test() ->
    with_envs(
        [
            {"FLUXER_BASE_DOMAIN", "fluxer.example"},
            {"FLUXER_PUBLIC_SCHEME", "https"},
            {"FLUXER_PUBLIC_PORT", "8443"},
            {"FLUXER_GATEWAY_MEDIA_PROXY_ENDPOINT", "https://fluxer.example/media"},
            {"FLUXER_GATEWAY_STATIC_CDN_ENDPOINT", "https://fluxer.example"}
        ],
        fun() ->
            Config = fluxer_gateway_config:load(),
            ?assertEqual(
                <<"https://fluxer.example:8443/media">>,
                maps:get(media_proxy_endpoint, Config)
            ),
            ?assertEqual(
                <<"https://fluxer.example:8443">>, maps:get(static_cdn_endpoint, Config)
            )
        end
    ).

public_endpoints_env_default_port_test() ->
    with_envs(
        [
            {"FLUXER_BASE_DOMAIN", "fluxer.example"},
            {"FLUXER_PUBLIC_SCHEME", "https"},
            {"FLUXER_PUBLIC_PORT", "443"},
            {"FLUXER_GATEWAY_MEDIA_PROXY_ENDPOINT", "https://fluxer.example/media"},
            {"FLUXER_GATEWAY_STATIC_CDN_ENDPOINT", "https://cdn.othercdn.net"}
        ],
        fun() ->
            Config = fluxer_gateway_config:load(),
            ?assertEqual(
                <<"https://fluxer.example/media">>, maps:get(media_proxy_endpoint, Config)
            ),
            ?assertEqual(
                <<"https://cdn.othercdn.net">>, maps:get(static_cdn_endpoint, Config)
            )
        end
    ).

public_endpoints_defaults_test() ->
    Config = fluxer_gateway_config:build_config(#{}),
    ?assertEqual(undefined, maps:get(media_proxy_endpoint, Config)),
    ?assertEqual(<<"http://localhost:8088">>, maps:get(static_cdn_endpoint, Config)).

with_envs([], Fun) ->
    Fun();
with_envs([{Name, Value} | Rest], Fun) ->
    with_env(Name, Value, fun() -> with_envs(Rest, Fun) end).

with_env(Name, Value, Fun) ->
    Previous = os:getenv(Name),
    os:putenv(Name, Value),
    try
        Fun()
    after
        restore_env(Name, Previous)
    end.

restore_env(Name, false) ->
    os:unsetenv(Name);
restore_env(Name, Previous) ->
    os:putenv(Name, Previous).
