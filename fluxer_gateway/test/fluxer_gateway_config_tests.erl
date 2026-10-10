%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(fluxer_gateway_config_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

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

pinned_guild_ids_reject_non_snowflakes_test() ->
    with_env("FLUXER_GATEWAY_PINNED_GUILD_IDS", "1100000000000000001,ab", fun() ->
        ?assertError({invalid_pinned_guild_id, "ab"}, fluxer_gateway_config:load())
    end).

env_value_reads_name_file_test() ->
    with_env_file(<<"from-file\r\n">>, fun(Path) ->
        with_envs(
            [{"FLUXER_GATEWAY_TEST_SECRET", ""}, {"FLUXER_GATEWAY_TEST_SECRET_FILE", Path}],
            fun() ->
                ?assertEqual(
                    "from-file", fluxer_gateway_config:env_value("FLUXER_GATEWAY_TEST_SECRET")
                )
            end
        )
    end).

env_value_trims_only_one_newline_test() ->
    with_env_file(<<"line1\nline2\n\n">>, fun(Path) ->
        with_env("FLUXER_GATEWAY_TEST_SECRET_FILE", Path, fun() ->
            ?assertEqual(
                "line1\nline2\n", fluxer_gateway_config:env_value("FLUXER_GATEWAY_TEST_SECRET")
            )
        end)
    end).

env_value_rejects_name_and_name_file_test() ->
    with_envs(
        [
            {"FLUXER_GATEWAY_TEST_SECRET", "direct"},
            {"FLUXER_GATEWAY_TEST_SECRET_FILE", "/run/secrets/x"}
        ],
        fun() ->
            ?assertError(
                {ambiguous_env, "FLUXER_GATEWAY_TEST_SECRET",
                    "FLUXER_GATEWAY_TEST_SECRET_FILE"},
                fluxer_gateway_config:env_value("FLUXER_GATEWAY_TEST_SECRET")
            )
        end
    ).

env_value_rejects_missing_name_file_test() ->
    Path = "/nonexistent/fluxer-gateway-test-secret",
    with_env("FLUXER_GATEWAY_TEST_SECRET_FILE", Path, fun() ->
        ?assertError(
            {unreadable_env_file, "FLUXER_GATEWAY_TEST_SECRET_FILE", Path, enoent},
            fluxer_gateway_config:env_value("FLUXER_GATEWAY_TEST_SECRET")
        )
    end).

with_env_file(Contents, Fun) ->
    Path = filename:join(
        filename:basedir(user_cache, "fluxer_gateway_tests"),
        integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Contents),
    try
        Fun(Path)
    after
        file:delete(Path)
    end.

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
