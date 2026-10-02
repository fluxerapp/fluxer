%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(fluxer_gateway_config).
-typing([eqwalizer]).

-export([load/0, build_config/1, optional_string/1, env_value/1]).
-export_type([config/0]).

-type config() :: map().
-type raw_value() ::
    binary()
    | number()
    | boolean()
    | [raw_value()]
    | #{binary() => raw_value()}
    | null
    | undefined.
-type log_level() :: debug | info | notice | warning | error | critical | alert | emergency.
-type gateway_role() :: websocket | sessions | presence | guilds | calls | push | all.

-define(MAX_CLUSTER_STATIC_PEERS, 256).

-spec load() -> config().
load() ->
    build_config(env_config()).

-spec env_config() -> map().
env_config() ->
    #{
        <<"env">> => env_binary("FLUXER_ENV", <<"development">>),
        <<"internal">> => env_internal_config(),
        <<"public">> => env_public_config(),
        <<"proxy">> => env_proxy_config(),
        <<"services">> => env_services_config()
    }.

-spec env_internal_config() -> map().
env_internal_config() ->
    #{
        <<"api">> => env_binary("FLUXER_INTERNAL_API_ENDPOINT", <<"http://127.0.0.1:8080">>)
    }.

-spec env_public_config() -> map().
env_public_config() ->
    #{
        <<"base_domain">> => env_optional_binary("FLUXER_BASE_DOMAIN"),
        <<"scheme">> => env_optional_binary("FLUXER_PUBLIC_SCHEME"),
        <<"port">> => env_optional_binary("FLUXER_PUBLIC_PORT")
    }.

-spec env_proxy_config() -> map().
env_proxy_config() ->
    #{
        <<"client_ip_header">> => env_binary(
            "FLUXER_CLIENT_IP_HEADER_NAME", <<"x-forwarded-for">>
        ),
        <<"trust_client_ip_header">> => env_bool("FLUXER_TRUST_CLIENT_IP_HEADER", false)
    }.

-spec env_services_config() -> map().
env_services_config() ->
    #{
        <<"gateway">> => env_gateway_config(),
        <<"nats">> => env_nats_config()
    }.

-spec env_gateway_config() -> map().
env_gateway_config() ->
    env_gateway_base_config().

-spec env_gateway_base_config() -> map().
env_gateway_base_config() ->
    #{
        <<"port">> => env_int("FLUXER_GATEWAY_PORT", 8771),
        <<"gateway_role">> => env_optional_binary("FLUXER_GATEWAY_ROLE"),
        <<"rpc_auth_token">> => env_binary("FLUXER_GATEWAY_RPC_AUTH_TOKEN", <<>>),
        <<"push_enabled">> => env_bool("FLUXER_GATEWAY_PUSH_ENABLED", true),
        <<"push_enrolled_clear_notifications_enabled">> => env_bool(
            "FLUXER_GATEWAY_PUSH_ENROLLED_CLEAR_NOTIFICATIONS_ENABLED", true
        ),
        <<"push_outbox_request_timeout_ms">> => env_int(
            "FLUXER_GATEWAY_PUSH_OUTBOX_REQUEST_TIMEOUT_MS", 100000
        ),
        <<"logger_level">> => env_optional_binary("FLUXER_GATEWAY_LOGGER_LEVEL"),
        <<"api_rpc_endpoint">> => env_optional_binary("FLUXER_GATEWAY_API_RPC_ENDPOINT"),
        <<"cluster_enabled">> => env_bool("FLUXER_GATEWAY_CLUSTER_ENABLED", false),
        <<"cluster_discovery_dns_name">> => env_optional_binary(
            "FLUXER_GATEWAY_CLUSTER_DISCOVERY_DNS_NAME"
        ),
        <<"cluster_discovery_node_basename">> => env_optional_binary(
            "FLUXER_GATEWAY_CLUSTER_DISCOVERY_NODE_BASENAME"
        ),
        <<"cluster_discovery_poll_interval_ms">> => env_int(
            "FLUXER_GATEWAY_CLUSTER_DISCOVERY_POLL_INTERVAL_MS", 5000
        ),
        <<"cluster_static_peers">> => env_optional_binary(
            "FLUXER_GATEWAY_CLUSTER_STATIC_PEERS"
        ),
        <<"media_proxy_endpoint">> => env_binary(
            "FLUXER_GATEWAY_MEDIA_PROXY_ENDPOINT", <<"http://localhost:8088/media">>
        ),
        <<"static_cdn_endpoint">> => env_binary(
            "FLUXER_GATEWAY_STATIC_CDN_ENDPOINT", <<"http://localhost:8088">>
        ),
        <<"presence_push_buffer_max_entries">> => env_int(
            "FLUXER_GATEWAY_PRESENCE_PUSH_BUFFER_MAX_ENTRIES", 128
        ),
        <<"presence_push_buffer_max_bytes">> => env_int(
            "FLUXER_GATEWAY_PRESENCE_PUSH_BUFFER_MAX_BYTES", 1048576
        ),
        <<"gateway_http_rpc_max_concurrency">> => env_int(
            "FLUXER_GATEWAY_HTTP_RPC_MAX_CONCURRENCY", 512
        ),
        <<"gateway_nats_rpc_max_handlers">> => env_int(
            "FLUXER_GATEWAY_NATS_RPC_MAX_HANDLERS", 512
        ),
        <<"gateway_http_failure_threshold">> => env_int(
            "FLUXER_GATEWAY_HTTP_FAILURE_THRESHOLD", 6
        ),
        <<"gateway_http_recovery_timeout_ms">> => env_int(
            "FLUXER_GATEWAY_HTTP_RECOVERY_TIMEOUT_MS", 15000
        )
    }.

-spec env_nats_config() -> map().
env_nats_config() ->
    #{
        <<"core_url">> => env_string("FLUXER_NATS_URL", "nats://nats:4222"),
        <<"auth_token">> => env_string("FLUXER_NATS_AUTH_TOKEN", "")
    }.

-spec build_config(map()) -> config().
build_config(RawConfig) ->
    Service = get_map(RawConfig, [<<"services">>, <<"gateway">>]),
    Internal = get_map(RawConfig, [<<"internal">>]),
    Nats = get_map(RawConfig, [<<"services">>, <<"nats">>]),
    Proxy = get_map(RawConfig, [<<"proxy">>]),
    Public = get_map(RawConfig, [<<"public">>]),
    lists:foldl(fun maps:merge/2, #{}, [
        build_core_config(Service, Internal, Nats, Proxy),
        build_push_config(Service, Public),
        build_sharding_config(Service),
        build_http_config(Service),
        build_cluster_config(Service, Public),
        build_misc_config(Service)
    ]).

-spec build_core_config(map(), map(), map(), map()) -> config().
build_core_config(Service, Internal, Nats, Proxy) ->
    #{
        port => get_int(Service, <<"port">>, 8080),
        gateway_role => normalize_gateway_role(get_value(Service, <<"gateway_role">>)),
        client_ip_header => get_binary(Proxy, <<"client_ip_header">>, <<"x-forwarded-for">>),
        trust_client_ip_header => get_bool(Proxy, <<"trust_client_ip_header">>, false),
        api_internal_url => get_binary(Internal, <<"api">>, <<"http://127.0.0.1:8088">>),
        api_rpc_endpoint => get_optional_binary(Service, <<"api_rpc_endpoint">>),
        nats_core_url => get_string(Nats, <<"core_url">>, "nats://127.0.0.1:4222"),
        nats_auth_token => get_string(Nats, <<"auth_token">>, ""),
        rpc_auth_token => get_string(Service, <<"rpc_auth_token">>, ""),
        gateway_nats_rpc_max_handlers =>
            get_int(Service, <<"gateway_nats_rpc_max_handlers">>, 512),
        identify_rate_limit_enabled => get_bool(
            Service, <<"identify_rate_limit_enabled">>, false
        )
    }.

-spec build_push_config(map(), map()) -> config().
build_push_config(Service, Public) ->
    #{
        push_enabled => get_bool(Service, <<"push_enabled">>, true),
        static_cdn_endpoint => public_endpoint(
            get_binary(Service, <<"static_cdn_endpoint">>, <<"http://localhost:8088">>), Public
        ),
        push_enrolled_clear_notifications_enabled => get_bool(
            Service, <<"push_enrolled_clear_notifications_enabled">>, true
        ),
        push_outbox_max_queue => get_int(Service, <<"push_outbox_max_queue">>, 10000),
        push_outbox_max_inflight => get_int(Service, <<"push_outbox_max_inflight">>, 64),
        push_outbox_request_timeout_ms => get_int(
            Service, <<"push_outbox_request_timeout_ms">>, 100000
        ),
        push_outbox_max_age_ms => get_int(Service, <<"push_outbox_max_age_ms">>, 300000)
    }.

-spec build_sharding_config(map()) -> config().
build_sharding_config(Service) ->
    #{
        presence_cache_shards => get_optional_int(Service, <<"presence_cache_shards">>),
        presence_bus_shards => get_optional_int(Service, <<"presence_bus_shards">>),
        presence_push_buffer_max_entries =>
            get_int(Service, <<"presence_push_buffer_max_entries">>, 128),
        presence_push_buffer_max_bytes =>
            get_int(Service, <<"presence_push_buffer_max_bytes">>, 1048576),
        presence_shards => get_optional_int(Service, <<"presence_shards">>),
        guild_counts_cache_shards => get_optional_int(Service, <<"guild_counts_cache_shards">>),
        guild_shards => get_optional_int(Service, <<"guild_shards">>),
        session_shards => get_optional_int(Service, <<"session_shards">>),
        session_connect_max_queue => get_int(Service, <<"session_connect_max_queue">>, 8192)
    }.

-spec build_http_config(map()) -> config().
build_http_config(Service) ->
    #{
        gateway_http_rpc_max_concurrency =>
            get_int(Service, <<"gateway_http_rpc_max_concurrency">>, 512),
        gateway_http_failure_threshold => get_int(
            Service, <<"gateway_http_failure_threshold">>, 6
        ),
        gateway_http_recovery_timeout_ms =>
            get_int(Service, <<"gateway_http_recovery_timeout_ms">>, 15000),
        gateway_http_cleanup_interval_ms =>
            get_int(Service, <<"gateway_http_cleanup_interval_ms">>, 30000),
        gateway_http_cleanup_max_age_ms =>
            get_int(Service, <<"gateway_http_cleanup_max_age_ms">>, 300000)
    }.

-spec build_cluster_config(map(), map()) -> config().
build_cluster_config(Service, Public) ->
    #{
        cluster_enabled => get_bool(Service, <<"cluster_enabled">>, false),
        cluster_discovery_dns_name =>
            optional_string(get_optional_binary(Service, <<"cluster_discovery_dns_name">>)),
        cluster_discovery_node_basename =>
            optional_string(
                get_optional_binary(Service, <<"cluster_discovery_node_basename">>)
            ),
        cluster_discovery_poll_interval_ms =>
            get_int(Service, <<"cluster_discovery_poll_interval_ms">>, 5000),
        cluster_static_peers =>
            parse_node_list(get_optional_binary(Service, <<"cluster_static_peers">>)),
        media_proxy_endpoint => public_endpoint(
            get_optional_binary(Service, <<"media_proxy_endpoint">>), Public
        )
    }.

-spec build_misc_config(map()) -> config().
build_misc_config(Service) ->
    #{
        handoff_enable_event_pause => get_bool(
            Service, <<"handoff_enable_event_pause">>, false
        ),
        voice_state_counts_sync_interval_ms =>
            get_int(Service, <<"voice_state_counts_sync_interval_ms">>, 30000),
        logger_level => get_log_level(Service, <<"logger_level">>, info)
    }.

-spec get_map(map(), [binary()]) -> map().
get_map(Map, Keys) ->
    case get_in(Map, Keys) of
        V when is_map(V) -> V;
        _ -> #{}
    end.

-spec env_string(string(), string()) -> string().
env_string(Name, Default) ->
    case env_value(Name) of
        undefined -> Default;
        Value -> Value
    end.

-spec env_value(string()) -> string() | undefined.
env_value(Name) ->
    case os:getenv(Name) of
        false -> undefined;
        Value -> non_blank(Value)
    end.

-spec non_blank(string()) -> string() | undefined.
non_blank(Value) ->
    case string:trim(Value) of
        "" -> undefined;
        _ -> Value
    end.

-spec env_binary(string(), binary()) -> binary().
env_binary(Name, Default) ->
    characters_to_binary_or_default(env_string(Name, binary_to_list(Default)), Default).

-spec env_optional_binary(string()) -> binary() | undefined.
env_optional_binary(Name) ->
    case env_value(Name) of
        undefined -> undefined;
        Value -> characters_to_binary_or_default(Value, undefined)
    end.

-spec characters_to_binary_or_default
    (string(), binary()) -> binary();
    (string(), undefined) -> binary() | undefined.
characters_to_binary_or_default(Value, Default) ->
    case unicode:characters_to_binary(Value) of
        Binary when is_binary(Binary) -> Binary;
        _ -> Default
    end.

-spec env_int(string(), integer()) -> integer().
env_int(Name, Default) ->
    case env_value(Name) of
        undefined -> Default;
        Value -> parse_int(Name, Value)
    end.

-spec parse_int(string(), string()) -> integer().
parse_int(Name, Value) ->
    try list_to_integer(string:trim(Value)) of
        Parsed -> Parsed
    catch
        error:badarg -> erlang:error({invalid_integer_env, Name, Value})
    end.

-spec env_bool(string(), boolean()) -> boolean().
env_bool(Name, Default) ->
    case string:lowercase(env_string(Name, "")) of
        "1" -> true;
        "true" -> true;
        "yes" -> true;
        "0" -> false;
        "false" -> false;
        "no" -> false;
        "" -> Default;
        _ -> Default
    end.

-spec get_int(map(), binary(), integer()) -> integer().
get_int(Map, Key, Default) when is_integer(Default) -> to_int(get_value(Map, Key), Default).
-spec get_optional_int(map(), binary()) -> integer() | undefined.
get_optional_int(Map, Key) -> to_optional_int(get_value(Map, Key)).
-spec get_bool(map(), binary(), boolean()) -> boolean().
get_bool(Map, Key, Default) when is_boolean(Default) -> to_bool(get_value(Map, Key), Default).
-spec get_string(map(), binary(), string()) -> string().
get_string(Map, Key, Default) when is_list(Default) -> to_string(get_value(Map, Key), Default).
-spec get_binary(map(), binary(), binary()) -> binary().
get_binary(Map, Key, Default) -> to_binary(get_value(Map, Key), Default).

-spec get_optional_binary(map(), binary()) -> binary() | undefined.
get_optional_binary(Map, Key) ->
    case get_value(Map, Key) of
        undefined -> undefined;
        V -> to_binary(V, undefined)
    end.

-spec get_log_level(map(), binary(), log_level()) -> log_level().
get_log_level(Map, Key, Default) when is_atom(Default) ->
    case normalize_log_level(get_value(Map, Key)) of
        undefined -> Default;
        L -> L
    end.

-spec get_in(term(), [binary()]) -> raw_value() | undefined.
get_in(Map, [Key | Rest]) when is_map(Map) ->
    case get_value(Map, Key) of
        undefined -> undefined;
        Value when Rest =:= [] -> Value;
        Value -> get_in(Value, Rest)
    end;
get_in(_, _) ->
    undefined.

-spec get_value(map(), binary()) -> raw_value() | undefined.
get_value(Map, Key) when is_map(Map) ->
    case maps:get(Key, Map, undefined) of
        undefined when is_binary(Key) -> maps:get(binary_to_list(Key), Map, undefined);
        Value -> Value
    end.

-spec to_int
    (term(), integer()) -> integer();
    (term(), undefined) -> integer() | undefined.
to_int(Value, _Default) when is_integer(Value) -> Value;
to_int(Value, _Default) when is_float(Value) -> trunc(Value);
to_int(Value, Default) -> string_to_int(to_string(Value, ""), Default).

-spec string_to_int
    (string(), integer()) -> integer();
    (string(), undefined) -> integer() | undefined.
string_to_int("", Default) ->
    Default;
string_to_int(Str, Default) ->
    case string:to_integer(Str) of
        {Int, _} when is_integer(Int) -> Int;
        {error, _} -> Default
    end.

-spec to_optional_int(term()) -> integer() | undefined.
to_optional_int(Value) -> to_int(Value, undefined).

-spec to_bool(term(), boolean()) -> boolean().
to_bool(Value, _Default) when is_boolean(Value) -> Value;
to_bool(Value, Default) when is_atom(Value) -> Default;
to_bool(Value, Default) ->
    case string:lowercase(to_string(Value, "")) of
        "true" -> true;
        "1" -> true;
        "false" -> false;
        "0" -> false;
        _ -> Default
    end.

-spec to_string(term(), string()) -> string().
to_string(undefined, Default) -> Default;
to_string(Bin, _) when is_binary(Bin) -> binary_to_list(Bin);
to_string(Str, _) when is_list(Str) -> config_char_list(Str);
to_string(Atom, _) when is_atom(Atom) -> atom_to_list(Atom);
to_string(_, Default) -> Default.

-spec config_char_list(list()) -> string().
config_char_list(List) -> eqwalizer:dynamic_cast(List).

-spec to_binary
    (term(), binary()) -> binary();
    (term(), undefined) -> binary() | undefined.
to_binary(undefined, Default) -> Default;
to_binary(Bin, _) when is_binary(Bin) -> Bin;
to_binary(Str, _) when is_list(Str) -> list_to_binary(config_char_list(Str));
to_binary(Atom, _) when is_atom(Atom) -> list_to_binary(atom_to_list(Atom));
to_binary(_, Default) -> Default.

-spec parse_node_list(binary() | undefined) -> [node()].
parse_node_list(undefined) ->
    [];
parse_node_list(Bin) when is_binary(Bin) ->
    Tokens = string:lexemes(binary_to_list(Bin), ","),
    parse_node_list(Tokens, ?MAX_CLUSTER_STATIC_PEERS, []).

-spec parse_node_list([string()], non_neg_integer(), [node()]) -> [node()].
parse_node_list(_Peers, 0, Acc) ->
    lists:reverse(Acc);
parse_node_list([], _Remaining, Acc) ->
    lists:reverse(Acc);
parse_node_list([Peer | Rest], Remaining, Acc) ->
    case gateway_node_name:from_string(Peer) of
        {ok, Node} -> parse_node_list(Rest, Remaining - 1, [Node | Acc]);
        error -> erlang:error({invalid_cluster_static_peer, Peer})
    end.

-spec normalize_log_level(term()) -> log_level() | undefined.
normalize_log_level(undefined) ->
    undefined;
normalize_log_level(Level) when is_atom(Level) -> normalize_log_level(atom_to_list(Level));
normalize_log_level(Level) when is_binary(Level) -> normalize_log_level(binary_to_list(Level));
normalize_log_level(Level) when is_list(Level) ->
    case string:lowercase(string:trim(config_char_list(Level))) of
        "debug" -> debug;
        "info" -> info;
        "notice" -> notice;
        "warning" -> warning;
        "error" -> error;
        "critical" -> critical;
        "alert" -> alert;
        "emergency" -> emergency;
        _ -> undefined
    end;
normalize_log_level(_) ->
    undefined.

-spec normalize_gateway_role(term()) -> gateway_role().
normalize_gateway_role(Value) when is_binary(Value) ->
    case Value of
        <<"websocket">> -> websocket;
        <<"sessions">> -> sessions;
        <<"presence">> -> presence;
        <<"guilds">> -> guilds;
        <<"calls">> -> calls;
        <<"push">> -> push;
        <<"all">> -> all;
        _ -> all
    end;
normalize_gateway_role(Value) when is_atom(Value) ->
    normalize_gateway_role(atom_to_binary(Value, utf8));
normalize_gateway_role(Value) when is_list(Value) ->
    normalize_gateway_role(unicode:characters_to_binary(config_char_list(Value)));
normalize_gateway_role(_) ->
    all.

-spec public_endpoint(binary() | undefined, map()) -> binary() | undefined.
public_endpoint(undefined, _Public) ->
    undefined;
public_endpoint(Url, Public) ->
    gateway_public_endpoint:normalize(
        Url,
        get_optional_binary(Public, <<"base_domain">>),
        get_optional_binary(Public, <<"scheme">>),
        get_optional_int(Public, <<"port">>)
    ).

-spec optional_string(binary() | undefined) -> string() | undefined.
optional_string(undefined) -> undefined;
optional_string(Bin) when is_binary(Bin) -> binary_to_list(Bin).
