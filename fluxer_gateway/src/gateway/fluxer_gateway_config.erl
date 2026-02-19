%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(fluxer_gateway_config).

-export([load/0, load_from/1]).

-type config() :: map().
-type log_level() :: debug | info | notice | warning | error | critical | alert | emergency.

-spec load() -> config().
load() ->
    case os:getenv("FLUXER_CONFIG") of
        false -> erlang:error({missing_env, "FLUXER_CONFIG"});
        "" -> erlang:error({missing_env, "FLUXER_CONFIG"});
        Path -> load_from(Path)
    end.

-spec load_from(string()) -> config().
load_from(Path) when is_list(Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            Json = json:decode(Content),
            build_config(Json);
        {error, Reason} ->
            erlang:error({json_read_failed, Path, Reason})
    end.

-spec build_config(map()) -> config().
build_config(Json) ->
    Service = get_map(Json, [<<"services">>, <<"gateway">>]),
    Nats = get_map(Json, [<<"services">>, <<"nats">>]),
    Telemetry = get_map(Json, [<<"telemetry">>]),
    Sentry = get_map(Json, [<<"sentry">>]),
    Vapid = get_map(Json, [<<"auth">>, <<"vapid">>]),
    #{
        port => get_int(Service, <<"port">>, 8080),
        admin_reload_secret => get_optional_binary(Service, <<"admin_reload_secret">>),
        nats_core_url => get_string(Nats, <<"core_url">>, "nats://127.0.0.1:4222"),
        nats_auth_token => get_string(Nats, <<"auth_token">>, ""),
        identify_rate_limit_enabled => get_bool(Service, <<"identify_rate_limit_enabled">>, false),
        push_enabled => get_bool(Service, <<"push_enabled">>, true),
        push_user_guild_settings_cache_mb => get_int(
            Service,
            <<"push_user_guild_settings_cache_mb">>,
            1024
        ),
        push_subscriptions_cache_mb => get_int(Service, <<"push_subscriptions_cache_mb">>, 1024),
        push_blocked_ids_cache_mb => get_int(Service, <<"push_blocked_ids_cache_mb">>, 1024),
        presence_cache_shards => get_optional_int(Service, <<"presence_cache_shards">>),
        presence_bus_shards => get_optional_int(Service, <<"presence_bus_shards">>),
        presence_shards => get_optional_int(Service, <<"presence_shards">>),
        guild_shards => get_optional_int(Service, <<"guild_shards">>),
        session_shards => get_optional_int(Service, <<"session_shards">>),
        push_badge_counts_cache_mb => get_int(Service, <<"push_badge_counts_cache_mb">>, 256),
        push_badge_counts_cache_ttl_seconds =>
            get_int(Service, <<"push_badge_counts_cache_ttl_seconds">>, 60),
        push_dispatcher_max_inflight => get_int(Service, <<"push_dispatcher_max_inflight">>, 16),
        push_dispatcher_max_queue => get_int(Service, <<"push_dispatcher_max_queue">>, 2048),
        gateway_http_push_connect_timeout_ms =>
            get_int(Service, <<"gateway_http_push_connect_timeout_ms">>, 3000),
        gateway_http_push_recv_timeout_ms =>
            get_int(Service, <<"gateway_http_push_recv_timeout_ms">>, 5000),
        gateway_http_rpc_max_concurrency =>
            get_int(Service, <<"gateway_http_rpc_max_concurrency">>, 512),
        gateway_http_push_max_concurrency =>
            get_int(Service, <<"gateway_http_push_max_concurrency">>, 256),
        gateway_http_failure_threshold =>
            get_int(Service, <<"gateway_http_failure_threshold">>, 6),
        gateway_http_recovery_timeout_ms =>
            get_int(Service, <<"gateway_http_recovery_timeout_ms">>, 15000),
        gateway_http_cleanup_interval_ms =>
            get_int(Service, <<"gateway_http_cleanup_interval_ms">>, 30000),
        gateway_http_cleanup_max_age_ms =>
            get_int(Service, <<"gateway_http_cleanup_max_age_ms">>, 300000),
        media_proxy_endpoint => get_optional_binary(Service, <<"media_proxy_endpoint">>),
        vapid_email => get_binary(Vapid, <<"email">>, <<>>),
        vapid_public_key => get_optional_binary(Vapid, <<"public_key">>),
        vapid_private_key => get_optional_binary(Vapid, <<"private_key">>),
        gateway_metrics_enabled => get_optional_bool(Service, <<"gateway_metrics_enabled">>),
        gateway_metrics_report_interval_ms =>
            get_optional_int(Service, <<"gateway_metrics_report_interval_ms">>),
        release_node => get_string(Service, <<"release_node">>, "fluxer_gateway@127.0.0.1"),
        logger_level => get_log_level(Service, <<"logger_level">>, info),
        telemetry => #{
            enabled => get_bool(Telemetry, <<"enabled">>, true),
            otlp_endpoint => get_string(Telemetry, <<"otlp_endpoint">>, ""),
            api_key => get_string(Telemetry, <<"api_key">>, ""),
            service_name => get_string(Telemetry, <<"service_name">>, "fluxer-gateway"),
            environment => get_string(Telemetry, <<"environment">>, "development"),
            trace_sampling_ratio => get_float(Telemetry, <<"trace_sampling_ratio">>, 1.0)
        },
        sentry => #{
            build_sha => get_string(Sentry, <<"build_sha">>, ""),
            release_channel => get_string(Sentry, <<"release_channel">>, "")
        }
    }.

-spec get_map(map(), [binary()]) -> map().
get_map(Map, Keys) ->
    case get_in(Map, Keys) of
        Value when is_map(Value) -> Value;
        _ -> #{}
    end.

-spec get_int(map(), binary(), integer()) -> integer().
get_int(Map, Key, Default) when is_integer(Default) ->
    to_int(get_value(Map, Key), Default).

-spec get_optional_int(map(), binary()) -> integer() | undefined.
get_optional_int(Map, Key) ->
    to_optional_int(get_value(Map, Key)).

-spec get_bool(map(), binary(), boolean()) -> boolean().
get_bool(Map, Key, Default) when is_boolean(Default) ->
    to_bool(get_value(Map, Key), Default).

-spec get_optional_bool(map(), binary()) -> boolean() | undefined.
get_optional_bool(Map, Key) ->
    case get_value(Map, Key) of
        undefined -> undefined;
        Value -> to_bool(Value, undefined)
    end.

-spec get_string(map(), binary(), string()) -> string().
get_string(Map, Key, Default) when is_list(Default) ->
    to_string(get_value(Map, Key), Default).

-spec get_binary(map(), binary(), binary() | undefined) -> binary() | undefined.
get_binary(Map, Key, Default) ->
    to_binary(get_value(Map, Key), Default).

-spec get_optional_binary(map(), binary()) -> binary() | undefined.
get_optional_binary(Map, Key) ->
    case get_value(Map, Key) of
        undefined -> undefined;
        Value -> to_binary(Value, undefined)
    end.

-spec get_log_level(map(), binary(), log_level()) -> log_level().
get_log_level(Map, Key, Default) when is_atom(Default) ->
    Value = get_value(Map, Key),
    case normalize_log_level(Value) of
        undefined -> Default;
        Level -> Level
    end.

-spec get_float(map(), binary(), number()) -> float().
get_float(Map, Key, Default) when is_number(Default) ->
    to_float(get_value(Map, Key), Default).

-spec get_in(term(), [binary()]) -> term().
get_in(Map, [Key | Rest]) when is_map(Map) ->
    case get_value(Map, Key) of
        undefined -> undefined;
        Value when Rest =:= [] -> Value;
        Value -> get_in(Value, Rest)
    end;
get_in(_, _) ->
    undefined.

-spec get_value(term(), binary()) -> term().
get_value(Map, Key) when is_map(Map) ->
    case maps:get(Key, Map, undefined) of
        undefined when is_binary(Key) ->
            maps:get(binary_to_list(Key), Map, undefined);
        Value ->
            Value
    end.

-spec to_int(term(), integer() | undefined) -> integer() | undefined.
to_int(Value, _Default) when is_integer(Value) ->
    Value;
to_int(Value, _Default) when is_float(Value) ->
    trunc(Value);
to_int(Value, Default) ->
    case to_string(Value, "") of
        "" ->
            Default;
        Str ->
            case string:to_integer(Str) of
                {Int, _} when is_integer(Int) -> Int;
                {error, _} -> Default
            end
    end.

-spec to_optional_int(term()) -> integer() | undefined.
to_optional_int(Value) ->
    case to_int(Value, undefined) of
        undefined -> undefined;
        Int -> Int
    end.

-spec to_bool(term(), boolean() | undefined) -> boolean() | undefined.
to_bool(Value, _Default) when is_boolean(Value) ->
    Value;
to_bool(Value, Default) when is_atom(Value) ->
    case Value of
        true -> true;
        false -> false;
        _ -> Default
    end;
to_bool(Value, Default) ->
    case string:lowercase(to_string(Value, "")) of
        "true" -> true;
        "1" -> true;
        "false" -> false;
        "0" -> false;
        _ -> Default
    end.

-spec to_string(term(), string()) -> string().
to_string(Value, Default) when is_list(Default) ->
    case Value of
        undefined -> Default;
        Bin when is_binary(Bin) -> binary_to_list(Bin);
        Str when is_list(Str) -> Str;
        Atom when is_atom(Atom) -> atom_to_list(Atom);
        _ -> Default
    end.

-spec to_binary(term(), binary() | undefined) -> binary() | undefined.
to_binary(Value, Default) ->
    case Value of
        undefined -> Default;
        Bin when is_binary(Bin) -> Bin;
        Str when is_list(Str) -> list_to_binary(Str);
        Atom when is_atom(Atom) -> list_to_binary(atom_to_list(Atom));
        _ -> Default
    end.

-spec to_float(term(), float()) -> float().
to_float(Value, _Default) when is_float(Value) ->
    Value;
to_float(Value, _Default) when is_integer(Value) ->
    float(Value);
to_float(Value, Default) ->
    case to_string(Value, "") of
        "" ->
            Default;
        Str ->
            case string:to_float(Str) of
                {Float, _} when is_float(Float) -> Float;
                {error, _} -> Default
            end
    end.

-spec normalize_log_level(term()) -> log_level() | undefined.
normalize_log_level(undefined) ->
    undefined;
normalize_log_level(Level) when is_atom(Level) ->
    normalize_log_level(atom_to_list(Level));
normalize_log_level(Level) when is_binary(Level) ->
    normalize_log_level(binary_to_list(Level));
normalize_log_level(Level) when is_list(Level) ->
    case string:lowercase(string:trim(Level)) of
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
