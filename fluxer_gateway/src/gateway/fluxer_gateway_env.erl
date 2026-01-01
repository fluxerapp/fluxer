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

-module(fluxer_gateway_env).

-export([load/0, get/1, get_optional/1, get_map/0, patch/1, update/1]).

-define(APP, fluxer_gateway).
-define(CONFIG_TERM_KEY, {fluxer_gateway, runtime_config}).

-type config() :: map().

-spec load() -> config().
load() ->
    set_config(build_config()).

-spec get(atom()) -> term().
get(Key) when is_atom(Key) ->
    Map = get_map(),
    maps:get(Key, Map, undefined).

-spec get_optional(atom()) -> term().
get_optional(Key) when is_atom(Key) ->
    ?MODULE:get(Key).

-spec get_map() -> config().
get_map() ->
    ensure_loaded().

-spec patch(map()) -> config().
patch(Patch) when is_map(Patch) ->
    Map = get_map(),
    set_config(maps:merge(Map, Patch)).

-spec update(fun((config()) -> config())) -> config().
update(Fun) when is_function(Fun, 1) ->
    Map = get_map(),
    set_config(Fun(Map)).

-spec set_config(config()) -> config().
set_config(Config) when is_map(Config) ->
    persistent_term:put(?CONFIG_TERM_KEY, Config),
    Config.

-spec ensure_loaded() -> config().
ensure_loaded() ->
    case persistent_term:get(?CONFIG_TERM_KEY, undefined) of
        Map when is_map(Map) ->
            Map;
        _ ->
            load()
    end.

-spec build_config() -> config().
build_config() ->
    #{
        ws_port => env_int("FLUXER_GATEWAY_WS_PORT", ws_port, 8080),
        rpc_port => env_int("FLUXER_GATEWAY_RPC_PORT", rpc_port, 8081),
        api_host => env_string("API_HOST", api_host, "api"),
        api_canary_host => env_optional_string("API_CANARY_HOST", api_canary_host),
        rpc_secret_key => env_binary("GATEWAY_RPC_SECRET", rpc_secret_key, undefined),
        identify_rate_limit_enabled => env_bool("FLUXER_GATEWAY_IDENTIFY_RATE_LIMIT_ENABLED", identify_rate_limit_enabled, false),
        push_enabled => env_bool("FLUXER_GATEWAY_PUSH_ENABLED", push_enabled, true),
        push_user_guild_settings_cache_mb => env_int("FLUXER_GATEWAY_PUSH_USER_GUILD_SETTINGS_CACHE_MB",
                                                    push_user_guild_settings_cache_mb, 1024),
        push_subscriptions_cache_mb => env_int("FLUXER_GATEWAY_PUSH_SUBSCRIPTIONS_CACHE_MB",
                                              push_subscriptions_cache_mb, 1024),
        push_blocked_ids_cache_mb => env_int("FLUXER_GATEWAY_PUSH_BLOCKED_IDS_CACHE_MB",
                                             push_blocked_ids_cache_mb, 1024),
        presence_cache_shards => env_optional_int("FLUXER_GATEWAY_PRESENCE_CACHE_SHARDS", presence_cache_shards),
        presence_bus_shards => env_optional_int("FLUXER_GATEWAY_PRESENCE_BUS_SHARDS", presence_bus_shards),
        presence_shards => env_optional_int("FLUXER_GATEWAY_PRESENCE_SHARDS", presence_shards),
        guild_shards => env_optional_int("FLUXER_GATEWAY_GUILD_SHARDS", guild_shards),
        metrics_host => env_optional_string("FLUXER_METRICS_HOST", metrics_host),
        push_badge_counts_cache_mb => app_env_int(push_badge_counts_cache_mb, 256),
        push_badge_counts_cache_ttl_seconds => app_env_int(push_badge_counts_cache_ttl_seconds, 60),
        media_proxy_endpoint => env_optional_binary("MEDIA_PROXY_ENDPOINT", media_proxy_endpoint),
        vapid_email => env_binary("VAPID_EMAIL", vapid_email, <<"support@fluxer.app">>),
        vapid_public_key => env_binary("VAPID_PUBLIC_KEY", vapid_public_key, undefined),
        vapid_private_key => env_binary("VAPID_PRIVATE_KEY", vapid_private_key, undefined),
        gateway_metrics_enabled => app_env_optional_bool(gateway_metrics_enabled),
        gateway_metrics_report_interval_ms => app_env_optional_int(gateway_metrics_report_interval_ms)
    }.

-spec env_int(string(), atom(), integer()) -> integer().
env_int(EnvVar, AppKey, Default) when is_atom(AppKey), is_integer(Default) ->
    case os:getenv(EnvVar) of
        false ->
            app_env_int(AppKey, Default);
        Value ->
            parse_int(Value, Default)
    end.

-spec env_optional_int(string(), atom()) -> integer() | undefined.
env_optional_int(EnvVar, AppKey) when is_atom(AppKey) ->
    case os:getenv(EnvVar) of
        false ->
            app_env_optional_int(AppKey);
        Value ->
            parse_int(Value, undefined)
    end.

-spec env_bool(string(), atom(), boolean()) -> boolean().
env_bool(EnvVar, AppKey, Default) when is_atom(AppKey), is_boolean(Default) ->
    case os:getenv(EnvVar) of
        false ->
            app_env_bool(AppKey, Default);
        Value ->
            parse_bool(Value, Default)
    end.

-spec env_string(string(), atom(), string()) -> string().
env_string(EnvVar, AppKey, Default) when is_atom(AppKey) ->
    case os:getenv(EnvVar) of
        false ->
            app_env_string(AppKey, Default);
        Value ->
            Value
    end.

-spec env_optional_string(string(), atom()) -> string() | undefined.
env_optional_string(EnvVar, AppKey) when is_atom(AppKey) ->
    case os:getenv(EnvVar) of
        false ->
            app_env_optional_string(AppKey);
        Value ->
            Value
    end.

-spec env_binary(string(), atom(), binary() | undefined) -> binary() | undefined.
env_binary(EnvVar, AppKey, Default) when is_atom(AppKey) ->
    case os:getenv(EnvVar) of
        false ->
            app_env_binary(AppKey, Default);
        Value ->
            to_binary(Value, Default)
    end.

-spec env_optional_binary(string(), atom()) -> binary() | undefined.
env_optional_binary(EnvVar, AppKey) when is_atom(AppKey) ->
    case os:getenv(EnvVar) of
        false ->
            app_env_optional_binary(AppKey);
        Value ->
            to_binary(Value, undefined)
    end.

-spec parse_int(string(), integer() | undefined) -> integer() | undefined.
parse_int(Value, Default) ->
    Str = string:trim(Value),
    try
        list_to_integer(Str)
    catch
        _:_ -> Default
    end.

-spec parse_bool(string(), boolean()) -> boolean().
parse_bool(Value, Default) ->
    Str = string:lowercase(string:trim(Value)),
    case Str of
        "true" -> true;
        "1" -> true;
        "false" -> false;
        "0" -> false;
        _ -> Default
    end.

-spec to_binary(string(), binary() | undefined) -> binary() | undefined.
to_binary(Value, Default) ->
    try
        list_to_binary(Value)
    catch
        _:_ -> Default
    end.

-spec app_env_int(atom(), integer()) -> integer().
app_env_int(Key, Default) ->
    case application:get_env(?APP, Key) of
        {ok, Value} when is_integer(Value) ->
            Value;
        _ ->
            Default
    end.

-spec app_env_optional_int(atom()) -> integer() | undefined.
app_env_optional_int(Key) ->
    case application:get_env(?APP, Key) of
        {ok, Value} when is_integer(Value) ->
            Value;
        _ ->
            undefined
    end.

-spec app_env_bool(atom(), boolean()) -> boolean().
app_env_bool(Key, Default) ->
    case application:get_env(?APP, Key) of
        {ok, Value} when is_boolean(Value) ->
            Value;
        _ ->
            Default
    end.

-spec app_env_optional_bool(atom()) -> boolean() | undefined.
app_env_optional_bool(Key) ->
    case application:get_env(?APP, Key) of
        {ok, Value} when is_boolean(Value) ->
            Value;
        _ ->
            undefined
    end.

-spec app_env_string(atom(), string()) -> string().
app_env_string(Key, Default) ->
    case application:get_env(?APP, Key) of
        {ok, Value} when is_list(Value) ->
            Value;
        {ok, Value} when is_binary(Value) ->
            binary_to_list(Value);
        _ ->
            Default
    end.

-spec app_env_optional_string(atom()) -> string() | undefined.
app_env_optional_string(Key) ->
    case application:get_env(?APP, Key) of
        {ok, Value} when is_list(Value) ->
            Value;
        {ok, Value} when is_binary(Value) ->
            binary_to_list(Value);
        _ ->
            undefined
    end.

-spec app_env_binary(atom(), binary() | undefined) -> binary() | undefined.
app_env_binary(Key, Default) ->
    case application:get_env(?APP, Key) of
        {ok, Value} when is_binary(Value) ->
            Value;
        {ok, Value} when is_list(Value) ->
            list_to_binary(Value);
        _ ->
            Default
    end.

-spec app_env_optional_binary(atom()) -> binary() | undefined.
app_env_optional_binary(Key) ->
    case application:get_env(?APP, Key) of
        {ok, Value} when is_binary(Value) ->
            Value;
        {ok, Value} when is_list(Value) ->
            list_to_binary(Value);
        _ ->
            undefined
    end.
