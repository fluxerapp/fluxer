%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(fluxer_gateway_env).
-typing([eqwalizer]).

-export([load/0, get/1, get_optional/1, get_map/0, patch/1, update/1]).
-export_type([config/0]).

-define(CONFIG_TERM_KEY, {fluxer_gateway, runtime_config}).

-type config() :: map().
-type logger_level() :: debug | info | notice | warning | error | critical | alert | emergency.

-spec load() -> config().
load() ->
    Config = build_config(),
    apply_system_config(Config),
    set_config(Config).

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
    fluxer_gateway_config:load().

-spec apply_system_config(config()) -> ok.
apply_system_config(Config) ->
    LoggerLevel = resolve_logger_level(Config),
    _ = logger:set_primary_config(level, LoggerLevel),
    _ = logger:set_handler_config(default, level, LoggerLevel),
    ok.

-spec resolve_logger_level(config()) -> logger_level().
resolve_logger_level(Config) ->
    Default = normalize_logger_level(maps:get(logger_level, Config, info)),
    case fluxer_gateway_config:env_value("LOGGER_LEVEL") of
        undefined -> Default;
        Value -> parse_logger_level(Value, Default)
    end.

-spec parse_logger_level(string(), logger_level()) -> logger_level().
parse_logger_level(Value, Default) ->
    case string:lowercase(string:trim(Value)) of
        "debug" -> debug;
        "info" -> info;
        "notice" -> notice;
        "warning" -> warning;
        "error" -> error;
        "critical" -> critical;
        "alert" -> alert;
        "emergency" -> emergency;
        _ -> Default
    end.

-spec normalize_logger_level(term()) -> logger_level().
normalize_logger_level(debug) -> debug;
normalize_logger_level(info) -> info;
normalize_logger_level(notice) -> notice;
normalize_logger_level(warning) -> warning;
normalize_logger_level(error) -> error;
normalize_logger_level(critical) -> critical;
normalize_logger_level(alert) -> alert;
normalize_logger_level(emergency) -> emergency;
normalize_logger_level(_) -> info.
