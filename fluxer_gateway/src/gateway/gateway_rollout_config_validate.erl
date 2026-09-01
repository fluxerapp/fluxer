%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rollout_config_validate).
-typing([eqwalizer]).

-export([validate/2]).

-type field_kind() ::
    percentage
    | positive_integer
    | relay_max_queue
    | rpc_timeout
    | reconcile_interval
    | concurrency
    | rollout_mode
    | voice_e2ee_scope
    | invalid.

-spec validate(term(), map()) -> {ok, map()} | {error, term()}.
validate(Config, Defaults) when is_map(Config) ->
    Merged = maps:merge(Defaults, maps:with(maps:keys(Defaults), Config)),
    case validate_fields(config_fields(), Merged) of
        ok -> {ok, Merged};
        {error, _Reason} = Error -> Error
    end;
validate(_, _Defaults) ->
    {error, invalid_config}.

-spec config_fields() -> [binary()].
config_fields() ->
    [
        <<"session_rollout_percentage">>,
        <<"session_rollout_mode">>,
        <<"guild_rollout_percentage">>,
        <<"rpc_request_timeout_ms">>,
        <<"max_concurrent_session_starts">>,
        <<"max_concurrent_guild_starts">>,
        <<"gateway_dispatch_relay_shards">>,
        <<"gateway_dispatch_relay_max_queue">>,
        <<"voice_e2ee_scope">>,
        <<"voice_reconciliation_v3_percentage">>,
        <<"voice_reconciliation_v3_interval_ms">>
    ].

-spec validate_fields([binary()], map()) -> ok | {error, term()}.
validate_fields([], _Config) ->
    ok;
validate_fields([Key | Rest], Config) ->
    Value = maps:get(Key, Config),
    case valid_config_field(Key, Value) of
        true -> validate_fields(Rest, Config);
        false -> {error, {invalid_field, Key, Value}}
    end.

-spec valid_config_field(binary(), term()) -> boolean().
valid_config_field(Key, Value) ->
    case config_field_kind(Key) of
        percentage ->
            valid_percentage_value(Value);
        positive_integer ->
            is_integer(Value) andalso Value > 0;
        relay_max_queue ->
            valid_relay_max_queue_value(Value);
        rpc_timeout ->
            is_integer(Value) andalso Value >= 1000 andalso Value =< 60000;
        reconcile_interval ->
            is_integer(Value) andalso Value >= 500 andalso Value =< 60000;
        concurrency ->
            is_integer(Value) andalso Value >= 1 andalso Value =< 10000;
        rollout_mode ->
            Value =:= <<"modulo">> orelse Value =:= <<"random">>;
        voice_e2ee_scope ->
            Value =:= <<"guild_feature_only">> orelse
                Value =:= <<"platform_wide">>;
        invalid ->
            false
    end.

-spec config_field_kind(binary()) -> field_kind().
config_field_kind(<<"session_rollout_percentage">>) -> percentage;
config_field_kind(<<"guild_rollout_percentage">>) -> percentage;
config_field_kind(<<"voice_reconciliation_v3_percentage">>) -> percentage;
config_field_kind(<<"gateway_dispatch_relay_shards">>) -> positive_integer;
config_field_kind(<<"gateway_dispatch_relay_max_queue">>) -> relay_max_queue;
config_field_kind(<<"rpc_request_timeout_ms">>) -> rpc_timeout;
config_field_kind(<<"voice_reconciliation_v3_interval_ms">>) -> reconcile_interval;
config_field_kind(<<"max_concurrent_session_starts">>) -> concurrency;
config_field_kind(<<"max_concurrent_guild_starts">>) -> concurrency;
config_field_kind(<<"session_rollout_mode">>) -> rollout_mode;
config_field_kind(<<"voice_e2ee_scope">>) -> voice_e2ee_scope;
config_field_kind(_) -> invalid.

-spec valid_percentage_value(term()) -> boolean().
valid_percentage_value(Value) ->
    is_number(Value) andalso Value >= 0 andalso Value =< 100.

-spec valid_relay_max_queue_value(term()) -> boolean().
valid_relay_max_queue_value(Value) ->
    is_integer(Value) andalso Value >= 1 andalso
        Value =< process_health_watchdog:kill_threshold().
