%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_delivery_config).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([
    start_link/0,
    config/0,
    config_version/0,
    update_counts/0,
    partition_users/1,
    partition_users/2,
    is_enrolled/1,
    bucket/2
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PERSISTENT_TERM_KEY, push_delivery_config).
-define(UPDATE_COUNTS_KEY, {push_delivery_config, update_counts}).
-define(NATS_SUBJECT, <<"config.push.delivery">>).
-define(FETCH_DELAY_MS, 2000).
-define(RECONCILE_INTERVAL_MS, 30000).
-define(NATS_SUBSCRIBE_RETRY_MS, 2000).
-define(RESOLUTION, 10000).
-define(MAX_TARGETED_USERS, 1000).
-define(MAX_SALT_BYTES, 64).
-define(MAX_USER_ID_DIGITS, 20).
-define(DEFAULT_SALT, <<"push-service-delivery-v1">>).
-define(FNV_OFFSET_BASIS_32, 16#811c9dc5).
-define(FNV_PRIME_32, 16#01000193).

-type user_id_set() :: #{binary() => true}.
-type config() :: #{
    enabled := boolean(),
    config_version := non_neg_integer(),
    rollout_basis_points := non_neg_integer(),
    rollout_salt := binary(),
    included := user_id_set(),
    excluded := user_id_set()
}.
-type state() :: #{
    nats_subscription := term(),
    nats_monitor := reference() | undefined,
    fetch_timer := reference() | undefined
}.
-type store_result() :: updated | unchanged | stale | rejected.

-export_type([config/0]).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec config() -> config().
config() ->
    case persistent_term:get(?PERSISTENT_TERM_KEY, undefined) of
        Config when is_map(Config) -> Config;
        _ -> default_config()
    end.

-spec config_version() -> non_neg_integer().
config_version() ->
    maps:get(config_version, config(), 0).

-spec update_counts() -> #{store_result() => non_neg_integer()}.
update_counts() ->
    Counters = persistent_term:get(?UPDATE_COUNTS_KEY, undefined),
    maps:from_list([{Result, read_count(Counters, Result)} || Result <- store_results()]).

-spec partition_users([integer()]) -> {[integer()], [integer()]}.
partition_users(UserIds) ->
    partition_users(config(), UserIds).

-spec partition_users(config(), [integer()]) -> {[integer()], [integer()]}.
partition_users(Config, UserIds) ->
    partition_enrolled(maps:get(enabled, Config, false), Config, UserIds).

-spec partition_enrolled(boolean(), config(), [integer()]) -> {[integer()], [integer()]}.
partition_enrolled(true, Config, UserIds) ->
    lists:partition(fun(UserId) -> enrolled(Config, integer_to_binary(UserId)) end, UserIds);
partition_enrolled(_Enabled, _Config, UserIds) ->
    {[], UserIds}.

-spec is_enrolled(integer()) -> boolean().
is_enrolled(UserId) ->
    Config = config(),
    is_enrolled(maps:get(enabled, Config, false), Config, UserId).

-spec is_enrolled(boolean(), config(), integer()) -> boolean().
is_enrolled(true, Config, UserId) ->
    enrolled(Config, integer_to_binary(UserId));
is_enrolled(_Enabled, _Config, _UserId) ->
    false.

-spec bucket(binary(), binary()) -> non_neg_integer().
bucket(UserId, Salt) ->
    hash(<<Salt/binary, ":", UserId/binary>>, ?FNV_OFFSET_BASIS_32) rem ?RESOLUTION.

-spec enrolled(config(), binary()) -> boolean().
enrolled(Config, UserId) ->
    case maps:is_key(UserId, maps:get(excluded, Config, #{})) of
        true ->
            false;
        false ->
            maps:is_key(UserId, maps:get(included, Config, #{})) orelse
                bucket(UserId, maps:get(rollout_salt, Config, ?DEFAULT_SALT)) <
                    maps:get(rollout_basis_points, Config, 0)
    end.

-spec hash(binary(), non_neg_integer()) -> non_neg_integer().
hash(<<>>, Hash) ->
    Hash;
hash(<<Byte:8, Rest/binary>>, Hash) ->
    hash(Rest, ((Hash bxor Byte) * ?FNV_PRIME_32) band 16#ffffffff).

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:process_flag(fullsweep_after, 50),
    persistent_term:put(?PERSISTENT_TERM_KEY, config()),
    self() ! subscribe_nats,
    {ok, #{
        nats_subscription => undefined,
        nats_monitor => undefined,
        fetch_timer => erlang:send_after(?FETCH_DELAY_MS, self(), fetch_config)
    }}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(subscribe_nats, State) ->
    {noreply, subscribe_to_nats(State)};
handle_info(fetch_config, State) ->
    count(fetch_config_from_api()),
    {noreply, State#{
        fetch_timer => erlang:send_after(?RECONCILE_INTERVAL_MS, self(), fetch_config)
    }};
handle_info({nats_resubscribed, ?NATS_SUBJECT}, State) ->
    count(fetch_config_from_api()),
    {noreply, State};
handle_info({nats_msg, ?NATS_SUBJECT, Payload, _ReplyTo}, State) when is_binary(Payload) ->
    count(apply_nats_payload(Payload)),
    {noreply, State};
handle_info({'DOWN', MonRef, process, _Pid, _Reason}, #{nats_monitor := MonRef} = State) ->
    erlang:send_after(?NATS_SUBSCRIBE_RETRY_MS, self(), subscribe_nats),
    {noreply, State#{nats_subscription => undefined, nats_monitor => undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec default_config() -> config().
default_config() ->
    #{
        enabled => false,
        config_version => 0,
        rollout_basis_points => 0,
        rollout_salt => ?DEFAULT_SALT,
        included => #{},
        excluded => #{}
    }.

-spec fetch_config_from_api() -> store_result().
fetch_config_from_api() ->
    RpcRequest = #{<<"type">> => <<"get_push_service_delivery_config">>},
    case api_rpc_client:call(RpcRequest) of
        {ok, #{<<"config">> := Config}} when is_map(Config) ->
            store_valid_config(Config, api);
        {ok, _Other} ->
            logger:warning("Push delivery config: unexpected API response format"),
            rejected;
        {error, Reason} ->
            logger:warning("Push delivery config failed to fetch from API", #{reason => Reason}),
            rejected
    end.

-spec apply_nats_payload(binary()) -> store_result().
apply_nats_payload(Payload) ->
    try json:decode(Payload) of
        #{<<"type">> := <<"push_service_delivery_config">>, <<"config">> := Config} when
            is_map(Config)
        ->
            store_valid_config(Config, nats);
        #{<<"config">> := Config} when is_map(Config) ->
            store_valid_config(Config, nats);
        Config when is_map(Config) ->
            store_valid_config(Config, nats);
        _Other ->
            logger:warning("Push delivery config: unexpected NATS payload format"),
            rejected
    catch
        Class:Reason ->
            logger:warning("Push delivery config failed to decode NATS payload", #{
                class => Class, reason => Reason
            }),
            rejected
    end.

-spec store_valid_config(map(), api | nats) -> store_result().
store_valid_config(Config, Source) ->
    case validate_config(Config) of
        {ok, Validated} ->
            store_validated_config(config(), Validated, Source);
        {error, Reason} ->
            logger:warning("Push delivery config rejected invalid config: ~p", [Reason]),
            rejected
    end.

-spec store_validated_config(config(), config(), api | nats) -> store_result().
store_validated_config(Previous, Previous, _Source) ->
    unchanged;
store_validated_config(
    #{config_version := Held}, #{config_version := Offered, enabled := true}, Source
) when
    Offered < Held
->
    logger:warning("Push delivery config ignored a lower config_version", #{
        source => Source, held => Held, offered => Offered
    }),
    stale;
store_validated_config(Previous, Current, Source) ->
    log_config_transitions(Previous, Current),
    persistent_term:put(?PERSISTENT_TERM_KEY, Current),
    logger:info("Push delivery config updated", #{source => Source}),
    ok = push_outbox:delivery_config_changed(),
    updated.

-spec validate_config(map()) -> {ok, config()} | {error, term()}.
validate_config(Wire) ->
    lists:foldl(
        fun(Field, Acc) -> validate_field(Field, Wire, Acc) end,
        {ok, default_config()},
        config_fields()
    ).

-spec config_fields() -> [{atom(), binary(), fun((term()) -> {ok, term()} | error)}].
config_fields() ->
    [
        {enabled, <<"enabled">>, fun validate_enabled/1},
        {config_version, <<"config_version">>, fun validate_config_version/1},
        {rollout_basis_points, <<"rollout_basis_points">>, fun validate_basis_points/1},
        {rollout_salt, <<"rollout_salt">>, fun validate_salt/1},
        {included, <<"included_user_ids">>, fun validate_user_ids/1},
        {excluded, <<"excluded_user_ids">>, fun validate_user_ids/1}
    ].

-spec validate_field(
    {atom(), binary(), fun((term()) -> {ok, term()} | error)},
    map(),
    {ok, config()} | {error, term()}
) -> {ok, config()} | {error, term()}.
validate_field(_Field, _Wire, {error, _} = Error) ->
    Error;
validate_field({Key, WireKey, Validate}, Wire, {ok, Acc}) ->
    case maps:find(WireKey, Wire) of
        error -> {ok, Acc};
        {ok, Value} -> store_validated_field(Key, WireKey, Value, Validate(Value), Acc)
    end.

-spec store_validated_field(atom(), binary(), term(), {ok, term()} | error, config()) ->
    {ok, config()} | {error, term()}.
store_validated_field(Key, _WireKey, _Value, {ok, Normalised}, Acc) ->
    {ok, Acc#{Key => Normalised}};
store_validated_field(_Key, WireKey, Value, error, _Acc) ->
    {error, {invalid_field, WireKey, Value}}.

-spec validate_enabled(term()) -> {ok, boolean()} | error.
validate_enabled(Value) when is_boolean(Value) ->
    {ok, Value};
validate_enabled(_Value) ->
    error.

-spec validate_config_version(term()) -> {ok, non_neg_integer()} | error.
validate_config_version(Value) when is_integer(Value), Value >= 0 ->
    {ok, Value};
validate_config_version(_Value) ->
    error.

-spec validate_basis_points(term()) -> {ok, non_neg_integer()} | error.
validate_basis_points(Value) when is_integer(Value), Value >= 0, Value =< ?RESOLUTION ->
    {ok, Value};
validate_basis_points(_Value) ->
    error.

-spec validate_salt(term()) -> {ok, binary()} | error.
validate_salt(Value) when is_binary(Value) ->
    validate_salt_bytes(Value, byte_size(Value));
validate_salt(_Value) ->
    error.

-spec validate_salt_bytes(binary(), non_neg_integer()) -> {ok, binary()} | error.
validate_salt_bytes(Value, Size) when Size >= 1, Size =< ?MAX_SALT_BYTES ->
    validate_printable_ascii(Value, Value);
validate_salt_bytes(_Value, _Size) ->
    error.

-spec validate_printable_ascii(binary(), binary()) -> {ok, binary()} | error.
validate_printable_ascii(<<>>, Value) ->
    {ok, Value};
validate_printable_ascii(<<Byte:8, Rest/binary>>, Value) when Byte >= 16#20, Byte =< 16#7e ->
    validate_printable_ascii(Rest, Value);
validate_printable_ascii(_Remaining, _Value) ->
    error.

-spec validate_user_ids(term()) -> {ok, user_id_set()} | error.
validate_user_ids(Value) when is_list(Value), length(Value) =< ?MAX_TARGETED_USERS ->
    collect_user_ids(Value, #{});
validate_user_ids(_Value) ->
    error.

-spec collect_user_ids([term()], user_id_set()) -> {ok, user_id_set()} | error.
collect_user_ids([], Acc) ->
    {ok, Acc};
collect_user_ids([UserId | Rest], Acc) when is_binary(UserId) ->
    collect_valid_user_id(UserId, is_user_id(UserId, byte_size(UserId)), Rest, Acc);
collect_user_ids(_UserIds, _Acc) ->
    error.

-spec collect_valid_user_id(binary(), boolean(), [term()], user_id_set()) ->
    {ok, user_id_set()} | error.
collect_valid_user_id(UserId, true, Rest, Acc) ->
    collect_user_ids(Rest, Acc#{UserId => true});
collect_valid_user_id(_UserId, false, _Rest, _Acc) ->
    error.

-spec is_user_id(binary(), non_neg_integer()) -> boolean().
is_user_id(UserId, Size) when Size >= 1, Size =< ?MAX_USER_ID_DIGITS ->
    is_all_digits(UserId);
is_user_id(_UserId, _Size) ->
    false.

-spec is_all_digits(binary()) -> boolean().
is_all_digits(<<>>) ->
    true;
is_all_digits(<<Byte:8, Rest/binary>>) when Byte >= $0, Byte =< $9 ->
    is_all_digits(Rest);
is_all_digits(_Remaining) ->
    false.

-spec subscribe_to_nats(state()) -> state().
subscribe_to_nats(#{nats_subscription := Sid} = State) when Sid =/= undefined ->
    State;
subscribe_to_nats(State) ->
    case subscribe_to_delivery_subject() of
        {ok, Sid} ->
            MonRef = monitor_nats_rpc(),
            logger:info("Push delivery config subscribed to NATS", #{subject => ?NATS_SUBJECT}),
            count(fetch_config_from_api()),
            State#{nats_subscription => Sid, nats_monitor => MonRef};
        {error, Reason} ->
            logger:debug("Push delivery config waiting for NATS subscription", #{
                subject => ?NATS_SUBJECT, reason => Reason
            }),
            erlang:send_after(?NATS_SUBSCRIBE_RETRY_MS, self(), subscribe_nats),
            State
    end.

-spec subscribe_to_delivery_subject() -> {ok, term()} | {error, term()}.
subscribe_to_delivery_subject() ->
    try gateway_nats_rpc:subscribe(?NATS_SUBJECT, <<>>) of
        {ok, Sid} -> {ok, Sid};
        {error, Reason} -> {error, Reason}
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

-spec monitor_nats_rpc() -> reference() | undefined.
monitor_nats_rpc() ->
    case whereis(gateway_nats_rpc) of
        Pid when is_pid(Pid) -> erlang:monitor(process, Pid);
        _ -> undefined
    end.

-spec count(store_result()) -> ok.
count(Result) ->
    counters:add(update_counters(), result_index(Result), 1).

-spec update_counters() -> counters:counters_ref().
update_counters() ->
    case persistent_term:get(?UPDATE_COUNTS_KEY, undefined) of
        undefined ->
            Counters = counters:new(length(store_results()), [atomics]),
            persistent_term:put(?UPDATE_COUNTS_KEY, Counters),
            Counters;
        Counters ->
            Counters
    end.

-spec read_count(counters:counters_ref() | undefined, store_result()) -> non_neg_integer().
read_count(undefined, _Result) ->
    0;
read_count(Counters, Result) ->
    counters:get(Counters, result_index(Result)).

-spec store_results() -> [store_result()].
store_results() ->
    [updated, unchanged, stale, rejected].

-spec result_index(store_result()) -> pos_integer().
result_index(updated) -> 1;
result_index(unchanged) -> 2;
result_index(stale) -> 3;
result_index(rejected) -> 4.

-spec log_config_transitions(config(), config()) -> ok.
log_config_transitions(Previous, Current) ->
    lists:foreach(
        fun(Key) -> log_key_transition(Key, Previous, Current) end,
        [enabled, rollout_basis_points, config_version]
    ).

-spec log_key_transition(atom(), config(), config()) -> ok.
log_key_transition(Key, Previous, Current) ->
    PreviousValue = maps:get(Key, Previous, undefined),
    CurrentValue = maps:get(Key, Current, undefined),
    case PreviousValue =:= CurrentValue of
        true ->
            ok;
        false ->
            logger:notice(
                "Push delivery config transition: key=~s previous=~p current=~p",
                [Key, PreviousValue, CurrentValue]
            )
    end.
