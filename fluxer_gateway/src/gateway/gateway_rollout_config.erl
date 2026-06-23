%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rollout_config).
-typing([eqwalizer]).
-behaviour(gen_server).
-compile({no_auto_import, [get/0]}).

-export([
    start_link/0,
    get/0,
    is_clustered/0,
    session_rollout_percentage/0,
    session_rollout_mode/0,
    guild_rollout_percentage/0,
    voice_e2ee_scope/0,
    gateway_dispatch_relay_shards/0,
    gateway_dispatch_relay_max_queue/0,
    rpc_request_timeout_ms/0,
    voice_reconciliation_v3_percentage/0,
    voice_reconciliation_v3_interval_ms/0,
    max_concurrent_session_starts/0,
    max_concurrent_guild_starts/0,
    is_session_eligible/1,
    is_guild_eligible/1,
    session_start_rollout_decision/1,
    subscribe_changes/1,
    unsubscribe_changes/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PERSISTENT_TERM_KEY, gateway_rollout_config).
-define(FETCH_DELAY_MS, 2000).
-define(NATS_SUBSCRIBE_RETRY_MS, 2000).
-define(NATS_SUBJECT, <<"config.gateway.rollout">>).

-type state() :: #{
    subscribers := [{pid(), reference()}],
    nats_subscription := term(),
    nats_monitor := reference() | undefined
}.
-type store_result() :: updated | unchanged | rejected.
-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get() -> map().
get() ->
    try
        persistent_term:get(?PERSISTENT_TERM_KEY)
    catch
        error:badarg -> default_config()
    end.

-spec is_clustered() -> boolean().
is_clustered() ->
    case fluxer_gateway_env:get(cluster_enabled) of
        true -> cluster_member_count() > 1;
        _ -> false
    end.

-spec cluster_member_count() -> non_neg_integer().
cluster_member_count() ->
    case persistent_term:get({gateway_cluster_membership, members}, undefined) of
        undefined -> 1;
        Members when is_list(Members) -> length(Members)
    end.

-spec session_rollout_percentage() -> number().
session_rollout_percentage() ->
    maps:get(<<"session_rollout_percentage">>, get(), 100).

-spec session_rollout_mode() -> modulo | random.
session_rollout_mode() ->
    case maps:get(<<"session_rollout_mode">>, get(), <<"modulo">>) of
        <<"random">> -> random;
        _ -> modulo
    end.

-spec guild_rollout_percentage() -> number().
guild_rollout_percentage() ->
    maps:get(<<"guild_rollout_percentage">>, get(), 100).

-spec voice_e2ee_scope() -> guild_feature_only | platform_wide.
voice_e2ee_scope() ->
    case maps:get(<<"voice_e2ee_scope">>, get(), <<"guild_feature_only">>) of
        <<"platform_wide">> -> platform_wide;
        _ -> guild_feature_only
    end.

-spec gateway_dispatch_relay_shards() -> pos_integer().
gateway_dispatch_relay_shards() ->
    max(1, maps:get(<<"gateway_dispatch_relay_shards">>, get(), 32)).

-spec gateway_dispatch_relay_max_queue() -> non_neg_integer().
gateway_dispatch_relay_max_queue() ->
    max(0, maps:get(<<"gateway_dispatch_relay_max_queue">>, get(), 50000)).

-spec rpc_request_timeout_ms() -> integer().
rpc_request_timeout_ms() ->
    maps:get(<<"rpc_request_timeout_ms">>, get(), 10000).

-spec voice_reconciliation_v3_percentage() -> number().
voice_reconciliation_v3_percentage() ->
    maps:get(<<"voice_reconciliation_v3_percentage">>, get(), 100).

-spec voice_reconciliation_v3_interval_ms() -> integer().
voice_reconciliation_v3_interval_ms() ->
    maps:get(<<"voice_reconciliation_v3_interval_ms">>, get(), 2000).

-spec max_concurrent_session_starts() -> integer().
max_concurrent_session_starts() ->
    maps:get(<<"max_concurrent_session_starts">>, get(), 512).

-spec max_concurrent_guild_starts() -> integer().
max_concurrent_guild_starts() ->
    maps:get(<<"max_concurrent_guild_starts">>, get(), 256).

-spec is_session_eligible(binary()) -> boolean().
is_session_eligible(UserId) ->
    check_eligibility(UserId, session_rollout_percentage(), session_rollout_mode()).

-spec is_guild_eligible(binary()) -> boolean().
is_guild_eligible(GuildId) ->
    check_eligibility(GuildId, guild_rollout_percentage(), modulo).

-spec session_start_rollout_decision(map()) -> eligible | not_eligible | missing_user_id.
session_start_rollout_decision(ApiData) ->
    Percentage = session_rollout_percentage(),
    Mode = session_rollout_mode(),
    case Percentage >= 100 of
        true ->
            eligible;
        false ->
            check_user_eligibility(ApiData, Percentage, Mode)
    end.

-spec check_user_eligibility(map(), number(), modulo | random) ->
    eligible | not_eligible | missing_user_id.
check_user_eligibility(ApiData, Percentage, Mode) ->
    case maps:get(<<"user_id">>, ApiData, undefined) of
        undefined ->
            missing_user_id;
        UserId ->
            eligibility_result(check_eligibility(UserId, Percentage, Mode))
    end.

-spec eligibility_result(boolean()) -> eligible | not_eligible.
eligibility_result(true) ->
    eligible;
eligibility_result(false) ->
    not_eligible.

-spec subscribe_changes(pid()) -> ok.
subscribe_changes(Pid) ->
    gen_server:cast(?MODULE, {subscribe, Pid}).

-spec unsubscribe_changes(pid()) -> ok.
unsubscribe_changes(Pid) ->
    gen_server:cast(?MODULE, {unsubscribe, Pid}).

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:process_flag(fullsweep_after, 50),
    persistent_term:put(?PERSISTENT_TERM_KEY, initial_config()),
    self() ! subscribe_nats,
    erlang:send_after(?FETCH_DELAY_MS, self(), fetch_initial_config),
    {ok, #{subscribers => [], nats_subscription => undefined, nats_monitor => undefined}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({subscribe, Pid}, #{subscribers := Subs} = State) when is_pid(Pid) ->
    case lists:keyfind(Pid, 1, Subs) of
        false ->
            MonRef = erlang:monitor(process, Pid),
            {noreply, State#{subscribers => [{Pid, MonRef} | Subs]}};
        _ ->
            {noreply, State}
    end;
handle_cast({unsubscribe, Pid}, #{subscribers := Subs} = State) when is_pid(Pid) ->
    case lists:keytake(Pid, 1, Subs) of
        {value, {Pid, MonRef}, Rest} ->
            erlang:demonitor(MonRef, [flush]),
            {noreply, State#{subscribers => Rest}};
        false ->
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(subscribe_nats, State) ->
    {noreply, subscribe_to_nats(State)};
handle_info(fetch_initial_config, State) ->
    maybe_notify_subscribers(fetch_config_from_api(), State),
    {noreply, State};
handle_info({nats_msg, ?NATS_SUBJECT, Payload, _ReplyTo}, State) when is_binary(Payload) ->
    maybe_notify_subscribers(apply_nats_payload(Payload), State),
    {noreply, State};
handle_info({'DOWN', MonRef, process, _Pid, _Reason}, #{nats_monitor := MonRef} = State) ->
    erlang:send_after(?NATS_SUBSCRIBE_RETRY_MS, self(), subscribe_nats),
    {noreply, State#{nats_subscription => undefined, nats_monitor => undefined}};
handle_info({'DOWN', _MonRef, process, Pid, _Reason}, #{subscribers := Subs} = State) ->
    {noreply, State#{subscribers => lists:keydelete(Pid, 1, Subs)}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec check_eligibility(binary(), number(), modulo | random) -> boolean().
check_eligibility(_Id, Percentage, _Mode) when Percentage >= 100 ->
    true;
check_eligibility(_Id, Percentage, _Mode) when Percentage =< 0 ->
    false;
check_eligibility(Id, Percentage, modulo) ->
    erlang:phash2(Id, 100) < Percentage;
check_eligibility(_Id, Percentage, random) ->
    rand:uniform(100) =< Percentage.

-spec default_config() -> map().
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

-spec initial_config() -> map().
initial_config() ->
    Existing = persistent_term:get(?PERSISTENT_TERM_KEY, undefined),
    validated_or_default(Existing).

-spec validated_or_default(term()) -> map().
validated_or_default(Config) ->
    case validate_config(Config) of
        {ok, Validated} -> Validated;
        {error, _Reason} -> default_config()
    end.

-spec validate_config(term()) -> {ok, map()} | {error, term()}.
validate_config(Config) ->
    gateway_rollout_config_validate:validate(Config, default_config()).

-spec fetch_config_from_api() -> store_result().
fetch_config_from_api() ->
    RpcRequest = #{<<"type">> => <<"get_gateway_rollout_config">>},
    case api_rpc_client:call(RpcRequest) of
        {ok, #{<<"config">> := Config}} when is_map(Config) ->
            store_valid_config(Config, api);
        {ok, _Other} ->
            logger:warning("Gateway rollout config: unexpected API response format"),
            rejected;
        {error, Reason} ->
            logger:warning("Gateway rollout config failed to fetch from API", #{
                reason => Reason
            }),
            rejected
    end.

-spec apply_nats_payload(binary()) -> store_result().
apply_nats_payload(Payload) ->
    try json:decode(Payload) of
        #{<<"type">> := <<"gateway_rollout_config">>, <<"config">> := Config} when
            is_map(Config)
        ->
            store_valid_config(Config, nats);
        #{<<"config">> := Config} when is_map(Config) ->
            store_valid_config(Config, nats);
        Config when is_map(Config) ->
            store_valid_config(Config, nats);
        _Other ->
            logger:warning("Gateway rollout config: unexpected NATS payload format"),
            rejected
    catch
        Class:Reason ->
            logger:warning("Gateway rollout config failed to decode NATS payload", #{
                class => Class, reason => Reason
            }),
            rejected
    end.

-spec store_valid_config(map(), api | nats) -> store_result().
store_valid_config(Config, Source) ->
    case validate_config(Config) of
        {ok, Validated} ->
            OldConfig = get(),
            store_validated_config(OldConfig, Validated, Source);
        {error, Reason} ->
            logger:warning(
                "Gateway rollout config rejected invalid config: ~p",
                [Reason]
            ),
            rejected
    end.

-spec store_validated_config(map(), map(), api | nats) -> store_result().
store_validated_config(OldConfig, OldConfig, _Source) ->
    unchanged;
store_validated_config(OldConfig, NewConfig, Source) ->
    log_config_transitions(OldConfig, NewConfig),
    persistent_term:put(?PERSISTENT_TERM_KEY, NewConfig),
    logger:info("Gateway rollout config updated", #{source => Source}),
    updated.

-spec subscribe_to_nats(state()) -> state().
subscribe_to_nats(#{nats_subscription := Sid} = State) when Sid =/= undefined ->
    State;
subscribe_to_nats(State) ->
    case subscribe_to_rollout_subject() of
        {ok, Sid} ->
            MonRef = monitor_nats_rpc(),
            logger:info("Gateway rollout config subscribed to NATS", #{subject => ?NATS_SUBJECT}),
            State#{nats_subscription => Sid, nats_monitor => MonRef};
        {error, Reason} ->
            logger:debug("Gateway rollout config waiting for NATS subscription", #{
                subject => ?NATS_SUBJECT, reason => Reason
            }),
            erlang:send_after(?NATS_SUBSCRIBE_RETRY_MS, self(), subscribe_nats),
            State
    end.

-spec subscribe_to_rollout_subject() -> {ok, term()} | {error, term()}.
subscribe_to_rollout_subject() ->
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

-spec log_config_transitions(map(), map()) -> ok.
log_config_transitions(OldConfig, NewConfig) ->
    WatchKeys = [
        <<"session_rollout_percentage">>,
        <<"guild_rollout_percentage">>,
        <<"rpc_request_timeout_ms">>,
        <<"voice_reconciliation_v3_percentage">>,
        <<"voice_reconciliation_v3_interval_ms">>
    ],
    lists:foreach(
        fun(Key) -> log_key_transition(Key, OldConfig, NewConfig) end,
        WatchKeys
    ),
    ok.

-spec log_key_transition(binary(), map(), map()) -> ok.
log_key_transition(Key, OldConfig, NewConfig) ->
    OldVal = maps:get(Key, OldConfig, undefined),
    NewVal = maps:get(Key, NewConfig, undefined),
    case OldVal =:= NewVal of
        true ->
            ok;
        false ->
            logger:notice(
                "Gateway rollout config transition:"
                " key=~s old=~p new=~p",
                [Key, OldVal, NewVal]
            )
    end.

-spec maybe_notify_subscribers(store_result(), state()) -> ok.
maybe_notify_subscribers(updated, State) ->
    notify_subscribers(State);
maybe_notify_subscribers(_Result, _State) ->
    ok.

-spec notify_subscribers(state()) -> ok.
notify_subscribers(#{subscribers := Subs}) ->
    lists:foreach(
        fun({Pid, _MonRef}) ->
            Pid ! rollout_config_changed
        end,
        Subs
    ),
    ok.
