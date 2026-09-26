%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_handler_identify).
-typing([eqwalizer]).

-export([
    handle_identify/3,
    handle_resume/2,
    handle_rollout_config_changed/1,
    handle_pending_identify_retry/1,
    handle_pending_identify_retry/2,
    start_session/3,
    start_session_with_drain_guard/4,
    handle_session_start_result/2,
    validate_identify_data/1,
    validate_resume_data/1,
    parse_ignored_events/1
]).

-type state() :: gateway_handler:state().
-type ws_result() :: gateway_handler:ws_result().
-type pending_retry_timer() :: {reference(), reference()} | reference() | undefined.

-export_type([state/0, ws_result/0]).

-define(PENDING_IDENTIFY_RETRY_BASE_MS, 1000).
-define(PENDING_IDENTIFY_RETRY_JITTER_MS, 1000).
-define(MAX_IGNORED_EVENTS, 256).

-spec handle_identify(map(), binary(), state()) -> ws_result().
handle_identify(Data, PeerIP, State) ->
    handle_rate_limited_identify(Data, PeerIP, State).

-spec handle_rate_limited_identify(map(), binary(), state()) -> ws_result().
handle_rate_limited_identify(Data, PeerIP, State) ->
    case session_abuse_protection:check_identify_rate(PeerIP) of
        {error, identify_rate_limited} ->
            hold_identify(<<"Identify rate limited">>, State);
        ok ->
            handle_identify_validated(Data, PeerIP, State)
    end.

-spec handle_identify_validated(map(), binary(), state()) -> ws_result().
handle_identify_validated(Data, PeerIP, State) ->
    GwTimings0 = gateway_timings:new(),
    ValidateStartedAt = gateway_timings:start(),
    ValidateResult = validate_identify_data(Data),
    GwTimings = gateway_timings:record_function(
        validate_identify_data,
        <<"gateway_handler_identify:validate_identify_data/1">>,
        ValidateStartedAt,
        GwTimings0
    ),
    case ValidateResult of
        {ok, Token, Properties, Presence, IgnoredEvents, Flags, InitialGuildId, Shard} ->
            do_identify(
                Token,
                Properties,
                Presence,
                IgnoredEvents,
                Flags,
                InitialGuildId,
                Shard,
                PeerIP,
                GwTimings,
                State
            );
        {error, invalid_shard} ->
            gateway_handler_encode:close_with_reason(
                invalid_shard,
                <<"Invalid shard">>,
                State
            );
        {error, _Reason} ->
            gateway_handler_encode:close_with_reason(
                decode_error,
                <<"Invalid identify payload">>,
                State
            )
    end.

-spec do_identify(
    binary(),
    map(),
    term(),
    list(),
    non_neg_integer(),
    integer() | undefined,
    gateway_sharding:shard() | undefined,
    binary(),
    gateway_timings:recorder(),
    state()
) -> ws_result().
do_identify(
    Token,
    Properties,
    Presence,
    IgnoredEvents,
    Flags,
    InitialGuildId,
    Shard,
    PeerIP,
    GwTimings,
    State
) ->
    SessionId = utils:generate_session_id(),
    IdentifyData = build_identify_data(
        Token,
        Properties,
        Presence,
        IgnoredEvents,
        Flags,
        InitialGuildId,
        Shard
    ),
    Request = #{
        session_id => SessionId,
        peer_ip => PeerIP,
        identify_data => IdentifyData,
        gw_timings => GwTimings,
        version => maps:get(version, State)
    },
    start_session(Request, self(), State).

-spec build_identify_data(
    binary(),
    map(),
    term(),
    [binary()],
    non_neg_integer(),
    integer() | undefined,
    gateway_sharding:shard() | undefined
) -> map().
build_identify_data(Token, Properties, Presence, IgnoredEvents, Flags, InitialGuildId, Shard) ->
    Base = #{
        token => Token,
        properties => Properties,
        presence => Presence,
        ignored_events => IgnoredEvents,
        flags => Flags
    },
    maybe_put(shard, Shard, maybe_put(initial_guild_id, InitialGuildId, Base)).

-spec maybe_put(atom(), term(), map()) -> map().
maybe_put(_Key, undefined, Map) ->
    Map;
maybe_put(Key, Value, Map) ->
    Map#{Key => Value}.

-spec validate_identify_data(map()) ->
    {ok, binary(), map(), term(), [binary()], non_neg_integer(), integer() | undefined,
        gateway_sharding:shard() | undefined}
    | {error, atom()}.
validate_identify_data(Data) when is_map(Data) ->
    try
        Token = maps:get(<<"token">>, Data),
        Properties = maps:get(<<"properties">>, Data),
        IgnoredEventsRaw = maps:get(<<"ignored_events">>, Data, []),
        InitialGuildIdRaw = maps:get(<<"initial_guild_id">>, Data, undefined),
        ShardRaw = maps:get(<<"shard">>, Data, undefined),
        validate_properties(
            Token, Properties, IgnoredEventsRaw, InitialGuildIdRaw, ShardRaw, Data
        )
    catch
        error:{badkey, _} -> {error, missing_required_field}
    end;
validate_identify_data(_Data) ->
    {error, invalid_data}.

-spec validate_properties(binary(), term(), term(), term(), term(), map()) ->
    {ok, binary(), map(), term(), [binary()], non_neg_integer(), integer() | undefined,
        gateway_sharding:shard() | undefined}
    | {error, atom()}.
validate_properties(Token, Properties, IgnoredEventsRaw, InitialGuildIdRaw, ShardRaw, Data) when
    is_map(Properties)
->
    Os = maps:get(<<"os">>, Properties),
    Browser = maps:get(<<"browser">>, Properties),
    Device = maps:get(<<"device">>, Properties),
    case {is_binary(Os), is_binary(Browser), is_binary(Device)} of
        {true, true, true} ->
            Presence = maps:get(<<"presence">>, Data, null),
            validate_ignored_events(
                Token,
                Properties,
                Presence,
                IgnoredEventsRaw,
                InitialGuildIdRaw,
                ShardRaw,
                Data
            );
        _ ->
            {error, invalid_properties}
    end;
validate_properties(_, _, _, _, _, _) ->
    {error, invalid_properties}.

-spec validate_ignored_events(binary(), map(), term(), term(), term(), term(), map()) ->
    {ok, binary(), map(), term(), [binary()], non_neg_integer(), integer() | undefined,
        gateway_sharding:shard() | undefined}
    | {error, atom()}.
validate_ignored_events(
    Token, Properties, Presence, IgnoredEventsRaw, InitialGuildIdRaw, ShardRaw, Data
) ->
    case parse_ignored_events(IgnoredEventsRaw) of
        {ok, IgnoredEvents} ->
            FlagsRaw = maps:get(<<"flags">>, Data, 0),
            validate_flags(
                Token,
                Properties,
                Presence,
                IgnoredEvents,
                InitialGuildIdRaw,
                ShardRaw,
                FlagsRaw
            );
        {error, Reason} ->
            {error, Reason}
    end.

-spec validate_flags(binary(), map(), term(), [binary()], term(), term(), term()) ->
    {ok, binary(), map(), term(), [binary()], non_neg_integer(), integer() | undefined,
        gateway_sharding:shard() | undefined}
    | {error, atom()}.
validate_flags(
    Token, Properties, Presence, IgnoredEvents, InitialGuildIdRaw, ShardRaw, Flags
) when
    is_integer(Flags), Flags >= 0
->
    InitialGuildId = parse_initial_guild_id(InitialGuildIdRaw),
    case gateway_sharding:parse_identify_shard(ShardRaw) of
        {ok, Shard} ->
            {ok, Token, Properties, Presence, IgnoredEvents, Flags, InitialGuildId, Shard};
        {error, invalid_shard} ->
            {error, invalid_shard}
    end;
validate_flags(_, _, _, _, _, _, _) ->
    {error, invalid_properties}.

-spec parse_ignored_events(term()) -> {ok, [binary()]} | {error, invalid_ignored_events}.
parse_ignored_events(undefined) ->
    {ok, []};
parse_ignored_events(null) ->
    {ok, []};
parse_ignored_events(Events) when is_list(Events), length(Events) =< ?MAX_IGNORED_EVENTS ->
    case lists:all(fun erlang:is_binary/1, Events) of
        true -> {ok, lists:usort([normalize_event_name(E) || E <- Events, is_binary(E)])};
        false -> {error, invalid_ignored_events}
    end;
parse_ignored_events(_) ->
    {error, invalid_ignored_events}.

-spec parse_initial_guild_id(term()) -> integer() | undefined.
parse_initial_guild_id(undefined) ->
    undefined;
parse_initial_guild_id(null) ->
    undefined;
parse_initial_guild_id(Value) when is_binary(Value) ->
    case validation:validate_snowflake(<<"initial_guild_id">>, Value) of
        {ok, GuildId} -> GuildId;
        {error, _, _} -> undefined
    end;
parse_initial_guild_id(_) ->
    undefined.

-spec normalize_event_name(binary()) -> binary().
normalize_event_name(Event) ->
    list_to_binary(string:uppercase(binary_to_list(Event))).

-spec start_session(map(), pid(), state()) -> ws_result().
start_session(Request, SocketPid, State) ->
    case gateway_rollout_config:session_rollout_percentage() =< 0 of
        true ->
            hold_session_start(Request, SocketPid, <<"Session starts paused">>, State);
        false ->
            start_session_with_drain_guard(
                gateway_node_router:is_draining(),
                Request,
                SocketPid,
                State
            )
    end.

-spec start_session_with_drain_guard(boolean(), map(), pid(), state()) -> ws_result().
start_session_with_drain_guard(true, Request, SocketPid, State) ->
    hold_session_start(Request, SocketPid, <<"Gateway draining">>, State);
start_session_with_drain_guard(false, Request, SocketPid, State) ->
    StartResult = session_manager:start(Request, SocketPid),
    handle_session_start_result(StartResult, Request, SocketPid, State).

-spec handle_session_start_result(term(), state()) -> ws_result().
handle_session_start_result(StartResult, State) ->
    handle_session_start_result(StartResult, undefined, undefined, State).

-spec handle_session_start_result(term(), map() | undefined, pid() | undefined, state()) ->
    ws_result().
handle_session_start_result({success, Pid}, _Request, _SocketPid, State) when is_pid(Pid) ->
    monitor(process, Pid),
    {ok, (clear_pending_identify(State))#{session_pid => Pid}};
handle_session_start_result({error, Reason}, Request, SocketPid, State) ->
    handle_session_start_error(Reason, Request, SocketPid, State);
handle_session_start_result(_, _Request, _SocketPid, State) ->
    gateway_handler_encode:close_with_reason(
        unknown_error, <<"Failed to start session">>, State
    ).

-spec handle_session_start_error(term(), map() | undefined, pid() | undefined, state()) ->
    ws_result().
handle_session_start_error(Reason, Request, SocketPid, State) ->
    case session_start_error_action(Reason) of
        {close, CloseReason, Message} ->
            gateway_handler_encode:close_with_reason(CloseReason, Message, State);
        {reconnect, Message} ->
            hold_session_start(Request, SocketPid, Message, State);
        unknown ->
            log_session_start_error(Reason, State)
    end.

-spec session_start_error_action(term()) ->
    {close, atom(), binary()} | {reconnect, binary()} | unknown.
session_start_error_action(invalid_token) ->
    {close, authentication_failed, <<"Invalid token">>};
session_start_error_action(rate_limited) ->
    {reconnect, <<"Session RPC rate limited">>};
session_start_error_action(invalid_shard) ->
    {close, invalid_shard, <<"Invalid shard">>};
session_start_error_action(sharding_required) ->
    {close, sharding_required, <<"Sharding required">>};
session_start_error_action(identify_rate_limited) ->
    {reconnect, <<"Identify rate limited">>};
session_start_error_action(draining) ->
    {reconnect, <<"Gateway draining">>};
session_start_error_action(not_eligible) ->
    {reconnect, <<"Session not eligible">>};
session_start_error_action(too_many_sessions) ->
    {close, rate_limited, <<"Too many sessions">>};
session_start_error_action(at_capacity) ->
    {reconnect, <<"Gateway at capacity">>};
session_start_error_action(timeout) ->
    {reconnect, <<"Session start timed out">>};
session_start_error_action({retries_exhausted, _Reason}) ->
    {reconnect, <<"Session RPC retry exhausted">>};
session_start_error_action({network_error, _Reason}) ->
    {reconnect, <<"Session RPC network error">>};
session_start_error_action({rpc_error, Status, _Reason}) when
    is_integer(Status), Status >= 500
->
    {reconnect, <<"Session RPC server error">>};
session_start_error_action(_) ->
    unknown.

-spec log_session_start_error(term(), state()) -> ws_result().
log_session_start_error(Reason, State) ->
    logger:error(
        "Session start failed: reason=~p peer_ip=~ts",
        [Reason, maps:get(peer_ip, State, <<"unknown">>)]
    ),
    gateway_handler_encode:close_with_reason(
        unknown_error, <<"Failed to start session">>, State
    ).

-spec handle_rollout_config_changed(state()) -> ws_result().
handle_rollout_config_changed(State) ->
    retry_pending_identify(State).

-spec handle_pending_identify_retry(state()) -> ws_result().
handle_pending_identify_retry(State) ->
    handle_legacy_pending_identify_retry(State).

-spec handle_pending_identify_retry(reference(), state()) -> ws_result().
handle_pending_identify_retry(Token, State) ->
    case maps:get(pending_identify_retry_timer, State, undefined) of
        {TimerRef, Token} ->
            cancel_pending_identify_retry_value({TimerRef, Token}),
            retry_pending_identify(State#{pending_identify_retry_timer => undefined});
        _ ->
            {ok, State}
    end.

-spec handle_legacy_pending_identify_retry(state()) -> ws_result().
handle_legacy_pending_identify_retry(State) ->
    case maps:get(pending_identify_retry_timer, State, undefined) of
        TimerRef when is_reference(TimerRef) ->
            cancel_pending_identify_retry_value(TimerRef),
            retry_pending_identify(State#{pending_identify_retry_timer => undefined});
        _ ->
            {ok, State}
    end.

-spec retry_pending_identify(state()) -> ws_result().
retry_pending_identify(#{pending_identify := {Request, SocketPid}} = State) when
    is_map(Request), is_pid(SocketPid)
->
    case gateway_rollout_config:session_rollout_percentage() > 0 of
        true ->
            gateway_rollout_config:unsubscribe_changes(self()),
            RetryState = clear_pending_identify(State),
            start_session_with_drain_guard(
                gateway_node_router:is_draining(),
                Request,
                SocketPid,
                RetryState
            );
        false ->
            {ok, schedule_pending_identify_retry(State)}
    end;
retry_pending_identify(State) ->
    {ok, clear_pending_identify(State)}.

-spec hold_session_start(map() | undefined, pid() | undefined, binary(), state()) ->
    ws_result().
hold_session_start(Request, SocketPid, Reason, State) when is_map(Request), is_pid(SocketPid) ->
    gateway_rollout_config:subscribe_changes(self()),
    logger:debug("Holding gateway identify: reason=~ts", [Reason]),
    HoldState = State#{pending_identify => {Request, SocketPid}},
    {ok, schedule_pending_identify_retry(HoldState)};
hold_session_start(_Request, _SocketPid, Reason, State) ->
    hold_identify(Reason, State).

-spec hold_identify(binary(), state()) -> ws_result().
hold_identify(Reason, State) ->
    logger:debug("Holding gateway identify: reason=~ts", [Reason]),
    {ok, State}.

-spec schedule_pending_identify_retry(state()) -> state().
schedule_pending_identify_retry(
    #{pending_identify_retry_timer := {TimerRef, Token}} = State
) when
    is_reference(TimerRef), is_reference(Token)
->
    State;
schedule_pending_identify_retry(#{pending_identify_retry_timer := TimerRef} = State) when
    is_reference(TimerRef)
->
    State;
schedule_pending_identify_retry(State) ->
    Delay =
        ?PENDING_IDENTIFY_RETRY_BASE_MS +
            erlang:phash2(
                {self(), erlang:monotonic_time()}, ?PENDING_IDENTIFY_RETRY_JITTER_MS
            ),
    Token = make_ref(),
    TimerRef = erlang:send_after(Delay, self(), {retry_pending_identify, Token}),
    State#{
        pending_identify_retry_timer => {TimerRef, Token}
    }.

-spec clear_pending_identify(state()) -> state().
clear_pending_identify(State) ->
    maybe_cancel_pending_identify_retry(
        maps:get(pending_identify_retry_timer, State, undefined)
    ),
    State#{pending_identify => undefined, pending_identify_retry_timer => undefined}.

-spec maybe_cancel_pending_identify_retry(pending_retry_timer()) -> ok.
maybe_cancel_pending_identify_retry(undefined) ->
    ok;
maybe_cancel_pending_identify_retry({TimerRef, _Token}) when is_reference(TimerRef) ->
    cancel_pending_identify_retry_value(TimerRef);
maybe_cancel_pending_identify_retry(TimerRef) when is_reference(TimerRef) ->
    cancel_pending_identify_retry_value(TimerRef).

-spec cancel_pending_identify_retry_value({reference(), reference()} | reference()) -> ok.
cancel_pending_identify_retry_value({TimerRef, _Token}) ->
    cancel_pending_identify_retry_value(TimerRef);
cancel_pending_identify_retry_value(TimerRef) ->
    _ = erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
    ok.

-spec handle_resume(map(), state()) -> ws_result().
handle_resume(Data, State) ->
    gateway_handler_resume:handle_resume(Data, State).

-spec validate_resume_data(map()) -> {ok, binary(), binary(), integer()} | {error, atom()}.
validate_resume_data(Data) ->
    gateway_handler_resume:validate_resume_data(Data).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

handle_session_start_capacity_holds_without_reconnect_test() ->
    {ok, _State} = handle_session_start_result({error, at_capacity}, identify_test_state()).

handle_session_start_rate_limit_holds_without_close_test() ->
    {ok, _State} = handle_session_start_result({error, rate_limited}, identify_test_state()).

start_session_with_drain_guard_holds_pending_identify_test() ->
    {ok, State} = start_session_with_drain_guard(
        true, #{token => <<"t">>}, self(), identify_test_state()
    ),
    {PendingRequest, PendingPid} = maps:get(pending_identify, State),
    ?assertEqual(#{token => <<"t">>}, PendingRequest),
    ?assert(is_pid(PendingPid)).

identify_test_state() ->
    #{
        encoding => json,
        compress_ctx => gateway_compress:new_context(none),
        peer_ip => <<"127.0.0.1">>
    }.

-endif.
