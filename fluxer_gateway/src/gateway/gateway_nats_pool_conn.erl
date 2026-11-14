%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_nats_pool_conn).
-typing([eqwalizer]).

-export([
    get_pool_conn/0,
    connect/3,
    connect_slot/2,
    handle_conn_down/4,
    find_slot_by_conn/2,
    update_persistent_term/2,
    schedule_reconnect/2,
    reset_failure_count/1,
    publish_ready_slots/1,
    subscribe_slot/3,
    guarded_sub/3,
    conn_ready/1,
    subscribe_retry_delay/1,
    unsubscribe_slot_sids/2,
    is_rpc_subject/1,
    pool_size/0,
    max_handlers/0,
    parse_nats_url/1,
    build_connect_opts/1,
    reconnect_delay/1,
    nats_rpc_enabled/0,
    decrement_handler_count/1,
    maybe_kill_connect_pid/1,
    ignore_disconnect/1,
    ignore_unsub/2
]).

-define(DEFAULT_POOL_SIZE, 16).
-define(DEFAULT_MAX_HANDLERS, 512).
-define(RECONNECT_BASE_DELAY_MS, 2000).
-define(RECONNECT_MAX_DELAY_MS, 30000).
-define(CONNECT_TIMEOUT_MS, 10000).
-define(PERSISTENT_TERM_KEY, {gateway_nats_pool, connections}).
-define(QUEUE_GROUP, <<"gateway">>).
-define(CIRCUIT_BREAKER_THRESHOLD, 5).
-define(CIRCUIT_BREAKER_COOLDOWN_MS, 15000).
-define(FAILURE_COUNTS_KEY, {gateway_nats_pool, failure_counts}).
-define(SUBSCRIBE_RETRY_BASE_MS, 250).
-define(SUBSCRIBE_RETRY_MAX_MS, 5000).
-define(SUBSCRIBE_TIMEOUT_MS, 5000).

-spec get_pool_conn() -> {ok, nats:conn()} | {error, no_connection}.
get_pool_conn() ->
    case persistent_term:get(?PERSISTENT_TERM_KEY, undefined) of
        undefined ->
            {error, no_connection};
        Conns ->
            pool_conn_from_tuple(Conns)
    end.

-spec pool_conn_from_tuple(tuple()) -> {ok, nats:conn()} | {error, no_connection}.
pool_conn_from_tuple(Conns) ->
    Size = tuple_size(Conns),
    Idx = erlang:phash2(self(), Size) + 1,
    case element(Idx, Conns) of
        undefined -> find_any_conn(Conns, Size, Idx, 1);
        Conn -> {ok, eqwalizer:dynamic_cast(Conn)}
    end.

-spec find_any_conn(tuple(), pos_integer(), pos_integer(), non_neg_integer()) ->
    {ok, nats:conn()} | {error, no_connection}.
find_any_conn(_Conns, Size, _Start, Tried) when Tried >= Size ->
    {error, no_connection};
find_any_conn(Conns, Size, Start, Tried) ->
    Idx = ((Start - 1 + Tried) rem Size) + 1,
    case element(Idx, Conns) of
        undefined -> find_any_conn(Conns, Size, Start, Tried + 1);
        Conn -> {ok, eqwalizer:dynamic_cast(Conn)}
    end.

-spec connect_slot(non_neg_integer(), map()) -> map().
connect_slot(Idx, #{connecting := Connecting} = State) ->
    case maps:is_key(Idx, Connecting) of
        true ->
            State;
        false ->
            do_connect_slot(Idx, Connecting, State)
    end.

-spec do_connect_slot(non_neg_integer(), map(), map()) -> map().
do_connect_slot(Idx, Connecting, State) ->
    NatsUrl = fluxer_gateway_env:get(nats_core_url),
    AuthToken = fluxer_gateway_env:get(nats_auth_token),
    case parse_nats_url(NatsUrl) of
        {ok, Host, Port} ->
            Opts = build_connect_opts(AuthToken),
            Parent = self(),
            Token = make_ref(),
            ConnPid = spawn_connect_slot(Idx, Host, Port, Opts, Parent),
            TimerRef = erlang:send_after(
                ?CONNECT_TIMEOUT_MS, self(), {connect_timeout, Idx, Token}
            ),
            Entry = #{pid => ConnPid, token => Token, timer => TimerRef},
            State#{connecting => Connecting#{Idx => Entry}};
        {error, Reason} ->
            logger:error("Gateway NATS pool failed to parse URL", #{reason => Reason}),
            schedule_reconnect(Idx, self()),
            State
    end.

-spec spawn_connect_slot(non_neg_integer(), string(), inet:port_number(), map(), pid()) ->
    pid().
spawn_connect_slot(Idx, Host, Port, Opts, Parent) ->
    spawn(fun() -> connect_slot_and_notify(Idx, Host, Port, Opts, Parent) end).

-spec connect_slot_and_notify(non_neg_integer(), string(), inet:port_number(), map(), pid()) ->
    ok.
connect_slot_and_notify(Idx, Host, Port, Opts, Parent) ->
    _ =
        case connect(Host, Port, Opts) of
            {ok, Conn} ->
                ok = nats:controlling_process(Conn, Parent),
                Parent ! {nats_pool_connect_result, Idx, self(), {ok, Conn}};
            {error, Reason} ->
                Parent ! {nats_pool_connect_result, Idx, self(), {error, Reason}}
        end,
    ok.

-spec connect(string(), inet:port_number(), map()) ->
    {ok, nats:conn()} | ignore | {error, term()}.
connect(Host, Port, Opts) ->
    Server = #{
        scheme => <<"nats">>, host => Host, port => Port, family => address_family(Host)
    },
    nats:connect(Server, Opts).

-spec address_family(string()) -> inet | inet6.
address_family(Host) ->
    case inet:getaddr(Host, inet) of
        {ok, _} -> inet;
        {error, _} -> inet6
    end.

-spec handle_conn_down(non_neg_integer(), nats:conn(), pos_integer(), map()) -> map().
handle_conn_down(
    Idx,
    Conn,
    _PoolSize,
    #{slots := Slots, subs := Subs, monitors := Monitors, connecting := Connecting} = State
) ->
    logger:warning("Gateway NATS pool slot connection lost, reconnecting", #{slot => Idx}),
    ignore_disconnect(Conn),
    cancel_entry_timer(maps:get(Idx, Connecting, undefined)),
    CleanMonitors = maps:filter(fun(_Ref, SlotIdx) -> SlotIdx =/= Idx end, Monitors),
    NewState = State#{
        slots => Slots#{Idx => undefined},
        subs => maps:remove(Idx, Subs),
        monitors => CleanMonitors,
        connecting => maps:remove(Idx, Connecting)
    },
    publish_ready_slots(NewState),
    schedule_reconnect(Idx, self()),
    NewState.

-spec cancel_entry_timer(term()) -> ok.
cancel_entry_timer(#{timer := TimerRef}) when is_reference(TimerRef) ->
    _ = erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
    ok;
cancel_entry_timer(_) ->
    ok.

-spec publish_ready_slots(map()) -> ok.
publish_ready_slots(#{slots := Slots, connecting := Connecting, pool_size := PoolSize}) ->
    Ready = maps:filter(
        fun(Idx, Conn) -> Conn =/= undefined andalso not maps:is_key(Idx, Connecting) end,
        Slots
    ),
    update_persistent_term(Ready, PoolSize).

-spec find_slot_by_conn(nats:conn(), map()) -> non_neg_integer() | undefined.
find_slot_by_conn(Conn, #{slots := Slots}) ->
    maps:fold(
        fun
            (_Idx, _C, Found) when Found =/= undefined -> Found;
            (Idx, C, _Acc) when C =:= Conn -> Idx;
            (_Idx, _C, Acc) -> Acc
        end,
        undefined,
        Slots
    ).

-spec update_persistent_term(map(), pos_integer()) -> ok.
update_persistent_term(Slots, PoolSize) ->
    Tuple = list_to_tuple(
        [maps:get(I, Slots, undefined) || I <- lists:seq(0, PoolSize - 1)]
    ),
    persistent_term:put(?PERSISTENT_TERM_KEY, Tuple).

-spec schedule_reconnect(non_neg_integer(), pid()) -> reference().
schedule_reconnect(Idx, Dest) ->
    Failures = increment_failure_count(Idx),
    Delay = reconnect_delay(Failures),
    erlang:send_after(Delay, Dest, {reconnect_slot, Idx}).

-spec reset_failure_count(non_neg_integer()) -> ok.
reset_failure_count(Idx) ->
    persistent_term:put({?FAILURE_COUNTS_KEY, Idx}, 0),
    ok.

-spec increment_failure_count(non_neg_integer()) -> non_neg_integer().
increment_failure_count(Idx) ->
    Key = {?FAILURE_COUNTS_KEY, Idx},
    Count =
        case persistent_term:get(Key, 0) of
            Previous when is_integer(Previous), Previous >= 0 -> Previous + 1;
            _ -> 1
        end,
    persistent_term:put(Key, Count),
    Count.

-spec reconnect_delay(non_neg_integer()) -> pos_integer().
reconnect_delay(Failures) when Failures >= ?CIRCUIT_BREAKER_THRESHOLD ->
    ?CIRCUIT_BREAKER_COOLDOWN_MS;
reconnect_delay(Failures) ->
    min(?RECONNECT_MAX_DELAY_MS, ?RECONNECT_BASE_DELAY_MS * (1 bsl (Failures - 1))).

-spec subscribe_slot(non_neg_integer(), nats:conn(), map()) -> map().
subscribe_slot(Idx, Conn, #{subs := Subs} = State) ->
    case maps:is_key(Idx, Subs) of
        true ->
            State;
        false ->
            subscribe_new_slot(Idx, Conn, Subs, State)
    end.

-spec subscribe_new_slot(non_neg_integer(), nats:conn(), map(), map()) -> map().
subscribe_new_slot(Idx, Conn, Subs, State) ->
    Role = fluxer_gateway_sup:current_role(),
    Subjects = gateway_nats_rpc_handler:rpc_subjects_for_role(Role),
    Results = [subscribe_slot_subject(Idx, Conn, Role, Subject) || Subject <- Subjects],
    Sids = [Sid || {ok, Sid} <- Results],
    case length(Sids) =:= length(Subjects) of
        true when Sids =:= [] ->
            State;
        true ->
            State#{subs => Subs#{Idx => Sids}, sub_failures => clear_sub_failures(Idx, State)};
        false ->
            unsubscribe_slot_sids(Conn, Sids),
            schedule_slot_resubscribe(Idx, State)
    end.

-spec subscribe_slot_subject(non_neg_integer(), nats:conn(), atom(), binary()) ->
    {ok, nats:sid()} | {error, term()}.
subscribe_slot_subject(Idx, Conn, Role, Subject) ->
    case guarded_sub(Conn, Subject, #{queue_group => ?QUEUE_GROUP}) of
        {ok, Sid} ->
            logger:info(
                "Gateway NATS pool slot ~p subscribed to ~s (role=~p)",
                [Idx, Subject, Role]
            ),
            {ok, Sid};
        {error, Reason} ->
            logger:error(
                "Gateway NATS pool slot ~p failed to subscribe to ~s: ~p",
                [Idx, Subject, Reason]
            ),
            {error, Reason}
    end.

-spec clear_sub_failures(non_neg_integer(), map()) -> map().
clear_sub_failures(Idx, State) ->
    maps:remove(Idx, maps:get(sub_failures, State, #{})).

-spec schedule_slot_resubscribe(non_neg_integer(), map()) -> map().
schedule_slot_resubscribe(Idx, State) ->
    Failures = maps:get(sub_failures, State, #{}),
    Count = maps:get(Idx, Failures, 0) + 1,
    Delay = subscribe_retry_delay(Count),
    logger:warning("Gateway NATS pool slot will retry its subscriptions", #{
        slot => Idx, attempt => Count, delay_ms => Delay
    }),
    erlang:send_after(Delay, self(), {resubscribe_slot, Idx}),
    State#{sub_failures => Failures#{Idx => Count}}.

-spec subscribe_retry_delay(term()) -> pos_integer().
subscribe_retry_delay(Failures) when is_integer(Failures), Failures > 5 ->
    ?SUBSCRIBE_RETRY_MAX_MS;
subscribe_retry_delay(Failures) when is_integer(Failures), Failures > 0 ->
    min(?SUBSCRIBE_RETRY_MAX_MS, ?SUBSCRIBE_RETRY_BASE_MS bsl (Failures - 1));
subscribe_retry_delay(_Failures) ->
    ?SUBSCRIBE_RETRY_BASE_MS.

-spec guarded_sub(nats:conn(), binary(), map()) -> {ok, nats:sid()} | {error, term()}.
guarded_sub(Conn, Subject, Opts) ->
    Guard = spawn(fun() -> disconnect_unless_done(Conn) end),
    try nats:sub(Conn, Subject, Opts) of
        Result -> Result
    catch
        exit:Reason -> {error, {exit, Reason}}
    after
        Guard ! done
    end.

-spec disconnect_unless_done(nats:conn()) -> ok.
disconnect_unless_done(Conn) ->
    receive
        done -> ok
    after ?SUBSCRIBE_TIMEOUT_MS ->
        logger:warning("Gateway NATS subscribe did not finish in time, closing the connection"),
        ignore_disconnect(Conn)
    end.

-spec conn_ready(nats:conn()) -> boolean().
conn_ready(Conn) ->
    case nats:is_ready(Conn) of
        true -> true;
        _ -> false
    end.

-spec unsubscribe_slot_sids(nats:conn(), term()) -> ok.
unsubscribe_slot_sids(Conn, Sids) when is_list(Sids) ->
    lists:foreach(
        fun(Sid) ->
            ignore_unsub(Conn, Sid)
        end,
        Sids
    ),
    ok;
unsubscribe_slot_sids(Conn, Sid) ->
    ignore_unsub(Conn, Sid),
    ok.

-spec is_rpc_subject(binary()) -> boolean().
is_rpc_subject(<<"rpc.gateway.", _/binary>>) -> true;
is_rpc_subject(_) -> false.

-spec pool_size() -> pos_integer().
pool_size() ->
    case fluxer_gateway_env:get(nats_pool_size) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> ?DEFAULT_POOL_SIZE
    end.

-spec max_handlers() -> pos_integer().
max_handlers() ->
    case fluxer_gateway_env:get(gateway_nats_rpc_max_handlers) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> ?DEFAULT_MAX_HANDLERS
    end.

-spec parse_nats_url(term()) -> {ok, string(), inet:port_number()} | {error, term()}.
parse_nats_url(Url) when is_list(Url) ->
    parse_nats_url(charlist_to_binary(Url));
parse_nats_url(<<"nats://", Rest/binary>>) ->
    parse_host_port(Rest);
parse_nats_url(<<"tls://", Rest/binary>>) ->
    parse_host_port(Rest);
parse_nats_url(Url) when is_binary(Url) ->
    parse_host_port(Url);
parse_nats_url(_) ->
    {error, invalid_nats_url}.

-spec parse_host_port(binary()) -> {ok, string(), inet:port_number()} | {error, term()}.
parse_host_port(HostPort) ->
    case binary:split(HostPort, <<":">>) of
        [Host, PortBin] ->
            parse_port(Host, PortBin);
        [Host] ->
            {ok, binary_to_list(Host), 4222}
    end.

-spec parse_port(binary(), binary()) ->
    {ok, string(), inet:port_number()} | {error, invalid_port}.
parse_port(Host, PortBin) ->
    try binary_to_integer(PortBin) of
        Port -> {ok, binary_to_list(Host), Port}
    catch
        error:badarg -> {error, invalid_port}
    end.

-spec build_connect_opts(term()) -> map().
build_connect_opts(AuthToken) when is_binary(AuthToken), byte_size(AuthToken) > 0 ->
    #{auth_token => AuthToken, buffer_size => 0};
build_connect_opts(AuthToken) when is_list(AuthToken) ->
    case AuthToken of
        "" -> #{buffer_size => 0};
        _ -> #{auth_token => charlist_to_binary(AuthToken), buffer_size => 0}
    end;
build_connect_opts(_) ->
    #{buffer_size => 0}.

-spec charlist_to_binary([term()]) -> binary().
charlist_to_binary(List) ->
    Bytes = lists:map(fun require_byte/1, List),
    list_to_binary(Bytes).

-spec require_byte(term()) -> byte().
require_byte(C) when is_integer(C), C >= 0, C =< 255 -> C;
require_byte(_) -> $?.

-spec ignore_disconnect(nats:conn()) -> ok.
ignore_disconnect(Conn) ->
    try nats:disconnect(Conn) of
        _ -> ok
    catch
        throw:_ -> ok;
        error:_ -> ok;
        exit:_ -> ok
    end.

-spec ignore_unsub(nats:conn(), term()) -> ok.
ignore_unsub(Conn, Sid) ->
    case coerce_sid(Sid) of
        {ok, TypedSid} -> safe_unsub(Conn, TypedSid);
        error -> ok
    end.

-spec safe_unsub(nats:conn(), nats:sid()) -> ok.
safe_unsub(Conn, TypedSid) ->
    try nats:unsub(Conn, TypedSid) of
        _ -> ok
    catch
        throw:_ -> ok;
        error:_ -> ok;
        exit:_ -> ok
    end.

-spec coerce_sid(term()) -> {ok, nats:sid()} | error.
coerce_sid({'$sid', N} = Sid) when is_integer(N) ->
    {ok, Sid};
coerce_sid(_) ->
    error.

-spec nats_rpc_enabled() -> boolean().
nats_rpc_enabled() ->
    case fluxer_gateway_env:get(nats_rpc_enabled) of
        false -> false;
        _ -> true
    end.

-spec decrement_handler_count(map()) -> map().
decrement_handler_count(#{handler_count := HC} = State) when HC > 0 ->
    State#{handler_count => HC - 1};
decrement_handler_count(State) ->
    State.

-spec maybe_kill_connect_pid(pid() | undefined) -> ok.
maybe_kill_connect_pid(Pid) when is_pid(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    ok;
maybe_kill_connect_pid(undefined) ->
    ok.
