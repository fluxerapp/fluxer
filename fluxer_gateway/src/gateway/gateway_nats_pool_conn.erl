%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_nats_pool_conn).
-typing([eqwalizer]).

-export([
    get_pool_conn/0,
    connect_slot/2,
    handle_conn_down/4,
    find_slot_by_conn/2,
    update_persistent_term/2,
    schedule_reconnect/2,
    reset_failure_count/1,
    subscribe_slot/3,
    unsubscribe_slot_sids/2,
    is_rpc_subject/1,
    pool_size/0,
    max_handlers/0,
    parse_nats_url/1,
    build_connect_opts/1,
    reconnect_delay/1
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
        Conn -> {ok, Conn}
    end.

-spec find_any_conn(tuple(), pos_integer(), pos_integer(), non_neg_integer()) ->
    {ok, nats:conn()} | {error, no_connection}.
find_any_conn(_Conns, Size, _Start, Tried) when Tried >= Size ->
    {error, no_connection};
find_any_conn(Conns, Size, Start, Tried) ->
    Idx = ((Start - 1 + Tried) rem Size) + 1,
    case element(Idx, Conns) of
        undefined -> find_any_conn(Conns, Size, Start, Tried + 1);
        Conn -> {ok, Conn}
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
        case nats:connect(Host, Port, Opts) of
            {ok, Conn} ->
                ok = nats:controlling_process(Conn, Parent),
                Parent ! {nats_pool_connect_result, Idx, self(), {ok, Conn}};
            {error, Reason} ->
                Parent ! {nats_pool_connect_result, Idx, self(), {error, Reason}}
        end,
    ok.

-spec handle_conn_down(non_neg_integer(), nats:conn(), pos_integer(), map()) -> map().
handle_conn_down(
    Idx,
    Conn,
    PoolSize,
    #{slots := Slots, subs := Subs, monitors := Monitors} = State
) ->
    logger:warning("Gateway NATS pool slot connection lost, reconnecting", #{slot => Idx}),
    ignore_disconnect(Conn),
    CleanMonitors = maps:filter(fun(_Ref, SlotIdx) -> SlotIdx =/= Idx end, Monitors),
    NewSlots = Slots#{Idx => undefined},
    NewSubs = maps:remove(Idx, Subs),
    update_persistent_term(NewSlots, PoolSize),
    schedule_reconnect(Idx, self()),
    State#{slots => NewSlots, subs => NewSubs, monitors => CleanMonitors}.

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
    Subjects = gateway_nats_rpc:rpc_subjects_for_role(Role),
    Sids = subscribe_slot_subjects(Idx, Conn, Role, Subjects),
    case Sids of
        [] -> State;
        _ -> State#{subs => Subs#{Idx => Sids}}
    end.

-spec subscribe_slot_subjects(non_neg_integer(), nats:conn(), atom(), [binary()]) -> [term()].
subscribe_slot_subjects(Idx, Conn, Role, Subjects) ->
    lists:filtermap(
        fun(Subject) ->
            subscribe_slot_subject(Idx, Conn, Role, Subject)
        end,
        Subjects
    ).

-spec subscribe_slot_subject(non_neg_integer(), nats:conn(), atom(), binary()) ->
    {true, term()} | false.
subscribe_slot_subject(Idx, Conn, Role, Subject) ->
    case nats:sub(Conn, Subject, #{queue_group => ?QUEUE_GROUP}) of
        {ok, Sid} ->
            logger:info(
                "Gateway NATS pool slot ~p subscribed to ~s (role=~p)",
                [Idx, Subject, Role]
            ),
            {true, Sid};
        {error, Reason} ->
            logger:error(
                "Gateway NATS pool slot ~p failed to subscribe to ~s: ~p",
                [Idx, Subject, Reason]
            ),
            false
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

reconnect_delay_first_failure_test() ->
    ?assertEqual(?RECONNECT_BASE_DELAY_MS, reconnect_delay(1)).

reconnect_delay_exponential_backoff_test() ->
    ?assertEqual(?RECONNECT_BASE_DELAY_MS * 2, reconnect_delay(2)),
    ?assertEqual(?RECONNECT_BASE_DELAY_MS * 4, reconnect_delay(3)).

reconnect_delay_capped_at_max_test() ->
    ?assert(reconnect_delay(4) =< ?RECONNECT_MAX_DELAY_MS).

reconnect_delay_circuit_breaker_kicks_in_test() ->
    ?assertEqual(?CIRCUIT_BREAKER_COOLDOWN_MS, reconnect_delay(?CIRCUIT_BREAKER_THRESHOLD)),
    ?assertEqual(
        ?CIRCUIT_BREAKER_COOLDOWN_MS, reconnect_delay(?CIRCUIT_BREAKER_THRESHOLD + 10)
    ).

reset_failure_count_resets_test() ->
    Idx = 999,
    persistent_term:put({?FAILURE_COUNTS_KEY, Idx}, 10),
    reset_failure_count(Idx),
    ?assertEqual(0, persistent_term:get({?FAILURE_COUNTS_KEY, Idx}, 0)),
    persistent_term:erase({?FAILURE_COUNTS_KEY, Idx}).

increment_failure_count_increments_test() ->
    Idx = 998,
    persistent_term:erase({?FAILURE_COUNTS_KEY, Idx}),
    ?assertEqual(1, increment_failure_count(Idx)),
    ?assertEqual(2, increment_failure_count(Idx)),
    ?assertEqual(3, increment_failure_count(Idx)),
    persistent_term:erase({?FAILURE_COUNTS_KEY, Idx}).

parse_nats_url_nats_scheme_test() ->
    ?assertEqual({ok, "127.0.0.1", 4222}, parse_nats_url(<<"nats://127.0.0.1:4222">>)).

parse_nats_url_default_port_test() ->
    ?assertEqual({ok, "localhost", 4222}, parse_nats_url(<<"nats://localhost">>)).

parse_nats_url_tls_scheme_test() ->
    ?assertEqual(
        {ok, "nats.example.com", 4443}, parse_nats_url(<<"tls://nats.example.com:4443">>)
    ).

parse_nats_url_invalid_test() ->
    ?assertEqual({error, invalid_nats_url}, parse_nats_url(undefined)).

build_connect_opts_with_token_test() ->
    ?assertEqual(
        #{auth_token => <<"secret">>, buffer_size => 0}, build_connect_opts(<<"secret">>)
    ).

build_connect_opts_empty_test() ->
    ?assertEqual(#{buffer_size => 0}, build_connect_opts(undefined)).

-endif.
