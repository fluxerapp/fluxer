%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_nats_rpc).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/0, get_connection/0, is_connected/0, subscribe/2]).
-export([enable_rpc_subscription/0, disable_rpc_subscription/0]).
-export([rpc_subjects_for_role/1, handle_rpc_request/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-dialyzer(
    {no_opaque, [
        handle_info/2,
        handle_connect_ok/3,
        conn_from_term/1
    ]}
).

-spec rpc_subjects_for_role(atom()) -> [binary()].
rpc_subjects_for_role(Role) ->
    gateway_nats_rpc_handler:rpc_subjects_for_role(Role).

-spec handle_rpc_request(binary(), binary(), binary()) -> ok.
handle_rpc_request(Subject, Payload, ReplyTo) ->
    gateway_nats_rpc_handler:handle_rpc_request(Subject, Payload, ReplyTo).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_connection() -> {ok, nats:conn() | undefined} | {error, term()}.
get_connection() ->
    gen_server:call(?MODULE, get_connection).

-spec is_connected() -> boolean().
is_connected() ->
    try get_connection() of
        {ok, Conn} when Conn =/= undefined -> true;
        _ -> false
    catch
        throw:_ -> false;
        error:_ -> false;
        exit:_ -> false
    end.

-spec subscribe(binary(), binary()) -> {ok, term()} | {error, term()}.
subscribe(Subject, QueueGroup) ->
    gen_server:call(?MODULE, {subscribe_subject, Subject, QueueGroup}, 5000).

-spec enable_rpc_subscription() -> ok | {error, term()}.
enable_rpc_subscription() ->
    gen_server:call(?MODULE, enable_rpc_subscription, 5000).

-spec disable_rpc_subscription() -> ok | {error, term()}.
disable_rpc_subscription() ->
    gen_server:call(?MODULE, disable_rpc_subscription, 5000).

-spec init([]) -> {ok, map()}.
init([]) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 10),
    RpcEnabled = nats_rpc_enabled(),
    self() ! connect,
    {ok, #{
        conn => undefined,
        sub => undefined,
        handler_count => 0,
        handler_refs => #{},
        max_handlers => gateway_nats_rpc_handler:max_handlers(),
        monitor_ref => undefined,
        connecting_pid => undefined,
        connecting_token => undefined,
        connecting_timer => undefined,
        rpc_enabled => RpcEnabled,
        extra_subscriptions => #{}
    }}.

-spec handle_call(term(), gen_server:from(), map()) -> {reply, term(), map()}.
handle_call(get_connection, _From, #{conn := Conn} = State) ->
    {reply, {ok, Conn}, State};
handle_call({subscribe_subject, Subject, QueueGroup}, _From, #{conn := Conn} = State) when
    Conn =/= undefined, is_binary(Subject), is_binary(QueueGroup)
->
    {Reply, NewState} = subscribe_extra_subject(Conn, Subject, QueueGroup, State),
    {reply, Reply, NewState};
handle_call({subscribe_subject, _Subject, _QueueGroup}, _From, State) ->
    {reply, {error, not_connected}, State};
handle_call(enable_rpc_subscription, _From, #{conn := Conn} = State) when
    Conn =/= undefined
->
    NewState = gateway_nats_rpc_handler:do_subscribe(State#{rpc_enabled => true}),
    ignore_pool_enable_subscription(),
    {reply, ok, NewState};
handle_call(enable_rpc_subscription, _From, State) ->
    ignore_pool_enable_subscription(),
    {reply, ok, State#{rpc_enabled => true}};
handle_call(disable_rpc_subscription, _From, State) ->
    {reply, ok, do_disable_rpc(State)};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(connect, State) ->
    {noreply, handle_connect(State)};
handle_info({nats_connect_result, Pid, {ok, Conn}}, State) when is_pid(Pid) ->
    {noreply, handle_connect_ok(Pid, conn_from_term(Conn), State)};
handle_info({nats_connect_result, Pid, {error, Reason}}, State) when is_pid(Pid) ->
    {noreply, handle_connect_error(Pid, Reason, State)};
handle_info({connect_timeout, Token}, State) when is_reference(Token) ->
    {noreply, handle_connect_timeout(Token, State)};
handle_info({Conn, ready}, #{conn := Conn} = State) ->
    RpcState = gateway_nats_rpc_handler:do_subscribe(State),
    {noreply, subscribe_extra_subjects(Conn, RpcState)};
handle_info({Conn, closed}, #{conn := Conn} = State) ->
    logger:warning("Gateway NATS RPC connection closed, reconnecting"),
    {noreply, reconnect_after_disconnect(Conn, State)};
handle_info({Conn, {error, Reason}}, #{conn := Conn} = State) ->
    logger:warning("Gateway NATS RPC connection error, reconnecting", #{reason => Reason}),
    {noreply, reconnect_after_disconnect(Conn, State)};
handle_info({Conn, _Sid, {msg, Subject, Payload, MsgOpts}}, #{conn := Conn} = State) when
    is_binary(Subject), is_binary(Payload), is_map(MsgOpts)
->
    {noreply, handle_msg(Subject, Payload, MsgOpts, State)};
handle_info({handler_done, _Pid}, State) ->
    {noreply, decrement_handler_count(State)};
handle_info(
    {'DOWN', MRef, process, _Pid, _Reason},
    #{handler_refs := HRefs, handler_count := HC} = State
) when
    is_reference(MRef), is_map_key(MRef, HRefs)
->
    {noreply, handle_handler_down(MRef, HRefs, HC, State)};
handle_info(
    {'DOWN', MRef, process, Conn, Reason},
    #{conn := Conn, monitor_ref := MRef} = State
) ->
    logger:warning("Gateway NATS RPC connection process died, reconnecting", #{reason => Reason}),
    {noreply, schedule_conn_reconnect(State)};
handle_info(_Info, State) ->
    {noreply, State}.

-spec handle_handler_down(reference(), map(), non_neg_integer(), map()) -> map().
handle_handler_down(MRef, HRefs, HandlerCount, State) ->
    State#{
        handler_refs => maps:remove(MRef, HRefs),
        handler_count => max(0, HandlerCount - 1)
    }.

-spec decrement_handler_count(map()) -> map().
decrement_handler_count(#{handler_count := HC} = State) when HC > 0 ->
    State#{handler_count => HC - 1};
decrement_handler_count(State) ->
    State.

-spec terminate(term(), map()) -> ok.
terminate(_Reason, #{conn := Conn} = State) when Conn =/= undefined ->
    cleanup_connect_attempt(State),
    ignore_disconnect(Conn),
    logger:info("Gateway NATS RPC subscriber stopped"),
    ok;
terminate(_Reason, State) ->
    cleanup_connect_attempt(State),
    ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec handle_connect(map()) -> map().
handle_connect(State) ->
    connect_if_needed(State).

-spec nats_rpc_enabled() -> boolean().
nats_rpc_enabled() ->
    case fluxer_gateway_env:get(nats_rpc_enabled) of
        false -> false;
        _ -> true
    end.

-spec connect_if_needed(map()) -> map().
connect_if_needed(State) ->
    case {maps:get(connecting_pid, State, undefined), maps:get(conn, State, undefined)} of
        {Pid, _Conn} when is_pid(Pid) -> State;
        {undefined, undefined} -> gateway_nats_rpc_handler:do_connect(State);
        {undefined, _Conn} -> State
    end.

-spec reconnect_after_disconnect(nats:conn(), map()) -> map().
reconnect_after_disconnect(Conn, State) ->
    ignore_disconnect(Conn),
    schedule_conn_reconnect(State).

-spec schedule_conn_reconnect(map()) -> map().
schedule_conn_reconnect(State) ->
    cancel_connect_timer(State),
    gateway_nats_rpc_handler:schedule_reconnect(
        State#{
            conn => undefined,
            sub => undefined,
            monitor_ref => undefined,
            connecting_pid => undefined,
            connecting_token => undefined,
            connecting_timer => undefined,
            extra_subscriptions => clear_extra_subscription_sids(State)
        }
    ).

-spec handle_connect_ok(pid(), nats:conn(), map()) -> map().
handle_connect_ok(Pid, Conn, State) ->
    case maps:find(connecting_pid, State) of
        {ok, Pid} ->
            cleanup_old(State),
            cancel_connect_timer(State),
            MRef = nats:monitor(Conn),
            logger:info("Gateway NATS RPC connected"),
            State#{
                conn => Conn,
                monitor_ref => MRef,
                connecting_pid => undefined,
                connecting_token => undefined,
                connecting_timer => undefined
            };
        _ ->
            ignore_disconnect(Conn),
            State
    end.

-spec cleanup_old(map()) -> ok.
cleanup_old(State) ->
    OldConn = maps:get(conn, State, undefined),
    case OldConn of
        undefined -> ok;
        _ -> ignore_disconnect(OldConn)
    end,
    OldMRef = maps:get(monitor_ref, State, undefined),
    case OldMRef of
        undefined -> ok;
        _ -> erlang:demonitor(OldMRef, [flush])
    end,
    ok.

-spec cleanup_connect_attempt(map()) -> ok.
cleanup_connect_attempt(State) ->
    cancel_connect_timer(State),
    maybe_kill_connect_pid(maps:get(connecting_pid, State, undefined)).

-spec cancel_connect_timer(map()) -> ok.
cancel_connect_timer(State) ->
    case maps:get(connecting_timer, State, undefined) of
        TimerRef when is_reference(TimerRef) ->
            _ = erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
            ok;
        _ ->
            ok
    end.

-spec maybe_kill_connect_pid(pid() | undefined) -> ok.
maybe_kill_connect_pid(Pid) when is_pid(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    ok;
maybe_kill_connect_pid(undefined) ->
    ok.

-spec handle_connect_error(pid(), term(), map()) -> map().
handle_connect_error(Pid, Reason, State) ->
    case maps:get(connecting_pid, State, undefined) of
        Pid ->
            cancel_connect_timer(State),
            logger:error("Gateway NATS RPC failed to connect", #{reason => Reason}),
            gateway_nats_rpc_handler:schedule_reconnect(
                State#{
                    connecting_pid => undefined,
                    connecting_token => undefined,
                    connecting_timer => undefined
                }
            );
        _ ->
            State
    end.

-spec handle_connect_timeout(reference(), map()) -> map().
handle_connect_timeout(Token, State) ->
    case maps:get(connecting_token, State, undefined) of
        Token ->
            Pid = maps:get(connecting_pid, State, undefined),
            maybe_kill_connect_pid(Pid),
            logger:warning("Gateway NATS RPC connect attempt timed out, retrying"),
            gateway_nats_rpc_handler:do_connect(
                State#{
                    connecting_pid => undefined,
                    connecting_token => undefined,
                    connecting_timer => undefined
                }
            );
        _ ->
            State
    end.

-spec subscribe_extra_subject(nats:conn(), binary(), binary(), map()) ->
    {{ok, term()} | {error, term()}, map()}.
subscribe_extra_subject(Conn, Subject, QueueGroup, #{extra_subscriptions := Extra} = State) ->
    Key = {Subject, QueueGroup},
    case maps:get(Key, Extra, undefined) of
        #{sid := Sid} when Sid =/= undefined ->
            {{ok, Sid}, State};
        _ ->
            subscribe_new_extra_subject(Conn, Subject, QueueGroup, Key, Extra, State)
    end.

-spec subscribe_new_extra_subject(nats:conn(), binary(), binary(), term(), map(), map()) ->
    {{ok, term()} | {error, term()}, map()}.
subscribe_new_extra_subject(Conn, Subject, QueueGroup, Key, Extra, State) ->
    case nats:sub(Conn, Subject, subscription_opts(QueueGroup)) of
        {ok, Sid} ->
            NewExtra = Extra#{
                Key => #{subject => Subject, queue_group => QueueGroup, sid => Sid}
            },
            {{ok, Sid}, State#{extra_subscriptions => NewExtra}};
        {error, Reason} ->
            {{error, Reason}, State}
    end.

-spec subscribe_extra_subjects(nats:conn(), map()) -> map().
subscribe_extra_subjects(Conn, #{extra_subscriptions := Extra} = State) ->
    NewExtra = maps:fold(
        fun(Key, Sub, Acc) ->
            subscribe_recorded_extra_subject(Conn, Key, Sub, Acc)
        end,
        Extra,
        Extra
    ),
    State#{extra_subscriptions => NewExtra}.

-spec subscribe_recorded_extra_subject(nats:conn(), term(), map(), map()) -> map().
subscribe_recorded_extra_subject(_Conn, _Key, #{sid := Sid}, Acc) when Sid =/= undefined ->
    Acc;
subscribe_recorded_extra_subject(
    Conn, Key, #{subject := Subject, queue_group := QueueGroup} = Sub, Acc
) ->
    case nats:sub(Conn, Subject, subscription_opts(QueueGroup)) of
        {ok, Sid} ->
            notify_resubscribed(Subject),
            Acc#{Key => Sub#{sid => Sid}};
        {error, Reason} ->
            logger:error("Gateway NATS RPC failed to resubscribe extra subject", #{
                subject => Subject, reason => Reason
            }),
            Acc#{Key => Sub#{sid => undefined}}
    end.

-spec clear_extra_subscription_sids(map()) -> map().
clear_extra_subscription_sids(#{extra_subscriptions := Extra}) ->
    maps:map(fun(_Key, Sub) -> Sub#{sid => undefined} end, Extra);
clear_extra_subscription_sids(_State) ->
    #{}.

-spec subscription_opts(binary()) -> map().
subscription_opts(<<>>) ->
    #{};
subscription_opts(QueueGroup) ->
    #{queue_group => QueueGroup}.

-spec handle_msg(binary(), binary(), map(), map()) -> map().
handle_msg(
    Subject,
    Payload,
    MsgOpts,
    #{
        handler_count := HC,
        max_handlers := MaxH,
        handler_refs := HRefs
    } = State
) ->
    case gateway_nats_rpc_handler:is_rpc_subject(Subject) of
        true ->
            dispatch_rpc(Subject, Payload, MsgOpts, HC, MaxH, HRefs, State);
        false ->
            deliver_subject(Subject, Payload, maps:get(reply_to, MsgOpts, undefined)),
            State
    end.

-spec subject_owner(binary()) -> atom() | undefined.
subject_owner(<<"config.gateway.rollout">>) -> gateway_rollout_config;
subject_owner(<<"config.push.delivery">>) -> push_delivery_config;
subject_owner(_Subject) -> undefined.

-spec notify_resubscribed(binary()) -> ok.
notify_resubscribed(Subject) ->
    case subject_owner(Subject) of
        undefined -> ok;
        Owner -> notify_owner(whereis(Owner), Subject)
    end.

-spec notify_owner(pid() | undefined, binary()) -> ok.
notify_owner(Pid, Subject) when is_pid(Pid) ->
    Pid ! {nats_resubscribed, Subject},
    ok;
notify_owner(undefined, _Subject) ->
    ok.

-spec deliver_subject(binary(), binary(), binary() | undefined) -> ok.
deliver_subject(Subject, Payload, ReplyTo) ->
    case subject_owner(Subject) of
        undefined ->
            logger:warning("Gateway NATS RPC has no owner for subject", #{subject => Subject});
        Owner ->
            deliver_to_owner(Owner, Subject, Payload, ReplyTo)
    end.

-spec deliver_to_owner(atom(), binary(), binary(), binary() | undefined) -> ok.
deliver_to_owner(Owner, Subject, Payload, ReplyTo) ->
    case whereis(Owner) of
        undefined ->
            logger:warning("Gateway NATS RPC subject owner is not running", #{
                subject => Subject, owner => Owner
            });
        Pid ->
            Pid ! {nats_msg, Subject, Payload, ReplyTo},
            ok
    end.

-spec dispatch_rpc(
    binary(),
    binary(),
    map(),
    non_neg_integer(),
    pos_integer(),
    map(),
    map()
) -> map().
dispatch_rpc(Subject, Payload, MsgOpts, HC, MaxH, HRefs, State) ->
    case maps:find(reply_to, MsgOpts) of
        error -> State;
        {ok, ReplyTo} -> do_dispatch_rpc(ReplyTo, Subject, Payload, HC, MaxH, HRefs, State)
    end.

-spec do_dispatch_rpc(
    binary(),
    binary(),
    binary(),
    non_neg_integer(),
    pos_integer(),
    map(),
    map()
) -> map().
do_dispatch_rpc(ReplyTo, _Subject, _Payload, HC, MaxH, _HRefs, State) when HC >= MaxH ->
    ErrResp = iolist_to_binary(
        json:encode(#{
            <<"ok">> => false, <<"error">> => <<"overloaded">>
        })
    ),
    _ = gateway_nats_pool:pub_reply(ReplyTo, ErrResp),
    State;
do_dispatch_rpc(ReplyTo, Subject, Payload, HC, _MaxH, HRefs, State) ->
    {_Pid, MRef} = spawn_monitor(fun() ->
        gateway_nats_rpc_handler:handle_rpc_request(Subject, Payload, ReplyTo)
    end),
    State#{handler_count => HC + 1, handler_refs => HRefs#{MRef => true}}.

-spec do_disable_rpc(map()) -> map().
do_disable_rpc(#{conn := Conn, sub := Sids} = State) when
    Conn =/= undefined, is_list(Sids)
->
    [ignore_unsub(Conn, Sid) || Sid <- Sids],
    ignore_pool_disable_subscription(),
    logger:info("Gateway NATS RPC unsubscribed", #{subscription_count => length(Sids)}),
    State#{sub => undefined, rpc_enabled => false};
do_disable_rpc(#{conn := Conn, sub := Sid} = State) when
    Conn =/= undefined, Sid =/= undefined
->
    ignore_unsub(Conn, Sid),
    ignore_pool_disable_subscription(),
    logger:info("Gateway NATS RPC unsubscribed"),
    State#{sub => undefined, rpc_enabled => false};
do_disable_rpc(State) ->
    ignore_pool_disable_subscription(),
    State#{rpc_enabled => false}.

-spec conn_from_term(term()) -> nats:conn().
conn_from_term(Conn) when is_pid(Conn) ->
    Conn.

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

-spec ignore_pool_enable_subscription() -> ok.
ignore_pool_enable_subscription() ->
    try gateway_nats_pool:enable_rpc_subscription() of
        _ -> ok
    catch
        throw:_ -> ok;
        error:_ -> ok;
        exit:_ -> ok
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_WAIT_LOOP_TIMEOUT_MS, 60000).

stale_connect_timeout_does_not_kill_current_connect_worker_test() ->
    Token = make_ref(),
    StaleToken = make_ref(),
    Pid = spawn(fun wait_forever/0),
    State = #{
        conn => undefined,
        sub => undefined,
        handler_count => 0,
        handler_refs => #{},
        max_handlers => 1,
        monitor_ref => undefined,
        connecting_pid => Pid,
        connecting_token => Token,
        connecting_timer => undefined,
        rpc_enabled => true,
        extra_subscriptions => #{}
    },
    try
        ?assertEqual(State, handle_connect_timeout(StaleToken, State)),
        ?assert(erlang:is_process_alive(Pid))
    after
        exit(Pid, kill)
    end.

connect_message_does_not_kill_inflight_connect_worker_test() ->
    Token = make_ref(),
    Pid = spawn(fun wait_forever/0),
    State = #{
        conn => undefined,
        sub => undefined,
        handler_count => 0,
        handler_refs => #{},
        max_handlers => 1,
        monitor_ref => undefined,
        connecting_pid => Pid,
        connecting_token => Token,
        connecting_timer => undefined,
        rpc_enabled => true,
        extra_subscriptions => #{}
    },
    try
        ?assertEqual(State, handle_connect(State)),
        ?assert(erlang:is_process_alive(Pid))
    after
        exit(Pid, kill)
    end.

wait_forever() ->
    receive
        stop -> ok
    after ?TEST_WAIT_LOOP_TIMEOUT_MS ->
        ok
    end.

-endif.

-spec ignore_pool_disable_subscription() -> ok.
ignore_pool_disable_subscription() ->
    try gateway_nats_pool:disable_rpc_subscription() of
        _ -> ok
    catch
        throw:_ -> ok;
        error:_ -> ok;
        exit:_ -> ok
    end.
