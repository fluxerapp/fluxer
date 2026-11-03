%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_nats_rpc).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/0, get_connection/0, subscribe/2]).
-export([enable_rpc_subscription/0, disable_rpc_subscription/0, nats_ready/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-dialyzer(
    {no_opaque, [
        handle_info/2,
        handle_connect_ok/3,
        handle_connect_timeout/2,
        conn_from_term/1
    ]}
).

-define(STATUS_KEY, {?MODULE, subscription_status}).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_connection() -> {ok, nats:conn() | undefined} | {error, term()}.
get_connection() ->
    gen_server:call(?MODULE, get_connection).

-spec subscribe(binary(), binary()) -> {ok, term()} | {error, term()}.
subscribe(Subject, QueueGroup) ->
    gen_server:call(?MODULE, {subscribe_subject, Subject, QueueGroup}, 5000).

-spec enable_rpc_subscription() -> ok | {error, term()}.
enable_rpc_subscription() ->
    gen_server:call(?MODULE, enable_rpc_subscription, 5000).

-spec disable_rpc_subscription() -> ok | {error, term()}.
disable_rpc_subscription() ->
    gen_server:call(?MODULE, disable_rpc_subscription, 5000).

-spec nats_ready() -> boolean().
nats_ready() ->
    case persistent_term:get(?STATUS_KEY, missing) of
        not_required -> true;
        ready -> gateway_nats_pool:has_ready_connection();
        _ -> false
    end.

-spec init([]) -> {ok, map()}.
init([]) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 10),
    RpcEnabled = gateway_nats_pool_conn:nats_rpc_enabled(),
    self() ! connect,
    {ok,
        publish_status(#{
            conn => undefined,
            ready => false,
            rpc_subs => #{},
            subscribe_timer => undefined,
            subscribe_failures => 0,
            handler_count => 0,
            handler_refs => #{},
            max_handlers => gateway_nats_pool_conn:max_handlers(),
            monitor_ref => undefined,
            connecting_pid => undefined,
            connecting_token => undefined,
            connecting_timer => undefined,
            rpc_enabled => RpcEnabled,
            extra_subscriptions => #{}
        })}.

-spec handle_call(term(), gen_server:from(), map()) -> {reply, term(), map()}.
handle_call(get_connection, _From, #{conn := Conn} = State) ->
    {reply, {ok, Conn}, State};
handle_call({subscribe_subject, Subject, QueueGroup}, _From, State) when
    is_binary(Subject), is_binary(QueueGroup)
->
    {Reply, NewState} = subscribe_extra_subject(Subject, QueueGroup, State),
    {reply, Reply, NewState};
handle_call(enable_rpc_subscription, _From, State) ->
    NewState = ensure_subscriptions(State#{rpc_enabled => true}),
    ignore_pool_call(fun gateway_nats_pool:enable_rpc_subscription/0),
    {reply, ok, NewState};
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
    {noreply, handle_ready(State)};
handle_info(resubscribe, State) ->
    {noreply, ensure_subscriptions(State#{subscribe_timer => undefined})};
handle_info({Conn, closed}, #{conn := Conn} = State) ->
    logger:warning("Gateway NATS RPC connection closed, reconnecting"),
    {noreply, reconnect_after_disconnect(Conn, State)};
handle_info({Conn, {error, Reason}}, #{conn := Conn} = State) ->
    logger:warning("Gateway NATS RPC connection error, reconnecting", #{reason => Reason}),
    {noreply, reconnect_after_disconnect(Conn, State)};
handle_info({Conn, closed}, State) when is_pid(Conn) ->
    gateway_nats_pool_conn:ignore_disconnect(Conn),
    {noreply, State};
handle_info({Conn, {error, _Reason}}, State) when is_pid(Conn) ->
    gateway_nats_pool_conn:ignore_disconnect(Conn),
    {noreply, State};
handle_info({Conn, _Sid, {msg, Subject, Payload, MsgOpts}}, #{conn := Conn} = State) when
    is_binary(Subject), is_binary(Payload), is_map(MsgOpts)
->
    {noreply, handle_msg(Subject, Payload, MsgOpts, State)};
handle_info({handler_done, _Pid}, State) ->
    {noreply, gateway_nats_pool_conn:decrement_handler_count(State)};
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

-spec terminate(term(), map()) -> ok.
terminate(_Reason, #{conn := Conn} = State) when Conn =/= undefined ->
    persistent_term:erase(?STATUS_KEY),
    cleanup_connect_attempt(State),
    gateway_nats_pool_conn:ignore_disconnect(Conn),
    logger:info("Gateway NATS RPC subscriber stopped"),
    ok;
terminate(_Reason, State) ->
    persistent_term:erase(?STATUS_KEY),
    cleanup_connect_attempt(State),
    ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec handle_connect(map()) -> map().
handle_connect(State) ->
    connect_if_needed(State).

-spec connect_if_needed(map()) -> map().
connect_if_needed(State) ->
    case {maps:get(connecting_pid, State, undefined), maps:get(conn, State, undefined)} of
        {Pid, _Conn} when is_pid(Pid) -> State;
        {undefined, undefined} -> gateway_nats_rpc_handler:do_connect(State);
        {undefined, _Conn} -> State
    end.

-spec reconnect_after_disconnect(nats:conn(), map()) -> map().
reconnect_after_disconnect(Conn, State) ->
    gateway_nats_pool_conn:ignore_disconnect(Conn),
    schedule_conn_reconnect(State).

-spec schedule_conn_reconnect(map()) -> map().
schedule_conn_reconnect(State) ->
    cancel_connect_timer(State),
    gateway_nats_rpc_handler:schedule_reconnect(
        publish_status(State#{
            conn => undefined,
            ready => false,
            rpc_subs => #{},
            monitor_ref => undefined,
            connecting_pid => undefined,
            connecting_token => undefined,
            connecting_timer => undefined,
            extra_subscriptions => clear_extra_subscription_sids(State)
        })
    ).

-spec handle_connect_ok(pid(), nats:conn(), map()) -> map().
handle_connect_ok(Pid, Conn, State) ->
    case maps:find(connecting_pid, State) of
        {ok, Pid} ->
            cleanup_old(State),
            MRef = nats:monitor(Conn),
            OpenState = State#{
                conn => Conn,
                ready => false,
                monitor_ref => MRef,
                connecting_pid => undefined
            },
            ready_if_already(gateway_nats_pool_conn:conn_ready(Conn), OpenState);
        _ ->
            gateway_nats_pool_conn:ignore_disconnect(Conn),
            State
    end.

-spec cleanup_old(map()) -> ok.
cleanup_old(State) ->
    OldConn = maps:get(conn, State, undefined),
    case OldConn of
        undefined -> ok;
        _ -> gateway_nats_pool_conn:ignore_disconnect(OldConn)
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
    gateway_nats_pool_conn:maybe_kill_connect_pid(maps:get(connecting_pid, State, undefined)).

-spec cancel_connect_timer(map()) -> ok.
cancel_connect_timer(State) ->
    case maps:get(connecting_timer, State, undefined) of
        TimerRef when is_reference(TimerRef) ->
            _ = erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
            ok;
        _ ->
            ok
    end.

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

-spec ready_if_already(boolean(), map()) -> map().
ready_if_already(true, State) ->
    handle_ready(State);
ready_if_already(false, State) ->
    State.

-spec handle_ready(map()) -> map().
handle_ready(State) ->
    cancel_connect_timer(State),
    logger:info("Gateway NATS RPC connected"),
    ensure_subscriptions(State#{
        ready => true,
        connecting_token => undefined,
        connecting_timer => undefined
    }).

-spec handle_connect_timeout(reference(), map()) -> map().
handle_connect_timeout(Token, #{connecting_token := Token, conn := Conn} = State) when
    Conn =/= undefined
->
    logger:warning("Gateway NATS RPC connection did not become ready, reconnecting"),
    reconnect_after_disconnect(conn_from_term(Conn), State);
handle_connect_timeout(Token, #{connecting_token := Token} = State) ->
    Pid = maps:get(connecting_pid, State, undefined),
    gateway_nats_pool_conn:maybe_kill_connect_pid(Pid),
    logger:warning("Gateway NATS RPC connect attempt timed out, retrying"),
    gateway_nats_rpc_handler:do_connect(
        State#{
            connecting_pid => undefined,
            connecting_token => undefined,
            connecting_timer => undefined
        }
    );
handle_connect_timeout(_Token, State) ->
    State.

-spec ensure_subscriptions(map()) -> map().
ensure_subscriptions(#{ready := true, conn := Conn} = State) when Conn =/= undefined ->
    {RpcState, RpcDone} = gateway_nats_rpc_handler:do_subscribe(State),
    {ExtraState, ExtraDone} = subscribe_extra_subjects(Conn, RpcState),
    publish_status(subscribe_outcome(RpcDone andalso ExtraDone, ExtraState));
ensure_subscriptions(State) ->
    publish_status(State).

-spec subscribe_outcome(boolean(), map()) -> map().
subscribe_outcome(true, State) ->
    State#{subscribe_failures => 0};
subscribe_outcome(false, #{subscribe_timer := TimerRef} = State) when is_reference(TimerRef) ->
    State;
subscribe_outcome(false, State) ->
    Failures = maps:get(subscribe_failures, State, 0) + 1,
    Delay = gateway_nats_pool_conn:subscribe_retry_delay(Failures),
    logger:warning("Gateway NATS RPC will retry its subscriptions", #{
        attempt => Failures, delay_ms => Delay
    }),
    State#{
        subscribe_failures => Failures,
        subscribe_timer => erlang:send_after(Delay, self(), resubscribe)
    }.

-spec publish_status(map()) -> map().
publish_status(State) ->
    Status = subscription_status(State),
    case persistent_term:get(?STATUS_KEY, undefined) of
        Status -> ok;
        _ -> persistent_term:put(?STATUS_KEY, Status)
    end,
    State.

-spec subscription_status(map()) -> not_required | ready | missing.
subscription_status(#{rpc_enabled := false}) ->
    not_required;
subscription_status(State) ->
    Role = fluxer_gateway_sup:current_role(),
    case gateway_nats_rpc_handler:rpc_subjects_for_role(Role) of
        [] -> not_required;
        Subjects -> subjects_status(Subjects, State)
    end.

-spec subjects_status([binary()], map()) -> ready | missing.
subjects_status(Subjects, #{ready := true, rpc_subs := Subs}) ->
    Subscribed = lists:all(fun(Subject) -> maps:is_key(Subject, Subs) end, Subjects),
    case Subscribed of
        true -> ready;
        false -> missing
    end;
subjects_status(_Subjects, _State) ->
    missing.

-spec subscribe_extra_subject(binary(), binary(), map()) ->
    {{ok, term()} | {error, term()}, map()}.
subscribe_extra_subject(Subject, QueueGroup, #{extra_subscriptions := Extra} = State) ->
    Key = {Subject, QueueGroup},
    case maps:get(Key, Extra, undefined) of
        #{sid := Sid} when Sid =/= undefined ->
            {{ok, Sid}, State};
        _ ->
            Wanted = #{subject => Subject, queue_group => QueueGroup, sid => undefined},
            subscribe_wanted_extra_subject(Key, State#{
                extra_subscriptions => Extra#{Key => Wanted}
            })
    end.

-spec subscribe_wanted_extra_subject(term(), map()) ->
    {{ok, term()} | {error, term()}, map()}.
subscribe_wanted_extra_subject(Key, #{ready := true, conn := Conn} = State) when
    Conn =/= undefined
->
    NewState = ensure_subscriptions(State),
    case maps:get(Key, maps:get(extra_subscriptions, NewState)) of
        #{sid := Sid} when Sid =/= undefined -> {{ok, Sid}, NewState};
        _ -> {{error, subscribe_failed}, NewState}
    end;
subscribe_wanted_extra_subject(_Key, State) ->
    {{error, not_connected}, State}.

-spec subscribe_extra_subjects(nats:conn(), map()) -> {map(), boolean()}.
subscribe_extra_subjects(Conn, #{extra_subscriptions := Extra} = State) ->
    NewExtra = maps:fold(
        fun(Key, Sub, Acc) ->
            subscribe_recorded_extra_subject(Conn, Key, Sub, Acc)
        end,
        Extra,
        Extra
    ),
    Done = lists:all(fun(#{sid := Sid}) -> Sid =/= undefined end, maps:values(NewExtra)),
    {State#{extra_subscriptions => NewExtra}, Done}.

-spec subscribe_recorded_extra_subject(nats:conn(), term(), map(), map()) -> map().
subscribe_recorded_extra_subject(_Conn, _Key, #{sid := Sid}, Acc) when Sid =/= undefined ->
    Acc;
subscribe_recorded_extra_subject(
    Conn, Key, #{subject := Subject, queue_group := QueueGroup} = Sub, Acc
) ->
    case gateway_nats_pool_conn:guarded_sub(Conn, Subject, subscription_opts(QueueGroup)) of
        {ok, Sid} ->
            notify_if_resubscribed(maps:get(subscribed_before, Sub, false), Subject),
            Acc#{Key => Sub#{sid => Sid, subscribed_before => true}};
        {error, Reason} ->
            logger:error("Gateway NATS RPC failed to subscribe extra subject", #{
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
subject_owner(<<"config.channel.threads">>) -> channel_threads_config;
subject_owner(_Subject) -> undefined.

-spec notify_if_resubscribed(boolean(), binary()) -> ok.
notify_if_resubscribed(true, Subject) ->
    notify_resubscribed(Subject);
notify_if_resubscribed(false, _Subject) ->
    ok.

-spec notify_resubscribed(binary()) -> ok.
notify_resubscribed(Subject) ->
    case subject_owner(Subject) of
        undefined -> ok;
        Owner -> notify_owner(whereis(Owner), Subject)
    end.

-spec notify_owner(pid() | port() | undefined, binary()) -> ok.
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
    _ = spawn(fun() -> gateway_nats_pool:pub_reply(ReplyTo, ErrResp) end),
    State;
do_dispatch_rpc(ReplyTo, Subject, Payload, HC, _MaxH, HRefs, State) ->
    {_Pid, MRef} = spawn_monitor(fun() ->
        gateway_nats_rpc_handler:handle_rpc_request(Subject, Payload, ReplyTo)
    end),
    State#{handler_count => HC + 1, handler_refs => HRefs#{MRef => true}}.

-spec do_disable_rpc(map()) -> map().
do_disable_rpc(#{conn := Conn, rpc_subs := Subs} = State) when
    Conn =/= undefined, map_size(Subs) > 0
->
    [gateway_nats_pool_conn:ignore_unsub(Conn, Sid) || Sid <- maps:values(Subs)],
    ignore_pool_call(fun gateway_nats_pool:disable_rpc_subscription/0),
    logger:info("Gateway NATS RPC unsubscribed", #{subscription_count => map_size(Subs)}),
    publish_status(State#{rpc_subs => #{}, rpc_enabled => false});
do_disable_rpc(State) ->
    ignore_pool_call(fun gateway_nats_pool:disable_rpc_subscription/0),
    publish_status(State#{rpc_subs => #{}, rpc_enabled => false}).

-spec conn_from_term(term()) -> nats:conn().
conn_from_term(Conn) when is_pid(Conn) ->
    Conn.

-spec ignore_pool_call(fun(() -> term())) -> ok.
ignore_pool_call(PoolCall) ->
    try PoolCall() of
        _ -> ok
    catch
        throw:_ -> ok;
        error:_ -> ok;
        exit:_ -> ok
    end.
