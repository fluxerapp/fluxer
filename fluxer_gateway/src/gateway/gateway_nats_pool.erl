%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_nats_pool).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/0, pub/2, pub_reply/2, reply_publish_failures/0, get_pool_status/0]).
-export([has_ready_connection/0]).
-export([enable_rpc_subscription/0, disable_rpc_subscription/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PERSISTENT_TERM_KEY, {?MODULE, connections}).
-define(REPLY_FAILURE_KEY, {?MODULE, reply_publish_failures}).

-dialyzer(
    {no_opaque, [
        handle_info/2,
        handle_connect_ok/4,
        handle_connect_ok_entry/5,
        accept_connect_ok/4,
        ignore_stale_connect_ok/2,
        handle_ready/2,
        accept_connect_timeout/3,
        handle_conn_event_down/2,
        handle_slot_monitor_down/5,
        conn_from_term/1
    ]}
).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec pub(binary(), iodata()) -> ok | {error, term()}.
pub(Subject, Payload) ->
    case gateway_nats_pool_conn:get_pool_conn() of
        {ok, Conn} -> nats:pub(Conn, Subject, Payload);
        Error -> Error
    end.

-spec pub_reply(binary(), iodata()) -> ok | {error, term()}.
pub_reply(Subject, Payload) ->
    case pub(Subject, Payload) of
        ok ->
            ok;
        {error, Reason} = Error ->
            record_reply_publish_failure(),
            logger:warning("Gateway NATS pool reply publish failed", #{
                subject => Subject, reason => Reason
            }),
            Error
    end.

-spec reply_publish_failures() -> non_neg_integer().
reply_publish_failures() ->
    case persistent_term:get(?REPLY_FAILURE_KEY, undefined) of
        Ref when Ref =/= undefined ->
            max(0, counters:get(Ref, 1));
        undefined ->
            0
    end.

-spec record_reply_publish_failure() -> ok.
record_reply_publish_failure() ->
    try
        Ref = reply_failure_counter(),
        counters:add(Ref, 1, 1),
        ok
    catch
        error:_ -> ok
    end.

-spec reply_failure_counter() -> counters:counters_ref().
reply_failure_counter() ->
    case persistent_term:get(?REPLY_FAILURE_KEY, undefined) of
        Ref when Ref =/= undefined ->
            Ref;
        undefined ->
            Ref = counters:new(1, [write_concurrency]),
            persistent_term:put(?REPLY_FAILURE_KEY, Ref),
            Ref
    end.

-spec has_ready_connection() -> boolean().
has_ready_connection() ->
    case gateway_nats_pool_conn:get_pool_conn() of
        {ok, _Conn} -> true;
        {error, _Reason} -> false
    end.

-spec get_pool_status() -> map().
get_pool_status() ->
    gen_server:call(?MODULE, get_pool_status).

-spec enable_rpc_subscription() -> ok.
enable_rpc_subscription() ->
    gen_server:call(?MODULE, enable_rpc_subscription, 5000).

-spec disable_rpc_subscription() -> ok.
disable_rpc_subscription() ->
    gen_server:call(?MODULE, disable_rpc_subscription, 5000).

-spec init([]) -> {ok, map()}.
init([]) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 10),
    PoolSize = gateway_nats_pool_conn:pool_size(),
    Slots = lists:duplicate(PoolSize, undefined),
    persistent_term:put(?PERSISTENT_TERM_KEY, list_to_tuple(Slots)),
    persistent_term:put(?REPLY_FAILURE_KEY, counters:new(1, [write_concurrency])),
    self() ! connect_all,
    {ok, #{
        pool_size => PoolSize,
        slots => #{},
        monitors => #{},
        connecting => #{},
        rpc_enabled => gateway_nats_pool_conn:nats_rpc_enabled(),
        subs => #{},
        sub_failures => #{},
        handler_count => 0,
        handler_refs => #{},
        max_handlers => gateway_nats_pool_conn:max_handlers()
    }}.

-spec handle_call(term(), gen_server:from(), map()) -> {reply, term(), map()}.
handle_call(get_pool_status, _From, State) ->
    {reply, build_pool_status(State), State};
handle_call(enable_rpc_subscription, _From, #{slots := Slots} = State) ->
    NewState = enable_subscriptions(Slots, State#{rpc_enabled => true}),
    {reply, ok, NewState};
handle_call(disable_rpc_subscription, _From, #{slots := Slots, subs := Subs} = State) ->
    disable_subscriptions(Slots, Subs),
    logger:info("Gateway NATS pool unsubscribed all slots from RPC"),
    {reply, ok, State#{rpc_enabled => false, subs => #{}}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(connect_all, #{pool_size := PoolSize} = State) ->
    {noreply, connect_all_slots(PoolSize, State)};
handle_info({nats_pool_connect_result, Idx, Pid, {ok, Conn}}, State) when
    is_integer(Idx), Idx >= 0, is_pid(Pid)
->
    {noreply, handle_connect_ok(Idx, Pid, conn_from_term(Conn), State)};
handle_info({nats_pool_connect_result, Idx, Pid, {error, Reason}}, State) when
    is_integer(Idx), Idx >= 0, is_pid(Pid)
->
    {noreply, handle_connect_error(Idx, Pid, Reason, State)};
handle_info({reconnect_slot, Idx}, #{slots := Slots} = State) when
    is_integer(Idx), Idx >= 0
->
    {noreply, reconnect_slot(Idx, Slots, State)};
handle_info({Conn, ready}, State) ->
    {noreply, handle_ready(conn_from_term(Conn), State)};
handle_info({resubscribe_slot, Idx}, State) when is_integer(Idx), Idx >= 0 ->
    {noreply, resubscribe_slot(Idx, State)};
handle_info({_Conn, _Sid, {msg, Subject, Payload, MsgOpts}}, State) when
    is_binary(Subject), is_binary(Payload), is_map(MsgOpts)
->
    {noreply, handle_rpc_msg(Subject, Payload, MsgOpts, State)};
handle_info({handler_done, _Pid}, State) ->
    {noreply, gateway_nats_pool_conn:decrement_handler_count(State)};
handle_info({Conn, closed}, State) ->
    {noreply, handle_conn_event_down(conn_from_term(Conn), State)};
handle_info({Conn, {error, _Reason}}, State) ->
    {noreply, handle_conn_event_down(conn_from_term(Conn), State)};
handle_info({'DOWN', MRef, process, Pid, _Reason}, State) when
    is_reference(MRef), is_pid(Pid)
->
    {noreply, handle_down(MRef, Pid, State)};
handle_info({connect_timeout, Idx, Token}, State) when
    is_integer(Idx), Idx >= 0, is_reference(Token)
->
    {noreply, handle_connect_timeout(Idx, Token, State)};
handle_info({connect_timeout, Idx}, State) when is_integer(Idx), Idx >= 0 ->
    {noreply, handle_legacy_connect_timeout(Idx, State)};
handle_info(_Info, State) ->
    {noreply, State}.

-spec connect_all_slots(pos_integer(), map()) -> map().
connect_all_slots(PoolSize, State) ->
    lists:foldl(
        fun gateway_nats_pool_conn:connect_slot/2,
        State,
        lists:seq(0, PoolSize - 1)
    ).

-spec reconnect_slot(non_neg_integer(), map(), map()) -> map().
reconnect_slot(Idx, Slots, State) ->
    case maps:find(Idx, Slots) of
        error -> gateway_nats_pool_conn:connect_slot(Idx, State);
        {ok, undefined} -> gateway_nats_pool_conn:connect_slot(Idx, State);
        {ok, _} -> State
    end.

-spec terminate(term(), map()) -> ok.
terminate(_Reason, #{slots := Slots}) ->
    maps:foreach(fun disconnect_slot/2, Slots),
    persistent_term:erase(?PERSISTENT_TERM_KEY),
    persistent_term:erase(?REPLY_FAILURE_KEY),
    ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec build_pool_status(map()) -> map().
build_pool_status(#{
    pool_size := PoolSize,
    slots := Slots,
    subs := Subs,
    handler_count := HandlerCount
}) ->
    ConnectedCount = maps:size(maps:filter(fun(_K, V) -> V =/= undefined end, Slots)),
    SubCount = maps:size(Subs),
    SlotList = [
        case maps:find(I, Slots) of
            error -> disconnected;
            {ok, _} -> connected
        end
     || I <- lists:seq(0, PoolSize - 1)
    ],
    #{
        connected => ConnectedCount,
        total => PoolSize,
        slots => SlotList,
        subscriptions => SubCount,
        handler_count => HandlerCount,
        reply_publish_failures => reply_publish_failures()
    }.

-spec enable_subscriptions(map(), map()) -> map().
enable_subscriptions(Slots, State) ->
    maps:fold(fun enable_slot_subscription/3, State, Slots).

-spec enable_slot_subscription(non_neg_integer(), nats:conn() | undefined, map()) -> map().
enable_slot_subscription(_Idx, undefined, State) ->
    State;
enable_slot_subscription(Idx, Conn, #{connecting := Connecting} = State) ->
    case maps:is_key(Idx, Connecting) of
        true -> State;
        false -> gateway_nats_pool_conn:subscribe_slot(Idx, Conn, State)
    end.

-spec resubscribe_slot(non_neg_integer(), map()) -> map().
resubscribe_slot(_Idx, #{rpc_enabled := false} = State) ->
    State;
resubscribe_slot(Idx, #{slots := Slots} = State) ->
    enable_slot_subscription(Idx, maps:get(Idx, Slots, undefined), State).

-spec disable_subscriptions(map(), map()) -> ok.
disable_subscriptions(Slots, Subs) ->
    maps:foreach(
        fun(Idx, Sids) ->
            disable_slot_subscription(Idx, Sids, Slots)
        end,
        Subs
    ).

-spec disable_slot_subscription(non_neg_integer(), term(), map()) -> ok.
disable_slot_subscription(Idx, Sids, Slots) ->
    case maps:find(Idx, Slots) of
        error -> ok;
        {ok, Conn} -> gateway_nats_pool_conn:unsubscribe_slot_sids(Conn, Sids)
    end.

-spec disconnect_slot(non_neg_integer(), nats:conn() | undefined) -> ok.
disconnect_slot(_Idx, undefined) ->
    ok;
disconnect_slot(_Idx, Conn) ->
    gateway_nats_pool_conn:ignore_disconnect(Conn).

-spec handle_connect_ok(non_neg_integer(), pid(), nats:conn(), map()) -> map().
handle_connect_ok(Idx, Pid, Conn, State) ->
    #{connecting := Connecting} = State,
    case maps:find(Idx, Connecting) of
        {ok, Entry} ->
            handle_connect_ok_entry(Idx, Pid, Entry, Conn, State);
        _ ->
            ignore_stale_connect_ok(Conn, State)
    end.

-spec handle_connect_ok_entry(non_neg_integer(), pid(), map(), nats:conn(), map()) -> map().
handle_connect_ok_entry(Idx, Pid, Entry, Conn, State) ->
    case connecting_entry_pid(Entry) of
        Pid -> accept_connect_ok(Idx, Entry, Conn, State);
        _ -> ignore_stale_connect_ok(Conn, State)
    end.

-spec accept_connect_ok(non_neg_integer(), map(), nats:conn(), map()) -> map().
accept_connect_ok(Idx, Entry, Conn, State) ->
    #{connecting := Connecting, slots := Slots, monitors := Monitors} = State,
    cleanup_old_conn(maps:get(Idx, Slots, undefined)),
    CleanMon = remove_slot_monitors(Idx, Monitors),
    MRef = nats:monitor(Conn),
    gateway_nats_pool_conn:reset_failure_count(Idx),
    NewState = State#{
        slots => Slots#{Idx => Conn},
        monitors => CleanMon#{MRef => Idx},
        connecting => Connecting#{Idx => connecting_entry_with_conn(Entry, Conn)}
    },
    case gateway_nats_pool_conn:conn_ready(Conn) of
        true -> mark_slot_ready(Idx, NewState);
        false -> NewState
    end.

-spec connecting_entry_with_conn(term(), nats:conn()) -> map().
connecting_entry_with_conn(Entry, Conn) when is_map(Entry) ->
    Entry#{conn => Conn};
connecting_entry_with_conn(Pid, Conn) ->
    #{pid => Pid, conn => Conn}.

-spec mark_slot_ready(non_neg_integer(), map()) -> map().
mark_slot_ready(Idx, #{connecting := Connecting} = State) ->
    cancel_connect_timer(maps:get(Idx, Connecting, undefined)),
    ReadyState = State#{connecting => maps:remove(Idx, Connecting)},
    gateway_nats_pool_conn:publish_ready_slots(ReadyState),
    logger:info("Gateway NATS pool slot connected", #{slot => Idx}),
    resubscribe_slot(Idx, ReadyState).

-spec ignore_stale_connect_ok(nats:conn(), map()) -> map().
ignore_stale_connect_ok(Conn, State) ->
    gateway_nats_pool_conn:ignore_disconnect(Conn),
    State.

-spec remove_slot_monitors(non_neg_integer(), map()) -> map().
remove_slot_monitors(Idx, Monitors) ->
    maps:filter(fun(_Ref, SlotIdx) -> SlotIdx =/= Idx end, Monitors).

-spec cleanup_old_conn(nats:conn() | undefined) -> ok.
cleanup_old_conn(undefined) -> ok;
cleanup_old_conn(OldConn) -> gateway_nats_pool_conn:ignore_disconnect(OldConn).

-spec handle_connect_error(non_neg_integer(), pid(), term(), map()) -> map().
handle_connect_error(Idx, Pid, Reason, #{connecting := Connecting} = State) ->
    case maps:find(Idx, Connecting) of
        {ok, Entry} ->
            handle_connect_error_entry(Idx, Pid, Entry, Reason, State);
        error ->
            State
    end.

-spec handle_connect_error_entry(non_neg_integer(), pid(), map(), term(), map()) -> map().
handle_connect_error_entry(Idx, Pid, Entry, Reason, State) ->
    case connecting_entry_pid(Entry) of
        Pid -> accept_connect_error(Idx, Entry, Reason, State);
        _ -> State
    end.

-spec accept_connect_error(non_neg_integer(), map(), term(), map()) -> map().
accept_connect_error(Idx, Entry, Reason, #{connecting := Connecting} = State) ->
    cancel_connect_timer(Entry),
    logger:error("Gateway NATS pool slot failed to connect", #{
        slot => Idx, reason => Reason
    }),
    gateway_nats_pool_conn:schedule_reconnect(Idx, self()),
    State#{connecting => maps:remove(Idx, Connecting)}.

-spec handle_ready(nats:conn(), map()) -> map().
handle_ready(Conn, #{connecting := Connecting} = State) ->
    case gateway_nats_pool_conn:find_slot_by_conn(Conn, State) of
        undefined -> State;
        Idx when is_map_key(Idx, Connecting) -> mark_slot_ready(Idx, State);
        Idx -> resubscribe_slot(Idx, State)
    end.

-spec handle_rpc_msg(binary(), binary(), map(), map()) -> map().
handle_rpc_msg(
    Subject,
    Payload,
    MsgOpts,
    #{
        handler_count := HC,
        max_handlers := MaxH,
        handler_refs := HRefs
    } = State
) ->
    case gateway_nats_pool_conn:is_rpc_subject(Subject) of
        false -> State;
        true -> dispatch_rpc_msg(Subject, Payload, MsgOpts, HC, MaxH, HRefs, State)
    end.

-spec dispatch_rpc_msg(
    binary(), binary(), map(), non_neg_integer(), pos_integer(), map(), map()
) ->
    map().
dispatch_rpc_msg(Subject, Payload, MsgOpts, HC, MaxH, HRefs, State) ->
    case maps:get(reply_to, MsgOpts, undefined) of
        undefined ->
            State;
        ReplyTo when HC >= MaxH ->
            send_overloaded_async(ReplyTo),
            State;
        ReplyTo ->
            {_Pid, MRef} = spawn_rpc_handler(Subject, Payload, ReplyTo),
            State#{handler_count => HC + 1, handler_refs => HRefs#{MRef => true}}
    end.

-spec spawn_rpc_handler(binary(), binary(), binary()) -> {pid(), reference()}.
spawn_rpc_handler(Subject, Payload, ReplyTo) ->
    spawn_monitor(fun() ->
        gateway_nats_rpc_handler:handle_rpc_request(Subject, Payload, ReplyTo)
    end).

-spec send_overloaded_async(binary()) -> ok.
send_overloaded_async(ReplyTo) ->
    _ = spawn(fun() -> send_overloaded(ReplyTo) end),
    ok.

-spec send_overloaded(binary()) -> ok | {error, term()}.
send_overloaded(ReplyTo) ->
    Resp = iolist_to_binary(
        json:encode(#{
            <<"ok">> => false, <<"error">> => <<"overloaded">>
        })
    ),
    gateway_nats_pool:pub_reply(ReplyTo, Resp).

-spec handle_conn_event_down(nats:conn(), map()) -> map().
handle_conn_event_down(Conn, #{pool_size := PoolSize} = State) ->
    case gateway_nats_pool_conn:find_slot_by_conn(Conn, State) of
        undefined ->
            gateway_nats_pool_conn:ignore_disconnect(Conn),
            State;
        Idx ->
            gateway_nats_pool_conn:handle_conn_down(Idx, Conn, PoolSize, State)
    end.

-spec handle_down(reference(), pid(), map()) -> map().
handle_down(
    MRef,
    Pid,
    #{
        handler_refs := HRefs,
        handler_count := HC,
        monitors := Monitors,
        slots := Slots,
        pool_size := PoolSize
    } = State
) ->
    case maps:is_key(MRef, HRefs) of
        true ->
            State#{
                handler_refs => maps:remove(MRef, HRefs),
                handler_count => max(0, HC - 1)
            };
        false ->
            handle_monitor_down(MRef, Pid, Monitors, Slots, PoolSize, State)
    end.

-spec handle_monitor_down(reference(), pid(), map(), map(), pos_integer(), map()) -> map().
handle_monitor_down(MRef, Pid, Monitors, Slots, PoolSize, State) ->
    case maps:find(MRef, Monitors) of
        error ->
            State;
        {ok, Idx} ->
            NewMonitors = maps:remove(MRef, Monitors),
            handle_slot_monitor_down(Idx, Pid, Slots, PoolSize, State#{monitors => NewMonitors})
    end.

-spec handle_slot_monitor_down(non_neg_integer(), pid(), map(), pos_integer(), map()) -> map().
handle_slot_monitor_down(Idx, Pid, Slots, PoolSize, State) ->
    case maps:find(Idx, Slots) of
        {ok, Pid} ->
            gateway_nats_pool_conn:handle_conn_down(
                Idx,
                conn_from_term(Pid),
                PoolSize,
                State
            );
        _ ->
            State
    end.

-spec handle_connect_timeout(non_neg_integer(), reference(), map()) -> map().
handle_connect_timeout(Idx, Token, #{connecting := Connecting} = State) ->
    case maps:find(Idx, Connecting) of
        {ok, Entry} ->
            handle_connect_timeout_entry(Idx, Token, Entry, State);
        error ->
            State
    end.

-spec handle_connect_timeout_entry(non_neg_integer(), reference(), map(), map()) -> map().
handle_connect_timeout_entry(Idx, Token, Entry, State) ->
    case connecting_entry_token(Entry) of
        Token -> accept_connect_timeout(Idx, Entry, State);
        _ -> State
    end.

-spec handle_legacy_connect_timeout(non_neg_integer(), map()) -> map().
handle_legacy_connect_timeout(Idx, #{connecting := Connecting} = State) ->
    case maps:find(Idx, Connecting) of
        {ok, Pid} when is_pid(Pid) -> accept_connect_timeout(Idx, Pid, State);
        _ -> State
    end.

-spec accept_connect_timeout(non_neg_integer(), term(), map()) -> map().
accept_connect_timeout(Idx, #{conn := Conn}, #{pool_size := PoolSize} = State) ->
    logger:warning("Gateway NATS pool slot connection did not become ready", #{slot => Idx}),
    gateway_nats_pool_conn:handle_conn_down(Idx, conn_from_term(Conn), PoolSize, State);
accept_connect_timeout(Idx, Entry, #{connecting := Connecting} = State) ->
    gateway_nats_pool_conn:maybe_kill_connect_pid(connecting_entry_pid(Entry)),
    logger:warning("Gateway NATS pool slot connect attempt timed out", #{slot => Idx}),
    gateway_nats_pool_conn:schedule_reconnect(Idx, self()),
    State#{connecting => maps:remove(Idx, Connecting)}.

-spec connecting_entry_pid(term()) -> pid() | undefined.
connecting_entry_pid(#{pid := Pid}) when is_pid(Pid) -> Pid;
connecting_entry_pid(Pid) when is_pid(Pid) -> Pid;
connecting_entry_pid(_) -> undefined.

-spec connecting_entry_token(term()) -> reference() | undefined.
connecting_entry_token(#{token := Token}) when is_reference(Token) -> Token;
connecting_entry_token(_) -> undefined.

-spec cancel_connect_timer(term()) -> ok.
cancel_connect_timer(#{timer := TimerRef}) when is_reference(TimerRef) ->
    _ = erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
    ok;
cancel_connect_timer(_) ->
    ok.

-spec conn_from_term(term()) -> nats:conn().
conn_from_term(Conn) when is_pid(Conn) ->
    Conn.
