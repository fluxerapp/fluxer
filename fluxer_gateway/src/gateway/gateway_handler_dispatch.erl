%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_handler_dispatch).
-typing([eqwalizer]).

-export([
    handle_opcode/3,
    handle_dispatch/4,
    handle_session_down/1,
    handle_session_reconnect/1,
    handle_presence_update/3,
    handle_request_guild_members/3,
    handle_request_guild_members_worker_down/1,
    handle_request_worker_down/4,
    handle_request_worker_timeout/3,
    cleanup_request_workers/1,
    handle_request_guild_counts/3,
    handle_request_channel_member_counts/3,
    handle_lazy_request/3,
    validate_presence_data/1,
    adjust_status/1
]).

-type state() :: gateway_handler:state().
-type ws_result() :: gateway_handler:ws_result().

-export_type([state/0, ws_result/0]).

-define(REQUEST_WORKER_TIMEOUT_MS, 10000).
-define(MAX_REQUEST_WORKERS, 4).

-type request_worker_type() ::
    request_guild_members | request_guild_counts | request_channel_member_counts | lazy_request.

-spec handle_opcode(atom(), map(), state()) -> ws_result().
handle_opcode(heartbeat, #{<<"d">> := Seq}, State) ->
    gateway_handler_heartbeat:handle_heartbeat(Seq, State);
handle_opcode(identify, #{<<"d">> := Data}, #{session_pid := undefined} = State) ->
    gateway_handler_identify:handle_identify(Data, peer_ip(State), State);
handle_opcode(identify, _, State) ->
    gateway_handler_encode:close_with_reason(
        already_authenticated, <<"Already authenticated">>, State
    );
handle_opcode(resume, #{<<"d">> := Data}, #{session_pid := undefined} = State) ->
    handle_rate_limited_resume(Data, State);
handle_opcode(resume, _, State) ->
    gateway_handler_encode:close_with_reason(
        already_authenticated, <<"Already authenticated">>, State
    );
handle_opcode(Op, #{<<"d">> := Data}, State) ->
    handle_authenticated_opcode(Op, Data, State);
handle_opcode(_, _, State) ->
    gateway_handler_encode:close_with_reason(unknown_opcode, <<"Unknown opcode">>, State).

-spec handle_authenticated_opcode(atom(), map(), state()) -> ws_result().
handle_authenticated_opcode(_Op, _Data, #{session_pid := undefined} = State) ->
    gateway_handler_encode:close_with_reason(not_authenticated, <<"Not authenticated">>, State);
handle_authenticated_opcode(presence_update, Data, #{session_pid := Pid} = State) when
    is_pid(Pid)
->
    handle_presence_update(Data, Pid, State);
handle_authenticated_opcode(voice_state_update, Data, #{session_pid := Pid} = State) when
    is_pid(Pid), is_map(Data)
->
    gateway_handler_voice:handle_voice_state_update(Pid, Data, State);
handle_authenticated_opcode(voice_state_update, _Data, State) ->
    gateway_handler_encode:close_with_reason(
        decode_error, <<"Invalid voice payload">>, State
    );
handle_authenticated_opcode(request_guild_members, Data, #{session_pid := Pid} = State) when
    is_pid(Pid)
->
    handle_request_guild_members(Data, Pid, State);
handle_authenticated_opcode(lazy_request, Data, #{session_pid := Pid} = State) when
    is_pid(Pid)
->
    handle_lazy_request(Data, Pid, State);
handle_authenticated_opcode(request_guild_counts, Data, #{session_pid := Pid} = State) when
    is_pid(Pid)
->
    handle_request_guild_counts(Data, Pid, State);
handle_authenticated_opcode(
    request_channel_member_counts, Data, #{session_pid := Pid} = State
) when
    is_pid(Pid)
->
    handle_request_channel_member_counts(Data, Pid, State);
handle_authenticated_opcode(_, _, State) ->
    gateway_handler_encode:close_with_reason(unknown_opcode, <<"Unknown opcode">>, State).

-spec handle_rate_limited_resume(map(), state()) -> ws_result().
handle_rate_limited_resume(Data, State) ->
    case session_abuse_protection:check_identify_rate(peer_ip(State)) of
        {error, identify_rate_limited} ->
            logger:debug("Holding gateway resume: reason=rate_limited"),
            {ok, State};
        ok ->
            gateway_handler_identify:handle_resume(Data, State)
    end.

-spec handle_dispatch(
    atom() | binary(), map() | list() | null | {pre_encoded, binary()}, integer(), state()
) -> ws_result().
handle_dispatch(Event, Data, Seq, State) ->
    case gateway_event_pause:is_frozen() of
        true -> {ok, State};
        false -> do_dispatch(Event, Data, Seq, State)
    end.

-spec do_dispatch(
    atom() | binary(), map() | list() | null | {pre_encoded, binary()}, integer(), state()
) ->
    ws_result().
do_dispatch(Event, {pre_encoded, EncodedData}, Seq, State) ->
    dispatch_pre_encoded(Event, EncodedData, Seq, State);
do_dispatch(Event, Data, Seq, State) ->
    dispatch_standard(Event, Data, Seq, State).

-spec peer_ip(state()) -> binary().
peer_ip(#{peer_ip := PeerIP}) when is_binary(PeerIP) ->
    PeerIP;
peer_ip(_State) ->
    <<"unknown">>.

-spec dispatch_pre_encoded(atom() | binary(), binary(), integer(), state()) -> ws_result().
dispatch_pre_encoded(Event, EncodedData, Seq, #{compress_ctx := CompressCtx} = State) ->
    EventName = gateway_handler_encode:dispatch_event_name(Event),
    JsonFrame = iolist_to_binary([
        <<"{\"op\":0,\"t\":\"">>,
        EventName,
        <<"\",\"s\":">>,
        integer_to_binary(Seq),
        <<",\"d\":">>,
        EncodedData,
        <<"}">>
    ]),
    Ctx = gateway_handler_encode:ensure_compress_ctx(CompressCtx),
    case gateway_compress:compress(JsonFrame, Ctx) of
        {ok, Compressed, NewCompressCtx} ->
            Frame = gateway_handler_encode:make_frame(Compressed, text, NewCompressCtx),
            {[Frame], State#{compress_ctx => NewCompressCtx}};
        {error, _Reason} ->
            {ok, State}
    end.

-spec dispatch_standard(
    atom() | binary(), map() | list() | null, integer(), state()
) -> ws_result().
dispatch_standard(Event, Data, Seq, State) ->
    EventName = gateway_handler_encode:dispatch_event_name(Event),
    Message = #{
        <<"op">> => constants:opcode_to_num(dispatch),
        <<"t">> => EventName,
        <<"d">> => Data,
        <<"s">> => Seq
    },
    case gateway_handler_encode:encode_and_compress(Message, State) of
        {ok, Frame, NewState} ->
            trigger_gc_after_large_dispatch(Data),
            {[Frame], NewState};
        {error, _Reason} ->
            {ok, State}
    end.

-spec trigger_gc_after_large_dispatch(term()) -> ok.
trigger_gc_after_large_dispatch(Data) when is_map(Data) ->
    case map_size(Data) > 50 of
        true ->
            _ = erlang:garbage_collect(self(), [{type, major}]),
            ok;
        false ->
            ok
    end;
trigger_gc_after_large_dispatch(_) ->
    ok.

-spec handle_session_down(state()) -> ws_result().
handle_session_down(State) ->
    Message = #{<<"op">> => constants:opcode_to_num(invalid_session), <<"d">> => false},
    cleanup_request_workers(State),
    NewState = State#{
        session_pid => undefined,
        request_workers => #{},
        request_guild_members_pid => undefined,
        request_guild_members_pending => undefined
    },
    case gateway_handler_encode:encode_and_compress(Message, NewState) of
        {ok, Frame, NewState2} -> {[Frame], NewState2};
        {error, _} -> {ok, NewState}
    end.

-spec handle_session_reconnect(state()) -> ws_result().
handle_session_reconnect(State) ->
    Message = #{<<"op">> => constants:opcode_to_num(reconnect)},
    CloseReason = <<"Session drain requested; reconnect to continue">>,
    case gateway_handler_encode:encode_and_compress(Message, State) of
        {ok, Frame, NewState} ->
            CloseCode = constants:close_code_to_num(unknown_error),
            {[Frame, {close, CloseCode, CloseReason}], NewState};
        {error, _Reason} ->
            gateway_handler_encode:close_with_reason(unknown_error, CloseReason, State)
    end.

-spec handle_presence_update(map(), pid(), state()) -> ws_result().
handle_presence_update(Data, Pid, State) ->
    case validate_presence_data(Data) of
        {ok, PresenceData} ->
            gen_server:cast(Pid, {presence_update, PresenceData}),
            {ok, State};
        {error, _} ->
            gateway_handler_encode:close_with_reason(
                decode_error, <<"Invalid presence payload">>, State
            )
    end.

-spec validate_presence_data(map()) -> {ok, map()} | {error, invalid_presence}.
validate_presence_data(Data) when is_map(Data) ->
    case maps:get(<<"status">>, Data, undefined) of
        undefined -> {error, invalid_presence};
        StatusRaw -> parse_presence_status(StatusRaw, Data)
    end;
validate_presence_data(_) ->
    {error, invalid_presence}.

-spec parse_presence_status(term(), map()) -> {ok, map()} | {error, invalid_presence}.
parse_presence_status(StatusRaw, Data) ->
    try
        Status = utils:parse_status(StatusRaw),
        AdjustedStatus = adjust_status(Status),
        Afk = presence_boolean(<<"afk">>, Data),
        Mobile = presence_boolean(<<"mobile">>, Data),
        Base = #{status => AdjustedStatus, afk => Afk, mobile => Mobile},
        Result = maybe_add_custom_status(Base, Data),
        {ok, Result}
    catch
        error:function_clause -> {error, invalid_presence}
    end.

-spec presence_boolean(binary(), map()) -> boolean().
presence_boolean(Key, Data) ->
    case maps:get(Key, Data, false) of
        true -> true;
        _ -> false
    end.

-spec maybe_add_custom_status(map(), map()) -> map().
maybe_add_custom_status(Base, Data) ->
    case maps:find(<<"custom_status">>, Data) of
        {ok, CS} -> Base#{<<"custom_status">> => CS};
        error -> Base
    end.

-spec adjust_status(atom()) -> atom().
adjust_status(offline) -> invisible;
adjust_status(Other) -> Other.

-spec handle_request_guild_members(map(), pid(), state()) -> ws_result().
handle_request_guild_members(
    Data, _Pid, #{request_guild_members_pid := RequestPid} = State
) when
    is_pid(RequestPid)
->
    {ok, replace_pending_guild_members(Data, State)};
handle_request_guild_members(Data, Pid, State) ->
    start_request_guild_members_worker(Data, Pid, State).

-spec replace_pending_guild_members(map(), state()) -> state().
replace_pending_guild_members(Data, State) ->
    case maps:get(request_guild_members_pending, State, undefined) of
        undefined ->
            ok;
        _Existing ->
            logger:debug(
                "Gateway request_guild_members superseded pending request while one in flight"
            )
    end,
    State#{request_guild_members_pending => Data}.

-spec start_request_guild_members_worker(map(), pid(), state()) -> ws_result().
start_request_guild_members_worker(Data, Pid, State) ->
    SocketPid = self(),
    Fun = fun() ->
        do_request_guild_members(Data, SocketPid, Pid)
    end,
    case start_request_worker(request_guild_members, Fun, State) of
        {ok, WorkerPid, WorkerState} ->
            {ok, WorkerState#{
                request_guild_members_pid => WorkerPid,
                request_guild_members_pending => undefined
            }};
        dropped ->
            {ok, State}
    end.

-spec do_request_guild_members(map(), pid(), pid()) -> ok.
do_request_guild_members(Data, SocketPid, SessionPid) ->
    try do_request_guild_members_inner(Data, SocketPid, SessionPid) of
        ok -> ok
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok;
        throw:_Reason -> ok
    end.

-spec do_request_guild_members_inner(map(), pid(), pid()) -> ok.
do_request_guild_members_inner(Data, SocketPid, SessionPid) ->
    case gen_server:call(SessionPid, {get_state}, 5000) of
        SessionState when is_map(SessionState) ->
            _ = guild_request_members:handle_request(
                Data, SocketPid, SessionState#{session_pid => SessionPid}
            ),
            ok;
        _ ->
            ok
    end.

-spec handle_request_guild_members_worker_down(state()) -> ws_result().
handle_request_guild_members_worker_down(State) ->
    PendingData = maps:get(request_guild_members_pending, State, undefined),
    SessionPid = maps:get(session_pid, State, undefined),
    BaseState = State#{
        request_guild_members_pid => undefined,
        request_guild_members_pending => undefined
    },
    maybe_restart_guild_members_worker(PendingData, SessionPid, BaseState).

-spec maybe_restart_guild_members_worker(term(), term(), state()) -> ws_result().
maybe_restart_guild_members_worker(Data, Pid, State) when is_map(Data), is_pid(Pid) ->
    start_request_guild_members_worker(Data, Pid, State);
maybe_restart_guild_members_worker(_, _, State) ->
    {ok, State}.

-spec handle_request_worker_down(reference(), pid(), term(), state()) -> ws_result().
handle_request_worker_down(Ref, Pid, Reason, State) ->
    Workers = request_workers(State),
    case maps:take(Ref, Workers) of
        {Worker, Remaining} ->
            handle_known_request_worker_down(Worker, Remaining, Reason, State);
        error ->
            handle_legacy_request_worker_down(Pid, State)
    end.

-spec handle_known_request_worker_down(map(), map(), term(), state()) -> ws_result().
handle_known_request_worker_down(Worker, Remaining, Reason, State) ->
    cancel_request_worker_timer(Worker),
    Type = maps:get(type, Worker, unknown),
    log_request_worker_finished(Type, Reason, maps:size(Remaining)),
    WorkerState = State#{request_workers => Remaining},
    case Type of
        request_guild_members ->
            handle_request_guild_members_worker_down(WorkerState);
        _ ->
            {ok, WorkerState}
    end.

-spec handle_legacy_request_worker_down(pid(), state()) -> ws_result().
handle_legacy_request_worker_down(Pid, #{request_guild_members_pid := Pid} = State) ->
    handle_request_guild_members_worker_down(State);
handle_legacy_request_worker_down(_Pid, State) ->
    {ok, State}.

-spec handle_request_worker_timeout(reference(), term(), state()) -> ws_result().
handle_request_worker_timeout(Ref, Type, State) ->
    Workers = request_workers(State),
    case maps:take(Ref, Workers) of
        {#{type := Type} = Worker, Remaining} ->
            timeout_request_worker(Ref, Worker, Remaining, State);
        _ ->
            {ok, State}
    end.

-spec timeout_request_worker(reference(), map(), map(), state()) -> ws_result().
timeout_request_worker(Ref, Worker, Remaining, State) ->
    erlang:demonitor(Ref, [flush]),
    maybe_kill_request_worker(maps:get(pid, Worker, undefined)),
    Type = maps:get(type, Worker, unknown),
    logger:warning("Gateway websocket request worker timed out", #{
        type => Type, remaining_workers => maps:size(Remaining)
    }),
    WorkerState = State#{request_workers => Remaining},
    case Type of
        request_guild_members ->
            handle_request_guild_members_worker_down(WorkerState);
        _ ->
            {ok, WorkerState}
    end.

-spec handle_request_guild_counts(map(), pid(), state()) -> ws_result().
handle_request_guild_counts(Data, Pid, State) ->
    SocketPid = self(),
    Fun = fun() -> do_request_guild_counts(Data, SocketPid, Pid) end,
    request_worker_result(start_request_worker(request_guild_counts, Fun, State), State).

-spec do_request_guild_counts(map(), pid(), pid()) -> ok.
do_request_guild_counts(Data, SocketPid, SessionPid) ->
    try do_request_guild_counts_inner(Data, SocketPid, SessionPid) of
        ok -> ok
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok;
        throw:_Reason -> ok
    end.

-spec do_request_guild_counts_inner(map(), pid(), pid()) -> ok.
do_request_guild_counts_inner(Data, SocketPid, SessionPid) ->
    case gen_server:call(SessionPid, {get_state}, 5000) of
        SessionState when is_map(SessionState) ->
            guild_request_counts:handle_request(
                Data, SocketPid, SessionState#{session_pid => SessionPid}
            );
        _ ->
            ok
    end.

-spec handle_request_channel_member_counts(map(), pid(), state()) -> ws_result().
handle_request_channel_member_counts(Data, Pid, State) ->
    SocketPid = self(),
    Fun = fun() -> do_request_channel_member_counts(Data, SocketPid, Pid) end,
    request_worker_result(
        start_request_worker(request_channel_member_counts, Fun, State), State
    ).

-spec do_request_channel_member_counts(map(), pid(), pid()) -> ok.
do_request_channel_member_counts(Data, SocketPid, SessionPid) ->
    try do_request_channel_member_counts_inner(Data, SocketPid, SessionPid) of
        ok -> ok
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok;
        throw:_Reason -> ok
    end.

-spec do_request_channel_member_counts_inner(map(), pid(), pid()) -> ok.
do_request_channel_member_counts_inner(Data, SocketPid, SessionPid) ->
    case gen_server:call(SessionPid, {get_state}, 5000) of
        SessionState when is_map(SessionState) ->
            guild_request_channel_member_counts:handle_request(
                Data, SocketPid, SessionState#{session_pid => SessionPid}
            );
        _ ->
            ok
    end.

-spec handle_lazy_request(map(), pid(), state()) -> ws_result().
handle_lazy_request(Data, Pid, State) ->
    SocketPid = self(),
    Fun = fun() -> do_lazy_request(Data, SocketPid, Pid) end,
    request_worker_result(start_request_worker(lazy_request, Fun, State), State).

-spec request_worker_result({ok, pid(), state()} | dropped, state()) -> ws_result().
request_worker_result({ok, _Pid, WorkerState}, _State) ->
    {ok, WorkerState};
request_worker_result(dropped, State) ->
    {ok, State}.

-spec start_request_worker(request_worker_type(), fun(() -> ok), state()) ->
    {ok, pid(), state()} | dropped.
start_request_worker(Type, Fun, State) ->
    Workers = request_workers(State),
    MaxWorkers = request_worker_max(State),
    case maps:size(Workers) < MaxWorkers of
        true ->
            {Pid, Ref} = spawn_monitor(Fun),
            TimerRef = erlang:send_after(
                ?REQUEST_WORKER_TIMEOUT_MS,
                self(),
                {gateway_request_worker_timeout, Ref, Type}
            ),
            Worker = #{pid => Pid, type => Type, timer => TimerRef},
            {ok, Pid, State#{request_workers => Workers#{Ref => Worker}}};
        false ->
            logger:warning("Gateway websocket request worker limit reached", #{
                type => Type,
                active_workers => maps:size(Workers),
                max_workers => MaxWorkers
            }),
            dropped
    end.

-spec request_workers(state()) -> map().
request_workers(State) ->
    maps:get(request_workers, State, #{}).

-spec request_worker_max(state()) -> pos_integer().
request_worker_max(State) ->
    case maps:get(request_worker_max, State, ?MAX_REQUEST_WORKERS) of
        Max when is_integer(Max), Max > 0 -> Max;
        _ -> ?MAX_REQUEST_WORKERS
    end.

-spec cleanup_request_workers(state()) -> ok.
cleanup_request_workers(State) ->
    maps:foreach(
        fun(Ref, Worker) ->
            erlang:demonitor(Ref, [flush]),
            cancel_request_worker_timer(Worker),
            maybe_kill_request_worker(maps:get(pid, Worker, undefined))
        end,
        request_workers(State)
    ),
    ok.

-spec cancel_request_worker_timer(map()) -> ok.
cancel_request_worker_timer(#{timer := TimerRef}) when is_reference(TimerRef) ->
    _ = erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
    ok;
cancel_request_worker_timer(_) ->
    ok.

-spec maybe_kill_request_worker(term()) -> ok.
maybe_kill_request_worker(Pid) when is_pid(Pid) ->
    exit(Pid, kill),
    ok;
maybe_kill_request_worker(_) ->
    ok.

-spec log_request_worker_finished(term(), term(), non_neg_integer()) -> ok.
log_request_worker_finished(Type, normal, Remaining) ->
    logger:debug("Gateway websocket request worker completed", #{
        type => Type, remaining_workers => Remaining
    });
log_request_worker_finished(Type, Reason, Remaining) ->
    logger:warning("Gateway websocket request worker exited", #{
        type => Type, reason => Reason, remaining_workers => Remaining
    }).

-spec do_lazy_request(map(), pid(), pid()) -> ok.
do_lazy_request(Data, SocketPid, SessionPid) ->
    try do_lazy_request_inner(Data, SocketPid, SessionPid) of
        ok -> ok
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok;
        throw:_Reason -> ok
    end.

-spec do_lazy_request_inner(map(), pid(), pid()) -> ok.
do_lazy_request_inner(Data, SocketPid, SessionPid) ->
    case gen_server:call(SessionPid, {get_state}, 5000) of
        SessionState when is_map(SessionState) ->
            guild_unified_subscriptions:handle_subscriptions(
                Data, SocketPid, SessionState#{session_pid => SessionPid}
            );
        _ ->
            ok
    end.
