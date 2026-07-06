%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_handler_resume).
-typing([eqwalizer]).

-export([
    handle_resume/2,
    validate_resume_data/1
]).

-type state() :: gateway_handler:state().
-type ws_result() :: gateway_handler:ws_result().

-export_type([state/0, ws_result/0]).

-spec handle_resume(map(), state()) -> ws_result().
handle_resume(Data, State) ->
    GwTimings0 = gateway_timings:new(),
    ValidateStartedAt = gateway_timings:start(),
    ValidateResult = validate_resume_data(Data),
    GwTimings = gateway_timings:record_function(
        validate_resume_data,
        <<"gateway_handler_identify:validate_resume_data/1">>,
        ValidateStartedAt,
        GwTimings0
    ),
    case ValidateResult of
        {ok, Token, SessionId, Seq} ->
            do_resume_lookup(Token, SessionId, Seq, GwTimings, State);
        {error, _} ->
            gateway_handler_encode:close_with_reason(
                decode_error,
                <<"Invalid resume payload">>,
                State
            )
    end.

-spec do_resume_lookup(binary(), binary(), integer(), gateway_timings:recorder(), state()) ->
    ws_result().
do_resume_lookup(Token, SessionId, Seq, GwTimings0, State) ->
    LookupStartedAt = gateway_timings:start(),
    LookupResult = session_manager:lookup_or_rehydrate(SessionId, Token, self()),
    GwTimings = gateway_timings:record_function(
        lookup_or_rehydrate,
        <<"session_manager:lookup_or_rehydrate/3">>,
        LookupStartedAt,
        resume_lookup_trace_meta(SessionId),
        GwTimings0
    ),
    case LookupResult of
        {ok, Pid} when is_pid(Pid) ->
            handle_resume_with_session(Pid, Token, Seq, GwTimings, State);
        {error, invalid_token} ->
            gateway_handler_encode:close_with_reason(
                authentication_failed,
                <<"Invalid token">>,
                State
            );
        {error, _} ->
            send_invalid_session(State)
    end.

-spec handle_resume_with_session(
    pid(), binary(), integer(), gateway_timings:recorder(), state()
) ->
    ws_result().
handle_resume_with_session(Pid, Token, Seq, GwTimings0, State) ->
    VerifyStartedAt = gateway_timings:start(),
    try gen_server:call(Pid, {token_verify, Token}, 5000) of
        VerifyResult ->
            GwTimings = gateway_timings:record_function(
                token_verify,
                <<"session_lifecycle:handle_token_verify/2">>,
                VerifyStartedAt,
                session_pid_trace_meta(Pid),
                GwTimings0
            ),
            handle_token_verify_result(VerifyResult, Pid, Seq, GwTimings, State)
    catch
        exit:_ ->
            gateway_handler_encode:close_with_reason(
                unknown_error,
                <<"Session unavailable">>,
                State
            )
    end.

-spec handle_token_verify_result(
    term(), pid(), integer(), gateway_timings:recorder(), state()
) -> ws_result().
handle_token_verify_result(true, Pid, Seq, GwTimings, State) ->
    do_resume_verified(Pid, Seq, GwTimings, State);
handle_token_verify_result(false, _Pid, _Seq, _GwTimings, State) ->
    gateway_handler_encode:close_with_reason(
        authentication_failed,
        <<"Invalid token">>,
        State
    );
handle_token_verify_result(_VerifyResult, _Pid, _Seq, _GwTimings, State) ->
    gateway_handler_encode:close_with_reason(
        unknown_error,
        <<"Session unavailable">>,
        State
    ).

-spec do_resume_verified(pid(), integer(), gateway_timings:recorder(), state()) -> ws_result().
do_resume_verified(Pid, Seq, GwTimings0, State) ->
    SocketPid = self(),
    ResumeStartedAt = gateway_timings:start(),
    try gen_server:call(Pid, {resume, Seq, SocketPid}, 5000) of
        ResumeResult ->
            GwTimings = gateway_timings:record_function(
                resume_session,
                <<"session_lifecycle:handle_resume/3">>,
                ResumeStartedAt,
                session_pid_trace_meta(Pid),
                GwTimings0
            ),
            handle_resume_call_result(ResumeResult, Pid, GwTimings, State)
    catch
        exit:_ ->
            gateway_handler_encode:close_with_reason(
                unknown_error,
                <<"Session unavailable">>,
                State
            )
    end.

-spec handle_resume_call_result(term(), pid(), gateway_timings:recorder(), state()) ->
    ws_result().
handle_resume_call_result({ok, MissedEvents, CurrentSeq}, Pid, GwTimings, State) when
    is_integer(CurrentSeq), is_list(MissedEvents)
->
    finalize_resume(Pid, CurrentSeq, MissedEvents, GwTimings, State);
handle_resume_call_result(invalid_seq, _Pid, _GwTimings, State) ->
    gateway_handler_encode:close_with_reason(invalid_seq, <<"Invalid sequence">>, State);
handle_resume_call_result(_ResumeResult, _Pid, _GwTimings, State) ->
    gateway_handler_encode:close_with_reason(
        unknown_error,
        <<"Session unavailable">>,
        State
    ).

-spec finalize_resume(pid(), integer(), [term()], gateway_timings:recorder(), state()) ->
    ws_result().
finalize_resume(Pid, Seq, MissedEvents, GwTimings0, State) ->
    SocketPid = self(),
    monitor(process, Pid),
    ReplayStartedAt = gateway_timings:start(),
    replay_missed_events(MissedEvents, SocketPid),
    GwTimings = gateway_timings:record_function(
        replay_missed_events,
        <<"gateway_handler_identify:replay_missed_events/2">>,
        ReplayStartedAt,
        GwTimings0
    ),
    ResumedData = put_resumed_gateway_timings(GwTimings),
    SocketPid ! {dispatch, resumed, ResumedData, Seq},
    erlang:garbage_collect(self(), [{type, major}]),
    {ok, State#{
        session_pid => Pid,
        heartbeat_state => #{
            last_ack => erlang:system_time(millisecond), waiting_for_ack => false
        }
    }}.

-spec resume_lookup_trace_meta(binary()) -> map().
resume_lookup_trace_meta(SessionId) ->
    try gateway_node_router:owner_node_result(SessionId, sessions) of
        {ok, OwnerNode} when is_atom(OwnerNode) ->
            remote_trace_meta(session_manager, OwnerNode);
        _ ->
            #{}
    catch
        error:_Reason -> #{};
        exit:_Reason -> #{}
    end.

-spec session_pid_trace_meta(pid()) -> map().
session_pid_trace_meta(Pid) ->
    node_trace_meta(session, node(Pid)).

-spec node_trace_meta(atom(), node()) -> map().
node_trace_meta(Operation, NodeName) ->
    #{
        remote => #{
            <<"operation">> => trace_key(Operation),
            <<"pod_name">> => node_pod_name_or_name(NodeName)
        }
    }.

-spec trace_key(atom()) -> binary().
trace_key(Value) ->
    atom_to_binary(Value, utf8).

-spec node_pod_name_or_name(node()) -> binary().
node_pod_name_or_name(NodeName) ->
    case gateway_node_metadata:pod_name_for_node(NodeName) of
        PodName when is_binary(PodName), byte_size(PodName) > 0 ->
            PodName;
        _ ->
            atom_to_binary(NodeName, utf8)
    end.

-spec remote_trace_meta(term(), node()) -> map().
remote_trace_meta(Operation, NodeName) ->
    case gateway_timings:remote_node(Operation, NodeName) of
        Remote when is_map(Remote) -> #{remote => Remote};
        _ -> #{}
    end.

-spec put_resumed_gateway_timings(gateway_timings:recorder()) -> map().
put_resumed_gateway_timings(GwTimings) ->
    #{<<"_timings_gw">> => gateway_timings_payload:finalize(GwTimings)}.

-spec replay_missed_events([term()], pid()) -> ok.
replay_missed_events([], _SocketPid) ->
    ok;
replay_missed_events([Event | Rest], SocketPid) when is_map(Event) ->
    SocketPid !
        {
            dispatch,
            maps:get(event, Event),
            guild_data_wire:payload(maps:get(data, Event)),
            maps:get(seq, Event)
        },
    replay_missed_events(Rest, SocketPid);
replay_missed_events([_ | Rest], SocketPid) ->
    replay_missed_events(Rest, SocketPid).

-spec validate_resume_data(map()) -> {ok, binary(), binary(), integer()} | {error, atom()}.
validate_resume_data(Data) when is_map(Data) ->
    Token = maps:get(<<"token">>, Data, undefined),
    SessionId = maps:get(<<"session_id">>, Data, undefined),
    Seq = maps:get(<<"seq">>, Data, undefined),
    validate_resume_fields(Token, SessionId, Seq);
validate_resume_data(_) ->
    {error, invalid_data}.

-spec validate_resume_fields(term(), term(), term()) ->
    {ok, binary(), binary(), integer()} | {error, atom()}.
validate_resume_fields(Token, SessionId, Seq) when
    is_binary(Token), is_binary(SessionId), is_integer(Seq)
->
    {ok, Token, SessionId, Seq};
validate_resume_fields(_, _, _) ->
    {error, missing_required_field}.

-spec send_invalid_session(state()) -> ws_result().
send_invalid_session(State) ->
    Message = #{<<"op">> => constants:opcode_to_num(invalid_session), <<"d">> => false},
    case gateway_handler_encode:encode_and_compress(Message, State) of
        {ok, Frame, NewState} -> {[Frame], NewState};
        {error, _} -> {ok, State}
    end.
