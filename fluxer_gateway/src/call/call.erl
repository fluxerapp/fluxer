%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(call).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/1, start_link_from_state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type call_data() :: #{
    channel_id := integer(),
    message_id := integer(),
    region := binary() | undefined,
    ringing := [integer()],
    recipients := [integer()]
}.
-type call_request() ::
    {get_state}
    | export_handoff_state
    | {update_region, binary() | undefined}
    | {ring_recipients, [integer()]}
    | {stop_ringing, [integer()]}
    | {join, integer(), map(), binary(), pid()}
    | {join, integer(), map(), binary(), pid(), binary() | undefined}
    | {confirm_connection, binary()}
    | {disconnect_user_if_in_channel, integer(), integer(), binary() | undefined}
    | {leave, binary()}
    | {update_voice_state, integer(), map()}
    | {get_sessions}
    | {get_pending_connections}.
-type cast_request() :: {join_async, integer(), map(), binary(), pid()}.
-type info_message() ::
    {'DOWN', reference(), process, pid(), term()}
    | {ring_timeout, integer()}
    | {pending_connection_timeout, binary()}
    | idle_timeout.
-type start_result() :: {ok, pid()} | {error, term()}.

-spec start_link(call_data()) -> start_result().
start_link(CallData) ->
    normalize_start_link(gen_server:start_link(?MODULE, CallData, [])).

-spec start_link_from_state(map()) -> start_result().
start_link_from_state(State) ->
    normalize_start_link(gen_server:start_link(?MODULE, {transferred, State}, [])).

-spec init(call_data() | {transferred, map()}) -> {ok, map()}.
init({transferred, TransferState}) ->
    erlang:process_flag(fullsweep_after, 10),
    State = call_handoff:restore_state(TransferState),
    erlang:garbage_collect(),
    {ok, State};
init(CallData) ->
    erlang:process_flag(fullsweep_after, 10),
    State = build_initial_state(CallData),
    FinalState = run_init_pipeline(State),
    erlang:garbage_collect(),
    {ok, FinalState}.

-spec build_initial_state(call_data()) -> map().
build_initial_state(#{
    channel_id := ChannelId,
    message_id := MessageId,
    region := Region,
    ringing := Ringing,
    recipients := Recipients
}) ->
    #{
        channel_id => ChannelId,
        message_id => MessageId,
        region => Region,
        ringing => [],
        pending_ringing => Ringing,
        recipients => Recipients,
        voice_states => #{},
        sessions => #{},
        pending_connections => #{},
        initiator_ready => false,
        ringing_timers => #{},
        idle_timer => undefined,
        created_at => erlang:system_time(millisecond),
        participants_history => sets:new(),
        last_call_event => undefined
    }.

-spec run_init_pipeline(map()) -> map().
run_init_pipeline(State) ->
    ReadyState = call_ringing:ensure_initiator_ready(State),
    {StateWithRinging, Dispatched} = call_ringing:maybe_dispatch_pending_ringing(
        ReadyState, false
    ),
    StateWithIdleTimer = call_ringing:reset_idle_timer(StateWithRinging),
    StateWithCreate = call_ringing:dispatch_call_create(StateWithIdleTimer),
    maybe_dispatch_initial_update(Dispatched, StateWithCreate).

-spec maybe_dispatch_initial_update(boolean(), map()) -> map().
maybe_dispatch_initial_update(true, State) ->
    State;
maybe_dispatch_initial_update(false, State) ->
    case maps:get(ringing, State) of
        [] -> State;
        _ -> call_ringing:dispatch_call_update(State)
    end.

-spec handle_call(term(), gen_server:from(), map()) ->
    {reply, term(), map()} | {stop, normal, term(), map()}.
handle_call(Request, _From, State) ->
    case decode_call_request(Request) of
        {ok, CallRequest} -> handle_call_request(CallRequest, State);
        error -> {reply, ok, State}
    end.

-spec handle_call_request(call_request(), map()) ->
    {reply, term(), map()} | {stop, normal, term(), map()}.
handle_call_request({get_state}, State) ->
    {reply, {ok, build_get_state_reply(State)}, State};
handle_call_request(export_handoff_state, State) ->
    {reply, {ok, call_handoff:export_state(State)}, State};
handle_call_request({update_region, NewRegion}, State) ->
    handle_update_region(NewRegion, State);
handle_call_request({ring_recipients, Recipients}, State) ->
    handle_ring_recipients(Recipients, State);
handle_call_request({stop_ringing, Recipients}, State) ->
    handle_stop_ringing(Recipients, State);
handle_call_request({join, UserId, VS, SId, SPid}, State) ->
    handle_join_request(UserId, VS, SId, SPid, undefined, State);
handle_call_request({join, UserId, VS, SId, SPid, CId}, State) ->
    handle_join_request(UserId, VS, SId, SPid, CId, State);
handle_call_request({confirm_connection, ConnectionId}, State) ->
    handle_confirm_connection(ConnectionId, State);
handle_call_request({disconnect_user_if_in_channel, UserId, ExpChId, CId}, State) ->
    call_voice:handle_disconnect_user(UserId, ExpChId, CId, State);
handle_call_request({leave, SessionId}, State) ->
    call_voice:handle_leave(SessionId, State);
handle_call_request({update_voice_state, UserId, VoiceState}, State) ->
    handle_update_voice_state(UserId, VoiceState, State);
handle_call_request({get_sessions}, State) ->
    handle_get_sessions(State);
handle_call_request({get_pending_connections}, State) ->
    handle_get_pending_connections(State).

-spec handle_join_request(
    integer(), map(), binary(), pid(), binary() | undefined, map()
) -> {reply, ok, map()}.
handle_join_request(UserId, VS, SId, SPid, CId, #{sessions := Sessions} = State) ->
    case maps:get(SId, Sessions, undefined) of
        {UserId, SPid, _Ref} ->
            {reply, ok, State};
        _ ->
            call_voice:handle_join_internal(UserId, VS, SId, SPid, CId, State)
    end.

-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast(Request, State) ->
    case decode_cast_request(Request) of
        {ok, {join_async, UserId, VoiceState, SessionId, SessionPid}} ->
            call_voice:handle_join_async(UserId, VoiceState, SessionId, SessionPid, State);
        error ->
            {noreply, State}
    end.

-spec handle_info(term(), map()) -> {noreply, map()} | {stop, normal, map()}.
handle_info(Info, State) ->
    case decode_info_message(Info) of
        {ok, InfoMessage} -> handle_info_message(InfoMessage, State);
        error -> {noreply, State}
    end.

-spec handle_info_message(info_message(), map()) -> {noreply, map()} | {stop, normal, map()}.
handle_info_message({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    call_voice:handle_session_down(Pid, State);
handle_info_message({ring_timeout, UserId}, State) ->
    call_ringing:handle_ring_timeout(UserId, State);
handle_info_message({pending_connection_timeout, ConnectionId}, State) ->
    handle_pending_timeout(ConnectionId, State);
handle_info_message(idle_timeout, State) ->
    call_ringing:handle_idle_timeout(State).

-spec terminate(term(), map()) -> ok.
terminate({shutdown, handoff}, _State) ->
    ok;
terminate(handoff, _State) ->
    ok;
terminate(normal, State) ->
    _ = call_state:cleanup_voice_state_counts(maps:get(voice_states, State, #{})),
    ok;
terminate(_Reason, State) ->
    _ = safe_dispatch_call_delete(State),
    _ = call_state:cleanup_voice_state_counts(maps:get(voice_states, State, #{})),
    ok.

-spec safe_dispatch_call_delete(map()) -> ok.
safe_dispatch_call_delete(State) ->
    try
        call_ringing:dispatch_call_delete(State)
    catch
        error:_ -> ok;
        exit:_ -> ok;
        throw:_ -> ok
    end.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec normalize_start_link(gen_server:start_ret()) -> start_result().
normalize_start_link({ok, Pid}) ->
    {ok, Pid};
normalize_start_link({error, Reason}) ->
    {error, Reason};
normalize_start_link(ignore) ->
    {error, ignored}.

-spec decode_call_request(term()) -> {ok, call_request()} | error.
decode_call_request({get_state}) ->
    {ok, {get_state}};
decode_call_request(export_handoff_state) ->
    {ok, export_handoff_state};
decode_call_request({update_region, undefined}) ->
    {ok, {update_region, undefined}};
decode_call_request({update_region, NewRegion}) when is_binary(NewRegion) ->
    {ok, {update_region, NewRegion}};
decode_call_request({ring_recipients, Recipients}) ->
    case decode_integer_list(Recipients) of
        {ok, DecodedRecipients} -> {ok, {ring_recipients, DecodedRecipients}};
        error -> error
    end;
decode_call_request({stop_ringing, Recipients}) ->
    case decode_integer_list(Recipients) of
        {ok, DecodedRecipients} -> {ok, {stop_ringing, DecodedRecipients}};
        error -> error
    end;
decode_call_request(Request) ->
    decode_session_call_request(Request).

-spec decode_session_call_request(term()) -> {ok, call_request()} | error.
decode_session_call_request({join, UserId, VoiceState, SessionId, SessionPid}) when
    is_integer(UserId), is_map(VoiceState), is_binary(SessionId), is_pid(SessionPid)
->
    {ok, {join, UserId, VoiceState, SessionId, SessionPid}};
decode_session_call_request({join, UserId, VoiceState, SessionId, SessionPid, undefined}) when
    is_integer(UserId),
    is_map(VoiceState),
    is_binary(SessionId),
    is_pid(SessionPid)
->
    {ok, {join, UserId, VoiceState, SessionId, SessionPid, undefined}};
decode_session_call_request(
    {join, UserId, VoiceState, SessionId, SessionPid, ConnectionId}
) when
    is_integer(UserId),
    is_map(VoiceState),
    is_binary(SessionId),
    is_pid(SessionPid),
    is_binary(ConnectionId)
->
    {ok, {join, UserId, VoiceState, SessionId, SessionPid, ConnectionId}};
decode_session_call_request({confirm_connection, ConnectionId}) when is_binary(ConnectionId) ->
    {ok, {confirm_connection, ConnectionId}};
decode_session_call_request(
    {disconnect_user_if_in_channel, UserId, ExpectedChannelId, ConnectionId}
) ->
    decode_disconnect_user_request(UserId, ExpectedChannelId, ConnectionId);
decode_session_call_request({leave, SessionId}) when is_binary(SessionId) ->
    {ok, {leave, SessionId}};
decode_session_call_request({update_voice_state, UserId, VoiceState}) when
    is_integer(UserId), is_map(VoiceState)
->
    {ok, {update_voice_state, UserId, VoiceState}};
decode_session_call_request({get_sessions}) ->
    {ok, {get_sessions}};
decode_session_call_request({get_pending_connections}) ->
    {ok, {get_pending_connections}};
decode_session_call_request(_) ->
    error.

-spec decode_disconnect_user_request(term(), term(), term()) -> {ok, call_request()} | error.
decode_disconnect_user_request(UserId, ExpectedChannelId, undefined) when
    is_integer(UserId), is_integer(ExpectedChannelId)
->
    {ok, {disconnect_user_if_in_channel, UserId, ExpectedChannelId, undefined}};
decode_disconnect_user_request(UserId, ExpectedChannelId, ConnectionId) when
    is_integer(UserId),
    is_integer(ExpectedChannelId),
    is_binary(ConnectionId)
->
    {ok, {disconnect_user_if_in_channel, UserId, ExpectedChannelId, ConnectionId}};
decode_disconnect_user_request(_UserId, _ExpectedChannelId, _ConnectionId) ->
    error.

-spec decode_integer_list(term()) -> {ok, [integer()]} | error.
decode_integer_list(Values) when is_list(Values) ->
    decode_integer_list(Values, []);
decode_integer_list(_Values) ->
    error.

-spec decode_integer_list([term()], [integer()]) -> {ok, [integer()]} | error.
decode_integer_list([], Acc) ->
    {ok, lists:reverse(Acc)};
decode_integer_list([Value | Rest], Acc) when is_integer(Value) ->
    decode_integer_list(Rest, [Value | Acc]);
decode_integer_list([_Value | _Rest], _Acc) ->
    error.

-spec decode_cast_request(term()) -> {ok, cast_request()} | error.
decode_cast_request({join_async, UserId, VoiceState, SessionId, SessionPid}) when
    is_integer(UserId), is_map(VoiceState), is_binary(SessionId), is_pid(SessionPid)
->
    {ok, {join_async, UserId, VoiceState, SessionId, SessionPid}};
decode_cast_request(_) ->
    error.

-spec decode_info_message(term()) -> {ok, info_message()} | error.
decode_info_message({'DOWN', Ref, process, Pid, Reason}) when is_reference(Ref), is_pid(Pid) ->
    {ok, {'DOWN', Ref, process, Pid, Reason}};
decode_info_message({ring_timeout, UserId}) when is_integer(UserId) ->
    {ok, {ring_timeout, UserId}};
decode_info_message({pending_connection_timeout, ConnectionId}) when is_binary(ConnectionId) ->
    {ok, {pending_connection_timeout, ConnectionId}};
decode_info_message(idle_timeout) ->
    {ok, idle_timeout};
decode_info_message(_) ->
    error.

-spec build_get_state_reply(map()) -> map().
build_get_state_reply(State) ->
    #{
        channel_id => integer_to_binary(maps:get(channel_id, State)),
        message_id => integer_to_binary(maps:get(message_id, State)),
        region => maps:get(region, State),
        ringing => call_state:integer_list_to_binaries(maps:get(ringing, State)),
        recipients => call_state:integer_list_to_binaries(maps:get(recipients, State)),
        voice_states => call_state:format_voice_states(maps:get(voice_states, State)),
        created_at => maps:get(created_at, State)
    }.

-spec handle_update_region(binary() | undefined, map()) -> {reply, ok, map()}.
handle_update_region(NewRegion, State) ->
    OldRegion = maps:get(region, State, undefined),
    NewState = State#{region => NewRegion},
    UpdatedState = call_ringing:dispatch_call_update(NewState),
    call_voice:maybe_spawn_region_switch(OldRegion, NewRegion, UpdatedState),
    {reply, ok, UpdatedState}.

-spec handle_ring_recipients([integer()], map()) -> {reply, ok, map()}.
handle_ring_recipients(Recipients, State) ->
    CurrentVoiceUsers = maps:keys(maps:get(voice_states, State)),
    PendingAdditions = [U || U <- Recipients, not lists:member(U, CurrentVoiceUsers)],
    NewPending = lists:usort(maps:get(pending_ringing, State) ++ PendingAdditions),
    StateWithPending = State#{pending_ringing => NewPending},
    {UpdatedState, _} = call_ringing:maybe_dispatch_pending_ringing(StateWithPending),
    {reply, ok, UpdatedState}.

-spec handle_stop_ringing([integer()], map()) -> {reply, ok, map()}.
handle_stop_ringing(Recipients, State) ->
    CancelledState = call_ringing:cancel_ringing_timers(Recipients, State),
    NewRinging = maps:get(ringing, CancelledState) -- Recipients,
    NewPending = maps:get(pending_ringing, CancelledState) -- Recipients,
    StateWithoutRecipients = CancelledState#{
        ringing => NewRinging, pending_ringing => NewPending
    },
    {UpdatedState, _} = call_ringing:maybe_dispatch_state_update(
        CancelledState, StateWithoutRecipients
    ),
    {reply, ok, UpdatedState}.

-spec handle_confirm_connection(binary(), map()) -> {reply, map(), map()}.
handle_confirm_connection(ConnectionId, State) ->
    ReadyState = call_ringing:ensure_initiator_ready(State),
    Pending = maps:get(pending_connections, ReadyState),
    case voice_pending_common:confirm_pending_connection(ConnectionId, Pending) of
        {not_found, _} ->
            {DispState, _} = call_ringing:maybe_dispatch_pending_ringing(ReadyState),
            {reply, #{success => true, already_confirmed => true}, DispState};
        {confirmed, NewPending} ->
            St = ReadyState#{pending_connections => NewPending},
            {DispState, _} = call_ringing:maybe_dispatch_pending_ringing(St),
            {reply, #{success => true}, DispState}
    end.

-spec handle_update_voice_state(integer(), map(), map()) -> {reply, term(), map()}.
handle_update_voice_state(UserId, VoiceState, State) ->
    case maps:is_key(UserId, maps:get(voice_states, State)) of
        true ->
            NewVS = (maps:get(voice_states, State))#{UserId => VoiceState},
            NewState = State#{voice_states => NewVS},
            CountedState = call_state:sync_voice_state_count_diff(State, NewState),
            UpdatedState = call_ringing:dispatch_call_update(CountedState),
            {reply, ok, UpdatedState};
        false ->
            {reply, {error, not_in_call}, State}
    end.

-spec handle_get_sessions(map()) -> {reply, map(), map()}.
handle_get_sessions(State) ->
    StateMap = #{
        sessions => maps:get(sessions, State),
        voice_states => maps:get(voice_states, State)
    },
    {reply, StateMap, State}.

-spec handle_get_pending_connections(map()) -> {reply, map(), map()}.
handle_get_pending_connections(State) ->
    PendingJoins = call_state:format_pending_connections(
        maps:get(pending_connections, State)
    ),
    {reply, #{pending_joins => PendingJoins}, State}.

-spec handle_pending_timeout(binary(), map()) -> {noreply, map()}.
handle_pending_timeout(ConnectionId, State) ->
    Pending = maps:get(pending_connections, State),
    case voice_pending_common:get_pending_connection(ConnectionId, Pending) of
        undefined ->
            {noreply, State};
        #{user_id := UserId, session_id := SessionId} ->
            check_pending_session(ConnectionId, UserId, SessionId, State)
    end.

-spec check_pending_session(binary(), integer(), binary(), map()) -> {noreply, map()}.
check_pending_session(ConnectionId, UserId, SessionId, State) ->
    case maps:get(SessionId, maps:get(sessions, State), undefined) of
        {UserId, SessionPid, _Ref} when is_pid(SessionPid) ->
            check_pending_session_alive(ConnectionId, UserId, SessionId, SessionPid, State);
        _ ->
            call_voice:disconnect_user_after_pending_timeout(
                ConnectionId, UserId, SessionId, State
            )
    end.

-spec check_pending_session_alive(binary(), integer(), binary(), pid(), map()) ->
    {noreply, map()}.
check_pending_session_alive(ConnectionId, UserId, SessionId, SessionPid, State) ->
    case call_voice:is_session_pid_alive(SessionPid) of
        true ->
            NewPending = voice_pending_common:remove_pending_connection(
                ConnectionId, maps:get(pending_connections, State)
            ),
            {noreply, State#{pending_connections => NewPending}};
        false ->
            call_voice:disconnect_user_after_pending_timeout(
                ConnectionId, UserId, SessionId, State
            )
    end.

