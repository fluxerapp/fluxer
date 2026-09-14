%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(call_voice).
-typing([eqwalizer]).

-export([
    handle_join_internal/6,
    handle_join_async/5,
    handle_session_down/2,
    handle_disconnect_user/4,
    handle_leave/2,
    disconnect_user_after_pending_timeout/4,
    maybe_notify_session_force_disconnect/4,
    maybe_spawn_region_switch/3,
    is_session_pid_alive/1
]).

-define(PENDING_CONNECTION_TIMEOUT_MS, 30000).

-spec handle_join_internal(
    integer(), map(), binary(), pid(), binary() | undefined, map()
) -> {reply, ok, map()}.
handle_join_internal(UserId, VoiceState, SessionId, SessionPid, ConnectionId, State) ->
    CleanState = call_ringing:cancel_ringing_timers([UserId], State),
    BaseState = call_ringing:remove_users_from_ringing([UserId], CleanState),
    #{
        voice_states := VoiceStates0,
        sessions := Sessions0,
        participants_history := History0
    } = BaseState,
    NewVoiceStates = VoiceStates0#{UserId => VoiceState},
    NewSessions = monitor_session_entry(
        Sessions0, SessionId, UserId, SessionPid
    ),
    NewHistory = sets:add_element(UserId, History0),
    NewPending = build_pending(ConnectionId, UserId, SessionId, BaseState),
    NewState = BaseState#{
        voice_states => NewVoiceStates,
        sessions => NewSessions,
        pending_connections => NewPending,
        participants_history => NewHistory
    },
    StateWithTimer = call_ringing:reset_idle_timer(NewState),
    {UpdatedState, Dispatched} = call_ringing:maybe_dispatch_state_update(
        BaseState, StateWithTimer
    ),
    {reply, ok, ensure_call_update(UpdatedState, Dispatched)}.

-spec build_pending(binary() | undefined, integer(), binary(), map()) -> map().
build_pending(undefined, _UserId, _SessionId, #{pending_connections := Pending}) ->
    Pending;
build_pending(
    ConnectionId,
    UserId,
    SessionId,
    #{channel_id := ChannelId, pending_connections := Pending}
) ->
    PendingMetadata = #{
        user_id => UserId,
        channel_id => ChannelId,
        connection_id => ConnectionId,
        session_id => SessionId,
        joined_at => erlang:system_time(millisecond)
    },
    erlang:send_after(
        ?PENDING_CONNECTION_TIMEOUT_MS,
        self(),
        {pending_connection_timeout, ConnectionId}
    ),
    voice_pending_common:add_pending_connection(
        ConnectionId, PendingMetadata, Pending
    ).

-spec monitor_session_entry(map(), binary(), integer(), pid()) -> map().
monitor_session_entry(Sessions, SessionId, UserId, SessionPid) ->
    case maps:get(SessionId, Sessions, undefined) of
        {_OldUserId, _OldPid, OldRef} ->
            demonitor(OldRef, [flush]);
        _ ->
            ok
    end,
    SessionRef = monitor(process, SessionPid),
    Sessions#{SessionId => {UserId, SessionPid, SessionRef}}.

-spec handle_join_async(integer(), map(), binary(), pid(), map()) -> {noreply, map()}.
handle_join_async(
    UserId, VoiceState, SessionId, SessionPid, #{channel_id := ChannelId} = State
) ->
    CleanState = call_ringing:cancel_ringing_timers([UserId], State),
    BaseState = call_ringing:remove_users_from_ringing([UserId], CleanState),
    #{
        voice_states := VoiceStates0,
        sessions := Sessions0,
        participants_history := History0
    } = BaseState,
    NewVS = VoiceStates0#{UserId => VoiceState},
    NewSess = monitor_session_entry(Sessions0, SessionId, UserId, SessionPid),
    NewHistory = sets:add_element(UserId, History0),
    NewState = BaseState#{
        voice_states => NewVS, sessions => NewSess, participants_history => NewHistory
    },
    StateWithTimer = call_ringing:reset_idle_timer(NewState),
    {UpdState, Dispatched} = call_ringing:maybe_dispatch_state_update(
        BaseState, StateWithTimer
    ),
    FinalState = ensure_call_update(UpdState, Dispatched),
    SessionPid ! {call_join_result, ChannelId, {ok, FinalState}},
    {noreply, FinalState}.

-spec maybe_remove_user_voice_state(integer(), map(), map()) -> map().
maybe_remove_user_voice_state(UserId, RemainingSessions, VoiceStates) ->
    case call_state:user_has_session(UserId, RemainingSessions) of
        true -> VoiceStates;
        false -> maps:remove(UserId, VoiceStates)
    end.

-spec ensure_call_update(map(), boolean()) -> map().
ensure_call_update(State, true) ->
    State;
ensure_call_update(State, false) ->
    call_ringing:dispatch_call_update(State).

-spec handle_session_down(pid(), map()) -> {noreply, map()} | {stop, normal, map()}.
handle_session_down(Pid, #{sessions := Sessions, voice_states := VoiceStates} = State) ->
    case call_state:find_session_by_pid(Pid, Sessions) of
        {ok, SessionId, UserId} ->
            NewSess = maps:remove(SessionId, Sessions),
            NewVS = maybe_remove_user_voice_state(UserId, NewSess, VoiceStates),
            BaseState = State#{voice_states => NewVS, sessions => NewSess},
            CleanState = call_ringing:cancel_ringing_timers([UserId], BaseState),
            RingState = call_ringing:remove_users_from_ringing([UserId], CleanState),
            {UpdState, Dispatched} = call_ringing:maybe_dispatch_state_update(
                State, RingState
            ),
            call_ringing:maybe_stop_or_noreply(UpdState, Dispatched);
        not_found ->
            {noreply, State}
    end.

-spec handle_disconnect_user(integer(), integer(), binary() | undefined, map()) ->
    {reply, term(), map()} | {stop, normal, term(), map()}.
handle_disconnect_user(
    UserId,
    ExpectedChannelId,
    ConnectionId,
    #{
        voice_states := VoiceStates,
        sessions := Sessions,
        pending_connections := PendingConns
    } = State
) ->
    CleanupFun = fun(DUId, DSId) ->
        maybe_notify_session_force_disconnect(DUId, DSId, ConnectionId, State)
    end,
    case
        voice_disconnect_common:disconnect_user_if_in_channel(
            UserId, ExpectedChannelId, ConnectionId, VoiceStates, Sessions, CleanupFun
        )
    of
        {not_found, _, _} ->
            NewPending = voice_pending_common:remove_pending_connection(
                ConnectionId, PendingConns
            ),
            Reply = #{success => true, ignored => true, reason => <<"not_in_call">>},
            {reply, Reply, State#{pending_connections => NewPending}};
        {channel_mismatch, _, _} ->
            Reply = #{success => true, ignored => true, reason => <<"channel_mismatch">>},
            {reply, Reply, State};
        {ok, NewVS, NewSess} ->
            do_disconnect_cleanup(UserId, ConnectionId, NewVS, NewSess, State)
    end.

-spec do_disconnect_cleanup(integer(), binary() | undefined, map(), map(), map()) ->
    {reply, term(), map()} | {stop, normal, term(), map()}.
do_disconnect_cleanup(
    UserId,
    ConnectionId,
    NewVS,
    NewSess,
    #{pending_connections := PendingConns} = State
) ->
    NewPending = voice_pending_common:remove_pending_connection(
        ConnectionId, PendingConns
    ),
    BaseState = State#{
        voice_states => NewVS, sessions => NewSess, pending_connections => NewPending
    },
    CleanState = call_ringing:cancel_ringing_timers([UserId], BaseState),
    RingState = call_ringing:remove_users_from_ringing([UserId], CleanState),
    {UpdState, Dispatched} = call_ringing:maybe_dispatch_state_update(State, RingState),
    call_ringing:maybe_stop_or_map_reply(UpdState, Dispatched, #{success => true}).

-spec handle_leave(binary(), map()) ->
    {reply, term(), map()} | {stop, normal, term(), map()}.
handle_leave(SessionId, #{sessions := Sessions, voice_states := VoiceStates} = State) ->
    case maps:get(SessionId, Sessions, undefined) of
        {UserId, _Pid, Ref} ->
            demonitor(Ref, [flush]),
            NewSess = maps:remove(SessionId, Sessions),
            NewVS = maybe_remove_user_voice_state(UserId, NewSess, VoiceStates),
            BaseState = State#{voice_states => NewVS, sessions => NewSess},
            CleanState = call_ringing:cancel_ringing_timers([UserId], BaseState),
            RingState = call_ringing:remove_users_from_ringing([UserId], CleanState),
            {UpdState, Dispatched} = call_ringing:maybe_dispatch_state_update(
                State, RingState
            ),
            call_ringing:maybe_stop_or_reply(UpdState, Dispatched, ok);
        undefined ->
            {reply, {error, not_found}, State}
    end.

-spec disconnect_user_after_pending_timeout(
    binary(), integer(), binary(), map()
) -> {noreply, map()}.
disconnect_user_after_pending_timeout(
    ConnectionId,
    UserId,
    SessionId,
    #{
        pending_connections := PendingConns,
        voice_states := VoiceStates,
        sessions := Sessions
    } = State
) ->
    NewPending = voice_pending_common:remove_pending_connection(
        ConnectionId, PendingConns
    ),
    case maps:is_key(UserId, VoiceStates) of
        true ->
            {noreply, State#{pending_connections => NewPending}};
        false ->
            NewSessions = call_state:remove_session_entry(
                SessionId, Sessions
            ),
            {noreply, State#{
                pending_connections => NewPending, sessions => NewSessions
            }}
    end.

-spec maybe_notify_session_force_disconnect(
    integer(), binary(), binary() | undefined, map()
) -> ok.
maybe_notify_session_force_disconnect(
    UserId,
    SessionId,
    ConnectionId,
    #{sessions := Sessions, channel_id := ChannelId}
) ->
    case maps:get(SessionId, Sessions, undefined) of
        {UserId, SessionPid, _Ref} when is_pid(SessionPid) ->
            gen_server:cast(SessionPid, {call_force_disconnect, ChannelId, ConnectionId}),
            ok;
        _ ->
            ok
    end.

-spec maybe_spawn_region_switch(
    binary() | undefined | null, binary() | undefined | null, map()
) -> ok.
maybe_spawn_region_switch(Region, Region, _State) ->
    ok;
maybe_spawn_region_switch(
    _OldRegion,
    NewRegion,
    #{
        voice_states := VoiceStates,
        sessions := Sessions,
        channel_id := ChannelId
    }
) ->
    maybe_spawn_region_switch_for_participants(
        maps:size(VoiceStates) > 0, NewRegion, VoiceStates, Sessions, ChannelId
    ).

-spec maybe_spawn_region_switch_for_participants(
    boolean(), binary() | undefined | null, map(), map(), integer()
) -> ok.
maybe_spawn_region_switch_for_participants(
    HasParticipants, NewRegion, VoiceStates, Sessions, ChannelId
) ->
    case HasParticipants andalso is_binary(NewRegion) of
        true ->
            spawn_voice_server_updates(NewRegion, VoiceStates, Sessions, ChannelId);
        false ->
            ok
    end.

-spec spawn_voice_server_updates(binary(), map(), map(), integer()) -> ok.
spawn_voice_server_updates(NewRegion, VoiceStates, Sessions, ChannelId) ->
    spawn(fun() ->
        send_voice_server_updates(NewRegion, VoiceStates, Sessions, ChannelId)
    end),
    ok.

-spec send_voice_server_updates(
    binary() | undefined | null, map(), map(), integer()
) -> ok.
send_voice_server_updates(NewRegion, _VS, _Sessions, _ChId) when not is_binary(NewRegion) ->
    ok;
send_voice_server_updates(NewRegion, VoiceStates, Sessions, ChannelId) ->
    maps:foreach(
        fun(_SessionId, {UserId, SessionPid, _Ref}) ->
            maybe_send_voice_server_update(
                UserId, SessionPid, NewRegion, ChannelId, VoiceStates
            )
        end,
        Sessions
    ),
    ok.

-spec maybe_send_voice_server_update(integer(), pid(), binary(), integer(), map()) -> ok.
maybe_send_voice_server_update(UserId, SessionPid, NewRegion, ChannelId, VoiceStates) ->
    case maps:get(UserId, VoiceStates, undefined) of
        undefined ->
            ok;
        VoiceState ->
            send_voice_server_update(ChannelId, UserId, SessionPid, NewRegion, VoiceState)
    end.

-spec send_voice_server_update(integer(), integer(), pid(), binary(), map()) -> ok.
send_voice_server_update(ChannelId, UserId, SessionPid, NewRegion, VoiceState) ->
    ConnectionId = maps:get(<<"connection_id">>, VoiceState, null),
    case ConnectionId of
        undefined ->
            ok;
        null ->
            ok;
        _ ->
            Req0 = voice_utils:build_voice_token_rpc_request(
                null, ChannelId, UserId, ConnectionId, null, null
            ),
            Req = voice_utils:add_rtc_region_to_request(Req0, rpc_region(NewRegion)),
            dispatch_voice_server_rpc(ChannelId, SessionPid, Req)
    end.

-spec rpc_region(binary()) -> binary() | null.
rpc_region(<<"automatic">>) ->
    null;
rpc_region(NewRegion) ->
    NewRegion.

-spec dispatch_voice_server_rpc(integer(), pid(), map()) -> ok.
dispatch_voice_server_rpc(ChannelId, SessionPid, Req) ->
    case rpc_client:call(Req) of
        {ok,
            #{
                <<"token">> := Token,
                <<"endpoint">> := Endpoint,
                <<"connectionId">> := ConnId
            } = _Data} ->
            VoiceServerUpdate = #{
                <<"token">> => Token,
                <<"endpoint">> => Endpoint,
                <<"channel_id">> => integer_to_binary(ChannelId),
                <<"connection_id">> => ConnId
            },
            gateway_dispatch_relay:dispatch(
                SessionPid, voice_server_update, VoiceServerUpdate, 0
            );
        _ ->
            ok
    end.

-spec is_session_pid_alive(pid()) -> boolean().
is_session_pid_alive(Pid) ->
    process_liveness:is_alive(Pid).
