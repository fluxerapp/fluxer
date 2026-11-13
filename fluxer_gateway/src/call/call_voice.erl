%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(call_voice).
-typing([eqwalizer]).

-export([
    handle_join_internal/7,
    handle_session_down/2,
    handle_disconnect_user/4,
    handle_leave/2,
    disconnect_user_after_pending_timeout/4,
    maybe_notify_session_force_disconnect/4,
    maybe_spawn_region_switch/3,
    convert_to_sfu/2,
    update_voice_state/3,
    relay_voice_signal/4,
    is_session_pid_alive/1
]).

-define(PENDING_CONNECTION_TIMEOUT_MS, 30000).

-spec handle_join_internal(
    integer(), map(), binary(), pid(), binary() | undefined, pos_integer() | undefined, map()
) -> {reply, ok | {error, atom()}, map()}.
handle_join_internal(
    UserId, VoiceState, SessionId, SessionPid, ConnectionId, MaxParticipants, State
) ->
    case join_violation(VoiceState, MaxParticipants, State) of
        {error, ErrorAtom} ->
            {reply, {error, ErrorAtom}, State};
        ok ->
            NewState = admit_join(
                UserId,
                VoiceState,
                SessionId,
                SessionPid,
                pending_connection_id(ConnectionId, VoiceState),
                State
            ),
            {reply, ok, NewState}
    end.

-spec join_violation(map(), pos_integer() | undefined, map()) -> ok | {error, atom()}.
join_violation(VoiceState, MaxParticipants, #{voice_states := VoiceStates}) ->
    case call_state:voice_state_connection_id(VoiceState) of
        ConnectionId when is_binary(ConnectionId) ->
            participant_violation(VoiceStates#{ConnectionId => VoiceState}, MaxParticipants);
        _ ->
            {error, voice_invalid_state}
    end.

-spec participant_violation(map(), pos_integer() | undefined) -> ok | {error, atom()}.
participant_violation(VoiceStates, MaxParticipants) ->
    Connections = maps:values(VoiceStates),
    case voice_p2p:channel_mode(Connections) of
        mixed ->
            voice_p2p:invariant_violation(Connections, MaxParticipants);
        _ ->
            voice_p2p:invariant_violation(
                maps:values(call_state:voice_states_by_user(VoiceStates)), MaxParticipants
            )
    end.

-spec admit_join(integer(), map(), binary(), pid(), binary() | undefined, map()) -> map().
admit_join(UserId, VoiceState, SessionId, SessionPid, PendingConnectionId, State) ->
    CleanState = call_ringing:cancel_ringing_timers([UserId], State),
    BaseState = call_ringing:remove_users_from_ringing([UserId], CleanState),
    #{
        voice_states := VoiceStates0,
        sessions := Sessions0,
        participants_history := History0
    } = BaseState,
    NewState = BaseState#{
        voice_states => VoiceStates0#{
            call_state:voice_state_connection_id(VoiceState) => VoiceState
        },
        sessions => monitor_session_entry(Sessions0, SessionId, UserId, SessionPid),
        pending_connections => build_pending(PendingConnectionId, UserId, SessionId, BaseState),
        participants_history => sets:add_element(UserId, History0)
    },
    StateWithTimer = call_ringing:reset_idle_timer(NewState),
    {UpdatedState, Dispatched} = call_ringing:maybe_dispatch_state_update(
        BaseState, StateWithTimer
    ),
    ensure_call_update(UpdatedState, Dispatched).

-spec pending_connection_id(binary() | undefined, map()) -> binary() | undefined.
pending_connection_id(ConnectionId, VoiceState) ->
    case voice_p2p:is_p2p(VoiceState) of
        true -> undefined;
        false -> ConnectionId
    end.

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

-spec remove_session_voice_states(binary(), integer(), map(), map()) -> map().
remove_session_voice_states(SessionId, UserId, RemainingSessions, VoiceStates) ->
    case call_state:user_has_session(UserId, RemainingSessions) of
        true ->
            call_state:without_session_voice_states(SessionId, VoiceStates);
        false ->
            maps:without(
                maps:keys(call_state:user_voice_states(UserId, VoiceStates)), VoiceStates
            )
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
            NewVS = remove_session_voice_states(SessionId, UserId, NewSess, VoiceStates),
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
    #{voice_states := VoiceStates, pending_connections := PendingConns} = State
) ->
    Targets = disconnect_targets(UserId, ConnectionId, VoiceStates),
    InChannel = maps:filter(
        fun(_ConnId, VS) ->
            snowflake_id:equal(ExpectedChannelId, maps:get(<<"channel_id">>, VS, undefined))
        end,
        Targets
    ),
    case {maps:size(Targets), maps:size(InChannel)} of
        {0, _} ->
            NewPending = voice_pending_common:remove_pending_connection(
                ConnectionId, PendingConns
            ),
            Reply = #{success => true, ignored => true, reason => <<"not_in_call">>},
            {reply, Reply, State#{pending_connections => NewPending}};
        {_, 0} ->
            Reply = #{success => true, ignored => true, reason => <<"channel_mismatch">>},
            {reply, Reply, State};
        _ ->
            do_disconnect_cleanup(UserId, InChannel, State)
    end.

-spec disconnect_targets(integer(), binary() | undefined, map()) -> map().
disconnect_targets(UserId, undefined, VoiceStates) ->
    call_state:user_voice_states(UserId, VoiceStates);
disconnect_targets(UserId, ConnectionId, VoiceStates) ->
    maps:with([ConnectionId], call_state:user_voice_states(UserId, VoiceStates)).

-spec do_disconnect_cleanup(integer(), map(), map()) ->
    {reply, term(), map()} | {stop, normal, term(), map()}.
do_disconnect_cleanup(
    UserId,
    Removed,
    #{
        voice_states := VoiceStates,
        sessions := Sessions,
        pending_connections := PendingConns
    } = State
) ->
    maps:foreach(
        fun(ConnId, VS) ->
            maybe_notify_session_force_disconnect(
                UserId, maps:get(<<"session_id">>, VS, undefined), ConnId, State
            )
        end,
        Removed
    ),
    NewVS = maps:without(maps:keys(Removed), VoiceStates),
    NewPending = maps:without(maps:keys(Removed), PendingConns),
    BaseState = State#{
        voice_states => NewVS,
        sessions => remove_idle_user_sessions(UserId, NewVS, Sessions),
        pending_connections => NewPending
    },
    CleanState = call_ringing:cancel_ringing_timers([UserId], BaseState),
    RingState = call_ringing:remove_users_from_ringing([UserId], CleanState),
    {UpdState, Dispatched} = call_ringing:maybe_dispatch_state_update(State, RingState),
    call_ringing:maybe_stop_or_map_reply(UpdState, Dispatched, #{success => true}).

-spec remove_idle_user_sessions(integer(), map(), map()) -> map().
remove_idle_user_sessions(UserId, VoiceStates, Sessions) ->
    Idle = [
        SessionId
     || {SessionId, {U, _Pid, _Ref}} <- maps:to_list(Sessions),
        U =:= UserId,
        not call_state:session_has_voice_state(SessionId, VoiceStates)
    ],
    lists:foldl(fun call_state:remove_session_entry/2, Sessions, Idle).

-spec handle_leave(binary(), map()) ->
    {reply, term(), map()} | {stop, normal, term(), map()}.
handle_leave(SessionId, #{sessions := Sessions, voice_states := VoiceStates} = State) ->
    case maps:get(SessionId, Sessions, undefined) of
        {UserId, _Pid, Ref} ->
            demonitor(Ref, [flush]),
            NewSess = maps:remove(SessionId, Sessions),
            NewVS = remove_session_voice_states(SessionId, UserId, NewSess, VoiceStates),
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
    _UserId,
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
    case
        maps:is_key(ConnectionId, VoiceStates) orelse
            call_state:session_has_voice_state(SessionId, VoiceStates)
    of
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
    integer(), binary() | undefined, binary() | undefined, map()
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

-spec spawn_voice_server_updates(binary() | undefined | null, map(), map(), integer()) -> ok.
spawn_voice_server_updates(NewRegion, VoiceStates, Sessions, ChannelId) ->
    spawn(fun() ->
        send_voice_server_updates(NewRegion, VoiceStates, Sessions, ChannelId)
    end),
    ok.

-spec update_voice_state(binary(), map(), map()) -> map().
update_voice_state(ConnectionId, VoiceState, #{voice_states := VoiceStates} = State) ->
    WasP2p = voice_p2p:is_p2p(maps:get(ConnectionId, VoiceStates)),
    case {WasP2p, voice_p2p:is_p2p(VoiceState)} of
        {true, false} ->
            convert_to_sfu(
                [ConnectionId], State#{voice_states => VoiceStates#{ConnectionId => VoiceState}}
            );
        {false, true} ->
            Converted = voice_p2p:to_sfu(VoiceState),
            NewState = State#{voice_states => VoiceStates#{ConnectionId => Converted}},
            ok = call_ringing:dispatch_voice_state_update(Converted, NewState),
            NewState;
        _ ->
            State#{voice_states => VoiceStates#{ConnectionId => VoiceState}}
    end.

-spec convert_to_sfu([binary()], map()) -> map().
convert_to_sfu(
    ExtraConnectionIds,
    #{voice_states := VoiceStates, sessions := Sessions, channel_id := ChannelId} = State
) ->
    Converted = maps:map(
        fun(_ConnId, VoiceState) -> voice_p2p:to_sfu(VoiceState) end,
        maps:filter(fun(_ConnId, VoiceState) -> voice_p2p:is_p2p(VoiceState) end, VoiceStates)
    ),
    NewVoiceStates = maps:merge(VoiceStates, Converted),
    Reconnecting = maps:with(ExtraConnectionIds ++ maps:keys(Converted), NewVoiceStates),
    case maps:size(Reconnecting) of
        0 ->
            State;
        _ ->
            NewState = State#{voice_states => NewVoiceStates},
            ok = dispatch_voice_state_updates(Converted, NewState),
            spawn_voice_server_updates(
                maps:get(region, State, undefined), Reconnecting, Sessions, ChannelId
            ),
            NewState
    end.

-spec dispatch_voice_state_updates(map(), map()) -> ok.
dispatch_voice_state_updates(VoiceStates, State) ->
    maps:foreach(
        fun(_ConnId, VoiceState) ->
            call_ringing:dispatch_voice_state_update(VoiceState, State)
        end,
        VoiceStates
    ).

-spec relay_voice_signal(binary(), binary(), map(), map()) -> ok.
relay_voice_signal(
    SessionId,
    To,
    Data,
    #{voice_states := VoiceStates, sessions := Sessions, channel_id := ChannelId}
) ->
    case voice_p2p:signal_route(maps:values(VoiceStates), SessionId, To) of
        {ok, Sender, Target} ->
            dispatch_voice_signal(
                maps:get(maps:get(<<"session_id">>, Target, undefined), Sessions, undefined),
                voice_p2p:signal_payload(null, ChannelId, Sender, Data)
            );
        error ->
            ok
    end.

-spec dispatch_voice_signal(term(), map()) -> ok.
dispatch_voice_signal({_UserId, SessionPid, _Ref}, Payload) when is_pid(SessionPid) ->
    gateway_dispatch_relay:dispatch(SessionPid, voice_signal, Payload, 0);
dispatch_voice_signal(_Session, _Payload) ->
    ok.

-spec send_voice_server_updates(
    binary() | undefined | null, map(), map(), integer()
) -> ok.
send_voice_server_updates(NewRegion, VoiceStates, Sessions, ChannelId) ->
    maps:foreach(
        fun(_ConnId, VoiceState) ->
            maybe_send_voice_server_update(VoiceState, NewRegion, ChannelId, Sessions)
        end,
        VoiceStates
    ),
    ok.

-spec maybe_send_voice_server_update(map(), binary() | undefined | null, integer(), map()) ->
    ok.
maybe_send_voice_server_update(VoiceState, NewRegion, ChannelId, Sessions) ->
    UserId = voice_state_utils:voice_state_user_id(VoiceState),
    case maps:get(maps:get(<<"session_id">>, VoiceState, undefined), Sessions, undefined) of
        {UserId, SessionPid, _Ref} when is_pid(SessionPid) ->
            send_voice_server_update(ChannelId, UserId, SessionPid, NewRegion, VoiceState);
        _ ->
            ok
    end.

-spec send_voice_server_update(
    integer(), integer(), pid(), binary() | undefined | null, map()
) -> ok.
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
            Req = voice_utils:add_rtc_region_to_request(
                Req0, dm_voice_state:voice_region_for_rpc(NewRegion)
            ),
            dispatch_voice_server_rpc(ChannelId, SessionPid, Req)
    end.

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
