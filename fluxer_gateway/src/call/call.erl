%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(call).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type state() :: #{
    channel_id := integer(),
    message_id := integer(),
    region := binary() | undefined,
    ringing := [integer()],
    pending_ringing := [integer()],
    recipients := [integer()],
    voice_states := #{integer() => map()},
    sessions := #{binary() => session_entry()},
    pending_connections := #{binary() => map()},
    initiator_ready := boolean(),
    ringing_timers := #{integer() => reference()},
    idle_timer := reference() | undefined,
    created_at := integer(),
    participants_history := sets:set(integer()),
    last_call_event := map() | undefined
}.
-type session_entry() :: {integer(), pid(), reference()}.
-type call_data() :: #{
    channel_id := integer(),
    message_id := integer(),
    region := binary() | undefined,
    ringing := [integer()],
    recipients := [integer()]
}.

-define(RING_TIMEOUT_MS, 30000).
-define(IDLE_TIMEOUT_MS, 120000).
-define(PENDING_CONNECTION_TIMEOUT_MS, 30000).

-spec start_link(call_data()) -> {ok, pid()} | {error, term()} | ignore.
start_link(CallData) ->
    gen_server:start_link(?MODULE, CallData, []).

-spec init(call_data()) -> {ok, state()}.
init(CallData) ->
    #{
        channel_id := ChannelId,
        message_id := MessageId,
        region := Region,
        ringing := Ringing,
        recipients := Recipients
    } = CallData,
    State = #{
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
    },
    ReadyState = ensure_initiator_ready(State),
    {StateWithRinging, Dispatched} = maybe_dispatch_pending_ringing(ReadyState, false),
    StateWithIdleTimer = reset_idle_timer(StateWithRinging),
    StateWithCreate = dispatch_call_create(StateWithIdleTimer),
    FinalState =
        case Dispatched of
            false ->
                case maps:get(ringing, StateWithCreate) of
                    [] -> StateWithCreate;
                    _ -> dispatch_call_update(StateWithCreate)
                end;
            true ->
                StateWithCreate
        end,
    {ok, FinalState}.

-spec handle_call(term(), gen_server:from(), state()) ->
    {reply, term(), state()} | {stop, normal, term(), state()}.
handle_call({get_state}, _From, State) ->
    CallData = #{
        channel_id => integer_to_binary(maps:get(channel_id, State)),
        message_id => integer_to_binary(maps:get(message_id, State)),
        region => maps:get(region, State),
        ringing => integer_list_to_binaries(maps:get(ringing, State)),
        recipients => integer_list_to_binaries(maps:get(recipients, State)),
        voice_states => format_voice_states(maps:get(voice_states, State)),
        created_at => maps:get(created_at, State)
    },
    {reply, {ok, CallData}, State};
handle_call({update_region, NewRegion}, _From, State) ->
    OldRegion = maps:get(region, State, undefined),
    NewState = State#{region => NewRegion},
    UpdatedState = dispatch_call_update(NewState),
    maybe_spawn_region_switch(OldRegion, NewRegion, UpdatedState),
    {reply, ok, UpdatedState};
handle_call({ring_recipients, Recipients}, _From, State) ->
    CurrentVoiceUsers = maps:keys(maps:get(voice_states, State)),
    PendingAdditions = [U || U <- Recipients, not lists:member(U, CurrentVoiceUsers)],
    NewPending = lists:usort(maps:get(pending_ringing, State) ++ PendingAdditions),
    StateWithPending = State#{pending_ringing => NewPending},
    {UpdatedState, _} = maybe_dispatch_pending_ringing(StateWithPending),
    {reply, ok, UpdatedState};
handle_call({stop_ringing, Recipients}, _From, State) ->
    CancelledState = cancel_ringing_timers(Recipients, State),
    NewRinging = maps:get(ringing, CancelledState) -- Recipients,
    NewPending = maps:get(pending_ringing, CancelledState) -- Recipients,
    StateWithoutRecipients = CancelledState#{
        ringing => NewRinging, pending_ringing => NewPending
    },
    {UpdatedState, _} = maybe_dispatch_state_update(CancelledState, StateWithoutRecipients),
    {reply, ok, UpdatedState};
handle_call({join, UserId, VoiceState, SessionId, SessionPid}, _From, State) ->
    handle_join_internal(UserId, VoiceState, SessionId, SessionPid, undefined, State);
handle_call({join, UserId, VoiceState, SessionId, SessionPid, ConnectionId}, _From, State) ->
    handle_join_internal(UserId, VoiceState, SessionId, SessionPid, ConnectionId, State);
handle_call({confirm_connection, ConnectionId}, _From, State) ->
    ReadyState = ensure_initiator_ready(State),
    case
        voice_pending_common:confirm_pending_connection(
            ConnectionId, maps:get(pending_connections, ReadyState)
        )
    of
        {not_found, _} ->
            {DispatchedState, _} = maybe_dispatch_pending_ringing(ReadyState),
            {reply, #{success => true, already_confirmed => true}, DispatchedState};
        {confirmed, NewPending} ->
            StateWithPending = ReadyState#{pending_connections => NewPending},
            {DispatchedState, _} = maybe_dispatch_pending_ringing(StateWithPending),
            {reply, #{success => true}, DispatchedState}
    end;
handle_call({disconnect_user_if_in_channel, UserId, ExpectedChannelId, ConnectionId}, _From, State) ->
    CleanupFun =
        fun(DisconnectedUserId, DisconnectedSessionId) ->
            maybe_notify_session_force_disconnect(
                DisconnectedUserId, DisconnectedSessionId, ConnectionId, State
            )
        end,
    case
        voice_disconnect_common:disconnect_user_if_in_channel(
            UserId,
            ExpectedChannelId,
            maps:get(voice_states, State),
            maps:get(sessions, State),
            CleanupFun
        )
    of
        {not_found, _, _} ->
            NewPending = voice_pending_common:remove_pending_connection(
                ConnectionId, maps:get(pending_connections, State)
            ),
            {reply, #{success => true, ignored => true, reason => <<"not_in_call">>}, State#{
                pending_connections => NewPending
            }};
        {channel_mismatch, _, _} ->
            {reply, #{success => true, ignored => true, reason => <<"channel_mismatch">>}, State};
        {ok, NewVoiceStates, NewSessions} ->
            NewPending = voice_pending_common:remove_pending_connection(
                ConnectionId, maps:get(pending_connections, State)
            ),
            BaseState = State#{
                voice_states => NewVoiceStates,
                sessions => NewSessions,
                pending_connections => NewPending
            },
            CancelledTimersState = cancel_ringing_timers([UserId], BaseState),
            RingCleanupState = remove_users_from_ringing([UserId], CancelledTimersState),
            {UpdatedState, Dispatched} = maybe_dispatch_state_update(BaseState, RingCleanupState),
            case maps:size(maps:get(voice_states, UpdatedState)) of
                0 ->
                    dispatch_call_delete(UpdatedState),
                    {stop, normal, #{success => true}, UpdatedState};
                _ ->
                    FinalState =
                        case Dispatched of
                            true -> UpdatedState;
                            false -> dispatch_call_update(UpdatedState)
                        end,
                    {reply, #{success => true}, FinalState}
            end
    end;
handle_call({leave, SessionId}, _From, State) ->
    case maps:get(SessionId, maps:get(sessions, State), undefined) of
        {UserId, _Pid, Ref} ->
            demonitor(Ref, [flush]),
            NewVoiceStates = maps:remove(UserId, maps:get(voice_states, State)),
            NewSessions = maps:remove(SessionId, maps:get(sessions, State)),
            BaseState = State#{
                voice_states => NewVoiceStates,
                sessions => NewSessions
            },
            CancelledTimersState = cancel_ringing_timers([UserId], BaseState),
            RingCleanupState = remove_users_from_ringing([UserId], CancelledTimersState),
            {UpdatedState, Dispatched} = maybe_dispatch_state_update(BaseState, RingCleanupState),
            case maps:size(maps:get(voice_states, UpdatedState)) of
                0 ->
                    dispatch_call_delete(UpdatedState),
                    {stop, normal, ok, UpdatedState};
                _ ->
                    FinalState =
                        case Dispatched of
                            true -> UpdatedState;
                            false -> dispatch_call_update(UpdatedState)
                        end,
                    {reply, ok, FinalState}
            end;
        undefined ->
            {reply, {error, not_found}, State}
    end;
handle_call({update_voice_state, UserId, VoiceState}, _From, State) ->
    case maps:is_key(UserId, maps:get(voice_states, State)) of
        true ->
            NewVoiceStates = maps:put(UserId, VoiceState, maps:get(voice_states, State)),
            NewState = State#{voice_states => NewVoiceStates},
            UpdatedState = dispatch_call_update(NewState),
            {reply, ok, UpdatedState};
        false ->
            {reply, {error, not_in_call}, State}
    end;
handle_call({get_sessions}, _From, State) ->
    StateMap = #{
        sessions => maps:get(sessions, State),
        voice_states => maps:get(voice_states, State)
    },
    {reply, StateMap, State};
handle_call({get_pending_connections}, _From, State) ->
    PendingJoins = format_pending_connections(maps:get(pending_connections, State)),
    {reply, #{pending_joins => PendingJoins}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({join_async, UserId, VoiceState, SessionId, SessionPid}, State) ->
    CleanState = cancel_ringing_timers([UserId], State),
    BaseState = remove_users_from_ringing([UserId], CleanState),
    NewVoiceStates = maps:put(UserId, VoiceState, maps:get(voice_states, BaseState)),
    SessionRef = monitor(process, SessionPid),
    NewSessions = maps:put(SessionId, {UserId, SessionPid, SessionRef}, maps:get(sessions, BaseState)),
    NewParticipantsHistory = sets:add_element(UserId, maps:get(participants_history, BaseState)),
    NewPending = maps:get(pending_connections, BaseState),
    NewState = BaseState#{
        voice_states => NewVoiceStates,
        sessions => NewSessions,
        pending_connections => NewPending,
        participants_history => NewParticipantsHistory
    },
    StateWithTimer = reset_idle_timer(NewState),
    {UpdatedState, Dispatched} = maybe_dispatch_state_update(BaseState, StateWithTimer),
    FinalState =
        case Dispatched of
            true -> UpdatedState;
            false -> dispatch_call_update(UpdatedState)
        end,
    SessionPid ! {call_join_result, maps:get(channel_id, State), {ok, FinalState}},
    {noreply, FinalState};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, normal, state()}.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case find_session_by_pid(Pid, maps:get(sessions, State)) of
        {ok, SessionId, UserId} ->
            NewVoiceStates = maps:remove(UserId, maps:get(voice_states, State)),
            NewSessions = maps:remove(SessionId, maps:get(sessions, State)),
            BaseState = State#{
                voice_states => NewVoiceStates,
                sessions => NewSessions
            },
            CancelledTimersState = cancel_ringing_timers([UserId], BaseState),
            RingCleanupState = remove_users_from_ringing([UserId], CancelledTimersState),
            {UpdatedState, Dispatched} = maybe_dispatch_state_update(BaseState, RingCleanupState),
            case maps:size(maps:get(voice_states, UpdatedState)) of
                0 ->
                    dispatch_call_delete(UpdatedState),
                    {stop, normal, UpdatedState};
                _ ->
                    FinalState =
                        case Dispatched of
                            true -> UpdatedState;
                            false -> dispatch_call_update(UpdatedState)
                        end,
                    {noreply, FinalState}
            end;
        not_found ->
            {noreply, State}
    end;
handle_info({ring_timeout, UserId}, State) ->
    case maps:get(UserId, maps:get(ringing_timers, State), undefined) of
        undefined ->
            {noreply, State};
        _ ->
            CancelState = cancel_ringing_timers([UserId], State),
            RingCleanupState = remove_users_from_ringing([UserId], CancelState),
            {UpdatedState, _} = maybe_dispatch_state_update(State, RingCleanupState),
            HasParticipants = maps:size(maps:get(voice_states, UpdatedState)) > 0,
            HasPendingRinging = length(maps:get(ringing, UpdatedState)) > 0,
            case HasParticipants orelse HasPendingRinging of
                true ->
                    {noreply, UpdatedState};
                false ->
                    dispatch_call_delete(UpdatedState),
                    {stop, normal, UpdatedState}
            end
    end;
handle_info({pending_connection_timeout, ConnectionId}, State) ->
    case
        voice_pending_common:get_pending_connection(
            ConnectionId, maps:get(pending_connections, State)
        )
    of
        undefined ->
            {noreply, State};
        #{user_id := UserId, session_id := SessionId} ->
            case maps:get(SessionId, maps:get(sessions, State), undefined) of
                {UserId, SessionPid, _Ref} when is_pid(SessionPid) ->
                    case erlang:is_process_alive(SessionPid) of
                        true ->
                            NewPending = voice_pending_common:remove_pending_connection(
                                ConnectionId, maps:get(pending_connections, State)
                            ),
                            {noreply, State#{pending_connections => NewPending}};
                        false ->
                            disconnect_user_after_pending_timeout(
                                ConnectionId, UserId, SessionId, State
                            )
                    end;
                _ ->
                    disconnect_user_after_pending_timeout(ConnectionId, UserId, SessionId, State)
            end
    end;
handle_info(idle_timeout, State) ->
    HasParticipants = maps:size(maps:get(voice_states, State)) > 0,
    HasPendingRinging = length(maps:get(ringing, State)) > 0,
    case HasParticipants orelse HasPendingRinging of
        true ->
            {noreply, reset_idle_timer(State)};
        false ->
            dispatch_call_delete(State),
            {stop, normal, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec disconnect_user_after_pending_timeout(binary(), integer(), binary(), state()) ->
    {stop, normal, state()} | {noreply, state()}.
disconnect_user_after_pending_timeout(ConnectionId, UserId, SessionId, State) ->
    NewPending = voice_pending_common:remove_pending_connection(
        ConnectionId, maps:get(pending_connections, State)
    ),
    VoiceStates = maps:get(voice_states, State),
    case maps:is_key(UserId, VoiceStates) of
        true ->
            {noreply, State#{pending_connections => NewPending}};
        false ->
            NewSessions = remove_session_entry(SessionId, maps:get(sessions, State)),
            {noreply, State#{
                pending_connections => NewPending,
                sessions => NewSessions
            }}
    end.

-spec maybe_notify_session_force_disconnect(integer(), binary(), binary() | undefined, state()) -> ok.
maybe_notify_session_force_disconnect(UserId, SessionId, ConnectionId, State) ->
    case maps:get(SessionId, maps:get(sessions, State), undefined) of
        {UserId, SessionPid, _Ref} when is_pid(SessionPid) ->
            ChannelId = maps:get(channel_id, State),
            gen_server:cast(SessionPid, {call_force_disconnect, ChannelId, ConnectionId}),
            ok;
        _ ->
            ok
    end.

-spec remove_session_entry(binary(), #{binary() => session_entry()}) ->
    #{binary() => session_entry()}.
remove_session_entry(SessionId, Sessions) ->
    case maps:get(SessionId, Sessions, undefined) of
        {_, _, Ref} ->
            demonitor(Ref, [flush]),
            maps:remove(SessionId, Sessions);
        _ ->
            Sessions
    end.

-spec dispatch_call_create(state()) -> state().
dispatch_call_create(State) ->
    Event = build_call_event(State),
    Recipients = maps:get(recipients, State),
    spawn(fun() ->
        lists:foreach(
            fun(RecipientId) ->
                presence_manager:dispatch_to_user(RecipientId, call_create, Event)
            end,
            Recipients
        )
    end),
    State#{last_call_event => Event}.

-spec maybe_dispatch_pending_ringing(state()) -> {state(), boolean()}.
maybe_dispatch_pending_ringing(State) ->
    maybe_dispatch_pending_ringing(State, true).

-spec dispatch_call_update(state()) -> state().
dispatch_call_update(State) ->
    Event = build_call_event(State),
    case maps:get(last_call_event, State, undefined) of
        Event ->
            State;
        _ ->
            Recipients = maps:get(recipients, State),
            spawn(fun() ->
                lists:foreach(
                    fun(RecipientId) ->
                        presence_manager:dispatch_to_user(RecipientId, call_update, Event)
                    end,
                    Recipients
                )
            end),
            State#{last_call_event => Event}
    end.

-spec dispatch_call_delete(state()) -> ok.
dispatch_call_delete(State) ->
    Event = #{
        channel_id => integer_to_binary(maps:get(channel_id, State))
    },
    Recipients = maps:get(recipients, State),
    spawn(fun() ->
        lists:foreach(
            fun(RecipientId) ->
                presence_manager:dispatch_to_user(RecipientId, call_delete, Event)
            end,
            Recipients
        )
    end),
    notify_call_ended(cancel_all_ringing_timers(State)),
    ok.

-spec notify_call_ended(state()) -> ok.
notify_call_ended(State) ->
    Participants = sets:to_list(maps:get(participants_history, State)),
    EndedAt = erlang:system_time(millisecond),
    Request = #{
        <<"type">> => <<"call_ended">>,
        <<"channel_id">> => integer_to_binary(maps:get(channel_id, State)),
        <<"message_id">> => integer_to_binary(maps:get(message_id, State)),
        <<"participants">> => integer_list_to_binaries(Participants),
        <<"ended_timestamp">> => EndedAt
    },
    spawn(fun() -> rpc_client:call(Request) end),
    ok.

-spec maybe_spawn_region_switch(
    binary() | undefined | null, binary() | undefined | null, state()
) -> ok.
maybe_spawn_region_switch(OldRegion, NewRegion, State) ->
    case OldRegion =:= NewRegion of
        true ->
            ok;
        false ->
            HasParticipants = maps:size(maps:get(voice_states, State)) > 0,
            case HasParticipants of
                true ->
                    spawn(fun() ->
                        send_voice_server_updates_for_region_switch(NewRegion, State)
                    end),
                    ok;
                false ->
                    ok
            end
    end.

-spec send_voice_server_updates_for_region_switch(binary() | undefined | null, state()) -> ok.
send_voice_server_updates_for_region_switch(NewRegion, State) when is_binary(NewRegion) ->
    VoiceStates = maps:get(voice_states, State),
    Sessions = maps:get(sessions, State),
    ChannelId = maps:get(channel_id, State),
    maps:foreach(
        fun(_SessionId, {UserId, SessionPid, _Ref}) ->
            case maps:get(UserId, VoiceStates, undefined) of
                undefined ->
                    ok;
                VoiceState ->
                    send_voice_server_update_for_user(ChannelId, UserId, SessionPid, NewRegion, VoiceState)
            end
        end,
        Sessions
    ),
    ok;
send_voice_server_updates_for_region_switch(_NewRegion, _State) ->
    ok.

-spec send_voice_server_update_for_user(integer(), integer(), pid(), binary(), map()) -> ok.
send_voice_server_update_for_user(ChannelId, UserId, SessionPid, NewRegion, VoiceState) ->
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
            Req = voice_utils:add_rtc_region_to_request(Req0, NewRegion),
            case rpc_client:call(Req) of
                {ok, Data} ->
                    Token = maps:get(<<"token">>, Data),
                    Endpoint = maps:get(<<"endpoint">>, Data),
                    NewConnectionId = maps:get(<<"connectionId">>, Data),
                    VoiceServerUpdate = #{
                        <<"token">> => Token,
                        <<"endpoint">> => Endpoint,
                        <<"channel_id">> => integer_to_binary(ChannelId),
                        <<"connection_id">> => NewConnectionId
                    },
                    gen_server:cast(SessionPid, {dispatch, voice_server_update, VoiceServerUpdate});
                _ ->
                    ok
            end
    end.

-spec ensure_initiator_ready(state()) -> state().
ensure_initiator_ready(State) ->
    case maps:get(initiator_ready, State) of
        true ->
            State;
        false ->
            State#{initiator_ready => true}
    end.

-spec maybe_dispatch_pending_ringing(state(), boolean()) -> {state(), boolean()}.
maybe_dispatch_pending_ringing(State, DispatchUpdates) ->
    case maps:get(initiator_ready, State) of
        false ->
            {State, false};
        true ->
            PendingUnique = lists:usort(maps:get(pending_ringing, State)),
            case PendingUnique of
                [] ->
                    {State#{pending_ringing => []}, false};
                _ ->
                    ConnectedUsers = maps:keys(maps:get(voice_states, State)),
                    AlreadyRinging = maps:get(ringing, State),
                    ToAdd =
                        [
                            User
                         || User <- PendingUnique,
                            not lists:member(User, ConnectedUsers),
                            not lists:member(User, AlreadyRinging)
                        ],
                    NewRinging =
                        case ToAdd of
                            [] -> AlreadyRinging;
                            _ -> lists:usort(AlreadyRinging ++ ToAdd)
                        end,
                    StateWithRinging = State#{pending_ringing => [], ringing => NewRinging},
                    StateWithTimers = start_ringing_timers(ToAdd, StateWithRinging),
                    case ToAdd of
                        [] ->
                            {StateWithTimers, false};
                        _ when DispatchUpdates ->
                            UpdatedState = dispatch_call_update(StateWithTimers),
                            {UpdatedState, true};
                        _ ->
                            {StateWithTimers, false}
                    end
            end
    end.

-spec maybe_dispatch_state_update(state(), state()) -> {state(), boolean()}.
maybe_dispatch_state_update(PrevState, NewState) ->
    case maps:get(initiator_ready, PrevState) of
        true ->
            case maps:get(ringing, PrevState) =:= maps:get(ringing, NewState) of
                true ->
                    {NewState, false};
                false ->
                    UpdatedState = dispatch_call_update(NewState),
                    {UpdatedState, true}
            end;
        false ->
            {NewState, false}
    end.

-spec remove_users_from_ringing([integer()], state()) -> state().
remove_users_from_ringing(Users, State) ->
    {NewRinging, NewPending} =
        lists:foldl(
            fun(User, {RingingAcc, PendingAcc}) ->
                {lists:delete(User, RingingAcc), lists:delete(User, PendingAcc)}
            end,
            {maps:get(ringing, State), maps:get(pending_ringing, State)},
            Users
        ),
    State#{ringing => NewRinging, pending_ringing => NewPending}.

-spec start_ringing_timers([integer()], state()) -> state().
start_ringing_timers([], State) ->
    State;
start_ringing_timers([User | Rest], State) ->
    case maps:is_key(User, maps:get(ringing_timers, State)) of
        true ->
            start_ringing_timers(Rest, State);
        false ->
            Ref = erlang:send_after(?RING_TIMEOUT_MS, self(), {ring_timeout, User}),
            UpdatedTimers = maps:put(User, Ref, maps:get(ringing_timers, State)),
            start_ringing_timers(Rest, State#{ringing_timers => UpdatedTimers})
    end.

-spec cancel_ringing_timers([integer()], state()) -> state().
cancel_ringing_timers([], State) ->
    State;
cancel_ringing_timers([User | Rest], State) ->
    case maps:is_key(User, maps:get(ringing_timers, State)) of
        true ->
            Ref = maps:get(User, maps:get(ringing_timers, State)),
            erlang:cancel_timer(Ref),
            UpdatedTimers = maps:remove(User, maps:get(ringing_timers, State)),
            cancel_ringing_timers(Rest, State#{ringing_timers => UpdatedTimers});
        false ->
            cancel_ringing_timers(Rest, State)
    end.

-spec cancel_all_ringing_timers(state()) -> state().
cancel_all_ringing_timers(State) ->
    TimerRefs = maps:values(maps:get(ringing_timers, State)),
    lists:foreach(fun erlang:cancel_timer/1, TimerRefs),
    State#{ringing_timers => #{}}.

-spec reset_idle_timer(state()) -> state().
reset_idle_timer(State) ->
    case maps:get(idle_timer, State) of
        undefined -> ok;
        OldRef -> erlang:cancel_timer(OldRef)
    end,
    NewRef = erlang:send_after(?IDLE_TIMEOUT_MS, self(), idle_timeout),
    State#{idle_timer => NewRef}.

-spec format_voice_state(map()) -> map().
format_voice_state(VoiceState) ->
    maps:map(
        fun
            (<<"user_id">>, V) when is_integer(V) -> integer_to_binary(V);
            (<<"channel_id">>, V) when is_integer(V) -> integer_to_binary(V);
            (<<"guild_id">>, V) when is_integer(V) -> integer_to_binary(V);
            (_, V) -> V
        end,
        VoiceState
    ).

-spec format_voice_states(#{integer() => map()}) -> [map()].
format_voice_states(VoiceStates) ->
    Sorted = lists:keysort(1, maps:to_list(VoiceStates)),
    [format_voice_state(VS) || {_UserId, VS} <- Sorted].

-spec format_pending_connections(#{binary() => map()}) -> [map()].
format_pending_connections(PendingConnections) ->
    Sorted = lists:keysort(1, maps:to_list(PendingConnections)),
    [format_pending_connection(ConnectionId, Metadata) || {ConnectionId, Metadata} <- Sorted].

-spec format_pending_connection(binary(), map()) -> map().
format_pending_connection(ConnectionId, Metadata) ->
    JoinedAt = maps:get(joined_at, Metadata, erlang:system_time(millisecond)),
    #{
        connection_id => ConnectionId,
        user_id => integer_to_binary(maps:get(user_id, Metadata)),
        token_nonce => maps:get(token_nonce, Metadata, <<>>),
        expires_at => JoinedAt + ?PENDING_CONNECTION_TIMEOUT_MS
    }.

-spec build_call_event(state()) -> map().
build_call_event(State) ->
    #{
        channel_id => integer_to_binary(maps:get(channel_id, State)),
        message_id => integer_to_binary(maps:get(message_id, State)),
        region => maps:get(region, State),
        ringing => integer_list_to_binaries(maps:get(ringing, State)),
        voice_states => format_voice_states(maps:get(voice_states, State))
    }.

-spec integer_list_to_binaries([integer()]) -> [binary()].
integer_list_to_binaries(Values) ->
    lists:map(fun integer_to_binary/1, Values).

-spec find_session_by_pid(pid(), #{binary() => session_entry()}) ->
    {ok, binary(), integer()} | not_found.
find_session_by_pid(Pid, Sessions) ->
    maps:fold(
        fun
            (SessionId, {UserId, P, _Ref}, _) when P =:= Pid ->
                {ok, SessionId, UserId};
            (_, _, Acc) ->
                Acc
        end,
        not_found,
        Sessions
    ).

-spec handle_join_internal(
    integer(), map(), binary(), pid(), binary() | undefined, state()
) -> {reply, ok, state()}.
handle_join_internal(UserId, VoiceState, SessionId, SessionPid, ConnectionId, State) ->
    CleanState = cancel_ringing_timers([UserId], State),
    BaseState = remove_users_from_ringing([UserId], CleanState),
    NewVoiceStates = maps:put(UserId, VoiceState, maps:get(voice_states, BaseState)),
    SessionRef = monitor(process, SessionPid),
    NewSessions = maps:put(SessionId, {UserId, SessionPid, SessionRef}, maps:get(sessions, BaseState)),
    NewParticipantsHistory = sets:add_element(UserId, maps:get(participants_history, BaseState)),
    NewPending =
        case ConnectionId of
            undefined ->
                maps:get(pending_connections, BaseState);
            _ ->
                PendingMetadata = #{
                    user_id => UserId,
                    channel_id => maps:get(channel_id, BaseState),
                    connection_id => ConnectionId,
                    session_id => SessionId
                },
                erlang:send_after(
                    ?PENDING_CONNECTION_TIMEOUT_MS,
                    self(),
                    {pending_connection_timeout, ConnectionId}
                ),
                voice_pending_common:add_pending_connection(
                    ConnectionId, PendingMetadata, maps:get(pending_connections, BaseState)
                )
        end,
    NewState = BaseState#{
        voice_states => NewVoiceStates,
        sessions => NewSessions,
        pending_connections => NewPending,
        participants_history => NewParticipantsHistory
    },
    StateWithTimer = reset_idle_timer(NewState),
    {UpdatedState, Dispatched} = maybe_dispatch_state_update(BaseState, StateWithTimer),
    FinalState =
        case Dispatched of
            true -> UpdatedState;
            false -> dispatch_call_update(UpdatedState)
        end,
    {reply, ok, FinalState}.

-ifdef(TEST).

integer_list_to_binaries_test() ->
    ?assertEqual([<<"1">>, <<"2">>, <<"3">>], integer_list_to_binaries([1, 2, 3])),
    ?assertEqual([], integer_list_to_binaries([])).

find_session_by_pid_test() ->
    Pid1 = self(),
    Sessions = #{
        <<"session1">> => {100, Pid1, make_ref()},
        <<"session2">> => {200, spawn(fun() -> ok end), make_ref()}
    },
    ?assertMatch({ok, <<"session1">>, 100}, find_session_by_pid(Pid1, Sessions)),
    ?assertEqual(not_found, find_session_by_pid(spawn(fun() -> ok end), Sessions)).

format_voice_state_test() ->
    VoiceState = #{
        <<"user_id">> => 123,
        <<"channel_id">> => 456,
        <<"guild_id">> => 789,
        <<"mute">> => false
    },
    Result = format_voice_state(VoiceState),
    ?assertEqual(<<"123">>, maps:get(<<"user_id">>, Result)),
    ?assertEqual(<<"456">>, maps:get(<<"channel_id">>, Result)),
    ?assertEqual(<<"789">>, maps:get(<<"guild_id">>, Result)),
    ?assertEqual(false, maps:get(<<"mute">>, Result)).

remove_users_from_ringing_test() ->
    State = #{
        channel_id => 1,
        message_id => 1,
        region => undefined,
        ringing => [1, 2, 3],
        pending_ringing => [4, 5],
        recipients => [],
        voice_states => #{},
        sessions => #{},
        pending_connections => #{},
        initiator_ready => false,
        ringing_timers => #{},
        idle_timer => undefined,
        created_at => 0,
        participants_history => sets:new()
    },
    Result = remove_users_from_ringing([2, 4], State),
    ?assertEqual([1, 3], maps:get(ringing, Result)),
    ?assertEqual([5], maps:get(pending_ringing, Result)).

join_from_ringing_adds_voice_state_test() ->
    UserId = 42,
    SessionId = <<"session-42">>,
    ConnectionId = <<"conn-42">>,
    VoiceState = #{
        <<"user_id">> => <<"42">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => ConnectionId
    },
    State = new_call_test_state(#{
        ringing => [UserId],
        initiator_ready => true
    }),
    {reply, ok, NewState} = handle_join_internal(
        UserId, VoiceState, SessionId, self(), ConnectionId, State
    ),
    ?assert(maps:is_key(UserId, maps:get(voice_states, NewState))),
    ?assertNot(lists:member(UserId, maps:get(ringing, NewState))).

leave_then_rejoin_keeps_voice_state_test() ->
    UserId = 42,
    OtherUserId = 84,
    SessionId = <<"session-42">>,
    OtherSessionId = <<"session-84">>,
    VoiceState = #{
        <<"user_id">> => <<"42">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => <<"conn-42">>
    },
    OtherVoiceState = #{
        <<"user_id">> => <<"84">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => <<"conn-84">>
    },
    State = new_call_test_state(#{
        voice_states => #{
            UserId => VoiceState,
            OtherUserId => OtherVoiceState
        },
        sessions => #{
            SessionId => {UserId, self(), make_ref()},
            OtherSessionId => {OtherUserId, self(), make_ref()}
        },
        initiator_ready => true
    }),
    {reply, ok, PostLeaveState} = handle_call({leave, SessionId}, {self(), make_ref()}, State),
    ?assertEqual(undefined, maps:get(UserId, maps:get(voice_states, PostLeaveState), undefined)),
    ?assertEqual(undefined, maps:get(SessionId, maps:get(sessions, PostLeaveState), undefined)),
    {reply, ok, RejoinedState} = handle_join_internal(
        UserId, VoiceState, SessionId, self(), <<"conn-42">>, PostLeaveState
    ),
    ?assert(maps:is_key(UserId, maps:get(voice_states, RejoinedState))).

pending_connection_timeout_preserves_active_voice_state_test() ->
    UserId = 42,
    SessionId = <<"session-42">>,
    ConnectionId = <<"conn-42">>,
    VoiceState = #{
        <<"user_id">> => <<"42">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => ConnectionId
    },
    State = new_call_test_state(#{
        voice_states => #{UserId => VoiceState},
        pending_connections => #{
            ConnectionId => #{
                user_id => UserId,
                session_id => SessionId,
                connection_id => ConnectionId,
                channel_id => 1
            }
        },
        initiator_ready => true
    }),
    {noreply, NewState} = disconnect_user_after_pending_timeout(ConnectionId, UserId, SessionId, State),
    ?assert(maps:is_key(UserId, maps:get(voice_states, NewState))),
    ?assertEqual(undefined, maps:get(ConnectionId, maps:get(pending_connections, NewState), undefined)).

disconnect_user_if_in_channel_notifies_session_cleanup_test() ->
    UserId = 42,
    OtherUserId = 84,
    SessionId = <<"session-42">>,
    OtherSessionId = <<"session-84">>,
    ConnectionId = <<"conn-42">>,
    VoiceState = #{
        <<"user_id">> => <<"42">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => ConnectionId
    },
    OtherVoiceState = #{
        <<"user_id">> => <<"84">>,
        <<"channel_id">> => <<"1">>,
        <<"connection_id">> => <<"conn-84">>
    },
    State = new_call_test_state(#{
        voice_states => #{
            UserId => VoiceState,
            OtherUserId => OtherVoiceState
        },
        sessions => #{
            SessionId => {UserId, self(), make_ref()},
            OtherSessionId => {OtherUserId, self(), make_ref()}
        },
        initiator_ready => true
    }),
    {reply, #{success := true}, NewState} = handle_call(
        {disconnect_user_if_in_channel, UserId, 1, ConnectionId},
        {self(), make_ref()},
        State
    ),
    ?assertEqual(undefined, maps:get(UserId, maps:get(voice_states, NewState), undefined)),
    receive
        {'$gen_cast', {call_force_disconnect, 1, ConnectionId}} ->
            ok
    after 1000 ->
        ?assert(false)
    end.

new_call_test_state(Overrides) ->
    maps:merge(
        #{
            channel_id => 1,
            message_id => 1,
            region => undefined,
            ringing => [],
            pending_ringing => [],
            recipients => [],
            voice_states => #{},
            sessions => #{},
            pending_connections => #{},
            initiator_ready => false,
            ringing_timers => #{},
            idle_timer => undefined,
            created_at => 0,
            participants_history => sets:new(),
            last_call_event => undefined
        },
        Overrides
    ).

-endif.
