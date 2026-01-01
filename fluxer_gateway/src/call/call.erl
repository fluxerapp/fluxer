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

-record(state, {
    channel_id,
    message_id,
    region,
    ringing = [],
    pending_ringing = [],
    recipients = [],
    voice_states = #{},
    sessions = #{},
    pending_connections = #{},
    initiator_ready = false,
    ringing_timers = #{},
    idle_timer = undefined,
    created_at,
    participants_history = sets:new() :: sets:set(integer())
}).
-type state() :: #state{
    channel_id :: integer(),
    message_id :: integer(),
    region :: term(),
    ringing :: [integer()],
    pending_ringing :: [integer()],
    recipients :: [integer()],
    voice_states :: map(),
    sessions :: map(),
    pending_connections :: map(),
    initiator_ready :: boolean(),
    ringing_timers :: map(),
    idle_timer :: reference() | undefined,
    created_at :: integer(),
    participants_history :: sets:set(integer())
}.
-define(RING_TIMEOUT_MS, 30000).
-define(IDLE_TIMEOUT_MS, 120000).

-spec start_link(map()) -> {ok, pid()} | {error, term()} | ignore.

start_link(CallData) ->
    gen_server:start_link(?MODULE, CallData, []).
-spec init(map()) -> {ok, state()}.

init(CallData) ->
    #{
        channel_id := ChannelId,
        message_id := MessageId,
        region := Region,
        ringing := Ringing,
        recipients := Recipients
    } = CallData,

    State = #state{
        channel_id = ChannelId,
        message_id = MessageId,
        region = Region,
        ringing = [],
        pending_ringing = Ringing,
        recipients = Recipients,
        created_at = erlang:system_time(millisecond)
    },

    ReadyState = ensure_initiator_ready(State),
    {StateWithRinging, Dispatched} = maybe_dispatch_pending_ringing(ReadyState, false),
    StateWithIdleTimer = reset_idle_timer(StateWithRinging),

    dispatch_call_create(StateWithIdleTimer),

    case Dispatched of
        false ->
            case StateWithIdleTimer#state.ringing of
                [] -> ok;
                _ -> dispatch_call_update(StateWithIdleTimer)
            end;
        true ->
            ok
    end,

    {ok, StateWithIdleTimer}.

handle_call({get_state}, _From, State) ->
    CallData = #{
        channel_id => integer_to_binary(State#state.channel_id),
        message_id => integer_to_binary(State#state.message_id),
        region => State#state.region,
        ringing => integer_list_to_binaries(State#state.ringing),
        voice_states => [format_voice_state(VS) || VS <- maps:values(State#state.voice_states)],
        created_at => State#state.created_at
    },
    {reply, {ok, CallData}, State};
handle_call({update_region, NewRegion}, _From, State) ->
    NewState = State#state{region = NewRegion},
    dispatch_call_update(NewState),

    {reply, ok, NewState};
handle_call({ring_recipients, Recipients}, _From, State) ->
    CurrentVoiceUsers = maps:keys(State#state.voice_states),
    PendingAdditions = [U || U <- Recipients, not lists:member(U, CurrentVoiceUsers)],
    NewPending = lists:usort(State#state.pending_ringing ++ PendingAdditions),
    StateWithPending = State#state{pending_ringing = NewPending},
    {UpdatedState, _} = maybe_dispatch_pending_ringing(StateWithPending),
    {reply, ok, UpdatedState};
handle_call({stop_ringing, Recipients}, _From, State) ->
    CancelledState = cancel_ringing_timers(Recipients, State),
    NewRinging = CancelledState#state.ringing -- Recipients,
    NewPending = CancelledState#state.pending_ringing -- Recipients,
    StateWithoutRecipients = CancelledState#state{
        ringing = NewRinging, pending_ringing = NewPending
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
            ConnectionId, ReadyState#state.pending_connections
        )
    of
        {not_found, _} ->
            {DispatchedState, _} = maybe_dispatch_pending_ringing(ReadyState),
            {reply, #{success => true, already_confirmed => true}, DispatchedState};
        {confirmed, NewPending} ->
            logger:info(
                "[call] Confirmed voice connection ~p for channel ~p",
                [ConnectionId, ReadyState#state.channel_id]
            ),
            StateWithPending = ReadyState#state{pending_connections = NewPending},
            {DispatchedState, _} = maybe_dispatch_pending_ringing(StateWithPending),
            {reply, #{success => true}, DispatchedState}
    end;
handle_call({disconnect_user_if_in_channel, UserId, ExpectedChannelId, ConnectionId}, _From, State) ->
    CleanupFun = fun(_U, _S) -> ok end,
    case
        voice_disconnect_common:disconnect_user_if_in_channel(
            UserId,
            ExpectedChannelId,
            State#state.voice_states,
            State#state.sessions,
            CleanupFun
        )
    of
        {not_found, _, _} ->
            NewPending = voice_pending_common:remove_pending_connection(
                ConnectionId, State#state.pending_connections
            ),
            {reply, #{success => true, ignored => true, reason => <<"not_in_call">>}, State#state{
                pending_connections = NewPending
            }};
        {channel_mismatch, _, _} ->
            {reply, #{success => true, ignored => true, reason => <<"channel_mismatch">>}, State};
        {ok, NewVoiceStates, NewSessions} ->
            NewPending = voice_pending_common:remove_pending_connection(
                ConnectionId, State#state.pending_connections
            ),
            BaseState = State#state{
                voice_states = NewVoiceStates,
                sessions = NewSessions,
                pending_connections = NewPending
            },
            CancelledTimersState = cancel_ringing_timers([UserId], BaseState),
            RingCleanupState = remove_users_from_ringing([UserId], CancelledTimersState),
            {UpdatedState, Dispatched} = maybe_dispatch_state_update(BaseState, RingCleanupState),
            case maps:size(UpdatedState#state.voice_states) of
                0 ->
                    dispatch_call_delete(UpdatedState),
                    {stop, normal, #{success => true}, UpdatedState};
                _ ->
                    case Dispatched of
                        true -> ok;
                        false -> dispatch_call_update(UpdatedState)
                    end,
                    {reply, #{success => true}, UpdatedState}
            end
    end;
handle_call({leave, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        {UserId, _Pid, Ref} ->
            demonitor(Ref, [flush]),

            NewVoiceStates = maps:remove(UserId, State#state.voice_states),
            NewSessions = maps:remove(SessionId, State#state.sessions),

            BaseState = State#state{
                voice_states = NewVoiceStates,
                sessions = NewSessions
            },
            CancelledTimersState = cancel_ringing_timers([UserId], BaseState),
            RingCleanupState = remove_users_from_ringing([UserId], CancelledTimersState),
            {UpdatedState, Dispatched} = maybe_dispatch_state_update(BaseState, RingCleanupState),

            case maps:size(UpdatedState#state.voice_states) of
                0 ->
                    dispatch_call_delete(UpdatedState),
                    {stop, normal, ok, UpdatedState};
                _ ->
                    case Dispatched of
                        true -> ok;
                        false -> dispatch_call_update(UpdatedState)
                    end,
                    {reply, ok, UpdatedState}
            end;
        undefined ->
            {reply, {error, not_found}, State}
    end;
handle_call({update_voice_state, UserId, VoiceState}, _From, State) ->
    case maps:is_key(UserId, State#state.voice_states) of
        true ->
            NewVoiceStates = maps:put(UserId, VoiceState, State#state.voice_states),
            NewState = State#state{voice_states = NewVoiceStates},
            dispatch_call_update(NewState),
            {reply, ok, NewState};
        false ->
            {reply, {error, not_in_call}, State}
    end;
handle_call({get_sessions}, _From, State) ->
    StateMap = #{
        sessions => State#state.sessions,
        voice_states => State#state.voice_states
    },
    {reply, StateMap, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case find_session_by_pid(Pid, State#state.sessions) of
        {ok, SessionId, UserId} ->
            NewVoiceStates = maps:remove(UserId, State#state.voice_states),
            NewSessions = maps:remove(SessionId, State#state.sessions),

            BaseState = State#state{
                voice_states = NewVoiceStates,
                sessions = NewSessions
            },
            CancelledTimersState = cancel_ringing_timers([UserId], BaseState),
            RingCleanupState = remove_users_from_ringing([UserId], CancelledTimersState),
            {UpdatedState, Dispatched} = maybe_dispatch_state_update(BaseState, RingCleanupState),

            case maps:size(UpdatedState#state.voice_states) of
                0 ->
                    dispatch_call_delete(UpdatedState),
                    {stop, normal, UpdatedState};
                _ ->
                    case Dispatched of
                        true -> ok;
                        false -> dispatch_call_update(UpdatedState)
                    end,
                    {noreply, UpdatedState}
            end;
        not_found ->
            {noreply, State}
    end;
handle_info({ring_timeout, UserId}, State) ->
    case maps:get(UserId, State#state.ringing_timers, undefined) of
        undefined ->
            {noreply, State};
        _ ->
            CancelState = cancel_ringing_timers([UserId], State),
            RingCleanupState = remove_users_from_ringing([UserId], CancelState),
            {UpdatedState, _} = maybe_dispatch_state_update(State, RingCleanupState),

            HasParticipants = maps:size(UpdatedState#state.voice_states) > 0,
            HasPendingRinging = length(UpdatedState#state.ringing) > 0,

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
            ConnectionId, State#state.pending_connections
        )
    of
        undefined ->
            {noreply, State};
        #{user_id := UserId, session_id := SessionId} ->
            logger:warning(
                "[call] Pending connection ~p timed out for user ~p in channel ~p",
                [ConnectionId, UserId, State#state.channel_id]
            ),

            case maps:get(SessionId, State#state.sessions, undefined) of
                {UserId, SessionPid, _Ref} when is_pid(SessionPid) ->
                    case erlang:is_process_alive(SessionPid) of
                        true ->
                            logger:warning(
                                "[call] Pending connection ~p timed out, but session is still alive; keeping user ~p in call",
                                [ConnectionId, UserId]
                            ),
                            NewPending = voice_pending_common:remove_pending_connection(
                                ConnectionId, State#state.pending_connections
                            ),
                            {noreply, State#state{pending_connections = NewPending}};
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
    HasParticipants = maps:size(State#state.voice_states) > 0,
    HasPendingRinging = length(State#state.ringing) > 0,

    case HasParticipants orelse HasPendingRinging of
        true ->
            {noreply, reset_idle_timer(State)};
        false ->
            logger:info(
                "[call] Idle timeout - deleting empty call for channel ~p",
                [State#state.channel_id]
            ),
            dispatch_call_delete(State),
            {stop, normal, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

disconnect_user_after_pending_timeout(ConnectionId, UserId, SessionId, State) ->
    NewPending = voice_pending_common:remove_pending_connection(
        ConnectionId, State#state.pending_connections
    ),

    NewVoiceStates = maps:remove(UserId, State#state.voice_states),

    NewSessions =
        case maps:get(SessionId, State#state.sessions, undefined) of
            undefined ->
                State#state.sessions;
            {_, _, Ref} ->
                demonitor(Ref, [flush]),
                maps:remove(SessionId, State#state.sessions)
        end,

    NewState = State#state{
        pending_connections = NewPending,
        voice_states = NewVoiceStates,
        sessions = NewSessions
    },

    case maps:size(NewVoiceStates) of
        0 ->
            dispatch_call_delete(NewState),
            {stop, normal, NewState};
        _ ->
            dispatch_call_update(NewState),
            {noreply, NewState}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

dispatch_call_create(State) ->
    Event = #{
        channel_id => integer_to_binary(State#state.channel_id),
        message_id => integer_to_binary(State#state.message_id),
        region => State#state.region,
        ringing => integer_list_to_binaries(State#state.ringing),
        voice_states => [format_voice_state(VS) || VS <- maps:values(State#state.voice_states)]
    },

    lists:foreach(
        fun(RecipientId) ->
            presence_manager:dispatch_to_user(RecipientId, call_create, Event)
        end,
        State#state.recipients
    ).

maybe_dispatch_pending_ringing(State) ->
    maybe_dispatch_pending_ringing(State, true).

dispatch_call_update(State) ->
    Event = #{
        channel_id => integer_to_binary(State#state.channel_id),
        message_id => integer_to_binary(State#state.message_id),
        region => State#state.region,
        ringing => integer_list_to_binaries(State#state.ringing),
        voice_states => [format_voice_state(VS) || VS <- maps:values(State#state.voice_states)]
    },

    lists:foreach(
        fun(RecipientId) ->
            presence_manager:dispatch_to_user(RecipientId, call_update, Event)
        end,
        State#state.recipients
    ).

dispatch_call_delete(State) ->
    Event = #{
        channel_id => integer_to_binary(State#state.channel_id)
    },

    lists:foreach(
        fun(RecipientId) ->
            presence_manager:dispatch_to_user(RecipientId, call_delete, Event)
        end,
        State#state.recipients
    ),

    notify_call_ended(cancel_all_ringing_timers(State)).

notify_call_ended(State) ->
    Participants = sets:to_list(State#state.participants_history),
    EndedAt = erlang:system_time(millisecond),

    Request = #{
        <<"type">> => <<"call_ended">>,
        <<"channel_id">> => integer_to_binary(State#state.channel_id),
        <<"message_id">> => integer_to_binary(State#state.message_id),
        <<"participants">> => integer_list_to_binaries(Participants),
        <<"ended_timestamp">> => EndedAt
    },

    spawn(fun() ->
        case rpc_client:call(Request) of
            {ok, _} ->
                logger:debug("[call] Successfully notified API of call end for channel ~p", [
                    State#state.channel_id
                ]);
            {error, Reason} ->
                logger:warning("[call] Failed to notify API of call end: ~p", [Reason])
        end
    end).

ensure_initiator_ready(State) ->
    case State#state.initiator_ready of
        true ->
            State;
        false ->
            State#state{initiator_ready = true}
    end.

maybe_dispatch_pending_ringing(State, DispatchUpdates) ->
    case State#state.initiator_ready of
        false ->
            {State, false};
        true ->
            PendingUnique = lists:usort(State#state.pending_ringing),
            case PendingUnique of
                [] ->
                    {State#state{pending_ringing = []}, false};
                _ ->
                    ConnectedUsers = maps:keys(State#state.voice_states),
                    AlreadyRinging = State#state.ringing,
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
                    StateWithRinging = State#state{pending_ringing = [], ringing = NewRinging},
                    StateWithTimers = start_ringing_timers(ToAdd, StateWithRinging),
                    case ToAdd of
                        [] ->
                            {StateWithTimers, false};
                        _ when DispatchUpdates ->
                            dispatch_call_update(StateWithTimers),
                            {StateWithTimers, true};
                        _ ->
                            {StateWithTimers, false}
                    end
            end
    end.

maybe_dispatch_state_update(PrevState, NewState) ->
    case PrevState#state.initiator_ready of
        true ->
            case PrevState#state.ringing =:= NewState#state.ringing of
                true ->
                    {NewState, false};
                false ->
                    dispatch_call_update(NewState),
                    {NewState, true}
            end;
        false ->
            {NewState, false}
    end.

remove_users_from_ringing(Users, State) ->
    {NewRinging, NewPending} =
        lists:foldl(
            fun(User, {RingingAcc, PendingAcc}) ->
                {lists:delete(User, RingingAcc), lists:delete(User, PendingAcc)}
            end,
            {State#state.ringing, State#state.pending_ringing},
            Users
        ),
    State#state{ringing = NewRinging, pending_ringing = NewPending}.

start_ringing_timers([], State) ->
    State;
start_ringing_timers([User | Rest], State) ->
    case maps:is_key(User, State#state.ringing_timers) of
        true ->
            start_ringing_timers(Rest, State);
        false ->
            Ref = erlang:send_after(?RING_TIMEOUT_MS, self(), {ring_timeout, User}),
            UpdatedTimers = maps:put(User, Ref, State#state.ringing_timers),
            start_ringing_timers(Rest, State#state{ringing_timers = UpdatedTimers})
    end.

cancel_ringing_timers([], State) ->
    State;
cancel_ringing_timers([User | Rest], State) ->
    case maps:is_key(User, State#state.ringing_timers) of
        true ->
            Ref = maps:get(User, State#state.ringing_timers),
            erlang:cancel_timer(Ref),
            UpdatedTimers = maps:remove(User, State#state.ringing_timers),
            cancel_ringing_timers(Rest, State#state{ringing_timers = UpdatedTimers});
        false ->
            cancel_ringing_timers(Rest, State)
    end.

cancel_all_ringing_timers(State) ->
    TimerRefs = maps:values(State#state.ringing_timers),
    [erlang:cancel_timer(Ref) || Ref <- TimerRefs],
    State#state{ringing_timers = #{}}.

reset_idle_timer(State) ->
    case State#state.idle_timer of
        undefined -> ok;
        OldRef -> erlang:cancel_timer(OldRef)
    end,
    NewRef = erlang:send_after(?IDLE_TIMEOUT_MS, self(), idle_timeout),
    State#state{idle_timer = NewRef}.

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

integer_list_to_binaries(Values) ->
    lists:map(fun integer_to_binary/1, Values).

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

handle_join_internal(UserId, VoiceState, SessionId, SessionPid, ConnectionId, State) ->
    CleanState = cancel_ringing_timers([UserId], State),
    BaseState = remove_users_from_ringing([UserId], CleanState),
    NewVoiceStates = maps:put(UserId, VoiceState, BaseState#state.voice_states),

    SessionRef = monitor(process, SessionPid),
    NewSessions = maps:put(SessionId, {UserId, SessionPid, SessionRef}, BaseState#state.sessions),
    NewParticipantsHistory = sets:add_element(UserId, BaseState#state.participants_history),

    NewPending =
        case ConnectionId of
            undefined ->
                BaseState#state.pending_connections;
            _ ->
                PendingMetadata = #{
                    user_id => UserId,
                    channel_id => BaseState#state.channel_id,
                    connection_id => ConnectionId,
                    session_id => SessionId
                },
                erlang:send_after(30000, self(), {pending_connection_timeout, ConnectionId}),
                voice_pending_common:add_pending_connection(
                    ConnectionId, PendingMetadata, BaseState#state.pending_connections
                )
        end,

    NewState = BaseState#state{
        voice_states = NewVoiceStates,
        sessions = NewSessions,
        pending_connections = NewPending,
        participants_history = NewParticipantsHistory
    },

    StateWithTimer = reset_idle_timer(NewState),
    {UpdatedState, Dispatched} = maybe_dispatch_state_update(BaseState, StateWithTimer),
    case Dispatched of
        true -> ok;
        false -> dispatch_call_update(UpdatedState)
    end,
    {reply, ok, UpdatedState}.
