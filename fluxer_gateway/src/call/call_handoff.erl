%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(call_handoff).
-typing([eqwalizer]).

-export([export_state/1, restore_state/1]).

-define(PENDING_CONNECTION_TIMEOUT_MS, 30000).

-spec export_state(map()) -> map().
export_state(State) ->
    #{
        channel_id => maps:get(channel_id, State),
        message_id => maps:get(message_id, State),
        region => maps:get(region, State, undefined),
        ringing => maps:get(ringing, State, []),
        pending_ringing => maps:get(pending_ringing, State, []),
        recipients => maps:get(recipients, State, []),
        caller_id => maps:get(caller_id, State, undefined),
        caller_name => maps:get(caller_name, State, undefined),
        caller_avatar => maps:get(caller_avatar, State, undefined),
        voice_states => maps:get(voice_states, State, #{}),
        sessions => export_sessions(maps:get(sessions, State, #{})),
        pending_connections => maps:get(pending_connections, State, #{}),
        initiator_ready => maps:get(initiator_ready, State, true),
        created_at => maps:get(created_at, State, erlang:system_time(millisecond)),
        participants_history => export_participants_history(
            maps:get(participants_history, State, sets:new())
        ),
        last_call_event => maps:get(last_call_event, State, undefined)
    }.

-spec restore_state(map()) -> map().
restore_state(TransferState) ->
    Sessions = restore_sessions(maps:get(sessions, TransferState, #{})),
    VoiceStates = restore_voice_states(
        maps:get(voice_states, TransferState, #{}), Sessions
    ),
    State0 = #{
        channel_id => maps:get(channel_id, TransferState),
        message_id => maps:get(message_id, TransferState),
        region => maps:get(region, TransferState, undefined),
        ringing => maps:get(ringing, TransferState, []),
        pending_ringing => maps:get(pending_ringing, TransferState, []),
        recipients => maps:get(recipients, TransferState, []),
        caller_id => maps:get(caller_id, TransferState, undefined),
        caller_name => maps:get(caller_name, TransferState, undefined),
        caller_avatar => maps:get(caller_avatar, TransferState, undefined),
        voice_states => VoiceStates,
        sessions => Sessions,
        pending_connections => maps:get(pending_connections, TransferState, #{}),
        initiator_ready => maps:get(initiator_ready, TransferState, true),
        ringing_timers => #{},
        idle_timer => undefined,
        created_at => maps:get(created_at, TransferState, erlang:system_time(millisecond)),
        participants_history => restore_participants_history(
            maps:get(participants_history, TransferState, [])
        ),
        last_call_event => maps:get(last_call_event, TransferState, undefined)
    },
    State1 = call_ringing:start_ringing_timers(maps:get(ringing, State0, []), State0),
    State2 = restart_pending_connection_timers(State1),
    State3 = call_ringing:reset_idle_timer(State2),
    _ = call_state:sync_voice_state_count_diff(#{voice_states => #{}}, State3),
    State3.

-spec export_sessions(map()) -> map().
export_sessions(Sessions) ->
    maps:fold(
        fun export_session/3,
        #{},
        Sessions
    ).

-spec export_session(term(), term(), map()) -> map().
export_session(SessionId, {UserId, SessionPid, _Ref}, Acc) when is_pid(SessionPid) ->
    maybe_export_live_session(SessionId, UserId, SessionPid, Acc);
export_session(_SessionId, _Value, Acc) ->
    Acc.

-spec maybe_export_live_session(term(), term(), pid(), map()) -> map().
maybe_export_live_session(SessionId, UserId, SessionPid, Acc) ->
    case process_liveness:is_alive(SessionPid) of
        true ->
            Acc#{SessionId => #{user_id => UserId, session_pid => SessionPid}};
        false ->
            Acc
    end.

-spec restore_sessions(map()) -> map().
restore_sessions(Sessions) ->
    maps:fold(
        fun restore_session/3,
        #{},
        Sessions
    ).

-spec restore_session(term(), term(), map()) -> map().
restore_session(SessionId, #{user_id := UserId, session_pid := SessionPid}, Acc) when
    is_pid(SessionPid)
->
    maybe_restore_session(SessionId, UserId, SessionPid, Acc);
restore_session(SessionId, {UserId, SessionPid, _Ref}, Acc) when is_pid(SessionPid) ->
    maybe_restore_session(SessionId, UserId, SessionPid, Acc);
restore_session(_SessionId, _Value, Acc) ->
    Acc.

-spec maybe_restore_session(term(), term(), pid(), map()) -> map().
maybe_restore_session(SessionId, UserId, SessionPid, Acc) ->
    case process_liveness:is_alive(SessionPid) of
        true ->
            Acc#{SessionId => {UserId, SessionPid, monitor(process, SessionPid)}};
        false ->
            Acc
    end.

-spec restore_voice_states(map(), map()) -> map().
restore_voice_states(VoiceStates, Sessions) ->
    ActiveUsers = maps:fold(
        fun(_SessionId, {UserId, _Pid, _Ref}, Acc) -> sets:add_element(UserId, Acc) end,
        sets:new(),
        Sessions
    ),
    maps:filter(
        fun(UserId, _VoiceState) -> sets:is_element(UserId, ActiveUsers) end,
        VoiceStates
    ).

-spec export_participants_history(term()) -> [term()].
export_participants_history(History) when is_map(History) ->
    maps:keys(History);
export_participants_history(_History) ->
    [].

-spec restore_participants_history(term()) -> sets:set().
restore_participants_history(History) when is_list(History) ->
    sets:from_list(History);
restore_participants_history(_History) ->
    sets:new().

-spec restart_pending_connection_timers(map()) -> map().
restart_pending_connection_timers(#{pending_connections := Pending} = State) ->
    maps:foreach(
        fun(ConnectionId, Metadata) ->
            erlang:send_after(
                pending_timeout_delay(Metadata),
                self(),
                {pending_connection_timeout, ConnectionId}
            ),
            ok
        end,
        Pending
    ),
    State.

-spec pending_timeout_delay(term()) -> non_neg_integer().
pending_timeout_delay(Metadata) when is_map(Metadata) ->
    JoinedAt = maps:get(joined_at, Metadata, erlang:system_time(millisecond)),
    Elapsed = erlang:system_time(millisecond) - normalize_joined_at(JoinedAt),
    max(0, ?PENDING_CONNECTION_TIMEOUT_MS - Elapsed);
pending_timeout_delay(_Metadata) ->
    ?PENDING_CONNECTION_TIMEOUT_MS.

-spec normalize_joined_at(term()) -> integer().
normalize_joined_at(Value) when is_integer(Value) ->
    Value;
normalize_joined_at(_Value) ->
    erlang:system_time(millisecond).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

restore_state_remonitors_live_sessions_test() ->
    SessionPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    SessionId = <<"session-a">>,
    VoiceState = #{<<"connection_id">> => <<"conn-a">>},
    TransferState = #{
        channel_id => 123,
        message_id => 456,
        region => null,
        ringing => [1],
        pending_ringing => [],
        recipients => [1],
        voice_states => #{1 => VoiceState},
        sessions => #{SessionId => #{user_id => 1, session_pid => SessionPid}},
        pending_connections => #{},
        participants_history => [1]
    },
    Restored = restore_state(TransferState),
    ?assertMatch(
        {1, SessionPid, Ref} when is_reference(Ref),
        maps:get(SessionId, maps:get(sessions, Restored))
    ),
    ?assertEqual(#{1 => VoiceState}, maps:get(voice_states, Restored)),
    SessionPid ! stop.

export_and_restore_round_trips_the_caller_test() ->
    State = #{
        channel_id => 123,
        message_id => 456,
        caller_id => 7,
        caller_name => <<"Ada">>,
        caller_avatar => <<"a1b2c3d4">>
    },
    Restored = restore_state(export_state(State)),
    ?assertEqual(
        #{caller_id => 7, caller_name => <<"Ada">>, caller_avatar => <<"a1b2c3d4">>},
        call_state:caller_from_state(Restored)
    ).

restore_state_reads_a_transfer_without_a_caller_as_undefined_test() ->
    Restored = restore_state(#{channel_id => 123, message_id => 456}),
    ?assertEqual(
        #{caller_id => undefined, caller_name => undefined, caller_avatar => undefined},
        call_state:caller_from_state(Restored)
    ).

-endif.
