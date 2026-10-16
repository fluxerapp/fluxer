%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_handler_heartbeat).
-typing([eqwalizer]).

-export([
    handle_heartbeat_check/1, handle_heartbeat_check/2,
    handle_heartbeat/2,
    schedule_heartbeat_check/1,
    cancel_heartbeat_timer/1
]).

-type state() :: gateway_handler:state().
-type ws_result() :: gateway_handler:ws_result().

-export_type([state/0, ws_result/0]).

-type timer_state() :: {reference(), reference()} | undefined.

-spec handle_heartbeat_check(reference(), state()) -> ws_result().
handle_heartbeat_check(Token, State) ->
    case maps:get(heartbeat_timer, State, undefined) of
        {TimerRef, Token} ->
            cancel_heartbeat_timer_value({TimerRef, Token}),
            handle_heartbeat_check(State#{heartbeat_timer => undefined});
        _ ->
            {ok, State}
    end.

-spec handle_heartbeat_check(state()) -> ws_result().
handle_heartbeat_check(#{heartbeat_state := HeartbeatState} = State) ->
    Now = erlang:system_time(millisecond),
    LastAck = maps:get(last_ack, HeartbeatState, Now),
    WaitingForAck = maps:get(waiting_for_ack, HeartbeatState, false),
    Timeout = constants:heartbeat_timeout(),
    Interval = constants:heartbeat_interval(),
    evaluate_heartbeat(State, Now, LastAck, WaitingForAck, Timeout, Interval).

-spec evaluate_heartbeat(state(), integer(), integer(), boolean(), integer(), integer()) ->
    ws_result().
evaluate_heartbeat(State, Now, LastAck, true, Timeout, _Interval) when
    (Now - LastAck) > Timeout
->
    gateway_handler_encode:close_with_reason(session_timeout, <<"Heartbeat timeout">>, State);
evaluate_heartbeat(State, Now, LastAck, _Waiting, _Timeout, Interval) when
    ((Now - LastAck) * 10) >= (Interval * 9)
->
    send_heartbeat_request(State);
evaluate_heartbeat(State, _Now, _LastAck, _Waiting, _Timeout, _Interval) ->
    {ok, schedule_heartbeat_check(State)}.

-spec send_heartbeat_request(state()) -> ws_result().
send_heartbeat_request(#{heartbeat_state := HeartbeatState} = State) ->
    Message = #{<<"op">> => constants:opcode_to_num(heartbeat), <<"d">> => null},
    NewState = schedule_heartbeat_check(State#{
        heartbeat_state => HeartbeatState#{waiting_for_ack => true}
    }),
    encode_or_keep_state(Message, NewState).

-spec handle_heartbeat(term(), state()) -> ws_result().
handle_heartbeat(Seq, #{heartbeat_state := HeartbeatState, session_pid := SessionPid} = State) ->
    case verify_heartbeat_ack(Seq, SessionPid) of
        true ->
            send_heartbeat_ack(State, HeartbeatState);
        false ->
            gateway_handler_encode:close_with_reason(invalid_seq, <<"Invalid sequence">>, State)
    end.

-spec send_heartbeat_ack(state(), map()) -> ws_result().
send_heartbeat_ack(State, HeartbeatState) ->
    NewHeartbeatState = HeartbeatState#{
        last_ack => erlang:system_time(millisecond),
        waiting_for_ack => false
    },
    AckMessage = #{<<"op">> => constants:opcode_to_num(heartbeat_ack)},
    NewState = State#{heartbeat_state => NewHeartbeatState},
    encode_or_keep_state(AckMessage, NewState).

-spec encode_or_keep_state(map(), state()) -> ws_result().
encode_or_keep_state(Message, State) ->
    case gateway_handler_encode:encode_and_compress(Message, State) of
        {ok, Frame, NewState} -> {[Frame], NewState};
        {error, _} -> {ok, State}
    end.

-spec verify_heartbeat_ack(term(), pid() | undefined) -> boolean().
verify_heartbeat_ack(_, undefined) ->
    true;
verify_heartbeat_ack(null, _) ->
    true;
verify_heartbeat_ack(SeqNum, Pid) when is_integer(SeqNum), is_pid(Pid) ->
    try gen_server:call(Pid, {heartbeat_ack, SeqNum}, 5000) of
        true -> true;
        ok -> true;
        _ -> false
    catch
        exit:_ -> false
    end;
verify_heartbeat_ack(_, _) ->
    false.

-spec schedule_heartbeat_check(state()) -> state().
schedule_heartbeat_check(State) ->
    case maps:get(heartbeat_timer, State, undefined) of
        {TimerRef, Token} when is_reference(TimerRef), is_reference(Token) ->
            State;
        _ ->
            Token = make_ref(),
            TimerRef = erlang:send_after(
                constants:heartbeat_interval() div 3,
                self(),
                {heartbeat_check, Token}
            ),
            State#{heartbeat_timer => {TimerRef, Token}}
    end.

-spec cancel_heartbeat_timer(state()) -> ok.
cancel_heartbeat_timer(State) ->
    cancel_heartbeat_timer_value(maps:get(heartbeat_timer, State, undefined)).

-spec cancel_heartbeat_timer_value(timer_state()) -> ok.
cancel_heartbeat_timer_value(undefined) ->
    ok;
cancel_heartbeat_timer_value({TimerRef, _Token}) ->
    _ = erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
    ok.
