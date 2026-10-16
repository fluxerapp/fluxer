%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_sessions_connect_cleanup).
-typing([eqwalizer]).

-export([
    cleanup_connect_admission_for_session/2,
    normalize_connect_queue/1,
    should_auto_stop_on_empty/1,
    maybe_mark_auto_stop_pending/1,
    clear_auto_stop_pending/1
]).

-type guild_state() :: map().
-type session_id() :: binary().
-export_type([guild_state/0, session_id/0]).

-define(AUTO_STOP_EMPTY_GRACE_MS, 30000).

-spec cleanup_connect_admission_for_session(session_id(), guild_state()) -> guild_state().
cleanup_connect_admission_for_session(SessionId, State) ->
    case maps:get(session_connect_pending, State, undefined) of
        #{SessionId := _} = Pending ->
            State1 = State#{session_connect_pending => maps:remove(SessionId, Pending)},
            cleanup_connect_queue(SessionId, State1);
        _ ->
            State
    end.

-spec normalize_connect_queue(term()) -> queue:queue() | undefined.
normalize_connect_queue({In, Out}) when is_list(In), is_list(Out) ->
    {In, Out};
normalize_connect_queue(Value) when is_list(Value) ->
    queue:from_list(Value);
normalize_connect_queue(_) ->
    undefined.

-spec should_auto_stop_on_empty(guild_state()) -> boolean().
should_auto_stop_on_empty(State) ->
    not maps:get(disable_auto_stop_on_empty, State, false).

-spec maybe_mark_auto_stop_pending(guild_state()) -> guild_state().
maybe_mark_auto_stop_pending(State) ->
    case should_auto_stop_on_empty(State) of
        true -> set_auto_stop_timer(State);
        false -> clear_auto_stop_pending(State)
    end.

-spec clear_auto_stop_pending(guild_state()) -> guild_state().
clear_auto_stop_pending(State) ->
    case maps:get(auto_stop_pending, State, undefined) of
        #{timer_ref := TimerRef} when is_reference(TimerRef) ->
            _ = erlang:cancel_timer(TimerRef),
            maps:remove(auto_stop_pending, State);
        _ ->
            maps:remove(auto_stop_pending, State)
    end.

-spec cleanup_connect_queue(session_id(), guild_state()) -> guild_state().
cleanup_connect_queue(SessionId, State) ->
    Queue0 = maps:get(session_connect_queue, State, undefined),
    case normalize_connect_queue(Queue0) of
        undefined -> State;
        Queue -> State#{session_connect_queue => filter_session_from_queue(SessionId, Queue)}
    end.

-spec filter_session_from_queue(session_id(), queue:queue()) -> queue:queue().
filter_session_from_queue(SessionId, Queue) ->
    queue:filter(
        fun(Item) ->
            queued_session_id(Item) =/= SessionId
        end,
        Queue
    ).

-spec queued_session_id(term()) -> session_id() | undefined.
queued_session_id(#{request := Request}) when is_map(Request) ->
    case maps:get(session_id, Request, undefined) of
        SessionId when is_binary(SessionId) -> SessionId;
        _ -> undefined
    end;
queued_session_id(_) ->
    undefined.

-spec set_auto_stop_timer(guild_state()) -> guild_state().
set_auto_stop_timer(State) ->
    case maps:get(auto_stop_pending, State, undefined) of
        #{token := Token} when is_reference(Token) ->
            State;
        _ ->
            Token = make_ref(),
            TimerRef = erlang:send_after(
                ?AUTO_STOP_EMPTY_GRACE_MS,
                self(),
                {check_auto_stop_empty, Token}
            ),
            State#{
                auto_stop_pending => #{
                    token => Token,
                    timer_ref => TimerRef,
                    started_at => erlang:monotonic_time(millisecond)
                }
            }
    end.
