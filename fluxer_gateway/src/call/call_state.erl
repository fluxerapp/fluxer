%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(call_state).
-typing([eqwalizer]).

-export([
    build_call_event/1,
    caller_from_state/1,
    put_caller/2,
    format_voice_state/1,
    format_voice_states/1,
    integer_list_to_binaries/1,
    find_session_by_pid/2,
    remove_session_entry/2,
    user_has_session/2,
    sync_voice_state_count_diff/2,
    cleanup_voice_state_counts/1
]).

-spec build_call_event(map()) -> map().
build_call_event(State) ->
    #{
        channel_id => integer_to_binary(maps:get(channel_id, State)),
        message_id => integer_to_binary(maps:get(message_id, State)),
        region => maps:get(region, State),
        ringing => integer_list_to_binaries(maps:get(ringing, State)),
        voice_states => format_voice_states(maps:get(voice_states, State))
    }.

-spec caller_from_state(map()) -> map().
caller_from_state(State) ->
    #{
        caller_id => maps:get(caller_id, State, undefined),
        caller_name => maps:get(caller_name, State, undefined),
        caller_avatar => maps:get(caller_avatar, State, undefined)
    }.

-spec put_caller(map(), map()) -> map().
put_caller(#{caller_id := Id, caller_name := Name} = Caller, State) when
    is_integer(Id), is_binary(Name), byte_size(Name) > 0
->
    State#{
        caller_id => Id,
        caller_name => Name,
        caller_avatar => maps:get(caller_avatar, Caller, undefined)
    };
put_caller(_Caller, State) ->
    State.

-spec integer_list_to_binaries([integer()]) -> [binary()].
integer_list_to_binaries(Values) ->
    lists:map(fun integer_to_binary/1, Values).

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

-spec user_has_session(integer(), #{binary() => {integer(), pid(), reference()}}) -> boolean().
user_has_session(UserId, Sessions) ->
    maps:fold(
        fun
            (_SessionId, {U, _Pid, _Ref}, false) -> U =:= UserId;
            (_SessionId, _Entry, true) -> true
        end,
        false,
        Sessions
    ).

-spec find_session_by_pid(pid(), #{binary() => {integer(), pid(), reference()}}) ->
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

-spec remove_session_entry(binary(), #{binary() => {integer(), pid(), reference()}}) ->
    #{binary() => {integer(), pid(), reference()}}.
remove_session_entry(SessionId, Sessions) ->
    case maps:get(SessionId, Sessions, undefined) of
        {_, _, Ref} ->
            demonitor(Ref, [flush]),
            maps:remove(SessionId, Sessions);
        _ ->
            Sessions
    end.

-spec sync_voice_state_count_diff(map(), map()) -> map().
sync_voice_state_count_diff(OldState, NewState) ->
    OldVoiceStates = maps:get(voice_states, OldState, #{}),
    NewVoiceStates = maps:get(voice_states, NewState, #{}),
    case OldVoiceStates =:= NewVoiceStates of
        true ->
            NewState;
        false ->
            _ = sync_replaced_voice_states(OldVoiceStates, NewVoiceStates),
            NewState
    end.

-spec sync_replaced_voice_states(map(), map()) -> ok.
sync_replaced_voice_states(OldVoiceStates, NewVoiceStates) ->
    maps:foreach(
        fun(UserId, OldVoiceState) ->
            sync_replaced_voice_state(UserId, OldVoiceState, NewVoiceStates)
        end,
        OldVoiceStates
    ),
    sync_voice_state_counts(NewVoiceStates).

-spec sync_replaced_voice_state(term(), map(), map()) -> ok.
sync_replaced_voice_state(UserId, OldVoiceState, NewVoiceStates) ->
    case maps:get(UserId, NewVoiceStates, undefined) of
        NewVS when is_map(NewVS) ->
            maybe_remove_replaced(OldVoiceState, NewVS);
        _ ->
            remove_voice_state_count(OldVoiceState)
    end.

-spec maybe_remove_replaced(map(), map()) -> ok.
maybe_remove_replaced(OldVoiceState, NewVoiceState) ->
    OldConnId = voice_state_connection_id(OldVoiceState),
    NewConnId = voice_state_connection_id(NewVoiceState),
    case OldConnId =:= NewConnId of
        true -> ok;
        false -> remove_voice_state_count(OldVoiceState)
    end.

-spec sync_voice_state_counts(map()) -> ok.
sync_voice_state_counts(VoiceStates) ->
    maps:foreach(
        fun(_UserId, VoiceState) ->
            _ = voice_state_counts_cache:upsert_voice_state(VoiceState),
            ok
        end,
        VoiceStates
    ),
    ok.

-spec cleanup_voice_state_counts(map()) -> ok.
cleanup_voice_state_counts(VoiceStates) ->
    maps:foreach(
        fun(_UserId, VoiceState) ->
            remove_voice_state_count(VoiceState)
        end,
        VoiceStates
    ),
    ok.

-spec remove_voice_state_count(term()) -> ok.
remove_voice_state_count(VoiceState) when is_map(VoiceState) ->
    _ = voice_state_counts_cache:remove_connection(
        voice_state_connection_id(VoiceState)
    ),
    ok;
remove_voice_state_count(_) ->
    ok.

-spec voice_state_connection_id(map()) -> term().
voice_state_connection_id(VoiceState) ->
    maps:get(
        <<"connection_id">>,
        VoiceState,
        maps:get(connection_id, VoiceState, undefined)
    ).
