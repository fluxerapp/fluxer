%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_handoff).
-typing([eqwalizer]).

-export([
    export_handoff_state/1,
    derived_data_keys/0,
    validate_handoff_state/1,
    remonitor_transferred_sessions/1,
    restore_transferred_session_state/1
]).

-type guild_state() :: map().

-export_type([guild_state/0]).

-spec export_handoff_state(guild_state()) -> map().
export_handoff_state(State) ->
    #{
        id => maps:get(id, State),
        data => maps:without(derived_data_keys(), maps:get(data, State, #{})),
        sessions => export_handoff_sessions(maps:get(sessions, State, #{})),
        voice_states => maps:get(voice_states, State, #{}),
        virtual_channel_access => maps:get(virtual_channel_access, State, #{}),
        virtual_channel_access_pending => maps:get(virtual_channel_access_pending, State, #{}),
        virtual_channel_access_preserve => maps:get(
            virtual_channel_access_preserve, State, #{}
        ),
        virtual_channel_access_move_pending =>
            maps:get(virtual_channel_access_move_pending, State, #{})
    }.

-spec derived_data_keys() -> [atom() | binary()].
derived_data_keys() ->
    [
        members_normalized,
        members_sorted_ids,
        member_list_revision,
        members_ets,
        role_perms_cache,
        overwrite_perms_cache,
        <<"role_index">>,
        <<"channel_index">>,
        <<"member_role_index">>
    ].

-spec validate_handoff_state(term()) -> ok | {error, [atom()]}.
validate_handoff_state(Exported) when is_map(Exported) ->
    RequiredKeys = [id, data, sessions, voice_states],
    Missing = [K || K <- RequiredKeys, not maps:is_key(K, Exported)],
    case Missing of
        [] ->
            validate_handoff_types(Exported);
        _ ->
            {error, Missing}
    end;
validate_handoff_state(_) ->
    {error, [not_a_map]}.

-spec validate_handoff_types(map()) -> ok | {error, [atom()]}.
validate_handoff_types(Exported) ->
    Checks = [
        {id, is_integer(maps:get(id, Exported))},
        {data, is_map(maps:get(data, Exported))},
        {sessions, is_map(maps:get(sessions, Exported))},
        {voice_states, is_map(maps:get(voice_states, Exported))}
    ],
    Failures = [K || {K, false} <- Checks],
    case Failures of
        [] -> ok;
        _ -> {error, Failures}
    end.

-spec remonitor_transferred_sessions(map()) -> map().
remonitor_transferred_sessions(State) ->
    Sessions = maps:get(sessions, State, #{}),
    case is_map(Sessions) of
        true -> State#{sessions => remonitor_session_map(Sessions)};
        false -> State#{sessions => #{}}
    end.

-spec restore_transferred_session_state(map()) -> map().
restore_transferred_session_state(State) ->
    Sessions = maps:get(sessions, State, #{}),
    case is_map(Sessions) of
        true ->
            maps:fold(
                fun restore_transferred_session/3,
                State,
                Sessions
            );
        false ->
            State
    end.

-spec export_handoff_sessions(map()) -> map().
export_handoff_sessions(Sessions) when is_map(Sessions) ->
    maps:fold(fun export_single_session/3, #{}, Sessions);
export_handoff_sessions(_) ->
    #{}.

-spec export_single_session(term(), term(), map()) -> map().
export_single_session(SessionId, SessionData, Acc) when is_map(SessionData) ->
    case maps:get(pid, SessionData, undefined) of
        Pid when is_pid(Pid) ->
            Acc#{SessionId => maps:remove(mref, SessionData)};
        _ ->
            Acc
    end;
export_single_session(_SessionId, _SessionData, Acc) ->
    Acc.

-spec remonitor_session_map(map()) -> map().
remonitor_session_map(Sessions) ->
    maps:filtermap(
        fun(_SessionId, SessionData) ->
            remonitor_single_session(SessionData)
        end,
        Sessions
    ).

-spec remonitor_single_session(term()) -> false | {true, map()}.
remonitor_single_session(SessionData) when is_map(SessionData) ->
    case maps:get(pid, SessionData, undefined) of
        Pid when is_pid(Pid) ->
            demonitor_session_ref(SessionData),
            {true, SessionData#{mref => erlang:monitor(process, Pid)}};
        _ ->
            false
    end;
remonitor_single_session(_SessionData) ->
    false.

-spec demonitor_session_ref(map()) -> ok.
demonitor_session_ref(SessionData) ->
    case maps:get(mref, SessionData, undefined) of
        Ref when is_reference(Ref) ->
            erlang:demonitor(Ref, [flush]),
            ok;
        _ ->
            ok
    end.

-spec restore_transferred_session(term(), term(), guild_state()) -> guild_state().
restore_transferred_session(_SessionId, SessionData, State) when is_map(SessionData) ->
    case active_session_user_id(SessionData) of
        UserId when is_integer(UserId) ->
            State1 = add_connected_user(UserId, State),
            guild_sessions_presence:subscribe_connected_user_presence(UserId, State1);
        undefined ->
            State
    end;
restore_transferred_session(_SessionId, _SessionData, State) ->
    State.

-spec active_session_user_id(map()) -> integer() | undefined.
active_session_user_id(SessionData) ->
    case
        {
            guild_sessions_connect:counts_as_connected(SessionData),
            maps:get(user_id, SessionData, undefined),
            maps:get(pid, SessionData, undefined)
        }
    of
        {true, UserId, Pid} when is_integer(UserId), UserId > 0, is_pid(Pid) ->
            UserId;
        _ ->
            undefined
    end.

-spec add_connected_user(integer(), guild_state()) -> guild_state().
add_connected_user(UserId, State) ->
    Counts = maps:get(user_session_counts, State, #{}),
    Connected = maps:get(connected_user_ids, State, sets:new()),
    State#{
        user_session_counts => Counts#{UserId => maps:get(UserId, Counts, 0) + 1},
        connected_user_ids => sets:add_element(UserId, Connected)
    }.
