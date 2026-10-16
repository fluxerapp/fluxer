%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_manager_shard_session_index).
-typing([eqwalizer]).

-export([
    put/3,
    remove/2,
    replace_sessions/2,
    cleanup_down/3,
    ref_index/1,
    build_ref_index/1
]).

-type session_id() :: binary().
-type session_ref() :: {pid(), reference()}.
-type session_map() :: #{session_id() => session_ref()}.
-type ref_index() :: #{reference() => session_id()}.
-type state() :: #{sessions => session_map(), session_refs => ref_index(), term() => term()}.

-export_type([session_id/0, session_ref/0, session_map/0, ref_index/0, state/0]).

-spec put(session_id(), session_ref(), map()) -> map().
put(SessionId, {Pid, Ref} = SessionRef, State) when
    is_binary(SessionId), is_pid(Pid), is_reference(Ref)
->
    Sessions0 = sessions(State),
    Refs0 = remove_existing_ref(SessionId, Sessions0, ref_index(State)),
    State#{
        sessions => Sessions0#{SessionId => SessionRef},
        session_refs => Refs0#{Ref => SessionId}
    }.

-spec remove(session_id(), map()) -> map().
remove(SessionId, State) when is_binary(SessionId) ->
    Sessions0 = sessions(State),
    Refs0 = ref_index(State),
    case maps:take(SessionId, Sessions0) of
        {{_Pid, Ref}, Sessions1} ->
            State#{sessions => Sessions1, session_refs => maps:remove(Ref, Refs0)};
        error ->
            State#{session_refs => Refs0}
    end.

-spec replace_sessions(session_map(), map()) -> map().
replace_sessions(Sessions, State) when is_map(Sessions) ->
    State#{sessions => Sessions, session_refs => build_ref_index(Sessions)}.

-spec cleanup_down(reference(), pid(), map()) -> map().
cleanup_down(Ref, Pid, State) when is_reference(Ref), is_pid(Pid) ->
    Sessions0 = sessions(State),
    Refs0 = ref_index(State),
    case maps:take(Ref, Refs0) of
        {SessionId, Refs1} ->
            cleanup_indexed_down(SessionId, Ref, Pid, Sessions0, Refs1, State);
        error ->
            cleanup_down_fallback(Pid, Sessions0, State)
    end.

-spec cleanup_indexed_down(session_id(), reference(), pid(), session_map(), ref_index(), map()) ->
    map().
cleanup_indexed_down(SessionId, Ref, Pid, Sessions0, Refs1, State) ->
    case maps:take(SessionId, Sessions0) of
        {{Pid, Ref}, Sessions1} ->
            _ = process_registry:cleanup_on_down(session, Pid, #{SessionId => {Pid, Ref}}),
            State#{sessions => Sessions1, session_refs => Refs1};
        {{_OtherPid, OtherRef}, Sessions1} ->
            State#{sessions => Sessions1, session_refs => maps:remove(OtherRef, Refs1)};
        error ->
            State#{session_refs => Refs1}
    end.

-spec cleanup_down_fallback(pid(), session_map(), map()) -> map().
cleanup_down_fallback(DeadPid, Sessions0, State) ->
    Sessions1 = maps:fold(
        fun
            (SessionId, {SessionPid, _Ref}, Acc) when SessionPid =:= DeadPid ->
                ok = process_registry:safe_unregister({session, SessionId}, DeadPid),
                Acc;
            (SessionId, SessionRef, Acc) ->
                Acc#{SessionId => SessionRef}
        end,
        #{},
        Sessions0
    ),
    replace_sessions(Sessions1, State).

-spec ref_index(map()) -> ref_index().
ref_index(State) ->
    case maps:get(session_refs, State, undefined) of
        Refs when is_map(Refs) -> Refs;
        _ -> build_ref_index(sessions(State))
    end.

-spec build_ref_index(session_map()) -> ref_index().
build_ref_index(Sessions) ->
    maps:fold(
        fun
            (SessionId, {_Pid, Ref}, Acc) when is_binary(SessionId), is_reference(Ref) ->
                Acc#{Ref => SessionId};
            (_SessionId, _Value, Acc) ->
                Acc
        end,
        #{},
        Sessions
    ).

-spec sessions(map()) -> session_map().
sessions(State) ->
    case maps:get(sessions, State, #{}) of
        Sessions when is_map(Sessions) -> Sessions;
        _ -> #{}
    end.

-spec remove_existing_ref(session_id(), session_map(), ref_index()) -> ref_index().
remove_existing_ref(SessionId, Sessions, Refs) ->
    case maps:get(SessionId, Sessions, undefined) of
        {_Pid, OldRef} when is_reference(OldRef) -> maps:remove(OldRef, Refs);
        _ -> Refs
    end.
