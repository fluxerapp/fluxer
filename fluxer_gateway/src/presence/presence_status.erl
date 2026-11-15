%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_status).
-typing([eqwalizer]).

-export([
    get_current_status/1,
    get_flattened_mobile/1,
    get_flattened_afk/1,
    collect_sessions_for_replace/1,
    effective_sessions/1
]).

-export_type([status/0, sessions/0]).

-type session_id() :: binary().
-type status() :: online | offline | idle | dnd | invisible.
-type session_entry() :: #{status := status(), afk := boolean(), mobile := boolean(), _ => _}.
-type sessions() :: #{session_id() => session_entry()}.

-spec effective_sessions(#{sessions := sessions(), _ => _}) -> sessions().
effective_sessions(#{sessions := Sessions} = State) ->
    case maps:get(settings_invisible, State, false) of
        true -> maps:map(fun(_SessionId, Session) -> force_invisible(Session) end, Sessions);
        false -> Sessions
    end.

-spec force_invisible(session_entry()) -> session_entry().
force_invisible(#{status := offline} = Session) ->
    Session;
force_invisible(Session) ->
    Session#{status => invisible}.

-spec get_current_status(sessions()) -> status().
get_current_status(Sessions) ->
    AllStatuses = [maps:get(status, S) || S <- maps:values(Sessions)],
    resolve_status_precedence(AllStatuses).

-spec resolve_status_precedence([status()]) -> status().
resolve_status_precedence(AllStatuses) ->
    StatusPrecedence = [dnd, online, idle, invisible],
    lists:foldl(
        fun(Status, Acc) ->
            promote_status(Status, Acc, AllStatuses)
        end,
        offline,
        StatusPrecedence
    ).

-spec promote_status(status(), status(), [status()]) -> status().
promote_status(Status, offline, AllStatuses) ->
    case lists:member(Status, AllStatuses) of
        true -> Status;
        false -> offline
    end;
promote_status(_Status, Acc, _AllStatuses) ->
    Acc.

-spec get_flattened_mobile(sessions()) -> boolean().
get_flattened_mobile(Sessions) ->
    get_current_status(Sessions) =:= online andalso
        lists:any(fun is_online_mobile_session/1, maps:values(Sessions)).

-spec is_online_mobile_session(map()) -> boolean().
is_online_mobile_session(Session) ->
    maps:get(status, Session, offline) =:= online andalso
        maps:get(mobile, Session, false).

-spec get_flattened_afk(sessions()) -> boolean().
get_flattened_afk(Sessions) ->
    HasMobile = get_flattened_mobile(Sessions),
    case HasMobile of
        true -> false;
        false -> all_sessions_afk(Sessions)
    end.

-spec all_sessions_afk(sessions()) -> boolean().
all_sessions_afk(Sessions) ->
    case maps:size(Sessions) of
        0 -> false;
        _ -> lists:all(fun is_session_afk/1, maps:values(Sessions))
    end.

-spec is_session_afk(map()) -> boolean().
is_session_afk(Session) ->
    maps:get(afk, Session, false).

-spec collect_sessions_for_replace(sessions()) -> [map()].
collect_sessions_for_replace(Sessions) ->
    Status = get_current_status(Sessions),
    Mobile = get_flattened_mobile(Sessions),
    Afk = get_flattened_afk(Sessions),
    BaseSessions = [
        #{
            <<"session_id">> => <<"all">>,
            <<"status">> => constants:status_type_atom(Status),
            <<"mobile">> => Mobile,
            <<"afk">> => Afk
        }
    ],
    SessionEntries = maps:fold(
        fun(SessionId, Session, Acc) ->
            [
                #{
                    <<"session_id">> => SessionId,
                    <<"status">> => constants:status_type_atom(maps:get(status, Session)),
                    <<"afk">> => maps:get(afk, Session, false),
                    <<"mobile">> => maps:get(mobile, Session, false)
                }
                | Acc
            ]
        end,
        [],
        Sessions
    ),
    BaseSessions ++ SessionEntries.
