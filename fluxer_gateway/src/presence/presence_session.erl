%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_session).
-typing([eqwalizer]).

-export([
    handle_session_connect/3,
    handle_presence_update/2,
    dispatch_sessions_replace/1,
    notify_sessions_guild_join/2,
    notify_sessions_guild_leave/2,
    notify_sessions/2,
    find_session_by_ref/2
]).

-export_type([session_id/0, state/0, connect_request/0, update_request/0, session_ref_map/0]).

-type session_id() :: binary().
-type status() :: online | offline | idle | dnd | invisible.
-type session_entry() :: #{
    session_id := session_id(),
    status := status(),
    afk := boolean(),
    mobile := boolean(),
    pid := pid(),
    mref := reference(),
    socket_pid := pid() | undefined
}.
-type sessions() :: #{session_id() => session_entry()}.
-type state() :: #{sessions := sessions(), _ => _}.
-type connect_request() :: map().
-type update_request() :: map().
-type session_ref_map() :: #{session_id() => map()}.

-spec handle_session_connect(connect_request(), pid(), state()) ->
    {reply, {ok, [map()]}, state()}.
handle_session_connect(Request, Pid, State) ->
    #{session_id := SessionId0} = Request,
    SessionId = normalize_session_id(SessionId0),
    Status = normalize_status(maps:get(status, Request, offline)),
    Afk = normalize_boolean(maps:get(afk, Request, false)),
    Mobile = normalize_boolean(maps:get(mobile, Request, false)),
    SocketPid = normalize_socket_pid(maps:get(socket_pid, Request, undefined)),
    Sessions = maps:get(sessions, State),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            Ref = monitor(process, Pid),
            SessionEntry = #{
                session_id => SessionId,
                status => Status,
                afk => Afk,
                mobile => Mobile,
                pid => Pid,
                mref => Ref,
                socket_pid => SocketPid
            },
            NewSessions = Sessions#{SessionId => SessionEntry},
            NewState = State#{sessions => NewSessions},
            SessionsData = presence_status:collect_sessions_for_replace(
                presence_status:effective_sessions(NewState)
            ),
            {reply, {ok, SessionsData}, NewState};
        Existing ->
            {UpdatedSession, NewState0} = refresh_existing_session(
                Existing, Pid, Status, Afk, Mobile, SocketPid, State
            ),
            NewSessions = Sessions#{SessionId => UpdatedSession},
            NewState = NewState0#{sessions => NewSessions},
            SessionsData = presence_status:collect_sessions_for_replace(
                presence_status:effective_sessions(NewState)
            ),
            {reply, {ok, SessionsData}, NewState}
    end.

-spec refresh_existing_session(
    session_entry(), pid(), status(), boolean(), boolean(), pid() | undefined, state()
) -> {session_entry(), state()}.
refresh_existing_session(Existing, Pid, Status, Afk, Mobile, SocketPid, State) ->
    {Ref, State1} = refresh_monitor(Existing, Pid, State),
    {
        Existing#{
            status => Status,
            afk => Afk,
            mobile => Mobile,
            pid => Pid,
            mref => Ref,
            socket_pid => SocketPid
        },
        State1
    }.

-spec refresh_monitor(session_entry(), pid(), state()) -> {reference(), state()}.
refresh_monitor(#{pid := Pid, mref := Ref}, Pid, State) when is_reference(Ref) ->
    {Ref, State};
refresh_monitor(Existing, Pid, State) ->
    case maps:get(mref, Existing, undefined) of
        Ref when is_reference(Ref) -> erlang:demonitor(Ref, [flush]);
        _ -> ok
    end,
    {monitor(process, Pid), State}.

-spec handle_presence_update(update_request(), state()) -> {noreply, state()}.
handle_presence_update(Request, State) ->
    #{session_id := SessionId0} = Request,
    SessionId = normalize_session_id(SessionId0),
    Status = normalize_status(maps:get(status, Request, offline)),
    Sessions = maps:get(sessions, State),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            {noreply, State};
        Session ->
            Afk = normalize_boolean(maps:get(afk, Request, maps:get(afk, Session, false))),
            Mobile = normalize_boolean(
                maps:get(mobile, Request, maps:get(mobile, Session, false))
            ),
            handle_session_presence_change(
                SessionId, Session, Status, Afk, Mobile, Sessions, State
            )
    end.

-spec handle_session_presence_change(
    session_id(),
    session_entry(),
    status(),
    boolean(),
    boolean(),
    sessions(),
    state()
) -> {noreply, state()}.
handle_session_presence_change(SessionId, Session, Status, Afk, Mobile, Sessions, State) ->
    case session_presence_changed(Session, Status, Afk, Mobile) of
        false ->
            {noreply, State};
        true ->
            UpdatedSession = Session#{status => Status, afk => Afk, mobile => Mobile},
            NewSessions = Sessions#{SessionId => UpdatedSession},
            NewState = State#{sessions => NewSessions},
            dispatch_sessions_replace(NewState),
            {noreply, NewState}
    end.

-spec session_presence_changed(session_entry(), status(), boolean(), boolean()) -> boolean().
session_presence_changed(Session, Status, Afk, Mobile) ->
    maps:get(status, Session) =/= Status orelse
        maps:get(afk, Session, false) =/= Afk orelse
        maps:get(mobile, Session, false) =/= Mobile.

-spec dispatch_sessions_replace(state()) -> ok.
dispatch_sessions_replace(State) ->
    Sessions = maps:get(sessions, State),
    SessionsData = presence_status:collect_sessions_for_replace(
        presence_status:effective_sessions(State)
    ),
    SessionPids = [maps:get(pid, S) || S <- maps:values(Sessions)],
    gateway_dispatch_relay:dispatch_many(
        [Pid || Pid <- SessionPids, is_pid(Pid)], sessions_replace, SessionsData, 0
    ),
    ok.

-spec notify_sessions_guild_join(integer(), state()) -> ok.
notify_sessions_guild_join(GuildId, State) ->
    notify_sessions({guild_join, GuildId}, State).

-spec notify_sessions_guild_leave(integer(), state()) -> ok.
notify_sessions_guild_leave(GuildId, State) ->
    notify_sessions({guild_leave, GuildId}, State).

-spec notify_sessions(term(), state()) -> ok.
notify_sessions(Msg, State) ->
    Sessions = maps:get(sessions, State),
    lists:foreach(
        fun
            (#{pid := Pid}) when is_pid(Pid) -> _ = shard_utils:safe_cast(Pid, Msg);
            (_) -> ok
        end,
        maps:values(Sessions)
    ).

-spec find_session_by_ref(reference(), session_ref_map()) -> {ok, session_id()} | not_found.
find_session_by_ref(Ref, Sessions) ->
    maps:fold(
        fun
            (SessionId, #{mref := MRef}, _) when MRef =:= Ref -> {ok, SessionId};
            (_, _, Acc) -> Acc
        end,
        not_found,
        Sessions
    ).

-spec normalize_session_id(term()) -> session_id().
normalize_session_id(SessionId) when is_binary(SessionId) ->
    SessionId;
normalize_session_id(SessionId) ->
    case type_conv:to_binary(SessionId) of
        Bin when is_binary(Bin) -> Bin;
        undefined -> <<>>
    end.

-spec normalize_status(term()) -> status().
normalize_status(online) -> online;
normalize_status(offline) -> offline;
normalize_status(idle) -> idle;
normalize_status(dnd) -> dnd;
normalize_status(invisible) -> invisible;
normalize_status(_) -> offline.

-spec normalize_boolean(term()) -> boolean().
normalize_boolean(true) -> true;
normalize_boolean(_) -> false.

-spec normalize_socket_pid(term()) -> pid() | undefined.
normalize_socket_pid(Pid) when is_pid(Pid) -> Pid;
normalize_socket_pid(_) -> undefined.
