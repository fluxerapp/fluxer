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

-module(presence_session).

-export([
    handle_session_connect/3,
    handle_presence_update/2,
    dispatch_sessions_replace/1,
    notify_sessions_guild_join/2,
    notify_sessions_guild_leave/2,
    find_session_by_ref/2
]).

handle_session_connect(Request, Pid, State) ->
    #{session_id := SessionId, status := Status} = Request,
    Afk = maps:get(afk, Request, false),
    Mobile = maps:get(mobile, Request, false),
    SocketPid = maps:get(socket_pid, Request, undefined),
    Sessions = maps:get(sessions, State),

    case maps:is_key(SessionId, Sessions) of
        true ->
            SessionsData = presence_status:collect_sessions_for_replace(Sessions),
            {reply, {ok, SessionsData}, State};
        false ->
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
            NewSessions = maps:put(SessionId, SessionEntry, Sessions),
            NewState = maps:put(sessions, NewSessions, State),

            SessionsData = presence_status:collect_sessions_for_replace(NewSessions),
            {reply, {ok, SessionsData}, NewState}
    end.

handle_presence_update(Request, State) ->
    #{session_id := SessionId, status := Status} = Request,
    Afk = maps:get(afk, Request, false),
    Sessions = maps:get(sessions, State),

    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            {noreply, State};
        Session ->
            UpdatedSession = Session#{status => Status, afk => Afk},
            NewSessions = maps:put(SessionId, UpdatedSession, Sessions),
            NewState = maps:put(sessions, NewSessions, State),
            dispatch_sessions_replace(NewState),
            {noreply, NewState}
    end.

dispatch_sessions_replace(State) ->
    Sessions = maps:get(sessions, State),
    SessionsData = presence_status:collect_sessions_for_replace(Sessions),
    SessionPids = [maps:get(pid, S) || S <- maps:values(Sessions)],

    lists:foreach(
        fun(Pid) when is_pid(Pid) ->
            gen_server:cast(Pid, {dispatch, sessions_replace, SessionsData})
        end,
        SessionPids
    ).

notify_sessions_guild_join(GuildId, State) ->
    Sessions = maps:get(sessions, State),
    SessionPids = [maps:get(pid, S) || S <- maps:values(Sessions)],
    lists:foreach(
        fun(Pid) when is_pid(Pid) ->
            gen_server:cast(Pid, {guild_join, GuildId})
        end,
        SessionPids
    ).

notify_sessions_guild_leave(GuildId, State) ->
    Sessions = maps:get(sessions, State),
    SessionPids = [maps:get(pid, S) || S <- maps:values(Sessions)],
    lists:foreach(
        fun(Pid) when is_pid(Pid) ->
            gen_server:cast(Pid, {guild_leave, GuildId})
        end,
        SessionPids
    ).

find_session_by_ref(Ref, Sessions) ->
    maps:fold(
        fun(SessionId, S, Acc) ->
            case maps:get(mref, S) of
                Ref -> {ok, SessionId};
                _ -> Acc
            end
        end,
        not_found,
        Sessions
    ).
