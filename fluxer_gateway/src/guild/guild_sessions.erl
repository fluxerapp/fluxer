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

-module(guild_sessions).

-export([
    handle_session_connect/3,
    handle_session_down/2,
    filter_sessions_for_channel/4,
    filter_sessions_for_manage_channels/4,
    filter_sessions_exclude_session/2,
    handle_user_offline/2,
    set_session_active_guild/3,
    set_session_passive_guild/3,
    build_initial_last_message_ids/1,
    is_session_active/2,
    subscribe_to_user_presence/2,
    unsubscribe_from_user_presence/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-import(guild_permissions, [can_view_channel/4, can_manage_channel/3, find_member_by_user_id/2]).
-import(guild_data, [get_guild_state/2]).
-import(guild_availability, [is_guild_unavailable_for_user/2]).

handle_session_connect(Request, Pid, State) ->
    #{session_id := SessionId, user_id := UserId} = Request,
    Sessions = maps:get(sessions, State, #{}),
    ActiveGuilds = maps:get(active_guilds, Request, sets:new()),
    InitialGuildId = maps:get(initial_guild_id, Request, undefined),
    UserRoles = session_passive:get_user_roles_for_guild(UserId, State),
    Bot = maps:get(bot, Request, false),
    GuildId = maps:get(id, State),

    case maps:is_key(SessionId, Sessions) of
        true ->
            {reply, {ok, get_guild_state(UserId, State)}, State};
        false ->
            Ref = monitor(process, Pid),
            GuildState = get_guild_state(UserId, State),
            InitialLastMessageIds = build_initial_last_message_ids(GuildState),
            SessionData = #{
                session_id => SessionId,
                user_id => UserId,
                pid => Pid,
                mref => Ref,
                active_guilds => ActiveGuilds,
                user_roles => UserRoles,
                bot => Bot,
                previous_passive_updates => InitialLastMessageIds
            },
            NewSessions = maps:put(SessionId, SessionData, Sessions),
            State1 = maps:put(sessions, NewSessions, State),

            State2 = subscribe_to_user_presence(UserId, State1),

            case is_guild_unavailable_for_user(UserId, State2) of
                true ->
                    GuildId = maps:get(id, State2),
                    UnavailableResponse = #{
                        <<"id">> => integer_to_binary(GuildId),
                        <<"unavailable">> => true
                    },
                    {reply, {ok, unavailable, UnavailableResponse}, State2};
                false ->
                    SyncedState = maybe_auto_sync_initial_guild(
                        SessionId,
                        GuildId,
                        InitialGuildId,
                        State2
                    ),
                    {reply, {ok, GuildState}, SyncedState}
            end
    end.

build_initial_last_message_ids(GuildState) ->
    Channels = maps:get(<<"channels">>, GuildState, []),
    lists:foldl(
        fun(Channel, Acc) ->
            ChannelIdBin = maps:get(<<"id">>, Channel, undefined),
            LastMessageId = maps:get(<<"last_message_id">>, Channel, null),
            case {ChannelIdBin, LastMessageId} of
                {undefined, _} -> Acc;
                {_, null} -> Acc;
                _ -> maps:put(ChannelIdBin, LastMessageId, Acc)
            end
        end,
        #{},
        Channels
    ).

handle_session_down(Ref, State) ->
    Sessions = maps:get(sessions, State, #{}),

    DisconnectingSession = maps:fold(
        fun(_K, S, Acc) ->
            case maps:get(mref, S) =:= Ref of
                true -> S;
                false -> Acc
            end
        end,
        undefined,
        Sessions
    ),

    State1 =
        case DisconnectingSession of
            undefined ->
                State;
            Session ->
                UserId = maps:get(user_id, Session),
                SessionId = maps:get(session_id, Session),
                StateAfterPresence = unsubscribe_from_user_presence(UserId, State),
                StateAfterMemberList = guild_member_list:unsubscribe_session(
                    SessionId, StateAfterPresence
                ),
                MemberSubs = maps:get(
                    member_subscriptions, StateAfterMemberList, guild_subscriptions:init_state()
                ),
                NewMemberSubs = guild_subscriptions:unsubscribe_session(SessionId, MemberSubs),
                maps:put(member_subscriptions, NewMemberSubs, StateAfterMemberList)
        end,

    NewSessions = maps:filter(fun(_K, S) -> maps:get(mref, S) =/= Ref end, Sessions),
    NewState = maps:put(sessions, NewSessions, State1),

    case map_size(NewSessions) of
        0 ->
            {stop, normal, NewState};
        _ ->
            {noreply, NewState}
    end.

filter_sessions_for_channel(Sessions, ChannelId, SessionIdOpt, State) ->
    GuildId = maps:get(id, State, 0),
    lists:filter(
        fun({Sid, S}) ->
            UserId = maps:get(user_id, S),
            Member = find_member_by_user_id(UserId, State),

            ExcludeSession =
                case SessionIdOpt of
                    undefined -> false;
                    SessionId -> Sid =:= SessionId
                end,

            case {ExcludeSession, Member} of
                {true, _} ->
                    false;
                {_, undefined} ->
                    logger:warning(
                        "[guild_sessions] Filtering out session with no member: "
                        "guild_id=~p session_id=~p user_id=~p",
                        [GuildId, Sid, UserId]
                    ),
                    false;
                {false, _} ->
                    can_view_channel(UserId, ChannelId, Member, State)
            end
        end,
        maps:to_list(Sessions)
    ).

filter_sessions_for_manage_channels(Sessions, ChannelId, SessionIdOpt, State) ->
    lists:filter(
        fun({Sid, S}) ->
            UserId = maps:get(user_id, S),

            ExcludeSession =
                case SessionIdOpt of
                    undefined -> false;
                    SessionId -> Sid =:= SessionId
                end,

            case ExcludeSession of
                true ->
                    false;
                false ->
                    can_manage_channel(UserId, ChannelId, State)
            end
        end,
        maps:to_list(Sessions)
    ).

filter_sessions_exclude_session(Sessions, SessionIdOpt) ->
    case SessionIdOpt of
        undefined ->
            maps:to_list(Sessions);
        SessionId ->
            [{Sid, S} || {Sid, S} <- maps:to_list(Sessions), Sid =/= SessionId]
    end.

subscribe_to_user_presence(UserId, State) ->
    PresenceSubs = maps:get(presence_subscriptions, State, #{}),
    CurrentCount = maps:get(UserId, PresenceSubs, 0),
    case CurrentCount of
        0 ->
            presence_bus:subscribe(UserId),
            NewSubs = maps:put(UserId, 1, PresenceSubs),
            StateWithSubs = maps:put(presence_subscriptions, NewSubs, State),
            maybe_send_cached_presence(UserId, StateWithSubs);
        _ ->
            NewSubs = maps:put(UserId, CurrentCount + 1, PresenceSubs),
            maps:put(presence_subscriptions, NewSubs, State)
    end.

unsubscribe_from_user_presence(UserId, State) ->
    PresenceSubs = maps:get(presence_subscriptions, State, #{}),
    CurrentCount = maps:get(UserId, PresenceSubs, 0),
    case CurrentCount of
        0 ->
            State;
        1 ->
            NewSubs = maps:put(UserId, 0, PresenceSubs),
            maps:put(presence_subscriptions, NewSubs, State);
        _ ->
            NewSubs = maps:put(UserId, CurrentCount - 1, PresenceSubs),
            maps:put(presence_subscriptions, NewSubs, State)
    end.

handle_user_offline(UserId, State) ->
    PresenceSubs = maps:get(presence_subscriptions, State, #{}),
    case maps:get(UserId, PresenceSubs, undefined) of
        0 ->
            presence_bus:unsubscribe(UserId),
            NewSubs = maps:remove(UserId, PresenceSubs),
            maps:put(presence_subscriptions, NewSubs, State);
        undefined ->
            State;
        _ ->
            State
    end.

maybe_send_cached_presence(UserId, State) ->
    case presence_cache:get(UserId) of
        {ok, Payload} ->
            case guild_presence:handle_bus_presence(UserId, Payload, State) of
                {noreply, UpdatedState} ->
                    UpdatedState
            end;
        _ ->
            State
    end.

set_session_active_guild(SessionId, GuildId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            State;
        SessionData ->
            NewSessionData = session_passive:set_active(GuildId, SessionData),
            NewSessions = maps:put(SessionId, NewSessionData, Sessions),
            maps:put(sessions, NewSessions, State)
    end.

set_session_passive_guild(SessionId, GuildId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            State;
        SessionData ->
            NewSessionData = session_passive:set_passive(GuildId, SessionData),
            NewSessionData2 = session_passive:clear_guild_synced(GuildId, NewSessionData),
            NewSessions = maps:put(SessionId, NewSessionData2, Sessions),
            maps:put(sessions, NewSessions, State)
    end.

is_session_active(SessionId, State) ->
    GuildId = maps:get(id, State, 0),
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            false;
        SessionData ->
            not session_passive:is_passive(GuildId, SessionData)
    end.

maybe_auto_sync_initial_guild(SessionId, GuildId, InitialGuildId, State) ->
    case InitialGuildId of
        GuildId ->
            Sessions = maps:get(sessions, State, #{}),
            case maps:get(SessionId, Sessions, undefined) of
                undefined ->
                    State;
                SessionData ->
                    SyncedSessionData = session_passive:mark_guild_synced(GuildId, SessionData),
                    NewSessions = maps:put(SessionId, SyncedSessionData, Sessions),
                    maps:put(sessions, NewSessions, State)
            end;
        _ ->
            State
    end.

-ifdef(TEST).

build_initial_last_message_ids_empty_channels_test() ->
    GuildState = #{<<"channels">> => []},
    Result = build_initial_last_message_ids(GuildState),
    ?assertEqual(#{}, Result),
    ok.

build_initial_last_message_ids_with_channels_test() ->
    GuildState = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"last_message_id">> => <<"500">>},
            #{<<"id">> => <<"101">>, <<"last_message_id">> => <<"600">>}
        ]
    },
    Result = build_initial_last_message_ids(GuildState),
    ?assertEqual(#{<<"100">> => <<"500">>, <<"101">> => <<"600">>}, Result),
    ok.

build_initial_last_message_ids_filters_null_test() ->
    GuildState = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"last_message_id">> => <<"500">>},
            #{<<"id">> => <<"101">>, <<"last_message_id">> => null},
            #{<<"id">> => <<"102">>}
        ]
    },
    Result = build_initial_last_message_ids(GuildState),
    ?assertEqual(#{<<"100">> => <<"500">>}, Result),
    ok.

build_initial_last_message_ids_no_channels_key_test() ->
    GuildState = #{},
    Result = build_initial_last_message_ids(GuildState),
    ?assertEqual(#{}, Result),
    ok.

-endif.
