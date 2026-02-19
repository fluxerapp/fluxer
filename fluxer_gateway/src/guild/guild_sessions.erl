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
    remove_session/2,
    filter_sessions_for_channel/4,
    filter_sessions_for_message/5,
    filter_sessions_for_manage_channels/4,
    filter_sessions_exclude_session/2,
    handle_user_offline/2,
    set_session_active_guild/3,
    set_session_passive_guild/3,
    build_initial_last_message_ids/1,
    is_session_active/2,
    subscribe_to_user_presence/2,
    unsubscribe_from_user_presence/2,
    set_session_viewable_channels/3,
    refresh_all_viewable_channels/1
]).

-type guild_state() :: map().
-type session_id() :: binary().
-type user_id() :: integer().
-type guild_id() :: integer().
-type channel_id() :: integer().
-type session_data() :: map().
-type sessions_map() :: #{session_id() => session_data()}.
-type session_pair() :: {session_id(), session_data()}.

-spec handle_session_connect(map(), pid(), guild_state()) ->
    {reply, {ok, map()} | {ok, unavailable, map()}, guild_state()}.
handle_session_connect(Request, Pid, State) ->
    #{session_id := SessionId, user_id := UserId} = Request,
    Sessions = maps:get(sessions, State, #{}),
    case maps:is_key(SessionId, Sessions) of
        true ->
            {reply, {ok, guild_data:get_guild_state(UserId, State)}, State};
        false ->
            register_new_session(Request, Pid, UserId, SessionId, State)
    end.

-spec register_new_session(map(), pid(), user_id(), session_id(), guild_state()) ->
    {reply, {ok, map()} | {ok, unavailable, map()}, guild_state()}.
register_new_session(Request, Pid, UserId, SessionId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    ActiveGuilds = maps:get(active_guilds, Request, sets:new()),
    InitialGuildId = maps:get(initial_guild_id, Request, undefined),
    UserRoles = session_passive:get_user_roles_for_guild(UserId, State),
    Bot = maps:get(bot, Request, false),
    GuildId = maps:get(id, State),
    Ref = monitor(process, Pid),
    GuildState = guild_data:get_guild_state(UserId, State),
    InitialLastMessageIds = build_initial_last_message_ids(GuildState),
    InitialChannelVersions = build_initial_channel_versions(GuildState),
    InitialViewableChannels = build_viewable_channel_map(
        guild_visibility:get_user_viewable_channels(UserId, State)
    ),
    SessionData = #{
        session_id => SessionId,
        user_id => UserId,
        pid => Pid,
        mref => Ref,
        active_guilds => ActiveGuilds,
        user_roles => UserRoles,
        bot => Bot,
        is_staff => maps:get(is_staff, Request, false),
        viewable_channels => InitialViewableChannels
    },
    InitialPassiveState = #{
        previous_passive_updates => InitialLastMessageIds,
        previous_passive_channel_versions => InitialChannelVersions,
        previous_passive_voice_states => #{}
    },
    passive_sync_registry:store(SessionId, GuildId, InitialPassiveState),
    NewSessions = maps:put(SessionId, SessionData, Sessions),
    State1 = maps:put(sessions, NewSessions, State),
    State2 = subscribe_to_user_presence(UserId, State1),
    _ = maybe_notify_coordinator(session_connected, SessionId, UserId, State2),
    case guild_availability:is_guild_unavailable_for_user(UserId, State2) of
        true ->
            UnavailableResponse = #{
                <<"id">> => integer_to_binary(GuildId),
                <<"unavailable">> => true
            },
            {reply, {ok, unavailable, UnavailableResponse}, State2};
        false ->
            SyncedState = maybe_auto_sync_initial_guild(SessionId, GuildId, InitialGuildId, State2),
            {reply, {ok, GuildState}, SyncedState}
    end.

-spec build_initial_last_message_ids(map()) -> #{binary() => binary()}.
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

-spec build_initial_channel_versions(map()) -> #{binary() => integer()}.
build_initial_channel_versions(GuildState) ->
    Channels = maps:get(<<"channels">>, GuildState, []),
    lists:foldl(
        fun(Channel, Acc) ->
            ChannelIdBin = maps:get(<<"id">>, Channel, undefined),
            case ChannelIdBin of
                undefined ->
                    Acc;
                _ ->
                    Version = map_utils:get_integer(Channel, <<"version">>, 0),
                    maps:put(ChannelIdBin, Version, Acc)
            end
        end,
        #{},
        Channels
    ).

-spec handle_session_down(reference(), guild_state()) ->
    {noreply, guild_state()} | {stop, normal, guild_state()}.
handle_session_down(Ref, State) ->
    Sessions = maps:get(sessions, State, #{}),
    DisconnectingSession = find_session_by_ref(Ref, Sessions),
    State1 = cleanup_disconnecting_session(DisconnectingSession, State),
    NewSessions = maps:filter(fun(_K, S) -> maps:get(mref, S) =/= Ref end, Sessions),
    NewState = maps:put(sessions, NewSessions, State1),
    case map_size(NewSessions) of
        0 ->
            case should_auto_stop_on_empty(NewState) of
                true -> {stop, normal, NewState};
                false -> {noreply, NewState}
            end;
        _ -> {noreply, NewState}
    end.

-spec remove_session(session_id(), guild_state()) -> guild_state().
remove_session(SessionId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            State;
        Session ->
            maybe_demonitor_session(Session),
            StateAfterCleanup = cleanup_disconnecting_session(Session, State),
            SessionsAfterCleanup = maps:get(sessions, StateAfterCleanup, #{}),
            NewSessions = maps:remove(SessionId, SessionsAfterCleanup),
            maps:put(sessions, NewSessions, StateAfterCleanup)
    end.

-spec maybe_demonitor_session(session_data()) -> ok.
maybe_demonitor_session(Session) ->
    Ref = maps:get(mref, Session, undefined),
    case is_reference(Ref) of
        true ->
            demonitor(Ref, [flush]),
            ok;
        false ->
            ok
    end.

-spec find_session_by_ref(reference(), sessions_map()) -> session_data() | undefined.
find_session_by_ref(Ref, Sessions) ->
    maps:fold(
        fun(_K, S, Acc) ->
            case maps:get(mref, S) =:= Ref of
                true -> S;
                false -> Acc
            end
        end,
        undefined,
        Sessions
    ).

-spec cleanup_disconnecting_session(session_data() | undefined, guild_state()) -> guild_state().
cleanup_disconnecting_session(undefined, State) ->
    State;
cleanup_disconnecting_session(Session, State) ->
    UserId = maps:get(user_id, Session),
    SessionId = maps:get(session_id, Session),
    GuildId = maps:get(id, State),
    _ = maybe_notify_coordinator(session_disconnected, SessionId, UserId, State),
    passive_sync_registry:delete(SessionId, GuildId),
    StateAfterPresence = unsubscribe_from_user_presence(UserId, State),
    StateAfterMemberList = guild_member_list:unsubscribe_session(SessionId, StateAfterPresence),
    MemberSubs = maps:get(
        member_subscriptions, StateAfterMemberList, guild_subscriptions:init_state()
    ),
    NewMemberSubs = guild_subscriptions:unsubscribe_session(SessionId, MemberSubs),
    StateAfterSubs = maps:put(member_subscriptions, NewMemberSubs, StateAfterMemberList),
    cleanup_connect_admission_for_session(SessionId, StateAfterSubs).

-spec cleanup_connect_admission_for_session(session_id(), guild_state()) -> guild_state().
cleanup_connect_admission_for_session(SessionId, State) ->
    Pending0 = maps:get(session_connect_pending, State, undefined),
    State1 =
        case Pending0 of
            Pending when is_map(Pending) ->
                maps:put(session_connect_pending, maps:remove(SessionId, Pending), State);
            _ ->
                State
        end,
    Queue0 = maps:get(session_connect_queue, State1, undefined),
    Queue = normalize_connect_queue(Queue0),
    case queue:is_queue(Queue) of
        true ->
            Filtered = queue:filter(
                fun(Item) ->
                    Request = maps:get(request, Item, #{}),
                    maps:get(session_id, Request, undefined) =/= SessionId
                end,
                Queue
            ),
            maps:put(session_connect_queue, Filtered, State1);
        false ->
            State1
    end.

-spec normalize_connect_queue(term()) -> queue:queue() | undefined.
normalize_connect_queue(Value) when is_list(Value) ->
    queue:from_list(Value);
normalize_connect_queue(Value) ->
    case queue:is_queue(Value) of
        true -> Value;
        false -> undefined
    end.

-spec should_auto_stop_on_empty(guild_state()) -> boolean().
should_auto_stop_on_empty(State) ->
    case maps:get(disable_auto_stop_on_empty, State, false) of
        true ->
            false;
        false ->
            case maps:get(very_large_guild_coordinator_pid, State, undefined) of
                Pid when is_pid(Pid) -> false;
                _ -> true
            end
    end.

-spec maybe_notify_coordinator(session_connected | session_disconnected, session_id(), user_id(), guild_state()) ->
    ok.
maybe_notify_coordinator(Type, SessionId, UserId, State) ->
    case {maps:get(very_large_guild_coordinator_pid, State, undefined),
        maps:get(very_large_guild_shard_index, State, undefined)}
    of
        {CoordPid, ShardIndex} when is_pid(CoordPid), is_integer(ShardIndex) ->
            Msg =
                case Type of
                    session_connected ->
                        {very_large_guild_session_connected, ShardIndex, SessionId, UserId};
                    session_disconnected ->
                        {very_large_guild_session_disconnected, ShardIndex, SessionId, UserId}
                end,
            CoordPid ! Msg,
            ok;
        _ ->
            ok
    end.

-spec filter_sessions_for_channel(
    sessions_map(), channel_id(), session_id() | undefined, guild_state()
) ->
    [session_pair()].
filter_sessions_for_channel(Sessions, ChannelId, SessionIdOpt, State) ->
    lists:filter(
        fun({Sid, S}) ->
            case maps:get(pending_connect, S, false) of
                true ->
                    false;
                false ->
            ExcludeSession = should_exclude_session(Sid, SessionIdOpt),
            case ExcludeSession of
                true ->
                    false;
                false ->
                    session_can_view_channel(S, ChannelId, State)
            end
            end
        end,
        maps:to_list(Sessions)
    ).

-spec filter_sessions_for_message(
    sessions_map(), channel_id(), binary(), session_id() | undefined, guild_state()
) ->
    [session_pair()].
filter_sessions_for_message(Sessions, ChannelId, MessageId, SessionIdOpt, State) ->
    lists:filter(
        fun({Sid, S}) ->
            case maps:get(pending_connect, S, false) of
                true ->
                    false;
                false ->
            ExcludeSession = should_exclude_session(Sid, SessionIdOpt),
            case ExcludeSession of
                true ->
                    false;
                false ->
                    UserId = maps:get(user_id, S, undefined),
                    case session_can_view_channel(S, ChannelId, State) of
                        false ->
                            false;
                        true ->
                            Perms = guild_permissions:get_member_permissions(UserId, ChannelId, State),
                            guild_permissions:can_access_message_by_permissions(
                                Perms, MessageId, State
                            )
                    end
            end
            end
        end,
        maps:to_list(Sessions)
    ).

-spec filter_sessions_for_manage_channels(
    sessions_map(), channel_id(), session_id() | undefined, guild_state()
) ->
    [session_pair()].
filter_sessions_for_manage_channels(Sessions, ChannelId, SessionIdOpt, State) ->
    lists:filter(
        fun({Sid, S}) ->
            case maps:get(pending_connect, S, false) of
                true ->
                    false;
                false ->
            UserId = maps:get(user_id, S),
            ExcludeSession = should_exclude_session(Sid, SessionIdOpt),
            case ExcludeSession of
                true -> false;
                false -> guild_permissions:can_manage_channel(UserId, ChannelId, State)
            end
            end
        end,
        maps:to_list(Sessions)
    ).

-spec filter_sessions_exclude_session(sessions_map(), session_id() | undefined) -> [session_pair()].
filter_sessions_exclude_session(Sessions, undefined) ->
    [{Sid, S} || {Sid, S} <- maps:to_list(Sessions), maps:get(pending_connect, S, false) =/= true];
filter_sessions_exclude_session(Sessions, SessionId) ->
    [
        {Sid, S}
     || {Sid, S} <- maps:to_list(Sessions),
        Sid =/= SessionId,
        maps:get(pending_connect, S, false) =/= true
    ].

-spec should_exclude_session(session_id(), session_id() | undefined) -> boolean().
should_exclude_session(_, undefined) -> false;
should_exclude_session(Sid, SessionId) -> Sid =:= SessionId.

-spec set_session_viewable_channels(session_id(), map(), guild_state()) -> guild_state().
set_session_viewable_channels(SessionId, ViewableChannels, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            State;
        SessionData ->
            NewSessionData = maps:put(viewable_channels, ViewableChannels, SessionData),
            NewSessions = maps:put(SessionId, NewSessionData, Sessions),
            maps:put(sessions, NewSessions, State)
    end.

-spec refresh_all_viewable_channels(guild_state()) -> guild_state().
refresh_all_viewable_channels(State) ->
    Sessions = maps:get(sessions, State, #{}),
    lists:foldl(
        fun({SessionId, SessionData}, AccState) ->
            UserId = maps:get(user_id, SessionData, undefined),
            case is_integer(UserId) of
                true ->
                    ViewableChannels = build_viewable_channel_map(
                        guild_visibility:get_user_viewable_channels(UserId, AccState)
                    ),
                    set_session_viewable_channels(SessionId, ViewableChannels, AccState);
                false ->
                    AccState
            end
        end,
        State,
        maps:to_list(Sessions)
    ).

-spec session_can_view_channel(session_data(), channel_id(), guild_state()) -> boolean().
session_can_view_channel(SessionData, ChannelId, State) ->
    UserId = maps:get(user_id, SessionData, undefined),
    case {UserId, maps:get(viewable_channels, SessionData, undefined)} of
        {Uid, ViewableChannels} when is_integer(Uid), is_map(ViewableChannels) ->
            case maps:is_key(ChannelId, ViewableChannels) of
                true ->
                    true;
                false ->
                    check_member_channel_access(Uid, ChannelId, State)
            end;
        {Uid, _} when is_integer(Uid) ->
            check_member_channel_access(Uid, ChannelId, State);
        _ ->
            false
    end.

-spec check_member_channel_access(user_id(), channel_id(), guild_state()) -> boolean().
check_member_channel_access(UserId, ChannelId, State) ->
    Member = guild_permissions:find_member_by_user_id(UserId, State),
    case Member of
        undefined ->
            false;
        _ ->
            guild_permissions:can_view_channel(UserId, ChannelId, Member, State)
    end.

-spec build_viewable_channel_map([channel_id()]) -> #{channel_id() => true}.
build_viewable_channel_map(ChannelIds) ->
    lists:foldl(
        fun(ChannelId, Acc) ->
            maps:put(ChannelId, true, Acc)
        end,
        #{},
        ChannelIds
    ).

-spec subscribe_to_user_presence(user_id(), guild_state()) -> guild_state().
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

-spec unsubscribe_from_user_presence(user_id(), guild_state()) -> guild_state().
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

-spec handle_user_offline(user_id(), guild_state()) -> guild_state().
handle_user_offline(UserId, State) ->
    PresenceSubs = maps:get(presence_subscriptions, State, #{}),
    case maps:get(UserId, PresenceSubs, undefined) of
        0 ->
            presence_bus:unsubscribe(UserId),
            NewSubs = maps:remove(UserId, PresenceSubs),
            StateWithSubs = maps:put(presence_subscriptions, NewSubs, State),
            MemberPresence = maps:get(member_presence, StateWithSubs, #{}),
            UpdatedMemberPresence = maps:remove(UserId, MemberPresence),
            maps:put(member_presence, UpdatedMemberPresence, StateWithSubs);
        _ ->
            State
    end.

-spec maybe_send_cached_presence(user_id(), guild_state()) -> guild_state().
maybe_send_cached_presence(UserId, State) ->
    case presence_cache:get(UserId) of
        {ok, Payload} ->
            case guild_presence:handle_bus_presence(UserId, Payload, State) of
                {noreply, UpdatedState} -> UpdatedState
            end;
        _ ->
            State
    end.

-spec set_session_active_guild(session_id(), guild_id(), guild_state()) -> guild_state().
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

-spec set_session_passive_guild(session_id(), guild_id(), guild_state()) -> guild_state().
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

-spec is_session_active(session_id(), guild_state()) -> boolean().
is_session_active(SessionId, State) ->
    GuildId = maps:get(id, State, 0),
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        undefined -> false;
        SessionData -> not session_passive:is_passive(GuildId, SessionData)
    end.

-spec maybe_auto_sync_initial_guild(
    session_id(), guild_id(), guild_id() | undefined, guild_state()
) ->
    guild_state().
maybe_auto_sync_initial_guild(SessionId, GuildId, GuildId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            State;
        SessionData ->
            SyncedSessionData = session_passive:mark_guild_synced(GuildId, SessionData),
            NewSessions = maps:put(SessionId, SyncedSessionData, Sessions),
            maps:put(sessions, NewSessions, State)
    end;
maybe_auto_sync_initial_guild(_SessionId, _GuildId, _InitialGuildId, State) ->
    State.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_initial_last_message_ids_empty_channels_test() ->
    GuildState = #{<<"channels">> => []},
    Result = build_initial_last_message_ids(GuildState),
    ?assertEqual(#{}, Result).

build_initial_last_message_ids_with_channels_test() ->
    GuildState = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"last_message_id">> => <<"500">>},
            #{<<"id">> => <<"101">>, <<"last_message_id">> => <<"600">>}
        ]
    },
    Result = build_initial_last_message_ids(GuildState),
    ?assertEqual(#{<<"100">> => <<"500">>, <<"101">> => <<"600">>}, Result).

build_initial_last_message_ids_filters_null_test() ->
    GuildState = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"last_message_id">> => <<"500">>},
            #{<<"id">> => <<"101">>, <<"last_message_id">> => null},
            #{<<"id">> => <<"102">>}
        ]
    },
    Result = build_initial_last_message_ids(GuildState),
    ?assertEqual(#{<<"100">> => <<"500">>}, Result).

build_initial_last_message_ids_no_channels_key_test() ->
    GuildState = #{},
    Result = build_initial_last_message_ids(GuildState),
    ?assertEqual(#{}, Result).

build_initial_channel_versions_test() ->
    GuildState = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"version">> => 5},
            #{<<"id">> => <<"101">>}
        ]
    },
    Result = build_initial_channel_versions(GuildState),
    ?assertEqual(#{<<"100">> => 5, <<"101">> => 0}, Result).

filter_sessions_for_channel_uses_cached_viewable_channels_test() ->
    SessionId = <<"s1">>,
    SessionData = #{
        session_id => SessionId,
        user_id => 10,
        pid => self(),
        viewable_channels => #{200 => true}
    },
    Sessions = #{SessionId => SessionData},
    State = #{
        sessions => Sessions,
        data => #{<<"members">> => #{}}
    },
    Result = filter_sessions_for_channel(Sessions, 200, undefined, State),
    ?assertEqual([{SessionId, SessionData}], Result).

refresh_all_viewable_channels_populates_cache_test() ->
    SessionId = <<"s1">>,
    UserId = 10,
    GuildId = 42,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        sessions => #{
            SessionId => #{
                session_id => SessionId,
                user_id => UserId,
                pid => self()
            }
        },
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => integer_to_binary(ViewPerm)}
            ],
            <<"members">> => #{
                UserId => #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => []}
            },
            <<"channels">> => [
                #{<<"id">> => <<"200">>, <<"permission_overwrites">> => []}
            ]
        }
    },
    UpdatedState = refresh_all_viewable_channels(State),
    UpdatedSessions = maps:get(sessions, UpdatedState, #{}),
    UpdatedSession = maps:get(SessionId, UpdatedSessions),
    ViewableChannels = maps:get(viewable_channels, UpdatedSession, #{}),
    ?assertEqual(true, maps:is_key(200, ViewableChannels)).

filter_sessions_exclude_session_undefined_test() ->
    Sessions = #{<<"s1">> => #{}, <<"s2">> => #{}},
    Result = filter_sessions_exclude_session(Sessions, undefined),
    ?assertEqual(2, length(Result)).

filter_sessions_exclude_session_specific_test() ->
    Sessions = #{<<"s1">> => #{}, <<"s2">> => #{}},
    Result = filter_sessions_exclude_session(Sessions, <<"s1">>),
    ?assertEqual(1, length(Result)),
    ?assertEqual([{<<"s2">>, #{}}], Result).

should_exclude_session_test() ->
    ?assertEqual(false, should_exclude_session(<<"s1">>, undefined)),
    ?assertEqual(true, should_exclude_session(<<"s1">>, <<"s1">>)),
    ?assertEqual(false, should_exclude_session(<<"s1">>, <<"s2">>)).

find_session_by_ref_found_test() ->
    Ref = make_ref(),
    Sessions = #{<<"s1">> => #{mref => Ref, user_id => 1}},
    Result = find_session_by_ref(Ref, Sessions),
    ?assertEqual(#{mref => Ref, user_id => 1}, Result).

find_session_by_ref_not_found_test() ->
    Ref = make_ref(),
    OtherRef = make_ref(),
    Sessions = #{<<"s1">> => #{mref => OtherRef, user_id => 1}},
    Result = find_session_by_ref(Ref, Sessions),
    ?assertEqual(undefined, Result).

remove_session_removes_entry_test() ->
    SessionId = <<"s1">>,
    SessionData = #{
        session_id => SessionId,
        user_id => 1,
        pid => self(),
        mref => make_ref(),
        active_guilds => sets:new(),
        user_roles => [],
        bot => false
    },
    State = #{
        id => 42,
        sessions => #{SessionId => SessionData},
        presence_subscriptions => #{1 => 1},
        member_list_subscriptions => #{},
        member_subscriptions => guild_subscriptions:init_state()
    },
    NewState = remove_session(SessionId, State),
    ?assertEqual(#{}, maps:get(sessions, NewState, #{})),
    ?assertEqual(0, maps:get(1, maps:get(presence_subscriptions, NewState, #{}), 0)).

remove_session_cleans_connect_pending_test() ->
    SessionId = <<"s1">>,
    SessionData = #{
        session_id => SessionId,
        user_id => 1,
        pid => self(),
        mref => make_ref(),
        active_guilds => sets:new(),
        user_roles => [],
        bot => false
    },
    State = #{
        id => 42,
        sessions => #{SessionId => SessionData},
        presence_subscriptions => #{1 => 1},
        member_list_subscriptions => #{},
        member_subscriptions => guild_subscriptions:init_state(),
        session_connect_pending => #{SessionId => 3, <<"s2">> => 1},
        session_connect_queue => [
            #{request => #{session_id => SessionId}, attempt => 3},
            #{request => #{session_id => <<"s2">>}, attempt => 1}
        ]
    },
    NewState = remove_session(SessionId, State),
    Pending = maps:get(session_connect_pending, NewState, #{}),
    ?assertEqual(false, maps:is_key(SessionId, Pending)),
    ?assertEqual(true, maps:is_key(<<"s2">>, Pending)),
    Queue0 = maps:get(session_connect_queue, NewState, queue:new()),
    Queue =
        case Queue0 of
            L when is_list(L) -> L;
            _ ->
                case queue:is_queue(Queue0) of
                    true -> queue:to_list(Queue0);
                    false -> []
                end
        end,
    ?assertEqual(
        1,
        length([
            Item
         || Item <- Queue,
            maps:get(session_id, maps:get(request, Item, #{}), undefined) =:= <<"s2">>
        ])
    ),
    ?assertEqual(
        0,
        length([
            Item
         || Item <- Queue,
            maps:get(session_id, maps:get(request, Item, #{}), undefined) =:= SessionId
        ])
    ),
    ok.

cleanup_connect_admission_queue_format_test() ->
    S1 = <<"s1">>,
    S2 = <<"s2">>,
    S3 = <<"s3">>,
    Queue = queue:from_list([
        #{request => #{session_id => S1}, attempt => 0},
        #{request => #{session_id => S2}, attempt => 1},
        #{request => #{session_id => S1}, attempt => 2},
        #{request => #{session_id => S3}, attempt => 3}
    ]),
    State0 = #{
        sessions => #{},
        session_connect_queue => Queue,
        presence_subscriptions => #{},
        member_list_subscriptions => #{},
        member_subscriptions => guild_subscriptions:init_state()
    },
    State1 = cleanup_connect_admission_for_session(S1, State0),
    ResultQueue0 = maps:get(session_connect_queue, State1),
    ResultQueue =
        case queue:is_queue(ResultQueue0) of
            true -> queue:to_list(ResultQueue0);
            false -> ResultQueue0
        end,
    ?assertEqual(2, length(ResultQueue)),
    SessionIds = [
        maps:get(session_id, maps:get(request, Item, #{}), undefined)
     || Item <- ResultQueue
    ],
    ?assertEqual(false, lists:member(S1, SessionIds)),
    ?assertEqual(true, lists:member(S2, SessionIds)),
    ?assertEqual(true, lists:member(S3, SessionIds)).

pending_connect_filtered_from_channel_sessions_test() ->
    NormalSession = #{
        session_id => <<"s1">>,
        user_id => 10,
        pid => self(),
        viewable_channels => #{200 => true}
    },
    PendingSession = #{
        session_id => <<"s2">>,
        user_id => 11,
        pid => self(),
        pending_connect => true,
        viewable_channels => #{200 => true}
    },
    Sessions = #{<<"s1">> => NormalSession, <<"s2">> => PendingSession},
    State = #{
        sessions => Sessions,
        data => #{<<"members">> => #{}}
    },
    Result = filter_sessions_for_channel(Sessions, 200, undefined, State),
    ?assertEqual(1, length(Result)),
    [{ResultSid, _}] = Result,
    ?assertEqual(<<"s1">>, ResultSid).

set_session_viewable_channels_test() ->
    Sessions = #{<<"s1">> => #{user_id => 1, pid => self()}},
    State = #{sessions => Sessions},
    ViewableChannels = #{100 => true, 200 => true},
    UpdatedState = set_session_viewable_channels(<<"s1">>, ViewableChannels, State),
    UpdatedSession = maps:get(<<"s1">>, maps:get(sessions, UpdatedState)),
    ?assertEqual(ViewableChannels, maps:get(viewable_channels, UpdatedSession)).

set_session_viewable_channels_missing_session_test() ->
    State = #{sessions => #{}},
    Result = set_session_viewable_channels(<<"nonexistent">>, #{100 => true}, State),
    ?assertEqual(State, Result).

set_session_active_guild_missing_session_test() ->
    State = #{sessions => #{}},
    Result = set_session_active_guild(<<"nonexistent">>, 42, State),
    ?assertEqual(State, Result).

set_session_passive_guild_missing_session_test() ->
    State = #{sessions => #{}},
    Result = set_session_passive_guild(<<"nonexistent">>, 42, State),
    ?assertEqual(State, Result).

is_session_active_missing_session_test() ->
    State = #{id => 42, sessions => #{}},
    ?assertEqual(false, is_session_active(<<"nonexistent">>, State)).

filter_sessions_for_channel_excludes_specified_session_test() ->
    S1 = #{session_id => <<"s1">>, user_id => 10, pid => self(), viewable_channels => #{200 => true}},
    S2 = #{session_id => <<"s2">>, user_id => 11, pid => self(), viewable_channels => #{200 => true}},
    Sessions = #{<<"s1">> => S1, <<"s2">> => S2},
    State = #{sessions => Sessions, data => #{<<"members">> => #{}}},
    Result = filter_sessions_for_channel(Sessions, 200, <<"s1">>, State),
    ?assertEqual(1, length(Result)),
    [{ResultSid, _}] = Result,
    ?assertEqual(<<"s2">>, ResultSid).

filter_sessions_for_channel_falls_back_to_permission_check_test() ->
    GuildId = 42,
    UserId = 10,
    ChannelId = 200,
    ViewPerm = constants:view_channel_permission(),
    SessionData = #{
        session_id => <<"s1">>,
        user_id => UserId,
        pid => self()
    },
    Sessions = #{<<"s1">> => SessionData},
    State = #{
        id => GuildId,
        sessions => Sessions,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => integer_to_binary(ViewPerm)}
            ],
            <<"members">> => #{
                UserId => #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => []}
            },
            <<"channels">> => [
                #{<<"id">> => integer_to_binary(ChannelId), <<"permission_overwrites">> => []}
            ]
        }
    },
    Result = filter_sessions_for_channel(Sessions, ChannelId, undefined, State),
    ?assertEqual(1, length(Result)).

filter_sessions_for_channel_no_member_returns_empty_test() ->
    SessionData = #{
        session_id => <<"s1">>,
        user_id => 999,
        pid => self()
    },
    Sessions = #{<<"s1">> => SessionData},
    State = #{
        sessions => Sessions,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"888">>},
            <<"members">> => [],
            <<"roles">> => [],
            <<"channels">> => []
        }
    },
    Result = filter_sessions_for_channel(Sessions, 200, undefined, State),
    ?assertEqual([], Result).

filter_sessions_exclude_session_filters_pending_test() ->
    Sessions = #{
        <<"s1">> => #{pending_connect => true},
        <<"s2">> => #{},
        <<"s3">> => #{pending_connect => false}
    },
    Result = filter_sessions_exclude_session(Sessions, undefined),
    ResultIds = lists:sort([Sid || {Sid, _} <- Result]),
    ?assertEqual([<<"s2">>, <<"s3">>], ResultIds).

subscribe_unsubscribe_presence_test_() ->
    {setup,
        fun() -> ensure_test_deps() end,
        fun(_) -> stop_test_deps() end,
        fun(_) ->
            [fun() ->
                State0 = #{presence_subscriptions => #{}},
                State1 = subscribe_to_user_presence(10, State0),
                Subs1 = maps:get(presence_subscriptions, State1),
                ?assertEqual(1, maps:get(10, Subs1)),
                State2 = subscribe_to_user_presence(10, State1),
                Subs2 = maps:get(presence_subscriptions, State2),
                ?assertEqual(2, maps:get(10, Subs2)),
                State3 = unsubscribe_from_user_presence(10, State2),
                Subs3 = maps:get(presence_subscriptions, State3),
                ?assertEqual(1, maps:get(10, Subs3)),
                State4 = unsubscribe_from_user_presence(10, State3),
                Subs4 = maps:get(presence_subscriptions, State4),
                ?assertEqual(0, maps:get(10, Subs4))
            end]
        end}.

unsubscribe_from_user_presence_zero_count_noop_test() ->
    State = #{presence_subscriptions => #{10 => 0}},
    Result = unsubscribe_from_user_presence(10, State),
    ?assertEqual(State, Result).

unsubscribe_from_user_presence_missing_user_noop_test() ->
    State = #{presence_subscriptions => #{}},
    Result = unsubscribe_from_user_presence(999, State),
    ?assertEqual(State, Result).

handle_user_offline_nonzero_count_noop_test() ->
    State = #{
        presence_subscriptions => #{10 => 1},
        member_presence => #{10 => #{<<"status">> => <<"online">>}}
    },
    Result = handle_user_offline(10, State),
    ?assertEqual(State, Result).

handle_user_offline_missing_user_noop_test() ->
    State = #{presence_subscriptions => #{}},
    Result = handle_user_offline(999, State),
    ?assertEqual(State, Result).

should_auto_stop_on_empty_default_test() ->
    State = #{},
    ?assertEqual(true, should_auto_stop_on_empty(State)).

should_auto_stop_on_empty_disabled_test() ->
    State = #{disable_auto_stop_on_empty => true},
    ?assertEqual(false, should_auto_stop_on_empty(State)).

should_auto_stop_on_empty_vlg_coordinator_test() ->
    State = #{very_large_guild_coordinator_pid => self()},
    ?assertEqual(false, should_auto_stop_on_empty(State)).

build_viewable_channel_map_test() ->
    Map = build_viewable_channel_map([100, 200, 300]),
    ?assertEqual(3, map_size(Map)),
    ?assertEqual(true, maps:get(100, Map)),
    ?assertEqual(true, maps:get(200, Map)),
    ?assertEqual(true, maps:get(300, Map)).

build_viewable_channel_map_empty_test() ->
    ?assertEqual(#{}, build_viewable_channel_map([])).

normalize_connect_queue_list_test() ->
    List = [#{a => 1}, #{a => 2}],
    Queue = normalize_connect_queue(List),
    ?assert(queue:is_queue(Queue)),
    ?assertEqual(2, queue:len(Queue)).

normalize_connect_queue_queue_test() ->
    Q = queue:from_list([1, 2, 3]),
    ?assertEqual(Q, normalize_connect_queue(Q)).

normalize_connect_queue_undefined_test() ->
    ?assertEqual(undefined, normalize_connect_queue(undefined)),
    ?assertEqual(undefined, normalize_connect_queue(42)).

ensure_test_deps() ->
    ensure_mock_registered(presence_bus),
    ensure_mock_registered(presence_cache).

stop_test_deps() ->
    stop_mock_registered(presence_bus),
    stop_mock_registered(presence_cache).

ensure_mock_registered(Name) ->
    case whereis(Name) of
        undefined ->
            Pid = spawn(fun() -> mock_gen_server_loop() end),
            register(Name, Pid),
            Pid;
        Pid ->
            Pid
    end.

stop_mock_registered(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid ->
            catch unregister(Name),
            Pid ! stop,
            ok
    end.

mock_gen_server_loop() ->
    receive
        {'$gen_call', From, {get, _}} ->
            gen_server:reply(From, not_found),
            mock_gen_server_loop();
        {'$gen_call', From, _Msg} ->
            gen_server:reply(From, ok),
            mock_gen_server_loop();
        stop ->
            ok;
        _ ->
            mock_gen_server_loop()
    end.

-endif.
