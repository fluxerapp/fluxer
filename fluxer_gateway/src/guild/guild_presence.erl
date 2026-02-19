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

-module(guild_presence).

-export([handle_bus_presence/3, send_cached_presence_to_session/3]).
-export([broadcast_presence_update/3]).

-type guild_state() :: map().
-type member() :: map().
-type user_id() :: integer().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec handle_bus_presence(user_id(), map(), guild_state()) -> {noreply, guild_state()}.
handle_bus_presence(UserId, Payload, State) ->
    case maps:get(<<"user_update">>, Payload, false) of
        true ->
            UserData = maps:get(<<"user">>, Payload, #{}),
            UpdatedState = handle_user_data_update(UserId, UserData, State),
            MemberData = find_member_by_user_id(UserId, UpdatedState),
            case is_map(MemberData) of
                true ->
                    maybe_notify_very_large_guild_member_list({upsert_member, MemberData}, UpdatedState);
                false ->
                    maybe_notify_very_large_guild_member_list({notify_member_update, UserId}, UpdatedState)
            end,
            maybe_broadcast_member_list_updates(UserId, State, UpdatedState),
            {noreply, UpdatedState};
        false ->
            Member = find_member_by_user_id(UserId, State),
            case Member of
                undefined ->
                    {noreply, State};
                _ ->
                    StatusBin = maps:get(<<"status">>, Payload, <<"offline">>),
                    NormalizedStatusBin = normalize_presence_status(StatusBin),
                    Status = constants:status_type_atom(NormalizedStatusBin),
                    Mobile = maps:get(<<"mobile">>, Payload, false),
                    Afk = maps:get(<<"afk">>, Payload, false),
                    MemberUser = maps:get(<<"user">>, Member, #{}),
                    CustomStatus = maps:get(<<"custom_status">>, Payload, null),
                    PresenceMap = presence_payload:build(
                        MemberUser,
                        NormalizedStatusBin,
                        Mobile,
                        Afk,
                        CustomStatus
                    ),
                    StateWithPresence = store_member_presence(UserId, PresenceMap, State),
                    broadcast_presence_update(UserId, PresenceMap, StateWithPresence),
                    maybe_notify_very_large_guild_member_list(
                        {presence_update, UserId, PresenceMap}, StateWithPresence
                    ),
                    maybe_broadcast_member_list_updates(UserId, State, StateWithPresence),
                    StateAfterOffline =
                        case Status of
                            offline ->
                                guild_sessions:handle_user_offline(UserId, StateWithPresence);
                            _ ->
                                StateWithPresence
                        end,
                    {noreply, StateAfterOffline}
            end
    end.

-spec maybe_broadcast_member_list_updates(user_id(), guild_state(), guild_state()) -> ok.
maybe_broadcast_member_list_updates(UserId, OldState, UpdatedState) ->
    case maps:get(very_large_guild_coordinator_pid, UpdatedState, undefined) of
        Pid when is_pid(Pid) ->
            ok;
        _ ->
            guild_member_list:broadcast_member_list_updates(UserId, OldState, UpdatedState)
    end.

-spec maybe_notify_very_large_guild_member_list(term(), guild_state()) -> ok.
maybe_notify_very_large_guild_member_list(NotifyMsg, State) ->
    case maps:get(very_large_guild_coordinator_pid, State, undefined) of
        CoordPid when is_pid(CoordPid) ->
            gen_server:cast(CoordPid, {very_large_guild_member_list_notify, NotifyMsg}),
            ok;
        _ ->
            ok
    end.

-spec broadcast_presence_update(user_id(), map(), guild_state()) -> ok.
broadcast_presence_update(UserId, Payload, State) ->
    case find_member_by_user_id(UserId, State) of
        undefined ->
            ok;
        _Member ->
            GuildId = map_utils:get_integer(State, id, 0),
            PresenceUpdate = maps:put(<<"guild_id">>, integer_to_binary(GuildId), Payload),
            Sessions = maps:get(sessions, State, #{}),
            MemberSubs = maps:get(member_subscriptions, State, guild_subscriptions:init_state()),
            SubscribedSessionIds = guild_subscriptions:get_subscribed_sessions(UserId, MemberSubs),
            TargetChannelMap = get_user_viewable_channel_map(UserId, Sessions, State),
            {ValidSessionIds, InvalidSessionIds} =
                partition_subscribed_sessions(
                    SubscribedSessionIds, Sessions, TargetChannelMap, UserId, State
                ),
            StateAfterInvalidRemovals =
                lists:foldl(
                    fun(SessionId, AccState) ->
                        remove_session_member_subscription(SessionId, UserId, AccState)
                    end,
                    State,
                    sets:to_list(sets:from_list(InvalidSessionIds))
                ),
            FinalSessions = maps:get(sessions, StateAfterInvalidRemovals, #{}),
            ValidSessionSet = sets:from_list(ValidSessionIds),
            SessionsToNotify = lists:filter(
                fun({SessionId, _}) -> sets:is_element(SessionId, ValidSessionSet) end,
                maps:to_list(FinalSessions)
            ),
            lists:foreach(
                fun({_SessionId, SessionData}) ->
                    SessionPid = maps:get(pid, SessionData),
                    case is_pid(SessionPid) of
                        true ->
                            gen_server:cast(
                                SessionPid, {dispatch, presence_update, PresenceUpdate}
                            );
                        false ->
                            ok
                    end
                end,
                SessionsToNotify
            ),
            ok
    end.

-spec normalize_presence_status(binary() | term()) -> binary().
normalize_presence_status(<<"invisible">>) -> <<"offline">>;
normalize_presence_status(Status) when is_binary(Status) -> Status;
normalize_presence_status(_) -> <<"offline">>.

-spec send_cached_presence_to_session(user_id(), binary(), guild_state()) -> guild_state().
send_cached_presence_to_session(UserId, SessionId, State) ->
    case presence_cache:get(UserId) of
        {ok, Payload} ->
            send_presence_payload_to_session(UserId, SessionId, Payload, State);
        _ ->
            State
    end.

-spec send_presence_payload_to_session(user_id(), binary(), map(), guild_state()) -> guild_state().
send_presence_payload_to_session(UserId, SessionId, Payload, State) ->
    GuildId = map_utils:get_integer(State, id, 0),
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        #{pid := SessionPid} when is_pid(SessionPid) ->
            Member = find_member_by_user_id(UserId, State),
            case Member of
                undefined ->
                    State;
                _ ->
                    StatusBin = maps:get(<<"status">>, Payload, <<"offline">>),
                    Mobile = maps:get(<<"mobile">>, Payload, false),
                    Afk = maps:get(<<"afk">>, Payload, false),
                    MemberUser = maps:get(<<"user">>, Member, #{}),
                    CustomStatus = maps:get(<<"custom_status">>, Payload, null),
                    PresenceBase =
                        presence_payload:build(MemberUser, StatusBin, Mobile, Afk, CustomStatus),
                    PresenceUpdate = maps:put(
                        <<"guild_id">>, integer_to_binary(GuildId), PresenceBase
                    ),
                    gen_server:cast(SessionPid, {dispatch, presence_update, PresenceUpdate}),
                    State
            end;
        _ ->
            State
    end.

-spec handle_user_data_update(user_id(), map(), guild_state()) -> guild_state().
handle_user_data_update(UserId, UserData, State) ->
    Data = guild_data(State),
    Members = guild_data_index:member_map(Data),
    case find_member_by_user_id(UserId, State) of
        undefined ->
            State;
        Member ->
            CurrentUserData = maps:get(<<"user">>, Member, #{}),
            case check_user_data_differs(CurrentUserData, UserData) of
                false ->
                    State;
                true ->
                    UpdatedMembers = maps:map(
                        fun(_MemberUserId, Member0) ->
                            maybe_replace_member(Member0, UserId, UserData)
                        end,
                        Members
                    ),
                    UpdatedData = guild_data_index:put_member_map(UpdatedMembers, Data),
                    UpdatedState = maps:put(data, UpdatedData, State),
                    maybe_dispatch_member_update(UserId, UpdatedState),
                    UpdatedState
            end
    end.

-spec maybe_replace_member(member(), user_id(), map()) -> member().
maybe_replace_member(Member, UserId, UserData) ->
    case member_id(Member) of
        UserId ->
            maps:put(<<"user">>, UserData, Member);
        _ ->
            Member
    end.

-spec maybe_dispatch_member_update(user_id(), guild_state()) -> ok.
maybe_dispatch_member_update(UserId, State) ->
    case find_member_by_user_id(UserId, State) of
        undefined ->
            ok;
        Member ->
            GuildId = map_utils:get_integer(State, id, 0),
            MemberUpdate = maps:put(<<"guild_id">>, integer_to_binary(GuildId), Member),
            gen_server:cast(
                self(), {dispatch, #{event => guild_member_update, data => MemberUpdate}}
            )
    end.

-spec guild_data(guild_state()) -> map().
guild_data(State) ->
    map_utils:ensure_map(map_utils:get_safe(State, data, #{})).

-spec member_id(map()) -> user_id() | undefined.
member_id(Member) ->
    User = map_utils:ensure_map(maps:get(<<"user">>, Member, #{})),
    map_utils:get_integer(User, <<"id">>, undefined).

-spec partition_subscribed_sessions([binary()], map(), map(), user_id(), guild_state()) ->
    {[binary()], [binary()]}.
partition_subscribed_sessions(SessionIds, Sessions, TargetChannelMap, TargetUserId, State) ->
    lists:foldl(
        fun(SessionId, {Valids, Invalids}) ->
            case maps:get(SessionId, Sessions, undefined) of
                undefined ->
                    {Valids, [SessionId | Invalids]};
                SessionData ->
                    SessionUserId = maps:get(user_id, SessionData, undefined),
                    Shared =
                        case SessionUserId of
                            undefined ->
                                false;
                            UserId when UserId =:= TargetUserId ->
                                false;
                            _ ->
                                session_shares_channels(
                                    SessionData, SessionUserId, TargetChannelMap, State
                                )
                        end,
                    case Shared of
                        true -> {[SessionId | Valids], Invalids};
                        false -> {Valids, [SessionId | Invalids]}
                    end
            end
        end,
        {[], []},
        SessionIds
    ).

-spec session_shares_channels(map(), user_id(), map(), guild_state()) -> boolean().
session_shares_channels(SessionData, SessionUserId, TargetChannelMap, State) ->
    case maps:get(viewable_channels, SessionData, undefined) of
        ViewableMap when is_map(ViewableMap) ->
            maps_share_any_key(ViewableMap, TargetChannelMap);
        _ ->
            SessionChannels = guild_visibility:viewable_channel_set(SessionUserId, State),
            TargetChannels = sets:from_list(maps:keys(TargetChannelMap)),
            not sets:is_empty(sets:intersection(SessionChannels, TargetChannels))
    end.

-spec maps_share_any_key(map(), map()) -> boolean().
maps_share_any_key(MapA, MapB) ->
    {Smaller, Larger} =
        case map_size(MapA) =< map_size(MapB) of
            true -> {MapA, MapB};
            false -> {MapB, MapA}
        end,
    maps_share_any_key_iter(maps:iterator(Smaller), Larger).

-spec maps_share_any_key_iter(maps:iterator(), map()) -> boolean().
maps_share_any_key_iter(Iterator, LargerMap) ->
    case maps:next(Iterator) of
        none ->
            false;
        {Key, _, NextIterator} ->
            case maps:is_key(Key, LargerMap) of
                true -> true;
                false -> maps_share_any_key_iter(NextIterator, LargerMap)
            end
    end.

-spec get_user_viewable_channel_map(user_id(), map(), guild_state()) -> map().
get_user_viewable_channel_map(UserId, Sessions, State) ->
    case find_session_viewable_channels_for_user(UserId, Sessions) of
        undefined ->
            ChannelList = guild_visibility:get_user_viewable_channels(UserId, State),
            maps:from_list([{Ch, true} || Ch <- ChannelList]);
        ViewableMap ->
            ViewableMap
    end.

-spec find_session_viewable_channels_for_user(user_id(), map()) -> map() | undefined.
find_session_viewable_channels_for_user(UserId, Sessions) ->
    find_session_viewable_channels_iter(UserId, maps:iterator(Sessions)).

-spec find_session_viewable_channels_iter(user_id(), maps:iterator()) -> map() | undefined.
find_session_viewable_channels_iter(UserId, Iterator) ->
    case maps:next(Iterator) of
        none ->
            undefined;
        {_, SessionData, NextIterator} ->
            case maps:get(user_id, SessionData, undefined) of
                UserId ->
                    case maps:get(viewable_channels, SessionData, undefined) of
                        ViewableChannels when is_map(ViewableChannels) ->
                            ViewableChannels;
                        _ ->
                            find_session_viewable_channels_iter(UserId, NextIterator)
                    end;
                _ ->
                    find_session_viewable_channels_iter(UserId, NextIterator)
            end
    end.

-spec remove_session_member_subscription(binary(), user_id(), guild_state()) -> guild_state().
remove_session_member_subscription(SessionId, UserId, State) ->
    MemberSubs = maps:get(member_subscriptions, State, guild_subscriptions:init_state()),
    NewMemberSubs = guild_subscriptions:unsubscribe(SessionId, UserId, MemberSubs),
    State1 = maps:put(member_subscriptions, NewMemberSubs, State),
    guild_sessions:unsubscribe_from_user_presence(UserId, State1).

-spec check_user_data_differs(map(), map()) -> boolean().
check_user_data_differs(CurrentUserData, NewUserData) ->
    utils:check_user_data_differs(CurrentUserData, NewUserData).

-spec find_member_by_user_id(user_id(), guild_state()) -> member() | undefined.
find_member_by_user_id(UserId, State) ->
    guild_permissions:find_member_by_user_id(UserId, State).

-spec store_member_presence(user_id(), map(), guild_state()) -> guild_state().
store_member_presence(UserId, PresenceMap, State) ->
    MemberPresence = maps:get(member_presence, State, #{}),
    UpdatedPresence = maps:put(UserId, PresenceMap, MemberPresence),
    maps:put(member_presence, UpdatedPresence, State).

-ifdef(TEST).

handle_bus_presence_non_member_noop_test() ->
    Payload = #{<<"status">> => <<"online">>, <<"user">> => #{<<"id">> => <<"99">>}},
    State = #{data => #{<<"members">> => []}, sessions => #{}},
    {noreply, NewState} = handle_bus_presence(99, Payload, State),
    ?assertEqual(State, NewState).

handle_bus_presence_broadcasts_test() ->
    State = presence_test_state(),
    Payload = #{
        <<"status">> => <<"online">>,
        <<"mobile">> => true,
        <<"afk">> => false,
        <<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"Alpha">>}
    },
    {noreply, _NewState} = handle_bus_presence(1, Payload, State),
    ok.

handle_bus_presence_user_update_test() ->
    State = presence_test_state(),
    UserData = #{<<"id">> => <<"1">>, <<"username">> => <<"Updated">>},
    Payload = #{<<"user">> => UserData, <<"user_update">> => true},
    {noreply, NewState} = handle_bus_presence(1, Payload, State),
    Data = maps:get(data, NewState),
    Member = maps:get(1, maps:get(<<"members">>, Data)),
    ?assertEqual(<<"Updated">>, maps:get(<<"username">>, maps:get(<<"user">>, Member))).

normalize_presence_status_test() ->
    ?assertEqual(<<"offline">>, normalize_presence_status(<<"invisible">>)),
    ?assertEqual(<<"online">>, normalize_presence_status(<<"online">>)),
    ?assertEqual(<<"idle">>, normalize_presence_status(<<"idle">>)),
    ?assertEqual(<<"offline">>, normalize_presence_status(undefined)).

handle_bus_presence_invisible_normalized_test() ->
    State = presence_test_state(),
    Payload = #{
        <<"status">> => <<"invisible">>,
        <<"mobile">> => false,
        <<"afk">> => false,
        <<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"Alpha">>}
    },
    {noreply, NewState} = handle_bus_presence(1, Payload, State),
    MemberPresence = maps:get(member_presence, NewState, #{}),
    UserPresence = maps:get(1, MemberPresence),
    ?assertEqual(<<"offline">>, maps:get(<<"status">>, UserPresence)).

maps_share_any_key_empty_test() ->
    ?assertEqual(false, maps_share_any_key(#{}, #{})),
    ?assertEqual(false, maps_share_any_key(#{1 => true}, #{})),
    ?assertEqual(false, maps_share_any_key(#{}, #{1 => true})).

maps_share_any_key_overlap_test() ->
    ?assertEqual(true, maps_share_any_key(#{1 => true, 2 => true}, #{2 => true, 3 => true})),
    ?assertEqual(true, maps_share_any_key(#{5 => true}, #{5 => true})).

maps_share_any_key_no_overlap_test() ->
    ?assertEqual(false, maps_share_any_key(#{1 => true, 2 => true}, #{3 => true, 4 => true})).

get_user_viewable_channel_map_uses_session_cache_test() ->
    Sessions = #{
        <<"s1">> => #{user_id => 10, viewable_channels => #{100 => true, 200 => true}},
        <<"s2">> => #{user_id => 20, viewable_channels => #{300 => true}}
    },
    State = #{sessions => Sessions, data => #{<<"members">> => #{}}},
    Result = get_user_viewable_channel_map(10, Sessions, State),
    ?assertEqual(#{100 => true, 200 => true}, Result).

get_user_viewable_channel_map_skips_session_without_cache_test() ->
    Sessions = #{
        <<"s1">> => #{user_id => 10},
        <<"s2">> => #{user_id => 10, viewable_channels => #{100 => true}}
    },
    State = #{sessions => Sessions, data => #{<<"members">> => #{}}},
    Result = get_user_viewable_channel_map(10, Sessions, State),
    ?assertEqual(#{100 => true}, Result).

session_shares_channels_uses_cached_viewable_test() ->
    SessionData = #{user_id => 20, viewable_channels => #{100 => true, 200 => true}},
    TargetChannelMap = #{200 => true, 300 => true},
    State = #{sessions => #{}, data => #{<<"members">> => #{}}},
    ?assertEqual(true, session_shares_channels(SessionData, 20, TargetChannelMap, State)).

session_shares_channels_no_overlap_test() ->
    SessionData = #{user_id => 20, viewable_channels => #{100 => true}},
    TargetChannelMap = #{200 => true, 300 => true},
    State = #{sessions => #{}, data => #{<<"members">> => #{}}},
    ?assertEqual(false, session_shares_channels(SessionData, 20, TargetChannelMap, State)).

partition_subscribed_sessions_uses_cached_channels_test() ->
    Sessions = #{
        <<"s1">> => #{user_id => 20, pid => self(), viewable_channels => #{100 => true}},
        <<"s2">> => #{user_id => 30, pid => self(), viewable_channels => #{200 => true}}
    },
    TargetChannelMap = #{100 => true, 300 => true},
    State = #{sessions => Sessions, data => #{<<"members">> => #{}}},
    {Valid, Invalid} = partition_subscribed_sessions(
        [<<"s1">>, <<"s2">>], Sessions, TargetChannelMap, 10, State
    ),
    ?assertEqual([<<"s1">>], Valid),
    ?assertEqual([<<"s2">>], Invalid).

partition_subscribed_sessions_excludes_target_user_test() ->
    Sessions = #{
        <<"s1">> => #{user_id => 10, pid => self(), viewable_channels => #{100 => true}}
    },
    TargetChannelMap = #{100 => true},
    State = #{sessions => Sessions, data => #{<<"members">> => #{}}},
    {Valid, Invalid} = partition_subscribed_sessions(
        [<<"s1">>], Sessions, TargetChannelMap, 10, State
    ),
    ?assertEqual([], Valid),
    ?assertEqual([<<"s1">>], Invalid).

partition_subscribed_sessions_missing_session_test() ->
    Sessions = #{},
    TargetChannelMap = #{100 => true},
    State = #{sessions => Sessions, data => #{<<"members">> => #{}}},
    {Valid, Invalid} = partition_subscribed_sessions(
        [<<"s1">>], Sessions, TargetChannelMap, 10, State
    ),
    ?assertEqual([], Valid),
    ?assertEqual([<<"s1">>], Invalid).

presence_test_state() ->
    #{
        id => 42,
        data => #{
            <<"members">> => #{
                1 => #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"Alpha">>}}
            }
        },
        sessions => #{}
    }.

-endif.
