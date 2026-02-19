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

-module(guild_visibility).

-export([
    get_user_viewable_channels/2,
    compute_and_dispatch_visibility_changes/2,
    compute_and_dispatch_visibility_changes_for_users/3,
    compute_and_dispatch_visibility_changes_for_channels/3,
    viewable_channel_set/2,
    have_shared_viewable_channel/3
]).

-type guild_state() :: map().
-type user_id() :: integer().
-type channel_id() :: integer().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec get_user_viewable_channels(user_id(), guild_state()) -> [channel_id()].
get_user_viewable_channels(UserId, State) ->
    Data = map_utils:ensure_map(map_utils:get_safe(State, data, #{})),
    Channels = map_utils:ensure_list(maps:get(<<"channels">>, Data, [])),
    Member = guild_permissions:find_member_by_user_id(UserId, State),
    case Member of
        undefined ->
            [];
        _ ->
            lists:filtermap(
                fun(Channel) ->
                    ChannelId = map_utils:get_integer(Channel, <<"id">>, undefined),
                    case ChannelId of
                        undefined ->
                            false;
                        _ ->
                            case
                                guild_permissions:can_view_channel(UserId, ChannelId, Member, State)
                            of
                                true -> {true, ChannelId};
                                false -> false
                            end
                    end
                end,
                Channels
            )
    end.

-spec viewable_channel_set(user_id(), guild_state()) -> sets:set(channel_id()).
viewable_channel_set(UserId, State) when is_integer(UserId) ->
    case get_cached_viewable_channel_map(UserId, State) of
        undefined ->
            sets:from_list(get_user_viewable_channels(UserId, State));
        ViewableChannelMap ->
            sets:from_list(maps:keys(ViewableChannelMap))
    end;
viewable_channel_set(_, _) ->
    sets:new().

-spec get_cached_viewable_channel_map(user_id(), guild_state()) -> map() | undefined.
get_cached_viewable_channel_map(UserId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    maps:fold(
        fun(_SessionId, SessionData, Acc) ->
            case Acc of
                undefined ->
                    case maps:get(user_id, SessionData, undefined) of
                        UserId ->
                            case maps:get(viewable_channels, SessionData, undefined) of
                                ViewableChannels when is_map(ViewableChannels) ->
                                    ViewableChannels;
                                _ ->
                                    undefined
                            end;
                        _ ->
                            undefined
                    end;
                _ ->
                    Acc
            end
        end,
        undefined,
        Sessions
    ).

-spec have_shared_viewable_channel(user_id(), user_id(), guild_state()) -> boolean().
have_shared_viewable_channel(UserId, OtherUserId, State) when
    is_integer(UserId), is_integer(OtherUserId), UserId =/= OtherUserId
->
    SetA = viewable_channel_set(UserId, State),
    SetB = viewable_channel_set(OtherUserId, State),
    not sets:is_empty(sets:intersection(SetA, SetB));
have_shared_viewable_channel(_, _, _) ->
    false.

-spec filter_connected_session_entries(map()) -> [{binary(), map()}].
filter_connected_session_entries(Sessions) ->
    [{Sid, S} || {Sid, S} <- maps:to_list(Sessions), maps:get(pending_connect, S, false) =/= true].

-spec compute_and_dispatch_visibility_changes(guild_state(), guild_state()) -> guild_state().
compute_and_dispatch_visibility_changes(OldState, NewState) ->
    compute_and_dispatch_visibility_changes_for_sessions(
        filter_connected_session_entries(maps:get(sessions, NewState, #{})),
        OldState,
        NewState
    ).

-spec compute_and_dispatch_visibility_changes_for_channels(
    [channel_id()], guild_state(), guild_state()
) ->
    guild_state().
compute_and_dispatch_visibility_changes_for_channels(ChannelIds, OldState, NewState) ->
    ValidChannelIds = lists:usort([Id || Id <- ChannelIds, is_integer(Id), Id > 0]),
    case ValidChannelIds of
        [] ->
            compute_and_dispatch_visibility_changes(OldState, NewState);
        _ ->
            compute_and_dispatch_channel_visibility_changes_for_sessions(
                filter_connected_session_entries(maps:get(sessions, NewState, #{})),
                ValidChannelIds,
                OldState,
                NewState
            )
    end.

-spec compute_and_dispatch_visibility_changes_for_users([user_id()], guild_state(), guild_state()) ->
    guild_state().
compute_and_dispatch_visibility_changes_for_users(UserIds, OldState, NewState) ->
    Sessions = maps:get(sessions, NewState, #{}),
    UserIdSet = sets:from_list(UserIds),
    TargetSessions = lists:filter(
        fun({_SessionId, SessionData}) ->
            maps:get(pending_connect, SessionData, false) =/= true andalso
                begin
                    SessionUserId = maps:get(user_id, SessionData, undefined),
                    is_integer(SessionUserId) andalso sets:is_element(SessionUserId, UserIdSet)
                end
        end,
        maps:to_list(Sessions)
    ),
    compute_and_dispatch_visibility_changes_for_sessions(TargetSessions, OldState, NewState).

-spec compute_and_dispatch_channel_visibility_changes_for_sessions(
    [{binary(), map()}], [channel_id()], guild_state(), guild_state()
) ->
    guild_state().
compute_and_dispatch_channel_visibility_changes_for_sessions(
    SessionEntries, ChannelIds, OldState, NewState
) ->
    GuildId = maps:get(id, NewState, 0),
    lists:foldl(
        fun({SessionId, SessionData}, AccState) ->
            compute_channel_visibility_changes_for_session(
                SessionId,
                SessionData,
                ChannelIds,
                OldState,
                AccState,
                GuildId
            )
        end,
        NewState,
        SessionEntries
    ).

-spec compute_channel_visibility_changes_for_session(
    binary(), map(), [channel_id()], guild_state(), guild_state(), integer()
) ->
    guild_state().
compute_channel_visibility_changes_for_session(
    SessionId, SessionData, ChannelIds, OldState, NewState, GuildId
) ->
    UserId = maps:get(user_id, SessionData, undefined),
    case is_integer(UserId) of
        false ->
            NewState;
        true ->
            Pid = maps:get(pid, SessionData, undefined),
            OldMember = guild_permissions:find_member_by_user_id(UserId, OldState),
            ConnectedSet = connected_voice_channel_set(UserId, NewState),
            InitialViewableMap = ensure_viewable_channel_map(SessionData, UserId, OldState),
            {FinalViewableMap, StateAfterChannels} = lists:foldl(
                fun(ChannelId, {ViewableMapAcc, StateAcc}) ->
                    OldVisible = channel_is_visible(UserId, ChannelId, OldMember, OldState),
                    {StateAfterPreserve, NewVisible} = ensure_new_channel_visibility(
                        UserId,
                        ChannelId,
                        ConnectedSet,
                        StateAcc
                    ),
                    UpdatedViewableMap = update_viewable_map_for_channel(
                        ViewableMapAcc, ChannelId, NewVisible
                    ),
                    case {OldVisible, NewVisible} of
                        {true, false} ->
                            dispatch_channel_delete(ChannelId, Pid, OldState, GuildId),
                            {UpdatedViewableMap, StateAfterPreserve};
                        {false, true} ->
                            dispatch_channel_create(
                                ChannelId, Pid, StateAfterPreserve, GuildId
                            ),
                            send_member_list_sync(
                                SessionId,
                                SessionData,
                                ChannelId,
                                GuildId,
                                StateAfterPreserve
                            ),
                            {UpdatedViewableMap, StateAfterPreserve};
                        _ ->
                            {UpdatedViewableMap, StateAfterPreserve}
                    end
                end,
                {InitialViewableMap, NewState},
                ChannelIds
            ),
            guild_sessions:set_session_viewable_channels(
                SessionId,
                FinalViewableMap,
                StateAfterChannels
            )
    end.

-spec ensure_viewable_channel_map(map(), user_id(), guild_state()) -> #{channel_id() => true}.
ensure_viewable_channel_map(SessionData, UserId, State) ->
    case maps:get(viewable_channels, SessionData, undefined) of
        ViewableChannels when is_map(ViewableChannels) ->
            ViewableChannels;
        _ ->
            viewable_channel_map(sets:from_list(get_user_viewable_channels(UserId, State)))
    end.

-spec ensure_new_channel_visibility(
    user_id(), channel_id(), sets:set(channel_id()), guild_state()
) ->
    {guild_state(), boolean()}.
ensure_new_channel_visibility(UserId, ChannelId, ConnectedSet, State) ->
    NewMember = guild_permissions:find_member_by_user_id(UserId, State),
    NewVisible0 = channel_is_visible(UserId, ChannelId, NewMember, State),
    case {NewVisible0, sets:is_element(ChannelId, ConnectedSet)} of
        {true, _} ->
            {State, true};
        {false, false} ->
            {State, false};
        {false, true} ->
            case guild_virtual_channel_access:has_virtual_access(UserId, ChannelId, State) of
                true ->
                    {State, true};
                false ->
                    State1 = guild_virtual_channel_access:add_virtual_access(
                        UserId,
                        ChannelId,
                        State
                    ),
                    State2 = guild_virtual_channel_access:clear_pending_join(
                        UserId,
                        ChannelId,
                        State1
                    ),
                    {State2, true}
            end
    end.

-spec channel_is_visible(user_id(), channel_id(), map() | undefined, guild_state()) -> boolean().
channel_is_visible(UserId, ChannelId, Member, State) ->
    guild_permissions:can_view_channel(UserId, ChannelId, Member, State).

-spec update_viewable_map_for_channel(map(), channel_id(), boolean()) -> map().
update_viewable_map_for_channel(ViewableMap, ChannelId, true) ->
    maps:put(ChannelId, true, ViewableMap);
update_viewable_map_for_channel(ViewableMap, ChannelId, false) ->
    maps:remove(ChannelId, ViewableMap).

-spec compute_and_dispatch_visibility_changes_for_sessions(
    [{binary(), map()}], guild_state(), guild_state()
) -> guild_state().
compute_and_dispatch_visibility_changes_for_sessions(SessionEntries, OldState, NewState) ->
    GuildId = maps:get(id, NewState, 0),
    lists:foldl(
        fun({SessionId, SessionData}, AccState) ->
            UserId = maps:get(user_id, SessionData),
            Pid = maps:get(pid, SessionData),
            OldSet = cached_viewable_channel_set(SessionData, UserId, OldState),
            NewViewable = get_user_viewable_channels(UserId, AccState),
            NewSet = sets:from_list(NewViewable),
            ConnectedSet = connected_voice_channel_set(UserId, AccState),
            Removed0 = sets:subtract(OldSet, NewSet),
            {StateWithAccess, PreservedSet} = preserve_connected_channels(
                UserId,
                Removed0,
                ConnectedSet,
                AccState
            ),
            NewSet2 = sets:union(NewSet, PreservedSet),
            StateWithCachedVisibility = guild_sessions:set_session_viewable_channels(
                SessionId,
                viewable_channel_map(NewSet2),
                StateWithAccess
            ),
            Removed = sets:subtract(OldSet, NewSet2),
            Added = sets:subtract(NewSet2, OldSet),
            lists:foreach(
                fun(ChannelId) ->
                    dispatch_channel_delete(ChannelId, Pid, OldState, GuildId)
                end,
                sets:to_list(Removed)
            ),
            lists:foreach(
                fun(ChannelId) ->
                    dispatch_channel_create(ChannelId, Pid, StateWithCachedVisibility, GuildId),
                    send_member_list_sync(
                        SessionId, SessionData, ChannelId, GuildId, StateWithCachedVisibility
                    )
                end,
                sets:to_list(Added)
            ),
            StateWithCachedVisibility
        end,
        NewState,
        SessionEntries
    ).

-spec cached_viewable_channel_set(map(), user_id(), guild_state()) -> sets:set(channel_id()).
cached_viewable_channel_set(SessionData, UserId, State) ->
    case maps:get(viewable_channels, SessionData, undefined) of
        ViewableMap when is_map(ViewableMap) ->
            sets:from_list(maps:keys(ViewableMap));
        _ ->
            sets:from_list(get_user_viewable_channels(UserId, State))
    end.

-spec viewable_channel_map(sets:set(channel_id())) -> #{channel_id() => true}.
viewable_channel_map(ChannelSet) ->
    sets:fold(
        fun(ChannelId, Acc) ->
            maps:put(ChannelId, true, Acc)
        end,
        #{},
        ChannelSet
    ).

-spec connected_voice_channel_set(user_id(), guild_state()) -> sets:set(channel_id()).
connected_voice_channel_set(UserId, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    maps:fold(
        fun(_ConnId, VoiceState, Acc) ->
            case voice_state_utils:voice_state_user_id(VoiceState) of
                UserId ->
                    case voice_state_utils:voice_state_channel_id(VoiceState) of
                        undefined -> Acc;
                        ChannelId -> sets:add_element(ChannelId, Acc)
                    end;
                _ ->
                    Acc
            end
        end,
        sets:new(),
        VoiceStates
    ).

-spec preserve_connected_channels(
    user_id(), sets:set(channel_id()), sets:set(channel_id()), guild_state()
) ->
    {guild_state(), sets:set(channel_id())}.
preserve_connected_channels(UserId, RemovedSet, ConnectedSet, State) ->
    ToPreserve = sets:intersection(RemovedSet, ConnectedSet),
    UpdatedState = sets:fold(
        fun(ChannelId, AccState) ->
            case guild_virtual_channel_access:has_virtual_access(UserId, ChannelId, AccState) of
                true ->
                    AccState;
                false ->
                    State1 = guild_virtual_channel_access:add_virtual_access(
                        UserId, ChannelId, AccState
                    ),
                    guild_virtual_channel_access:clear_pending_join(UserId, ChannelId, State1)
            end
        end,
        State,
        ToPreserve
    ),
    {UpdatedState, ToPreserve}.

-spec dispatch_channel_delete(channel_id(), pid(), guild_state(), integer()) -> ok.
dispatch_channel_delete(ChannelId, SessionPid, OldState, GuildId) ->
    case is_pid(SessionPid) of
        true ->
            case guild_permissions:find_channel_by_id(ChannelId, OldState) of
                undefined ->
                    ok;
                _Channel ->
                    ChannelDelete = #{
                        <<"id">> => integer_to_binary(ChannelId),
                        <<"guild_id">> => integer_to_binary(GuildId)
                    },
                    gen_server:cast(SessionPid, {dispatch, channel_delete, ChannelDelete})
            end;
        false ->
            ok
    end.

-spec dispatch_channel_create(channel_id(), pid(), guild_state(), integer()) -> ok.
dispatch_channel_create(ChannelId, SessionPid, NewState, GuildId) ->
    case is_pid(SessionPid) of
        true ->
            case guild_permissions:find_channel_by_id(ChannelId, NewState) of
                undefined ->
                    ok;
                Channel ->
                    ChannelWithGuild = maps:put(
                        <<"guild_id">>, integer_to_binary(GuildId), Channel
                    ),
                    gen_server:cast(SessionPid, {dispatch, channel_create, ChannelWithGuild})
            end;
        false ->
            ok
    end.

-spec send_member_list_sync(binary(), map(), channel_id(), integer(), guild_state()) -> ok.
send_member_list_sync(SessionId, SessionData, ChannelId, GuildId, State) ->
    SessionPid = maps:get(pid, SessionData),
    case is_pid(SessionPid) of
        false ->
            ok;
        true ->
            ListId = guild_member_list:calculate_list_id(ChannelId, State),
            MemberListSubs = maps:get(member_list_subscriptions, State, #{}),
            ListSubs = maps:get(ListId, MemberListSubs, #{}),
            Ranges = maps:get(SessionId, ListSubs, []),
            case Ranges of
                [] ->
                    ok;
                _ ->
                    SessionUserId = maps:get(user_id, SessionData),
                    case can_send_member_list(SessionUserId, ChannelId, State) of
                        true ->
                            SyncResponse = guild_member_list:build_sync_response(
                                GuildId, ListId, Ranges, State
                            ),
                            SyncResponseWithChannel = maps:put(
                                <<"channel_id">>, integer_to_binary(ChannelId), SyncResponse
                            ),
                            gen_server:cast(
                                SessionPid,
                                {dispatch, guild_member_list_update, SyncResponseWithChannel}
                            );
                        false ->
                            ok
                    end
            end
    end.

-spec can_send_member_list(user_id() | undefined, channel_id(), guild_state()) -> boolean().
can_send_member_list(UserId, ChannelId, State) ->
    is_integer(UserId) andalso
        guild_permissions:can_view_channel(UserId, ChannelId, undefined, State).

-ifdef(TEST).

get_user_viewable_channels_returns_empty_for_non_member_test() ->
    State = #{
        data => #{
            <<"channels">> => [#{<<"id">> => <<"100">>, <<"type">> => 0}],
            <<"members">> => []
        }
    },
    ?assertEqual([], get_user_viewable_channels(999, State)).

viewable_channel_set_returns_empty_for_invalid_user_test() ->
    State = #{data => #{}},
    ?assertEqual(sets:new(), viewable_channel_set(undefined, State)).

have_shared_viewable_channel_same_user_test() ->
    State = #{data => #{}},
    ?assertEqual(false, have_shared_viewable_channel(100, 100, State)).

preserves_connected_channel_visibility_on_permission_loss_test() ->
    UserId = 10,
    GuildId = 1,
    ChannelId = 5,
    ViewPerm = constants:view_channel_permission(),
    OldState = visibility_state(GuildId, UserId, ChannelId, ViewPerm, true),
    NewState = visibility_state(GuildId, UserId, ChannelId, 0, true),
    UpdatedState = compute_and_dispatch_visibility_changes(OldState, NewState),
    ?assert(guild_virtual_channel_access:has_virtual_access(UserId, ChannelId, UpdatedState)),
    ?assertEqual(
        false,
        guild_virtual_channel_access:is_pending_join(UserId, ChannelId, UpdatedState)
    ),
    ?assert(guild_permissions:can_view_channel(UserId, ChannelId, undefined, UpdatedState)).

does_not_add_virtual_access_when_not_connected_test() ->
    UserId = 20,
    GuildId = 2,
    ChannelId = 6,
    ViewPerm = constants:view_channel_permission(),
    OldState = visibility_state(GuildId, UserId, ChannelId, ViewPerm, false),
    NewState = visibility_state(GuildId, UserId, ChannelId, 0, false),
    UpdatedState = compute_and_dispatch_visibility_changes(OldState, NewState),
    ?assertNot(guild_virtual_channel_access:has_virtual_access(UserId, ChannelId, UpdatedState)).

does_not_add_virtual_access_when_permission_remains_test() ->
    UserId = 30,
    GuildId = 3,
    ChannelId = 7,
    ViewPerm = constants:view_channel_permission(),
    OldState = visibility_state(GuildId, UserId, ChannelId, ViewPerm, true),
    NewState = visibility_state(GuildId, UserId, ChannelId, ViewPerm, true),
    UpdatedState = compute_and_dispatch_visibility_changes(OldState, NewState),
    ?assertNot(guild_virtual_channel_access:has_virtual_access(UserId, ChannelId, UpdatedState)).

compute_and_dispatch_visibility_changes_for_users_targets_selected_users_test() ->
    GuildId = 33,
    ChannelId = 77,
    RoleId = 101,
    ViewPerm = constants:view_channel_permission(),
    Session10 = #{
        session_id => <<"s10">>,
        user_id => 10,
        pid => self(),
        viewable_channels => #{ChannelId => true}
    },
    Session20 = #{
        session_id => <<"s20">>,
        user_id => 20,
        pid => self(),
        viewable_channels => #{ChannelId => true}
    },
    OldState = #{
        id => GuildId,
        sessions => #{<<"s10">> => Session10, <<"s20">> => Session20},
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>},
                #{<<"id">> => integer_to_binary(RoleId), <<"permissions">> => integer_to_binary(ViewPerm)}
            ],
            <<"members">> => #{
                10 => #{<<"user">> => #{<<"id">> => <<"10">>}, <<"roles">> => [integer_to_binary(RoleId)]},
                20 => #{<<"user">> => #{<<"id">> => <<"20">>}, <<"roles">> => [integer_to_binary(RoleId)]}
            },
            <<"channels">> => [#{<<"id">> => integer_to_binary(ChannelId), <<"permission_overwrites">> => []}]
        }
    },
    NewState = #{
        id => GuildId,
        sessions => #{<<"s10">> => Session10, <<"s20">> => Session20},
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>},
                #{<<"id">> => integer_to_binary(RoleId), <<"permissions">> => integer_to_binary(ViewPerm)}
            ],
            <<"members">> => #{
                10 => #{<<"user">> => #{<<"id">> => <<"10">>}, <<"roles">> => []},
                20 => #{<<"user">> => #{<<"id">> => <<"20">>}, <<"roles">> => [integer_to_binary(RoleId)]}
            },
            <<"channels">> => [#{<<"id">> => integer_to_binary(ChannelId), <<"permission_overwrites">> => []}]
        }
    },
    UpdatedState = compute_and_dispatch_visibility_changes_for_users([10], OldState, NewState),
    UpdatedSessions = maps:get(sessions, UpdatedState),
    UpdatedSession10 = maps:get(<<"s10">>, UpdatedSessions),
    UpdatedSession20 = maps:get(<<"s20">>, UpdatedSessions),
    ?assertEqual(false, maps:is_key(ChannelId, maps:get(viewable_channels, UpdatedSession10, #{}))),
    ?assertEqual(true, maps:is_key(ChannelId, maps:get(viewable_channels, UpdatedSession20, #{}))).

compute_and_dispatch_visibility_changes_for_channels_limits_to_changed_channels_test() ->
    GuildId = 44,
    UserId = 10,
    ChannelA = 100,
    ChannelB = 101,
    ViewPerm = constants:view_channel_permission(),
    BaseRole = #{
        <<"id">> => integer_to_binary(GuildId),
        <<"permissions">> => integer_to_binary(ViewPerm)
    },
    Session = #{
        session_id => <<"s10">>,
        user_id => UserId,
        pid => self(),
        viewable_channels => #{ChannelA => true, ChannelB => true}
    },
    OldState = #{
        id => GuildId,
        sessions => #{<<"s10">> => Session},
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [BaseRole],
            <<"members">> => #{
                UserId => #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => []
                }
            },
            <<"channels">> => [
                #{<<"id">> => integer_to_binary(ChannelA), <<"permission_overwrites">> => []},
                #{<<"id">> => integer_to_binary(ChannelB), <<"permission_overwrites">> => []}
            ]
        }
    },
    NewState = #{
        id => GuildId,
        sessions => #{<<"s10">> => Session},
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [BaseRole],
            <<"members">> => #{
                UserId => #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => []
                }
            },
            <<"channels">> => [
                #{
                    <<"id">> => integer_to_binary(ChannelA),
                    <<"permission_overwrites">> => [
                        #{
                            <<"id">> => integer_to_binary(GuildId),
                            <<"type">> => 0,
                            <<"allow">> => <<"0">>,
                            <<"deny">> => integer_to_binary(ViewPerm)
                        }
                    ]
                },
                #{<<"id">> => integer_to_binary(ChannelB), <<"permission_overwrites">> => []}
            ]
        }
    },
    UpdatedState = compute_and_dispatch_visibility_changes_for_channels(
        [ChannelA],
        OldState,
        NewState
    ),
    UpdatedSession = maps:get(<<"s10">>, maps:get(sessions, UpdatedState)),
    UpdatedViewable = maps:get(viewable_channels, UpdatedSession, #{}),
    ?assertEqual(false, maps:is_key(ChannelA, UpdatedViewable)),
    ?assertEqual(true, maps:is_key(ChannelB, UpdatedViewable)).

visibility_state(GuildId, UserId, ChannelId, Perms, Connected) ->
    VoiceStates =
        case Connected of
            true ->
                #{
                    <<"conn">> => #{
                        <<"user_id">> => integer_to_binary(UserId),
                        <<"guild_id">> => integer_to_binary(GuildId),
                        <<"channel_id">> => integer_to_binary(ChannelId),
                        <<"connection_id">> => <<"conn">>
                    }
                };
            false ->
                #{}
        end,
    Sessions = #{<<"s1">> => #{user_id => UserId, pid => self()}},
    Data = #{
        <<"guild">> => #{<<"owner_id">> => <<"999">>},
        <<"roles">> => [
            #{
                <<"id">> => integer_to_binary(GuildId),
                <<"permissions">> => integer_to_binary(Perms)
            }
        ],
        <<"members">> => [
            #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => []}
        ],
        <<"channels">> => [
            #{<<"id">> => integer_to_binary(ChannelId), <<"permission_overwrites">> => []}
        ]
    },
    #{
        id => GuildId,
        data => Data,
        sessions => Sessions,
        voice_states => VoiceStates
    }.

filter_connected_session_entries_excludes_pending_test() ->
    Normal = #{session_id => <<"s1">>, user_id => 1, pending_connect => false},
    Pending = #{session_id => <<"s2">>, user_id => 2, pending_connect => true},
    NoPending = #{session_id => <<"s3">>, user_id => 3},
    Sessions = #{<<"s1">> => Normal, <<"s2">> => Pending, <<"s3">> => NoPending},
    Result = filter_connected_session_entries(Sessions),
    ResultIds = lists:sort([Sid || {Sid, _} <- Result]),
    ?assertEqual([<<"s1">>, <<"s3">>], ResultIds).

administrator_sees_all_channels_test() ->
    GuildId = 50,
    UserId = 10,
    ChannelId = 100,
    Admin = constants:administrator_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => integer_to_binary(Admin)}
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => []}
            ],
            <<"channels">> => [
                #{
                    <<"id">> => integer_to_binary(ChannelId),
                    <<"permission_overwrites">> => [
                        #{
                            <<"id">> => integer_to_binary(GuildId),
                            <<"type">> => 0,
                            <<"allow">> => <<"0">>,
                            <<"deny">> => integer_to_binary(constants:view_channel_permission())
                        }
                    ]
                }
            ]
        }
    },
    Channels = get_user_viewable_channels(UserId, State),
    ?assertEqual([ChannelId], Channels).

owner_sees_all_channels_test() ->
    GuildId = 60,
    OwnerId = 10,
    ChannelId = 200,
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => integer_to_binary(OwnerId)},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>}
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => integer_to_binary(OwnerId)}, <<"roles">> => []}
            ],
            <<"channels">> => [
                #{<<"id">> => integer_to_binary(ChannelId), <<"permission_overwrites">> => []}
            ]
        }
    },
    Channels = get_user_viewable_channels(OwnerId, State),
    ?assertEqual([ChannelId], Channels).

everyone_role_grants_view_test() ->
    GuildId = 70,
    UserId = 10,
    ChannelId = 300,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => integer_to_binary(ViewPerm)}
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => []}
            ],
            <<"channels">> => [
                #{<<"id">> => integer_to_binary(ChannelId), <<"permission_overwrites">> => []}
            ]
        }
    },
    Channels = get_user_viewable_channels(UserId, State),
    ?assertEqual([ChannelId], Channels).

channel_overwrite_denies_view_test() ->
    GuildId = 80,
    UserId = 10,
    RoleId = 200,
    ChannelId = 400,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => integer_to_binary(ViewPerm)},
                #{<<"id">> => integer_to_binary(RoleId), <<"permissions">> => <<"0">>}
            ],
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => [integer_to_binary(RoleId)]
                }
            ],
            <<"channels">> => [
                #{
                    <<"id">> => integer_to_binary(ChannelId),
                    <<"permission_overwrites">> => [
                        #{
                            <<"id">> => integer_to_binary(RoleId),
                            <<"type">> => 0,
                            <<"allow">> => <<"0">>,
                            <<"deny">> => integer_to_binary(ViewPerm)
                        }
                    ]
                }
            ]
        }
    },
    Channels = get_user_viewable_channels(UserId, State),
    ?assertEqual([], Channels).

role_overwrite_allows_view_test() ->
    GuildId = 90,
    UserId = 10,
    RoleId = 300,
    ChannelId = 500,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => integer_to_binary(ViewPerm)},
                #{<<"id">> => integer_to_binary(RoleId), <<"permissions">> => <<"0">>}
            ],
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => [integer_to_binary(RoleId)]
                }
            ],
            <<"channels">> => [
                #{
                    <<"id">> => integer_to_binary(ChannelId),
                    <<"permission_overwrites">> => [
                        #{
                            <<"id">> => integer_to_binary(GuildId),
                            <<"type">> => 0,
                            <<"allow">> => <<"0">>,
                            <<"deny">> => integer_to_binary(ViewPerm)
                        },
                        #{
                            <<"id">> => integer_to_binary(RoleId),
                            <<"type">> => 0,
                            <<"allow">> => integer_to_binary(ViewPerm),
                            <<"deny">> => <<"0">>
                        }
                    ]
                }
            ]
        }
    },
    Channels = get_user_viewable_channels(UserId, State),
    ?assertEqual([ChannelId], Channels).

user_overwrite_denies_view_test() ->
    GuildId = 91,
    UserId = 10,
    RoleId = 301,
    ChannelId = 501,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => integer_to_binary(ViewPerm)},
                #{<<"id">> => integer_to_binary(RoleId), <<"permissions">> => <<"0">>}
            ],
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => [integer_to_binary(RoleId)]
                }
            ],
            <<"channels">> => [
                #{
                    <<"id">> => integer_to_binary(ChannelId),
                    <<"permission_overwrites">> => [
                        #{
                            <<"id">> => integer_to_binary(RoleId),
                            <<"type">> => 0,
                            <<"allow">> => integer_to_binary(ViewPerm),
                            <<"deny">> => <<"0">>
                        },
                        #{
                            <<"id">> => integer_to_binary(UserId),
                            <<"type">> => 1,
                            <<"allow">> => <<"0">>,
                            <<"deny">> => integer_to_binary(ViewPerm)
                        }
                    ]
                }
            ]
        }
    },
    Channels = get_user_viewable_channels(UserId, State),
    ?assertEqual([], Channels).

viewable_channel_set_uses_cached_session_data_test() ->
    UserId = 10,
    State = #{
        sessions => #{
            <<"s1">> => #{
                user_id => UserId,
                viewable_channels => #{100 => true, 200 => true}
            }
        },
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"members">> => [],
            <<"channels">> => [],
            <<"roles">> => []
        }
    },
    ChannelSet = viewable_channel_set(UserId, State),
    ?assertEqual(true, sets:is_element(100, ChannelSet)),
    ?assertEqual(true, sets:is_element(200, ChannelSet)),
    ?assertEqual(false, sets:is_element(999, ChannelSet)).

have_shared_viewable_channel_shared_test() ->
    GuildId = 100,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        sessions => #{},
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => integer_to_binary(ViewPerm)}
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => <<"10">>}, <<"roles">> => []},
                #{<<"user">> => #{<<"id">> => <<"20">>}, <<"roles">> => []}
            ],
            <<"channels">> => [
                #{<<"id">> => <<"500">>, <<"permission_overwrites">> => []}
            ]
        }
    },
    ?assertEqual(true, have_shared_viewable_channel(10, 20, State)).

have_shared_viewable_channel_no_shared_test() ->
    GuildId = 101,
    ViewPerm = constants:view_channel_permission(),
    RoleId = 200,
    State = #{
        id => GuildId,
        sessions => #{},
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>},
                #{<<"id">> => integer_to_binary(RoleId), <<"permissions">> => integer_to_binary(ViewPerm)}
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => <<"10">>}, <<"roles">> => [integer_to_binary(RoleId)]},
                #{<<"user">> => #{<<"id">> => <<"20">>}, <<"roles">> => []}
            ],
            <<"channels">> => [
                #{
                    <<"id">> => <<"500">>,
                    <<"permission_overwrites">> => [
                        #{
                            <<"id">> => integer_to_binary(GuildId),
                            <<"type">> => 0,
                            <<"allow">> => <<"0">>,
                            <<"deny">> => integer_to_binary(ViewPerm)
                        }
                    ]
                }
            ]
        }
    },
    ?assertEqual(false, have_shared_viewable_channel(10, 20, State)).

update_viewable_map_for_channel_add_test() ->
    Map = #{100 => true},
    Result = update_viewable_map_for_channel(Map, 200, true),
    ?assertEqual(true, maps:is_key(200, Result)),
    ?assertEqual(true, maps:is_key(100, Result)).

update_viewable_map_for_channel_remove_test() ->
    Map = #{100 => true, 200 => true},
    Result = update_viewable_map_for_channel(Map, 100, false),
    ?assertEqual(false, maps:is_key(100, Result)),
    ?assertEqual(true, maps:is_key(200, Result)).

multiple_channels_partial_visibility_test() ->
    GuildId = 110,
    UserId = 10,
    ViewPerm = constants:view_channel_permission(),
    RoleId = 300,
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => integer_to_binary(ViewPerm)},
                #{<<"id">> => integer_to_binary(RoleId), <<"permissions">> => <<"0">>}
            ],
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => []
                }
            ],
            <<"channels">> => [
                #{<<"id">> => <<"100">>, <<"permission_overwrites">> => []},
                #{
                    <<"id">> => <<"101">>,
                    <<"permission_overwrites">> => [
                        #{
                            <<"id">> => integer_to_binary(GuildId),
                            <<"type">> => 0,
                            <<"allow">> => <<"0">>,
                            <<"deny">> => integer_to_binary(ViewPerm)
                        }
                    ]
                },
                #{<<"id">> => <<"102">>, <<"permission_overwrites">> => []}
            ]
        }
    },
    Channels = lists:sort(get_user_viewable_channels(UserId, State)),
    ?assertEqual([100, 102], Channels).

viewable_channel_map_test() ->
    Set = sets:from_list([10, 20, 30]),
    Map = viewable_channel_map(Set),
    ?assertEqual(3, map_size(Map)),
    ?assertEqual(true, maps:get(10, Map)),
    ?assertEqual(true, maps:get(20, Map)),
    ?assertEqual(true, maps:get(30, Map)).

viewable_channel_map_empty_test() ->
    Map = viewable_channel_map(sets:new()),
    ?assertEqual(#{}, Map).

-endif.
