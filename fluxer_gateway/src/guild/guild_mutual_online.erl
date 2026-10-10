%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_mutual_online).
-typing([eqwalizer]).

-export([compute_count/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export_type([user_id/0, guild_state/0]).

-type user_id() :: integer().
-type guild_state() :: map().
-type viewable_index() :: #{user_id() => map()}.
-type index_ctx() :: {map(), viewable_index(), guild_state()}.
-type memo() :: guild_maintenance:viewable_memo().

-spec compute_count(user_id() | term(), guild_state()) -> non_neg_integer().
compute_count(UserId, State) when is_integer(UserId), UserId > 0 ->
    case viewer_sees_everything(UserId, State) of
        true ->
            guild_member_list:get_online_count(State);
        false ->
            slow_count(UserId, State)
    end;
compute_count(_, _) ->
    0.

-spec viewer_sees_everything(user_id(), guild_state()) -> boolean().
viewer_sees_everything(UserId, State) ->
    Perms = guild_permissions:get_member_permissions(UserId, undefined, State),
    permission_bits:has(Perms, constants:administrator_permission()).

-spec slow_count(user_id(), guild_state()) -> non_neg_integer().
slow_count(UserId, State) ->
    ViewerSet = guild_visibility:viewable_channel_set(UserId, State),
    case sets:is_empty(ViewerSet) of
        true ->
            self_online_count(UserId, State);
        false ->
            count_mutually_visible_indexed(UserId, ViewerSet, State)
    end.

-spec self_online_count(user_id(), guild_state()) -> non_neg_integer().
self_online_count(UserId, State) ->
    case is_self_online(UserId, State) of
        true -> 1;
        false -> 0
    end.

-spec count_mutually_visible_indexed(user_id(), sets:set(), guild_state()) ->
    non_neg_integer().
count_mutually_visible_indexed(UserId, ViewerSet, State) ->
    {Count, _Memo} = count_and_memo(UserId, ViewerSet, State),
    Count.

-spec count_and_memo(user_id(), sets:set(), guild_state()) ->
    {non_neg_integer(), memo() | undefined}.
count_and_memo(UserId, ViewerSet, State) ->
    Tab = maps:get(member_presence, State),
    Ctx = {viewer_channel_map(ViewerSet, State), build_viewable_index(State), State},
    sets:fold(
        fun(OtherUserId, Acc) ->
            Presence = guild_state_member:lookup_presence(Tab, OtherUserId),
            count_online_member_indexed(UserId, Ctx, OtherUserId, Presence, Acc)
        end,
        {0, undefined},
        guild_member_list_connected:connected_session_user_ids(State)
    ).

-spec count_online_member_indexed(
    user_id(), index_ctx(), term(), term(), {non_neg_integer(), memo() | undefined}
) -> {non_neg_integer(), memo() | undefined}.
count_online_member_indexed(UserId, Ctx, OtherUserId, Presence, {Count, Memo} = Acc) when
    is_integer(OtherUserId), is_map(Presence), OtherUserId > 0
->
    case is_online(Presence) of
        false ->
            Acc;
        true when OtherUserId =:= UserId ->
            {Count + 1, Memo};
        true ->
            case shares_viewable_channel(OtherUserId, Ctx, Memo) of
                {true, Memo1} -> {Count + 1, Memo1};
                {false, Memo1} -> {Count, Memo1}
            end
    end;
count_online_member_indexed(_UserId, _Ctx, _OtherUserId, _Presence, Acc) ->
    Acc.

-spec shares_viewable_channel(user_id(), index_ctx(), memo() | undefined) ->
    {boolean(), memo() | undefined}.
shares_viewable_channel(OtherUserId, {ViewerMap, Index, State}, Memo) ->
    case maps:find(OtherUserId, Index) of
        {ok, OtherMap} ->
            {maps_share_any_key(OtherMap, ViewerMap), Memo};
        error ->
            {OtherMap, Memo1} = guild_maintenance:memoised_member_viewable_channel_map(
                OtherUserId, State, ensure_memo(Memo, State)
            ),
            {maps_share_any_key(OtherMap, ViewerMap), Memo1}
    end.

-spec ensure_memo(memo() | undefined, guild_state()) -> memo().
ensure_memo(undefined, State) ->
    guild_maintenance:new_viewable_memo(State);
ensure_memo(Memo, _State) ->
    Memo.

-spec viewer_channel_map(sets:set(), guild_state()) -> map().
viewer_channel_map(ViewerSet, State) ->
    case guild_thread_gate:needs_variant(State) of
        false ->
            sets:fold(fun(ChannelId, Acc) -> Acc#{ChannelId => true} end, #{}, ViewerSet);
        true ->
            Index = guild_data_index:channel_index(maps:get(data, State, #{})),
            sets:fold(
                fun(ChannelId, Acc) -> put_shared(ChannelId, Index, Acc) end, #{}, ViewerSet
            )
    end.

-spec put_shared(term(), map(), map()) -> map().
put_shared(ChannelId, Index, Acc) ->
    case maps:get(ChannelId, Index, undefined) of
        #{<<"type">> := Type} ->
            case guild_thread_gate:is_thread_only_type(Type) of
                true -> Acc;
                false -> Acc#{ChannelId => true}
            end;
        _ ->
            Acc#{ChannelId => true}
    end.

-spec build_viewable_index(guild_state()) -> viewable_index().
build_viewable_index(State) ->
    build_index_from_sessions(maps:get(sessions, State, #{})).

-spec build_index_from_sessions(term()) -> viewable_index().
build_index_from_sessions(Sessions) when is_map(Sessions) ->
    build_index_iter(maps:iterator(Sessions), #{});
build_index_from_sessions(_) ->
    #{}.

-spec build_index_iter(maps:iterator(), viewable_index()) -> viewable_index().
build_index_iter(Iterator, Acc) ->
    case maps:next(Iterator) of
        none ->
            Acc;
        {_, SessionData, Next} when is_map(SessionData) ->
            build_index_iter(Next, index_session(SessionData, Acc));
        {_, _, Next} ->
            build_index_iter(Next, Acc)
    end.

-spec index_session(map(), viewable_index()) -> viewable_index().
index_session(SessionData, Acc) ->
    SessionUserId = maps:get(user_id, SessionData, undefined),
    ViewableChannels = maps:get(viewable_channels, SessionData, undefined),
    index_session_entry(SessionUserId, ViewableChannels, Acc).

-spec index_session_entry(term(), term(), viewable_index()) -> viewable_index().
index_session_entry(UserId, ViewableChannels, Acc) when
    is_integer(UserId), is_map(ViewableChannels)
->
    put_first_session_map(UserId, ViewableChannels, Acc);
index_session_entry(_, _, Acc) ->
    Acc.

-spec put_first_session_map(user_id(), map(), viewable_index()) -> viewable_index().
put_first_session_map(UserId, ViewableChannels, Acc) ->
    case maps:is_key(UserId, Acc) of
        true -> Acc;
        false -> Acc#{UserId => ViewableChannels}
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
        none -> false;
        {Key, _, NextIterator} -> key_matches_or_continue(Key, NextIterator, LargerMap)
    end.

-spec key_matches_or_continue(term(), maps:iterator(), map()) -> boolean().
key_matches_or_continue(Key, NextIterator, LargerMap) ->
    case maps:is_key(Key, LargerMap) of
        true -> true;
        false -> maps_share_any_key_iter(NextIterator, LargerMap)
    end.

-spec is_self_online(user_id(), guild_state()) -> boolean().
is_self_online(UserId, State) ->
    Connected = guild_member_list_connected:connected_session_user_ids(State),
    sets:is_element(UserId, Connected) andalso
        is_online(guild_state_member:lookup_presence(maps:get(member_presence, State), UserId)).

-spec is_online(map()) -> boolean().
is_online(Presence) ->
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    Status =/= <<"offline">> andalso Status =/= <<"invisible">>.

-ifdef(TEST).

view_perm() -> constants:view_channel_permission().
returns_zero_for_invalid_user_test() ->
    ?assertEqual(0, compute_count(0, #{})),
    ?assertEqual(0, compute_count(undefined, #{})),
    ?assertEqual(0, compute_count(-5, #{})).

slow_path_counts_only_mutually_visible_members_test() ->
    GuildId = 1,
    BotRoleId = 5000,
    State = mutual_visibility_state(GuildId, BotRoleId),
    Result = compute_count(10, State),
    ?assertEqual(2, Result).

forum_shared_only_with_a_member_is_not_counted_in_an_active_guild_test() ->
    State = mutual_visibility_state(1, 5000),
    Data = maps:get(data, State),
    Forum = #{
        <<"id">> => <<"200">>,
        <<"type">> => 15,
        <<"permission_overwrites">> => [
            user_view_overwrite(<<"10">>), user_view_overwrite(<<"40">>)
        ]
    },
    Members = maps:get(<<"members">>, Data),
    WithMember = Data#{
        <<"members">> => Members#{
            40 => #{<<"user">> => #{<<"id">> => <<"40">>}, <<"roles">> => []}
        }
    },
    WithForum = WithMember#{<<"channels">> => [Forum | maps:get(<<"channels">>, Data)]},
    Gate = #{thread_gate => #{active => true, version => 1}},
    ?assertEqual(3, compute_count(10, State#{data => WithForum})),
    ?assertEqual(
        compute_count(10, State#{data => maps:merge(WithMember, Gate)}),
        compute_count(10, State#{data => maps:merge(WithForum, Gate)})
    ),
    ?assertEqual(2, compute_count(10, State#{data => maps:merge(WithForum, Gate)})).

mutual_visibility_state(GuildId, BotRoleId) ->
    #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => mutual_visibility_roles(GuildId, BotRoleId),
            <<"members">> => mutual_visibility_members(BotRoleId),
            <<"channels">> => mutual_visibility_channels(BotRoleId)
        },
        member_presence => mutual_visibility_presence(),
        connected_user_ids => sets:from_list([10, 20, 30, 40]),
        sessions => #{}
    }.

mutual_visibility_roles(GuildId, BotRoleId) ->
    [
        #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>},
        #{<<"id">> => integer_to_binary(BotRoleId), <<"permissions">> => <<"0">>}
    ].

mutual_visibility_members(BotRoleId) ->
    #{
        10 => #{<<"user">> => #{<<"id">> => <<"10">>}, <<"roles">> => []},
        20 => #{
            <<"user">> => #{<<"id">> => <<"20">>},
            <<"roles">> => [integer_to_binary(BotRoleId)]
        },
        30 => #{<<"user">> => #{<<"id">> => <<"30">>}, <<"roles">> => []}
    }.

mutual_visibility_channels(BotRoleId) ->
    [channel_with_user_view_overwrites(), channel_with_role_view(BotRoleId)].

channel_with_role_view(BotRoleId) ->
    #{
        <<"id">> => <<"101">>,
        <<"type">> => 0,
        <<"permission_overwrites">> => [
            #{
                <<"id">> => integer_to_binary(BotRoleId),
                <<"type">> => 0,
                <<"allow">> => integer_to_binary(view_perm()),
                <<"deny">> => <<"0">>
            }
        ]
    }.

channel_with_user_view_overwrites() ->
    #{
        <<"id">> => <<"100">>,
        <<"type">> => 0,
        <<"permission_overwrites">> => [
            user_view_overwrite(<<"10">>),
            user_view_overwrite(<<"30">>)
        ]
    }.

user_view_overwrite(UserId) ->
    #{
        <<"id">> => UserId,
        <<"type">> => 1,
        <<"allow">> => integer_to_binary(view_perm()),
        <<"deny">> => <<"0">>
    }.

mutual_visibility_presence() ->
    make_presence_tab(#{
        10 => #{<<"status">> => <<"online">>},
        20 => #{<<"status">> => <<"online">>},
        30 => #{<<"status">> => <<"online">>},
        40 => #{<<"status">> => <<"online">>}
    }).

slow_path_returns_self_when_viewer_sees_no_channels_test() ->
    GuildId = 1,
    Roles = [#{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>}],
    Members = #{
        10 => #{<<"user">> => #{<<"id">> => <<"10">>}, <<"roles">> => []}
    },
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => Roles,
            <<"members">> => Members,
            <<"channels">> => []
        },
        member_presence => make_presence_tab(#{10 => #{<<"status">> => <<"online">>}}),
        connected_user_ids => sets:from_list([10]),
        sessions => #{}
    },
    ?assertEqual(1, compute_count(10, State)),
    ?assertEqual(0, compute_count(10, State#{connected_user_ids => sets:new()})).

slow_path_returns_zero_when_viewer_offline_and_no_channels_test() ->
    GuildId = 1,
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>}
            ],
            <<"members">> => #{
                10 => #{<<"user">> => #{<<"id">> => <<"10">>}, <<"roles">> => []}
            },
            <<"channels">> => []
        },
        member_presence => make_presence_tab(#{10 => #{<<"status">> => <<"offline">>}}),
        connected_user_ids => sets:from_list([10]),
        sessions => #{}
    },
    ?assertEqual(0, compute_count(10, State)).

index_counts_with_cached_session_channels_test() ->
    Base = mutual_visibility_state(1, 5000),
    State = Base#{sessions => cached_viewable_sessions()},
    ?assertEqual(3, compute_count(10, State)).

disconnected_online_row_is_not_counted_test() ->
    Base = mutual_visibility_state(1, 5000),
    State = Base#{connected_user_ids => sets:from_list([10, 20, 40])},
    ?assertNot(guild_member_list_connected:user_is_online(30, State)),
    ?assertEqual(1, compute_count(10, State)),
    ViewerSet = guild_visibility:viewable_channel_set(10, State),
    ?assertEqual(
        reference_count_mutually_visible(10, ViewerSet, State), compute_count(10, State)
    ).

disconnected_viewer_is_not_counted_test() ->
    Base = mutual_visibility_state(1, 5000),
    State = Base#{connected_user_ids => sets:from_list([20, 30, 40])},
    ?assertEqual(1, compute_count(10, State)).

counts_only_members_the_member_list_shows_online_test() ->
    Base = mutual_visibility_state(1, 5000),
    lists:foreach(
        fun(Connected) ->
            State = Base#{connected_user_ids => sets:from_list(Connected)},
            ViewerSet = guild_visibility:viewable_channel_set(10, State),
            Expected = length([
                U
             || U <- [10, 20, 30, 40],
                guild_member_list_connected:user_is_online(U, State),
                U =:= 10 orelse
                    not sets:is_empty(
                        sets:intersection(
                            ViewerSet, guild_visibility:viewable_channel_set(U, State)
                        )
                    )
            ]),
            ?assertEqual(Expected, compute_count(10, State))
        end,
        [[], [10], [30], [10, 30], [20, 40], [10, 20, 30, 40]]
    ).

cached_viewable_sessions() ->
    #{
        <<"s20a">> => #{user_id => 20, viewable_channels => undefined},
        <<"s20b">> => #{user_id => 20, viewable_channels => #{100 => true}},
        <<"s40">> => #{user_id => 40, viewable_channels => #{101 => true}}
    }.

-spec reference_count_mutually_visible(user_id(), sets:set(), guild_state()) ->
    non_neg_integer().
reference_count_mutually_visible(UserId, ViewerSet, State) ->
    Tab = maps:get(member_presence, State),
    ets:foldl(
        fun({OtherUserId, Presence}, Acc) ->
            reference_count_online_member(UserId, ViewerSet, State, OtherUserId, Presence, Acc)
        end,
        0,
        Tab
    ).

-spec reference_count_online_member(
    user_id(), sets:set(), guild_state(), term(), term(), non_neg_integer()
) -> non_neg_integer().
reference_count_online_member(UserId, ViewerSet, State, OtherUserId, Presence, Acc) when
    is_integer(OtherUserId), is_map(Presence), OtherUserId > 0
->
    Connected = sets:is_element(OtherUserId, maps:get(connected_user_ids, State)),
    case Connected andalso is_online(Presence) of
        false -> Acc;
        true when OtherUserId =:= UserId -> Acc + 1;
        true -> reference_count_if_mutually_visible(OtherUserId, ViewerSet, State, Acc)
    end;
reference_count_online_member(_UserId, _ViewerSet, _State, _OtherUserId, _Presence, Acc) ->
    Acc.

-spec reference_count_if_mutually_visible(
    user_id(), sets:set(), guild_state(), non_neg_integer()
) -> non_neg_integer().
reference_count_if_mutually_visible(OtherUserId, ViewerSet, State, Acc) ->
    OtherSet = guild_visibility:viewable_channel_set(OtherUserId, State),
    case sets:is_empty(sets:intersection(ViewerSet, OtherSet)) of
        true -> Acc;
        false -> Acc + 1
    end.

sessionless_user_overwrite_is_not_shared_with_same_roles_test() ->
    State = sessionless_role_state(
        [user_overwrite(<<"30">>, 0, view_perm())], #{}, <<"999">>, #{}
    ),
    ?assertEqual(2, compute_count(10, State)),
    ?assertEqual(2, reference_slow_count(10, State)).

sessionless_virtual_access_is_not_shared_with_same_roles_test() ->
    State = sessionless_role_state([], #{40 => sets:from_list([100])}, <<"999">>, #{}),
    ?assertEqual(4, compute_count(10, State)),
    ?assertEqual(4, reference_slow_count(10, State)).

sessionless_owner_is_not_shared_with_same_roles_test() ->
    State = sessionless_role_state([], #{}, <<"40">>, #{}),
    ?assertEqual(4, compute_count(10, State)),
    ?assertEqual(4, reference_slow_count(10, State)).

sessionless_members_with_different_base_permissions_test() ->
    State = sessionless_role_state(
        [], #{}, <<"999">>, #{10 => [5000, 6000], 40 => [6000], 50 => [6001]}
    ),
    ?assertEqual(4, compute_count(10, State)),
    ?assertEqual(4, reference_slow_count(10, State)).

sessionless_members_with_reordered_and_repeated_roles_test() ->
    State = sessionless_role_state(
        [], #{}, <<"999">>, #{20 => [6001, 5000], 30 => [5000, 5000, 6001]}
    ),
    ?assertEqual(3, compute_count(10, State)),
    ?assertEqual(3, reference_slow_count(10, State)).

sessionless_role_state(ExtraOverwrites, VirtualAccess, OwnerId, RoleOverrides) ->
    RoleId = 5000,
    Roles = maps:merge(
        #{10 => [RoleId], 20 => [RoleId], 30 => [RoleId], 40 => [], 50 => []}, RoleOverrides
    ),
    Member = fun(Id) ->
        #{
            <<"user">> => #{<<"id">> => integer_to_binary(Id)},
            <<"roles">> => [integer_to_binary(R) || R <- maps:get(Id, Roles)]
        }
    end,
    #{
        id => 1,
        data => #{
            <<"guild">> => #{<<"owner_id">> => OwnerId},
            <<"roles">> =>
                mutual_visibility_roles(1, RoleId) ++
                [
                    #{
                        <<"id">> => <<"6000">>,
                        <<"permissions">> => integer_to_binary(view_perm())
                    },
                    #{<<"id">> => <<"6001">>, <<"permissions">> => <<"0">>}
                ],
            <<"members">> => maps:from_list([{Id, Member(Id)} || Id <- [10, 20, 30, 40, 50]]),
            <<"channels">> => [
                #{
                    <<"id">> => <<"100">>,
                    <<"type">> => 0,
                    <<"permission_overwrites">> => [
                        overwrite(<<"1">>, 0, 0, view_perm()),
                        overwrite(integer_to_binary(RoleId), 0, view_perm(), 0)
                        | ExtraOverwrites
                    ]
                },
                #{
                    <<"id">> => <<"101">>,
                    <<"type">> => 0,
                    <<"permission_overwrites">> => [overwrite(<<"1">>, 0, 0, view_perm())]
                },
                #{<<"id">> => <<"102">>, <<"type">> => 0, <<"permission_overwrites">> => []}
            ]
        },
        virtual_channel_access => VirtualAccess,
        member_presence => make_presence_tab(
            maps:from_keys([10, 20, 30, 40, 50], #{<<"status">> => <<"online">>})
        ),
        connected_user_ids => sets:from_list([10, 20, 30, 40, 50]),
        sessions => #{}
    }.

user_overwrite(UserId, Allow, Deny) ->
    overwrite(UserId, 1, Allow, Deny).

overwrite(Id, Type, Allow, Deny) ->
    #{
        <<"id">> => Id,
        <<"type">> => Type,
        <<"allow">> => integer_to_binary(Allow),
        <<"deny">> => integer_to_binary(Deny)
    }.

reference_slow_count(UserId, State) ->
    ViewerSet = guild_visibility:viewable_channel_set(UserId, State),
    case sets:is_empty(ViewerSet) of
        true -> self_online_count(UserId, State);
        false -> reference_count_mutually_visible(UserId, ViewerSet, State)
    end.

make_presence_tab(Map) ->
    Tab = ets:new(test_member_presence, [set, public]),
    maps:foreach(fun(K, V) -> ets:insert(Tab, {K, V}) end, Map),
    Tab.

-endif.
