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
    Tab = maps:get(member_presence, State),
    Ctx = {viewer_channel_map(ViewerSet), build_viewable_index(State), State},
    ets:foldl(
        fun({OtherUserId, Presence}, Acc) ->
            count_online_member_indexed(UserId, Ctx, OtherUserId, Presence, Acc)
        end,
        0,
        Tab
    ).

-spec count_online_member_indexed(
    user_id(), index_ctx(), term(), term(), non_neg_integer()
) -> non_neg_integer().
count_online_member_indexed(UserId, Ctx, OtherUserId, Presence, Acc) when
    is_integer(OtherUserId), is_map(Presence), OtherUserId > 0
->
    case is_online(Presence) of
        false -> Acc;
        true when OtherUserId =:= UserId -> Acc + 1;
        true -> count_if_mutually_visible_indexed(OtherUserId, Ctx, Acc)
    end;
count_online_member_indexed(_UserId, _Ctx, _OtherUserId, _Presence, Acc) ->
    Acc.

-spec count_if_mutually_visible_indexed(user_id(), index_ctx(), non_neg_integer()) ->
    non_neg_integer().
count_if_mutually_visible_indexed(OtherUserId, Ctx, Acc) ->
    case shares_viewable_channel(OtherUserId, Ctx) of
        true -> Acc + 1;
        false -> Acc
    end.

-spec shares_viewable_channel(user_id(), index_ctx()) -> boolean().
shares_viewable_channel(OtherUserId, {ViewerMap, Index, State}) ->
    case maps:find(OtherUserId, Index) of
        {ok, OtherMap} -> maps_share_any_key(OtherMap, ViewerMap);
        error -> channel_list_shares_any(OtherUserId, ViewerMap, State)
    end.

-spec channel_list_shares_any(user_id(), map(), guild_state()) -> boolean().
channel_list_shares_any(OtherUserId, ViewerMap, State) ->
    Channels = guild_visibility:get_user_viewable_channels(OtherUserId, State),
    lists:any(fun(ChannelId) -> maps:is_key(ChannelId, ViewerMap) end, Channels).

-spec viewer_channel_map(sets:set()) -> map().
viewer_channel_map(ViewerSet) ->
    sets:fold(fun(ChannelId, Acc) -> Acc#{ChannelId => true} end, #{}, ViewerSet).

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
    Tab = maps:get(member_presence, State),
    case ets:lookup(Tab, UserId) of
        [{_, P}] -> is_online(P);
        [] -> false
    end.

-spec is_online(term()) -> boolean().
is_online(Presence) when is_map(Presence) ->
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    Status =/= <<"offline">> andalso Status =/= <<"invisible">>;
is_online(_) ->
    false.

-ifdef(TEST).

view_perm() -> constants:view_channel_permission().
admin_perm() -> constants:administrator_permission().

returns_zero_for_invalid_user_test() ->
    ?assertEqual(0, compute_count(0, #{})),
    ?assertEqual(0, compute_count(undefined, #{})),
    ?assertEqual(0, compute_count(-5, #{})).

admin_viewer_returns_count_without_member_list_store_test() ->
    GuildId = 1,
    AdminRoleId = 9001,
    State = admin_viewer_state(GuildId, AdminRoleId),
    Result = compute_count(100, State),
    ?assert(is_integer(Result) andalso Result >= 0).

admin_viewer_state(GuildId, AdminRoleId) ->
    Roles = [
        #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>},
        #{
            <<"id">> => integer_to_binary(AdminRoleId),
            <<"permissions">> => integer_to_binary(admin_perm())
        }
    ],
    Members = #{
        100 => #{
            <<"user">> => #{<<"id">> => <<"100">>},
            <<"roles">> => [integer_to_binary(AdminRoleId)]
        }
    },
    #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => Roles,
            <<"members">> => Members,
            <<"channels">> => [
                #{<<"id">> => <<"5">>, <<"type">> => 0, <<"permission_overwrites">> => []}
            ]
        },
        member_presence => make_presence_tab(#{100 => #{<<"status">> => <<"online">>}}),
        sessions => #{}
    }.

slow_path_counts_only_mutually_visible_members_test() ->
    GuildId = 1,
    BotRoleId = 5000,
    State = mutual_visibility_state(GuildId, BotRoleId),
    Result = compute_count(10, State),
    ?assertEqual(2, Result).

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
        sessions => #{}
    },
    ?assertEqual(1, compute_count(10, State)).

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
        sessions => #{}
    },
    ?assertEqual(0, compute_count(10, State)).

index_matches_scan_without_sessions_test() ->
    State = mutual_visibility_state(1, 5000),
    ViewerSet = guild_visibility:viewable_channel_set(10, State),
    Expected = reference_count_mutually_visible(10, ViewerSet, State),
    ?assertEqual(2, Expected),
    ?assertEqual(Expected, compute_count(10, State)).

index_matches_scan_with_cached_session_channels_test() ->
    Base = mutual_visibility_state(1, 5000),
    State = Base#{sessions => cached_viewable_sessions()},
    ViewerSet = guild_visibility:viewable_channel_set(10, State),
    Expected = reference_count_mutually_visible(10, ViewerSet, State),
    ?assertEqual(3, Expected),
    ?assertEqual(Expected, compute_count(10, State)).

index_counts_with_cached_session_channels_test() ->
    Base = mutual_visibility_state(1, 5000),
    State = Base#{sessions => cached_viewable_sessions()},
    ?assertEqual(3, compute_count(10, State)).

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
    case is_online(Presence) of
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

make_presence_tab(Map) ->
    Tab = ets:new(test_member_presence, [set, public]),
    maps:foreach(fun(K, V) -> ets:insert(Tab, {K, V}) end, Map),
    Tab.

-endif.
