%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_presence_sync).
-typing([eqwalizer]).

-export([
    sync_online_status/2,
    sync_member_data/2,
    partition_subscribed_sessions/5,
    get_user_viewable_channel_map/3,
    session_pids/2
]).

-export_type([guild_state/0, user_id/0]).

-type guild_state() :: map().
-type user_id() :: integer().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec sync_online_status(user_id(), guild_state()) -> ok.
sync_online_status(UserId, State) ->
    IsOnline = compute_is_online(UserId, State),
    case maps:find(member_list_engine, State) of
        error ->
            ok;
        {ok, Ref} ->
            guild_member_list_store:set_online(Ref, UserId, IsOnline)
    end,
    guild_member_list_channel_engine:sync_online(UserId, IsOnline, State).

-spec sync_member_data(user_id(), guild_state()) -> ok.
sync_member_data(UserId, State) ->
    Data = maps:get(data, State, #{}),
    case guild_data_index:get_member(UserId, Data) of
        undefined -> ok;
        Member -> sync_local_nif_member_data(UserId, Member, State)
    end.

-spec sync_local_nif_member_data(user_id(), map(), guild_state()) -> ok.
sync_local_nif_member_data(UserId, Member, State) ->
    ok = sync_default_member_list_data(UserId, Member, State),
    guild_member_list_channel_engine:update_user_all(UserId, State).

-spec sync_default_member_list_data(user_id(), map(), guild_state()) -> ok.
sync_default_member_list_data(UserId, Member, State) ->
    case maps:find(member_list_engine, State) of
        error ->
            ok;
        {ok, Ref} ->
            DisplayName = guild_member_list_common:get_member_display_name(Member),
            SortKey = guild_member_list_common:casefold_binary(DisplayName),
            RoleIds = guild_member_list_store:extract_role_ids(Member),
            IsOnline = compute_is_online(UserId, State),
            guild_member_list_store:update_member(Ref, UserId, SortKey, RoleIds, IsOnline)
    end.

-spec compute_is_online(user_id(), guild_state()) -> boolean().
compute_is_online(UserId, State) ->
    ConnectedUserIds = guild_member_list_common:connected_session_user_ids(State),
    Presence = guild_state_member:lookup_presence(maps:get(member_presence, State), UserId),
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    IsConnected = sets:is_element(UserId, ConnectedUserIds),
    IsConnected andalso Status =/= <<"offline">> andalso Status =/= <<"invisible">>.

-spec partition_subscribed_sessions([binary()], map(), map(), user_id(), guild_state()) ->
    {[binary()], [binary()]}.
partition_subscribed_sessions(SessionIds, Sessions, TargetChannelMap, TargetUserId, State) ->
    lists:foldl(
        fun(SessionId, {Valids, Invalids}) ->
            partition_session(SessionId, Sessions, TargetChannelMap, TargetUserId, State, {
                Valids, Invalids
            })
        end,
        {[], []},
        SessionIds
    ).

-spec partition_session(
    binary(), map(), map(), user_id(), guild_state(), {[binary()], [binary()]}
) -> {[binary()], [binary()]}.
partition_session(
    SessionId, Sessions, TargetChannelMap, TargetUserId, State, {Valids, Invalids}
) ->
    case classify_session(SessionId, Sessions, TargetChannelMap, TargetUserId, State) of
        valid -> {[SessionId | Valids], Invalids};
        invalid -> {Valids, [SessionId | Invalids]}
    end.

-spec classify_session(binary(), map(), map(), user_id(), guild_state()) -> valid | invalid.
classify_session(SessionId, Sessions, TargetChannelMap, TargetUserId, State) ->
    case maps:find(SessionId, Sessions) of
        error ->
            invalid;
        {ok, SessionData} ->
            classify_session_data(SessionData, TargetChannelMap, TargetUserId, State)
    end.

-spec classify_session_data(map(), map(), user_id(), guild_state()) -> valid | invalid.
classify_session_data(SessionData, TargetChannelMap, TargetUserId, State) ->
    SessionUserId = maps:get(user_id, SessionData, undefined),
    case SessionUserId of
        undefined -> invalid;
        TargetUserId -> invalid;
        _ -> classify_non_target_session(SessionData, SessionUserId, TargetChannelMap, State)
    end.

-spec classify_non_target_session(map(), user_id(), map(), guild_state()) -> valid | invalid.
classify_non_target_session(SessionData, SessionUserId, TargetChannelMap, State) ->
    case session_shares_channels(SessionData, SessionUserId, TargetChannelMap, State) of
        true -> valid;
        false -> invalid
    end.

-spec session_shares_channels(map(), user_id(), map(), guild_state()) -> boolean().
session_shares_channels(SessionData, SessionUserId, TargetChannelMap, State) ->
    case maps:find(viewable_channels, SessionData) of
        {ok, ViewableMap} when is_map(ViewableMap) ->
            maps_share_any_key(ViewableMap, TargetChannelMap);
        _ ->
            session_channel_list_shares(SessionUserId, TargetChannelMap, State)
    end.

-spec session_channel_list_shares(user_id(), map(), guild_state()) -> boolean().
session_channel_list_shares(SessionUserId, TargetChannelMap, State) ->
    SessionChannelList = guild_visibility:get_user_viewable_channels(SessionUserId, State),
    lists:any(fun(Ch) -> maps:is_key(Ch, TargetChannelMap) end, SessionChannelList).

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
        {_, SessionData, NextIterator} when is_map(SessionData) ->
            viewable_channels_or_continue(UserId, SessionData, NextIterator);
        {_, _, NextIterator} ->
            find_session_viewable_channels_iter(UserId, NextIterator)
    end.

-spec viewable_channels_or_continue(user_id(), map(), maps:iterator()) -> map() | undefined.
viewable_channels_or_continue(UserId, SessionData, NextIterator) ->
    ViewableChannels = maps:get(viewable_channels, SessionData, undefined),
    case {maps:get(user_id, SessionData, undefined), ViewableChannels} of
        {UserId, Map} when is_map(Map) ->
            Map;
        _ ->
            find_session_viewable_channels_iter(UserId, NextIterator)
    end.

-spec session_pids([binary()], map()) -> [pid()].
session_pids(SessionIds, Sessions) ->
    lists:filtermap(
        fun(SessionId) ->
            session_pid(SessionId, Sessions)
        end,
        SessionIds
    ).

-spec session_pid(binary(), map()) -> {true, pid()} | false.
session_pid(SessionId, Sessions) ->
    case maps:find(SessionId, Sessions) of
        {ok, SessionData} -> session_data_pid(SessionData);
        error -> false
    end.

-spec session_data_pid(map()) -> {true, pid()} | false.
session_data_pid(SessionData) ->
    case maps:get(pid, SessionData, undefined) of
        Pid when is_pid(Pid) -> {true, Pid};
        _ -> false
    end.

-ifdef(TEST).

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
    assert_partition_result(Sessions, #{100 => true}, [], [<<"s1">>]).

partition_subscribed_sessions_missing_session_test() ->
    assert_partition_result(#{}, #{100 => true}, [], [<<"s1">>]).

sync_member_data_updates_loaded_channel_engines_test() ->
    GuildId = 100,
    UserId = 10,
    ChannelId = 500,
    BotsRoleId = 200,
    CountingRoleId = 300,
    DefaultRef = guild_member_list_engine:new(),
    ChannelRef = guild_member_list_engine:new(),
    try
        ok = load_sync_member_test_ref(DefaultRef, UserId, CountingRoleId, BotsRoleId),
        ok = load_sync_member_test_ref(ChannelRef, UserId, CountingRoleId, BotsRoleId),
        State = sync_member_data_test_state(
            GuildId, UserId, ChannelId, BotsRoleId, CountingRoleId, DefaultRef, ChannelRef
        ),
        ?assertEqual(stale_groups(BotsRoleId), guild_member_list_engine:get_groups(ChannelRef)),
        ok = sync_member_data(UserId, State),
        ?assertEqual(
            hoisted_groups(BotsRoleId), guild_member_list_engine:get_groups(ChannelRef)
        )
    after
        guild_member_list_engine:destroy(DefaultRef),
        guild_member_list_engine:destroy(ChannelRef)
    end.

assert_partition_result(Sessions, TargetChannelMap, ExpectedValid, ExpectedInvalid) ->
    State = #{sessions => Sessions, data => #{<<"members">> => #{}}},
    {Valid, Invalid} = partition_subscribed_sessions(
        [<<"s1">>], Sessions, TargetChannelMap, 10, State
    ),
    ?assertEqual(ExpectedValid, Valid),
    ?assertEqual(ExpectedInvalid, Invalid).

-spec load_sync_member_test_ref(
    guild_member_list_store:store_ref(), integer(), integer(), integer()
) -> ok.
load_sync_member_test_ref(Ref, UserId, MemberRoleId, HoistedRoleId) ->
    guild_member_list_engine:bulk_load(
        Ref, [{UserId, <<"counting">>, [MemberRoleId], true}], [HoistedRoleId]
    ).

sync_member_data_test_state(
    GuildId, UserId, ChannelId, BotsRoleId, CountingRoleId, DefaultRef, ChannelRef
) ->
    Data = guild_data_index:normalize_data(#{
        <<"guild">> => #{<<"owner_id">> => integer_to_binary(UserId)},
        <<"roles">> => [
            role(GuildId, <<"everyone">>, false, 0),
            role(BotsRoleId, <<"bots">>, true, 1),
            role(CountingRoleId, <<"Counting">>, false, 1)
        ],
        <<"members">> => [
            #{
                <<"user">> => #{
                    <<"id">> => integer_to_binary(UserId),
                    <<"username">> => <<"Counting">>
                },
                <<"roles">> => [
                    integer_to_binary(BotsRoleId), integer_to_binary(CountingRoleId)
                ]
            }
        ],
        <<"channels">> => [
            #{<<"id">> => integer_to_binary(ChannelId), <<"permission_overwrites">> => []}
        ]
    }),
    #{
        id => GuildId,
        data => Data,
        sessions => #{},
        member_presence => #{UserId => #{<<"status">> => <<"online">>}},
        connected_user_ids => sets:from_list([UserId]),
        member_list_engine => DefaultRef,
        channel_member_list_engines => #{integer_to_binary(ChannelId) => ChannelRef}
    }.

role(RoleId, Name, Hoist, Position) ->
    #{
        <<"id">> => integer_to_binary(RoleId),
        <<"name">> => Name,
        <<"hoist">> => Hoist,
        <<"position">> => Position,
        <<"permissions">> => <<"0">>
    }.

stale_groups(RoleId) ->
    [{integer_to_binary(RoleId), 0}, {<<"online">>, 1}, {<<"offline">>, 0}].

hoisted_groups(RoleId) ->
    [{integer_to_binary(RoleId), 1}, {<<"online">>, 0}, {<<"offline">>, 0}].

-endif.
