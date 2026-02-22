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

-module(guild_member_list).

-export([
    calculate_list_id/2,
    get_member_groups/2,
    subscribe_ranges/4,
    unsubscribe_session/2,
    get_items_in_range/3,
    handle_member_update/3,
    build_sync_response/4,
    member_list_delta/4,
    member_list_snapshot/2,
    get_online_count/1,
    broadcast_member_list_updates/3,
    broadcast_all_member_list_updates/1,
    broadcast_member_list_updates_for_channel/2,
    normalize_ranges/1,
    get_members_cursor/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type guild_state() :: map().
-type list_id() :: binary().
-type range() :: {non_neg_integer(), non_neg_integer()}.
-type group_item() :: map().
-type list_item() :: map().
-type user_id() :: integer().
-type channel_id() :: integer().

-define(MAX_RANGE_END, 100000).

-spec validate_range(range()) -> range() | invalid.
validate_range({Start, End}) when
    is_integer(Start),
    is_integer(End),
    Start >= 0,
    End >= 0,
    Start =< End,
    End =< ?MAX_RANGE_END
->
    {Start, End};
validate_range(_) ->
    invalid.

-spec normalize_ranges([range()]) -> [range()].
normalize_ranges(Ranges) ->
    ValidRanges = lists:filtermap(
        fun(R) ->
            case validate_range(R) of
                invalid -> false;
                Valid -> {true, Valid}
            end
        end,
        Ranges
    ),
    merge_overlapping_ranges(lists:sort(ValidRanges)).

-spec merge_overlapping_ranges([range()]) -> [range()].
merge_overlapping_ranges([]) ->
    [];
merge_overlapping_ranges([Single]) ->
    [Single];
merge_overlapping_ranges([{S1, E1}, {S2, E2} | Rest]) when S2 =< E1 + 1 ->
    merge_overlapping_ranges([{S1, max(E1, E2)} | Rest]);
merge_overlapping_ranges([Range | Rest]) ->
    [Range | merge_overlapping_ranges(Rest)].

-spec calculate_list_id(channel_id(), guild_state()) -> list_id().
calculate_list_id(ChannelId, State) when is_integer(ChannelId), ChannelId > 0 ->
    Data = maps:get(data, State, #{}),
    Channels = map_utils:ensure_list(maps:get(<<"channels">>, Data, [])),
    ChannelIdBin = integer_to_binary(ChannelId),
    ChannelExists = lists:any(
        fun(Ch) -> maps:get(<<"id">>, Ch, undefined) =:= ChannelIdBin end, Channels
    ),
    case ChannelExists of
        true -> ChannelIdBin;
        false -> <<"0">>
    end;
calculate_list_id(_, _) ->
    <<"0">>.

-spec get_member_groups(list_id(), guild_state()) -> [group_item()].
get_member_groups(ListId, State) ->
    Data = maps:get(data, State, #{}),
    Members = guild_data_index:member_values(Data),
    Roles = map_utils:ensure_list(maps:get(<<"roles">>, Data, [])),
    GuildId = maps:get(id, State, 0),
    HoistedRoles = guild_member_list_common:get_hoisted_roles_sorted(Roles, GuildId),
    FilteredMembers = guild_member_list_common:filter_members_for_list(ListId, Members, State),
    {OnlineMembers, OfflineMembers} = guild_member_list_common:partition_members_by_online(FilteredMembers, State),
    RoleGroups = guild_member_list_common:build_role_groups(HoistedRoles, OnlineMembers),
    OnlineGroup = #{
        <<"id">> => <<"online">>, <<"count">> => guild_member_list_common:count_ungrouped_online(OnlineMembers, HoistedRoles)
    },
    OfflineGroup = #{<<"id">> => <<"offline">>, <<"count">> => length(OfflineMembers)},
    RoleGroups ++ [OnlineGroup, OfflineGroup].

-spec subscribe_ranges(binary(), list_id(), [range()], guild_state()) ->
    {guild_state(), boolean(), [range()]}.
subscribe_ranges(SessionId, ListId, Ranges, State) ->
    NormalizedRanges = normalize_ranges(Ranges),
    Subscriptions = maps:get(member_list_subscriptions, State, #{}),
    {NewSubscriptions, _OldRanges, ShouldSync} =
        guild_member_list_common:update_subscriptions(SessionId, ListId, NormalizedRanges, Subscriptions),
    NewState = maps:put(member_list_subscriptions, NewSubscriptions, State),
    {NewState, ShouldSync, NormalizedRanges}.

-spec unsubscribe_session(binary(), guild_state()) -> guild_state().
unsubscribe_session(SessionId, State) ->
    Subscriptions = maps:get(member_list_subscriptions, State, #{}),
    NewSubscriptions = guild_member_list_common:remove_session_from_subscriptions(SessionId, Subscriptions),
    maps:put(member_list_subscriptions, NewSubscriptions, State).

-spec get_items_in_range(list_id(), range(), guild_state()) -> [list_item()].
get_items_in_range(ListId, {Start, End}, State) ->
    Groups = get_member_groups(ListId, State),
    SortedMembers = get_sorted_members_for_list(ListId, State),
    Items = guild_member_list_common:build_member_list_items(Groups, SortedMembers, State),
    guild_member_list_common:slice_items(Items, Start, End).

-spec handle_member_update(map(), list_id(), guild_state()) -> [map()].
handle_member_update(MemberUpdate, TargetListId, State) ->
    UserId = guild_member_list_common:get_member_user_id(MemberUpdate),
    {CurrentMember, UpdatedMember, NewState} = guild_member_list_common:upsert_member_in_state(UserId, MemberUpdate, State),
    OldSortedMembers = get_sorted_members_for_list(TargetListId, State),
    NewSortedMembers = get_sorted_members_for_list(TargetListId, NewState),
    OldInList = guild_member_list_common:member_in_list(UserId, OldSortedMembers),
    NewInList = guild_member_list_common:member_in_list(UserId, NewSortedMembers),
    case {OldInList, NewInList} of
        {false, false} ->
            [];
        _ ->
            RolesChanged =
                case CurrentMember of
                    undefined ->
                        false;
                    CM ->
                        maps:get(<<"roles">>, CM, []) =/= maps:get(<<"roles">>, UpdatedMember, [])
                end,
            case RolesChanged of
                true ->
                    full_sync_ops(TargetListId, NewState);
                false ->
                    OldItems = build_full_items(TargetListId, State, OldSortedMembers),
                    NewItems = build_full_items(TargetListId, NewState, NewSortedMembers),
                    guild_member_list_common:diff_items_to_ops(OldItems, NewItems)
            end
    end.

-spec build_sync_response(integer(), list_id(), [range()], guild_state()) -> map().
build_sync_response(GuildId, ListId, Ranges, State) ->
    NormalizedRanges = normalize_ranges(Ranges),
    Groups = get_member_groups(ListId, State),
    SortedMembers = get_sorted_members_for_list(ListId, State),
    Items = guild_member_list_common:build_member_list_items(Groups, SortedMembers, State),
    {OnlineMembers, _OfflineMembers} = guild_member_list_common:partition_members_by_online(SortedMembers, State),
    Ops = lists:map(
        fun({Start, End}) ->
            #{
                <<"op">> => <<"SYNC">>,
                <<"range">> => [Start, End],
                <<"items">> => guild_member_list_common:slice_items(Items, Start, End)
            }
        end,
        NormalizedRanges
    ),
    #{
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"id">> => ListId,
        <<"member_count">> => length(SortedMembers),
        <<"online_count">> => length(OnlineMembers),
        <<"groups">> => Groups,
        <<"ops">> => Ops
    }.

-spec get_online_count(guild_state()) -> non_neg_integer().
get_online_count(State) ->
    Data = maps:get(data, State, #{}),
    Members = guild_data_index:member_values(Data),
    EligibleMembers = guild_member_list_common:filter_members_for_list(<<"0">>, Members, State),
    {OnlineMembers, _} = guild_member_list_common:partition_members_by_online(EligibleMembers, State),
    length(OnlineMembers).

-spec broadcast_member_list_updates(user_id(), guild_state(), guild_state()) -> ok.
broadcast_member_list_updates(_UserId, OldState, UpdatedState) ->
    GuildId = maps:get(id, UpdatedState, 0),
    MemberListSubs = maps:get(member_list_subscriptions, UpdatedState, #{}),
    Sessions = maps:get(sessions, UpdatedState, #{}),
    maps:foreach(
        fun(ListId, ListSubs) ->
            case member_list_delta(ListId, OldState, UpdatedState, _UserId) of
                {MemberCount, OnlineCount, Groups, Ops, true} ->
                    Payload = #{
                        <<"guild_id">> => integer_to_binary(GuildId),
                        <<"id">> => ListId,
                        <<"member_count">> => MemberCount,
                        <<"online_count">> => OnlineCount,
                        <<"groups">> => Groups,
                        <<"ops">> => Ops
                    },
                    send_member_list_update_to_sessions(
                        ListId, ListSubs, Sessions, Payload, UpdatedState
                    );
                _ ->
                    ok
            end
        end,
        MemberListSubs
    ),
    ok.

-spec send_member_list_update_to_sessions(list_id(), map(), map(), map(), guild_state()) -> ok.
send_member_list_update_to_sessions(ListId, ListSubs, Sessions, Payload, State) ->
    ChannelId = guild_member_list_common:list_id_channel_id(ListId),
    maps:foreach(
        fun(SessionId, _Ranges) ->
            case maps:get(SessionId, Sessions, undefined) of
                #{pid := SessionPid} = SessionData ->
                    case is_pid(SessionPid) of
                        true ->
                            case guild_member_list_common:session_can_view_channel(SessionData, ChannelId, State) of
                                true ->
                                    gen_server:cast(
                                        SessionPid, {dispatch, guild_member_list_update, Payload}
                                    );
                                false ->
                                    ok
                            end;
                        false ->
                            ok
                    end;
                _ ->
                    ok
            end
        end,
        ListSubs
    ).

-spec member_list_delta(list_id(), guild_state(), guild_state(), user_id()) ->
    {non_neg_integer(), non_neg_integer(), [group_item()], [list_item()], boolean()}.
member_list_delta(ListId, OldState, UpdatedState, UserId) ->
    {OldCount, OldOnline, OldGroups, OldItems} = member_list_snapshot(ListId, OldState),
    {MemberCount, OnlineCount, Groups, Items} = member_list_snapshot(ListId, UpdatedState),
    case guild_member_list_common:presence_move_ops(UserId, OldState, UpdatedState, OldItems, Items) of
        {true, Ops} ->
            {MemberCount, OnlineCount, Groups, Ops, true};
        {false, _} ->
            Ops = guild_member_list_common:diff_items_to_ops(OldItems, Items),
            Changed =
                Ops =/= [] orelse OldCount =/= MemberCount orelse OldOnline =/= OnlineCount orelse
                    OldGroups =/= Groups,
            {MemberCount, OnlineCount, Groups, Ops, Changed}
    end.

-spec member_list_snapshot(list_id(), guild_state()) ->
    {non_neg_integer(), non_neg_integer(), [group_item()], [list_item()]}.
member_list_snapshot(ListId, State) ->
    Groups = get_member_groups(ListId, State),
    SortedMembers = get_sorted_members_for_list(ListId, State),
    Items = guild_member_list_common:build_member_list_items(Groups, SortedMembers, State),
    {OnlineMembers, _OfflineMembers} = guild_member_list_common:partition_members_by_online(SortedMembers, State),
    {length(SortedMembers), length(OnlineMembers), Groups, Items}.

-spec broadcast_all_member_list_updates(guild_state()) -> ok.
broadcast_all_member_list_updates(State) ->
    GuildId = maps:get(id, State, 0),
    MemberListSubs = maps:get(member_list_subscriptions, State, #{}),
    Sessions = maps:get(sessions, State, #{}),
    maps:foreach(
        fun(ListId, ListSubs) ->
            ChannelId = guild_member_list_common:list_id_channel_id(ListId),
            maps:foreach(
                fun(SessionId, Ranges) ->
                    case maps:get(SessionId, Sessions, undefined) of
                        SessionData when is_map(SessionData) ->
                            SessionPid = maps:get(pid, SessionData, undefined),
                            case
                                {
                                    is_pid(SessionPid),
                                    guild_member_list_common:session_can_view_channel(SessionData, ChannelId, State)
                                }
                            of
                                {true, true} ->
                                    SyncResponse = build_sync_response(
                                        GuildId, ListId, Ranges, State
                                    ),
                                    gen_server:cast(
                                        SessionPid,
                                        {dispatch, guild_member_list_update, SyncResponse}
                                    );
                                _ ->
                                    ok
                            end;
                        _ ->
                            ok
                    end
                end,
                ListSubs
            )
        end,
        MemberListSubs
    ),
    ok.

-spec broadcast_member_list_updates_for_channel(channel_id(), guild_state()) -> ok.
broadcast_member_list_updates_for_channel(ChannelId, State) ->
    GuildId = maps:get(id, State, 0),
    ListId = calculate_list_id(ChannelId, State),
    MemberListSubs = maps:get(member_list_subscriptions, State, #{}),
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(ListId, MemberListSubs, undefined) of
        undefined ->
            ok;
        ListSubs ->
            maps:foreach(
                fun(SessionId, Ranges) ->
                    case maps:get(SessionId, Sessions, undefined) of
                        SessionData when is_map(SessionData) ->
                            SessionPid = maps:get(pid, SessionData, undefined),
                            case
                                {
                                    is_pid(SessionPid),
                                    guild_member_list_common:session_can_view_channel(SessionData, ChannelId, State)
                                }
                            of
                                {true, true} ->
                                    SyncResponse = build_sync_response(
                                        GuildId, ListId, Ranges, State
                                    ),
                                    SyncResponseWithChannel = maps:put(
                                        <<"channel_id">>, integer_to_binary(ChannelId), SyncResponse
                                    ),
                                    gen_server:cast(
                                        SessionPid,
                                        {dispatch, guild_member_list_update,
                                            SyncResponseWithChannel}
                                    );
                                _ ->
                                    ok
                            end;
                        _ ->
                            ok
                    end
                end,
                ListSubs
            )
    end,
    ok.

-spec get_sorted_members_for_list(list_id(), guild_state()) -> [map()].
get_sorted_members_for_list(ListId, State) ->
    Data = maps:get(data, State, #{}),
    Members = guild_data_index:member_values(Data),
    FilteredMembers = guild_member_list_common:filter_members_for_list(ListId, Members, State),
    Decorated = [{guild_member_list_common:get_member_sort_key(M), M} || M <- FilteredMembers],
    Sorted = lists:sort(fun({KeyA, _}, {KeyB, _}) -> KeyA =< KeyB end, Decorated),
    [M || {_, M} <- Sorted].

-spec build_full_items(list_id(), guild_state(), [map()]) -> [list_item()].
build_full_items(ListId, State, SortedMembers) ->
    Groups = get_member_groups(ListId, State),
    guild_member_list_common:build_member_list_items(Groups, SortedMembers, State).

-spec full_sync_ops(list_id(), guild_state()) -> [map()].
full_sync_ops(ListId, State) ->
    SortedMembers = get_sorted_members_for_list(ListId, State),
    Items = build_full_items(ListId, State, SortedMembers),
    guild_member_list_common:full_sync_from_items(Items).

-spec get_members_cursor(map(), guild_state()) -> {reply, map(), guild_state()}.
get_members_cursor(Request, State) ->
    Limit = maps:get(<<"limit">>, Request, 1),
    AfterId = maps:get(<<"after">>, Request, undefined),
    SortedMembers = sort_members_by_user_id(State),
    FilteredMembers = filter_members_after(SortedMembers, AfterId),
    ResponseMembers = take_first(FilteredMembers, Limit),
    {reply,
        #{
            members => ResponseMembers,
            total => length(SortedMembers)
        },
        State}.

-spec take_first([map()], integer()) -> [map()].
take_first(_Members, Limit) when Limit =< 0 ->
    [];
take_first(Members, Limit) ->
    Count = min(Limit, length(Members)),
    case Count of
        0 -> [];
        _ -> lists:sublist(Members, 1, Count)
    end.

-spec filter_members_after([map()], integer() | undefined) -> [map()].
filter_members_after(Members, undefined) ->
    Members;
filter_members_after(Members, AfterId) ->
    lists:dropwhile(
        fun(Member) ->
            guild_member_list_common:get_member_user_id(Member) =< AfterId
        end,
        Members
    ).

-spec sort_members_by_user_id(guild_state()) -> [map()].
sort_members_by_user_id(State) ->
    Data = maps:get(data, State, #{}),
    Members = guild_data_index:member_values(Data),
    lists:sort(
        fun(A, B) ->
            guild_member_list_common:get_member_user_id(A) =< guild_member_list_common:get_member_user_id(B)
        end,
        Members
    ).

-ifdef(TEST).

calculate_list_id_returns_channel_id_test() ->
    State = #{
        id => 100,
        data => #{
            <<"channels">> => [
                #{<<"id">> => <<"500">>}
            ]
        }
    },
    ?assertEqual(<<"500">>, calculate_list_id(500, State)).

calculate_list_id_missing_channel_test() ->
    State = #{
        id => 100,
        data => #{}
    },
    ?assertEqual(<<"0">>, calculate_list_id(123, State)).

subscribe_ranges_test() ->
    State = #{member_list_subscriptions => #{}},
    {NewState, ShouldSync, NormalizedRanges} =
        subscribe_ranges(<<"session1">>, <<"500">>, [{0, 99}], State),
    Subs = maps:get(member_list_subscriptions, NewState),
    ChannelSubs = maps:get(<<"500">>, Subs),
    ?assertEqual([{0, 99}], maps:get(<<"session1">>, ChannelSubs)),
    ?assertEqual(true, ShouldSync),
    ?assertEqual([{0, 99}], NormalizedRanges).

normalize_ranges_empty_test() ->
    ?assertEqual([], normalize_ranges([])).

normalize_ranges_single_test() ->
    ?assertEqual([{0, 99}], normalize_ranges([{0, 99}])).

normalize_ranges_merges_overlapping_test() ->
    ?assertEqual([{0, 150}], normalize_ranges([{0, 100}, {50, 150}])).

normalize_ranges_merges_adjacent_test() ->
    ?assertEqual([{0, 200}], normalize_ranges([{0, 100}, {101, 200}])).

normalize_ranges_keeps_separate_test() ->
    ?assertEqual([{0, 50}, {100, 150}], normalize_ranges([{0, 50}, {100, 150}])).

normalize_ranges_filters_invalid_test() ->
    ?assertEqual([{0, 50}], normalize_ranges([{100, 50}, {0, 50}, {-1, 10}])).

normalize_ranges_sorts_and_merges_test() ->
    ?assertEqual([{0, 200}], normalize_ranges([{150, 200}, {0, 100}, {50, 160}])).

validate_range_valid_test() ->
    ?assertEqual({0, 100}, validate_range({0, 100})).

validate_range_invalid_reversed_test() ->
    ?assertEqual(invalid, validate_range({100, 50})).

validate_range_invalid_negative_test() ->
    ?assertEqual(invalid, validate_range({-1, 50})).

validate_range_invalid_too_large_test() ->
    ?assertEqual(invalid, validate_range({0, 100001})).

get_members_cursor_returns_atom_keys_test() ->
    Member1 = #{<<"user">> => #{<<"id">> => <<"2">>}},
    Member2 = #{<<"user">> => #{<<"id">> => <<"1">>}},
    State = #{data => #{<<"members">> => [Member1, Member2]}},
    {reply, Reply, _NewState} = get_members_cursor(#{<<"limit">> => 1}, State),
    ?assert(maps:is_key(members, Reply)),
    ?assert(maps:is_key(total, Reply)),
    ?assertNot(maps:is_key(<<"members">>, Reply)),
    ?assertNot(maps:is_key(<<"total">>, Reply)).

get_online_count_ignores_members_without_connected_session_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>}}
    ],
    State = #{
        data => #{<<"members">> => Members},
        sessions => #{<<"s1">> => #{user_id => 1}},
        member_presence => #{
            1 => #{<<"status">> => <<"online">>},
            2 => #{<<"status">> => <<"online">>}
        }
    },
    ?assertEqual(1, get_online_count(State)).

filter_members_for_list_keeps_members_without_connected_session_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>}}
    ],
    State = #{
        sessions => #{<<"s1">> => #{user_id => 1}},
        data => #{<<"channels">> => []}
    },
    FilteredMembers = guild_member_list_common:filter_members_for_list(<<"0">>, Members, State),
    ?assertEqual([1, 2], [guild_member_list_common:get_member_user_id(Member) || Member <- FilteredMembers]).

get_member_groups_counts_members_without_connected_session_as_offline_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>}}
    ],
    State = #{
        id => 123,
        data => #{
            <<"members">> => Members,
            <<"roles">> => []
        },
        sessions => #{<<"s1">> => #{user_id => 1}},
        member_presence => #{
            1 => #{<<"status">> => <<"online">>},
            2 => #{<<"status">> => <<"online">>}
        }
    },
    Groups = get_member_groups(<<"0">>, State),
    OnlineGroup = lists:nth(1, Groups),
    OfflineGroup = lists:nth(2, Groups),
    ?assertEqual(1, maps:get(<<"count">>, OnlineGroup)),
    ?assertEqual(1, maps:get(<<"count">>, OfflineGroup)).

validate_range_zero_length_valid_test() ->
    ?assertEqual({0, 0}, validate_range({0, 0})).

validate_range_max_boundary_test() ->
    ?assertEqual({0, 100000}, validate_range({0, 100000})).

validate_range_non_integer_test() ->
    ?assertEqual(invalid, validate_range({a, b})),
    ?assertEqual(invalid, validate_range({<<"0">>, <<"10">>})).

validate_range_single_element_test() ->
    ?assertEqual(invalid, validate_range({50})).

normalize_ranges_duplicate_ranges_test() ->
    ?assertEqual([{0, 99}], normalize_ranges([{0, 99}, {0, 99}])).

normalize_ranges_subset_ranges_test() ->
    ?assertEqual([{0, 100}], normalize_ranges([{0, 100}, {10, 50}])).

normalize_ranges_three_separate_ranges_test() ->
    Result = normalize_ranges([{0, 10}, {50, 60}, {100, 110}]),
    ?assertEqual([{0, 10}, {50, 60}, {100, 110}], Result).

normalize_ranges_all_invalid_test() ->
    ?assertEqual([], normalize_ranges([{10, 5}, {-1, -1}, {200000, 300000}])).

normalize_ranges_mixed_valid_and_invalid_test() ->
    ?assertEqual([{10, 50}], normalize_ranges([{100, 50}, {10, 50}, {-1, 10}])).

normalize_ranges_touching_ranges_merge_test() ->
    ?assertEqual([{0, 20}], normalize_ranges([{0, 10}, {11, 20}])).

normalize_ranges_gap_of_one_no_merge_test() ->
    ?assertEqual([{0, 10}, {12, 20}], normalize_ranges([{0, 10}, {12, 20}])).

calculate_list_id_zero_channel_test() ->
    State = #{id => 100, data => #{}},
    ?assertEqual(<<"0">>, calculate_list_id(0, State)).

calculate_list_id_negative_channel_test() ->
    State = #{id => 100, data => #{}},
    ?assertEqual(<<"0">>, calculate_list_id(-1, State)).

calculate_list_id_non_integer_test() ->
    State = #{id => 100, data => #{}},
    ?assertEqual(<<"0">>, calculate_list_id(not_an_integer, State)).

calculate_list_id_channel_in_data_test() ->
    State = #{
        id => 100,
        data => #{
            <<"channels">> => [
                #{<<"id">> => <<"500">>},
                #{<<"id">> => <<"600">>}
            ]
        }
    },
    ?assertEqual(<<"500">>, calculate_list_id(500, State)),
    ?assertEqual(<<"600">>, calculate_list_id(600, State)).

calculate_list_id_consistent_across_calls_test() ->
    State = #{
        id => 100,
        data => #{
            <<"channels">> => [#{<<"id">> => <<"500">>}]
        }
    },
    ?assertEqual(calculate_list_id(500, State), calculate_list_id(500, State)).

build_sync_response_zero_members_test() ->
    State = #{
        id => 100,
        data => #{
            <<"members">> => [],
            <<"roles">> => []
        },
        sessions => #{},
        member_presence => #{}
    },
    Response = build_sync_response(100, <<"0">>, [{0, 99}], State),
    ?assertEqual(<<"100">>, maps:get(<<"guild_id">>, Response)),
    ?assertEqual(<<"0">>, maps:get(<<"id">>, Response)),
    ?assertEqual(0, maps:get(<<"member_count">>, Response)),
    ?assertEqual(0, maps:get(<<"online_count">>, Response)).

build_sync_response_boundary_100_members_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => integer_to_binary(I)}, <<"roles">> => []}
        || I <- lists:seq(1, 100)
    ],
    State = #{
        id => 100,
        data => #{
            <<"members">> => Members,
            <<"roles">> => []
        },
        sessions => #{},
        member_presence => #{}
    },
    Response = build_sync_response(100, <<"0">>, [{0, 99}], State),
    ?assertEqual(100, maps:get(<<"member_count">>, Response)),
    ?assertEqual(0, maps:get(<<"online_count">>, Response)).

build_sync_response_with_role_groups_test() ->
    HoistedRole = #{<<"id">> => <<"200">>, <<"hoist">> => true, <<"position">> => 5, <<"name">> => <<"Admins">>},
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"200">>]},
        #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => []},
        #{<<"user">> => #{<<"id">> => <<"3">>}, <<"roles">> => []}
    ],
    State = #{
        id => 100,
        data => #{
            <<"members">> => Members,
            <<"roles">> => [HoistedRole]
        },
        sessions => #{<<"s1">> => #{user_id => 1}, <<"s2">> => #{user_id => 2}},
        member_presence => #{
            1 => #{<<"status">> => <<"online">>},
            2 => #{<<"status">> => <<"online">>}
        }
    },
    Response = build_sync_response(100, <<"0">>, [{0, 99}], State),
    Groups = maps:get(<<"groups">>, Response),
    ?assert(length(Groups) >= 3),
    GroupIds = [maps:get(<<"id">>, G) || G <- Groups],
    ?assert(lists:member(<<"200">>, GroupIds)),
    ?assert(lists:member(<<"online">>, GroupIds)),
    ?assert(lists:member(<<"offline">>, GroupIds)).

get_member_groups_empty_guild_test() ->
    State = #{
        id => 100,
        data => #{
            <<"members">> => [],
            <<"roles">> => []
        },
        sessions => #{},
        member_presence => #{}
    },
    Groups = get_member_groups(<<"0">>, State),
    OnlineGroups = [G || G <- Groups, maps:get(<<"id">>, G) =:= <<"online">>],
    OfflineGroups = [G || G <- Groups, maps:get(<<"id">>, G) =:= <<"offline">>],
    ?assertEqual(1, length(OnlineGroups)),
    ?assertEqual(1, length(OfflineGroups)),
    ?assertEqual(0, maps:get(<<"count">>, hd(OnlineGroups))),
    ?assertEqual(0, maps:get(<<"count">>, hd(OfflineGroups))).

get_member_groups_only_offline_members_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>}}
    ],
    State = #{
        id => 100,
        data => #{
            <<"members">> => Members,
            <<"roles">> => []
        },
        sessions => #{},
        member_presence => #{}
    },
    Groups = get_member_groups(<<"0">>, State),
    OnlineGroup = hd([G || G <- Groups, maps:get(<<"id">>, G) =:= <<"online">>]),
    OfflineGroup = hd([G || G <- Groups, maps:get(<<"id">>, G) =:= <<"offline">>]),
    ?assertEqual(0, maps:get(<<"count">>, OnlineGroup)),
    ?assertEqual(2, maps:get(<<"count">>, OfflineGroup)).

get_member_groups_zero_online_members_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>}},
        #{<<"user">> => #{<<"id">> => <<"3">>}}
    ],
    State = #{
        id => 100,
        data => #{
            <<"members">> => Members,
            <<"roles">> => []
        },
        sessions => #{<<"s1">> => #{user_id => 1}},
        member_presence => #{
            1 => #{<<"status">> => <<"offline">>}
        }
    },
    Groups = get_member_groups(<<"0">>, State),
    OnlineGroup = hd([G || G <- Groups, maps:get(<<"id">>, G) =:= <<"online">>]),
    ?assertEqual(0, maps:get(<<"count">>, OnlineGroup)).

subscribe_ranges_filters_invalid_test() ->
    State = #{member_list_subscriptions => #{}},
    {NewState, _ShouldSync, NormalizedRanges} =
        subscribe_ranges(<<"s1">>, <<"0">>, [{100, 50}, {0, 10}], State),
    ?assertEqual([{0, 10}], NormalizedRanges),
    Subs = maps:get(member_list_subscriptions, NewState),
    ?assertEqual([{0, 10}], maps:get(<<"s1">>, maps:get(<<"0">>, Subs))).

subscribe_ranges_empty_ranges_test() ->
    State = #{member_list_subscriptions => #{<<"0">> => #{<<"s1">> => [{0, 99}]}}},
    {NewState, ShouldSync, NormalizedRanges} =
        subscribe_ranges(<<"s1">>, <<"0">>, [], State),
    ?assertEqual([], NormalizedRanges),
    ?assertEqual(false, ShouldSync),
    Subs = maps:get(member_list_subscriptions, NewState),
    ?assertEqual(#{}, Subs).

unsubscribe_session_test() ->
    State = #{member_list_subscriptions => #{
        <<"0">> => #{<<"s1">> => [{0, 99}], <<"s2">> => [{0, 50}]},
        <<"500">> => #{<<"s1">> => [{0, 50}]}
    }},
    NewState = unsubscribe_session(<<"s1">>, State),
    Subs = maps:get(member_list_subscriptions, NewState),
    ?assertNot(maps:is_key(<<"500">>, Subs)),
    ?assertEqual(#{<<"s2">> => [{0, 50}]}, maps:get(<<"0">>, Subs)).

unsubscribe_session_not_subscribed_test() ->
    State = #{member_list_subscriptions => #{<<"0">> => #{<<"s1">> => [{0, 99}]}}},
    NewState = unsubscribe_session(<<"nonexistent">>, State),
    Subs = maps:get(member_list_subscriptions, NewState),
    ?assertEqual(#{<<"s1">> => [{0, 99}]}, maps:get(<<"0">>, Subs)).

get_online_count_zero_online_test() ->
    State = #{
        data => #{<<"members">> => [], <<"roles">> => []},
        sessions => #{},
        member_presence => #{}
    },
    ?assertEqual(0, get_online_count(State)).

get_online_count_multiple_sessions_same_user_test() ->
    Members = [#{<<"user">> => #{<<"id">> => <<"1">>}}],
    State = #{
        data => #{<<"members">> => Members},
        sessions => #{
            <<"s1">> => #{user_id => 1},
            <<"s2">> => #{user_id => 1}
        },
        member_presence => #{1 => #{<<"status">> => <<"online">>}}
    },
    ?assertEqual(1, get_online_count(State)).

member_list_delta_no_change_test() ->
    State = #{
        id => 100,
        data => #{
            <<"members">> => [#{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => []}],
            <<"roles">> => []
        },
        sessions => #{},
        member_presence => #{}
    },
    {MemberCount, OnlineCount, _Groups, Ops, Changed} =
        member_list_delta(<<"0">>, State, State, 1),
    ?assertEqual(1, MemberCount),
    ?assertEqual(0, OnlineCount),
    ?assertEqual([], Ops),
    ?assertEqual(false, Changed).

member_list_delta_member_added_test() ->
    OldState = #{
        id => 100,
        data => #{<<"members">> => [], <<"roles">> => []},
        sessions => #{},
        member_presence => #{}
    },
    NewState = #{
        id => 100,
        data => #{
            <<"members">> => [#{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => []}],
            <<"roles">> => []
        },
        sessions => #{},
        member_presence => #{}
    },
    {MemberCount, _OnlineCount, _Groups, _Ops, Changed} =
        member_list_delta(<<"0">>, OldState, NewState, 1),
    ?assertEqual(1, MemberCount),
    ?assertEqual(true, Changed).

member_list_delta_member_going_online_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => []},
        #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => []}
    ],
    OldState = #{
        id => 100,
        data => #{<<"members">> => Members, <<"roles">> => []},
        sessions => #{},
        member_presence => #{}
    },
    NewState = #{
        id => 100,
        data => #{<<"members">> => Members, <<"roles">> => []},
        sessions => #{<<"s1">> => #{user_id => 1}},
        member_presence => #{1 => #{<<"status">> => <<"online">>}}
    },
    {_MemberCount, OnlineCount, _Groups, _Ops, Changed} =
        member_list_delta(<<"0">>, OldState, NewState, 1),
    ?assertEqual(1, OnlineCount),
    ?assertEqual(true, Changed).

member_list_snapshot_empty_test() ->
    State = #{
        id => 100,
        data => #{<<"members">> => [], <<"roles">> => []},
        sessions => #{},
        member_presence => #{}
    },
    {MemberCount, OnlineCount, Groups, Items} = member_list_snapshot(<<"0">>, State),
    ?assertEqual(0, MemberCount),
    ?assertEqual(0, OnlineCount),
    ?assertEqual(2, length(Groups)),
    ?assertEqual(2, length(Items)).

get_items_in_range_empty_guild_test() ->
    State = #{
        id => 100,
        data => #{<<"members">> => [], <<"roles">> => []},
        sessions => #{},
        member_presence => #{}
    },
    Items = get_items_in_range(<<"0">>, {0, 99}, State),
    ?assertEqual(2, length(Items)).

get_items_in_range_with_members_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}, <<"roles">> => []},
        #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"bob">>}, <<"roles">> => []}
    ],
    State = #{
        id => 100,
        data => #{<<"members">> => Members, <<"roles">> => []},
        sessions => #{},
        member_presence => #{}
    },
    Items = get_items_in_range(<<"0">>, {0, 99}, State),
    ?assert(length(Items) >= 3).

get_members_cursor_with_after_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>}},
        #{<<"user">> => #{<<"id">> => <<"3">>}}
    ],
    State = #{data => #{<<"members">> => Members}},
    {reply, Reply, _} = get_members_cursor(#{<<"limit">> => 10, <<"after">> => 1}, State),
    ReturnedMembers = maps:get(members, Reply),
    ?assertEqual(2, length(ReturnedMembers)),
    Ids = [guild_member_list_common:get_member_user_id(M) || M <- ReturnedMembers],
    ?assert(lists:all(fun(Id) -> Id > 1 end, Ids)).

get_members_cursor_empty_guild_test() ->
    State = #{data => #{<<"members">> => []}},
    {reply, Reply, _} = get_members_cursor(#{<<"limit">> => 10}, State),
    ?assertEqual([], maps:get(members, Reply)),
    ?assertEqual(0, maps:get(total, Reply)).

get_members_cursor_zero_limit_test() ->
    Members = [#{<<"user">> => #{<<"id">> => <<"1">>}}],
    State = #{data => #{<<"members">> => Members}},
    {reply, Reply, _} = get_members_cursor(#{<<"limit">> => 0}, State),
    ?assertEqual([], maps:get(members, Reply)).

get_members_cursor_after_last_member_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>}}
    ],
    State = #{data => #{<<"members">> => Members}},
    {reply, Reply, _} = get_members_cursor(#{<<"limit">> => 10, <<"after">> => 999}, State),
    ?assertEqual([], maps:get(members, Reply)).

member_with_no_user_field_sorted_test() ->
    Members = [
        #{<<"nick">> => <<"orphan">>},
        #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}}
    ],
    State = #{
        id => 100,
        data => #{<<"members">> => Members, <<"roles">> => []},
        sessions => #{},
        member_presence => #{}
    },
    Groups = get_member_groups(<<"0">>, State),
    ?assert(is_list(Groups)).

display_name_change_triggers_resort_test() ->
    OldMember = #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}, <<"nick">> => <<"aaa">>},
    NewMember = #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}, <<"nick">> => <<"zzz">>},
    OldState = #{
        id => 100,
        data => #{<<"members">> => [OldMember, #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"mmm">>}, <<"roles">> => []}], <<"roles">> => []},
        sessions => #{},
        member_presence => #{}
    },
    NewState = #{
        id => 100,
        data => #{<<"members">> => [NewMember, #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"mmm">>}, <<"roles">> => []}], <<"roles">> => []},
        sessions => #{},
        member_presence => #{}
    },
    {_, _, _, _, Changed} = member_list_delta(<<"0">>, OldState, NewState, 1),
    ?assertEqual(true, Changed).

empty_guild_with_only_everyone_role_test() ->
    EveryoneRole = #{<<"id">> => <<"100">>, <<"hoist">> => false, <<"position">> => 0, <<"name">> => <<"@everyone">>},
    State = #{
        id => 100,
        data => #{<<"members">> => [], <<"roles">> => [EveryoneRole]},
        sessions => #{},
        member_presence => #{}
    },
    Groups = get_member_groups(<<"0">>, State),
    OnlineGroup = hd([G || G <- Groups, maps:get(<<"id">>, G) =:= <<"online">>]),
    OfflineGroup = hd([G || G <- Groups, maps:get(<<"id">>, G) =:= <<"offline">>]),
    ?assertEqual(0, maps:get(<<"count">>, OnlineGroup)),
    ?assertEqual(0, maps:get(<<"count">>, OfflineGroup)).

presence_update_for_member_not_in_list_test() ->
    Members = [#{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => []}],
    OldState = #{
        id => 100,
        data => #{<<"members">> => Members, <<"roles">> => []},
        sessions => #{},
        member_presence => #{}
    },
    NewState = #{
        id => 100,
        data => #{<<"members">> => Members, <<"roles">> => []},
        sessions => #{<<"s1">> => #{user_id => 999}},
        member_presence => #{999 => #{<<"status">> => <<"online">>}}
    },
    {_, _, _, _Ops, _Changed} = member_list_delta(<<"0">>, OldState, NewState, 999),
    ok.

member_gaining_hoisted_role_triggers_group_change_test() ->
    HoistedRole = #{<<"id">> => <<"200">>, <<"hoist">> => true, <<"position">> => 5, <<"name">> => <<"VIP">>},
    MemberBefore = #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => []},
    MemberAfter = #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"200">>]},
    OldState = #{
        id => 100,
        data => #{<<"members">> => [MemberBefore], <<"roles">> => [HoistedRole]},
        sessions => #{<<"s1">> => #{user_id => 1}},
        member_presence => #{1 => #{<<"status">> => <<"online">>}}
    },
    NewState = #{
        id => 100,
        data => #{<<"members">> => [MemberAfter], <<"roles">> => [HoistedRole]},
        sessions => #{<<"s1">> => #{user_id => 1}},
        member_presence => #{1 => #{<<"status">> => <<"online">>}}
    },
    {_, _, Groups, _Ops, Changed} = member_list_delta(<<"0">>, OldState, NewState, 1),
    ?assertEqual(true, Changed),
    VIPGroup = hd([G || G <- Groups, maps:get(<<"id">>, G) =:= <<"200">>]),
    ?assertEqual(1, maps:get(<<"count">>, VIPGroup)).

member_losing_hoisted_role_returns_to_online_test() ->
    HoistedRole = #{<<"id">> => <<"200">>, <<"hoist">> => true, <<"position">> => 5, <<"name">> => <<"VIP">>},
    MemberBefore = #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"200">>]},
    MemberAfter = #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => []},
    OldState = #{
        id => 100,
        data => #{<<"members">> => [MemberBefore], <<"roles">> => [HoistedRole]},
        sessions => #{<<"s1">> => #{user_id => 1}},
        member_presence => #{1 => #{<<"status">> => <<"online">>}}
    },
    NewState = #{
        id => 100,
        data => #{<<"members">> => [MemberAfter], <<"roles">> => [HoistedRole]},
        sessions => #{<<"s1">> => #{user_id => 1}},
        member_presence => #{1 => #{<<"status">> => <<"online">>}}
    },
    {_, _, Groups, _Ops, Changed} = member_list_delta(<<"0">>, OldState, NewState, 1),
    ?assertEqual(true, Changed),
    OnlineGroup = hd([G || G <- Groups, maps:get(<<"id">>, G) =:= <<"online">>]),
    ?assertEqual(1, maps:get(<<"count">>, OnlineGroup)),
    VIPGroup = hd([G || G <- Groups, maps:get(<<"id">>, G) =:= <<"200">>]),
    ?assertEqual(0, maps:get(<<"count">>, VIPGroup)).

-endif.
