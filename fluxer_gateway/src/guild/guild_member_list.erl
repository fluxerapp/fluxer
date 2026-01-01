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
    get_online_count/1,
    broadcast_member_list_updates/3,
    broadcast_all_member_list_updates/1,
    broadcast_member_list_updates_for_channel/2,
    normalize_ranges/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type guild_state() :: map().
-type list_id() :: binary().
-type range() :: {non_neg_integer(), non_neg_integer()}.
-type member_item() :: map().
-type group_item() :: map().
-type list_item() :: member_item() | group_item().
-type user_id() :: integer().

-define(MAX_RANGE_END, 100000).

-spec validate_range(range()) -> range() | invalid.
validate_range({Start, End}) when is_integer(Start), is_integer(End),
                                   Start >= 0, End >= 0, Start =< End,
                                   End =< ?MAX_RANGE_END ->
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

-spec calculate_list_id(integer(), guild_state()) -> list_id().
calculate_list_id(ChannelId, _State) when is_integer(ChannelId), ChannelId > 0 ->
    integer_to_binary(ChannelId);
calculate_list_id(_, _) ->
    <<"0">>.

-spec get_member_groups(list_id(), guild_state()) -> [group_item()].
get_member_groups(ListId, State) ->
    Data = maps:get(data, State, #{}),
    Members = map_utils:ensure_list(maps:get(<<"members">>, Data, [])),
    Roles = map_utils:ensure_list(maps:get(<<"roles">>, Data, [])),
    GuildId = maps:get(id, State, 0),
    HoistedRoles = get_hoisted_roles_sorted(Roles, GuildId),
    FilteredMembers = filter_members_for_list(ListId, Members, State),
    {OnlineMembers, OfflineMembers} = partition_members_by_online(FilteredMembers, State),
    RoleGroups = build_role_groups(HoistedRoles, OnlineMembers),
    OnlineGroup = #{<<"id">> => <<"online">>, <<"count">> => count_ungrouped_online(OnlineMembers, HoistedRoles)},
    OfflineGroup = #{<<"id">> => <<"offline">>, <<"count">> => length(OfflineMembers)},
    RoleGroups ++ [OnlineGroup, OfflineGroup].

-spec subscribe_ranges(binary(), list_id(), [range()], guild_state()) -> {guild_state(), boolean(), [range()]}.
subscribe_ranges(SessionId, ListId, Ranges, State) ->
    NormalizedRanges = normalize_ranges(Ranges),
    Subscriptions = maps:get(member_list_subscriptions, State, #{}),
    ListSubs0 = maps:get(ListId, Subscriptions, #{}),
    OldRanges = maps:get(SessionId, ListSubs0, []),
    {_ListSubs1, Subscriptions1} =
        case NormalizedRanges of
            [] ->
                Trimmed = maps:remove(SessionId, ListSubs0),
                case map_size(Trimmed) of
                    0 -> {Trimmed, maps:remove(ListId, Subscriptions)};
                    _ -> {Trimmed, maps:put(ListId, Trimmed, Subscriptions)}
                end;
            _ ->
                Updated = maps:put(SessionId, NormalizedRanges, ListSubs0),
                {Updated, maps:put(ListId, Updated, Subscriptions)}
        end,
    NewState = maps:put(member_list_subscriptions, Subscriptions1, State),
    ShouldSync =
        case NormalizedRanges of
            [] -> false;
            _ -> NormalizedRanges =/= OldRanges
        end,
    {NewState, ShouldSync, NormalizedRanges}.

-spec unsubscribe_session(binary(), guild_state()) -> guild_state().
unsubscribe_session(SessionId, State) ->
    Subscriptions = maps:get(member_list_subscriptions, State, #{}),
    NewSubscriptions =
        maps:fold(
            fun(ListId, ListSubs, Acc) ->
                Trimmed = maps:remove(SessionId, ListSubs),
                case map_size(Trimmed) of
                    0 -> Acc;
                    _ -> maps:put(ListId, Trimmed, Acc)
                end
            end,
            #{},
            Subscriptions
        ),
    maps:put(member_list_subscriptions, NewSubscriptions, State).

-spec get_items_in_range(list_id(), range(), guild_state()) -> [list_item()].
get_items_in_range(ListId, {Start, End}, State) ->
    Groups = get_member_groups(ListId, State),
    SortedMembers = get_sorted_members_for_list(ListId, State),
    Items = build_member_list_items(Groups, SortedMembers, State),
    slice_items(Items, Start, End).

-spec handle_member_update(map(), list_id(), guild_state()) -> [map()].
handle_member_update(MemberUpdate, TargetListId, State) ->
    UserId = get_member_user_id(MemberUpdate),
    {CurrentMember, UpdatedMember, NewState} = upsert_member_in_state(UserId, MemberUpdate, State),
    OldSortedMembers = get_sorted_members_for_list(TargetListId, State),
    NewSortedMembers = get_sorted_members_for_list(TargetListId, NewState),
    OldInList = member_in_list(UserId, OldSortedMembers),
    NewInList = member_in_list(UserId, NewSortedMembers),
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
                    diff_items_to_ops(OldItems, NewItems)
            end
    end.

-spec build_sync_response(integer(), list_id(), [range()], guild_state()) -> map().
build_sync_response(GuildId, ListId, Ranges, State) ->
    NormalizedRanges = normalize_ranges(Ranges),
    Groups = get_member_groups(ListId, State),
    SortedMembers = get_sorted_members_for_list(ListId, State),
    Items = build_member_list_items(Groups, SortedMembers, State),
    {OnlineMembers, _OfflineMembers} = partition_members_by_online(SortedMembers, State),
    Ops = lists:map(
        fun({Start, End}) ->
            #{
                <<"op">> => <<"SYNC">>,
                <<"range">> => [Start, End],
                <<"items">> => slice_items(Items, Start, End)
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
    Members = map_utils:ensure_list(maps:get(<<"members">>, Data, [])),
    {OnlineMembers, _} = partition_members_by_online(Members, State),
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
                    send_member_list_update_to_sessions(ListId, ListSubs, Sessions, Payload, UpdatedState);
                _ ->
                    ok
            end
        end,
        MemberListSubs
    ),
    ok.

-spec send_member_list_update_to_sessions(list_id(), map(), map(), map(), guild_state()) -> ok.
send_member_list_update_to_sessions(ListId, ListSubs, Sessions, Payload, State) ->
    ChannelId = list_id_channel_id(ListId),
    maps:foreach(
        fun(SessionId, _Ranges) ->
            case maps:get(SessionId, Sessions, undefined) of
                #{pid := SessionPid} = SessionData ->
                    case is_pid(SessionPid) of
                        true ->
                            case session_can_view_channel(SessionData, ChannelId, State) of
                                true ->
                                    gen_server:cast(SessionPid, {dispatch, guild_member_list_update, Payload});
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
    case presence_move_ops(UserId, OldState, UpdatedState, OldItems, Items) of
        {true, Ops} ->
            {MemberCount, OnlineCount, Groups, Ops, true};
        {false, _} ->
            Ops = diff_items_to_ops(OldItems, Items),
            Changed = Ops =/= [] orelse OldCount =/= MemberCount orelse OldOnline =/= OnlineCount orelse OldGroups =/= Groups,
            {MemberCount, OnlineCount, Groups, Ops, Changed}
    end.

presence_move_ops(UserId, OldState, UpdatedState, OldItems, NewItems) ->
    case presence_status_changed(UserId, OldState, UpdatedState) of
        false ->
            {false, []};
        true ->
            case {find_member_entry(UserId, OldItems), find_member_entry(UserId, NewItems)} of
                {{ok, OldIdx, _}, {ok, NewIdx, NewItem}} ->
                    case OldIdx =:= NewIdx of
                        true ->
                            {false, []};
                        false ->
                            DeleteOps = delete_ops(OldIdx, 1),
                            InsertIdx = adjusted_insert_index(OldIdx, NewIdx),
                            InsertOps = insert_ops(InsertIdx, [NewItem]),
                            {true, DeleteOps ++ InsertOps}
                    end;
                _ ->
                    {false, []}
            end
    end.

presence_status_changed(UserId, OldState, UpdatedState) ->
    OldPresence = resolve_presence_for_user(OldState, UserId),
    NewPresence = resolve_presence_for_user(UpdatedState, UserId),
    OldStatus = maps:get(<<"status">>, OldPresence, <<"offline">>),
    NewStatus = maps:get(<<"status">>, NewPresence, <<"offline">>),
    OldStatus =/= NewStatus.

find_member_entry(UserId, Items) ->
    find_member_entry(UserId, Items, 0).

find_member_entry(_UserId, [], _Index) ->
    {error, not_found};
find_member_entry(UserId, [Item | Rest], Index) ->
    case maps:get(<<"member">>, Item, undefined) of
        undefined ->
            find_member_entry(UserId, Rest, Index + 1);
        Member ->
            case get_member_user_id(Member) =:= UserId of
                true -> {ok, Index, Item};
                false -> find_member_entry(UserId, Rest, Index + 1)
            end
    end.

adjusted_insert_index(OldIdx, NewIdx) when NewIdx > OldIdx ->
    NewIdx - 1;
adjusted_insert_index(_OldIdx, NewIdx) ->
    NewIdx.

-spec member_list_snapshot(list_id(), guild_state()) ->
    {non_neg_integer(), non_neg_integer(), [group_item()], [list_item()]}.
member_list_snapshot(ListId, State) ->
    Groups = get_member_groups(ListId, State),
    SortedMembers = get_sorted_members_for_list(ListId, State),
    Items = build_member_list_items(Groups, SortedMembers, State),
    {OnlineMembers, _OfflineMembers} = partition_members_by_online(SortedMembers, State),
    {length(SortedMembers), length(OnlineMembers), Groups, Items}.

-spec broadcast_all_member_list_updates(guild_state()) -> ok.
broadcast_all_member_list_updates(State) ->
    GuildId = maps:get(id, State, 0),
    MemberListSubs = maps:get(member_list_subscriptions, State, #{}),
    Sessions = maps:get(sessions, State, #{}),
    maps:foreach(
        fun(ListId, ListSubs) ->
            ChannelId = list_id_channel_id(ListId),
            maps:foreach(
                fun(SessionId, Ranges) ->
                    case maps:get(SessionId, Sessions, undefined) of
                        SessionData when is_map(SessionData) ->
                            SessionPid = maps:get(pid, SessionData, undefined),
                            case {is_pid(SessionPid), session_can_view_channel(SessionData, ChannelId, State)} of
                                {true, true} ->
                                    SyncResponse = build_sync_response(GuildId, ListId, Ranges, State),
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

-spec broadcast_member_list_updates_for_channel(integer(), guild_state()) -> ok.
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
                            case {is_pid(SessionPid), session_can_view_channel(SessionData, ChannelId, State)} of
                                {true, true} ->
                                    SyncResponse = build_sync_response(GuildId, ListId, Ranges, State),
                                    SyncResponseWithChannel = maps:put(<<"channel_id">>, integer_to_binary(ChannelId), SyncResponse),
                                    gen_server:cast(
                                        SessionPid,
                                        {dispatch, guild_member_list_update, SyncResponseWithChannel}
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

-spec get_hoisted_roles_sorted([map()], integer()) -> [map()].
get_hoisted_roles_sorted(Roles, GuildId) ->
    HoistedRoles = lists:filter(
        fun(Role) ->
            IsHoist = maps:get(<<"hoist">>, Role, false),
            RoleId = map_utils:get_integer(Role, <<"id">>, 0),
            IsHoist andalso RoleId =/= GuildId
        end,
        Roles
    ),
    lists:sort(
        fun(A, B) ->
            PosA = get_effective_hoist_position(A),
            PosB = get_effective_hoist_position(B),
            PosA > PosB
        end,
        HoistedRoles
    ).

-spec get_effective_hoist_position(map()) -> integer().
get_effective_hoist_position(Role) ->
    case maps:get(<<"hoist_position">>, Role, null) of
        null -> maps:get(<<"position">>, Role, 0);
        undefined -> maps:get(<<"position">>, Role, 0);
        HoistPos when is_integer(HoistPos) -> HoistPos;
        _ -> maps:get(<<"position">>, Role, 0)
    end.

-spec filter_members_for_list(list_id(), [map()], guild_state()) -> [map()].
filter_members_for_list(ListId, Members, State) ->
    ChannelId = list_id_channel_id(ListId),
    case ChannelId of
        0 ->
            Members;
        _ ->
            lists:filter(
                fun(Member) ->
                    Uid = get_member_user_id(Member),
                    guild_permissions:can_view_channel(Uid, ChannelId, Member, State)
                end,
                Members
            )
    end.

-spec partition_members_by_online([map()], guild_state()) -> {[map()], [map()]}.
partition_members_by_online(Members, State) ->
    lists:partition(
        fun(Member) ->
            UserId = get_member_user_id(Member),
            Presence = resolve_presence_for_user(State, UserId),
            Status = maps:get(<<"status">>, Presence, <<"offline">>),
            Status =/= <<"offline">> andalso Status =/= <<"invisible">>
        end,
        Members
    ).

-spec build_role_groups([map()], [map()]) -> [map()].
build_role_groups(HoistedRoles, OnlineMembers) ->
    lists:map(
        fun(Role) ->
            RoleId = map_utils:get_integer(Role, <<"id">>, 0),
            Count = count_members_with_top_role(RoleId, OnlineMembers, HoistedRoles),
            #{<<"id">> => integer_to_binary(RoleId), <<"count">> => Count}
        end,
        HoistedRoles
    ).

-spec count_members_with_top_role(integer(), [map()], [map()]) -> non_neg_integer().
count_members_with_top_role(RoleId, Members, HoistedRoles) ->
    HoistedRoleIds = [map_utils:get_integer(R, <<"id">>, 0) || R <- HoistedRoles],
    length(lists:filter(
        fun(Member) ->
            MemberRoles = map_utils:ensure_list(maps:get(<<"roles">>, Member, [])),
            MemberRoleIds = [type_conv:to_integer(R) || R <- MemberRoles],
            TopHoisted = find_top_hoisted_role(MemberRoleIds, HoistedRoleIds),
            TopHoisted =:= RoleId
        end,
        Members
    )).

-spec find_top_hoisted_role([integer()], [integer()]) -> integer() | undefined.
find_top_hoisted_role(MemberRoleIds, HoistedRoleIds) ->
    case lists:filter(fun(R) -> lists:member(R, MemberRoleIds) end, HoistedRoleIds) of
        [] -> undefined;
        [Top | _] -> Top
    end.

-spec count_ungrouped_online([map()], [map()]) -> non_neg_integer().
count_ungrouped_online(OnlineMembers, HoistedRoles) ->
    HoistedRoleIds = [map_utils:get_integer(R, <<"id">>, 0) || R <- HoistedRoles],
    length(lists:filter(
        fun(Member) ->
            MemberRoles = map_utils:ensure_list(maps:get(<<"roles">>, Member, [])),
            MemberRoleIds = [type_conv:to_integer(R) || R <- MemberRoles],
            TopHoisted = find_top_hoisted_role(MemberRoleIds, HoistedRoleIds),
            TopHoisted =:= undefined
        end,
        OnlineMembers
    )).

-spec get_sorted_members_for_list(list_id(), guild_state()) -> [map()].
get_sorted_members_for_list(ListId, State) ->
    Data = maps:get(data, State, #{}),
    Members = map_utils:ensure_list(maps:get(<<"members">>, Data, [])),
    FilteredMembers = filter_members_for_list(ListId, Members, State),
    lists:sort(
        fun(A, B) ->
            get_member_sort_key(A) =< get_member_sort_key(B)
        end,
        FilteredMembers
    ).

-spec get_member_user_id(map()) -> integer().
get_member_user_id(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    map_utils:get_integer(User, <<"id">>, 0).

-spec normalize_name(term()) -> binary().
normalize_name(undefined) -> <<>>;
normalize_name(null) -> <<>>;
normalize_name(<<_/binary>> = B) -> B;
normalize_name(L) when is_list(L) ->
    try unicode:characters_to_binary(L) catch _:_ -> <<>> end;
normalize_name(I) when is_integer(I) ->
    integer_to_binary(I);
normalize_name(_) ->
    <<>>.

-spec get_member_display_name(map()) -> binary().
get_member_display_name(Member) ->
    Nick0 = maps:get(<<"nick">>, Member, undefined),
    Nick = normalize_name(Nick0),
    case Nick =:= <<>> of
        false ->
            Nick;
        true ->
            User = maps:get(<<"user">>, Member, #{}),
            GlobalName = normalize_name(maps:get(<<"global_name">>, User, undefined)),
            case GlobalName =:= <<>> of
                false -> GlobalName;
                true -> normalize_name(maps:get(<<"username">>, User, undefined))
            end
    end.

-spec get_member_sort_key(map()) -> {binary(), integer()}.
get_member_sort_key(Member) ->
    Name = get_member_display_name(Member),
    Folded = casefold_binary(Name),
    {Folded, get_member_user_id(Member)}.

-spec casefold_binary(term()) -> binary().
casefold_binary(Value) ->
    Bin = normalize_name(Value),
    try
        unicode:characters_to_binary(string:casefold(unicode:characters_to_list(Bin)))
    catch
        _:_ -> Bin
    end.

add_presence_to_member(Member, State) ->
    UserId = get_member_user_id(Member),
    Presence = resolve_presence_for_user(State, UserId),
    maps:put(<<"presence">>, Presence, Member).

default_presence() ->
    #{
        <<"status">> => <<"offline">>,
        <<"mobile">> => false,
        <<"afk">> => false
    }.

resolve_presence_for_user(State, UserId) ->
    Presences = maps:get(presences, State, #{}),
    case maps:get(UserId, Presences, undefined) of
        undefined -> fetch_presence_from_cache(UserId);
        Presence -> Presence
    end.

fetch_presence_from_cache(UserId) ->
    try presence_cache:get(UserId) of
        {ok, Presence} -> Presence;
        _ -> default_presence()
    catch
        exit:{noproc, _} -> default_presence()
    end.

-spec build_member_list_items([group_item()], [map()], guild_state()) -> [list_item()].
build_member_list_items(Groups, Members, State) ->
    Data = maps:get(data, State, #{}),
    Roles = map_utils:ensure_list(maps:get(<<"roles">>, Data, [])),
    GuildId = maps:get(id, State, 0),
    HoistedRoles = get_hoisted_roles_sorted(Roles, GuildId),
    HoistedRoleIds = [map_utils:get_integer(R, <<"id">>, 0) || R <- HoistedRoles],
    {OnlineMembers, OfflineMembers} = partition_members_by_online(Members, State),
    lists:flatmap(
        fun(Group) ->
            GroupId = maps:get(<<"id">>, Group),
            GroupHeader = #{<<"group">> => Group},
            case GroupId of
                <<"online">> ->
                    UngroupedOnline = lists:filter(
                        fun(M) ->
                            MemberRoles = map_utils:ensure_list(maps:get(<<"roles">>, M, [])),
                            MemberRoleIds = [type_conv:to_integer(R) || R <- MemberRoles],
                            find_top_hoisted_role(MemberRoleIds, HoistedRoleIds) =:= undefined
                        end,
                        OnlineMembers
                    ),
                    [GroupHeader | [#{<<"member">> => add_presence_to_member(M, State)} || M <- UngroupedOnline]];
                <<"offline">> ->
                    [GroupHeader | [#{<<"member">> => add_presence_to_member(M, State)} || M <- OfflineMembers]];
                RoleIdBin ->
                    RoleId = type_conv:to_integer(RoleIdBin),
                    RoleMembers = lists:filter(
                        fun(M) ->
                            MemberRoles = map_utils:ensure_list(maps:get(<<"roles">>, M, [])),
                            MemberRoleIds = [type_conv:to_integer(R) || R <- MemberRoles],
                            find_top_hoisted_role(MemberRoleIds, HoistedRoleIds) =:= RoleId
                        end,
                        OnlineMembers
                    ),
                    [GroupHeader | [#{<<"member">> => add_presence_to_member(M, State)} || M <- RoleMembers]]
            end
        end,
        Groups
    ).

-spec slice_items([list_item()], non_neg_integer(), non_neg_integer()) -> [list_item()].
slice_items(Items, Start, End) ->
    SafeEnd = min(End, length(Items) - 1),
    case Start > SafeEnd of
        true -> [];
        false -> lists:sublist(Items, Start + 1, SafeEnd - Start + 1)
    end.

-spec member_in_list(integer(), [map()]) -> boolean().
member_in_list(UserId, Members) ->
    lists:any(fun(M) -> get_member_user_id(M) =:= UserId end, Members).

-spec build_full_items(list_id(), guild_state(), [map()]) -> [list_item()].
build_full_items(ListId, State, SortedMembers) ->
    Groups = get_member_groups(ListId, State),
    build_member_list_items(Groups, SortedMembers, State).

-spec full_sync_ops(list_id(), guild_state()) -> [map()].
full_sync_ops(ListId, State) ->
    SortedMembers = get_sorted_members_for_list(ListId, State),
    Items = build_full_items(ListId, State, SortedMembers),
    case length(Items) of
        0 -> [];
        N ->
            [#{
                <<"op">> => <<"SYNC">>,
                <<"range">> => [0, N - 1],
                <<"items">> => Items
            }]
    end.

-spec upsert_member_in_state(integer(), map(), guild_state()) -> {map() | undefined, map(), guild_state()}.
upsert_member_in_state(UserId, MemberUpdate, State) ->
    Data = maps:get(data, State, #{}),
    Members0 = map_utils:ensure_list(maps:get(<<"members">>, Data, [])),
    {Found, CurrentMember} = find_member_by_user_id(UserId, Members0),
    UpdatedMember =
        case Found of
            true -> deep_merge_member(CurrentMember, MemberUpdate);
            false -> MemberUpdate
        end,
    Members1 =
        case Found of
            true ->
                lists:map(
                    fun(M) ->
                        case get_member_user_id(M) =:= UserId of
                            true -> UpdatedMember;
                            false -> M
                        end
                    end,
                    Members0
                );
            false ->
                Members0 ++ [UpdatedMember]
        end,
    Data1 = maps:put(<<"members">>, Members1, Data),
    NewState = maps:put(data, Data1, State),
    {case Found of true -> CurrentMember; false -> undefined end, UpdatedMember, NewState}.

-spec find_member_by_user_id(integer(), [map()]) -> {boolean(), map()} | {false, undefined}.
find_member_by_user_id(UserId, Members) ->
    case lists:search(fun(M) -> get_member_user_id(M) =:= UserId end, Members) of
        {value, M} -> {true, M};
        false -> {false, undefined}
    end.

-spec deep_merge_member(map(), map()) -> map().
deep_merge_member(CurrentMember, MemberUpdate) ->
    User0 = maps:get(<<"user">>, CurrentMember, #{}),
    UserU = maps:get(<<"user">>, MemberUpdate, undefined),
    MemberUpdate1 =
        case UserU of
            undefined ->
                MemberUpdate;
            null ->
                MemberUpdate;
            UM when is_map(UM) ->
                maps:put(<<"user">>, maps:merge(User0, UM), MemberUpdate);
            _ ->
                MemberUpdate
        end,
    maps:merge(CurrentMember, MemberUpdate1).

-spec diff_items_to_ops([list_item()], [list_item()]) -> [map()].
diff_items_to_ops(OldItems, NewItems) ->
    OldKeys = item_keys(OldItems),
    NewKeys = item_keys(NewItems),
    case OldKeys =:= NewKeys of
        true ->
            updates_for_changed_items(OldItems, NewItems);
        false ->
            LenOld = length(OldKeys),
            LenNew = length(NewKeys),
            case LenOld =:= LenNew of
                true ->
                    case mismatch_span(OldKeys, NewKeys) of
                        none ->
                            [];
                        {Start, End} ->
                            [sync_range_op(Start, End, NewItems)]
                    end;
                false ->
                    case try_pure_insert_delete(OldItems, NewItems, OldKeys, NewKeys) of
                        {ok, Ops} -> Ops;
                        error -> full_sync_from_items(NewItems)
                    end
            end
    end.

-spec full_sync_from_items([list_item()]) -> [map()].
full_sync_from_items(Items) ->
    case length(Items) of
        0 -> [];
        N ->
            [#{
                <<"op">> => <<"SYNC">>,
                <<"range">> => [0, N - 1],
                <<"items">> => Items
            }]
    end.

-spec item_keys([list_item()]) -> [term()].
item_keys(Items) ->
    lists:map(fun item_key/1, Items).

-spec item_key(list_item()) -> term().
item_key(Item) ->
    case maps:get(<<"group">>, Item, undefined) of
        undefined ->
            case maps:get(<<"member">>, Item, undefined) of
                undefined ->
                    unknown;
                Member ->
                    {member, get_member_user_id(Member)}
            end;
        Group ->
            {group, maps:get(<<"id">>, Group, <<>>)}
    end.

-spec updates_for_changed_items([list_item()], [list_item()]) -> [map()].
updates_for_changed_items(OldItems, NewItems) ->
    lists:reverse(
        lists:foldl(
            fun({Idx, OldItem, NewItem}, Acc) ->
                case OldItem =:= NewItem of
                    true ->
                        Acc;
                    false ->
                        [#{
                            <<"op">> => <<"UPDATE">>,
                            <<"index">> => Idx,
                            <<"item">> => NewItem
                        } | Acc]
                end
            end,
            [],
            zip_with_index(OldItems, NewItems)
        )
    ).

-spec zip_with_index([term()], [term()]) -> [{non_neg_integer(), term(), term()}].
zip_with_index(A, B) ->
    zip_with_index(A, B, 0, []).
zip_with_index([], [], _I, Acc) ->
    lists:reverse(Acc);
zip_with_index([HA | TA], [HB | TB], I, Acc) ->
    zip_with_index(TA, TB, I + 1, [{I, HA, HB} | Acc]);
zip_with_index(_, _, _I, Acc) ->
    lists:reverse(Acc).

-spec mismatch_span([term()], [term()]) -> none | {non_neg_integer(), non_neg_integer()}.
mismatch_span(A, B) ->
    mismatch_span(A, B, 0, none).
mismatch_span([], [], _I, none) ->
    none;
mismatch_span([], [], _I, {S, E}) ->
    {S, E};
mismatch_span([HA | TA], [HB | TB], I, Span) ->
    Span1 =
        case HA =:= HB of
            true ->
                Span;
            false ->
                case Span of
                    none -> {I, I};
                    {S, _E} -> {S, I}
                end
        end,
    mismatch_span(TA, TB, I + 1, Span1);
mismatch_span(_, _, _I, none) ->
    none;
mismatch_span(_, _, _I, {S, E}) ->
    {S, E}.

-spec sync_range_op(non_neg_integer(), non_neg_integer(), [list_item()]) -> map().
sync_range_op(Start, End, NewItems) ->
    #{
        <<"op">> => <<"SYNC">>,
        <<"range">> => [Start, End],
        <<"items">> => slice_items(NewItems, Start, End)
    }.

-spec try_pure_insert_delete([list_item()], [list_item()], [term()], [term()]) -> {ok, [map()]} | error.
try_pure_insert_delete(OldItems, NewItems, OldKeys, NewKeys) ->
    LenOld = length(OldKeys),
    LenNew = length(NewKeys),
    case {LenOld < LenNew, LenOld > LenNew} of
        {true, _} ->
            Diff = LenNew - LenOld,
            Idx = first_mismatch_index(OldKeys, NewKeys),
            SuffixOld = lists:nthtail(Idx, OldKeys),
            SuffixNew = lists:nthtail(Idx + Diff, NewKeys),
            case SuffixOld =:= SuffixNew of
                true ->
                    InsertedItems = slice_items(NewItems, Idx, Idx + Diff - 1),
                    OldAfter = insert_many(OldItems, Idx, InsertedItems),
                    InsertOps = insert_ops(Idx, InsertedItems),
                    UpdateOps = updates_for_changed_items(OldAfter, NewItems),
                    {ok, InsertOps ++ UpdateOps};
                false ->
                    error
            end;
        {_, true} ->
            Diff = LenOld - LenNew,
            Idx = first_mismatch_index(OldKeys, NewKeys),
            SuffixOld = lists:nthtail(Idx + Diff, OldKeys),
            SuffixNew = lists:nthtail(Idx, NewKeys),
            case SuffixOld =:= SuffixNew of
                true ->
                    OldAfter = delete_many(OldItems, Idx, Diff),
                    DeleteOps = delete_ops(Idx, Diff),
                    UpdateOps = updates_for_changed_items(OldAfter, NewItems),
                    {ok, DeleteOps ++ UpdateOps};
                false ->
                    error
            end;
        _ ->
            error
    end.

-spec first_mismatch_index([term()], [term()]) -> non_neg_integer().
first_mismatch_index(A, B) ->
    first_mismatch_index(A, B, 0).
first_mismatch_index([], _B, I) ->
    I;
first_mismatch_index(_A, [], I) ->
    I;
first_mismatch_index([HA | TA], [HB | TB], I) ->
    case HA =:= HB of
        true -> first_mismatch_index(TA, TB, I + 1);
        false -> I
    end.

-spec insert_many([term()], non_neg_integer(), [term()]) -> [term()].
insert_many(List, Index, ToInsert) ->
    {Left, Right} = lists:split(Index, List),
    Left ++ ToInsert ++ Right.

-spec delete_many([term()], non_neg_integer(), non_neg_integer()) -> [term()].
delete_many(List, Index, Count) ->
    {Left, Rest} = lists:split(Index, List),
    {_Drop, Right} = lists:split(Count, Rest),
    Left ++ Right.

-spec insert_ops(non_neg_integer(), [list_item()]) -> [map()].
insert_ops(StartIdx, Items) ->
    insert_ops(StartIdx, Items, 0, []).
insert_ops(_StartIdx, [], _Offset, Acc) ->
    lists:reverse(Acc);
insert_ops(StartIdx, [Item | Rest], Offset, Acc) ->
    Op = #{
        <<"op">> => <<"INSERT">>,
        <<"index">> => StartIdx + Offset,
        <<"item">> => Item
    },
    insert_ops(StartIdx, Rest, Offset + 1, [Op | Acc]).

-spec delete_ops(non_neg_integer(), non_neg_integer()) -> [map()].
delete_ops(_Idx, 0) ->
    [];
delete_ops(Idx, Count) ->
    lists:map(
        fun(_) ->
            #{
                <<"op">> => <<"DELETE">>,
                <<"index">> => Idx
            }
        end,
        lists:seq(1, Count)
    ).

-spec session_can_view_channel(map(), integer(), guild_state()) -> boolean().
session_can_view_channel(_SessionData, ChannelId, _State) when not is_integer(ChannelId); ChannelId =< 0 ->
    false;
session_can_view_channel(SessionData, ChannelId, State) ->
    case maps:get(user_id, SessionData, undefined) of
        UserId when is_integer(UserId) ->
            guild_permissions:can_view_channel(UserId, ChannelId, undefined, State);
        _ ->
            false
    end.

-spec list_id_channel_id(list_id()) -> integer().
list_id_channel_id(ListId) when is_binary(ListId) ->
    case type_conv:to_integer(ListId) of
        undefined -> 0;
        Id -> Id
    end;
list_id_channel_id(_) ->
    0.

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

list_id_channel_id_parses_binary_test() ->
    ?assertEqual(500, list_id_channel_id(<<"500">>)).

list_id_channel_id_invalid_value_test() ->
    ?assertEqual(0, list_id_channel_id(<<"abc">>)).

subscribe_ranges_test() ->
    State = #{member_list_subscriptions => #{}},
    {NewState, ShouldSync, NormalizedRanges} =
        subscribe_ranges(<<"session1">>, <<"500">>, [{0, 99}], State),
    Subs = maps:get(member_list_subscriptions, NewState),
    ChannelSubs = maps:get(<<"500">>, Subs),
    ?assertEqual([{0, 99}], maps:get(<<"session1">>, ChannelSubs)),
    ?assertEqual(true, ShouldSync),
    ?assertEqual([{0, 99}], NormalizedRanges).

find_top_hoisted_role_respects_position_test() ->
    MemberRoleIds = [100, 200, 300],
    HoistedRoleIdsSortedByPosition = [300, 200],
    ?assertEqual(300, find_top_hoisted_role(MemberRoleIds, HoistedRoleIdsSortedByPosition)).

find_top_hoisted_role_returns_undefined_when_no_match_test() ->
    MemberRoleIds = [100, 400],
    HoistedRoleIdsSortedByPosition = [300, 200],
    ?assertEqual(undefined, find_top_hoisted_role(MemberRoleIds, HoistedRoleIdsSortedByPosition)).

find_top_hoisted_role_handles_single_hoisted_role_test() ->
    MemberRoleIds = [100, 200, 300],
    HoistedRoleIdsSortedByPosition = [200],
    ?assertEqual(200, find_top_hoisted_role(MemberRoleIds, HoistedRoleIdsSortedByPosition)).

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

-endif.
