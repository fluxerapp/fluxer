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

-module(guild_member_list_common).

-export([
    get_member_user_id/1,
    get_member_display_name/1,
    get_member_sort_key/1,
    normalize_name/1,
    casefold_binary/1,
    get_hoisted_roles_sorted/2,
    get_effective_hoist_position/1,
    build_role_groups/2,
    count_members_with_top_role/3,
    find_top_hoisted_role/2,
    count_ungrouped_online/2,
    default_presence/0,
    resolve_presence_for_user/2,
    add_presence_to_member/2,
    connected_session_user_ids/1,
    partition_members_by_online/2,
    filter_members_for_list/3,
    list_id_channel_id/1,
    session_can_view_channel/3,
    build_member_list_items/3,
    slice_items/3,
    update_subscriptions/4,
    remove_session_from_subscriptions/2,
    diff_items_to_ops/2,
    full_sync_from_items/1,
    item_keys/1,
    item_key/1,
    updates_for_changed_items/2,
    zip_with_index/2,
    mismatch_span/2,
    sync_range_op/3,
    try_pure_insert_delete/4,
    first_mismatch_index/2,
    insert_many/3,
    delete_many/3,
    insert_ops/2,
    delete_ops/2,
    find_member_entry/2,
    adjusted_insert_index/2,
    member_in_list/2,
    presence_move_ops/5,
    presence_status_changed/3,
    deep_merge_member/2,
    upsert_member_in_state/3
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
-type channel_id() :: integer().
-type session_id() :: binary().

-spec get_member_user_id(map()) -> integer().
get_member_user_id(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    map_utils:get_integer(User, <<"id">>, 0).

-spec normalize_name(term()) -> binary().
normalize_name(undefined) ->
    <<>>;
normalize_name(null) ->
    <<>>;
normalize_name(<<_/binary>> = B) ->
    B;
normalize_name(L) when is_list(L) ->
    try
        unicode:characters_to_binary(L)
    catch
        _:_ -> <<>>
    end;
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
    length(
        lists:filter(
            fun(Member) ->
                MemberRoles = map_utils:ensure_list(maps:get(<<"roles">>, Member, [])),
                MemberRoleIds = [type_conv:to_integer(R) || R <- MemberRoles],
                TopHoisted = find_top_hoisted_role(MemberRoleIds, HoistedRoleIds),
                TopHoisted =:= RoleId
            end,
            Members
        )
    ).

-spec find_top_hoisted_role([integer()], [integer()]) -> integer() | undefined.
find_top_hoisted_role(MemberRoleIds, HoistedRoleIds) ->
    case lists:filter(fun(R) -> lists:member(R, MemberRoleIds) end, HoistedRoleIds) of
        [] -> undefined;
        [Top | _] -> Top
    end.

-spec count_ungrouped_online([map()], [map()]) -> non_neg_integer().
count_ungrouped_online(OnlineMembers, HoistedRoles) ->
    HoistedRoleIds = [map_utils:get_integer(R, <<"id">>, 0) || R <- HoistedRoles],
    length(
        lists:filter(
            fun(Member) ->
                MemberRoles = map_utils:ensure_list(maps:get(<<"roles">>, Member, [])),
                MemberRoleIds = [type_conv:to_integer(R) || R <- MemberRoles],
                TopHoisted = find_top_hoisted_role(MemberRoleIds, HoistedRoleIds),
                TopHoisted =:= undefined
            end,
            OnlineMembers
        )
    ).

-spec default_presence() -> map().
default_presence() ->
    #{
        <<"status">> => <<"offline">>,
        <<"mobile">> => false,
        <<"afk">> => false
    }.

-spec resolve_presence_for_user(guild_state(), user_id()) -> map().
resolve_presence_for_user(State, UserId) ->
    MemberPresence = maps:get(member_presence, State, #{}),
    maps:get(UserId, MemberPresence, default_presence()).

-spec add_presence_to_member(map(), guild_state()) -> map().
add_presence_to_member(Member, State) ->
    UserId = get_member_user_id(Member),
    Presence = resolve_presence_for_user(State, UserId),
    maps:put(<<"presence">>, Presence, Member).

-spec connected_session_user_ids(guild_state()) -> sets:set().
connected_session_user_ids(State) ->
    Sessions = maps:get(sessions, State, #{}),
    maps:fold(
        fun(_SessionId, SessionData, Acc) ->
            case maps:get(user_id, SessionData, undefined) of
                UserId when is_integer(UserId), UserId > 0 ->
                    sets:add_element(UserId, Acc);
                _ ->
                    Acc
            end
        end,
        sets:new(),
        Sessions
    ).

-spec partition_members_by_online([map()], guild_state()) -> {[map()], [map()]}.
partition_members_by_online(Members, State) ->
    ConnectedUserIds = connected_session_user_ids(State),
    lists:partition(
        fun(Member) ->
            UserId = get_member_user_id(Member),
            Presence = resolve_presence_for_user(State, UserId),
            Status = maps:get(<<"status">>, Presence, <<"offline">>),
            IsConnected = sets:is_element(UserId, ConnectedUserIds),
            IsOnlineStatus = Status =/= <<"offline">> andalso Status =/= <<"invisible">>,
            IsConnected andalso IsOnlineStatus
        end,
        Members
    ).

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

-spec list_id_channel_id(list_id()) -> channel_id().
list_id_channel_id(ListId) when is_binary(ListId) ->
    case type_conv:to_integer(ListId) of
        undefined -> 0;
        Id -> Id
    end;
list_id_channel_id(_) ->
    0.

-spec session_can_view_channel(map(), channel_id(), guild_state()) -> boolean().
session_can_view_channel(_SessionData, ChannelId, _State) when
    not is_integer(ChannelId); ChannelId =< 0
->
    false;
session_can_view_channel(SessionData, ChannelId, State) ->
    case {maps:get(user_id, SessionData, undefined), maps:get(viewable_channels, SessionData, undefined)} of
        {UserId, ViewableChannels} when is_integer(UserId), is_map(ViewableChannels) ->
            maps:is_key(ChannelId, ViewableChannels) orelse
                guild_permissions:can_view_channel(UserId, ChannelId, undefined, State);
        {UserId, _} when is_integer(UserId) ->
            guild_permissions:can_view_channel(UserId, ChannelId, undefined, State);
        _ ->
            false
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
                    [
                        GroupHeader
                        | [
                            #{<<"member">> => add_presence_to_member(M, State)}
                         || M <- UngroupedOnline
                        ]
                    ];
                <<"offline">> ->
                    [
                        GroupHeader
                        | [
                            #{<<"member">> => add_presence_to_member(M, State)}
                         || M <- OfflineMembers
                        ]
                    ];
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
                    [
                        GroupHeader
                        | [#{<<"member">> => add_presence_to_member(M, State)} || M <- RoleMembers]
                    ]
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

-spec update_subscriptions(session_id(), list_id(), [range()], map()) ->
    {map(), [range()], boolean()}.
update_subscriptions(SessionId, ListId, NormalizedRanges, Subscriptions) ->
    ListSubs0 = maps:get(ListId, Subscriptions, #{}),
    OldRanges = maps:get(SessionId, ListSubs0, []),
    NewSubscriptions =
        case NormalizedRanges of
            [] ->
                Trimmed = maps:remove(SessionId, ListSubs0),
                case map_size(Trimmed) of
                    0 -> maps:remove(ListId, Subscriptions);
                    _ -> maps:put(ListId, Trimmed, Subscriptions)
                end;
            _ ->
                Updated = maps:put(SessionId, NormalizedRanges, ListSubs0),
                maps:put(ListId, Updated, Subscriptions)
        end,
    ShouldSync =
        case NormalizedRanges of
            [] -> false;
            _ -> NormalizedRanges =/= OldRanges
        end,
    {NewSubscriptions, OldRanges, ShouldSync}.

-spec remove_session_from_subscriptions(session_id(), map()) -> map().
remove_session_from_subscriptions(SessionId, Subscriptions) ->
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
    ).

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
        0 ->
            [];
        N ->
            [
                #{
                    <<"op">> => <<"SYNC">>,
                    <<"range">> => [0, N - 1],
                    <<"items">> => Items
                }
            ]
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
                        [
                            #{
                                <<"op">> => <<"UPDATE">>,
                                <<"index">> => Idx,
                                <<"item">> => NewItem
                            }
                            | Acc
                        ]
                end
            end,
            [],
            zip_with_index(OldItems, NewItems)
        )
    ).

-spec zip_with_index([term()], [term()]) -> [{non_neg_integer(), term(), term()}].
zip_with_index(A, B) ->
    zip_with_index(A, B, 0, []).

-spec zip_with_index([term()], [term()], non_neg_integer(), [{non_neg_integer(), term(), term()}]) ->
    [{non_neg_integer(), term(), term()}].
zip_with_index([], [], _I, Acc) ->
    lists:reverse(Acc);
zip_with_index([HA | TA], [HB | TB], I, Acc) ->
    zip_with_index(TA, TB, I + 1, [{I, HA, HB} | Acc]);
zip_with_index(_, _, _I, Acc) ->
    lists:reverse(Acc).

-spec mismatch_span([term()], [term()]) -> none | {non_neg_integer(), non_neg_integer()}.
mismatch_span(A, B) ->
    mismatch_span(A, B, 0, none).

-spec mismatch_span(
    [term()], [term()], non_neg_integer(), none | {non_neg_integer(), non_neg_integer()}
) ->
    none | {non_neg_integer(), non_neg_integer()}.
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

-spec try_pure_insert_delete([list_item()], [list_item()], [term()], [term()]) ->
    {ok, [map()]} | error.
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

-spec first_mismatch_index([term()], [term()], non_neg_integer()) -> non_neg_integer().
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

-spec insert_ops(non_neg_integer(), [list_item()], non_neg_integer(), [map()]) -> [map()].
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

-spec find_member_entry(user_id(), [list_item()]) ->
    {ok, non_neg_integer(), list_item()} | {error, not_found}.
find_member_entry(UserId, Items) ->
    find_member_entry(UserId, Items, 0).

-spec find_member_entry(user_id(), [list_item()], non_neg_integer()) ->
    {ok, non_neg_integer(), list_item()} | {error, not_found}.
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

-spec adjusted_insert_index(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
adjusted_insert_index(OldIdx, NewIdx) when NewIdx > OldIdx ->
    NewIdx - 1;
adjusted_insert_index(_OldIdx, NewIdx) ->
    NewIdx.

-spec member_in_list(user_id(), [map()]) -> boolean().
member_in_list(UserId, Members) ->
    lists:any(fun(M) -> get_member_user_id(M) =:= UserId end, Members).

-spec presence_move_ops(user_id(), guild_state(), guild_state(), [list_item()], [list_item()]) ->
    {boolean(), [map()]}.
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

-spec presence_status_changed(user_id(), guild_state(), guild_state()) -> boolean().
presence_status_changed(UserId, OldState, UpdatedState) ->
    OldPresence = resolve_presence_for_user(OldState, UserId),
    NewPresence = resolve_presence_for_user(UpdatedState, UserId),
    OldStatus = maps:get(<<"status">>, OldPresence, <<"offline">>),
    NewStatus = maps:get(<<"status">>, NewPresence, <<"offline">>),
    OldStatus =/= NewStatus.

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

-spec upsert_member_in_state(user_id(), map(), guild_state()) ->
    {map() | undefined, map(), guild_state()}.
upsert_member_in_state(UserId, MemberUpdate, State) ->
    Data = maps:get(data, State, #{}),
    Members0 = guild_data_index:member_map(Data),
    CurrentMember = maps:get(UserId, Members0, undefined),
    UpdatedMember =
        case CurrentMember of
            undefined -> MemberUpdate;
            _ -> deep_merge_member(CurrentMember, MemberUpdate)
        end,
    Members1 = maps:put(UserId, UpdatedMember, Members0),
    Data1 = guild_data_index:put_member_map(Members1, Data),
    NewState = maps:put(data, Data1, State),
    {
        CurrentMember,
        UpdatedMember,
        NewState
    }.

-ifdef(TEST).

get_member_user_id_extracts_id_test() ->
    Member = #{<<"user">> => #{<<"id">> => <<"42">>}},
    ?assertEqual(42, get_member_user_id(Member)).

get_member_user_id_returns_zero_for_missing_user_test() ->
    ?assertEqual(0, get_member_user_id(#{})).

get_member_display_name_prefers_nick_test() ->
    Member = #{
        <<"nick">> => <<"Cool Nick">>,
        <<"user">> => #{<<"global_name">> => <<"Global">>, <<"username">> => <<"user">>}
    },
    ?assertEqual(<<"Cool Nick">>, get_member_display_name(Member)).

get_member_display_name_falls_back_to_global_name_test() ->
    Member = #{
        <<"user">> => #{<<"global_name">> => <<"Global">>, <<"username">> => <<"user">>}
    },
    ?assertEqual(<<"Global">>, get_member_display_name(Member)).

get_member_display_name_falls_back_to_username_test() ->
    Member = #{
        <<"user">> => #{<<"username">> => <<"user">>}
    },
    ?assertEqual(<<"user">>, get_member_display_name(Member)).

find_top_hoisted_role_respects_position_test() ->
    MemberRoleIds = [100, 200, 300],
    HoistedRoleIdsSortedByPosition = [300, 200],
    ?assertEqual(300, find_top_hoisted_role(MemberRoleIds, HoistedRoleIdsSortedByPosition)).

find_top_hoisted_role_returns_undefined_when_no_match_test() ->
    MemberRoleIds = [100, 400],
    HoistedRoleIdsSortedByPosition = [300, 200],
    ?assertEqual(undefined, find_top_hoisted_role(MemberRoleIds, HoistedRoleIdsSortedByPosition)).

list_id_channel_id_parses_binary_test() ->
    ?assertEqual(500, list_id_channel_id(<<"500">>)).

list_id_channel_id_invalid_value_test() ->
    ?assertEqual(0, list_id_channel_id(<<"abc">>)).

session_can_view_channel_uses_cached_visibility_test() ->
    SessionData = #{user_id => 12, viewable_channels => #{500 => true}},
    State = #{data => #{<<"members">> => #{}}},
    ?assertEqual(true, session_can_view_channel(SessionData, 500, State)).

session_can_view_channel_rejects_when_cache_misses_and_user_missing_test() ->
    SessionData = #{user_id => 99, viewable_channels => #{}},
    State = #{data => #{<<"members">> => #{}}},
    ?assertEqual(false, session_can_view_channel(SessionData, 500, State)).

update_subscriptions_adds_session_test() ->
    Subs = #{},
    {NewSubs, _OldRanges, ShouldSync} =
        update_subscriptions(<<"s1">>, <<"500">>, [{0, 99}], Subs),
    ListSubs = maps:get(<<"500">>, NewSubs),
    ?assertEqual([{0, 99}], maps:get(<<"s1">>, ListSubs)),
    ?assertEqual(true, ShouldSync).

update_subscriptions_removes_session_on_empty_ranges_test() ->
    Subs = #{<<"500">> => #{<<"s1">> => [{0, 99}]}},
    {NewSubs, _OldRanges, ShouldSync} =
        update_subscriptions(<<"s1">>, <<"500">>, [], Subs),
    ?assertEqual(#{}, NewSubs),
    ?assertEqual(false, ShouldSync).

remove_session_from_subscriptions_removes_all_lists_test() ->
    Subs = #{
        <<"500">> => #{<<"s1">> => [{0, 99}], <<"s2">> => [{0, 50}]},
        <<"600">> => #{<<"s1">> => [{0, 50}]}
    },
    NewSubs = remove_session_from_subscriptions(<<"s1">>, Subs),
    ?assertEqual(#{<<"500">> => #{<<"s2">> => [{0, 50}]}}, NewSubs).

remove_session_from_subscriptions_removes_empty_lists_test() ->
    Subs = #{<<"500">> => #{<<"s1">> => [{0, 99}]}},
    NewSubs = remove_session_from_subscriptions(<<"s1">>, Subs),
    ?assertEqual(#{}, NewSubs).

slice_items_basic_test() ->
    Items = [a, b, c, d, e],
    ?assertEqual([b, c, d], slice_items(Items, 1, 3)).

slice_items_empty_when_start_exceeds_end_test() ->
    Items = [a, b, c],
    ?assertEqual([], slice_items(Items, 5, 3)).

deep_merge_member_merges_user_test() ->
    Current = #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}, <<"nick">> => <<"a">>},
    Update = #{<<"user">> => #{<<"id">> => <<"1">>, <<"avatar">> => <<"new">>}, <<"nick">> => <<"b">>},
    Result = deep_merge_member(Current, Update),
    User = maps:get(<<"user">>, Result),
    ?assertEqual(<<"alice">>, maps:get(<<"username">>, User)),
    ?assertEqual(<<"new">>, maps:get(<<"avatar">>, User)),
    ?assertEqual(<<"b">>, maps:get(<<"nick">>, Result)).

sort_key_same_display_name_deterministic_by_user_id_test() ->
    MemberA = #{<<"nick">> => <<"Alice">>, <<"user">> => #{<<"id">> => <<"100">>}},
    MemberB = #{<<"nick">> => <<"Alice">>, <<"user">> => #{<<"id">> => <<"200">>}},
    KeyA = get_member_sort_key(MemberA),
    KeyB = get_member_sort_key(MemberB),
    ?assert(KeyA < KeyB).

sort_key_same_name_lower_id_first_test() ->
    MemberA = #{<<"nick">> => <<"zebra">>, <<"user">> => #{<<"id">> => <<"1">>}},
    MemberB = #{<<"nick">> => <<"zebra">>, <<"user">> => #{<<"id">> => <<"999">>}},
    ?assert(get_member_sort_key(MemberA) < get_member_sort_key(MemberB)).

sort_key_mixed_case_names_test() ->
    MemberLower = #{<<"nick">> => <<"alice">>, <<"user">> => #{<<"id">> => <<"1">>}},
    MemberUpper = #{<<"nick">> => <<"Alice">>, <<"user">> => #{<<"id">> => <<"2">>}},
    KeyLower = get_member_sort_key(MemberLower),
    KeyUpper = get_member_sort_key(MemberUpper),
    {FoldedLower, _} = KeyLower,
    {FoldedUpper, _} = KeyUpper,
    ?assertEqual(FoldedLower, FoldedUpper).

sort_key_unicode_names_test() ->
    MemberUnicode = #{<<"nick">> => <<"Ωmega"/utf8>>, <<"user">> => #{<<"id">> => <<"1">>}},
    MemberAscii = #{<<"nick">> => <<"alpha">>, <<"user">> => #{<<"id">> => <<"2">>}},
    KeyU = get_member_sort_key(MemberUnicode),
    KeyA = get_member_sort_key(MemberAscii),
    ?assert(is_tuple(KeyU)),
    ?assert(is_tuple(KeyA)).

sort_key_empty_display_name_test() ->
    MemberEmpty = #{<<"user">> => #{<<"id">> => <<"1">>}},
    Key = get_member_sort_key(MemberEmpty),
    {Folded, Id} = Key,
    ?assertEqual(<<>>, Folded),
    ?assertEqual(1, Id).

sort_key_empty_name_sorts_before_nonempty_test() ->
    MemberEmpty = #{<<"user">> => #{<<"id">> => <<"1">>}},
    MemberNamed = #{<<"nick">> => <<"alice">>, <<"user">> => #{<<"id">> => <<"2">>}},
    ?assert(get_member_sort_key(MemberEmpty) < get_member_sort_key(MemberNamed)).

get_member_display_name_null_nick_test() ->
    Member = #{<<"nick">> => null, <<"user">> => #{<<"username">> => <<"bob">>}},
    ?assertEqual(<<"bob">>, get_member_display_name(Member)).

get_member_display_name_undefined_nick_test() ->
    Member = #{<<"nick">> => undefined, <<"user">> => #{<<"username">> => <<"charlie">>}},
    ?assertEqual(<<"charlie">>, get_member_display_name(Member)).

get_member_display_name_empty_nick_test() ->
    Member = #{<<"nick">> => <<>>, <<"user">> => #{<<"username">> => <<"dave">>}},
    ?assertEqual(<<"dave">>, get_member_display_name(Member)).

get_member_display_name_no_user_field_test() ->
    Member = #{},
    ?assertEqual(<<>>, get_member_display_name(Member)).

get_member_user_id_no_id_in_user_test() ->
    Member = #{<<"user">> => #{<<"username">> => <<"noone">>}},
    ?assertEqual(0, get_member_user_id(Member)).

hoisted_roles_same_position_sort_by_id_test() ->
    RoleA = #{<<"id">> => <<"200">>, <<"hoist">> => true, <<"position">> => 5},
    RoleB = #{<<"id">> => <<"300">>, <<"hoist">> => true, <<"position">> => 5},
    Sorted = get_hoisted_roles_sorted([RoleA, RoleB], 100),
    ?assertEqual(2, length(Sorted)).

hoisted_roles_excludes_everyone_role_test() ->
    GuildId = 100,
    EveryoneRole = #{<<"id">> => <<"100">>, <<"hoist">> => true, <<"position">> => 0},
    OtherRole = #{<<"id">> => <<"200">>, <<"hoist">> => true, <<"position">> => 1},
    Sorted = get_hoisted_roles_sorted([EveryoneRole, OtherRole], GuildId),
    ?assertEqual(1, length(Sorted)),
    ?assertEqual(<<"200">>, maps:get(<<"id">>, hd(Sorted))).

hoisted_roles_missing_hoist_defaults_false_test() ->
    GuildId = 100,
    RoleNoHoist = #{<<"id">> => <<"200">>, <<"position">> => 5},
    Sorted = get_hoisted_roles_sorted([RoleNoHoist], GuildId),
    ?assertEqual([], Sorted).

hoisted_roles_sorted_by_descending_position_test() ->
    GuildId = 100,
    RoleLow = #{<<"id">> => <<"200">>, <<"hoist">> => true, <<"position">> => 1},
    RoleHigh = #{<<"id">> => <<"300">>, <<"hoist">> => true, <<"position">> => 10},
    Sorted = get_hoisted_roles_sorted([RoleLow, RoleHigh], GuildId),
    [First, Second] = Sorted,
    ?assertEqual(<<"300">>, maps:get(<<"id">>, First)),
    ?assertEqual(<<"200">>, maps:get(<<"id">>, Second)).

hoisted_roles_uses_hoist_position_over_position_test() ->
    GuildId = 100,
    Role = #{<<"id">> => <<"200">>, <<"hoist">> => true, <<"position">> => 3, <<"hoist_position">> => 10},
    ?assertEqual(10, get_effective_hoist_position(Role)).

hoisted_roles_hoist_position_null_falls_back_test() ->
    Role = #{<<"id">> => <<"200">>, <<"position">> => 7, <<"hoist_position">> => null},
    ?assertEqual(7, get_effective_hoist_position(Role)).

hoisted_roles_hoist_position_undefined_falls_back_test() ->
    Role = #{<<"id">> => <<"200">>, <<"position">> => 7, <<"hoist_position">> => undefined},
    ?assertEqual(7, get_effective_hoist_position(Role)).

hoisted_roles_empty_list_test() ->
    ?assertEqual([], get_hoisted_roles_sorted([], 100)).

build_role_groups_empty_guild_test() ->
    ?assertEqual([], build_role_groups([], [])).

build_role_groups_all_members_same_hoisted_role_test() ->
    HoistedRole = #{<<"id">> => <<"200">>, <<"hoist">> => true, <<"position">> => 5},
    Member1 = #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"200">>]},
    Member2 = #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => [<<"200">>]},
    Groups = build_role_groups([HoistedRole], [Member1, Member2]),
    ?assertEqual(1, length(Groups)),
    ?assertEqual(2, maps:get(<<"count">>, hd(Groups))).

count_ungrouped_online_no_hoisted_roles_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => []},
        #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => []}
    ],
    ?assertEqual(2, count_ungrouped_online(Members, [])).

count_ungrouped_online_all_hoisted_test() ->
    HoistedRole = #{<<"id">> => <<"200">>, <<"hoist">> => true, <<"position">> => 5},
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"200">>]},
        #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => [<<"200">>]}
    ],
    ?assertEqual(0, count_ungrouped_online(Members, [HoistedRole])).

count_members_with_top_role_no_matching_members_test() ->
    HoistedRole = #{<<"id">> => <<"200">>, <<"hoist">> => true, <<"position">> => 5},
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"300">>]}
    ],
    ?assertEqual(0, count_members_with_top_role(200, Members, [HoistedRole])).

default_presence_returns_offline_test() ->
    P = default_presence(),
    ?assertEqual(<<"offline">>, maps:get(<<"status">>, P)),
    ?assertEqual(false, maps:get(<<"mobile">>, P)),
    ?assertEqual(false, maps:get(<<"afk">>, P)).

resolve_presence_missing_user_returns_default_test() ->
    State = #{member_presence => #{1 => #{<<"status">> => <<"online">>}}},
    P = resolve_presence_for_user(State, 999),
    ?assertEqual(<<"offline">>, maps:get(<<"status">>, P)).

resolve_presence_empty_presence_map_test() ->
    State = #{member_presence => #{}},
    P = resolve_presence_for_user(State, 1),
    ?assertEqual(<<"offline">>, maps:get(<<"status">>, P)).

resolve_presence_no_presence_key_test() ->
    State = #{},
    P = resolve_presence_for_user(State, 1),
    ?assertEqual(<<"offline">>, maps:get(<<"status">>, P)).

add_presence_to_member_test() ->
    Member = #{<<"user">> => #{<<"id">> => <<"42">>}},
    State = #{member_presence => #{42 => #{<<"status">> => <<"dnd">>}}},
    Result = add_presence_to_member(Member, State),
    ?assertEqual(#{<<"status">> => <<"dnd">>}, maps:get(<<"presence">>, Result)).

presence_status_changed_online_to_offline_test() ->
    OldState = #{member_presence => #{1 => #{<<"status">> => <<"online">>}}},
    NewState = #{member_presence => #{1 => #{<<"status">> => <<"offline">>}}},
    ?assertEqual(true, presence_status_changed(1, OldState, NewState)).

presence_status_changed_same_status_test() ->
    OldState = #{member_presence => #{1 => #{<<"status">> => <<"online">>}}},
    NewState = #{member_presence => #{1 => #{<<"status">> => <<"online">>}}},
    ?assertEqual(false, presence_status_changed(1, OldState, NewState)).

presence_status_changed_user_not_in_either_test() ->
    OldState = #{member_presence => #{}},
    NewState = #{member_presence => #{}},
    ?assertEqual(false, presence_status_changed(999, OldState, NewState)).

presence_status_changed_user_added_test() ->
    OldState = #{member_presence => #{}},
    NewState = #{member_presence => #{1 => #{<<"status">> => <<"online">>}}},
    ?assertEqual(true, presence_status_changed(1, OldState, NewState)).

partition_members_no_sessions_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>}}
    ],
    State = #{
        sessions => #{},
        member_presence => #{
            1 => #{<<"status">> => <<"online">>},
            2 => #{<<"status">> => <<"online">>}
        }
    },
    {Online, Offline} = partition_members_by_online(Members, State),
    ?assertEqual(0, length(Online)),
    ?assertEqual(2, length(Offline)).

partition_members_invisible_is_offline_test() ->
    Members = [#{<<"user">> => #{<<"id">> => <<"1">>}}],
    State = #{
        sessions => #{<<"s1">> => #{user_id => 1}},
        member_presence => #{1 => #{<<"status">> => <<"invisible">>}}
    },
    {Online, Offline} = partition_members_by_online(Members, State),
    ?assertEqual(0, length(Online)),
    ?assertEqual(1, length(Offline)).

partition_members_empty_list_test() ->
    State = #{sessions => #{}, member_presence => #{}},
    {Online, Offline} = partition_members_by_online([], State),
    ?assertEqual([], Online),
    ?assertEqual([], Offline).

connected_session_user_ids_ignores_invalid_test() ->
    State = #{sessions => #{
        <<"s1">> => #{user_id => 10},
        <<"s2">> => #{user_id => 0},
        <<"s3">> => #{user_id => -1},
        <<"s4">> => #{},
        <<"s5">> => #{user_id => undefined}
    }},
    Ids = connected_session_user_ids(State),
    ?assertEqual(true, sets:is_element(10, Ids)),
    ?assertEqual(false, sets:is_element(0, Ids)),
    ?assertEqual(false, sets:is_element(-1, Ids)),
    ?assertEqual(1, sets:size(Ids)).

slice_items_beyond_bounds_test() ->
    Items = [a, b, c],
    ?assertEqual([], slice_items(Items, 10, 20)).

slice_items_empty_list_test() ->
    ?assertEqual([], slice_items([], 0, 5)).

slice_items_end_beyond_length_test() ->
    Items = [a, b, c],
    ?assertEqual([a, b, c], slice_items(Items, 0, 100)).

slice_items_single_element_test() ->
    Items = [a, b, c],
    ?assertEqual([b], slice_items(Items, 1, 1)).

slice_items_zero_length_range_test() ->
    Items = [a, b, c],
    ?assertEqual([a], slice_items(Items, 0, 0)).

slice_items_full_range_test() ->
    Items = [a, b, c, d, e],
    ?assertEqual([a, b, c, d, e], slice_items(Items, 0, 4)).

diff_identical_lists_no_ops_test() ->
    Item1 = #{<<"group">> => #{<<"id">> => <<"online">>, <<"count">> => 1}},
    Item2 = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}}},
    Items = [Item1, Item2],
    ?assertEqual([], diff_items_to_ops(Items, Items)).

diff_empty_to_empty_no_ops_test() ->
    ?assertEqual([], diff_items_to_ops([], [])).

diff_empty_to_nonempty_produces_ops_test() ->
    NewItem = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}}},
    Ops = diff_items_to_ops([], [NewItem]),
    ?assert(length(Ops) >= 1),
    Op = hd(Ops),
    OpType = maps:get(<<"op">>, Op),
    ?assert(OpType =:= <<"SYNC">> orelse OpType =:= <<"INSERT">>).

diff_nonempty_to_empty_test() ->
    OldItem = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}}},
    Ops = diff_items_to_ops([OldItem], []),
    ?assertEqual(1, length(Ops)),
    Op = hd(Ops),
    ?assertEqual(<<"DELETE">>, maps:get(<<"op">>, Op)).

diff_single_insert_at_beginning_test() ->
    Existing = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"2">>}}},
    NewMember = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}}},
    OldItems = [Existing],
    NewItems = [NewMember, Existing],
    Ops = diff_items_to_ops(OldItems, NewItems),
    InsertOps = [Op || Op <- Ops, maps:get(<<"op">>, Op) =:= <<"INSERT">>],
    ?assert(length(InsertOps) >= 1).

diff_single_insert_at_end_test() ->
    Existing = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}}},
    NewMember = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"2">>}}},
    OldItems = [Existing],
    NewItems = [Existing, NewMember],
    Ops = diff_items_to_ops(OldItems, NewItems),
    InsertOps = [Op || Op <- Ops, maps:get(<<"op">>, Op) =:= <<"INSERT">>],
    ?assert(length(InsertOps) >= 1).

diff_single_delete_at_beginning_test() ->
    Member1 = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}}},
    Member2 = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"2">>}}},
    OldItems = [Member1, Member2],
    NewItems = [Member2],
    Ops = diff_items_to_ops(OldItems, NewItems),
    DeleteOps = [Op || Op <- Ops, maps:get(<<"op">>, Op) =:= <<"DELETE">>],
    ?assert(length(DeleteOps) >= 1).

diff_single_delete_at_end_test() ->
    Member1 = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}}},
    Member2 = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"2">>}}},
    OldItems = [Member1, Member2],
    NewItems = [Member1],
    Ops = diff_items_to_ops(OldItems, NewItems),
    DeleteOps = [Op || Op <- Ops, maps:get(<<"op">>, Op) =:= <<"DELETE">>],
    ?assert(length(DeleteOps) >= 1).

diff_update_existing_member_test() ->
    OldMember = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"old">>}},
    NewMember = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"new">>}},
    Ops = diff_items_to_ops([OldMember], [NewMember]),
    ?assertEqual(1, length(Ops)),
    ?assertEqual(<<"UPDATE">>, maps:get(<<"op">>, hd(Ops))),
    ?assertEqual(0, maps:get(<<"index">>, hd(Ops))).

diff_completely_different_lists_full_sync_test() ->
    OldItem = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}}},
    NewItem1 = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"2">>}}},
    NewItem2 = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"3">>}}},
    Ops = diff_items_to_ops([OldItem], [NewItem1, NewItem2]),
    HasSync = lists:any(fun(Op) -> maps:get(<<"op">>, Op) =:= <<"SYNC">> end, Ops),
    HasInsert = lists:any(fun(Op) -> maps:get(<<"op">>, Op) =:= <<"INSERT">> end, Ops),
    ?assert(HasSync orelse HasInsert).

full_sync_from_items_empty_test() ->
    ?assertEqual([], full_sync_from_items([])).

full_sync_from_items_single_test() ->
    Item = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}}},
    [Op] = full_sync_from_items([Item]),
    ?assertEqual(<<"SYNC">>, maps:get(<<"op">>, Op)),
    ?assertEqual([0, 0], maps:get(<<"range">>, Op)),
    ?assertEqual([Item], maps:get(<<"items">>, Op)).

full_sync_from_items_multiple_test() ->
    Items = [
        #{<<"group">> => #{<<"id">> => <<"online">>}},
        #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}}},
        #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"2">>}}}
    ],
    [Op] = full_sync_from_items(Items),
    ?assertEqual([0, 2], maps:get(<<"range">>, Op)).

item_key_group_test() ->
    Item = #{<<"group">> => #{<<"id">> => <<"online">>}},
    ?assertEqual({group, <<"online">>}, item_key(Item)).

item_key_member_test() ->
    Item = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"42">>}}},
    ?assertEqual({member, 42}, item_key(Item)).

item_key_unknown_test() ->
    Item = #{<<"something">> => <<"else">>},
    ?assertEqual(unknown, item_key(Item)).

item_key_member_no_user_field_test() ->
    Item = #{<<"member">> => #{<<"nick">> => <<"orphan">>}},
    ?assertEqual({member, 0}, item_key(Item)).

item_key_member_user_no_id_test() ->
    Item = #{<<"member">> => #{<<"user">> => #{<<"username">> => <<"noone">>}}},
    ?assertEqual({member, 0}, item_key(Item)).

find_member_entry_in_empty_list_test() ->
    ?assertEqual({error, not_found}, find_member_entry(1, [])).

find_member_entry_skips_groups_test() ->
    Items = [
        #{<<"group">> => #{<<"id">> => <<"online">>}},
        #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"42">>}}}
    ],
    {ok, Idx, _Item} = find_member_entry(42, Items),
    ?assertEqual(1, Idx).

find_member_entry_not_found_test() ->
    Items = [
        #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}}},
        #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"2">>}}}
    ],
    ?assertEqual({error, not_found}, find_member_entry(999, Items)).

adjusted_insert_index_new_after_old_test() ->
    ?assertEqual(4, adjusted_insert_index(2, 5)).

adjusted_insert_index_new_before_old_test() ->
    ?assertEqual(1, adjusted_insert_index(5, 1)).

adjusted_insert_index_same_position_test() ->
    ?assertEqual(3, adjusted_insert_index(3, 3)).

member_in_list_found_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>}}
    ],
    ?assertEqual(true, member_in_list(1, Members)).

member_in_list_not_found_test() ->
    Members = [#{<<"user">> => #{<<"id">> => <<"1">>}}],
    ?assertEqual(false, member_in_list(999, Members)).

member_in_list_empty_test() ->
    ?assertEqual(false, member_in_list(1, [])).

presence_move_ops_no_status_change_test() ->
    State = #{member_presence => #{1 => #{<<"status">> => <<"online">>}}},
    Items = [#{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}}}],
    {Moved, Ops} = presence_move_ops(1, State, State, Items, Items),
    ?assertEqual(false, Moved),
    ?assertEqual([], Ops).

presence_move_ops_member_not_found_test() ->
    OldState = #{member_presence => #{1 => #{<<"status">> => <<"online">>}}},
    NewState = #{member_presence => #{1 => #{<<"status">> => <<"offline">>}}},
    {Moved, Ops} = presence_move_ops(1, OldState, NewState, [], []),
    ?assertEqual(false, Moved),
    ?assertEqual([], Ops).

presence_move_ops_same_index_test() ->
    OldState = #{member_presence => #{1 => #{<<"status">> => <<"online">>}}},
    NewState = #{member_presence => #{1 => #{<<"status">> => <<"dnd">>}}},
    Item = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}}},
    {Moved, Ops} = presence_move_ops(1, OldState, NewState, [Item], [Item]),
    ?assertEqual(false, Moved),
    ?assertEqual([], Ops).

update_subscriptions_same_range_twice_no_sync_test() ->
    Subs0 = #{},
    {Subs1, _, ShouldSync1} = update_subscriptions(<<"s1">>, <<"500">>, [{0, 99}], Subs0),
    ?assertEqual(true, ShouldSync1),
    {_Subs2, _, ShouldSync2} = update_subscriptions(<<"s1">>, <<"500">>, [{0, 99}], Subs1),
    ?assertEqual(false, ShouldSync2).

update_subscriptions_changed_ranges_triggers_sync_test() ->
    Subs0 = #{},
    {Subs1, _, _} = update_subscriptions(<<"s1">>, <<"500">>, [{0, 99}], Subs0),
    {_Subs2, OldRanges, ShouldSync2} = update_subscriptions(<<"s1">>, <<"500">>, [{0, 199}], Subs1),
    ?assertEqual([{0, 99}], OldRanges),
    ?assertEqual(true, ShouldSync2).

update_subscriptions_multiple_sessions_overlapping_test() ->
    Subs0 = #{},
    {Subs1, _, _} = update_subscriptions(<<"s1">>, <<"500">>, [{0, 50}], Subs0),
    {Subs2, _, _} = update_subscriptions(<<"s2">>, <<"500">>, [{25, 75}], Subs1),
    ListSubs = maps:get(<<"500">>, Subs2),
    ?assertEqual([{0, 50}], maps:get(<<"s1">>, ListSubs)),
    ?assertEqual([{25, 75}], maps:get(<<"s2">>, ListSubs)).

remove_session_nonexistent_session_test() ->
    Subs = #{<<"500">> => #{<<"s1">> => [{0, 99}]}},
    NewSubs = remove_session_from_subscriptions(<<"nonexistent">>, Subs),
    ?assertEqual(Subs, NewSubs).

remove_session_empty_subscriptions_test() ->
    NewSubs = remove_session_from_subscriptions(<<"s1">>, #{}),
    ?assertEqual(#{}, NewSubs).

deep_merge_member_update_without_user_field_test() ->
    Current = #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}, <<"nick">> => <<"a">>},
    Update = #{<<"nick">> => <<"b">>},
    Result = deep_merge_member(Current, Update),
    User = maps:get(<<"user">>, Result),
    ?assertEqual(<<"alice">>, maps:get(<<"username">>, User)),
    ?assertEqual(<<"b">>, maps:get(<<"nick">>, Result)).

deep_merge_member_null_user_in_update_test() ->
    Current = #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}},
    Update = #{<<"user">> => null, <<"nick">> => <<"new">>},
    Result = deep_merge_member(Current, Update),
    ?assertEqual(null, maps:get(<<"user">>, Result)),
    ?assertEqual(<<"new">>, maps:get(<<"nick">>, Result)).

deep_merge_member_new_roles_test() ->
    Current = #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"100">>]},
    Update = #{<<"roles">> => [<<"200">>, <<"300">>]},
    Result = deep_merge_member(Current, Update),
    ?assertEqual([<<"200">>, <<"300">>], maps:get(<<"roles">>, Result)).

deep_merge_member_removing_all_roles_test() ->
    Current = #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"100">>, <<"200">>]},
    Update = #{<<"roles">> => []},
    Result = deep_merge_member(Current, Update),
    ?assertEqual([], maps:get(<<"roles">>, Result)).

deep_merge_member_non_map_user_in_update_test() ->
    Current = #{<<"user">> => #{<<"id">> => <<"1">>}},
    Update = #{<<"user">> => <<"invalid">>},
    Result = deep_merge_member(Current, Update),
    ?assertEqual(<<"invalid">>, maps:get(<<"user">>, Result)).

upsert_new_member_into_empty_state_test() ->
    Data = guild_data_index:normalize_data(#{
        <<"members">> => [],
        <<"roles">> => [],
        <<"channels">> => []
    }),
    State = #{data => Data},
    MemberUpdate = #{<<"user">> => #{<<"id">> => <<"42">>}, <<"nick">> => <<"new">>},
    {CurrentMember, UpdatedMember, NewState} = upsert_member_in_state(42, MemberUpdate, State),
    ?assertEqual(undefined, CurrentMember),
    ?assertEqual(<<"new">>, maps:get(<<"nick">>, UpdatedMember)),
    NewData = maps:get(data, NewState),
    NewMembers = guild_data_index:member_map(NewData),
    ?assertEqual(true, maps:is_key(42, NewMembers)).

upsert_existing_member_with_changed_roles_test() ->
    ExistingMember = #{<<"user">> => #{<<"id">> => <<"42">>}, <<"roles">> => [<<"100">>]},
    Data = guild_data_index:normalize_data(#{
        <<"members">> => [ExistingMember],
        <<"roles">> => [],
        <<"channels">> => []
    }),
    State = #{data => Data},
    MemberUpdate = #{<<"roles">> => [<<"200">>, <<"300">>]},
    {CurrentMember, UpdatedMember, _NewState} = upsert_member_in_state(42, MemberUpdate, State),
    ?assertEqual([<<"100">>], maps:get(<<"roles">>, CurrentMember)),
    ?assertEqual([<<"200">>, <<"300">>], maps:get(<<"roles">>, UpdatedMember)).

mismatch_span_identical_test() ->
    ?assertEqual(none, mismatch_span([a, b, c], [a, b, c])).

mismatch_span_completely_different_test() ->
    ?assertEqual({0, 2}, mismatch_span([a, b, c], [x, y, z])).

mismatch_span_single_mismatch_middle_test() ->
    ?assertEqual({1, 1}, mismatch_span([a, b, c], [a, x, c])).

mismatch_span_empty_lists_test() ->
    ?assertEqual(none, mismatch_span([], [])).

insert_many_at_beginning_test() ->
    ?assertEqual([x, y, a, b, c], insert_many([a, b, c], 0, [x, y])).

insert_many_at_end_test() ->
    ?assertEqual([a, b, c, x, y], insert_many([a, b, c], 3, [x, y])).

insert_many_into_empty_test() ->
    ?assertEqual([x, y], insert_many([], 0, [x, y])).

delete_many_at_beginning_test() ->
    ?assertEqual([c, d], delete_many([a, b, c, d], 0, 2)).

delete_many_at_end_test() ->
    ?assertEqual([a, b], delete_many([a, b, c, d], 2, 2)).

delete_many_from_middle_test() ->
    ?assertEqual([a, d], delete_many([a, b, c, d], 1, 2)).

insert_ops_produces_correct_indices_test() ->
    Items = [a, b, c],
    Ops = insert_ops(5, Items),
    ?assertEqual(3, length(Ops)),
    [Op1, Op2, Op3] = Ops,
    ?assertEqual(5, maps:get(<<"index">>, Op1)),
    ?assertEqual(6, maps:get(<<"index">>, Op2)),
    ?assertEqual(7, maps:get(<<"index">>, Op3)),
    ?assert(lists:all(fun(Op) -> maps:get(<<"op">>, Op) =:= <<"INSERT">> end, Ops)).

insert_ops_empty_test() ->
    ?assertEqual([], insert_ops(0, [])).

delete_ops_produces_same_index_test() ->
    Ops = delete_ops(3, 3),
    ?assertEqual(3, length(Ops)),
    ?assert(lists:all(fun(Op) -> maps:get(<<"index">>, Op) =:= 3 end, Ops)),
    ?assert(lists:all(fun(Op) -> maps:get(<<"op">>, Op) =:= <<"DELETE">> end, Ops)).

delete_ops_zero_count_test() ->
    ?assertEqual([], delete_ops(0, 0)).

first_mismatch_index_at_start_test() ->
    ?assertEqual(0, first_mismatch_index([x, b, c], [a, b, c])).

first_mismatch_index_at_end_test() ->
    ?assertEqual(2, first_mismatch_index([a, b, c], [a, b, x])).

first_mismatch_index_identical_test() ->
    ?assertEqual(3, first_mismatch_index([a, b, c], [a, b, c])).

first_mismatch_index_different_lengths_test() ->
    ?assertEqual(2, first_mismatch_index([a, b], [a, b, c])).

first_mismatch_index_empty_test() ->
    ?assertEqual(0, first_mismatch_index([], [])).

zip_with_index_basic_test() ->
    Result = zip_with_index([a, b], [x, y]),
    ?assertEqual([{0, a, x}, {1, b, y}], Result).

zip_with_index_different_lengths_test() ->
    Result = zip_with_index([a, b, c], [x]),
    ?assertEqual([{0, a, x}], Result).

zip_with_index_empty_test() ->
    ?assertEqual([], zip_with_index([], [])).

updates_for_changed_items_no_changes_test() ->
    Items = [#{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}}}],
    ?assertEqual([], updates_for_changed_items(Items, Items)).

updates_for_changed_items_single_change_test() ->
    OldItem = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"old">>}},
    NewItem = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"new">>}},
    Ops = updates_for_changed_items([OldItem], [NewItem]),
    ?assertEqual(1, length(Ops)),
    ?assertEqual(<<"UPDATE">>, maps:get(<<"op">>, hd(Ops))),
    ?assertEqual(0, maps:get(<<"index">>, hd(Ops))).

updates_for_changed_items_multiple_changes_test() ->
    Old1 = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"a">>}},
    Old2 = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"2">>}, <<"nick">> => <<"b">>}},
    New1 = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"x">>}},
    New2 = #{<<"member">> => #{<<"user">> => #{<<"id">> => <<"2">>}, <<"nick">> => <<"y">>}},
    Ops = updates_for_changed_items([Old1, Old2], [New1, New2]),
    ?assertEqual(2, length(Ops)),
    Indices = [maps:get(<<"index">>, Op) || Op <- Ops],
    ?assertEqual([0, 1], Indices).

list_id_channel_id_non_binary_test() ->
    ?assertEqual(0, list_id_channel_id(123)),
    ?assertEqual(0, list_id_channel_id(undefined)).

list_id_channel_id_zero_test() ->
    ?assertEqual(0, list_id_channel_id(<<"0">>)).

list_id_channel_id_empty_binary_test() ->
    ?assertEqual(0, list_id_channel_id(<<>>)).

session_can_view_channel_non_integer_channel_test() ->
    SessionData = #{user_id => 1, viewable_channels => #{}},
    State = #{data => #{<<"members">> => #{}}},
    ?assertEqual(false, session_can_view_channel(SessionData, not_an_integer, State)).

session_can_view_channel_zero_channel_test() ->
    SessionData = #{user_id => 1, viewable_channels => #{}},
    State = #{data => #{<<"members">> => #{}}},
    ?assertEqual(false, session_can_view_channel(SessionData, 0, State)).

session_can_view_channel_negative_channel_test() ->
    SessionData = #{user_id => 1, viewable_channels => #{}},
    State = #{data => #{<<"members">> => #{}}},
    ?assertEqual(false, session_can_view_channel(SessionData, -5, State)).

session_can_view_channel_no_user_id_test() ->
    SessionData = #{},
    State = #{data => #{<<"members">> => #{}}},
    ?assertEqual(false, session_can_view_channel(SessionData, 500, State)).

session_can_view_channel_no_viewable_channels_map_test() ->
    SessionData = #{user_id => 1},
    State = #{data => #{<<"members">> => #{}}},
    ?assertEqual(false, session_can_view_channel(SessionData, 500, State)).

casefold_binary_mixed_case_test() ->
    ?assertEqual(<<"hello">>, casefold_binary(<<"HeLLo">>)).

casefold_binary_already_lower_test() ->
    ?assertEqual(<<"world">>, casefold_binary(<<"world">>)).

casefold_binary_empty_test() ->
    ?assertEqual(<<>>, casefold_binary(<<>>)).

casefold_binary_integer_test() ->
    ?assertEqual(<<"42">>, casefold_binary(42)).

casefold_binary_undefined_test() ->
    ?assertEqual(<<>>, casefold_binary(undefined)).

normalize_name_binary_test() ->
    ?assertEqual(<<"hello">>, normalize_name(<<"hello">>)).

normalize_name_integer_test() ->
    ?assertEqual(<<"42">>, normalize_name(42)).

normalize_name_null_test() ->
    ?assertEqual(<<>>, normalize_name(null)).

normalize_name_undefined_test() ->
    ?assertEqual(<<>>, normalize_name(undefined)).

normalize_name_list_test() ->
    ?assertEqual(<<"hello">>, normalize_name("hello")).

normalize_name_other_test() ->
    ?assertEqual(<<>>, normalize_name(#{})).

-endif.
