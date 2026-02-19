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

-module(guild_data_index).

-export([
    normalize_data/1,
    member_map/1,
    member_values/1,
    member_list/1,
    member_count/1,
    member_ids/1,
    member_role_index/1,
    get_member/2,
    put_member/2,
    put_member_map/2,
    put_member_list/2,
    remove_member/2,
    role_list/1,
    role_index/1,
    put_roles/2,
    channel_list/1,
    channel_index/1,
    put_channels/2
]).

-type guild_data() :: map().
-type member() :: map().
-type role() :: map().
-type channel() :: map().
-type user_id() :: integer().
-type snowflake_id() :: integer().
-type role_member_index() :: #{snowflake_id() => #{user_id() => true}}.

-spec normalize_data(guild_data()) -> guild_data().
normalize_data(Data) when is_map(Data) ->
    MemberMap = member_map(Data),
    Roles = role_list(Data),
    Channels = channel_list(Data),
    Data1 = maps:put(<<"members">>, MemberMap, Data),
    Data2 = maps:put(<<"roles">>, Roles, Data1),
    Data3 = maps:put(<<"channels">>, Channels, Data2),
    Data4 = maps:put(<<"role_index">>, build_id_index(Roles), Data3),
    Data5 = maps:put(<<"channel_index">>, build_id_index(Channels), Data4),
    maps:put(<<"member_role_index">>, build_member_role_index(MemberMap), Data5);
normalize_data(Data) ->
    Data.

-spec member_map(guild_data()) -> #{user_id() => member()}.
member_map(Data) when is_map(Data) ->
    case maps:get(<<"members">>, Data, #{}) of
        Members when is_map(Members) ->
            normalize_member_map(Members);
        Members when is_list(Members) ->
            build_member_map(Members);
        _ ->
            #{}
    end;
member_map(_) ->
    #{}.

-spec member_list(guild_data()) -> [member()].
member_list(Data) ->
    MemberPairs = maps:to_list(member_map(Data)),
    SortedPairs = lists:sort(fun({A, _}, {B, _}) -> A =< B end, MemberPairs),
    [Member || {_UserId, Member} <- SortedPairs].

-spec member_values(guild_data()) -> [member()].
member_values(Data) ->
    maps:values(member_map(Data)).

-spec member_count(guild_data()) -> non_neg_integer().
member_count(Data) ->
    map_size(member_map(Data)).

-spec member_ids(guild_data()) -> [user_id()].
member_ids(Data) ->
    maps:keys(member_map(Data)).

-spec member_role_index(guild_data()) -> role_member_index().
member_role_index(Data) when is_map(Data) ->
    case maps:get(<<"member_role_index">>, Data, undefined) of
        Index when is_map(Index) -> normalize_member_role_index(Index);
        _ -> build_member_role_index(member_map(Data))
    end;
member_role_index(_) ->
    #{}.

-spec get_member(user_id(), guild_data()) -> member() | undefined.
get_member(UserId, Data) when is_integer(UserId) ->
    maps:get(UserId, member_map(Data), undefined);
get_member(_, _) ->
    undefined.

-spec put_member(member(), guild_data()) -> guild_data().
put_member(Member, Data) when is_map(Member), is_map(Data) ->
    case member_user_id(Member) of
        undefined ->
            Data;
        UserId ->
            MemberMap = member_map(Data),
            ExistingMember = maps:get(UserId, MemberMap, undefined),
            ExistingRoles = member_role_ids(ExistingMember),
            UpdatedRoles = member_role_ids(Member),
            RoleIndex = member_role_index(Data),
            RoleIndex1 = remove_user_from_member_role_index(UserId, ExistingRoles, RoleIndex),
            RoleIndex2 = add_user_to_member_role_index(UserId, UpdatedRoles, RoleIndex1),
            Data1 = maps:put(<<"members">>, maps:put(UserId, Member, MemberMap), Data),
            maps:put(<<"member_role_index">>, RoleIndex2, Data1)
    end;
put_member(_, Data) ->
    Data.

-spec put_member_map(#{user_id() => member()}, guild_data()) -> guild_data().
put_member_map(MemberMap, Data) when is_map(MemberMap), is_map(Data) ->
    NormalizedMemberMap = normalize_member_map(MemberMap),
    Data1 = maps:put(<<"members">>, NormalizedMemberMap, Data),
    maps:put(<<"member_role_index">>, build_member_role_index(NormalizedMemberMap), Data1);
put_member_map(_, Data) ->
    Data.

-spec put_member_list([member()], guild_data()) -> guild_data().
put_member_list(Members, Data) when is_list(Members), is_map(Data) ->
    put_member_map(build_member_map(Members), Data);
put_member_list(_, Data) ->
    Data.

-spec remove_member(user_id(), guild_data()) -> guild_data().
remove_member(UserId, Data) when is_integer(UserId), is_map(Data) ->
    MemberMap = member_map(Data),
    Member = maps:get(UserId, MemberMap, undefined),
    MemberRoles = member_role_ids(Member),
    RoleIndex = member_role_index(Data),
    RoleIndex1 = remove_user_from_member_role_index(UserId, MemberRoles, RoleIndex),
    Data1 = maps:put(<<"members">>, maps:remove(UserId, MemberMap), Data),
    maps:put(<<"member_role_index">>, RoleIndex1, Data1);
remove_member(_, Data) ->
    Data.

-spec role_list(guild_data()) -> [role()].
role_list(Data) when is_map(Data) ->
    ensure_list(maps:get(<<"roles">>, Data, []));
role_list(_) ->
    [].

-spec role_index(guild_data()) -> #{snowflake_id() => role()}.
role_index(Data) when is_map(Data) ->
    case maps:get(<<"role_index">>, Data, undefined) of
        Index when is_map(Index) -> normalize_id_index(Index);
        _ -> build_id_index(role_list(Data))
    end;
role_index(_) ->
    #{}.

-spec put_roles([role()], guild_data()) -> guild_data().
put_roles(Roles, Data) when is_map(Data) ->
    RoleList = ensure_list(Roles),
    Data1 = maps:put(<<"roles">>, RoleList, Data),
    maps:put(<<"role_index">>, build_id_index(RoleList), Data1);
put_roles(_, Data) ->
    Data.

-spec channel_list(guild_data()) -> [channel()].
channel_list(Data) when is_map(Data) ->
    ensure_list(maps:get(<<"channels">>, Data, []));
channel_list(_) ->
    [].

-spec channel_index(guild_data()) -> #{snowflake_id() => channel()}.
channel_index(Data) when is_map(Data) ->
    case maps:get(<<"channel_index">>, Data, undefined) of
        Index when is_map(Index) -> normalize_id_index(Index);
        _ -> build_id_index(channel_list(Data))
    end;
channel_index(_) ->
    #{}.

-spec put_channels([channel()], guild_data()) -> guild_data().
put_channels(Channels, Data) when is_map(Data) ->
    ChannelList = ensure_list(Channels),
    Data1 = maps:put(<<"channels">>, ChannelList, Data),
    maps:put(<<"channel_index">>, build_id_index(ChannelList), Data1);
put_channels(_, Data) ->
    Data.

-spec build_member_map([member()]) -> #{user_id() => member()}.
build_member_map(Members) ->
    lists:foldl(
        fun(Member, Acc) ->
            case member_user_id(Member) of
                undefined ->
                    Acc;
                UserId ->
                    maps:put(UserId, Member, Acc)
            end
        end,
        #{},
        Members
    ).

-spec normalize_member_map(map()) -> #{user_id() => member()}.
normalize_member_map(MemberMap) ->
    maps:fold(
        fun(Key, Member, Acc) ->
            case normalize_member_key(Key, Member) of
                undefined ->
                    Acc;
                UserId ->
                    maps:put(UserId, Member, Acc)
            end
        end,
        #{},
        MemberMap
    ).

-spec normalize_member_key(term(), member()) -> user_id() | undefined.
normalize_member_key(Key, Member) ->
    case type_conv:to_integer(Key) of
        undefined -> member_user_id(Member);
        UserId -> UserId
    end.

-spec member_user_id(member()) -> user_id() | undefined.
member_user_id(Member) when is_map(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    map_utils:get_integer(User, <<"id">>, undefined);
member_user_id(_) ->
    undefined.

-spec member_role_ids(term()) -> [snowflake_id()].
member_role_ids(Member) when is_map(Member) ->
    extract_integer_list(maps:get(<<"roles">>, Member, []));
member_role_ids(_) ->
    [].

-spec build_member_role_index(#{user_id() => member()}) -> role_member_index().
build_member_role_index(MemberMap) ->
    maps:fold(
        fun(UserId, Member, Acc) ->
            add_user_to_member_role_index(UserId, member_role_ids(Member), Acc)
        end,
        #{},
        MemberMap
    ).

-spec normalize_member_role_index(map()) -> role_member_index().
normalize_member_role_index(Index) ->
    maps:fold(
        fun(RoleKey, Members, Acc) ->
            case type_conv:to_integer(RoleKey) of
                undefined ->
                    Acc;
                RoleId ->
                    NormalizedMembers = normalize_member_role_members(Members),
                    case map_size(NormalizedMembers) of
                        0 ->
                            Acc;
                        _ ->
                            maps:put(RoleId, NormalizedMembers, Acc)
                    end
            end
        end,
        #{},
        Index
    ).

-spec normalize_member_role_members(term()) -> #{user_id() => true}.
normalize_member_role_members(Members) when is_map(Members) ->
    maps:fold(
        fun(UserKey, _Flag, Acc) ->
            case type_conv:to_integer(UserKey) of
                undefined ->
                    Acc;
                UserId ->
                    maps:put(UserId, true, Acc)
            end
        end,
        #{},
        Members
    );
normalize_member_role_members(_) ->
    #{}.

-spec add_user_to_member_role_index(user_id(), [snowflake_id()], role_member_index()) ->
    role_member_index().
add_user_to_member_role_index(UserId, RoleIds, RoleIndex) when is_integer(UserId) ->
    lists:foldl(
        fun(RoleId, Acc) ->
            RoleMembers = maps:get(RoleId, Acc, #{}),
            maps:put(RoleId, maps:put(UserId, true, RoleMembers), Acc)
        end,
        RoleIndex,
        RoleIds
    ).

-spec remove_user_from_member_role_index(user_id(), [snowflake_id()], role_member_index()) ->
    role_member_index().
remove_user_from_member_role_index(UserId, RoleIds, RoleIndex) when is_integer(UserId) ->
    lists:foldl(
        fun(RoleId, Acc) ->
            RoleMembers = maps:get(RoleId, Acc, #{}),
            UpdatedRoleMembers = maps:remove(UserId, RoleMembers),
            case map_size(UpdatedRoleMembers) of
                0 ->
                    maps:remove(RoleId, Acc);
                _ ->
                    maps:put(RoleId, UpdatedRoleMembers, Acc)
            end
        end,
        RoleIndex,
        RoleIds
    ).

-spec build_id_index([map()]) -> #{snowflake_id() => map()}.
build_id_index(Items) ->
    lists:foldl(
        fun(Item, Acc) ->
            case map_utils:get_integer(Item, <<"id">>, undefined) of
                undefined ->
                    Acc;
                Id ->
                    maps:put(Id, Item, Acc)
            end
        end,
        #{},
        Items
    ).

-spec normalize_id_index(map()) -> #{snowflake_id() => map()}.
normalize_id_index(Index) ->
    maps:fold(
        fun(Key, Item, Acc) ->
            case type_conv:to_integer(Key) of
                undefined ->
                    case map_utils:get_integer(Item, <<"id">>, undefined) of
                        undefined -> Acc;
                        Id -> maps:put(Id, Item, Acc)
                    end;
                Id ->
                    maps:put(Id, Item, Acc)
            end
        end,
        #{},
        Index
    ).

-spec ensure_list(term()) -> list().
ensure_list(List) when is_list(List) ->
    List;
ensure_list(_) ->
    [].

-spec extract_integer_list(term()) -> [integer()].
extract_integer_list(List) when is_list(List) ->
    lists:reverse(
        lists:foldl(
            fun(Value, Acc) ->
                case type_conv:to_integer(Value) of
                    undefined -> Acc;
                    Int -> [Int | Acc]
                end
            end,
            [],
            List
        )
    );
extract_integer_list(_) ->
    [].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

member_map_from_list_test() ->
    Data = #{
        <<"members">> => [
            #{<<"user">> => #{<<"id">> => <<"2">>}, <<"nick">> => <<"beta">>},
            #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"alpha">>}
        ]
    },
    MemberMap = member_map(Data),
    ?assertMatch(#{1 := _, 2 := _}, MemberMap),
    ?assertEqual(2, member_count(Data)).

member_list_is_sorted_test() ->
    Data = #{
        <<"members">> => #{
            5 => #{<<"user">> => #{<<"id">> => <<"5">>}},
            2 => #{<<"user">> => #{<<"id">> => <<"2">>}}
        }
    },
    [First, Second] = member_list(Data),
    ?assertEqual(2, member_user_id(First)),
    ?assertEqual(5, member_user_id(Second)).

member_values_returns_members_without_sorting_test() ->
    Data = #{
        <<"members">> => #{
            9 => #{<<"user">> => #{<<"id">> => <<"9">>}},
            2 => #{<<"user">> => #{<<"id">> => <<"2">>}}
        }
    },
    Values = member_values(Data),
    ?assertEqual(2, length(Values)).

put_member_updates_entry_test() ->
    Data = #{
        <<"members">> => #{
            10 => #{<<"user">> => #{<<"id">> => <<"10">>}, <<"nick">> => <<"old">>}
        }
    },
    UpdatedData = put_member(
        #{<<"user">> => #{<<"id">> => <<"10">>}, <<"nick">> => <<"new">>},
        Data
    ),
    UpdatedMember = get_member(10, UpdatedData),
    ?assertEqual(<<"new">>, maps:get(<<"nick">>, UpdatedMember)).

remove_member_removes_entry_test() ->
    Data = #{
        <<"members">> => #{
            10 => #{<<"user">> => #{<<"id">> => <<"10">>}}
        }
    },
    UpdatedData = remove_member(10, Data),
    ?assertEqual(undefined, get_member(10, UpdatedData)).

normalize_data_builds_indexes_test() ->
    Data = #{
        <<"members">> => [#{<<"user">> => #{<<"id">> => <<"1">>}}],
        <<"roles">> => [#{<<"id">> => <<"100">>}],
        <<"channels">> => [#{<<"id">> => <<"200">>}]
    },
    Normalized = normalize_data(Data),
    ?assert(is_map(maps:get(<<"members">>, Normalized))),
    ?assertMatch(#{100 := _}, role_index(Normalized)),
    ?assertMatch(#{200 := _}, channel_index(Normalized)).

member_role_index_builds_role_to_user_lookup_test() ->
    Data = #{
        <<"members">> => #{
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"10">>, <<"11">>]},
            2 => #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => [<<"11">>]}
        }
    },
    Index = member_role_index(Data),
    ?assertEqual(#{1 => true}, maps:get(10, Index)),
    ?assertEqual(#{1 => true, 2 => true}, maps:get(11, Index)).

put_member_and_remove_member_keep_member_role_index_in_sync_test() ->
    Data0 = #{
        <<"members">> => #{
            3 => #{<<"user">> => #{<<"id">> => <<"3">>}, <<"roles">> => [<<"20">>]}
        }
    },
    Data1 = put_member(
        #{<<"user">> => #{<<"id">> => <<"3">>}, <<"roles">> => [<<"30">>]},
        Data0
    ),
    Index1 = member_role_index(Data1),
    ?assertEqual(undefined, maps:get(20, Index1, undefined)),
    ?assertEqual(#{3 => true}, maps:get(30, Index1)),
    Data2 = remove_member(3, Data1),
    Index2 = member_role_index(Data2),
    ?assertEqual(undefined, maps:get(30, Index2, undefined)).

normalize_data_empty_lists_test() ->
    Data = #{
        <<"members">> => [],
        <<"roles">> => [],
        <<"channels">> => []
    },
    Normalized = normalize_data(Data),
    ?assertEqual(#{}, maps:get(<<"members">>, Normalized)),
    ?assertEqual([], maps:get(<<"roles">>, Normalized)),
    ?assertEqual([], maps:get(<<"channels">>, Normalized)),
    ?assertEqual(#{}, maps:get(<<"role_index">>, Normalized)),
    ?assertEqual(#{}, maps:get(<<"channel_index">>, Normalized)),
    ?assertEqual(#{}, maps:get(<<"member_role_index">>, Normalized)).

normalize_data_non_map_input_test() ->
    ?assertEqual(not_a_map, normalize_data(not_a_map)),
    ?assertEqual(42, normalize_data(42)).

normalize_data_missing_keys_defaults_test() ->
    Data = #{},
    Normalized = normalize_data(Data),
    ?assertEqual(#{}, maps:get(<<"members">>, Normalized)),
    ?assertEqual([], maps:get(<<"roles">>, Normalized)),
    ?assertEqual([], maps:get(<<"channels">>, Normalized)).

normalize_data_already_map_members_test() ->
    Data = #{
        <<"members">> => #{
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"a">>}
        },
        <<"roles">> => [],
        <<"channels">> => []
    },
    Normalized = normalize_data(Data),
    Members = maps:get(<<"members">>, Normalized),
    ?assertEqual(1, map_size(Members)),
    ?assertMatch(#{1 := _}, Members).

member_map_non_map_input_test() ->
    ?assertEqual(#{}, member_map(not_a_map)),
    ?assertEqual(#{}, member_map(42)).

member_map_invalid_members_value_test() ->
    Data = #{<<"members">> => <<"invalid">>},
    ?assertEqual(#{}, member_map(Data)).

member_map_members_without_user_test() ->
    Data = #{<<"members">> => [#{<<"nick">> => <<"orphan">>}]},
    ?assertEqual(#{}, member_map(Data)).

member_map_duplicate_user_ids_last_wins_test() ->
    Data = #{
        <<"members">> => [
            #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"first">>},
            #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"second">>}
        ]
    },
    MemberMap = member_map(Data),
    ?assertEqual(1, map_size(MemberMap)),
    ?assertEqual(<<"second">>, maps:get(<<"nick">>, maps:get(1, MemberMap))).

member_list_empty_test() ->
    Data = #{<<"members">> => #{}},
    ?assertEqual([], member_list(Data)).

member_ids_returns_all_user_ids_test() ->
    Data = #{
        <<"members">> => #{
            5 => #{<<"user">> => #{<<"id">> => <<"5">>}},
            3 => #{<<"user">> => #{<<"id">> => <<"3">>}},
            8 => #{<<"user">> => #{<<"id">> => <<"8">>}}
        }
    },
    Ids = lists:sort(member_ids(Data)),
    ?assertEqual([3, 5, 8], Ids).

get_member_non_integer_key_test() ->
    Data = #{<<"members">> => #{1 => #{<<"user">> => #{<<"id">> => <<"1">>}}}},
    ?assertEqual(undefined, get_member(not_an_integer, Data)).

get_member_missing_user_test() ->
    Data = #{<<"members">> => #{}},
    ?assertEqual(undefined, get_member(999, Data)).

put_member_no_user_id_returns_unchanged_test() ->
    Data = #{<<"members">> => #{}},
    ?assertEqual(Data, put_member(#{<<"nick">> => <<"orphan">>}, Data)).

put_member_non_map_member_returns_unchanged_test() ->
    Data = #{<<"members">> => #{}},
    ?assertEqual(Data, put_member(not_a_map, Data)).

put_member_non_map_data_returns_unchanged_test() ->
    ?assertEqual(not_a_map, put_member(#{<<"user">> => #{<<"id">> => <<"1">>}}, not_a_map)).

put_member_adds_new_member_test() ->
    Data = #{<<"members">> => #{}},
    UpdatedData = put_member(
        #{<<"user">> => #{<<"id">> => <<"42">>}, <<"nick">> => <<"new">>},
        Data
    ),
    ?assertMatch(#{42 := _}, maps:get(<<"members">>, UpdatedData)),
    ?assertEqual(<<"new">>, maps:get(<<"nick">>, get_member(42, UpdatedData))).

put_member_map_replaces_all_members_test() ->
    Data = #{
        <<"members">> => #{1 => #{<<"user">> => #{<<"id">> => <<"1">>}}}
    },
    NewMap = #{2 => #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => [<<"10">>]}},
    Updated = put_member_map(NewMap, Data),
    ?assertEqual(undefined, get_member(1, Updated)),
    ?assertMatch(#{2 := _}, maps:get(<<"members">>, Updated)).

put_member_map_non_map_returns_unchanged_test() ->
    Data = #{<<"members">> => #{}},
    ?assertEqual(Data, put_member_map(not_a_map, Data)).

put_member_list_converts_and_stores_test() ->
    Data = #{<<"members">> => #{}},
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>}}
    ],
    Updated = put_member_list(Members, Data),
    ?assertEqual(2, map_size(maps:get(<<"members">>, Updated))).

put_member_list_non_list_returns_unchanged_test() ->
    Data = #{<<"members">> => #{}},
    ?assertEqual(Data, put_member_list(not_a_list, Data)).

remove_member_non_integer_returns_unchanged_test() ->
    Data = #{<<"members">> => #{1 => #{<<"user">> => #{<<"id">> => <<"1">>}}}},
    ?assertEqual(Data, remove_member(not_an_int, Data)).

remove_member_non_existent_test() ->
    Data = #{<<"members">> => #{1 => #{<<"user">> => #{<<"id">> => <<"1">>}}}},
    Updated = remove_member(999, Data),
    ?assertEqual(1, map_size(maps:get(<<"members">>, Updated))).

role_list_non_map_input_test() ->
    ?assertEqual([], role_list(not_a_map)).

role_list_non_list_roles_value_test() ->
    Data = #{<<"roles">> => <<"invalid">>},
    ?assertEqual([], role_list(Data)).

role_index_non_map_input_test() ->
    ?assertEqual(#{}, role_index(not_a_map)).

role_index_from_list_test() ->
    Data = #{
        <<"roles">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"Admin">>},
            #{<<"id">> => <<"200">>, <<"name">> => <<"Member">>}
        ]
    },
    Index = role_index(Data),
    ?assertEqual(2, map_size(Index)),
    ?assertEqual(<<"Admin">>, maps:get(<<"name">>, maps:get(100, Index))).

put_roles_updates_list_and_index_test() ->
    Data = #{
        <<"roles">> => [#{<<"id">> => <<"1">>, <<"name">> => <<"old">>}]
    },
    NewRoles = [
        #{<<"id">> => <<"10">>, <<"name">> => <<"new1">>},
        #{<<"id">> => <<"20">>, <<"name">> => <<"new2">>}
    ],
    Updated = put_roles(NewRoles, Data),
    ?assertEqual(NewRoles, role_list(Updated)),
    ?assertEqual(2, map_size(role_index(Updated))).

put_roles_non_map_data_returns_unchanged_test() ->
    ?assertEqual(not_a_map, put_roles([], not_a_map)).

channel_list_non_map_input_test() ->
    ?assertEqual([], channel_list(not_a_map)).

channel_list_non_list_channels_value_test() ->
    Data = #{<<"channels">> => <<"invalid">>},
    ?assertEqual([], channel_list(Data)).

channel_index_non_map_input_test() ->
    ?assertEqual(#{}, channel_index(not_a_map)).

channel_index_from_list_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"300">>, <<"name">> => <<"general">>},
            #{<<"id">> => <<"301">>, <<"name">> => <<"random">>}
        ]
    },
    Index = channel_index(Data),
    ?assertEqual(2, map_size(Index)),
    ?assertEqual(<<"general">>, maps:get(<<"name">>, maps:get(300, Index))).

put_channels_updates_list_and_index_test() ->
    Data = #{<<"channels">> => []},
    NewChannels = [
        #{<<"id">> => <<"50">>, <<"name">> => <<"ch1">>},
        #{<<"id">> => <<"51">>, <<"name">> => <<"ch2">>}
    ],
    Updated = put_channels(NewChannels, Data),
    ?assertEqual(NewChannels, channel_list(Updated)),
    ?assertEqual(2, map_size(channel_index(Updated))).

put_channels_non_map_data_returns_unchanged_test() ->
    ?assertEqual(not_a_map, put_channels([], not_a_map)).

member_role_index_non_map_input_test() ->
    ?assertEqual(#{}, member_role_index(not_a_map)).

member_role_index_members_without_roles_test() ->
    Data = #{
        <<"members">> => #{
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}}
        }
    },
    Index = member_role_index(Data),
    ?assertEqual(#{}, Index).

member_role_index_shared_roles_test() ->
    Data = #{
        <<"members">> => #{
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"10">>]},
            2 => #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => [<<"10">>]},
            3 => #{<<"user">> => #{<<"id">> => <<"3">>}, <<"roles">> => [<<"10">>, <<"20">>]}
        }
    },
    Index = member_role_index(Data),
    ?assertEqual(#{1 => true, 2 => true, 3 => true}, maps:get(10, Index)),
    ?assertEqual(#{3 => true}, maps:get(20, Index)).

build_id_index_skips_items_without_id_test() ->
    Items = [
        #{<<"id">> => <<"1">>, <<"name">> => <<"first">>},
        #{<<"name">> => <<"no_id">>},
        #{<<"id">> => <<"2">>, <<"name">> => <<"second">>}
    ],
    Index = build_id_index(Items),
    ?assertEqual(2, map_size(Index)),
    ?assertEqual(<<"first">>, maps:get(<<"name">>, maps:get(1, Index))).

build_id_index_empty_list_test() ->
    ?assertEqual(#{}, build_id_index([])).

extract_integer_list_mixed_types_test() ->
    ?assertEqual([1, 2, 3], extract_integer_list([<<"1">>, 2, <<"3">>])),
    ?assertEqual([1, 3], extract_integer_list([<<"1">>, <<"invalid">>, <<"3">>])),
    ?assertEqual([], extract_integer_list(not_a_list)).

ensure_list_test() ->
    ?assertEqual([1, 2], ensure_list([1, 2])),
    ?assertEqual([], ensure_list(not_a_list)),
    ?assertEqual([], ensure_list(#{})).

normalize_member_map_with_binary_keys_test() ->
    MemberMap = #{
        <<"42">> => #{<<"user">> => #{<<"id">> => <<"42">>}, <<"nick">> => <<"test">>}
    },
    Normalized = normalize_member_map(MemberMap),
    ?assertMatch(#{42 := _}, Normalized),
    ?assertEqual(<<"test">>, maps:get(<<"nick">>, maps:get(42, Normalized))).

put_member_multiple_roles_index_test() ->
    Data = #{<<"members">> => #{}},
    Member = #{<<"user">> => #{<<"id">> => <<"7">>}, <<"roles">> => [<<"10">>, <<"20">>, <<"30">>]},
    Updated = put_member(Member, Data),
    Index = member_role_index(Updated),
    ?assertEqual(#{7 => true}, maps:get(10, Index)),
    ?assertEqual(#{7 => true}, maps:get(20, Index)),
    ?assertEqual(#{7 => true}, maps:get(30, Index)).

remove_member_cleans_empty_role_entries_test() ->
    Data0 = #{<<"members">> => #{}},
    Data1 = put_member(
        #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => [<<"10">>]},
        Data0
    ),
    Data2 = put_member(
        #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => [<<"10">>, <<"20">>]},
        Data1
    ),
    Data3 = remove_member(1, Data2),
    Index = member_role_index(Data3),
    ?assertEqual(#{2 => true}, maps:get(10, Index)),
    ?assertEqual(#{2 => true}, maps:get(20, Index)),
    Data4 = remove_member(2, Data3),
    Index2 = member_role_index(Data4),
    ?assertEqual(undefined, maps:get(10, Index2, undefined)),
    ?assertEqual(undefined, maps:get(20, Index2, undefined)).

-endif.
