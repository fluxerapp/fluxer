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

-module(guild_member_storage).

-export([
    new/0,
    insert_member/2,
    remove_member/2,
    get_member/2,
    get_members_by_ids/2,
    search_members/3,
    get_range/3,
    count/1,
    compute_list_id/1
]).

-record(member_storage, {
    members_table :: ets:tid(),
    display_name_index :: gb_trees:tree()
}).

-type storage() :: #member_storage{}.
-type user_id() :: integer().
-type member() :: map().

-export_type([storage/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec new() -> storage().
new() ->
    MembersTable = ets:new(members, [set, private]),
    DisplayNameIndex = gb_trees:empty(),
    #member_storage{
        members_table = MembersTable,
        display_name_index = DisplayNameIndex
    }.

-spec insert_member(member(), storage()) -> storage().
insert_member(Member, Storage) ->
    UserId = extract_user_id(Member),
    case UserId of
        undefined ->
            Storage;
        _ ->
            OldMember = get_member(UserId, Storage),
            Storage1 = remove_from_index(OldMember, Storage),
            ets:insert(Storage1#member_storage.members_table, {UserId, Member}),
            add_to_index(UserId, Member, Storage1)
    end.

-spec remove_member(user_id(), storage()) -> storage().
remove_member(UserId, Storage) ->
    case get_member(UserId, Storage) of
        undefined ->
            Storage;
        Member ->
            Storage1 = remove_from_index(Member, Storage),
            ets:delete(Storage1#member_storage.members_table, UserId),
            Storage1
    end.

-spec get_member(user_id(), storage()) -> member() | undefined.
get_member(UserId, Storage) ->
    case ets:lookup(Storage#member_storage.members_table, UserId) of
        [{UserId, Member}] -> Member;
        [] -> undefined
    end.

-spec get_members_by_ids([user_id()], storage()) -> [member()].
get_members_by_ids(UserIds, Storage) ->
    lists:filtermap(
        fun(UserId) ->
            case get_member(UserId, Storage) of
                undefined -> false;
                Member -> {true, Member}
            end
        end,
        UserIds
    ).

-spec search_members(binary(), non_neg_integer(), storage()) -> [member()].
search_members(Query, Limit, Storage) when is_binary(Query), Limit > 0 ->
    NormalizedQuery = normalize_display_name(Query),
    case NormalizedQuery of
        <<>> ->
            [];
        _ ->
            search_by_prefix(NormalizedQuery, Limit, Storage)
    end;
search_members(_, _, _) ->
    [].

-spec get_range(non_neg_integer(), non_neg_integer(), storage()) -> [member()].
get_range(Offset, Limit, Storage) when is_integer(Offset), is_integer(Limit), Limit > 0 ->
    Index = Storage#member_storage.display_name_index,
    case gb_trees:size(Index) of
        Size when Offset >= Size ->
            [];
        Size ->
            AllKeys = gb_trees:keys(Index),
            EndIdx = min(Offset + Limit, Size),
            SelectedKeys = lists:sublist(AllKeys, Offset + 1, EndIdx - Offset),
            lists:filtermap(
                fun(Key) ->
                    UserId = gb_trees:get(Key, Index),
                    case get_member(UserId, Storage) of
                        undefined -> false;
                        Member -> {true, Member}
                    end
                end,
                SelectedKeys
            )
    end;
get_range(_, _, _) ->
    [].

-spec count(storage()) -> non_neg_integer().
count(Storage) ->
    ets:info(Storage#member_storage.members_table, size).

-spec compute_list_id([user_id()]) -> integer().
compute_list_id(UserIds) ->
    SortedIds = lists:sort(UserIds),
    Combined = lists:foldl(
        fun(Id, Acc) -> <<Acc/binary, (integer_to_binary(Id))/binary, ",">> end,
        <<>>,
        SortedIds
    ),
    erlang:phash2(Combined, 16#FFFFFFFF).

-spec extract_user_id(member()) -> user_id() | undefined.
extract_user_id(Member) when is_map(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    map_utils:get_integer(User, <<"id">>, undefined);
extract_user_id(_) ->
    undefined.

-spec get_display_name(member()) -> binary().
get_display_name(Member) when is_map(Member) ->
    Nick = maps:get(<<"nick">>, Member, undefined),
    case Nick of
        undefined ->
            User = maps:get(<<"user">>, Member, #{}),
            GlobalName = maps:get(<<"global_name">>, User, undefined),
            case GlobalName of
                undefined ->
                    maps:get(<<"username">>, User, <<>>);
                _ ->
                    GlobalName
            end;
        _ ->
            Nick
    end.

-spec normalize_display_name(binary()) -> binary().
normalize_display_name(Name) when is_binary(Name) ->
    LowerName = string:lowercase(binary_to_list(Name)),
    list_to_binary(LowerName).

-spec add_to_index(user_id(), member(), storage()) -> storage().
add_to_index(UserId, Member, Storage) ->
    DisplayName = get_display_name(Member),
    NormalizedName = normalize_display_name(DisplayName),
    Key = make_index_key(NormalizedName, UserId),
    Index = Storage#member_storage.display_name_index,
    NewIndex = gb_trees:enter(Key, UserId, Index),
    Storage#member_storage{display_name_index = NewIndex}.

-spec remove_from_index(member() | undefined, storage()) -> storage().
remove_from_index(undefined, Storage) ->
    Storage;
remove_from_index(Member, Storage) ->
    UserId = extract_user_id(Member),
    DisplayName = get_display_name(Member),
    NormalizedName = normalize_display_name(DisplayName),
    Key = make_index_key(NormalizedName, UserId),
    Index = Storage#member_storage.display_name_index,
    case gb_trees:is_defined(Key, Index) of
        true ->
            NewIndex = gb_trees:delete(Key, Index),
            Storage#member_storage{display_name_index = NewIndex};
        false ->
            Storage
    end.

-spec make_index_key(binary(), user_id()) -> {binary(), user_id()}.
make_index_key(NormalizedName, UserId) ->
    {NormalizedName, UserId}.

-spec search_by_prefix(binary(), non_neg_integer(), storage()) -> [member()].
search_by_prefix(Prefix, Limit, Storage) ->
    Index = Storage#member_storage.display_name_index,
    AllKeys = gb_trees:keys(Index),
    Matches = lists:filtermap(
        fun({Name, UserId}) ->
            PrefixLen = byte_size(Prefix),
            case Name of
                <<Prefix:PrefixLen/binary, _/binary>> ->
                    case get_member(UserId, Storage) of
                        undefined -> false;
                        Member -> {true, Member}
                    end;
                _ ->
                    false
            end
        end,
        AllKeys
    ),
    lists:sublist(Matches, Limit).

-ifdef(TEST).

new_creates_empty_storage_test() ->
    Storage = new(),
    ?assertEqual(0, count(Storage)).

insert_and_get_member_test() ->
    Storage = new(),
    Member = #{
        <<"user">> => #{
            <<"id">> => <<"123">>,
            <<"username">> => <<"testuser">>
        },
        <<"roles">> => []
    },
    Storage1 = insert_member(Member, Storage),
    ?assertEqual(1, count(Storage1)),
    Retrieved = get_member(123, Storage1),
    ?assertEqual(Member, Retrieved).

remove_member_test() ->
    Storage = new(),
    Member = #{
        <<"user">> => #{
            <<"id">> => <<"123">>,
            <<"username">> => <<"testuser">>
        }
    },
    Storage1 = insert_member(Member, Storage),
    Storage2 = remove_member(123, Storage1),
    ?assertEqual(0, count(Storage2)),
    ?assertEqual(undefined, get_member(123, Storage2)).

get_members_by_ids_test() ->
    Storage = new(),
    Member1 = #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}},
    Member2 = #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"bob">>}},
    Storage1 = insert_member(Member1, Storage),
    Storage2 = insert_member(Member2, Storage1),
    Members = get_members_by_ids([1, 2, 999], Storage2),
    ?assertEqual(2, length(Members)).

search_members_by_prefix_test() ->
    Storage = new(),
    Member1 = #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}},
    Member2 = #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"bob">>}},
    Member3 = #{<<"user">> => #{<<"id">> => <<"3">>, <<"username">> => <<"alicia">>}},
    Storage1 = insert_member(Member1, Storage),
    Storage2 = insert_member(Member2, Storage1),
    Storage3 = insert_member(Member3, Storage2),
    Results = search_members(<<"ali">>, 10, Storage3),
    ?assertEqual(2, length(Results)).

display_name_nick_priority_test() ->
    Member = #{
        <<"user">> => #{
            <<"id">> => <<"1">>,
            <<"username">> => <<"user">>,
            <<"global_name">> => <<"Global">>
        },
        <<"nick">> => <<"Nickname">>
    },
    ?assertEqual(<<"Nickname">>, get_display_name(Member)).

display_name_global_name_fallback_test() ->
    Member = #{
        <<"user">> => #{
            <<"id">> => <<"1">>,
            <<"username">> => <<"user">>,
            <<"global_name">> => <<"Global">>
        }
    },
    ?assertEqual(<<"Global">>, get_display_name(Member)).

display_name_username_fallback_test() ->
    Member = #{
        <<"user">> => #{
            <<"id">> => <<"1">>,
            <<"username">> => <<"user">>
        }
    },
    ?assertEqual(<<"user">>, get_display_name(Member)).

compute_list_id_test() ->
    Id1 = compute_list_id([1, 2, 3]),
    Id2 = compute_list_id([3, 2, 1]),
    ?assertEqual(Id1, Id2).

get_range_test() ->
    Storage = new(),
    Member1 = #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}},
    Member2 = #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"bob">>}},
    Member3 = #{<<"user">> => #{<<"id">> => <<"3">>, <<"username">> => <<"charlie">>}},
    Storage1 = insert_member(Member1, Storage),
    Storage2 = insert_member(Member2, Storage1),
    Storage3 = insert_member(Member3, Storage2),
    Results = get_range(1, 2, Storage3),
    ?assertEqual(2, length(Results)).

-endif.
