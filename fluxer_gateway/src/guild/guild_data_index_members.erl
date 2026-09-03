%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_index_members).
-typing([eqwalizer]).

-export([
    member_map/1,
    member_values/1,
    member_list/1,
    member_count/1,
    member_ids/1,
    sorted_member_ids/2,
    member_role_index/1,
    get_member/2,
    get_member_ets/2,
    put_member/2,
    put_member_map/2,
    put_member_list/2,
    remove_member/2,
    build_member_map/1,
    normalize_member_map/1,
    member_user_id/1,
    member_role_ids/1,
    build_member_role_index/1,
    add_user_to_member_role_index/3,
    remove_user_from_member_role_index/3
]).

-type guild_data() :: map().
-type member() :: map().
-type user_id() :: integer().
-type snowflake_id() :: integer().
-type role_member_index() :: #{snowflake_id() => #{user_id() => true}}.

-export_type([guild_data/0, member/0, user_id/0, snowflake_id/0, role_member_index/0]).

-spec member_map(term()) -> #{user_id() => member()}.
member_map(Data) when is_map(Data) ->
    case maps:find(members_normalized, Data) of
        {ok, NMap} when is_map(NMap) ->
            maybe_normalize_member_map(NMap);
        _ ->
            member_map_from_members(maps:get(<<"members">>, Data, #{}))
    end;
member_map(_) ->
    #{}.

-spec member_map_from_members(term()) -> #{user_id() => member()}.
member_map_from_members(Members) when is_map(Members) ->
    maybe_normalize_member_map(Members);
member_map_from_members(Members) when is_list(Members) ->
    build_member_map(Members);
member_map_from_members(_) ->
    #{}.

-spec get_member_ets(term(), term()) -> member() | undefined.
get_member_ets(UserId, Data) when is_integer(UserId), is_map(Data) ->
    case members_ets_table(Data) of
        Tab when is_reference(Tab) ->
            lookup_member_ets(UserId, Tab);
        undefined ->
            get_member(UserId, Data)
    end;
get_member_ets(_, _) ->
    undefined.

-spec lookup_member_ets(user_id(), ets:tid()) -> member() | undefined.
lookup_member_ets(UserId, Tab) ->
    case ets:lookup(Tab, UserId) of
        [{_, Member}] -> Member;
        [] -> undefined
    end.

-spec maybe_normalize_member_map(map()) -> #{user_id() => member()}.
maybe_normalize_member_map(Members) ->
    case maps:next(maps:iterator(Members)) of
        none -> Members;
        {K, _, _} when is_integer(K) -> Members;
        _ -> normalize_member_map(Members)
    end.

-spec member_list(term()) -> [member()].
member_list(Data) ->
    MemberMap = member_map(Data),
    SortedIds = sorted_member_ids(Data, MemberMap),
    [Member || UserId <- SortedIds, {ok, Member} <- [maps:find(UserId, MemberMap)]].

%% Every member mutation drops members_sorted_ids, so a cache that is present always
%% equals lists:sort(maps:keys(member_map(Data))) and callers may page it as-is.
-spec sorted_member_ids(term(), #{user_id() => member()}) -> [user_id()].
sorted_member_ids(Data, MemberMap) when is_map(Data) ->
    case maps:find(members_sorted_ids, Data) of
        {ok, Ids} when is_list(Ids) -> eqwalizer:dynamic_cast(Ids);
        _ -> lists:sort(maps:keys(MemberMap))
    end;
sorted_member_ids(_Data, MemberMap) ->
    lists:sort(maps:keys(MemberMap)).

-spec invalidate_sorted_member_ids(guild_data()) -> guild_data().
invalidate_sorted_member_ids(Data) ->
    maps:remove(members_sorted_ids, Data).

-spec member_values(term()) -> [member()].
member_values(Data) ->
    maps:values(member_map(Data)).

-spec member_count(term()) -> non_neg_integer().
member_count(Data) ->
    map_size(member_map(Data)).

-spec member_ids(term()) -> [user_id()].
member_ids(Data) ->
    maps:keys(member_map(Data)).

-spec member_role_index(term()) -> role_member_index().
member_role_index(Data) when is_map(Data) ->
    case maps:find(<<"member_role_index">>, Data) of
        {ok, Index} when is_map(Index) -> existing_member_role_index(Index);
        _ -> build_member_role_index(member_map(Data))
    end;
member_role_index(_) ->
    #{}.

-spec get_member(term(), term()) -> member() | undefined.
get_member(UserId, Data) when is_integer(UserId) ->
    maps:get(UserId, member_map(Data), undefined);
get_member(_, _) ->
    undefined.

-spec put_member(term(), guild_data()) -> guild_data().
put_member(Member, Data) when is_map(Member), is_map(Data) ->
    NormalizedMember = normalize_member(Member),
    case member_user_id(NormalizedMember) of
        undefined ->
            Data;
        UserId ->
            MemberMap = member_map(Data),
            ExistingMember = maps:get(UserId, MemberMap, undefined),
            ExistingRoles = member_role_ids(ExistingMember),
            UpdatedRoles = member_role_ids(NormalizedMember),
            RoleIndex = member_role_index(Data),
            RoleIndex1 = remove_user_from_member_role_index(UserId, ExistingRoles, RoleIndex),
            RoleIndex2 = add_user_to_member_role_index(UserId, UpdatedRoles, RoleIndex1),
            NewMemberMap = MemberMap#{UserId => NormalizedMember},
            maybe_warn_large_member_map(NewMemberMap),
            sync_member_ets(Data, UserId, NormalizedMember),
            Data1 = Data#{
                <<"members">> => NewMemberMap,
                members_normalized => NewMemberMap,
                <<"member_role_index">> => RoleIndex2
            },
            invalidate_sorted_member_ids(Data1)
    end;
put_member(_, Data) ->
    Data.

-spec put_member_map(term(), guild_data()) -> guild_data().
put_member_map(MemberMap, Data) when is_map(MemberMap), is_map(Data) ->
    NormalizedMemberMap = normalize_member_map(MemberMap),
    Data#{
        <<"members">> => NormalizedMemberMap,
        members_normalized => NormalizedMemberMap,
        members_sorted_ids => lists:sort(maps:keys(NormalizedMemberMap)),
        <<"member_role_index">> => build_member_role_index(NormalizedMemberMap)
    };
put_member_map(_, Data) ->
    Data.

-spec put_member_list(term(), guild_data()) -> guild_data().
put_member_list(Members, Data) when is_list(Members), is_map(Data) ->
    put_member_map(build_member_map(Members), Data);
put_member_list(_, Data) ->
    Data.

-spec remove_member(term(), guild_data()) -> guild_data().
remove_member(UserId, Data) when is_integer(UserId), is_map(Data) ->
    MemberMap = member_map(Data),
    Member = maps:get(UserId, MemberMap, undefined),
    MemberRoles = member_role_ids(Member),
    RoleIndex = member_role_index(Data),
    RoleIndex1 = remove_user_from_member_role_index(UserId, MemberRoles, RoleIndex),
    RemovedMap = maps:remove(UserId, MemberMap),
    delete_member_ets(Data, UserId),
    Data1 = Data#{
        <<"members">> => RemovedMap,
        members_normalized => RemovedMap,
        <<"member_role_index">> => RoleIndex1
    },
    invalidate_sorted_member_ids(Data1);
remove_member(_, Data) ->
    Data.

-spec build_member_map([term()]) -> #{user_id() => member()}.
build_member_map(Members) ->
    lists:foldl(
        fun
            (Member, Acc) when is_map(Member) ->
                add_member_to_map(Member, Acc);
            (_, Acc) ->
                Acc
        end,
        #{},
        Members
    ).

-spec add_member_to_map(member(), #{user_id() => member()}) -> #{user_id() => member()}.
add_member_to_map(Member, Acc) ->
    case member_user_id(Member) of
        undefined -> Acc;
        UserId -> Acc#{UserId => normalize_member(Member)}
    end.

-spec normalize_member_map(map()) -> #{user_id() => member()}.
normalize_member_map(MemberMap) ->
    maps:fold(
        fun
            (Key, Member, Acc) when is_map(Member) ->
                add_normalized_member(Key, Member, Acc);
            (_, _, Acc) ->
                Acc
        end,
        #{},
        MemberMap
    ).

-spec add_normalized_member(term(), member(), #{user_id() => member()}) ->
    #{user_id() => member()}.
add_normalized_member(Key, Member, Acc) ->
    case normalize_member_safe(Member) of
        undefined -> Acc;
        NormalizedMember -> put_normalized_member(Key, NormalizedMember, Acc)
    end.

-spec normalize_member_safe(member()) -> member() | undefined.
normalize_member_safe(Member) ->
    try normalize_member(Member) of
        NormalizedMember -> NormalizedMember
    catch
        error:{invalid_snowflake, _} -> undefined
    end.

-spec put_normalized_member(term(), member(), #{user_id() => member()}) ->
    #{user_id() => member()}.
put_normalized_member(Key, NormalizedMember, Acc) ->
    case normalize_member_key(Key, NormalizedMember) of
        undefined -> Acc;
        UserId -> Acc#{UserId => NormalizedMember}
    end.

-spec normalize_member_key(term(), term()) -> user_id() | undefined.
normalize_member_key(Key, Member) ->
    KeyId = snowflake_id:parse_maybe(Key),
    MemberId = member_user_id(Member),
    case {KeyId, MemberId} of
        {undefined, Id} -> Id;
        {Id, Id} when is_integer(Id) -> Id;
        _ -> undefined
    end.

-spec member_user_id(term()) -> user_id() | undefined.
member_user_id(Member) when is_map(Member) ->
    User = member_user(Member),
    snowflake_id:parse_maybe(maps:get(<<"id">>, User, undefined));
member_user_id(_) ->
    undefined.

-spec member_user(member()) -> map().
member_user(Member) ->
    case maps:get(<<"user">>, Member, #{}) of
        User when is_map(User) -> User;
        _ -> error(badarg)
    end.

-spec member_role_ids(term()) -> [snowflake_id()].
member_role_ids(Member) when is_map(Member) ->
    guild_data_index:extract_integer_list(maps:get(<<"roles">>, Member, []));
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

-spec add_user_to_member_role_index(user_id(), [snowflake_id()], role_member_index()) ->
    role_member_index().
add_user_to_member_role_index(UserId, RoleIds, RoleIndex) when is_integer(UserId) ->
    lists:foldl(
        fun(RoleId, Acc) ->
            add_role_member(UserId, RoleId, Acc)
        end,
        RoleIndex,
        RoleIds
    ).

-spec normalize_member(member()) -> member().
normalize_member(Member) ->
    case guild_data_normalize:member(Member) of
        Normalized when is_map(Normalized) -> Normalized;
        _ -> Member
    end.

-spec existing_member_role_index(map()) -> role_member_index().
existing_member_role_index(Index) ->
    Index.

-spec add_role_member(user_id(), term(), role_member_index()) -> role_member_index().
add_role_member(UserId, RoleId, RoleIndex) when is_integer(RoleId), RoleId > 0 ->
    RoleMembers = maps:get(RoleId, RoleIndex, #{}),
    RoleIndex#{RoleId => RoleMembers#{UserId => true}};
add_role_member(_UserId, _RoleId, RoleIndex) ->
    RoleIndex.

-spec remove_user_from_member_role_index(user_id(), [snowflake_id()], role_member_index()) ->
    role_member_index().
remove_user_from_member_role_index(UserId, RoleIds, RoleIndex) when is_integer(UserId) ->
    lists:foldl(
        fun(RoleId, Acc) ->
            remove_role_member(UserId, RoleId, Acc)
        end,
        RoleIndex,
        RoleIds
    ).

-spec remove_role_member(user_id(), term(), role_member_index()) -> role_member_index().
remove_role_member(UserId, RoleId, RoleIndex) when is_integer(RoleId), RoleId > 0 ->
    RoleMembers = maps:get(RoleId, RoleIndex, #{}),
    UpdatedRoleMembers = maps:remove(UserId, RoleMembers),
    case map_size(UpdatedRoleMembers) of
        0 -> maps:remove(RoleId, RoleIndex);
        _ -> RoleIndex#{RoleId => UpdatedRoleMembers}
    end;
remove_role_member(_UserId, _RoleId, RoleIndex) ->
    RoleIndex.

-define(LARGE_MEMBER_THRESHOLD, 100000).

-spec maybe_warn_large_member_map(#{user_id() => member()}) -> ok.
maybe_warn_large_member_map(MemberMap) ->
    Size = map_size(MemberMap),
    case Size =:= ?LARGE_MEMBER_THRESHOLD of
        true ->
            logger:warning(
                "guild_large_member_map: member_count=~p threshold=~p",
                [Size, ?LARGE_MEMBER_THRESHOLD]
            );
        false ->
            ok
    end.

-spec sync_member_ets(guild_data(), user_id(), member()) -> true | ok.
sync_member_ets(Data, UserId, Member) ->
    case members_ets_table(Data) of
        Tab when is_reference(Tab) -> ets:insert(Tab, {UserId, Member});
        undefined -> ok
    end.

-spec delete_member_ets(guild_data(), user_id()) -> true | ok.
delete_member_ets(Data, UserId) ->
    case members_ets_table(Data) of
        Tab when is_reference(Tab) -> ets:delete(Tab, UserId);
        undefined -> ok
    end.

-spec members_ets_table(guild_data()) -> ets:tid() | undefined.
members_ets_table(#{members_ets := Tab}) ->
    Tab;
members_ets_table(_) ->
    undefined.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sorted_member_ids_equals_sorted_keys_test() ->
    lists:foreach(fun assert_sorted_ids_match_keys/1, sorted_ids_fixtures()).

assert_sorted_ids_match_keys(Data) ->
    MemberMap = member_map(Data),
    ?assertEqual(lists:sort(maps:keys(MemberMap)), sorted_member_ids(Data, MemberMap)).

sorted_member_ids_cache_dropped_by_mutations_test() ->
    Base = put_member_map(fixture_member_map([12, 3, 7]), #{}),
    ?assertEqual([3, 7, 12], maps:get(members_sorted_ids, Base)),
    ?assertNot(maps:is_key(members_sorted_ids, put_member(fixture_member(5), Base))),
    ?assertNot(maps:is_key(members_sorted_ids, remove_member(3, Base))).

sorted_ids_fixtures() ->
    Base = put_member_map(fixture_member_map([12, 3, 7]), #{}),
    Large = put_member_map(fixture_member_map(fixture_user_ids(40)), #{}),
    [
        #{},
        #{<<"members">> => #{}},
        #{<<"members">> => fixture_member_list([9, 2, 5])},
        #{<<"members">> => #{<<"9">> => fixture_member(9), <<"2">> => fixture_member(2)}},
        #{<<"members">> => fixture_member_list(fixture_user_ids(40))},
        Base,
        Large,
        put_member(fixture_member(5), Base),
        put_member(fixture_member(500), Large),
        remove_member(3, Base),
        put_member_list(fixture_member_list([4, 1]), Base)
    ].

fixture_user_ids(Count) ->
    lists:sort(fun(A, B) -> erlang:phash2(A) =< erlang:phash2(B) end, lists:seq(1, Count)).

fixture_member_map(UserIds) ->
    maps:from_list([{UserId, fixture_member(UserId)} || UserId <- UserIds]).

fixture_member_list(UserIds) ->
    [fixture_member(UserId) || UserId <- UserIds].

fixture_member(UserId) ->
    #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}}.

-endif.
