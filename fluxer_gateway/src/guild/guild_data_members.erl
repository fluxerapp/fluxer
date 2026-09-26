%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_members).
-typing([eqwalizer]).

-export([
    get_guild_member/2,
    get_guild_members_batch/2,
    has_member/2,
    list_guild_members/2,
    search_guild_members/2,
    find_member_by_user_id/2,
    paginate_members/3
]).

-type guild_state() :: map().
-type guild_member() :: map().
-type user_id() :: integer().

-export_type([guild_state/0, guild_member/0, user_id/0]).

-spec get_guild_member(map(), guild_state()) -> {reply, map(), guild_state()}.
get_guild_member(#{user_id := UserId}, State) ->
    case find_member_by_user_id(UserId, State) of
        undefined ->
            {reply, #{success => false, member_data => null}, State};
        Member ->
            {reply, #{success => true, member_data => Member}, State}
    end.

-spec get_guild_members_batch(map(), guild_state()) -> {reply, map(), guild_state()}.
get_guild_members_batch(#{user_ids := UserIds}, State) when is_list(UserIds) ->
    Members = lists:foldl(
        fun(UserId, Acc) -> add_member_to_batch(UserId, State, Acc) end,
        #{},
        lists:usort(UserIds)
    ),
    {reply, #{members => Members}, State}.

-spec add_member_to_batch(user_id(), guild_state(), map()) -> map().
add_member_to_batch(UserId, State, Acc) ->
    case find_member_by_user_id(UserId, State) of
        undefined -> Acc;
        Member -> Acc#{UserId => Member}
    end.

-spec has_member(map(), guild_state()) -> {reply, map(), guild_state()}.
has_member(#{user_id := UserId}, State) ->
    case find_member_by_user_id(UserId, State) of
        undefined ->
            {reply, #{has_member => false}, State};
        _ ->
            {reply, #{has_member => true}, State}
    end.

-spec list_guild_members(map(), guild_state()) -> {reply, map(), guild_state()}.
list_guild_members(#{limit := Limit, offset := Offset}, State) ->
    Data = guild_data_index:ensure_data_map(State),
    MemberMap = guild_data_index:member_map(Data),
    SortedIds = guild_data_index_members:sorted_member_ids(Data, MemberMap),
    PageIds = paginate_members(SortedIds, Limit, Offset),
    PaginatedMembers = lookup_members(PageIds, MemberMap),
    {reply, #{members => PaginatedMembers, total => map_size(MemberMap)}, State}.

-spec lookup_members([term()], map()) -> [guild_member()].
lookup_members(UserIds, MemberMap) ->
    [Member || UserId <- UserIds, {ok, Member} <- [maps:find(UserId, MemberMap)]].

-spec search_guild_members(map(), guild_state()) -> {reply, map(), guild_state()}.
search_guild_members(#{query := Query, limit := Limit}, State) when
    is_binary(Query), is_integer(Limit), Limit >= 0
->
    Data = guild_data_index:ensure_data_map(State),
    MemberMap = guild_data_index:member_map(Data),
    Members = collect_query_matches(MemberMap, string:lowercase(Query), Limit),
    {reply, #{members => Members, total => map_size(MemberMap)}, State};
search_guild_members(_Request, State) ->
    {reply, #{members => [], total => 0}, State}.

-spec collect_query_matches(#{user_id() => guild_member()}, binary(), non_neg_integer()) ->
    [guild_member()].
collect_query_matches(_MemberMap, _NormalizedQuery, 0) ->
    [];
collect_query_matches(MemberMap, NormalizedQuery, Limit) ->
    collect_query_matches_iter(maps:iterator(MemberMap), NormalizedQuery, Limit, []).

-spec collect_query_matches_iter(
    maps:iterator(), binary(), non_neg_integer(), [guild_member()]
) -> [guild_member()].
collect_query_matches_iter(_Iterator, _NormalizedQuery, 0, Acc) ->
    lists:reverse(Acc);
collect_query_matches_iter(Iterator, NormalizedQuery, Remaining, Acc) ->
    case maps:next(Iterator) of
        none ->
            lists:reverse(Acc);
        {_UserId, Member, NextIterator} when is_map(Member) ->
            collect_query_match(Member, NormalizedQuery, Remaining, Acc, NextIterator);
        {_UserId, _Member, NextIterator} ->
            collect_query_matches_iter(NextIterator, NormalizedQuery, Remaining, Acc)
    end.

-spec collect_query_match(
    guild_member(), binary(), pos_integer(), [guild_member()], maps:iterator()
) -> [guild_member()].
collect_query_match(Member, NormalizedQuery, Remaining, Acc, NextIterator) ->
    case
        guild_request_members_search:member_matches_normalized_query(Member, NormalizedQuery)
    of
        true ->
            collect_query_matches_iter(
                NextIterator, NormalizedQuery, Remaining - 1, [Member | Acc]
            );
        false ->
            collect_query_matches_iter(NextIterator, NormalizedQuery, Remaining, Acc)
    end.

-spec find_member_by_user_id(user_id(), guild_state()) -> guild_member() | undefined.
find_member_by_user_id(UserId, State) ->
    guild_permissions:find_member_by_user_id(UserId, State).

-spec paginate_members([term()], non_neg_integer(), non_neg_integer()) -> [term()].
paginate_members(Items, Limit, Offset) ->
    take_page(drop_items(Items, Offset), Limit, []).

-spec drop_items([term()], non_neg_integer()) -> [term()].
drop_items(Items, 0) ->
    Items;
drop_items([], _Offset) ->
    [];
drop_items([_ | Rest], Offset) ->
    drop_items(Rest, Offset - 1).

-spec take_page([term()], non_neg_integer(), [term()]) -> [term()].
take_page(_Remaining, 0, Acc) ->
    lists:reverse(Acc);
take_page([], _Limit, Acc) ->
    lists:reverse(Acc);
take_page([Item | Rest], Limit, Acc) ->
    take_page(Rest, Limit - 1, [Item | Acc]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

list_guild_members_matches_full_list_paging_test() ->
    lists:foreach(fun assert_list_page_matches/1, list_paging_cases()).

assert_list_page_matches({State, Limit, Offset}) ->
    {reply, Reply, _} = list_guild_members(#{limit => Limit, offset => Offset}, State),
    ?assertEqual(legacy_list_guild_members(Limit, Offset, State), Reply).

legacy_list_guild_members(Limit, Offset, State) ->
    Data = guild_data_index:ensure_data_map(State),
    AllMembers = guild_data_index:member_list(Data),
    #{members => paginate_members(AllMembers, Limit, Offset), total => length(AllMembers)}.

list_paging_cases() ->
    [
        {State, Limit, Offset}
     || State <- list_paging_states(),
        Limit <- [0, 1, 2, 5, 99],
        Offset <- [0, 1, 3, 5, 6, 20, 39, 40, 41, 99]
    ].

list_paging_states() ->
    Members = paging_members(paging_user_ids(40)),
    Cached = guild_data_index:normalize_data(#{<<"members">> => Members}),
    [
        #{data => Cached},
        #{data => guild_data_index:put_member(paging_member(500), Cached)},
        #{data => guild_data_index:remove_member(7, Cached)},
        #{data => #{<<"members">> => Members}},
        #{data => guild_data_index:normalize_data(#{<<"members">> => []})},
        #{data => #{<<"members">> => #{}}},
        #{}
    ].

paging_members(UserIds) ->
    [paging_member(UserId) || UserId <- UserIds].

paging_user_ids(Count) ->
    lists:sort(fun(A, B) -> erlang:phash2(A) =< erlang:phash2(B) end, lists:seq(1, Count)).

paging_member(UserId) ->
    #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}}.

-endif.
