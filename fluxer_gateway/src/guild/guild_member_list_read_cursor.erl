%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_read_cursor).
-typing([eqwalizer]).

-export([get_members_cursor/2]).

-type guild_state() :: map().

-export_type([guild_state/0]).

-spec get_members_cursor(map(), guild_state()) -> {reply, map(), guild_state()}.
get_members_cursor(Request, State) ->
    Limit = maps:get(<<"limit">>, Request, 1),
    AfterId = snowflake_id:parse_optional(maps:get(<<"after">>, Request, undefined)),
    members_cursor(Limit, AfterId, State).

-spec members_cursor(integer(), integer() | undefined, guild_state()) ->
    {reply, map(), guild_state()}.
members_cursor(Limit, _AfterId, State) when Limit =< 0 ->
    Data = maps:get(data, State, #{}),
    MemberMap = guild_data_index:member_map(Data),
    {reply, #{members => [], total => map_size(MemberMap)}, State};
members_cursor(Limit, AfterId, State) ->
    Data = maps:get(data, State, #{}),
    MemberMap = guild_data_index:member_map(Data),
    Total = map_size(MemberMap),
    SortedIds = guild_data_index_members:sorted_member_ids(Data, MemberMap),
    FilteredIds = filter_ids_after(SortedIds, AfterId),
    ResponseMembers = take_members(FilteredIds, Limit, MemberMap),
    {reply, #{members => ResponseMembers, total => Total}, State}.

-spec take_members([integer()], integer(), map()) -> [map()].
take_members(_Ids, Limit, _MemberMap) when Limit =< 0 -> [];
take_members(Ids, Limit, MemberMap) ->
    [Member || Id <- lists:sublist(Ids, Limit), {ok, Member} <- [maps:find(Id, MemberMap)]].

-spec filter_ids_after([integer()], integer() | undefined) -> [integer()].
filter_ids_after(Ids, undefined) -> Ids;
filter_ids_after(Ids, AfterId) -> lists:dropwhile(fun(Id) -> Id =< AfterId end, Ids).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_members_cursor_matches_full_sort_test() ->
    lists:foreach(fun assert_cursor_page_matches/1, cursor_cases()).

assert_cursor_page_matches({State, Limit, After}) ->
    {reply, Reply, _} = get_members_cursor(cursor_request(Limit, After), State),
    ?assertEqual(legacy_members_cursor(Limit, After, State), Reply).

legacy_members_cursor(Limit, After, State) ->
    MemberMap = guild_data_index:member_map(maps:get(data, State, #{})),
    AfterId = snowflake_id:parse_optional(After),
    Ids = filter_ids_after(lists:sort(maps:keys(MemberMap)), AfterId),
    #{members => legacy_take_members(Ids, Limit, MemberMap), total => map_size(MemberMap)}.

legacy_take_members(_Ids, Limit, _MemberMap) when Limit =< 0 -> [];
legacy_take_members(Ids, Limit, MemberMap) ->
    [maps:get(Id, MemberMap) || Id <- lists:sublist(Ids, Limit)].

cursor_request(Limit, undefined) -> #{<<"limit">> => Limit};
cursor_request(Limit, After) -> #{<<"limit">> => Limit, <<"after">> => After}.

cursor_cases() ->
    [
        {State, Limit, After}
     || State <- cursor_states(),
        Limit <- [0, 1, 2, 99],
        After <- [undefined, 1, 5, 12, 39, 40, 41, 99]
    ].

cursor_states() ->
    Members = cursor_members(cursor_user_ids(40)),
    Cached = guild_data_index:normalize_data(#{<<"members">> => Members}),
    [
        #{data => Cached},
        #{data => guild_data_index:put_member(cursor_member(500), Cached)},
        #{data => #{<<"members">> => Members}},
        #{data => guild_data_index:normalize_data(#{<<"members">> => []})},
        #{}
    ].

cursor_members(UserIds) ->
    [cursor_member(UserId) || UserId <- UserIds].

cursor_user_ids(Count) ->
    lists:sort(fun(A, B) -> erlang:phash2(A) =< erlang:phash2(B) end, lists:seq(1, Count)).

cursor_member(UserId) ->
    #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}}.

-endif.
