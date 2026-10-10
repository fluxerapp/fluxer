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
