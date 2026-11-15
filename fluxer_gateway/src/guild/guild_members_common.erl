%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_members_common).
-typing([eqwalizer]).

-export([
    guild_data/1,
    guild_channels/1,
    owner_id/1,
    member_user_id/1,
    member_roles/1,
    member_has_any_role_set/2,
    is_member_bot/1,
    member_can_view_channel/4,
    normalize_int_list/1,
    unique_preserve/1,
    build_connected_user_ids/2
]).

-export_type([guild_state/0, member/0, user_id/0, role_id/0, channel_id/0]).

-type guild_state() :: map().
-type member() :: map().
-type user_id() :: integer().
-type role_id() :: integer().
-type channel_id() :: integer().

-spec guild_data(guild_state()) -> map().
guild_data(State) -> map_utils:ensure_map(map_utils:get_safe(State, data, #{})).

-spec guild_channels(guild_state()) -> [map()].
guild_channels(State) -> guild_data_index_channels:channel_list(guild_data(State)).

-spec owner_id(guild_state()) -> user_id() | undefined.
owner_id(State) ->
    Guild = map_utils:ensure_map(maps:get(<<"guild">>, guild_data(State), #{})),
    snowflake_id:parse_maybe(maps:get(<<"owner_id">>, Guild, undefined)).

-spec member_user_id(member()) -> user_id() | undefined.
member_user_id(Member) ->
    User = map_utils:ensure_map(maps:get(<<"user">>, Member, #{})),
    snowflake_id:parse_maybe(maps:get(<<"id">>, User, undefined)).

-spec member_roles(member()) -> [role_id()].
member_roles(Member) ->
    normalize_int_list(map_utils:ensure_list(maps:get(<<"roles">>, Member, []))).

-spec member_has_any_role_set(member(), gb_sets:set(role_id())) -> boolean().
member_has_any_role_set(Member, RoleIdSet) ->
    lists:any(fun(RoleId) -> gb_sets:is_member(RoleId, RoleIdSet) end, member_roles(Member)).

-spec is_member_bot(member()) -> boolean().
is_member_bot(Member) ->
    User = map_utils:ensure_map(maps:get(<<"user">>, Member, #{})),
    truthy(maps:get(<<"bot">>, User, false)).

-spec member_can_view_channel(user_id(), channel_id(), member(), guild_state()) -> boolean().
member_can_view_channel(UserId, ChannelId, Member, State) when is_integer(ChannelId) ->
    guild_permissions_check:can_view_channel(UserId, ChannelId, Member, State);
member_can_view_channel(_, _, _, _) ->
    false.

-spec normalize_int_list(term()) -> [integer()].
normalize_int_list(List) ->
    lists:reverse(lists:foldl(fun normalize_int/2, [], map_utils:ensure_list(List))).

-spec normalize_int(term(), [integer()]) -> [integer()].
normalize_int(Value, Acc) ->
    case snowflake_id:parse_maybe(Value) of
        undefined -> Acc;
        Int -> [Int | Acc]
    end.

-spec unique_preserve([integer()]) -> [integer()].
unique_preserve(List) ->
    {Result, _} = lists:foldl(fun unique_value/2, {[], #{}}, List),
    lists:reverse(Result).

-spec unique_value(integer(), {[integer()], map()}) -> {[integer()], map()}.
unique_value(Value, {Acc, Seen}) ->
    case maps:is_key(Value, Seen) of
        true -> {Acc, Seen};
        false -> {[Value | Acc], Seen#{Value => true}}
    end.

-spec build_connected_user_ids(boolean(), map()) -> gb_sets:set(user_id()).
build_connected_user_ids(false, _Sessions) ->
    gb_sets:empty();
build_connected_user_ids(true, Sessions) ->
    gb_sets:from_list(maps:fold(fun add_connected_user_id/3, [], Sessions)).

-spec add_connected_user_id(term(), map(), [user_id()]) -> [user_id()].
add_connected_user_id(_SessionId, SessionData, Acc) ->
    case maps:get(user_id, SessionData, undefined) of
        UserId when is_integer(UserId) -> [UserId | Acc];
        _ -> Acc
    end.

-spec truthy(term()) -> boolean().
truthy(true) -> true;
truthy(_) -> false.
