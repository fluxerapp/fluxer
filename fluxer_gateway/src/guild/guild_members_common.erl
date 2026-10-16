%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_members_common).
-typing([eqwalizer]).

-export([
    guild_data/1,
    guild_members/1,
    guild_roles/1,
    guild_channels/1,
    owner_id/1,
    member_user_id/1,
    member_roles/1,
    member_has_any_role_set/2,
    is_member_bot/1,
    member_can_view_channel/4,
    normalize_int_list/1,
    unique_preserve/1,
    role_ids_from_roles/1,
    collect_mentions_for_user_ids/5,
    collect_mentions/5,
    build_connected_user_ids/2,
    check_should_mention/3
]).

-export_type([guild_state/0, member/0, user_id/0, role_id/0, channel_id/0]).

-type guild_state() :: map().
-type member() :: map().
-type user_id() :: integer().
-type role_id() :: integer().
-type channel_id() :: integer().

-spec guild_data(guild_state()) -> map().
guild_data(State) -> map_utils:ensure_map(map_utils:get_safe(State, data, #{})).

-spec guild_members(guild_state()) -> [member()].
guild_members(State) -> guild_data_index_members:member_values(guild_data(State)).

-spec guild_roles(guild_state()) -> [map()].
guild_roles(State) -> guild_data_index_roles:role_list(guild_data(State)).

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

-spec role_ids_from_roles([map()]) -> [role_id()].
role_ids_from_roles(Roles) ->
    lists:filtermap(fun role_id_from_role/1, Roles).

-spec role_id_from_role(map()) -> {true, role_id()} | false.
role_id_from_role(Role) ->
    SafeRole = map_utils:ensure_map(Role),
    case snowflake_id:parse_maybe(maps:get(<<"id">>, SafeRole, undefined)) of
        undefined -> false;
        RoleId -> {true, RoleId}
    end.

-spec collect_mentions_for_user_ids(
    [user_id()], user_id(), channel_id(), guild_state(), fun((user_id(), member()) -> boolean())
) -> [user_id()].
collect_mentions_for_user_ids(UserIds, AuthorId, ChannelId, State, Predicate) ->
    MemberMap = guild_data_index_members:member_map(guild_data(State)),
    lists:filtermap(
        fun(UserId) ->
            collect_mention_for_user_id(
                UserId, AuthorId, ChannelId, State, Predicate, MemberMap
            )
        end,
        lists:usort(UserIds)
    ).

-spec collect_mention_for_user_id(
    user_id(),
    user_id(),
    channel_id(),
    guild_state(),
    fun((user_id(), member()) -> boolean()),
    map()
) -> {true, user_id()} | false.
collect_mention_for_user_id(UserId, UserId, _ChannelId, _State, _Predicate, _MemberMap) ->
    false;
collect_mention_for_user_id(UserId, _AuthorId, ChannelId, State, Predicate, MemberMap) ->
    case maps:get(UserId, MemberMap, undefined) of
        undefined -> false;
        Member -> maybe_collect_user_mention(UserId, Member, ChannelId, State, Predicate)
    end.

-spec maybe_collect_user_mention(
    user_id(), member(), channel_id(), guild_state(), fun((user_id(), member()) -> boolean())
) -> {true, user_id()} | false.
maybe_collect_user_mention(UserId, Member, ChannelId, State, Predicate) ->
    case
        Predicate(UserId, Member) andalso
            member_can_view_channel(UserId, ChannelId, Member, State)
    of
        true -> {true, UserId};
        false -> false
    end.

-spec collect_mentions(
    [member()], user_id(), channel_id(), guild_state(), fun((member()) -> boolean())
) -> [user_id()].
collect_mentions(Members, AuthorId, ChannelId, State, Predicate) ->
    lists:filtermap(
        fun(Member) ->
            collect_mention(Member, AuthorId, ChannelId, State, Predicate)
        end,
        Members
    ).

-spec collect_mention(
    member(), user_id(), channel_id(), guild_state(), fun((member()) -> boolean())
) -> {true, user_id()} | false.
collect_mention(Member, AuthorId, ChannelId, State, Predicate) ->
    case member_user_id(Member) of
        undefined -> false;
        AuthorId -> false;
        UserId -> maybe_collect_member_mention(UserId, Member, ChannelId, State, Predicate)
    end.

-spec maybe_collect_member_mention(
    user_id(), member(), channel_id(), guild_state(), fun((member()) -> boolean())
) -> {true, user_id()} | false.
maybe_collect_member_mention(UserId, Member, ChannelId, State, Predicate) ->
    case Predicate(Member) andalso member_can_view_channel(UserId, ChannelId, Member, State) of
        true -> {true, UserId};
        false -> false
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

-spec check_should_mention(user_id(), member(), map()) -> boolean().
check_should_mention(UserId, Member, Context) ->
    #{
        mention_everyone := MentionEveryone,
        mention_here := MentionHere,
        has_role_mentions := HasRoleMentions,
        has_direct_mentions := HasDirectMentions,
        role_id_set := RoleIdSet,
        direct_user_id_set := DirectUserIdSet,
        connected_user_ids := ConnectedUserIds
    } = Context,
    MentionEveryone orelse
        (MentionHere andalso gb_sets:is_member(UserId, ConnectedUserIds)) orelse
        (HasRoleMentions andalso member_has_any_role_set(Member, RoleIdSet)) orelse
        (HasDirectMentions andalso gb_sets:is_member(UserId, DirectUserIdSet)).

-spec truthy(term()) -> boolean().
truthy(true) -> true;
truthy(_) -> false.
