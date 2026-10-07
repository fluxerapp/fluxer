%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_store).
-typing([eqwalizer]).

-export([
    new/1,
    bulk_load/3,
    update_member/5,
    remove_member/2,
    set_online/3,
    set_hoisted_roles/2,
    get_counts/1,
    get_groups/1,
    get_items/3,
    prepare_member_tuples/2,
    prepare_hoisted_role_ids/2,
    extract_role_ids/1
]).

-type store_ref() :: ets:table().
-type member_tuple() :: {integer(), binary(), [integer()], boolean()}.
-type guild_state() :: #{member_presence := ets:tid() | map(), term() => term()}.

-export_type([store_ref/0, member_tuple/0, guild_state/0]).

-spec new(integer()) -> store_ref().
new(_GuildId) ->
    guild_member_list_engine:new().

-spec bulk_load(store_ref(), [member_tuple()], [integer()]) -> ok.
bulk_load(Ref, Members, HoistedRoleIds) ->
    guild_member_list_engine:bulk_load(Ref, Members, HoistedRoleIds).

-spec update_member(store_ref(), integer(), binary(), [integer()], boolean()) -> ok.
update_member(Ref, UserId, SortKey, RoleIds, IsOnline) ->
    guild_member_list_engine:update_member(Ref, UserId, SortKey, RoleIds, IsOnline).

-spec remove_member(store_ref(), integer()) -> ok.
remove_member(Ref, UserId) ->
    guild_member_list_engine:remove_member(Ref, UserId).

-spec set_online(store_ref(), integer(), boolean()) -> ok.
set_online(Ref, UserId, IsOnline) ->
    guild_member_list_engine:set_online(Ref, UserId, IsOnline).

-spec set_hoisted_roles(store_ref(), [integer()]) -> changed | unchanged.
set_hoisted_roles(Ref, HoistedRoleIds) ->
    guild_member_list_engine:set_hoisted_roles(Ref, HoistedRoleIds).

-spec get_counts(store_ref()) -> {non_neg_integer(), non_neg_integer()}.
get_counts(Ref) ->
    guild_member_list_engine:get_counts(Ref).

-spec get_groups(store_ref()) -> [{binary(), non_neg_integer()}].
get_groups(Ref) ->
    guild_member_list_engine:get_groups(Ref).

-spec get_items(store_ref(), non_neg_integer(), non_neg_integer()) ->
    [{group, binary(), non_neg_integer()} | {member, integer()}].
get_items(Ref, Start, End) ->
    guild_member_list_engine:get_items(Ref, Start, End).

-spec prepare_member_tuples(map(), guild_state()) -> [member_tuple()].
prepare_member_tuples(MemberMap, State) ->
    ConnectedUserIds = guild_member_list_common:connected_session_user_ids(State),
    PresenceTab = maps:get(member_presence, State),
    maps:fold(
        fun(UserId, Member, Acc) ->
            member_tuple(UserId, Member, PresenceTab, ConnectedUserIds, Acc)
        end,
        [],
        MemberMap
    ).

-spec member_tuple(integer(), map(), ets:tid() | map(), sets:set(integer()), [member_tuple()]) ->
    [member_tuple()].
member_tuple(UserId, Member, PresenceTab, ConnectedUserIds, Acc) ->
    DisplayName = guild_member_list_common:get_member_display_name(Member),
    SortKey = guild_member_list_common:casefold_binary(DisplayName),
    RoleIds = extract_role_ids(Member),
    Presence = guild_state_member:lookup_presence(PresenceTab, UserId),
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    IsConnected = sets:is_element(UserId, ConnectedUserIds),
    IsOnline =
        IsConnected andalso Status =/= <<"offline">> andalso Status =/= <<"invisible">>,
    [{UserId, SortKey, RoleIds, IsOnline} | Acc].

-spec prepare_hoisted_role_ids([map()], integer() | undefined) -> [integer()].
prepare_hoisted_role_ids(Roles, GuildId) ->
    HoistedRoles = guild_member_list_groups:get_hoisted_roles_sorted(Roles, GuildId),
    [
        Id
     || Role <- HoistedRoles,
        Id <- [snowflake_id:parse_optional(maps:get(<<"id">>, Role, undefined))],
        is_integer(Id),
        Id > 0
    ].

-spec extract_role_ids(map()) -> [integer()].
extract_role_ids(Member) ->
    RawRoles = map_utils:ensure_list(
        maps:get(<<"roles">>, Member, [])
    ),
    lists:filtermap(fun filtermap_snowflake/1, RawRoles).

-spec filtermap_snowflake(term()) -> {true, integer()} | false.
filtermap_snowflake(RoleId) ->
    case snowflake_id:parse_optional(RoleId) of
        Id when is_integer(Id), Id > 0 -> {true, Id};
        _ -> false
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_returns_nif_ref_test() ->
    Ref = new(123),
    ?assert(is_reference(Ref)).

-endif.
