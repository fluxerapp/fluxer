%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_groups).
-typing([eqwalizer]).

-export([
    get_hoisted_roles_sorted/2,
    get_effective_hoist_position/1,
    find_top_hoisted_role/2
]).

-spec get_hoisted_roles_sorted([map()], integer() | undefined) -> [map()].
get_hoisted_roles_sorted(Roles, GuildId) ->
    HoistedRoles = lists:filter(
        fun(Role) ->
            IsHoist = maps:get(<<"hoist">>, Role, false),
            RoleId = role_id(Role),
            IsHoist andalso RoleId =/= undefined andalso RoleId =/= GuildId
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

-spec find_top_hoisted_role([integer()], [integer()]) -> integer() | undefined.
find_top_hoisted_role(MemberRoleIds, HoistedRoleIds) ->
    Shared = [RId || RId <- HoistedRoleIds, role_is_member(RId, MemberRoleIds)],
    case Shared of
        [] -> undefined;
        [Top | _] -> Top
    end.

-spec role_is_member(integer(), [integer()]) -> boolean().
role_is_member(RoleId, MemberRoleIds) ->
    lists:member(RoleId, MemberRoleIds).

-spec role_id(map()) -> integer() | undefined.
role_id(Role) ->
    case snowflake_id:parse_maybe(maps:get(<<"id">>, Role, undefined)) of
        RoleId when is_integer(RoleId), RoleId > 0 -> RoleId;
        _ -> undefined
    end.
