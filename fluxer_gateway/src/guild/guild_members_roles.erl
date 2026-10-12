%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_members_roles).
-typing([eqwalizer]).

-export([
    can_manage_roles/2,
    can_manage_role/2,
    get_assignable_roles/2,
    check_target_member/2,
    user_ids_for_any_role/2
]).

-export_type([guild_state/0, guild_reply/1, user_id/0, role_id/0]).

-type guild_state() :: map().
-type guild_reply(T) :: {reply, T, guild_state()}.
-type role() :: map().
-type user_id() :: integer().
-type optional_user_id() :: user_id() | undefined.
-type role_id() :: integer().

-spec user_ids_for_any_role([role_id()], guild_state()) -> [user_id()].
user_ids_for_any_role(RoleIds, State) ->
    Data = guild_members_common:guild_data(State),
    MemberRoleIndex = guild_data_index_members:member_role_index(Data),
    gb_sets:to_list(
        lists:foldl(
            fun(RoleId, AccSet) ->
                add_role_user_ids(RoleId, MemberRoleIndex, AccSet)
            end,
            gb_sets:empty(),
            RoleIds
        )
    ).

-spec add_role_user_ids(role_id(), map(), gb_sets:set(user_id())) -> gb_sets:set(user_id()).
add_role_user_ids(RoleId, MemberRoleIndex, AccSet) ->
    case maps:get(RoleId, MemberRoleIndex, undefined) of
        undefined -> AccSet;
        UserMap -> add_user_map_ids(UserMap, AccSet)
    end.

-spec add_user_map_ids(map(), gb_sets:set(user_id())) -> gb_sets:set(user_id()).
add_user_map_ids(UserMap, AccSet) ->
    lists:foldl(fun gb_sets:add/2, AccSet, maps:keys(UserMap)).

-spec can_manage_roles(map(), guild_state()) -> guild_reply(map()).
can_manage_roles(#{user_id := UserId, target_user_id := _TU, role_id := RoleId}, State) ->
    OwnerId = guild_members_common:owner_id(State),
    Reply = check_can_manage(UserId, RoleId, OwnerId, State),
    {reply, #{can_manage => Reply}, State};
can_manage_roles(#{user_id := UserId, role_id := RoleId}, State) ->
    OwnerId = guild_members_common:owner_id(State),
    Reply = check_can_manage(UserId, RoleId, OwnerId, State),
    {reply, #{can_manage => Reply}, State}.

-spec can_manage_role(map(), guild_state()) -> guild_reply(map()).
can_manage_role(#{user_id := UserId, role_id := RoleId}, State) ->
    Data = guild_members_common:guild_data(State),
    Roles = guild_data_index_roles:role_index(Data),
    Reply =
        case guild_permissions_check:find_role_by_id(RoleId, Roles) of
            undefined ->
                false;
            Role ->
                UserMax = guild_permissions_check:get_max_role_position(UserId, State),
                RolePos = maps:get(<<"position">>, Role, 0),
                can_rank_manage_role(UserMax, RolePos, UserId, RoleId, State)
        end,
    {reply, #{can_manage => Reply}, State}.

-spec get_assignable_roles(map(), guild_state()) -> guild_reply(map()).
get_assignable_roles(#{user_id := UserId}, State) ->
    Roles = guild_members_common:guild_roles(State),
    OwnerId = guild_members_common:owner_id(State),
    RoleIds =
        case UserId =:= OwnerId of
            true ->
                guild_members_common:role_ids_from_roles(Roles);
            false ->
                UserRoleRank = get_member_role_rank(UserId, State),
                assignable_role_ids(Roles, UserRoleRank)
        end,
    {reply, #{role_ids => RoleIds}, State}.

-spec assignable_role_ids([role()], {integer(), integer()}) -> [role_id()].
assignable_role_ids(Roles, UserRoleRank) ->
    lists:filtermap(
        fun(Role) ->
            assignable_role_id(Role, UserRoleRank)
        end,
        Roles
    ).

-spec assignable_role_id(role(), {integer(), integer()}) -> {true, role_id()} | false.
assignable_role_id(Role, UserRoleRank) ->
    case {role_rank(Role) < UserRoleRank, role_id(Role)} of
        {true, Id} when is_integer(Id) -> {true, Id};
        _ -> false
    end.

-spec check_target_member(map(), guild_state()) -> guild_reply(map()).
check_target_member(#{user_id := UserId, target_user_id := TargetUserId}, State) ->
    OwnerId = guild_members_common:owner_id(State),
    CanManage = check_can_manage_target(UserId, TargetUserId, OwnerId, State),
    {reply, #{can_manage => CanManage}, State}.

-spec check_can_manage(user_id(), role_id(), optional_user_id(), guild_state()) -> boolean().
check_can_manage(UserId, _RoleId, UserId, _State) ->
    true;
check_can_manage(UserId, RoleId, _OwnerId, State) ->
    UserPerms = guild_permissions:get_member_permissions(UserId, undefined, State),
    case bitset:has(UserPerms, constants:manage_roles_permission()) of
        false ->
            false;
        true ->
            Data = guild_members_common:guild_data(State),
            Roles = guild_data_index_roles:role_index(Data),
            check_role_position(UserId, RoleId, Roles, State)
    end.

-spec check_role_position(user_id(), role_id(), map(), guild_state()) -> boolean().
check_role_position(UserId, RoleId, Roles, State) ->
    case guild_permissions_check:find_role_by_id(RoleId, Roles) of
        undefined -> false;
        Role -> check_role_position_for_role(UserId, Role, State)
    end.

-spec check_role_position_for_role(user_id(), role(), guild_state()) -> boolean().
check_role_position_for_role(UserId, Role, State) ->
    UserMax = guild_permissions_check:get_max_role_position(UserId, State),
    RolePos = maps:get(<<"position">>, Role, 0),
    case role_id(Role) of
        undefined -> false;
        RId -> can_rank_manage_role(UserMax, RolePos, UserId, RId, State)
    end.

-spec can_rank_manage_role(integer(), integer(), user_id(), role_id(), guild_state()) ->
    boolean().
can_rank_manage_role(UserMax, RolePos, UserId, RoleId, State) ->
    UserMax > RolePos orelse
        (UserMax =:= RolePos andalso compare_role_ids_eq_pos(UserId, RoleId, State)).

-spec compare_role_ids_eq_pos(user_id(), role_id(), guild_state()) -> boolean().
compare_role_ids_eq_pos(UserId, TargetRoleId, State) ->
    case guild_permissions_check:find_member_by_user_id(UserId, State) of
        undefined ->
            false;
        Member ->
            MemberRoles = guild_members_common:member_roles(Member),
            Data = guild_members_common:guild_data(State),
            Roles = guild_data_index_roles:role_index(Data),
            highest_role_below(get_highest_role(MemberRoles, Roles), TargetRoleId)
    end.

-spec highest_role_below(map() | undefined, role_id()) -> boolean().
highest_role_below(undefined, _TargetRoleId) ->
    false;
highest_role_below(HighestRole, TargetRoleId) ->
    case role_id(HighestRole) of
        undefined -> false;
        HighestRoleId -> HighestRoleId < TargetRoleId
    end.

-spec get_highest_role([role_id()], [role()] | map()) -> role() | undefined.
get_highest_role(MemberRoleIds, Roles) ->
    lists:foldl(
        fun(RoleId, Acc) ->
            maybe_highest_role(RoleId, Roles, Acc)
        end,
        undefined,
        MemberRoleIds
    ).

-spec maybe_highest_role(role_id(), [role()] | map(), role() | undefined) -> role() | undefined.
maybe_highest_role(RoleId, Roles, Acc) ->
    case guild_permissions_check:find_role_by_id(RoleId, Roles) of
        undefined -> Acc;
        Role -> compare_roles(Role, Acc)
    end.

-spec compare_roles(role(), role() | undefined) -> role().
compare_roles(Role, undefined) ->
    Role;
compare_roles(Role, AccRole) ->
    AccPos = maps:get(<<"position">>, AccRole, 0),
    RolePos = maps:get(<<"position">>, Role, 0),
    case RolePos > AccPos of
        true -> Role;
        false -> compare_roles_with_same_or_lower_position(RolePos, AccPos, Role, AccRole)
    end.

-spec compare_roles_with_same_or_lower_position(integer(), integer(), role(), role()) -> role().
compare_roles_with_same_or_lower_position(RolePos, AccPos, Role, AccRole) ->
    case RolePos =:= AccPos of
        true -> compare_equal_position_roles(Role, AccRole);
        false -> AccRole
    end.

-spec check_can_manage_target(user_id(), user_id(), optional_user_id(), guild_state()) ->
    boolean().
check_can_manage_target(UserId, _Target, UserId, _State) ->
    true;
check_can_manage_target(_UserId, OwnerId, OwnerId, _State) ->
    false;
check_can_manage_target(UserId, TargetUserId, _OwnerId, State) ->
    get_member_role_rank(UserId, State) > get_member_role_rank(TargetUserId, State).

-spec get_member_role_rank(user_id(), guild_state()) -> {integer(), integer()}.
get_member_role_rank(UserId, State) ->
    case guild_permissions_check:find_member_by_user_id(UserId, State) of
        undefined -> {-1, 0};
        Member -> member_role_rank(Member, State)
    end.

-spec member_role_rank(map(), guild_state()) -> {integer(), integer()}.
member_role_rank(Member, State) ->
    Data = guild_members_common:guild_data(State),
    Roles = guild_data_index_roles:role_index(Data),
    case get_highest_role(guild_members_common:member_roles(Member), Roles) of
        undefined -> {-1, 0};
        HighestRole -> role_rank(HighestRole)
    end.

-spec compare_equal_position_roles(role(), role()) -> role().
compare_equal_position_roles(Role, AccRole) ->
    case {role_id(Role), role_id(AccRole)} of
        {undefined, _} -> AccRole;
        {_, undefined} -> Role;
        {RoleId, AccRoleId} when RoleId < AccRoleId -> Role;
        _ -> AccRole
    end.

-spec role_rank(role()) -> {integer(), integer()}.
role_rank(Role) ->
    case role_id(Role) of
        undefined ->
            {maps:get(<<"position">>, Role, 0), -16#7fffffffffffffff};
        RoleId ->
            {maps:get(<<"position">>, Role, 0), -RoleId}
    end.

-spec role_id(role()) -> role_id() | undefined.
role_id(Role) ->
    snowflake_id:parse_optional(maps:get(<<"id">>, Role, undefined)).
