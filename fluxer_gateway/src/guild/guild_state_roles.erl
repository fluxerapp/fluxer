%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_state_roles).
-typing([eqwalizer]).

-export([
    handle_role_create/2,
    handle_role_update/2,
    handle_role_update_bulk/2,
    handle_role_delete/2,
    sync_hoisted_roles/1,
    recompute_visibility_for_roles/3,
    extract_role_ids_from_role_update/1,
    extract_role_ids_from_role_update_bulk/1,
    extract_role_ids_from_role_delete/1
]).

-type guild_state() :: map().
-type guild_data() :: map().
-type event_data() :: map().

-export_type([guild_state/0, guild_data/0, event_data/0]).
-type user_id() :: integer().
-type role_id() :: integer().

-spec handle_role_create(event_data(), guild_data()) -> guild_data().
handle_role_create(EventData, Data) ->
    case role_data(EventData) of
        undefined ->
            Data;
        RoleData ->
            Roles = guild_data_index_roles:role_list(Data),
            guild_data_index:put_roles([RoleData | Roles], Data)
    end.

-spec handle_role_update(event_data(), guild_data()) -> guild_data().
handle_role_update(EventData, Data) ->
    case role_data(EventData) of
        undefined ->
            Data;
        RoleData ->
            Roles = guild_data_index_roles:role_list(Data),
            RoleId = maps:get(<<"id">>, RoleData),
            UpdatedRoles = guild_state_utils:replace_item_by_id(Roles, RoleId, RoleData),
            guild_data_index:put_roles(UpdatedRoles, Data)
    end.

-spec handle_role_update_bulk(event_data(), guild_data()) -> guild_data().
handle_role_update_bulk(EventData, Data) ->
    Roles = guild_data_index_roles:role_list(Data),
    BulkRoles = maps:get(<<"roles">>, EventData, []),
    UpdatedRoles = guild_state_utils:bulk_update_items(Roles, BulkRoles),
    guild_data_index:put_roles(UpdatedRoles, Data).

-spec handle_role_delete(event_data(), guild_data()) -> guild_data().
handle_role_delete(EventData, Data) ->
    case role_id(maps:get(<<"role_id">>, EventData, undefined)) of
        undefined ->
            Data;
        RoleIdInt ->
            Roles = guild_data_index_roles:role_list(Data),
            FilteredRoles = guild_state_utils:remove_item_by_id(Roles, RoleIdInt),
            Data1 = guild_data_index:put_roles(FilteredRoles, Data),
            Data2 = guild_state_roles_delete:strip_role_from_members(RoleIdInt, Data1),
            guild_state_roles_delete:strip_role_from_channel_overwrites(RoleIdInt, Data2)
    end.

-spec sync_hoisted_roles(guild_state()) -> guild_state().
sync_hoisted_roles(State) ->
    case guild_id(State) of
        undefined ->
            State;
        GuildId ->
            Data = maps:get(data, State, #{}),
            Roles = map_utils:ensure_list(maps:get(<<"roles">>, Data, [])),
            HoistedRoleIds = guild_member_list_store:prepare_hoisted_role_ids(Roles, GuildId),
            _ = sync_zero_engine_hoisted_roles(HoistedRoleIds, State),
            _ = guild_member_list_channel_engine:set_hoisted_roles_all(HoistedRoleIds, State),
            State
    end.

-spec sync_zero_engine_hoisted_roles([integer()], guild_state()) -> boolean().
sync_zero_engine_hoisted_roles(HoistedRoleIds, State) ->
    case maps:get(member_list_engine, State, undefined) of
        undefined ->
            false;
        Ref ->
            guild_member_list_engine:set_hoisted_roles(Ref, HoistedRoleIds) =:= changed
    end.

-spec recompute_visibility_for_roles([integer()], guild_state(), guild_state()) ->
    guild_state().
recompute_visibility_for_roles(RoleIds, StateWithUpdatedUser, UpdatedState) ->
    case guild_id(UpdatedState) of
        undefined ->
            UpdatedState;
        GuildId ->
            ValidRoleIds = positive_role_ids(RoleIds),
            recompute_visibility_for_valid_roles(
                GuildId, ValidRoleIds, StateWithUpdatedUser, UpdatedState
            )
    end.

-spec recompute_visibility_for_valid_roles(
    role_id(), [role_id()], guild_state(), guild_state()
) -> guild_state().
recompute_visibility_for_valid_roles(GuildId, ValidRoleIds, StateWithUpdatedUser, UpdatedState) ->
    case lists:member(GuildId, ValidRoleIds) of
        true ->
            guild_visibility_overwrites:compute_and_dispatch_visibility_changes(
                StateWithUpdatedUser,
                UpdatedState
            );
        false ->
            UserIds = affected_user_ids_for_roles(
                ValidRoleIds, StateWithUpdatedUser, UpdatedState
            ),
            recompute_for_user_ids(UserIds, StateWithUpdatedUser, UpdatedState)
    end.

-spec recompute_for_user_ids([user_id()], guild_state(), guild_state()) -> guild_state().
recompute_for_user_ids([], _StateWithUpdatedUser, UpdatedState) ->
    UpdatedState;
recompute_for_user_ids(UserIds, StateWithUpdatedUser, UpdatedState) ->
    guild_visibility_overwrites:compute_and_dispatch_visibility_changes_for_users(
        UserIds,
        StateWithUpdatedUser,
        UpdatedState
    ).

-spec affected_user_ids_for_roles([integer()], guild_state(), guild_state()) -> [user_id()].
affected_user_ids_for_roles(RoleIds, StateWithUpdatedUser, UpdatedState) ->
    OldData = maps:get(data, StateWithUpdatedUser, #{}),
    NewData = maps:get(data, UpdatedState, #{}),
    OldMemberRoleIndex = guild_data_index_members:member_role_index(OldData),
    NewMemberRoleIndex = guild_data_index_members:member_role_index(NewData),
    UserIdSet = lists:foldl(
        fun(RoleId, AccSet) ->
            collect_users_for_role(RoleId, OldMemberRoleIndex, NewMemberRoleIndex, AccSet)
        end,
        sets:new(),
        lists:usort(RoleIds)
    ),
    user_id_set_to_list(UserIdSet).

-spec collect_users_for_role(integer(), map(), map(), sets:set()) -> sets:set().
collect_users_for_role(RoleId, OldIndex, NewIndex, AccSet) ->
    AccSet1 = add_role_index_users(maps:get(RoleId, OldIndex, #{}), AccSet),
    add_role_index_users(maps:get(RoleId, NewIndex, #{}), AccSet1).

-spec add_role_index_users(map(), sets:set()) -> sets:set().
add_role_index_users(RoleUsers, AccSet) ->
    maps:fold(
        fun(UserId, _Value, CurrentSet) ->
            sets:add_element(UserId, CurrentSet)
        end,
        AccSet,
        RoleUsers
    ).

-spec user_id_set_to_list(sets:set()) -> [user_id()].
user_id_set_to_list(UserIdSet) ->
    lists:filtermap(fun user_id_from_term/1, sets:to_list(UserIdSet)).

-spec user_id_from_term(term()) -> false | {true, user_id()}.
user_id_from_term(UserId) when is_integer(UserId), UserId > 0 ->
    {true, UserId};
user_id_from_term(_) ->
    false.

-spec extract_role_ids_from_role_update(event_data()) -> [integer()].
extract_role_ids_from_role_update(EventData) ->
    RoleData = maps:get(<<"role">>, EventData, #{}),
    case role_id(maps:get(<<"id">>, RoleData, undefined)) of
        undefined -> [];
        RoleId -> [RoleId]
    end.

-spec extract_role_ids_from_role_update_bulk(event_data()) -> [integer()].
extract_role_ids_from_role_update_bulk(EventData) ->
    Roles = maps:get(<<"roles">>, EventData, []),
    lists:filtermap(fun role_id_from_data/1, Roles).

-spec extract_role_ids_from_role_delete(event_data()) -> [integer()].
extract_role_ids_from_role_delete(EventData) ->
    case role_id(maps:get(<<"role_id">>, EventData, undefined)) of
        undefined -> [];
        RoleId -> [RoleId]
    end.

-spec role_data(event_data()) -> map() | undefined.
role_data(EventData) ->
    case maps:get(<<"role">>, EventData, undefined) of
        RoleData when is_map(RoleData) -> validate_role_data(RoleData);
        _ -> undefined
    end.

-spec validate_role_data(map()) -> map() | undefined.
validate_role_data(RoleData) ->
    case role_id(maps:get(<<"id">>, RoleData, undefined)) of
        undefined -> undefined;
        _ -> RoleData
    end.

-spec guild_id(guild_state()) -> role_id() | undefined.
guild_id(State) ->
    role_id(maps:get(id, State, undefined)).

-spec positive_role_ids([term()]) -> [role_id()].
positive_role_ids(RoleIds) ->
    lists:filtermap(fun positive_role_id/1, RoleIds).

-spec role_id_from_data(map()) -> false | {true, role_id()}.
role_id_from_data(RoleData) ->
    case role_id(maps:get(<<"id">>, RoleData, undefined)) of
        undefined -> false;
        RoleId -> {true, RoleId}
    end.

-spec positive_role_id(term()) -> false | {true, role_id()}.
positive_role_id(Value) ->
    case role_id(Value) of
        undefined -> false;
        RoleId -> {true, RoleId}
    end.

-spec role_id(term()) -> role_id() | undefined.
role_id(Value) ->
    case snowflake_id:parse_maybe(Value) of
        Id when is_integer(Id), Id > 0 -> Id;
        _ -> undefined
    end.
