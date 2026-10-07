%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_permissions).
-typing([eqwalizer]).

-export([
    get_member_permissions/3,
    compute_member_permissions/4,
    member_base_permissions/3,
    channel_permissions/4,
    can_view_channel/4,
    viewable_channel_ids/4,
    can_view_channel_by_permissions/4,
    can_view_channel_members/4,
    can_manage_channel/3,
    can_access_message_by_permissions/3,
    apply_channel_overwrites/5,
    get_max_role_position/2,
    find_member_by_user_id/2,
    find_role_by_id/2,
    find_channel_by_id/2,
    view_inputs/2,
    aggregate_role_permissions_cached/4
]).

-export_type([
    permission/0,
    user_id/0,
    role_id/0,
    maybe_channel_id/0,
    guild_state/0,
    member/0,
    maybe_member/0,
    member_roles/0,
    base_permissions/0
]).

-define(ALL_PERMISSIONS, 16#FFFFFFFFFFFFFFFF).

-type permission() :: non_neg_integer().
-type user_id() :: integer().
-type role_id() :: integer().
-type maybe_channel_id() :: integer() | undefined.
-type guild_state() :: map().
-type guild_data() :: map().
-type member() :: map().
-type maybe_member() :: member() | undefined.
-type member_roles() :: [role_id()].
-type base_permissions() :: permission() | {permission(), member_roles(), role_id()}.

-spec get_member_permissions(user_id(), maybe_channel_id(), guild_state()) -> permission().
get_member_permissions(UserId, ChannelId, State) ->
    compute_member_permissions(UserId, ChannelId, undefined, State).

-spec compute_member_permissions(user_id(), maybe_channel_id(), maybe_member(), guild_state()) ->
    permission().
compute_member_permissions(UserId, ChannelId, ProvidedMember, State) ->
    case thread_for_channel(ChannelId, State) of
        undefined ->
            overwrite_permissions(
                member_base_permissions(UserId, ProvidedMember, State), UserId, ChannelId, State
            );
        Thread ->
            guild_thread_permissions:resolve(UserId, Thread, ProvidedMember, State)
    end.

-spec member_base_permissions(user_id(), maybe_member(), guild_state()) -> base_permissions().
member_base_permissions(UserId, ProvidedMember, State) when is_integer(UserId) ->
    case guild_permissions_common:resolve_data_map(State) of
        undefined ->
            0;
        Data ->
            base_permissions_for_data(UserId, ProvidedMember, State, Data)
    end;
member_base_permissions(_, _, _) ->
    0.

-spec channel_permissions(base_permissions(), user_id(), maybe_channel_id(), guild_state()) ->
    permission().
channel_permissions(Base, UserId, ChannelId, State) ->
    case thread_for_channel(ChannelId, State) of
        undefined ->
            overwrite_permissions(Base, UserId, ChannelId, State);
        Thread ->
            guild_thread_permissions:resolve(
                UserId, Thread, find_member_by_user_id(UserId, State), State
            )
    end.

-spec overwrite_permissions(base_permissions(), user_id(), maybe_channel_id(), guild_state()) ->
    permission().
overwrite_permissions({Permissions, MemberRoles, GuildId}, UserId, ChannelId, State) ->
    guild_permissions_overwrites:maybe_apply_channel_overwrites(
        Permissions, UserId, MemberRoles, ChannelId, GuildId, State
    );
overwrite_permissions(Permissions, _UserId, _ChannelId, _State) ->
    Permissions.

-spec thread_for_channel(maybe_channel_id(), guild_state()) -> map() | undefined.
thread_for_channel(ChannelId, State) when is_integer(ChannelId) ->
    case guild_thread_gate:store(State) of
        undefined ->
            undefined;
        Tab ->
            try
                guild_thread_store:get_thread(Tab, ChannelId)
            catch
                error:badarg -> undefined
            end
    end;
thread_for_channel(_ChannelId, _State) ->
    undefined.

-spec base_permissions_for_data(user_id(), maybe_member(), guild_state(), guild_data()) ->
    base_permissions().
base_permissions_for_data(UserId, ProvidedMember, State, Data) ->
    OwnerId = guild_owner_id(Data),
    case UserId =:= OwnerId of
        true -> ?ALL_PERMISSIONS;
        false -> non_owner_base_permissions(UserId, ProvidedMember, State, Data)
    end.

-spec can_view_channel(user_id(), integer(), maybe_member(), guild_state()) -> boolean().
can_view_channel(UserId, ChannelId, Member, State) ->
    guild_permissions_check:can_view_channel(UserId, ChannelId, Member, State).

-spec viewable_channel_ids(user_id(), base_permissions(), [map()], guild_state()) ->
    #{integer() => true}.
viewable_channel_ids(UserId, Base, Channels, State) ->
    guild_permissions_check:viewable_channel_ids(UserId, Base, Channels, State).

-spec can_view_channel_by_permissions(user_id(), integer(), maybe_member(), guild_state()) ->
    boolean().
can_view_channel_by_permissions(UserId, ChannelId, Member, State) ->
    guild_permissions_check:can_view_channel_by_permissions(UserId, ChannelId, Member, State).

-spec can_view_channel_members(user_id(), integer(), maybe_member(), guild_state()) ->
    boolean().
can_view_channel_members(UserId, ChannelId, Member, State) ->
    guild_permissions_check:can_view_channel_members(UserId, ChannelId, Member, State).

-spec can_manage_channel(user_id(), maybe_channel_id(), guild_state()) -> boolean().
can_manage_channel(UserId, ChannelId, State) ->
    guild_permissions_check:can_manage_channel(UserId, ChannelId, State).

-spec can_access_message_by_permissions(permission(), binary(), guild_state()) -> boolean().
can_access_message_by_permissions(Permissions, MessageId, State) ->
    guild_permissions_check:can_access_message_by_permissions(Permissions, MessageId, State).

-spec apply_channel_overwrites(
    permission(), user_id() | undefined, member_roles(), map(), role_id()
) -> permission().
apply_channel_overwrites(BasePerms, UserId, MemberRoles, Channel, EveryoneRoleId) ->
    guild_permissions_overwrites:apply_channel_overwrites(
        BasePerms, UserId, MemberRoles, Channel, EveryoneRoleId
    ).

-spec get_max_role_position(user_id(), guild_state()) -> integer().
get_max_role_position(UserId, State) ->
    guild_permissions_check:get_max_role_position(UserId, State).

-spec find_member_by_user_id(user_id(), guild_state()) -> member() | undefined.
find_member_by_user_id(UserId, State) ->
    guild_permissions_check:find_member_by_user_id(UserId, State).

-spec find_role_by_id(role_id(), [map()] | map()) -> map() | undefined.
find_role_by_id(RoleId, Roles) ->
    guild_permissions_check:find_role_by_id(RoleId, Roles).

-spec find_channel_by_id(integer(), guild_state()) -> map() | undefined.
find_channel_by_id(ChannelId, State) ->
    guild_permissions_check:find_channel_by_id(ChannelId, State).

-spec view_inputs(integer(), guild_state()) -> term().
view_inputs(ChannelId, State) ->
    guild_permissions_check:view_inputs(ChannelId, State).

-spec non_owner_base_permissions(user_id(), maybe_member(), guild_state(), guild_data()) ->
    base_permissions().
non_owner_base_permissions(UserId, ProvidedMember, State, Data) ->
    case resolve_member(UserId, ProvidedMember, State) of
        undefined ->
            0;
        Member ->
            member_role_base_permissions(Member, State, Data)
    end.

-spec member_role_base_permissions(member(), guild_state(), guild_data()) -> base_permissions().
member_role_base_permissions(Member, State, Data) ->
    case guild_id(State) of
        undefined ->
            0;
        GuildId ->
            Roles = guild_data_index:role_index(Data),
            RolePermsCache = maps:get(role_perms_cache, Data, #{}),
            BasePermissions = cached_role_permissions(GuildId, RolePermsCache, Roles),
            MemberRoles = guild_permissions_common:member_role_ids(Member),
            Permissions = aggregate_role_permissions_cached(
                MemberRoles, RolePermsCache, Roles, BasePermissions
            ),
            case permission_bits:has(Permissions, constants:administrator_permission()) of
                true -> ?ALL_PERMISSIONS;
                false -> {Permissions, MemberRoles, GuildId}
            end
    end.

-spec resolve_member(user_id(), maybe_member(), guild_state()) -> maybe_member().
resolve_member(_UserId, Member, _State) when is_map(Member) ->
    Member;
resolve_member(UserId, _Member, State) ->
    guild_permissions_check:find_member_by_user_id(UserId, State).

-spec guild_owner_id(guild_data()) -> user_id() | undefined.
guild_owner_id(Data) ->
    Guild = maps:get(<<"guild">>, Data, #{}),
    snowflake_id:parse_optional(maps:get(<<"owner_id">>, Guild, undefined)).

-spec guild_id(guild_state()) -> integer() | undefined.
guild_id(State) ->
    case maps:get(id, State, undefined) of
        undefined -> snowflake_id:parse_optional(maps:get(<<"id">>, State, undefined));
        GuildId when is_integer(GuildId) -> GuildId;
        GuildId -> snowflake_id:parse(GuildId)
    end.

-spec cached_role_permissions(role_id(), map(), [map()] | map()) -> permission().
cached_role_permissions(RoleId, Cache, Roles) ->
    case maps:get(RoleId, Cache, undefined) of
        V when is_integer(V) -> V;
        _ -> base_role_permissions(RoleId, Roles)
    end.

-spec base_role_permissions(role_id(), [map()] | map()) -> permission().
base_role_permissions(GuildId, Roles) ->
    case guild_permissions_check:find_role_by_id(GuildId, Roles) of
        undefined -> 0;
        Role -> guild_permissions_common:role_permissions(Role)
    end.

-spec aggregate_role_permissions_cached(
    member_roles(), map(), [map()] | map(), permission()
) -> permission().
aggregate_role_permissions_cached(MemberRoles, Cache, Roles, BasePermissions) ->
    lists:foldl(
        fun(RoleId, Acc) ->
            add_cached_role_permissions(RoleId, Cache, Roles, Acc)
        end,
        BasePermissions,
        MemberRoles
    ).

-spec add_cached_role_permissions(role_id(), map(), [map()] | map(), permission()) ->
    permission().
add_cached_role_permissions(RoleId, Cache, Roles, Acc) ->
    case maps:get(RoleId, Cache, undefined) of
        V when is_integer(V) ->
            permission_bits:add(Acc, V);
        _ ->
            add_role_permissions(RoleId, Roles, Acc)
    end.

-spec add_role_permissions(role_id(), [map()] | map(), permission()) -> permission().
add_role_permissions(RoleId, Roles, Acc) ->
    case guild_permissions_check:find_role_by_id(RoleId, Roles) of
        undefined -> Acc;
        Role -> permission_bits:add(Acc, guild_permissions_common:role_permissions(Role))
    end.
