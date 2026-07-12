%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_permissions_check).
-typing([eqwalizer]).

-export([
    can_view_channel/4,
    can_view_channel_by_permissions/4,
    can_view_channel_members/4,
    can_manage_channel/3,
    can_access_message_by_permissions/3,
    get_max_role_position/2,
    find_member_by_user_id/2,
    find_role_by_id/2,
    find_channel_by_id/2
]).

-export_type([
    permission/0,
    user_id/0,
    role_id/0,
    channel_id/0,
    maybe_channel_id/0,
    guild_state/0,
    member/0,
    role/0,
    channel/0,
    maybe_member/0
]).

-type permission() :: non_neg_integer().
-type user_id() :: integer().
-type role_id() :: integer().
-type channel_id() :: integer().
-type maybe_channel_id() :: channel_id() | undefined.
-type guild_state() :: map().
-type guild_data() :: #{members_ets => ets:tid(), term() => term()}.
-type member() :: map().
-type role() :: map().
-type channel() :: map().
-type maybe_member() :: member() | undefined.

-spec can_view_channel(user_id(), channel_id(), maybe_member(), guild_state()) -> boolean().
can_view_channel(UserId, ChannelId, Member, State) ->
    guild_virtual_channel_access:has_virtual_access(UserId, ChannelId, State) orelse
        can_view_channel_by_permissions(UserId, ChannelId, Member, State) orelse
        is_category_with_viewable_child(UserId, ChannelId, Member, State).

-spec can_view_channel_by_permissions(user_id(), channel_id(), maybe_member(), guild_state()) ->
    boolean().
can_view_channel_by_permissions(UserId, ChannelId, Member, State) ->
    Perms = guild_permissions:compute_member_permissions(UserId, ChannelId, Member, State),
    permission_bits:has(Perms, constants:view_channel_permission()).

-spec can_view_channel_members(user_id(), channel_id(), maybe_member(), guild_state()) ->
    boolean().
can_view_channel_members(UserId, ChannelId, Member, State) ->
    Perms = guild_permissions:compute_member_permissions(UserId, ChannelId, Member, State),
    permission_bits:has(Perms, constants:view_channel_members_permission()).

-spec can_manage_channel(user_id(), maybe_channel_id(), guild_state()) -> boolean().
can_manage_channel(UserId, ChannelId, State) ->
    Perms = guild_permissions:get_member_permissions(UserId, ChannelId, State),
    permission_bits:has(Perms, constants:manage_channels_permission()).

-spec can_access_message_by_permissions(permission(), binary(), guild_state()) -> boolean().
can_access_message_by_permissions(Permissions, MessageId, State) ->
    HasReadHistory = permission_bits:has(
        Permissions, constants:read_message_history_permission()
    ),
    case HasReadHistory of
        true ->
            true;
        false ->
            check_message_history_cutoff(MessageId, State)
    end.

-spec check_message_history_cutoff(binary(), guild_state()) -> boolean().
check_message_history_cutoff(MessageId, State) ->
    case get_message_history_cutoff(State) of
        null ->
            false;
        CutoffMs ->
            MessageMs = snowflake_util:extract_timestamp(MessageId),
            MessageMs >= CutoffMs
    end.

-spec get_message_history_cutoff(guild_state()) -> integer() | null.
get_message_history_cutoff(State) ->
    case guild_permissions_common:resolve_data_map(State) of
        undefined ->
            null;
        Data ->
            Guild = maps:get(<<"guild">>, Data, #{}),
            parse_cutoff(maps:get(<<"message_history_cutoff">>, Guild, null))
    end.

-spec parse_cutoff(term()) -> integer() | null.
parse_cutoff(null) ->
    null;
parse_cutoff(CutoffBin) when is_binary(CutoffBin) ->
    calendar:rfc3339_to_system_time(binary_to_list(CutoffBin), [{unit, millisecond}]);
parse_cutoff(CutoffInt) when is_integer(CutoffInt) ->
    CutoffInt;
parse_cutoff(_) ->
    null.

-spec get_max_role_position(user_id(), guild_state()) -> integer().
get_max_role_position(UserId, State) ->
    ResolvedData = guild_permissions_common:resolve_data_map(State),
    case {find_member_by_user_id(UserId, State), ResolvedData} of
        {undefined, _} ->
            -1;
        {_, undefined} ->
            -1;
        {Member, Data} ->
            Roles = guild_data_index:role_index(Data),
            compute_max_position(Member, Roles)
    end.

-spec compute_max_position(member(), [role()] | map()) -> integer().
compute_max_position(Member, Roles) ->
    lists:foldl(
        fun(RoleId, MaxPos) ->
            max_role_position_for_id(RoleId, Roles, MaxPos)
        end,
        -1,
        guild_permissions_common:member_role_ids(Member)
    ).

-spec max_role_position_for_id(role_id(), [role()] | map(), integer()) -> integer().
max_role_position_for_id(RoleId, Roles, MaxPos) ->
    case find_role_by_id(RoleId, Roles) of
        undefined -> MaxPos;
        Role -> max(role_position(Role), MaxPos)
    end.

-spec find_member_by_user_id(user_id(), guild_state()) -> member() | undefined.
find_member_by_user_id(UserId, State) when is_integer(UserId) ->
    case guild_permissions_common:resolve_data_map(State) of
        undefined -> undefined;
        Data -> find_member_in_data(UserId, Data)
    end;
find_member_by_user_id(_, _) ->
    undefined.

-spec find_member_in_data(user_id(), guild_data()) -> member() | undefined.
find_member_in_data(UserId, Data) when is_integer(UserId), is_map(Data) ->
    case members_ets_table(Data) of
        Tab when is_reference(Tab) ->
            find_member_in_ets(UserId, Tab);
        undefined ->
            find_member_without_ets(UserId, Data)
    end;
find_member_in_data(_, _) ->
    undefined.

-spec find_member_in_ets(user_id(), ets:tid()) -> member() | undefined.
find_member_in_ets(UserId, Tab) ->
    case ets:lookup(Tab, UserId) of
        [{_, Member}] -> Member;
        [] -> undefined
    end.

-spec find_member_without_ets(user_id(), guild_data()) -> member() | undefined.
find_member_without_ets(UserId, Data) ->
    case maps:get(<<"members">>, Data, undefined) of
        Members when is_map(Members) -> find_member_in_map(UserId, Members);
        Members when is_list(Members) -> find_member_in_list(UserId, Members);
        _ -> undefined
    end.

-spec members_ets_table(guild_data()) -> ets:tid() | undefined.
members_ets_table(#{members_ets := Tab}) ->
    Tab;
members_ets_table(_) ->
    undefined.

-spec find_member_in_map(user_id(), map()) -> member() | undefined.
find_member_in_map(UserId, Members) when is_integer(UserId), is_map(Members) ->
    case snowflake_id:get(UserId, Members, undefined) of
        Member when is_map(Member) ->
            Member;
        _ ->
            undefined
    end.

-spec find_member_in_list(user_id(), [term()]) -> member() | undefined.
find_member_in_list(UserId, Members) when is_integer(UserId), is_list(Members) ->
    find_member_in_list_loop(UserId, Members).

-spec find_member_in_list_loop(user_id(), [term()]) -> member() | undefined.
find_member_in_list_loop(_UserId, []) ->
    undefined;
find_member_in_list_loop(UserId, [Member | Rest]) when is_map(Member) ->
    case member_user_id(Member) of
        UserId -> Member;
        _ -> find_member_in_list_loop(UserId, Rest)
    end;
find_member_in_list_loop(UserId, [_ | Rest]) ->
    find_member_in_list_loop(UserId, Rest).

-spec member_user_id(member()) -> user_id() | undefined.
member_user_id(Member) when is_map(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    snowflake_id:parse_maybe(maps:get(<<"id">>, User, undefined)).

-spec find_role_by_id(role_id(), [role()] | map()) -> role() | undefined.
find_role_by_id(RoleId, Roles) when is_map(Roles) ->
    case snowflake_id:parse_maybe(RoleId) of
        undefined -> undefined;
        Id -> maps:get(Id, Roles, undefined)
    end;
find_role_by_id(RoleId, Roles) ->
    case snowflake_id:parse_maybe(RoleId) of
        undefined ->
            undefined;
        TargetId ->
            find_role_in_list(TargetId, guild_permissions_common:ensure_list(Roles))
    end.

-spec find_role_in_list(role_id(), [term()]) -> role() | undefined.
find_role_in_list(_TargetId, []) ->
    undefined;
find_role_in_list(TargetId, [Role | Rest]) when is_map(Role) ->
    case role_id(Role) of
        TargetId -> Role;
        _ -> find_role_in_list(TargetId, Rest)
    end;
find_role_in_list(TargetId, [_ | Rest]) ->
    find_role_in_list(TargetId, Rest).

-spec role_id(role()) -> role_id() | undefined.
role_id(Role) ->
    snowflake_id:parse_maybe(maps:get(<<"id">>, Role, undefined)).

-spec find_channel_by_id(channel_id(), guild_state()) -> channel() | undefined.
find_channel_by_id(ChannelId, State) ->
    case
        {snowflake_id:parse_maybe(ChannelId), guild_permissions_common:resolve_data_map(State)}
    of
        {ResolvedChannelId, Data} when is_integer(ResolvedChannelId), is_map(Data) ->
            case maps:get(ResolvedChannelId, guild_data_index:channel_index(Data), undefined) of
                undefined ->
                    maps:get(
                        ResolvedChannelId, guild_state_channels:thread_index(Data), undefined
                    );
                Channel ->
                    Channel
            end;
        _ ->
            undefined
    end.

-spec is_category_with_viewable_child(user_id(), channel_id(), maybe_member(), guild_state()) ->
    boolean().
is_category_with_viewable_child(UserId, ChannelId, Member, State) ->
    case find_channel_by_id(ChannelId, State) of
        #{<<"type">> := 4} -> any_child_viewable(UserId, ChannelId, Member, State);
        _ -> false
    end.

-spec any_child_viewable(user_id(), channel_id(), maybe_member(), guild_state()) -> boolean().
any_child_viewable(UserId, CategoryId, Member, State) ->
    case guild_permissions_common:resolve_data_map(State) of
        undefined ->
            false;
        Data ->
            any_child_viewable_in_data(UserId, CategoryId, Member, State, Data)
    end.

-spec any_child_viewable_in_data(user_id(), channel_id(), maybe_member(), guild_state(), map()) ->
    boolean().
any_child_viewable_in_data(UserId, CategoryId, Member, State, Data) ->
    Channels = map_utils:ensure_list(maps:get(<<"channels">>, Data, [])),
    lists:any(
        fun(Channel) -> is_viewable_child(Channel, UserId, CategoryId, Member, State) end,
        Channels
    ).

-spec is_viewable_child(map(), user_id(), channel_id(), maybe_member(), guild_state()) ->
    boolean().
is_viewable_child(Channel, UserId, CategoryId, Member, State) ->
    ParentId = snowflake_id:parse_maybe(maps:get(<<"parent_id">>, Channel, undefined)),
    ChildId = snowflake_id:parse_maybe(maps:get(<<"id">>, Channel, undefined)),
    case {ParentId, ChildId} of
        {CategoryId, ResolvedChildId} when is_integer(ResolvedChildId) ->
            can_view_channel_by_permissions(UserId, ResolvedChildId, Member, State);
        _ ->
            false
    end.

-spec role_position(role()) -> integer().
role_position(Role) ->
    case guild_data_normalize_schema:int(maps:get(<<"position">>, Role, undefined)) of
        undefined -> 0;
        Position -> Position
    end.
