%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_state_roles_delete).
-typing([eqwalizer]).

-export([strip_role_from_members/2, strip_role_from_channel_overwrites/2]).

-type guild_data() :: map().
-type user_id() :: integer().

-export_type([guild_data/0]).

-spec strip_role_from_members(term(), guild_data()) -> guild_data().
strip_role_from_members(RoleId, Data) when is_integer(RoleId) ->
    MemberRoleIndex = guild_data_index_members:member_role_index(Data),
    AffectedUsers = maps:keys(maps:get(RoleId, MemberRoleIndex, #{})),
    lists:foldl(
        fun(UserId, AccData) ->
            strip_role_from_single_member(UserId, RoleId, AccData)
        end,
        Data,
        AffectedUsers
    );
strip_role_from_members(_RoleId, Data) ->
    Data.

-spec strip_role_from_single_member(user_id(), integer(), guild_data()) -> guild_data().
strip_role_from_single_member(UserId, RoleIdInt, AccData) ->
    case guild_data_index_members:get_member(UserId, AccData) of
        undefined ->
            AccData;
        Member ->
            remove_role_from_member(Member, RoleIdInt, AccData)
    end.

-spec remove_role_from_member(map(), integer(), guild_data()) -> guild_data().
remove_role_from_member(Member, RoleIdInt, AccData) ->
    MemberRoles = maps:get(<<"roles">>, Member, []),
    FilteredRoles = lists:filter(
        fun(R) ->
            snowflake_id:parse(R) =/= RoleIdInt
        end,
        MemberRoles
    ),
    guild_data_index_members:put_member(Member#{<<"roles">> => FilteredRoles}, AccData).

-spec strip_role_from_channel_overwrites(term(), guild_data()) -> guild_data().
strip_role_from_channel_overwrites(RoleId, Data) when is_integer(RoleId) ->
    Channels = guild_data_index_channels:channel_list(Data),
    UpdatedChannels = lists:map(
        fun(Channel) -> strip_role_overwrite_from_channel(Channel, RoleId) end,
        Channels
    ),
    guild_data_index:put_channels(UpdatedChannels, Data);
strip_role_from_channel_overwrites(_RoleId, Data) ->
    Data.

-spec strip_role_overwrite_from_channel(map(), integer()) -> map().
strip_role_overwrite_from_channel(Channel, RoleIdInt) when is_map(Channel) ->
    Overwrites = permission_overwrites(Channel),
    FilteredOverwrites = lists:filter(
        fun(Overwrite) -> not is_role_overwrite_match(Overwrite, RoleIdInt) end,
        Overwrites
    ),
    Channel#{<<"permission_overwrites">> => FilteredOverwrites}.

-spec permission_overwrites(map()) -> [term()].
permission_overwrites(Channel) ->
    case maps:get(<<"permission_overwrites">>, Channel, []) of
        Overwrites when is_list(Overwrites) -> Overwrites;
        _ -> error(badarg)
    end.

-spec is_role_overwrite_match(map() | term(), integer()) -> boolean().
is_role_overwrite_match(Overwrite, RoleIdInt) when is_map(Overwrite) ->
    OverwriteType = map_utils:get_integer(Overwrite, <<"type">>, undefined),
    OverwriteId = snowflake_id:parse_optional(maps:get(<<"id">>, Overwrite, undefined)),
    OverwriteType =:= 0 andalso OverwriteId =:= RoleIdInt;
is_role_overwrite_match(_, _) ->
    false.
