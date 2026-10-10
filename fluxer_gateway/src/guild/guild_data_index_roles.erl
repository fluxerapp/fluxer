%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_index_roles).
-typing([eqwalizer]).

-export([
    role_list/1,
    role_index/1,
    put_roles/2,
    build_role_perms_cache/1
]).

-type guild_data() :: map().
-type role() :: map().
-type snowflake_id() :: integer().

-export_type([guild_data/0, role/0, snowflake_id/0]).

-spec role_list(term()) -> [role()].
role_list(Data) when is_map(Data) ->
    [
        normalize_role(Role)
     || Role <- guild_data_index:ensure_list(
            maps:get(<<"roles">>, Data, [])
        ),
        is_map(Role)
    ];
role_list(_) ->
    [].

-spec role_index(term()) -> #{snowflake_id() => role()}.
role_index(Data) when is_map(Data) ->
    case maps:get(<<"role_index">>, Data, undefined) of
        Index when is_map(Index) -> existing_role_index(Index);
        _ -> guild_data_index:build_id_index(role_list(Data))
    end;
role_index(_) ->
    #{}.

-spec put_roles(term(), term()) -> term().
put_roles(Roles, Data) when is_map(Data) ->
    RoleList = [
        normalize_role(Role)
     || Role <- guild_data_index:ensure_list(Roles),
        is_map(Role)
    ],
    Data#{
        <<"roles">> => RoleList,
        <<"role_index">> => guild_data_index:build_id_index(RoleList),
        role_perms_cache => build_role_perms_cache(RoleList)
    };
put_roles(_, Data) ->
    Data.

-spec build_role_perms_cache([role()]) -> #{integer() => integer()}.
build_role_perms_cache(Roles) ->
    lists:foldl(
        fun
            (Role, Acc) when is_map(Role) ->
                cache_role_permissions(Role, Acc);
            (_, Acc) ->
                Acc
        end,
        #{},
        Roles
    ).

-spec cache_role_permissions(role(), map()) -> map().
cache_role_permissions(Role, Acc) ->
    RoleId = snowflake_id:parse_optional(maps:get(<<"id">>, Role, undefined)),
    Perms = permission_bits:parse_optional(maps:get(<<"permissions">>, Role, undefined)),
    cache_role_permissions(RoleId, Perms, Acc).

-spec cache_role_permissions(term(), term(), map()) -> map().
cache_role_permissions(RoleId, Perms, Acc) when is_integer(RoleId), is_integer(Perms) ->
    Acc#{RoleId => Perms};
cache_role_permissions(_RoleId, _Perms, Acc) ->
    Acc.

-spec normalize_role(role()) -> role().
normalize_role(Role) ->
    case guild_data_normalize:role(Role) of
        Normalized when is_map(Normalized) -> Normalized;
        _ -> Role
    end.

-spec existing_role_index(map()) -> #{snowflake_id() => role()}.
existing_role_index(Index) ->
    Index.
