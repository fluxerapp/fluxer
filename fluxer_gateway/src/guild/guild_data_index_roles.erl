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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

role_list_non_map_input_test() ->
    ?assertEqual([], role_list(not_a_map)).

role_list_non_list_roles_value_test() ->
    ?assertEqual([], role_list(#{<<"roles">> => <<"invalid">>})).

role_index_non_map_input_test() ->
    ?assertEqual(#{}, role_index(not_a_map)).

role_index_from_list_test() ->
    Data = #{
        <<"roles">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"Admin">>},
            #{<<"id">> => <<"200">>, <<"name">> => <<"Member">>}
        ]
    },
    Index = role_index(Data),
    ?assertEqual(2, map_size(Index)),
    ?assertEqual(<<"Admin">>, maps:get(<<"name">>, maps:get(100, Index))).

put_roles_updates_list_and_index_test() ->
    Data = #{<<"roles">> => [#{<<"id">> => <<"1">>, <<"name">> => <<"old">>}]},
    NewRoles = [
        #{<<"id">> => <<"10">>, <<"name">> => <<"new1">>},
        #{<<"id">> => <<"20">>, <<"name">> => <<"new2">>}
    ],
    Updated = put_roles(NewRoles, Data),
    ?assertEqual(
        [
            #{<<"id">> => 10, <<"name">> => <<"new1">>},
            #{<<"id">> => 20, <<"name">> => <<"new2">>}
        ],
        role_list(Updated)
    ),
    ?assertEqual(2, map_size(role_index(Updated))).

put_roles_non_map_data_returns_unchanged_test() ->
    ?assertEqual(not_a_map, put_roles([], not_a_map)).

-endif.
