%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_permissions_common).
-typing([eqwalizer]).

-export([
    ensure_list/1,
    extract_integer_list/1,
    resolve_data_map/1,
    member_role_ids/1,
    role_permissions/1
]).

-export_type([permission/0, member/0, role/0, guild_state/0, guild_data/0]).

-type permission() :: non_neg_integer().
-type member() :: map().
-type role() :: map().
-type guild_state() :: map().
-type guild_data() :: map().

-spec ensure_list(term()) -> list().
ensure_list(Value) -> map_utils:ensure_list(Value).

-spec extract_integer_list(term()) -> [integer()].
extract_integer_list(List) when is_list(List) ->
    snowflake_id:parse_list(List);
extract_integer_list(undefined) ->
    [];
extract_integer_list(null) ->
    [];
extract_integer_list(Value) ->
    erlang:error({invalid_snowflake_list, Value}).

-spec resolve_data_map(guild_state() | map()) -> guild_data() | undefined.
resolve_data_map(State) when is_map(State) ->
    case maps:find(data, State) of
        {ok, Data} when is_map(Data) -> Data;
        {ok, Data} -> Data;
        error -> state_as_data_map(State)
    end;
resolve_data_map(_) ->
    undefined.

-spec state_as_data_map(map()) -> guild_data() | undefined.
state_as_data_map(State) ->
    case maps:is_key(<<"members">>, State) of
        true -> State;
        false -> undefined
    end.

-spec member_role_ids(member()) -> [integer()].
member_role_ids(Member) ->
    RoleIds = maps:get(<<"roles">>, Member, []),
    extract_integer_list(RoleIds).

-spec role_permissions(role()) -> permission().
role_permissions(Role) ->
    case permission_bits:parse_optional(maps:get(<<"permissions">>, Role, undefined)) of
        undefined -> 0;
        Permissions -> Permissions
    end.
