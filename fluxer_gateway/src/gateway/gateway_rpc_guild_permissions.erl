%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild_permissions).

-typing([eqwalizer]).

-export([handle/2, get_permissions_cached_or_rpc/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(GUILD_CALL_TIMEOUT, 4000).

-spec handle(binary(), map()) -> term().
handle(<<"guild.get_user_permissions">>, P) -> handle_get_user_permissions(P);
handle(<<"guild.get_user_permissions_batch">>, P) -> handle_get_user_permissions_batch(P);
handle(<<"guild.check_permission">>, P) -> handle_check_permission(P);
handle(<<"guild.can_manage_roles">>, P) -> handle_can_manage_roles(P);
handle(<<"guild.can_manage_role">>, P) -> handle_can_manage_role(P);
handle(<<"guild.get_assignable_roles">>, P) -> handle_get_assignable_roles(P);
handle(<<"guild.get_user_max_role_position">>, P) -> handle_get_max_role_position(P).

-spec handle_get_user_permissions(map()) -> map().
handle_get_user_permissions(
    #{<<"guild_id">> := GIB, <<"user_id">> := UIB, <<"channel_id">> := CIB}
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UIB),
    ChannelId = parse_channel_id(CIB),
    case get_permissions_cached_or_rpc(GuildId, UserId, ChannelId) of
        {ok, Perms} -> #{<<"permissions">> => integer_to_binary(Perms)};
        {error, guild_not_found} -> gateway_rpc_error:raise(<<"guild_not_found">>);
        error -> gateway_rpc_error:raise(<<"permissions_error">>)
    end.

-spec handle_get_user_permissions_batch(map()) -> map().
handle_get_user_permissions_batch(
    #{<<"guild_ids">> := GIBs, <<"user_id">> := UIB, <<"channel_id">> := CIB}
) ->
    GuildIds = validation:snowflake_list_or_throw(<<"guild_ids">>, GIBs),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UIB),
    ChannelId = parse_channel_id(CIB),
    UniqueGuildIds = lists:usort(GuildIds),
    gateway_rpc_guild_routing:validate_batch_size(length(UniqueGuildIds)),
    Results = gateway_rpc_guild_routing:process_batch(
        UniqueGuildIds,
        fun(GuildId) -> batch_permission_result(GuildId, UserId, ChannelId) end,
        5000
    ),
    #{<<"permissions">> => [R || R <- Results, is_map(R)]}.

-spec batch_permission_result(term(), integer(), integer() | undefined) -> map() | undefined.
batch_permission_result(GuildId, UserId, ChannelId) when is_integer(GuildId) ->
    case get_permissions_cached_or_rpc(GuildId, UserId, ChannelId) of
        {ok, Permissions} when is_integer(Permissions) ->
            #{
                <<"guild_id">> => integer_to_binary(GuildId),
                <<"permissions">> => integer_to_binary(Permissions)
            };
        _ ->
            undefined
    end;
batch_permission_result(_, _, _) ->
    undefined.

-spec handle_check_permission(map()) -> map().
handle_check_permission(
    #{
        <<"guild_id">> := GIB,
        <<"user_id">> := UIB,
        <<"permission">> := PB,
        <<"channel_id">> := CIB
    }
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UIB),
    Permission = permission_or_throw(PB),
    ChannelId = parse_channel_id(CIB),
    case get_permissions_cached_or_rpc(GuildId, UserId, ChannelId) of
        {ok, Perms} ->
            #{<<"has_permission">> => permission_bits:has(Perms, Permission)};
        {error, guild_not_found} ->
            gateway_rpc_error:raise(<<"guild_not_found">>);
        error ->
            gateway_rpc_error:raise(<<"permission_check_error">>)
    end.

-spec handle_can_manage_roles(map()) -> term().
handle_can_manage_roles(
    #{
        <<"guild_id">> := GIB,
        <<"user_id">> := UIB,
        <<"target_user_id">> := TIB,
        <<"role_id">> := RIB
    }
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UIB),
    TargetUserId = validation:snowflake_or_throw(<<"target_user_id">>, TIB),
    RoleId = validation:snowflake_or_throw(<<"role_id">>, RIB),
    Req = #{user_id => UserId, target_user_id => TargetUserId, role_id => RoleId},
    do_can_manage_roles(GuildId, Req).

-spec do_can_manage_roles(integer(), map()) -> term().
do_can_manage_roles(GuildId, Req) ->
    gateway_rpc_guild_infra:with_guild(GuildId, fun(Pid) ->
        guild_call_can_manage(Pid, {can_manage_roles, Req})
    end).

-spec handle_can_manage_role(map()) -> term().
handle_can_manage_role(
    #{<<"guild_id">> := GIB, <<"user_id">> := UIB, <<"role_id">> := RIB}
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UIB),
    RoleId = validation:snowflake_or_throw(<<"role_id">>, RIB),
    Req = #{user_id => UserId, role_id => RoleId},
    do_can_manage_role(GuildId, Req).

-spec do_can_manage_role(integer(), map()) -> term().
do_can_manage_role(GuildId, Req) ->
    gateway_rpc_guild_infra:with_guild(GuildId, fun(Pid) ->
        guild_call_can_manage(Pid, {can_manage_role, Req})
    end).

-spec guild_call_can_manage(pid(), term()) -> map().
guild_call_can_manage(Pid, Msg) ->
    case gen_server:call(Pid, Msg, ?GUILD_CALL_TIMEOUT) of
        #{can_manage := CM} ->
            #{<<"can_manage">> => CM};
        _ ->
            gateway_rpc_error:raise(<<"role_management_check_error">>)
    end.

-spec handle_get_assignable_roles(map()) -> term().
handle_get_assignable_roles(
    #{<<"guild_id">> := GIB, <<"user_id">> := UIB}
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UIB),
    do_get_assignable_roles(GuildId, UserId).

-spec do_get_assignable_roles(integer(), integer()) -> term().
do_get_assignable_roles(GuildId, UserId) ->
    gateway_rpc_guild_infra:with_guild(GuildId, fun(Pid) ->
        guild_call_assignable(Pid, UserId)
    end).

-spec guild_call_assignable(pid(), integer()) -> map().
guild_call_assignable(Pid, UserId) ->
    Msg = {get_assignable_roles, #{user_id => UserId}},
    case gen_server:call(Pid, Msg, ?GUILD_CALL_TIMEOUT) of
        #{role_ids := RoleIds} ->
            #{<<"role_ids">> => [integer_to_binary(R) || R <- RoleIds]};
        _ ->
            gateway_rpc_error:raise(<<"assignable_roles_error">>)
    end.

-spec handle_get_max_role_position(map()) -> term().
handle_get_max_role_position(
    #{<<"guild_id">> := GIB, <<"user_id">> := UIB}
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UIB),
    do_get_max_role_position(GuildId, UserId).

-spec do_get_max_role_position(integer(), integer()) -> term().
do_get_max_role_position(GuildId, UserId) ->
    gateway_rpc_guild_infra:with_guild(GuildId, fun(Pid) ->
        guild_call_max_pos(Pid, UserId)
    end).

-spec guild_call_max_pos(pid(), integer()) -> map().
guild_call_max_pos(Pid, UserId) ->
    Msg = {get_user_max_role_position, #{user_id => UserId}},
    case gen_server:call(Pid, Msg, ?GUILD_CALL_TIMEOUT) of
        #{position := Pos} ->
            #{<<"position">> => Pos};
        _ ->
            gateway_rpc_error:raise(<<"max_role_position_error">>)
    end.

-spec get_permissions_cached_or_rpc(
    integer(), integer(), integer() | undefined
) -> {ok, integer()} | {error, guild_not_found} | error.
get_permissions_cached_or_rpc(GuildId, UserId, ChannelId) ->
    case guild_permission_cache:get_permissions(GuildId, UserId, ChannelId) of
        {ok, Perms} -> {ok, Perms};
        {error, not_found} -> get_permissions_via_rpc(GuildId, UserId, ChannelId)
    end.

-spec get_permissions_via_rpc(
    integer(), integer(), integer() | undefined
) -> {ok, integer()} | {error, guild_not_found} | error.
get_permissions_via_rpc(GuildId, UserId, ChannelId) ->
    case gateway_rpc_guild_infra:ensure_guild_pid(GuildId) of
        {ok, Pid} ->
            Msg = {get_user_permissions, #{user_id => UserId, channel_id => ChannelId}},
            get_perms_from_guild(GuildId, Pid, Msg);
        error ->
            {error, guild_not_found}
    end.

-spec get_perms_from_guild(integer(), pid(), term()) -> {ok, integer()} | error.
get_perms_from_guild(GuildId, Pid, Msg) ->
    case
        gateway_rpc_guild_infra:safe_guild_call(
            GuildId, Pid, Msg, ?GUILD_CALL_TIMEOUT
        )
    of
        {ok, #{permissions := P}} when is_integer(P) -> {ok, P};
        _ -> error
    end.

-spec parse_channel_id(binary()) -> integer() | undefined.
parse_channel_id(<<"0">>) -> undefined;
parse_channel_id(CIB) -> validation:snowflake_or_throw(<<"channel_id">>, CIB).

-spec permission_or_throw(term()) -> non_neg_integer().
permission_or_throw(Value) ->
    try permission_bits:parse(Value) of
        Permission when is_integer(Permission) -> Permission
    catch
        error:{invalid_bitset, _} -> gateway_rpc_error:raise(<<"invalid_params">>)
    end.

-ifdef(TEST).
parse_channel_id_test() ->
    ?assertEqual(undefined, parse_channel_id(<<"0">>)).

permission_or_throw_accepts_zero_test() ->
    ?assertEqual(0, permission_or_throw(<<"0">>)).

permission_or_throw_rejects_malformed_test() ->
    ?assertError(
        {gateway_rpc_error, <<"invalid_params">>}, permission_or_throw(<<"bad">>)
    ).

check_permission_unknown_guild_raises_guild_not_found_test() ->
    Payload = #{
        <<"guild_id">> => <<"1">>,
        <<"user_id">> => <<"2">>,
        <<"permission">> => <<"1">>,
        <<"channel_id">> => <<"0">>
    },
    ?assertError(
        {gateway_rpc_error, <<"guild_not_found">>},
        handle(<<"guild.check_permission">>, Payload)
    ).

get_user_permissions_unknown_guild_raises_guild_not_found_test() ->
    Payload = #{
        <<"guild_id">> => <<"1">>,
        <<"user_id">> => <<"2">>,
        <<"channel_id">> => <<"0">>
    },
    ?assertError(
        {gateway_rpc_error, <<"guild_not_found">>},
        handle(<<"guild.get_user_permissions">>, Payload)
    ).

get_user_permissions_batch_rejects_oversized_test() ->
    GuildIds = [integer_to_binary(N) || N <- lists:seq(1, 101)],
    Payload = #{
        <<"guild_ids">> => GuildIds,
        <<"user_id">> => <<"1">>,
        <<"channel_id">> => <<"0">>
    },
    ?assertError(
        {gateway_rpc_error, <<"batch_too_large">>},
        handle(<<"guild.get_user_permissions_batch">>, Payload)
    ).
-endif.
