%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild_members).

-typing([eqwalizer]).

-export([handle/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(GUILD_CALL_TIMEOUT, 4000).

-spec handle(binary(), map()) -> term().
handle(<<"guild.get_member">>, P) -> handle_get_member(P);
handle(<<"guild.has_member">>, P) -> handle_has_member(P);
handle(<<"guild.list_members">>, P) -> handle_list_members(P);
handle(<<"guild.list_members_cursor">>, P) -> handle_list_members_cursor(P);
handle(<<"guild.get_members_with_role">>, P) -> handle_get_members_with_role(P);
handle(<<"guild.check_target_member">>, P) -> handle_check_target_member(P).

-spec handle_get_member(map()) -> map().
handle_get_member(#{<<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case get_member_cached_or_rpc(GuildId, UserId) of
        {ok, MemberData} when is_map(MemberData) ->
            #{<<"success">> => true, <<"member_data">> => guild_data_wire:payload(MemberData)};
        {ok, undefined} ->
            #{<<"success">> => false};
        {error, guild_not_found} ->
            gateway_rpc_error:raise(<<"guild_not_found">>);
        error ->
            gateway_rpc_error:raise(<<"guild_member_error">>)
    end.

-spec handle_has_member(map()) -> map().
handle_has_member(#{<<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case get_has_member_cached_or_rpc(GuildId, UserId) of
        {ok, HasMember} -> #{<<"has_member">> => HasMember};
        {error, guild_not_found} -> gateway_rpc_error:raise(<<"guild_not_found">>);
        error -> gateway_rpc_error:raise(<<"membership_check_error">>)
    end.

-spec handle_list_members(map()) -> term().
handle_list_members(
    #{<<"guild_id">> := GuildIdBin, <<"limit">> := Limit, <<"offset">> := Offset}
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    Msg = {list_guild_members, #{limit => Limit, offset => Offset}},
    do_list_members(GuildId, Msg).

-spec do_list_members(integer(), term()) -> term().
do_list_members(GuildId, Msg) ->
    gateway_rpc_guild_infra:with_guild(GuildId, fun(Pid) ->
        guild_call_members(Pid, Msg)
    end).

-spec guild_call_members(pid(), term()) -> map().
guild_call_members(Pid, Msg) ->
    case gen_server:call(Pid, Msg, ?GUILD_CALL_TIMEOUT) of
        #{members := Members, total := Total} ->
            #{<<"members">> => guild_data_wire:payload(Members), <<"total">> => Total};
        _ ->
            gateway_rpc_error:raise(<<"guild_members_error">>)
    end.

-spec handle_list_members_cursor(map()) -> term().
handle_list_members_cursor(Request) ->
    GuildIdBin = maps:get(<<"guild_id">>, Request, undefined),
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    Limit = clamp_limit(maps:get(<<"limit">>, Request, 1)),
    AfterId = parse_optional_snowflake(
        maps:get(<<"after">>, Request, undefined), <<"after">>
    ),
    Msg = {list_guild_members_cursor, #{<<"limit">> => Limit, <<"after">> => AfterId}},
    do_list_members(GuildId, Msg).

-spec handle_get_members_with_role(map()) -> map().
handle_get_members_with_role(
    #{<<"guild_id">> := GuildIdBin, <<"role_id">> := RoleIdBin}
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    RoleId = validation:snowflake_or_throw(<<"role_id">>, RoleIdBin),
    case get_members_with_role_cached_or_rpc(GuildId, RoleId) of
        {ok, UserIds} ->
            #{<<"user_ids">> => [integer_to_binary(U) || U <- UserIds]};
        error ->
            gateway_rpc_error:raise(<<"members_with_role_error">>)
    end.

-spec handle_check_target_member(map()) -> term().
handle_check_target_member(
    #{
        <<"guild_id">> := GuildIdBin,
        <<"user_id">> := UserIdBin,
        <<"target_user_id">> := TargetUserIdBin
    }
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    TUserId = validation:snowflake_or_throw(<<"target_user_id">>, TargetUserIdBin),
    Req = #{user_id => UserId, target_user_id => TUserId},
    do_check_target_member(GuildId, Req).

-spec do_check_target_member(integer(), map()) -> term().
do_check_target_member(GuildId, Req) ->
    gateway_rpc_guild_infra:with_guild(GuildId, fun(Pid) ->
        guild_call_check_target(Pid, Req)
    end).

-spec guild_call_check_target(pid(), map()) -> map().
guild_call_check_target(Pid, Req) ->
    case gen_server:call(Pid, {check_target_member, Req}, ?GUILD_CALL_TIMEOUT) of
        #{can_manage := CanManage} ->
            #{<<"can_manage">> => CanManage};
        _ ->
            gateway_rpc_error:raise(<<"target_member_check_error">>)
    end.

-spec get_has_member_cached_or_rpc(
    integer(), integer()
) -> {ok, boolean()} | {error, guild_not_found} | error.
get_has_member_cached_or_rpc(GuildId, UserId) ->
    case guild_permission_cache:has_member(GuildId, UserId) of
        {ok, HasMember} -> {ok, HasMember};
        {error, not_found} -> get_has_member_via_rpc(GuildId, UserId)
    end.

-spec get_has_member_via_rpc(
    integer(), integer()
) -> {ok, boolean()} | {error, guild_not_found} | error.
get_has_member_via_rpc(GuildId, UserId) ->
    case gateway_rpc_guild_infra:ensure_guild_pid(GuildId) of
        {ok, Pid} ->
            Msg = {has_member, #{user_id => UserId}},
            has_member_from_guild(GuildId, Pid, Msg);
        error ->
            {error, guild_not_found}
    end.

-spec has_member_from_guild(integer(), pid(), term()) -> {ok, boolean()} | error.
has_member_from_guild(GuildId, Pid, Msg) ->
    case
        gateway_rpc_guild_infra:safe_guild_call(
            GuildId, Pid, Msg, ?GUILD_CALL_TIMEOUT
        )
    of
        {ok, #{has_member := HM}} when is_boolean(HM) -> {ok, HM};
        _ -> error
    end.

-spec get_member_cached_or_rpc(
    integer(), integer()
) -> {ok, map() | undefined} | {error, guild_not_found} | error.
get_member_cached_or_rpc(GuildId, UserId) ->
    case guild_permission_cache:get_member(GuildId, UserId) of
        {ok, MemberData} when is_map(MemberData) ->
            maybe_refresh_member(GuildId, UserId, MemberData);
        {ok, MemberOrUndefined} ->
            {ok, MemberOrUndefined};
        {error, not_found} ->
            get_member_via_rpc(GuildId, UserId)
    end.

-spec maybe_refresh_member(integer(), integer(), map()) ->
    {ok, map() | undefined} | {error, guild_not_found} | error.
maybe_refresh_member(GuildId, UserId, MemberData) ->
    Key = <<"communication_disabled_until">>,
    case maps:is_key(Key, MemberData) of
        true -> {ok, MemberData};
        false -> get_member_via_rpc(GuildId, UserId)
    end.

-spec get_member_via_rpc(
    integer(), integer()
) -> {ok, map() | undefined} | {error, guild_not_found} | error.
get_member_via_rpc(GuildId, UserId) ->
    case gateway_rpc_guild_infra:ensure_guild_pid(GuildId) of
        {ok, Pid} ->
            Msg = {get_guild_member, #{user_id => UserId}},
            member_from_guild(GuildId, Pid, Msg);
        error ->
            {error, guild_not_found}
    end.

-spec member_from_guild(integer(), pid(), term()) ->
    {ok, map() | undefined} | error.
member_from_guild(GuildId, Pid, Msg) ->
    case
        gateway_rpc_guild_infra:safe_guild_call(
            GuildId, Pid, Msg, ?GUILD_CALL_TIMEOUT
        )
    of
        {ok, #{success := true, member_data := MD}} when is_map(MD) ->
            {ok, MD};
        {ok, #{success := false}} ->
            {ok, undefined};
        _ ->
            error
    end.

-spec get_members_with_role_cached_or_rpc(
    integer(), integer()
) -> {ok, [integer()]} | error.
get_members_with_role_cached_or_rpc(GuildId, RoleId) ->
    case guild_permission_cache:get_role_members(GuildId, RoleId) of
        {ok, UserIds} ->
            {ok, UserIds};
        {error, not_found} ->
            get_members_with_role_via_rpc(GuildId, RoleId)
    end.

-spec get_members_with_role_via_rpc(
    integer(), integer()
) -> {ok, [integer()]} | error.
get_members_with_role_via_rpc(GuildId, RoleId) ->
    case gateway_rpc_guild_infra:ensure_guild_pid(GuildId) of
        {ok, Pid} ->
            Msg = {get_members_with_role, #{role_id => RoleId}},
            role_members_from_guild(GuildId, Pid, Msg);
        error ->
            error
    end.

-spec role_members_from_guild(integer(), pid(), term()) -> {ok, [integer()]} | error.
role_members_from_guild(GuildId, Pid, Msg) ->
    case
        gateway_rpc_guild_infra:safe_guild_call(
            GuildId, Pid, Msg, ?GUILD_CALL_TIMEOUT
        )
    of
        {ok, #{user_ids := UserIds}} when is_list(UserIds) ->
            {ok, integer_entries(UserIds)};
        _ ->
            error
    end.

-spec integer_entries([term()]) -> [integer()].
integer_entries(Values) ->
    [Value || Value <- Values, is_integer(Value)].

-spec clamp_limit(term()) -> pos_integer().
clamp_limit(Value) ->
    case type_conv:to_integer(Value) of
        undefined -> 1;
        N when N < 1 -> 1;
        N when N > 1000 -> 1000;
        N when is_integer(N) -> N
    end.

-spec parse_optional_snowflake(term(), binary()) -> integer() | undefined.
parse_optional_snowflake(undefined, _) -> undefined;
parse_optional_snowflake(Value, Field) -> validation:snowflake_or_throw(Field, Value).

-ifdef(TEST).

clamp_limit_test() ->
    ?assertEqual(1, clamp_limit(undefined)),
    ?assertEqual(1, clamp_limit(0)),
    ?assertEqual(50, clamp_limit(50)),
    ?assertEqual(1000, clamp_limit(2000)).

-endif.
