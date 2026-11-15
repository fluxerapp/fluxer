%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild_visitors).
-typing([eqwalizer]).

-export([handle/2]).

-define(GUILD_CALL_TIMEOUT, 5000).

-spec handle(binary(), map()) -> map().
handle(<<"guild.evict_visitors">>, P) -> handle_evict(P).

-spec handle_evict(map()) -> map().
handle_evict(#{<<"guild_id">> := GuildIdBin, <<"ip_keys">> := IpKeys}) when is_list(IpKeys) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case [Key || Key <- IpKeys, is_binary(Key), byte_size(Key) > 0] of
        [] -> #{<<"user_ids">> => []};
        Keys -> evict(GuildId, Keys)
    end;
handle_evict(_P) ->
    gateway_rpc_error:raise(<<"invalid_ip_keys">>).

-spec evict(integer(), [binary()]) -> map().
evict(GuildId, Keys) ->
    Result =
        case gateway_rpc_guild_infra:get_guild_pid(GuildId) of
            {ok, Pid} -> {reply, safe_call(GuildId, Pid, {evict_visitors, Keys})};
            error -> not_running
        end,
    case Result of
        {reply, {ok, UserIds}} when is_list(UserIds) ->
            #{<<"user_ids">> => [integer_to_binary(Id) || Id <- UserIds, is_integer(Id)]};
        not_running ->
            #{<<"user_ids">> => []};
        _ ->
            gateway_rpc_error:raise(<<"guild_visitors_error">>)
    end.

-spec safe_call(integer(), pid(), term()) -> term().
safe_call(GuildId, Pid, Msg) ->
    try
        gen_server:call(Pid, Msg, ?GUILD_CALL_TIMEOUT)
    catch
        exit:{noproc, _} -> error;
        exit:{timeout, _} -> call_failed(GuildId, timeout);
        exit:{Reason, {gen_server, call, _}} -> call_failed(GuildId, Reason);
        exit:Reason -> call_failed(GuildId, Reason)
    end.

-spec call_failed(integer(), term()) -> error.
call_failed(GuildId, Reason) ->
    logger:warning("guild_visitor_eviction_failed: guild_id=~p reason=~p", [GuildId, Reason]),
    error.
