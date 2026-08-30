%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild).

-typing([eqwalizer]).

-export([execute_method/2, execute_method_local/2, batch_lookup_guild_pids/1]).

-define(REMOTE_EXECUTE_TIMEOUT, 15000).

-spec execute_method(binary(), map()) -> term().
execute_method(Method, Params) ->
    case maybe_forward_to_owner(Method, Params) of
        local ->
            execute_method_local(Method, Params);
        {forwarded, Result} ->
            Result;
        {forward_failed, Reason} ->
            logger:warning(
                "gateway_rpc_guild owner forward failed: method=~ts reason=~p",
                [Method, Reason]
            ),
            gateway_rpc_error:raise(<<"timeout">>)
    end.

-spec execute_method_local(binary(), map()) -> term().
execute_method_local(M, P) -> route(M, P).

-spec batch_lookup_guild_pids([integer()]) -> ok.
batch_lookup_guild_pids(GuildIds) -> gateway_rpc_guild_infra:batch_lookup_guild_pids(GuildIds).

-spec route(binary(), map()) -> term().
route(M, P) ->
    Handler = route_handler(M),
    Handler(M, P).

-define(HANDLER_MAP, #{
    <<"guild.dispatch">> => fun gateway_rpc_guild_lifecycle:handle/2,
    <<"guild.get_data">> => fun gateway_rpc_guild_lifecycle:handle/2,
    <<"guild.get_auth_context">> => fun gateway_rpc_guild_lifecycle:handle/2,
    <<"guild.start">> => fun gateway_rpc_guild_lifecycle:handle/2,
    <<"guild.stop">> => fun gateway_rpc_guild_lifecycle:handle/2,
    <<"guild.reload">> => fun gateway_rpc_guild_lifecycle:handle/2,
    <<"guild.reload_all">> => fun gateway_rpc_guild_lifecycle:handle/2,
    <<"guild.shutdown">> => fun gateway_rpc_guild_lifecycle:handle/2,
    <<"guild.get_counts">> => fun gateway_rpc_guild_counts:handle/2,
    <<"guild.get_online_counts_batch">> => fun gateway_rpc_guild_counts:handle/2,
    <<"guild.get_member">> => fun gateway_rpc_guild_members:handle/2,
    <<"guild.has_member">> => fun gateway_rpc_guild_members:handle/2,
    <<"guild.list_members">> => fun gateway_rpc_guild_members:handle/2,
    <<"guild.list_members_cursor">> => fun gateway_rpc_guild_members:handle/2,
    <<"guild.get_members_with_role">> => fun gateway_rpc_guild_members:handle/2,
    <<"guild.check_target_member">> => fun gateway_rpc_guild_members:handle/2,
    <<"guild.get_user_permissions">> => fun gateway_rpc_guild_permissions:handle/2,
    <<"guild.get_user_permissions_batch">> => fun gateway_rpc_guild_permissions:handle/2,
    <<"guild.check_permission">> => fun gateway_rpc_guild_permissions:handle/2,
    <<"guild.can_manage_roles">> => fun gateway_rpc_guild_permissions:handle/2,
    <<"guild.can_manage_role">> => fun gateway_rpc_guild_permissions:handle/2,
    <<"guild.get_assignable_roles">> => fun gateway_rpc_guild_permissions:handle/2,
    <<"guild.get_user_max_role_position">> => fun gateway_rpc_guild_permissions:handle/2,
    <<"guild.get_viewable_channels">> => fun gateway_rpc_guild_channels:handle/2,
    <<"guild.resolve_channel_mentions">> => fun gateway_rpc_guild_channels:handle/2,
    <<"guild.get_vanity_url_channel">> => fun gateway_rpc_guild_channels:handle/2,
    <<"guild.get_first_viewable_text_channel">> => fun gateway_rpc_guild_channels:handle/2,
    <<"guild.get_category_channel_count">> => fun gateway_rpc_guild_channels:handle/2,
    <<"guild.get_channel_count">> => fun gateway_rpc_guild_channels:handle/2,
    <<"guild.get_users_to_mention_by_roles">> => fun gateway_rpc_guild_mentions:handle/2,
    <<"guild.get_users_to_mention_by_user_ids">> => fun gateway_rpc_guild_mentions:handle/2,
    <<"guild.get_all_users_to_mention">> => fun gateway_rpc_guild_mentions:handle/2,
    <<"guild.resolve_all_mentions">> => fun gateway_rpc_guild_mentions:handle/2,
    <<"guild.resolve_mention_sources">> => fun gateway_rpc_guild_mentions:handle/2,
    <<"guild.resolve_mention_sources_page">> => fun gateway_rpc_guild_mentions:handle/2,
    <<"guild.update_member_voice">> => fun gateway_rpc_guild_voice:handle/2,
    <<"guild.disconnect_voice_user">> => fun gateway_rpc_guild_voice:handle/2,
    <<"guild.disconnect_voice_user_if_in_channel">> => fun gateway_rpc_guild_voice:handle/2,
    <<"guild.disconnect_all_voice_users_in_channel">> => fun gateway_rpc_guild_voice:handle/2,
    <<"guild.confirm_voice_connection_from_livekit">> => fun gateway_rpc_guild_voice:handle/2,
    <<"guild.repair_voice_state_from_cache">> => fun gateway_rpc_guild_voice:handle/2,
    <<"guild.get_voice_states_for_channel">> => fun gateway_rpc_guild_voice:handle/2,
    <<"guild.get_pending_joins_for_channel">> => fun gateway_rpc_guild_voice:handle/2,
    <<"guild.move_member">> => fun gateway_rpc_guild_voice:handle/2,
    <<"guild.get_voice_state">> => fun gateway_rpc_guild_voice:handle/2,
    <<"guild.switch_voice_region">> => fun gateway_rpc_guild_voice:handle/2,
    <<"guild.batch_voice_state_update">> => fun gateway_rpc_guild_voice:handle/2
}).

-spec route_handler(binary()) -> fun((binary(), map()) -> term()).
route_handler(Method) -> maps:get(Method, ?HANDLER_MAP).

-spec maybe_forward_to_owner(binary(), map()) ->
    local | {forwarded, term()} | {forward_failed, term()}.
maybe_forward_to_owner(Method, Params) ->
    case extract_guild_id(Params) of
        {ok, GuildId} ->
            OwnerScope = gateway_rpc_guild_routing:guild_owner_scope(GuildId),
            maybe_forward_owner_scope(OwnerScope, Method, Params);
        error ->
            local
    end.

-spec maybe_forward_owner_scope(local | {remote, node()} | unavailable, binary(), map()) ->
    local | {forwarded, term()} | {forward_failed, term()}.
maybe_forward_owner_scope(local, _Method, _Params) ->
    local;
maybe_forward_owner_scope(unavailable, _Method, _Params) ->
    {forward_failed, unavailable};
maybe_forward_owner_scope({remote, OwnerNode}, Method, Params) ->
    forward_to_remote(OwnerNode, Method, Params).

-spec forward_to_remote(node(), binary(), map()) ->
    {forwarded, term()} | {forward_failed, term()}.
forward_to_remote(OwnerNode, Method, Params) ->
    Args = [Method, Params],
    try
        erpc:call(
            OwnerNode,
            gateway_rpc_guild,
            execute_method_local,
            Args,
            ?REMOTE_EXECUTE_TIMEOUT
        )
    of
        Result -> {forwarded, Result}
    catch
        throw:{error, Msg} -> gateway_rpc_error:raise(Msg);
        throw:Msg when is_binary(Msg) -> gateway_rpc_error:raise(Msg);
        exit:timeout -> gateway_rpc_error:raise(<<"timeout">>);
        exit:{timeout, _} -> gateway_rpc_error:raise(<<"timeout">>);
        error:{erpc, Reason} -> {forward_failed, {erpc, Reason}};
        error:{gateway_rpc_error, Msg} -> gateway_rpc_error:raise(Msg);
        error:{exception, throw, {error, Msg}, _} -> gateway_rpc_error:raise(Msg);
        error:{exception, throw, Msg, _} when is_binary(Msg) -> gateway_rpc_error:raise(Msg);
        error:{exception, {gateway_rpc_error, Msg}, _} -> gateway_rpc_error:raise(Msg);
        error:{exception, error, {gateway_rpc_error, Msg}, _} -> gateway_rpc_error:raise(Msg);
        error:{exception, exit, {timeout, _}, _} -> gateway_rpc_error:raise(<<"timeout">>);
        error:{exception, Class, Reason, _} -> {forward_failed, {exception, Class, Reason}};
        throw:Reason -> {forward_failed, {throw, Reason}};
        error:Reason -> {forward_failed, {error, Reason}};
        exit:Reason -> {forward_failed, {exit, Reason}}
    end.

-spec extract_guild_id(map()) -> {ok, integer()} | error.
extract_guild_id(#{<<"guild_id">> := GuildIdBin}) ->
    try validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin) of
        GuildId -> decode_guild_id(GuildId)
    catch
        throw:_Reason -> error;
        error:_Reason -> error;
        exit:_Reason -> error
    end;
extract_guild_id(_) ->
    error.

-spec decode_guild_id(integer()) -> {ok, integer()}.
decode_guild_id(GuildId) ->
    {ok, GuildId}.
