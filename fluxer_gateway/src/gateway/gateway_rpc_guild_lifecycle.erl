%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild_lifecycle).
-typing([eqwalizer]).

-export([handle/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(GUILD_CALL_TIMEOUT, 4000).
-define(RELOAD_ALL_TIMEOUT, 15000).

-spec handle(binary(), map()) -> term().
handle(<<"guild.dispatch">>, P) -> handle_dispatch(P);
handle(<<"guild.get_data">>, P) -> handle_get_data(P);
handle(<<"guild.get_auth_context">>, P) -> handle_get_auth_context(P);
handle(<<"guild.start">>, P) -> handle_start(P);
handle(<<"guild.stop">>, P) -> handle_stop(P);
handle(<<"guild.reload">>, P) -> handle_reload(P);
handle(<<"guild.reload_all">>, P) -> handle_reload_all(P);
handle(<<"guild.shutdown">>, P) -> handle_shutdown(P).

-spec handle_dispatch(map()) -> term().
handle_dispatch(#{<<"guild_id">> := GuildIdBin, <<"event">> := Event, <<"data">> := Data}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    gateway_rpc_guild_infra:with_guild(GuildId, fun(Pid) ->
        EventAtom = constants:dispatch_event_atom(Event),
        IsAlive = gateway_rpc_guild_infra:is_cached_guild_pid_alive(Pid),
        logger:debug(
            "rpc guild.dispatch: guild_id=~p event=~p pid=~p alive=~p",
            [GuildId, EventAtom, Pid, IsAlive]
        ),
        gen_server:cast(Pid, {dispatch, #{event => EventAtom, data => Data}}),
        true
    end).

-spec handle_get_data(map()) -> term().
handle_get_data(#{<<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = optional_user_id(UserIdBin),
    gateway_rpc_guild_infra:with_guild(
        GuildId,
        fun(Pid) ->
            get_data_from_guild(Pid, UserId)
        end,
        <<"guild_not_found">>
    ).

-spec handle_get_auth_context(map()) -> term().
handle_get_auth_context(#{<<"guild_id">> := GuildIdBin, <<"user_id">> := UserIdBin} = Params) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    UserId = optional_user_id(UserIdBin),
    ChannelId = optional_channel_id(maps:get(<<"channel_id">>, Params, null)),
    gateway_rpc_guild_infra:with_guild(
        GuildId,
        fun(Pid) ->
            get_auth_context_from_guild(Pid, UserId, ChannelId)
        end,
        <<"guild_not_found">>
    ).

-spec optional_channel_id(term()) -> integer() | null.
optional_channel_id(Value) ->
    case validation:validate_optional_snowflake(Value) of
        {ok, null} -> null;
        {ok, ChannelId} when is_integer(ChannelId) -> ChannelId;
        _ -> gateway_rpc_error:raise(validation_invalid_params)
    end.

-spec get_auth_context_from_guild(pid(), integer() | null, integer() | null) -> term().
get_auth_context_from_guild(Pid, UserId, ChannelId) ->
    Request = {get_guild_auth_context, #{user_id => UserId, channel_id => ChannelId}},
    case gen_server:call(Pid, Request, ?GUILD_CALL_TIMEOUT) of
        #{auth_context := null} ->
            gateway_rpc_error:raise(<<"forbidden">>);
        #{auth_context := AuthContext} ->
            AuthContext;
        _ ->
            gateway_rpc_error:raise(<<"guild_data_error">>)
    end.

-spec optional_user_id(term()) -> integer() | null.
optional_user_id(Value) ->
    case validation:validate_optional_snowflake(Value) of
        {ok, null} -> null;
        {ok, UserId} when is_integer(UserId) -> UserId;
        _ -> gateway_rpc_error:raise(validation_invalid_params)
    end.

-spec get_data_from_guild(pid(), integer() | null) -> term().
get_data_from_guild(Pid, UserId) ->
    case gen_server:call(Pid, {get_guild_data, #{user_id => UserId}}, ?GUILD_CALL_TIMEOUT) of
        #{guild_data := null, error_reason := <<"forbidden">>} ->
            gateway_rpc_error:raise(<<"forbidden">>);
        #{guild_data := null} ->
            gateway_rpc_error:raise(<<"forbidden">>);
        #{guild_data := GuildData} ->
            guild_data_wire:payload(GuildData);
        _ ->
            gateway_rpc_error:raise(<<"guild_data_error">>)
    end.

-spec handle_start(map()) -> true.
handle_start(#{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gateway_rpc_guild_infra:get_guild_pid_with_retry(GuildId) of
        {ok, _Pid} -> true;
        error -> gateway_rpc_error:raise(<<"guild_start_error">>)
    end.

-spec handle_stop(map()) -> true.
handle_stop(#{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    Request = {stop_guild, GuildId},
    case
        gateway_rpc_guild_routing:call_owner_guild_manager(
            GuildId, Request, ?GUILD_CALL_TIMEOUT
        )
    of
        ok -> true;
        _ -> gateway_rpc_error:raise(<<"guild_stop_error">>)
    end.

-spec handle_reload(map()) -> true.
handle_reload(#{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    Request = {reload_guild, GuildId},
    case
        gateway_rpc_guild_routing:call_owner_guild_manager(
            GuildId, Request, ?GUILD_CALL_TIMEOUT
        )
    of
        ok -> true;
        {error, not_found} -> reload_via_start(GuildId);
        _ -> gateway_rpc_error:raise(<<"guild_reload_error">>)
    end.

-spec reload_via_start(integer()) -> true.
reload_via_start(GuildId) ->
    Request = {start_or_lookup, GuildId},
    case gateway_rpc_guild_routing:call_owner_guild_manager(GuildId, Request, 20000) of
        {ok, _Pid} -> true;
        _ -> gateway_rpc_error:raise(<<"guild_reload_error">>)
    end.

-spec handle_reload_all(map()) -> #{binary() => non_neg_integer()}.
handle_reload_all(#{<<"guild_ids">> := GuildIdsBin}) ->
    GuildIds = validation:snowflake_list_or_throw(<<"guild_ids">>, GuildIdsBin),
    Count = reload_all_guilds_by_owner(GuildIds),
    #{<<"count">> => Count}.

-spec handle_shutdown(map()) -> true.
handle_shutdown(#{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    Request = {shutdown_guild, GuildId},
    case
        gateway_rpc_guild_routing:call_owner_guild_manager(
            GuildId, Request, ?GUILD_CALL_TIMEOUT
        )
    of
        ok -> true;
        {error, timeout} -> shutdown_via_stop(GuildId);
        _ -> gateway_rpc_error:raise(<<"guild_shutdown_error">>)
    end.

-spec shutdown_via_stop(integer()) -> true.
shutdown_via_stop(GuildId) ->
    Request = {stop_guild, GuildId},
    case
        gateway_rpc_guild_routing:call_owner_guild_manager(
            GuildId, Request, ?GUILD_CALL_TIMEOUT
        )
    of
        ok -> true;
        _ -> gateway_rpc_error:raise(<<"guild_shutdown_error">>)
    end.

-spec reload_all_guilds_by_owner([integer()]) -> non_neg_integer().
reload_all_guilds_by_owner(GuildIds) ->
    OwnerGroups = gateway_rpc_guild_routing:owner_groups_for_reload_all(GuildIds),
    Counts = gateway_rpc_guild_routing:process_batch(
        OwnerGroups,
        fun reload_owner_group_item/1,
        ?RELOAD_ALL_TIMEOUT
    ),
    lists:foldl(
        fun
            (Count, Acc) when is_integer(Count), Count >= 0 -> Acc + Count;
            (_, Acc) -> Acc
        end,
        0,
        Counts
    ).

-spec reload_owner_group_item(term()) -> non_neg_integer().
reload_owner_group_item({Node, GuildIds}) when is_atom(Node), is_list(GuildIds) ->
    reload_owner_group({Node, integer_entries(GuildIds)});
reload_owner_group_item(_) ->
    0.

-spec integer_entries([term()]) -> [integer()].
integer_entries(Values) ->
    [Value || Value <- Values, is_integer(Value)].

-spec reload_owner_group({node(), [integer()]}) -> non_neg_integer().
reload_owner_group({OwnerNode, OwnerGuildIds}) ->
    Ref = owner_guild_manager_ref(OwnerNode),
    try gen_server:call(Ref, {reload_all_guilds, OwnerGuildIds}, ?RELOAD_ALL_TIMEOUT) of
        #{count := C} when is_integer(C), C >= 0 -> C;
        _ -> 0
    catch
        throw:_Reason -> 0;
        error:_Reason -> 0;
        exit:_Reason -> 0
    end.

-spec owner_guild_manager_ref(node()) -> guild_manager | {guild_manager, node()}.
owner_guild_manager_ref(OwnerNode) when OwnerNode =:= node() ->
    guild_manager;
owner_guild_manager_ref(OwnerNode) ->
    {guild_manager, OwnerNode}.

-ifdef(TEST).
optional_user_id_preserves_null_for_skip_membership_check_test() ->
    ?assertEqual(null, optional_user_id(null)).

optional_user_id_accepts_snowflake_test() ->
    ?assertEqual(123, optional_user_id(<<"123">>)).

optional_channel_id_defaults_to_null_test() ->
    ?assertEqual(null, optional_channel_id(null)).

optional_channel_id_accepts_snowflake_test() ->
    ?assertEqual(456, optional_channel_id(<<"456">>)).

optional_channel_id_rejects_garbage_test() ->
    ?assertError({gateway_rpc_error, _}, optional_channel_id(<<"nope">>)).
-endif.
