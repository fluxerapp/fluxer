%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild_voice).

-typing([eqwalizer]).

-export([handle/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(GUILD_CALL_TIMEOUT, 4000).
-define(BATCH_TIMEOUT_MS, 5000).

-spec handle(binary(), map()) -> term().
handle(<<"guild.update_member_voice">>, P) ->
    handle_update_member_voice(P);
handle(<<"guild.disconnect_voice_user">>, P) ->
    handle_disconnect_voice_user(P);
handle(<<"guild.disconnect_voice_user_if_in_channel">>, P) ->
    handle_disconnect_if_in_channel(P);
handle(<<"guild.disconnect_all_voice_users_in_channel">>, P) ->
    handle_disconnect_all_in_channel(P);
handle(<<"guild.confirm_voice_connection_from_livekit">>, P) ->
    handle_confirm_connection(P);
handle(<<"guild.get_voice_states_for_channel">>, P) ->
    handle_get_voice_states(P);
handle(<<"guild.get_pending_joins_for_channel">>, P) ->
    handle_get_pending_joins(P);
handle(<<"guild.move_member">>, P) ->
    handle_move_member(P);
handle(<<"guild.get_voice_state">>, P) ->
    handle_get_voice_state(P);
handle(<<"guild.switch_voice_region">>, P) ->
    handle_switch_voice_region(P);
handle(<<"guild.batch_voice_state_update">>, P) ->
    handle_batch_update(P).

-spec handle_update_member_voice(map()) -> term().
handle_update_member_voice(#{
    <<"guild_id">> := GIB,
    <<"user_id">> := UIB,
    <<"mute">> := Mute,
    <<"deaf">> := Deaf
}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UIB),
    Request = #{user_id => UserId, mute => Mute, deaf => Deaf},
    gateway_rpc_guild_infra:with_voice_server(GuildId, fun(VPid, _) ->
        call_update_member_voice(VPid, Request)
    end).

-spec call_update_member_voice(pid(), map()) -> map().
call_update_member_voice(VPid, Request) ->
    case gen_server:call(VPid, {update_member_voice, Request}, ?GUILD_CALL_TIMEOUT) of
        #{success := true} -> #{<<"success">> => true};
        #{error := E} -> raise_voice_error(E)
    end.

-spec handle_disconnect_voice_user(map()) -> term().
handle_disconnect_voice_user(#{<<"guild_id">> := GIB, <<"user_id">> := UIB} = P) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UIB),
    Request = #{user_id => UserId, connection_id => maps:get(<<"connection_id">>, P, null)},
    gateway_rpc_guild_infra:with_voice_server(GuildId, fun(VPid, _) ->
        call_disconnect_voice_user(VPid, Request)
    end).

-spec call_disconnect_voice_user(pid(), map()) -> map().
call_disconnect_voice_user(VPid, Request) ->
    case gen_server:call(VPid, {disconnect_voice_user, Request}, ?GUILD_CALL_TIMEOUT) of
        #{success := true} -> #{<<"success">> => true};
        #{error := E} -> raise_voice_error(E)
    end.

-spec handle_disconnect_if_in_channel(map()) -> term().
handle_disconnect_if_in_channel(
    #{
        <<"guild_id">> := GIB,
        <<"user_id">> := UIB,
        <<"expected_channel_id">> := ECIB
    } = P
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UIB),
    ExpCId = validation:snowflake_or_throw(<<"expected_channel_id">>, ECIB),
    Req = build_disconnect_if_in_channel_req(
        UserId,
        ExpCId,
        maps:get(<<"connection_id">>, P, undefined)
    ),
    gateway_rpc_guild_infra:with_voice_server(GuildId, fun(VPid, _) ->
        call_disconnect_if_in_channel(VPid, Req)
    end).

-spec call_disconnect_if_in_channel(pid(), map()) -> map().
call_disconnect_if_in_channel(VPid, Request) ->
    Result = gen_server:call(
        VPid,
        {disconnect_voice_user_if_in_channel, Request},
        ?GUILD_CALL_TIMEOUT
    ),
    format_disconnect_if_in_channel_result(Result).

-spec format_disconnect_if_in_channel_result(term()) -> map().
format_disconnect_if_in_channel_result(#{success := true, ignored := true}) ->
    #{<<"success">> => true, <<"ignored">> => true};
format_disconnect_if_in_channel_result(#{success := true}) ->
    #{<<"success">> => true};
format_disconnect_if_in_channel_result(#{error := E}) ->
    raise_voice_error(E).

-spec build_disconnect_if_in_channel_req(integer(), integer(), term()) -> map().
build_disconnect_if_in_channel_req(UserId, ExpCId, undefined) ->
    #{user_id => UserId, expected_channel_id => ExpCId};
build_disconnect_if_in_channel_req(UserId, ExpCId, ConnId) ->
    #{user_id => UserId, expected_channel_id => ExpCId, connection_id => ConnId}.

-spec handle_disconnect_all_in_channel(map()) -> term().
handle_disconnect_all_in_channel(#{<<"guild_id">> := GIB, <<"channel_id">> := CIB}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, CIB),
    gateway_rpc_guild_infra:with_voice_server(GuildId, fun(VPid, _) ->
        call_disconnect_all_in_channel(VPid, ChannelId)
    end).

-spec call_disconnect_all_in_channel(pid(), integer()) -> map().
call_disconnect_all_in_channel(VPid, ChannelId) ->
    Request = {disconnect_all_voice_users_in_channel, #{channel_id => ChannelId}},
    case gen_server:call(VPid, Request, ?GUILD_CALL_TIMEOUT) of
        #{success := true, disconnected_count := Cnt} ->
            #{<<"success">> => true, <<"disconnected_count">> => Cnt};
        #{error := E} ->
            raise_voice_error(E)
    end.

-spec handle_confirm_connection(map()) -> term().
handle_confirm_connection(P) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, maps:get(<<"guild_id">>, P)),
    gateway_rpc_guild_infra:with_voice_server(GuildId, fun(VPid, _) ->
        confirm_voice_connection(VPid, P)
    end).

-spec confirm_voice_connection(pid(), map()) -> map().
confirm_voice_connection(VPid, P) ->
    Req = #{
        connection_id => maps:get(<<"connection_id">>, P),
        token_nonce => maps:get(<<"token_nonce">>, P, undefined)
    },
    Result = gen_server:call(
        VPid,
        {confirm_voice_connection_from_livekit, Req},
        ?GUILD_CALL_TIMEOUT
    ),
    format_confirm_result(Result).

-spec format_confirm_result(term()) -> map().
format_confirm_result(#{success := true}) ->
    #{<<"success">> => true};
format_confirm_result(#{success := false, error := E}) ->
    #{<<"success">> => false, <<"error">> => normalize_voice_rpc_error(E)};
format_confirm_result({error, _, EA}) ->
    #{<<"success">> => false, <<"error">> => normalize_voice_rpc_error(EA)};
format_confirm_result(#{error := E}) ->
    raise_voice_error(E).

-spec handle_get_voice_states(map()) -> term().
handle_get_voice_states(P) ->
    case validate_gc_params(P) of
        {ok, GId, CIdBin} -> get_voice_states_for_guild(GId, CIdBin);
        error -> #{<<"voice_states">> => []}
    end.

-spec get_voice_states_for_guild(integer(), binary()) -> term().
get_voice_states_for_guild(GuildId, CIdBin) ->
    gateway_rpc_guild_infra:with_voice_server(GuildId, fun(VPid, _) ->
        get_voice_states_for_channel(VPid, CIdBin)
    end).

-spec get_voice_states_for_channel(pid(), binary()) -> map().
get_voice_states_for_channel(VPid, CIdBin) ->
    case gen_server:call(VPid, {get_voice_states_for_channel, CIdBin}, 10000) of
        #{voice_states := VS} -> #{<<"voice_states">> => VS};
        _ -> gateway_rpc_error:raise(<<"voice_states_error">>)
    end.

-spec handle_get_pending_joins(map()) -> term().
handle_get_pending_joins(P) ->
    case validate_gc_params(P) of
        {ok, GId, CIdBin} -> get_pending_joins_for_guild(GId, CIdBin);
        error -> #{<<"pending_joins">> => []}
    end.

-spec get_pending_joins_for_guild(integer(), binary()) -> term().
get_pending_joins_for_guild(GuildId, CIdBin) ->
    gateway_rpc_guild_infra:with_voice_server(GuildId, fun(VPid, _) ->
        get_pending_joins_for_channel(VPid, CIdBin)
    end).

-spec get_pending_joins_for_channel(pid(), binary()) -> map().
get_pending_joins_for_channel(VPid, CIdBin) ->
    case gen_server:call(VPid, {get_pending_joins_for_channel, CIdBin}, 10000) of
        #{pending_joins := PJ} -> #{<<"pending_joins">> => PJ};
        _ -> gateway_rpc_error:raise(<<"pending_joins_error">>)
    end.

-spec handle_move_member(map()) -> term().
handle_move_member(
    #{
        <<"guild_id">> := GIB,
        <<"user_id">> := UIB,
        <<"moderator_id">> := MIB,
        <<"channel_id">> := CIB
    } = P
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UIB),
    ModeratorId = validation:snowflake_or_throw(<<"moderator_id">>, MIB),
    {ok, ChannelId} = validation:validate_optional_snowflake(CIB),
    ConnId = maps:get(<<"connection_id">>, P, null),
    gateway_rpc_guild_infra:with_voice_server(GuildId, fun(VPid, GPid) ->
        move_member(VPid, GPid, GuildId, UserId, ModeratorId, ChannelId, ConnId)
    end).

-spec move_member(pid(), pid(), integer(), integer(), integer(), integer() | null, term()) ->
    map().
move_member(VPid, GPid, GuildId, UserId, ModeratorId, ChannelId, ConnId) ->
    Req = #{
        user_id => UserId,
        moderator_id => ModeratorId,
        channel_id => ChannelId,
        connection_id => ConnId
    },
    Result = gen_server:call(VPid, {move_member, Req}, ?GUILD_CALL_TIMEOUT),
    gateway_rpc_guild_voice_util:handle_move_member_result(Result, GuildId, ChannelId, GPid).

-spec handle_get_voice_state(map()) -> term().
handle_get_voice_state(#{<<"guild_id">> := GIB, <<"user_id">> := UIB}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UIB),
    gateway_rpc_guild_infra:with_voice_server(GuildId, fun(VPid, _) ->
        get_voice_state(VPid, UserId)
    end).

-spec get_voice_state(pid(), integer()) -> map().
get_voice_state(VPid, UserId) ->
    case gen_server:call(VPid, {get_voice_state, #{user_id => UserId}}, ?GUILD_CALL_TIMEOUT) of
        #{voice_state := null} -> #{<<"voice_state">> => null};
        #{voice_state := VS} -> #{<<"voice_state">> => VS};
        _ -> gateway_rpc_error:raise(<<"voice_state_error">>)
    end.

-spec handle_switch_voice_region(map()) -> term().
handle_switch_voice_region(#{<<"guild_id">> := GIB, <<"channel_id">> := CIB}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, CIB),
    gateway_rpc_guild_infra:with_voice_server(GuildId, fun(VPid, GPid) ->
        switch_voice_region(VPid, GPid, GuildId, ChannelId)
    end).

-spec switch_voice_region(pid(), pid(), integer(), integer()) -> map().
switch_voice_region(VPid, GPid, GuildId, ChannelId) ->
    Request = {switch_voice_region, #{channel_id => ChannelId}},
    case gen_server:call(VPid, Request, ?GUILD_CALL_TIMEOUT) of
        #{success := true} ->
            spawn_switch_voice_region(GuildId, ChannelId, GPid),
            #{<<"success">> => true};
        #{error := E} ->
            raise_voice_error(E)
    end.

-spec spawn_switch_voice_region(integer(), integer(), pid()) -> ok.
spawn_switch_voice_region(GuildId, ChannelId, GPid) ->
    spawn(fun() -> guild_voice:switch_voice_region(GuildId, ChannelId, GPid) end),
    ok.

-spec handle_batch_update(map()) -> term().
handle_batch_update(#{<<"updates">> := Updates}) ->
    gateway_rpc_guild_routing:validate_batch_size(length(Updates)),
    Parsed = lists:map(fun gateway_rpc_guild_voice_util:parse_voice_update/1, Updates),
    Results = gateway_rpc_guild_routing:process_batch(
        Parsed,
        fun gateway_rpc_guild_voice_util:process_voice_update/1,
        ?BATCH_TIMEOUT_MS
    ),
    #{<<"results">> => Results}.

-spec raise_voice_error(term()) -> no_return().
raise_voice_error(Error) ->
    gateway_rpc_error:raise(normalize_voice_rpc_error(Error)).

-spec normalize_voice_rpc_error(term()) -> binary().
normalize_voice_rpc_error(Error) ->
    gateway_rpc_guild_voice_util:normalize_voice_rpc_error(Error).

-spec validate_gc_params(term()) -> {ok, integer(), binary()} | error.
validate_gc_params(P) when is_map(P) ->
    GuildIdResult = validation:validate_snowflake(
        <<"guild_id">>,
        maps:get(<<"guild_id">>, P, undefined)
    ),
    ChannelIdResult = validation:validate_snowflake(
        <<"channel_id">>,
        maps:get(<<"channel_id">>, P, undefined)
    ),
    case {GuildIdResult, ChannelIdResult} of
        {{ok, GId}, {ok, CId}} -> {ok, GId, integer_to_binary(CId)};
        _ -> error
    end;
validate_gc_params(_) ->
    error.

-ifdef(TEST).

guild_voice_channel_read_malformed_payloads_test() ->
    ?assertEqual(
        #{<<"voice_states">> => []},
        handle(<<"guild.get_voice_states_for_channel">>, #{})
    ),
    ?assertEqual(
        #{<<"pending_joins">> => []},
        handle(
            <<"guild.get_pending_joins_for_channel">>,
            #{<<"guild_id">> => null, <<"channel_id">> => <<"2">>}
        )
    ).

-endif.
