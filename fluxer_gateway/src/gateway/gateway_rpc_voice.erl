%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_voice).
-typing([eqwalizer]).

-export([execute_method/2]).
-export([
    normalize_pending_joins/1,
    normalize_voice_states/1,
    validate_voice_channel_read_params/1
]).

-spec execute_method(binary(), map()) -> term().
execute_method(<<"voice.confirm_connection">>, P) ->
    handle_confirm_connection(P);
execute_method(<<"voice.disconnect_user_if_in_channel">>, P) ->
    handle_disconnect_if_in_channel(P);
execute_method(<<"voice.get_voice_states_for_channel">>, P) ->
    handle_get_voice_states(P);
execute_method(<<"voice.get_pending_joins_for_channel">>, P) ->
    handle_get_pending_joins(P);
execute_method(Method, _Params) ->
    gateway_rpc_error:raise(<<"Unknown method: ", Method/binary>>).

-spec handle_confirm_connection(map()) -> term().
handle_confirm_connection(Params) ->
    ChannelIdBin = maps:get(<<"channel_id">>, Params),
    ConnectionId = maps:get(<<"connection_id">>, Params),
    case parse_optional_guild_id(Params) of
        undefined ->
            gateway_rpc_call:execute_method(
                <<"call.confirm_connection">>,
                #{<<"channel_id">> => ChannelIdBin, <<"connection_id">> => ConnectionId}
            );
        GuildId ->
            TokenNonce = maps:get(<<"token_nonce">>, Params, undefined),
            gateway_rpc_guild:execute_method(
                <<"guild.confirm_voice_connection_from_livekit">>,
                #{
                    <<"guild_id">> => integer_to_binary(GuildId),
                    <<"connection_id">> => ConnectionId,
                    <<"token_nonce">> => TokenNonce
                }
            )
    end.

-spec handle_disconnect_if_in_channel(map()) -> term().
handle_disconnect_if_in_channel(Params) ->
    ChannelIdBin = maps:get(<<"channel_id">>, Params),
    UserIdBin = maps:get(<<"user_id">>, Params),
    ConnectionId = maps:get(<<"connection_id">>, Params, undefined),
    case parse_optional_guild_id(Params) of
        undefined ->
            CallParams = #{<<"channel_id">> => ChannelIdBin, <<"user_id">> => UserIdBin},
            gateway_rpc_call:execute_method(
                <<"call.disconnect_user_if_in_channel">>,
                maybe_put_connection_id(ConnectionId, CallParams)
            );
        GuildId ->
            GuildParams = #{
                <<"guild_id">> => integer_to_binary(GuildId),
                <<"user_id">> => UserIdBin,
                <<"expected_channel_id">> => ChannelIdBin
            },
            gateway_rpc_guild:execute_method(
                <<"guild.disconnect_voice_user_if_in_channel">>,
                maybe_put_connection_id(ConnectionId, GuildParams)
            )
    end.

-spec handle_get_voice_states(map()) -> map() | term().
handle_get_voice_states(Params) ->
    case validate_voice_channel_read_params(Params) of
        {ok, ChannelIdBin, undefined} ->
            build_dm_voice_states_response(ChannelIdBin);
        {ok, ChannelIdBin, GuildId} ->
            gateway_rpc_guild:execute_method(
                <<"guild.get_voice_states_for_channel">>,
                #{
                    <<"guild_id">> => integer_to_binary(GuildId),
                    <<"channel_id">> => ChannelIdBin
                }
            );
        error ->
            #{<<"voice_states">> => []}
    end.

-spec handle_get_pending_joins(map()) -> map() | term().
handle_get_pending_joins(Params) ->
    case validate_voice_channel_read_params(Params) of
        {ok, ChannelIdBin, undefined} ->
            normalize_pending_joins_response(
                gateway_rpc_call:execute_method(
                    <<"call.get_pending_joins">>,
                    #{<<"channel_id">> => ChannelIdBin}
                )
            );
        {ok, ChannelIdBin, GuildId} ->
            gateway_rpc_guild:execute_method(
                <<"guild.get_pending_joins_for_channel">>,
                #{
                    <<"guild_id">> => integer_to_binary(GuildId),
                    <<"channel_id">> => ChannelIdBin
                }
            );
        error ->
            #{<<"pending_joins">> => []}
    end.

-spec parse_optional_guild_id(map()) -> integer() | undefined.
parse_optional_guild_id(Params) ->
    case maps:get(<<"guild_id">>, Params, undefined) of
        undefined ->
            undefined;
        null ->
            undefined;
        0 ->
            undefined;
        <<"0">> ->
            undefined;
        GuildIdBin ->
            validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin)
    end.

-spec validate_voice_channel_read_params(term()) ->
    {ok, binary(), integer() | undefined} | error.
validate_voice_channel_read_params(Params) when is_map(Params) ->
    ChannelIdValue = maps:get(<<"channel_id">>, Params, undefined),
    case validation:validate_snowflake(<<"channel_id">>, ChannelIdValue) of
        {ok, ChannelId} -> validate_voice_channel_guild_id(ChannelId, Params);
        _ -> error
    end;
validate_voice_channel_read_params(_) ->
    error.

-spec validate_voice_channel_guild_id(integer(), map()) ->
    {ok, binary(), integer() | undefined} | error.
validate_voice_channel_guild_id(ChannelId, Params) ->
    case validate_optional_guild_id(Params) of
        {ok, GuildId} -> {ok, integer_to_binary(ChannelId), GuildId};
        error -> error
    end.

-spec validate_optional_guild_id(map()) -> {ok, integer() | undefined} | error.
validate_optional_guild_id(Params) ->
    case maps:get(<<"guild_id">>, Params, undefined) of
        undefined ->
            {ok, undefined};
        null ->
            {ok, undefined};
        0 ->
            {ok, undefined};
        <<"0">> ->
            {ok, undefined};
        GuildIdBin ->
            Result = validation:validate_snowflake(<<"guild_id">>, GuildIdBin),
            validate_guild_id_result(Result)
    end.

-spec validate_guild_id_result(term()) -> {ok, integer()} | error.
validate_guild_id_result({ok, GuildId}) when is_integer(GuildId) ->
    {ok, GuildId};
validate_guild_id_result(_Result) ->
    error.

-spec maybe_put_connection_id(binary() | undefined, map()) -> map().
maybe_put_connection_id(undefined, Params) ->
    Params;
maybe_put_connection_id(ConnectionId, Params) ->
    Params#{<<"connection_id">> => ConnectionId}.

-spec build_dm_voice_states_response(binary()) -> map().
build_dm_voice_states_response(ChannelIdBin) ->
    case gateway_rpc_call:execute_method(<<"call.get">>, #{<<"channel_id">> => ChannelIdBin}) of
        null ->
            #{<<"voice_states">> => []};
        CallData when is_map(CallData) ->
            VoiceStates = get_map_value(CallData, [<<"voice_states">>, voice_states]),
            #{<<"voice_states">> => normalize_voice_states(VoiceStates)}
    end.

-spec normalize_voice_states(term()) -> [map()].
normalize_voice_states(VoiceStates) ->
    normalize_entries(VoiceStates, fun normalize_voice_state_entry/2).

-spec normalize_entries(term(), fun((term(), [map()]) -> [map()])) -> [map()].
normalize_entries(Entries, Fun) when is_list(Entries) ->
    lists:reverse(lists:foldl(Fun, [], Entries));
normalize_entries(_Entries, _Fun) ->
    [].

-spec normalize_voice_state_entry(term(), [map()]) -> [map()].
normalize_voice_state_entry(VoiceState, Acc) when is_map(VoiceState) ->
    {ConnectionId, UserId} = connection_user_ids(VoiceState),
    ChannelId = normalize_id(get_map_value(VoiceState, [<<"channel_id">>, channel_id])),
    case {ConnectionId, UserId, ChannelId} of
        {undefined, _, _} ->
            Acc;
        {_, undefined, _} ->
            Acc;
        {_, _, undefined} ->
            Acc;
        _ ->
            NormalizedVoiceState0 = #{
                <<"connection_id">> => ConnectionId,
                <<"user_id">> => UserId,
                <<"channel_id">> => ChannelId
            },
            NormalizedVoiceState = maybe_attach_voice_routing_metadata(
                NormalizedVoiceState0, VoiceState
            ),
            [NormalizedVoiceState | Acc]
    end;
normalize_voice_state_entry(_, Acc) ->
    Acc.

-spec maybe_attach_voice_routing_metadata(map(), map()) -> map().
maybe_attach_voice_routing_metadata(NormalizedVoiceState, VoiceState) ->
    WithRegion = maybe_put_normalized_id(
        NormalizedVoiceState,
        <<"region_id">>,
        get_map_value(VoiceState, [<<"region_id">>, region_id])
    ),
    maybe_put_normalized_id(
        WithRegion,
        <<"server_id">>,
        get_map_value(VoiceState, [<<"server_id">>, server_id])
    ).

-spec maybe_put_normalized_id(map(), binary(), term()) -> map().
maybe_put_normalized_id(Map, Key, Value) ->
    case normalize_id(Value) of
        undefined -> Map;
        Normalized -> Map#{Key => Normalized}
    end.

-spec normalize_pending_joins_response(term()) -> map().
normalize_pending_joins_response(Response) when is_map(Response) ->
    PendingJoins = get_map_value(Response, [<<"pending_joins">>, pending_joins]),
    #{<<"pending_joins">> => normalize_pending_joins(PendingJoins)};
normalize_pending_joins_response(_) ->
    #{<<"pending_joins">> => []}.

-spec normalize_pending_joins(term()) -> [map()].
normalize_pending_joins(PendingJoins) ->
    normalize_entries(PendingJoins, fun normalize_pending_join_entry/2).

-spec normalize_pending_join_entry(term(), [map()]) -> [map()].
normalize_pending_join_entry(PendingJoin, Acc) when is_map(PendingJoin) ->
    {ConnectionId, UserId} = connection_user_ids(PendingJoin),
    TokenNonce = normalize_token_nonce(
        get_map_value(PendingJoin, [<<"token_nonce">>, token_nonce])
    ),
    ExpiresAt = normalize_expiry(get_map_value(PendingJoin, [<<"expires_at">>, expires_at])),
    case {ConnectionId, UserId} of
        {undefined, _} ->
            Acc;
        {_, undefined} ->
            Acc;
        _ ->
            NormalizedPendingJoin = #{
                <<"connection_id">> => ConnectionId,
                <<"user_id">> => UserId,
                <<"token_nonce">> => TokenNonce,
                <<"expires_at">> => ExpiresAt
            },
            [NormalizedPendingJoin | Acc]
    end;
normalize_pending_join_entry(_, Acc) ->
    Acc.

-spec connection_user_ids(map()) -> {binary() | undefined, binary() | undefined}.
connection_user_ids(Map) ->
    ConnectionId = normalize_id(get_map_value(Map, [<<"connection_id">>, connection_id])),
    UserId = normalize_id(get_map_value(Map, [<<"user_id">>, user_id])),
    {ConnectionId, UserId}.

-spec normalize_id(term()) -> binary() | undefined.
normalize_id(undefined) -> undefined;
normalize_id(Value) when is_binary(Value) -> Value;
normalize_id(Value) when is_integer(Value) -> integer_to_binary(Value);
normalize_id(_) -> undefined.

-spec normalize_token_nonce(term()) -> binary().
normalize_token_nonce(undefined) -> <<>>;
normalize_token_nonce(Value) when is_binary(Value) -> Value;
normalize_token_nonce(Value) when is_integer(Value) -> integer_to_binary(Value);
normalize_token_nonce(_) -> <<>>.

-spec normalize_expiry(term()) -> integer().
normalize_expiry(Value) when is_integer(Value) -> Value;
normalize_expiry(_) -> 0.

-spec get_map_value(map(), [term()]) -> term().
get_map_value(_Map, []) ->
    undefined;
get_map_value(Map, [Key | Rest]) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> get_map_value(Map, Rest)
    end.
