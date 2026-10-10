%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_connection_token).

-typing([eqwalizer]).

-export([request_voice_token/8]).
-export([request_voice_token/10]).

-spec request_voice_token(
    integer(),
    integer(),
    integer(),
    binary() | null,
    map(),
    binary() | null,
    binary() | undefined | null,
    binary() | undefined | null
) -> {ok, map()} | {error, term()}.
request_voice_token(
    GuildId,
    ChannelId,
    UserId,
    ConnectionId,
    VoicePermissions,
    TokenNonce,
    Latitude,
    Longitude
) ->
    case
        request_voice_token(
            GuildId,
            ChannelId,
            UserId,
            ConnectionId,
            VoicePermissions,
            TokenNonce,
            Latitude,
            Longitude,
            sfu,
            undefined
        )
    of
        {ok, TokenData} when is_map(TokenData) -> {ok, TokenData};
        {ok, _Grant} -> {error, voice_token_failed};
        {error, Reason} -> {error, Reason}
    end.

-spec request_voice_token(
    integer(),
    integer(),
    integer(),
    binary() | null,
    map(),
    binary() | null,
    binary() | undefined | null,
    binary() | undefined | null,
    voice_p2p:join_decision(),
    binary() | undefined
) -> {ok, map() | voice_p2p:token_grant()} | {error, term()}.
request_voice_token(
    GuildId,
    ChannelId,
    UserId,
    ConnectionId,
    VoicePermissions,
    TokenNonce,
    Latitude,
    Longitude,
    Decision,
    CountryCode
) ->
    Req = voice_p2p:add_to_token_request(
        voice_utils:build_voice_token_rpc_request(
            GuildId,
            ChannelId,
            UserId,
            ConnectionId,
            Latitude,
            Longitude,
            VoicePermissions,
            TokenNonce
        ),
        Decision,
        CountryCode
    ),
    log_token_start(GuildId, ChannelId, UserId, ConnectionId, TokenNonce),
    case rpc_client:call(Req) of
        {ok, Data} ->
            handle_token_success(GuildId, ChannelId, UserId, Data);
        {error, {rpc_error, Status, Body}} ->
            handle_token_rpc_error(GuildId, ChannelId, UserId, ConnectionId, Status, Body);
        {error, Reason} ->
            handle_token_error(GuildId, ChannelId, UserId, ConnectionId, Reason)
    end.

-spec log_token_start(integer(), integer(), integer(), binary() | null, binary() | null) -> ok.
log_token_start(GuildId, ChannelId, UserId, ConnectionId, TokenNonce) ->
    logger:debug(
        token_start_log_message(),
        [GuildId, ChannelId, UserId, ConnectionId, TokenNonce]
    ).

-spec token_start_log_message() -> string().
token_start_log_message() ->
    "guild_voice_request_token_start: guild_id=~p channel_id=~p "
    "user_id=~p connection_id=~p token_nonce=~p".

-spec handle_token_success(integer(), integer(), integer(), map()) ->
    {ok, map() | voice_p2p:token_grant()}.
handle_token_success(GuildId, ChannelId, UserId, Data) ->
    case voice_p2p:grant(Data) of
        sfu -> handle_sfu_token_success(GuildId, ChannelId, UserId, Data);
        Grant -> {ok, Grant}
    end.

-spec handle_sfu_token_success(integer(), integer(), integer(), map()) -> {ok, map()}.
handle_sfu_token_success(GuildId, ChannelId, UserId, Data) ->
    logger:debug(
        token_success_log_message(),
        [
            GuildId,
            ChannelId,
            UserId,
            maps:get(<<"connectionId">>, Data),
            maps:get(<<"endpoint">>, Data)
        ]
    ),
    {ok, #{
        token => maps:get(<<"token">>, Data),
        endpoint => maps:get(<<"endpoint">>, Data),
        connection_id => maps:get(<<"connectionId">>, Data),
        region_id => guild_voice_connection_util:normalize_optional_binary(
            maps:get(<<"regionId">>, Data, undefined)
        ),
        server_id => guild_voice_connection_util:normalize_optional_binary(
            maps:get(<<"serverId">>, Data, undefined)
        )
    }}.

-spec token_success_log_message() -> string().
token_success_log_message() ->
    "guild_voice_request_token_ok: guild_id=~p channel_id=~p "
    "user_id=~p connection_id=~p endpoint=~p".

-spec handle_token_rpc_error(integer(), integer(), integer(), binary() | null, term(), term()) ->
    {error, atom()}.
handle_token_rpc_error(GuildId, ChannelId, UserId, ConnectionId, Status, Body) ->
    log_token_failure(
        token_rpc_error_log_message(), GuildId, ChannelId, UserId, ConnectionId, [Status, Body]
    ),
    case guild_voice_unclaimed_account_utils:parse_unclaimed_error(Body) of
        true -> {error, voice_unclaimed_account};
        false -> {error, voice_token_failed}
    end.

-spec handle_token_error(integer(), integer(), integer(), binary() | null, term()) ->
    {error, atom()}.
handle_token_error(GuildId, ChannelId, UserId, ConnectionId, Reason) ->
    log_token_failure(
        token_error_log_message(), GuildId, ChannelId, UserId, ConnectionId, [Reason]
    ),
    {error, voice_token_failed}.

-spec log_token_failure(string(), integer(), integer(), integer(), binary() | null, [term()]) ->
    ok.
log_token_failure(Message, GuildId, ChannelId, UserId, ConnectionId, Details) ->
    logger:warning(Message, [GuildId, ChannelId, UserId, ConnectionId] ++ Details).

-spec token_rpc_error_log_message() -> string().
token_rpc_error_log_message() ->
    "guild_voice_request_token_rpc_error: guild_id=~p channel_id=~p "
    "user_id=~p connection_id=~p status=~p body=~p".

-spec token_error_log_message() -> string().
token_error_log_message() ->
    "guild_voice_request_token_error: guild_id=~p channel_id=~p "
    "user_id=~p connection_id=~p reason=~p".
