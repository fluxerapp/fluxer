%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_connection).

-typing([eqwalizer]).

-export([voice_state_update/2]).
-export([confirm_voice_connection_from_livekit/2]).
-export([request_voice_token/4]).
-export([request_voice_token/5]).
-export([request_voice_token/6]).
-export([request_voice_token/8]).
-export([sweep_expired_pending_joins/1]).

-export_type([
    guild_state/0,
    voice_state_map/0,
    voice_reply/0
]).

-type guild_state() :: map().
-type voice_state_map() :: #{binary() => map()}.
-type voice_reply() :: {reply, map() | {error, atom(), atom()}, guild_state()}.

-spec voice_state_update(map(), guild_state()) -> voice_reply().
voice_state_update(Request, State) ->
    Context = guild_voice_connection_util:build_context(Request),
    case maps:get(user_id, Context) of
        undefined ->
            {reply, gateway_errors:error(voice_invalid_user_id), State};
        UserId ->
            handle_with_member(Context, UserId, State)
    end.

-spec confirm_voice_connection_from_livekit(map(), guild_state()) -> voice_reply().
confirm_voice_connection_from_livekit(Request, State) ->
    guild_voice_connection_confirm:confirm_voice_connection_from_livekit(Request, State).

-spec request_voice_token(integer(), integer(), integer(), map()) ->
    {ok, map()} | {error, term()}.
request_voice_token(GuildId, ChannelId, UserId, VoicePermissions) ->
    guild_voice_connection_token:request_voice_token(
        GuildId, ChannelId, UserId, VoicePermissions
    ).

-spec request_voice_token(integer(), integer(), integer(), binary() | null, map()) ->
    {ok, map()} | {error, term()}.
request_voice_token(GuildId, ChannelId, UserId, ConnectionId, VoicePermissions) ->
    guild_voice_connection_token:request_voice_token(
        GuildId, ChannelId, UserId, ConnectionId, VoicePermissions
    ).

-spec request_voice_token(
    integer(), integer(), integer(), binary() | null, map(), binary() | null
) -> {ok, map()} | {error, term()}.
request_voice_token(GuildId, ChannelId, UserId, ConnectionId, VoicePermissions, TokenNonce) ->
    guild_voice_connection_token:request_voice_token(
        GuildId, ChannelId, UserId, ConnectionId, VoicePermissions, TokenNonce
    ).

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
request_voice_token(GuildId, ChannelId, UserId, ConnId, Perms, Nonce, Lat, Long) ->
    guild_voice_connection_token:request_voice_token(
        GuildId, ChannelId, UserId, ConnId, Perms, Nonce, Lat, Long
    ).

-spec sweep_expired_pending_joins(guild_state()) -> guild_state().
sweep_expired_pending_joins(State) ->
    guild_voice_connection_pending:sweep_expired_pending_joins(State).

-spec handle_with_member(map(), integer(), guild_state()) ->
    voice_reply().
handle_with_member(Context, UserId, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    case guild_voice_member:find_member_by_user_id(UserId, State) of
        undefined ->
            {reply, gateway_errors:error(voice_member_not_found), State};
        Member ->
            handle_member_voice(Context, Member, VoiceStates, State)
    end.

-spec handle_member_voice(map(), map(), voice_state_map(), guild_state()) ->
    voice_reply().
handle_member_voice(Context, Member, VoiceStates, State) ->
    case maps:get(channel_id, Context) of
        undefined ->
            {reply, gateway_errors:error(voice_invalid_channel_id), State};
        null ->
            handle_disconnect(Context, VoiceStates, State);
        ChannelIdValue ->
            handle_connect_or_update(Context, ChannelIdValue, Member, VoiceStates, State)
    end.

-spec handle_disconnect(map(), voice_state_map(), guild_state()) ->
    voice_reply().
handle_disconnect(Context, VoiceStates, State) ->
    guild_voice_disconnect:handle_voice_disconnect(
        maps:get(connection_id, Context),
        maps:get(session_id, Context),
        maps:get(user_id, Context),
        VoiceStates,
        State
    ).

-spec handle_connect_or_update(map(), integer(), map(), voice_state_map(), guild_state()) ->
    voice_reply().
handle_connect_or_update(Context, ChannelIdValue, Member, VoiceStates, State) ->
    Channel = guild_voice_member:find_channel_by_id(ChannelIdValue, State),
    case Channel of
        undefined ->
            {reply, gateway_errors:error(voice_channel_not_found), State};
        _ ->
            route_by_connection_id(Context, ChannelIdValue, Member, Channel, VoiceStates, State)
    end.

-spec route_by_connection_id(map(), integer(), map(), map(), voice_state_map(), guild_state()) ->
    voice_reply().
route_by_connection_id(Context, ChannelIdValue, Member, Channel, VoiceStates, State) ->
    case maps:get(connection_id, Context) of
        undefined ->
            guild_voice_connection_join:handle_new_connection(
                Context, Member, Channel, VoiceStates, State
            );
        _ ->
            guild_voice_connection_update:handle_update_connection(
                Context, ChannelIdValue, Member, Channel, VoiceStates, State
            )
    end.
