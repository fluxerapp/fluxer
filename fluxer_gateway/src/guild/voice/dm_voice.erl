%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(dm_voice).
-typing([eqwalizer]).

-export([voice_state_update/2]).
-export([get_voice_token/6]).
-export([disconnect_voice_user/2]).
-export([]).

-export_type([
    dm_state/0,
    voice_state/0
]).

-type dm_state() :: map().
-type voice_state() :: map().

-spec voice_state_update(map(), dm_state()) ->
    {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
voice_state_update(Request, State) ->
    #{
        user_id := UserId,
        channel_id := ChannelId
    } = Request,
    ConnectionId = maps:get(connection_id, Request, undefined),
    VoiceStates = maps:get(dm_voice_states, State, #{}),
    GatewaySessionId = maps:get(id, State, undefined),
    logger:debug(dm_voice_state_update_log_message(), [
        UserId, GatewaySessionId, ChannelId, ConnectionId
    ]),
    case ChannelId of
        null ->
            dm_voice_state:handle_dm_disconnect(ConnectionId, UserId, VoiceStates, State);
        ChannelIdValue ->
            handle_connect(ChannelIdValue, UserId, Request, State)
    end.

-spec handle_connect(integer(), integer(), map(), dm_state()) ->
    {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
handle_connect(ChannelIdValue, UserId, Request, State) ->
    Channels = maps:get(channels, State, #{}),
    UserId = maps:get(user_id, State),
    case dm_voice_ring:fetch_dm_channel_via_rpc(ChannelIdValue, UserId) of
        {ok, Channel} ->
            NewChannels = Channels#{ChannelIdValue => Channel},
            NewState = State#{channels => NewChannels},
            dm_voice_connect:handle_dm_voice_with_channel(
                Channel, ChannelIdValue, UserId, Request, NewState
            );
        {error, _Reason} ->
            {reply, gateway_errors:error(dm_channel_not_found), State}
    end.

-spec dm_voice_state_update_log_message() -> string().
dm_voice_state_update_log_message() ->
    "dm_voice_state_update_start: user_id=~p gateway_session_id=~p "
    "channel_id=~p connection_id=~p".

-spec get_voice_token(integer(), integer(), binary(), pid(), term(), term()) -> ok | error.
get_voice_token(ChannelId, UserId, SessionId, SessionPid, Latitude, Longitude) ->
    dm_voice_token:get_voice_token(
        ChannelId, UserId, SessionId, SessionPid, Latitude, Longitude
    ).

-spec disconnect_voice_user(integer(), dm_state()) -> {reply, map(), dm_state()}.
disconnect_voice_user(UserId, State) ->
    dm_voice_state:disconnect_voice_user(UserId, State).
