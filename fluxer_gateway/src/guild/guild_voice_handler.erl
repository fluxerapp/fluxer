%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(guild_voice_handler).

-export([
    handle_call/3,
    handle_cast/2
]).

-type guild_state() :: map().

-spec handle_call(term(), gen_server:from(), guild_state()) ->
    {reply, term(), guild_state()}
    | {noreply, guild_state()}.
handle_call({voice_state_update, Request}, _From, State) ->
    guild_voice:voice_state_update(Request, State);
handle_call({get_voice_state, Request}, _From, State) ->
    guild_voice:get_voice_state(Request, State);
handle_call({update_member_voice, Request}, _From, State) ->
    guild_voice:update_member_voice(Request, State);
handle_call({disconnect_voice_user, Request}, _From, State) ->
    guild_voice:disconnect_voice_user(Request, State);
handle_call({disconnect_voice_user_if_in_channel, Request}, _From, State) ->
    guild_voice:disconnect_voice_user_if_in_channel(Request, State);
handle_call({disconnect_all_voice_users_in_channel, Request}, _From, State) ->
    guild_voice:disconnect_all_voice_users_in_channel(Request, State);
handle_call({confirm_voice_connection_from_livekit, Request}, _From, State) ->
    guild_voice:confirm_voice_connection_from_livekit(Request, State);
handle_call({move_member, Request}, _From, State) ->
    guild_voice:move_member(Request, State);
handle_call({switch_voice_region, Request}, _From, State) ->
    guild_voice:switch_voice_region_handler(Request, State);
handle_call({add_virtual_channel_access, UserId, ChannelId}, _From, State) ->
    NewState = guild_virtual_channel_access:add_virtual_access(UserId, ChannelId, State),
    guild_virtual_channel_access:dispatch_channel_visibility_change(
        UserId, ChannelId, add, NewState
    ),
    {reply, ok, NewState};
handle_call({store_pending_connection, ConnectionId, Metadata}, _From, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    NewPendingConnections = maps:put(ConnectionId, Metadata, PendingConnections),
    NewState = maps:put(pending_voice_connections, NewPendingConnections, State),
    {reply, ok, NewState};
handle_call({get_voice_states_for_channel, ChannelIdBin}, _From, State) ->
    VoiceStates = maps:get(voice_states, State, #{}),
    Filtered = maps:fold(
        fun(ConnId, VS, Acc) ->
            case maps:get(<<"channel_id">>, VS, null) of
                ChannelIdBin ->
                    [#{
                        connection_id => ConnId,
                        user_id => maps:get(<<"user_id">>, VS, null),
                        channel_id => ChannelIdBin
                    } | Acc];
                _ ->
                    Acc
            end
        end,
        [],
        VoiceStates
    ),
    {reply, #{voice_states => Filtered}, State};
handle_call({get_pending_joins_for_channel, ChannelIdBin}, _From, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    ChannelIdInt = binary_to_integer(ChannelIdBin),
    Filtered = maps:fold(
        fun(ConnId, Metadata, Acc) ->
            case maps:get(channel_id, Metadata, undefined) of
                ChannelIdInt ->
                    [#{
                        connection_id => ConnId,
                        user_id => integer_to_binary(maps:get(user_id, Metadata, 0)),
                        token_nonce => maps:get(token_nonce, Metadata, null),
                        expires_at => maps:get(expires_at, Metadata, 0)
                    } | Acc];
                _ ->
                    Acc
            end
        end,
        [],
        PendingConnections
    ),
    {reply, #{pending_joins => Filtered}, State}.

-spec handle_cast(term(), guild_state()) -> {noreply, guild_state()}.
handle_cast({relay_voice_state_update, VoiceState, OldChannelIdBin}, State) ->
    State1 = relay_upsert_voice_state(VoiceState, State),
    StateNoRelay = maps:remove(very_large_guild_coordinator_pid, State1),
    _ = guild_voice_broadcast:broadcast_voice_state_update(VoiceState, StateNoRelay, OldChannelIdBin),
    {noreply, State1};
handle_cast(
    {relay_voice_server_update, GuildId, ChannelId, SessionId, Token, Endpoint, ConnectionId},
    State
) ->
    StateNoRelay = maps:remove(very_large_guild_coordinator_pid, State),
    _ = guild_voice_broadcast:broadcast_voice_server_update_to_session(
        GuildId,
        ChannelId,
        SessionId,
        Token,
        Endpoint,
        ConnectionId,
        StateNoRelay
    ),
    {noreply, State};
handle_cast({store_pending_connection, ConnectionId, Metadata}, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    NewPendingConnections = maps:put(ConnectionId, Metadata, PendingConnections),
    NewState = maps:put(pending_voice_connections, NewPendingConnections, State),
    {noreply, NewState};
handle_cast({add_virtual_channel_access, UserId, ChannelId}, State) ->
    NewState = guild_virtual_channel_access:add_virtual_access(UserId, ChannelId, State),
    guild_virtual_channel_access:dispatch_channel_visibility_change(
        UserId, ChannelId, add, NewState
    ),
    {noreply, NewState};
handle_cast({remove_virtual_channel_access, UserId, ChannelId}, State) ->
    guild_virtual_channel_access:dispatch_channel_visibility_change(
        UserId, ChannelId, remove, State
    ),
    NewState = guild_virtual_channel_access:remove_virtual_access(UserId, ChannelId, State),
    {noreply, NewState};
handle_cast({cleanup_virtual_access_for_user, UserId}, State) ->
    NewState = guild_voice_disconnect:cleanup_virtual_channel_access_for_user(UserId, State),
    {noreply, NewState}.

-spec relay_upsert_voice_state(map(), guild_state()) -> guild_state().
relay_upsert_voice_state(VoiceState, State) when is_map(VoiceState) ->
    ConnectionId = maps:get(<<"connection_id">>, VoiceState, undefined),
    case ConnectionId of
        undefined ->
            State;
        _ ->
            VoiceStates0 = maps:get(voice_states, State, #{}),
            ChannelId = maps:get(<<"channel_id">>, VoiceState, null),
            VoiceStates =
                case ChannelId of
                    null -> maps:remove(ConnectionId, VoiceStates0);
                    _ -> maps:put(ConnectionId, VoiceState, VoiceStates0)
                end,
            maps:put(voice_states, VoiceStates, State)
    end;
relay_upsert_voice_state(_, State) ->
    State.
