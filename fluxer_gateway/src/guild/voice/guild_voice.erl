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

-module(guild_voice).

-export([voice_state_update/2]).
-export([get_voice_state/2]).
-export([update_member_voice/2]).
-export([disconnect_voice_user/2]).
-export([disconnect_voice_user_if_in_channel/2]).
-export([disconnect_all_voice_users_in_channel/2]).
-export([confirm_voice_connection_from_livekit/2]).
-export([move_member/2]).
-export([broadcast_voice_state_update/3]).
-export([broadcast_voice_server_update_to_session/6]).
-export([send_voice_server_update_for_move/5]).
-export([send_voice_server_updates_for_move/4]).
-export([switch_voice_region_handler/2]).
-export([switch_voice_region/3]).
-export([get_voice_states_list/1]).
-export([handle_virtual_channel_access_for_move/4]).
-export([cleanup_virtual_access_on_disconnect/2]).

voice_state_update(Request, State) ->
    case guild_voice_connection:voice_state_update(Request, State) of
        {reply, Response, NewState} ->
            {reply, Response, NewState};
        {error, Category, Message} ->
            {reply, {error, Category, Message}, State}
    end.

get_voice_state(Request, State) ->
    guild_voice_state:get_voice_state(Request, State).

get_voice_states_list(State) ->
    guild_voice_state:get_voice_states_list(State).

update_member_voice(Request, State) ->
    guild_voice_member:update_member_voice(Request, State).

disconnect_voice_user(Request, State) ->
    guild_voice_disconnect:disconnect_voice_user(Request, State).

disconnect_voice_user_if_in_channel(Request, State) ->
    guild_voice_disconnect:disconnect_voice_user_if_in_channel(Request, State).

disconnect_all_voice_users_in_channel(Request, State) ->
    guild_voice_disconnect:disconnect_all_voice_users_in_channel(Request, State).

confirm_voice_connection_from_livekit(Request, State) ->
    case guild_voice_connection:confirm_voice_connection_from_livekit(Request, State) of
        {reply, Response, NewState} ->
            {reply, Response, NewState};
        {error, Category, Message} ->
            {reply, {error, Category, Message}, State}
    end.

move_member(Request, State) ->
    guild_voice_move:move_member(Request, State).

send_voice_server_update_for_move(GuildId, ChannelId, UserId, SessionId, GuildPid) ->
    guild_voice_move:send_voice_server_update_for_move(
        GuildId, ChannelId, UserId, SessionId, GuildPid
    ).

send_voice_server_updates_for_move(GuildId, ChannelId, SessionDataList, GuildPid) ->
    guild_voice_move:send_voice_server_updates_for_move(
        GuildId, ChannelId, SessionDataList, GuildPid
    ).

broadcast_voice_state_update(VoiceState, State, OldChannelIdBin) ->
    guild_voice_broadcast:broadcast_voice_state_update(VoiceState, State, OldChannelIdBin).

broadcast_voice_server_update_to_session(GuildId, SessionId, Token, Endpoint, ConnectionId, State) ->
    guild_voice_broadcast:broadcast_voice_server_update_to_session(
        GuildId, SessionId, Token, Endpoint, ConnectionId, State
    ).

switch_voice_region_handler(Request, State) ->
    guild_voice_region:switch_voice_region_handler(Request, State).

switch_voice_region(GuildId, ChannelId, GuildPid) ->
    guild_voice_region:switch_voice_region(GuildId, ChannelId, GuildPid).

handle_virtual_channel_access_for_move(UserId, ChannelId, _ConnectionsToMove, GuildPid) ->
    case gen_server:call(GuildPid, {get_sessions}, 10000) of
        State when is_map(State) ->
            Member = guild_permissions:find_member_by_user_id(UserId, State),
            case Member of
                undefined ->
                    ok;
                _ ->
                    HasViewPermission = guild_permissions:can_view_channel_by_permissions(
                        UserId, ChannelId, Member, State
                    ),
                    case HasViewPermission of
                        true ->
                            ok;
                        false ->
                            gen_server:cast(
                                GuildPid,
                                {add_virtual_channel_access, UserId, ChannelId}
                            )
                    end
            end;
        _ ->
            ok
    end.

cleanup_virtual_access_on_disconnect(UserId, GuildPid) ->
    gen_server:cast(GuildPid, {cleanup_virtual_access_for_user, UserId}).
