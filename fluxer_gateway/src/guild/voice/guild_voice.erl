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
-export([broadcast_voice_server_update_to_session/7]).
-export([send_voice_server_update_for_move/5]).
-export([send_voice_server_updates_for_move/4]).
-export([switch_voice_region_handler/2]).
-export([switch_voice_region/3]).
-export([get_voice_states_list/1]).
-export([handle_virtual_channel_access_for_move/4]).
-export([cleanup_virtual_access_on_disconnect/2]).

-type guild_state() :: map().
-type voice_state() :: map().

-spec voice_state_update(map(), guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
voice_state_update(Request, State) ->
    guild_voice_connection:voice_state_update(Request, State).

-spec get_voice_state(map(), guild_state()) -> {reply, map(), guild_state()}.
get_voice_state(Request, State) ->
    guild_voice_state:get_voice_state(Request, State).

-spec get_voice_states_list(guild_state()) -> [voice_state()].
get_voice_states_list(State) ->
    guild_voice_state:get_voice_states_list(State).

-spec update_member_voice(map(), guild_state()) -> {reply, map(), guild_state()}.
update_member_voice(Request, State) ->
    guild_voice_member:update_member_voice(Request, State).

-spec disconnect_voice_user(map(), guild_state()) -> {reply, map(), guild_state()}.
disconnect_voice_user(Request, State) ->
    guild_voice_disconnect:disconnect_voice_user(Request, State).

-spec disconnect_voice_user_if_in_channel(map(), guild_state()) -> {reply, map(), guild_state()}.
disconnect_voice_user_if_in_channel(Request, State) ->
    guild_voice_disconnect:disconnect_voice_user_if_in_channel(Request, State).

-spec disconnect_all_voice_users_in_channel(map(), guild_state()) -> {reply, map(), guild_state()}.
disconnect_all_voice_users_in_channel(Request, State) ->
    guild_voice_disconnect:disconnect_all_voice_users_in_channel(Request, State).

-spec confirm_voice_connection_from_livekit(map(), guild_state()) ->
    {reply, map(), guild_state()} | {error, atom(), atom()}.
confirm_voice_connection_from_livekit(Request, State) ->
    guild_voice_connection:confirm_voice_connection_from_livekit(Request, State).

-spec move_member(map(), guild_state()) -> {reply, map(), guild_state()}.
move_member(Request, State) ->
    guild_voice_move:move_member(Request, State).

-spec send_voice_server_update_for_move(integer(), integer(), integer(), binary(), pid()) -> ok.
send_voice_server_update_for_move(GuildId, ChannelId, UserId, SessionId, GuildPid) ->
    guild_voice_move:send_voice_server_update_for_move(
        GuildId, ChannelId, UserId, SessionId, GuildPid
    ).

-spec send_voice_server_updates_for_move(integer(), integer(), [map()], pid()) -> ok.
send_voice_server_updates_for_move(GuildId, ChannelId, SessionDataList, GuildPid) ->
    guild_voice_move:send_voice_server_updates_for_move(
        GuildId, ChannelId, SessionDataList, GuildPid
    ).

-spec broadcast_voice_state_update(voice_state(), guild_state(), binary() | null) -> ok.
broadcast_voice_state_update(VoiceState, State, OldChannelIdBin) ->
    guild_voice_broadcast:broadcast_voice_state_update(VoiceState, State, OldChannelIdBin).

-spec broadcast_voice_server_update_to_session(
    integer(), integer(), binary(), binary(), binary(), binary(), guild_state()
) -> ok.
broadcast_voice_server_update_to_session(
    GuildId,
    ChannelId,
    SessionId,
    Token,
    Endpoint,
    ConnectionId,
    State
) ->
    guild_voice_broadcast:broadcast_voice_server_update_to_session(
        GuildId, ChannelId, SessionId, Token, Endpoint, ConnectionId, State
    ).

-spec switch_voice_region_handler(map(), guild_state()) -> {reply, map(), guild_state()}.
switch_voice_region_handler(Request, State) ->
    guild_voice_region:switch_voice_region_handler(Request, State).

-spec switch_voice_region(integer(), integer(), pid()) -> ok | {error, term()}.
switch_voice_region(GuildId, ChannelId, GuildPid) ->
    guild_voice_region:switch_voice_region(GuildId, ChannelId, GuildPid).

-spec handle_virtual_channel_access_for_move(integer(), integer(), map(), pid()) -> ok.
handle_virtual_channel_access_for_move(UserId, ChannelId, _ConnectionsToMove, GuildPid) ->
    case gen_server:call(GuildPid, {get_sessions}, 10000) of
        State when is_map(State) ->
            Member = guild_permissions:find_member_by_user_id(UserId, State),
            case Member of
                undefined ->
                    ok;
                _ ->
                    Permissions = guild_permissions:get_member_permissions(
                        UserId, ChannelId, State
                    ),
                    ViewPerm = constants:view_channel_permission(),
                    ConnectPerm = constants:connect_permission(),
                    HasView = (Permissions band ViewPerm) =:= ViewPerm,
                    HasConnect = (Permissions band ConnectPerm) =:= ConnectPerm,
                    case HasView andalso HasConnect of
                        true ->
                            ok;
                        false ->
                            gen_server:call(
                                GuildPid,
                                {add_virtual_channel_access, UserId, ChannelId},
                                10000
                            )
                    end
            end;
        _ ->
            ok
    end.

-spec cleanup_virtual_access_on_disconnect(integer(), pid()) -> ok.
cleanup_virtual_access_on_disconnect(UserId, GuildPid) ->
    GuildId = resolve_guild_id_from_pid(GuildPid),
    TargetPid = resolve_voice_server(GuildId, GuildPid),
    gen_server:cast(TargetPid, {cleanup_virtual_access_for_user, UserId}).

-spec resolve_guild_id_from_pid(pid()) -> integer() | undefined.
resolve_guild_id_from_pid(GuildPid) ->
    try gen_server:call(GuildPid, {get_sessions}, 5000) of
        State when is_map(State) ->
            maps:get(id, State, undefined);
        _ ->
            undefined
    catch
        _:_ -> undefined
    end.

-spec resolve_voice_server(integer() | undefined, pid()) -> pid().
resolve_voice_server(undefined, FallbackPid) ->
    FallbackPid;
resolve_voice_server(GuildId, FallbackPid) ->
    case guild_voice_server:lookup(GuildId) of
        {ok, VoicePid} -> VoicePid;
        {error, not_found} -> FallbackPid
    end.
