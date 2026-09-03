%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice).
-typing([eqwalizer]).

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

-export_type([
    guild_state/0,
    voice_state/0,
    voice_reply/0
]).

-type guild_state() :: map().
-type voice_state() :: map().
-type voice_reply() :: {reply, map() | {error, atom(), atom()}, guild_state()}.

-spec voice_state_update(map(), guild_state()) -> voice_reply().
voice_state_update(Request, State) ->
    guild_voice_connection:voice_state_update(Request, State).

-spec get_voice_state(map(), guild_state()) -> {reply, map(), guild_state()}.
get_voice_state(Request, State) ->
    guild_voice_state:get_voice_state(Request, State).

-spec get_voice_states_list(guild_state()) -> [voice_state()].
get_voice_states_list(State) ->
    guild_voice_state:get_voice_states_list(State).

-spec update_member_voice(map(), guild_state()) -> voice_reply().
update_member_voice(Request, State) ->
    guild_voice_member:update_member_voice(Request, State).

-spec disconnect_voice_user(map(), guild_state()) -> voice_reply().
disconnect_voice_user(Request, State) ->
    guild_voice_disconnect:disconnect_voice_user(Request, State).

-spec disconnect_voice_user_if_in_channel(map(), guild_state()) -> voice_reply().
disconnect_voice_user_if_in_channel(Request, State) ->
    guild_voice_disconnect:disconnect_voice_user_if_in_channel(Request, State).

-spec disconnect_all_voice_users_in_channel(map(), guild_state()) -> voice_reply().
disconnect_all_voice_users_in_channel(Request, State) ->
    guild_voice_disconnect:disconnect_all_voice_users_in_channel(Request, State).

-spec confirm_voice_connection_from_livekit(map(), guild_state()) -> voice_reply().
confirm_voice_connection_from_livekit(Request, State) ->
    guild_voice_connection:confirm_voice_connection_from_livekit(Request, State).

-spec move_member(map(), guild_state()) -> voice_reply().
move_member(Request, State) ->
    guild_voice_move:move_member(Request, State).

-spec send_voice_server_update_for_move(
    integer(), integer(), integer(), binary() | undefined, pid()
) ->
    ok.
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

-spec switch_voice_region_handler(map(), guild_state()) -> voice_reply().
switch_voice_region_handler(Request, State) ->
    guild_voice_region:switch_voice_region_handler(Request, State).

-spec switch_voice_region(integer(), integer(), pid()) -> ok | {error, term()}.
switch_voice_region(GuildId, ChannelId, GuildPid) ->
    guild_voice_region:switch_voice_region(GuildId, ChannelId, GuildPid).

-spec handle_virtual_channel_access_for_move(integer(), integer(), map(), pid()) -> ok.
handle_virtual_channel_access_for_move(UserId, ChannelId, _ConnectionsToMove, GuildPid) ->
    case guild_voice_server_state:guild_state_call(GuildPid, 10000) of
        State when is_map(State) ->
            maybe_grant_move_access(UserId, ChannelId, State, GuildPid);
        _ ->
            ok
    end.

-spec maybe_grant_move_access(integer(), integer(), map(), pid()) -> ok.
maybe_grant_move_access(UserId, ChannelId, State, GuildPid) ->
    Member = guild_permissions:find_member_by_user_id(UserId, State),
    case Member of
        undefined ->
            ok;
        _ ->
            maybe_grant_access_if_needed(UserId, ChannelId, State, GuildPid)
    end.

-spec maybe_grant_access_if_needed(integer(), integer(), map(), pid()) -> ok.
maybe_grant_access_if_needed(UserId, ChannelId, State, GuildPid) ->
    case has_voice_channel_access(UserId, ChannelId, State) of
        true ->
            ok;
        false ->
            gen_server:call(
                GuildPid,
                {add_virtual_channel_access, UserId, ChannelId},
                10000
            )
    end.

-spec has_voice_channel_access(integer(), integer(), map()) -> boolean().
has_voice_channel_access(UserId, ChannelId, State) ->
    Permissions = guild_permissions:get_member_permissions(UserId, ChannelId, State),
    ViewPerm = constants:view_channel_permission(),
    ConnectPerm = constants:connect_permission(),
    HasView = permission_bits:has(Permissions, ViewPerm),
    HasConnect = permission_bits:has(Permissions, ConnectPerm),
    HasView andalso HasConnect.

-spec cleanup_virtual_access_on_disconnect(integer(), pid()) -> ok.
cleanup_virtual_access_on_disconnect(UserId, GuildPid) ->
    GuildId = resolve_guild_id_from_pid(GuildPid),
    TargetPid = resolve_voice_server(GuildId, GuildPid),
    gen_server:cast(TargetPid, {cleanup_virtual_access_for_user, UserId}).

-spec resolve_guild_id_from_pid(pid()) -> integer() | undefined.
resolve_guild_id_from_pid(GuildPid) ->
    try guild_voice_server_state:guild_id_call(GuildPid, 5000) of
        GuildId when is_integer(GuildId) ->
            GuildId;
        _ ->
            undefined
    catch
        _:_ -> undefined
    end.

-spec resolve_voice_server(integer() | undefined, pid()) -> pid().
resolve_voice_server(GuildId, FallbackPid) ->
    guild_voice_server:resolve(GuildId, FallbackPid).
