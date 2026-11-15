%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_permission_sync).

-typing([eqwalizer]).

-export([
    sync_all_voice_permissions_for_channels/2,
    sync_roles_after_change/3,
    maybe_sync_permissions_on_member_update/2
]).

-export_type([
    guild_state/0,
    voice_state/0,
    user_id/0,
    channel_id/0
]).

-type guild_state() :: map().
-type voice_state() :: map().
-type user_id() :: integer().
-type channel_id() :: integer().
-type target() :: {user_id(), binary()}.

-define(RESYNC_TIMEOUT_MS, 15000).

-spec sync_user_voice_permissions(user_id(), guild_state()) -> ok.
sync_user_voice_permissions(UserId, State) ->
    VoiceStates = guild_voice_lifecycle:authoritative_voice_states(State),
    case state_guild_id(State) of
        undefined ->
            ok;
        GuildId ->
            Targets = sync_matching_user_voice_states(GuildId, UserId, VoiceStates, State),
            dispatch_livekit_resync(GuildId, Targets)
    end.

-spec sync_matching_user_voice_states(integer(), user_id(), map(), guild_state()) -> [target()].
sync_matching_user_voice_states(GuildId, UserId, VoiceStates, State) ->
    maps:fold(
        fun(_ConnId, VoiceState, Acc) ->
            maybe_sync_user_voice_state(GuildId, UserId, VoiceState, State) ++ Acc
        end,
        [],
        VoiceStates
    ).

-spec maybe_sync_user_voice_state(integer(), user_id(), voice_state(), guild_state()) ->
    [target()].
maybe_sync_user_voice_state(GuildId, UserId, VoiceState, State) ->
    case voice_state_utils:voice_state_user_id(VoiceState) of
        UserId -> sync_voice_state_permissions(GuildId, UserId, VoiceState, State);
        _ -> []
    end.

-spec sync_all_voice_permissions_for_channels([channel_id()], guild_state()) -> ok.
sync_all_voice_permissions_for_channels([], _State) ->
    ok;
sync_all_voice_permissions_for_channels(ChannelIds, State) ->
    VoiceStates = guild_voice_lifecycle:authoritative_voice_states(State),
    case state_guild_id(State) of
        undefined ->
            ok;
        GuildId ->
            Targets = sync_channels(GuildId, ChannelIds, VoiceStates, State),
            dispatch_livekit_resync(GuildId, Targets)
    end.

-spec sync_channels(integer(), [channel_id()], map(), guild_state()) -> [target()].
sync_channels(GuildId, ChannelIds, VoiceStates, State) ->
    lists:append([
        do_sync_channel_permissions(GuildId, ChannelId, VoiceStates, State)
     || ChannelId <- ChannelIds
    ]).

-spec do_sync_channel_permissions(integer(), channel_id(), map(), guild_state()) -> [target()].
do_sync_channel_permissions(GuildId, ChannelId, VoiceStates, State) ->
    maps:fold(
        fun(_ConnId, VoiceState, Acc) ->
            maybe_sync_channel_voice_state(GuildId, ChannelId, VoiceState, State) ++ Acc
        end,
        [],
        VoiceStates
    ).

-spec maybe_sync_channel_voice_state(integer(), channel_id(), voice_state(), guild_state()) ->
    [target()].
maybe_sync_channel_voice_state(GuildId, ChannelId, VoiceState, State) ->
    case voice_state_utils:voice_state_channel_id(VoiceState) of
        ChannelId -> sync_channel_voice_state_permissions(GuildId, VoiceState, State);
        _ -> []
    end.

-spec sync_channel_voice_state_permissions(integer(), voice_state(), guild_state()) ->
    [target()].
sync_channel_voice_state_permissions(GuildId, VoiceState, State) ->
    case voice_state_utils:voice_state_user_id(VoiceState) of
        undefined -> [];
        UserId -> sync_voice_state_permissions(GuildId, UserId, VoiceState, State)
    end.

-spec maybe_sync_permissions_on_member_update(map(), guild_state()) -> ok.
maybe_sync_permissions_on_member_update(MemberUpdate, State) ->
    UserId = guild_members_common:member_user_id(MemberUpdate),
    case UserId of
        undefined ->
            ok;
        _ ->
            sync_user_voice_permissions(UserId, State)
    end.

-spec sync_voice_state_permissions(integer(), user_id(), voice_state(), guild_state()) ->
    [target()].
sync_voice_state_permissions(GuildId, UserId, VoiceState, State) ->
    ChannelId = voice_state_utils:voice_state_channel_id(VoiceState),
    ConnectionId = maps:get(<<"connection_id">>, VoiceState, undefined),
    case {ChannelId, ConnectionId} of
        {undefined, _} ->
            [];
        {_, undefined} ->
            [];
        {ChId, ConnId} when is_integer(ChId), is_binary(ConnId) ->
            sync_validated_voice_state(GuildId, UserId, ChId, ConnId, VoiceState, State)
    end.

-spec sync_validated_voice_state(
    integer(), user_id(), channel_id(), binary(), voice_state(), guild_state()
) -> [target()].
sync_validated_voice_state(GuildId, UserId, ChId, ConnId, VoiceState, State) ->
    case user_has_base_voice_access(UserId, ChId, State) of
        true ->
            VoicePermissions0 = voice_utils:compute_voice_permissions(UserId, ChId, State),
            VoicePermissions = VoicePermissions0#{
                deaf => maps:get(<<"deaf">>, VoiceState, false)
            },
            ok = maybe_clear_self_stream(ChId, VoiceState, VoicePermissions, State),
            dispatch_permission_update(GuildId, ChId, UserId, ConnId, VoicePermissions, State);
        false ->
            ok = dispatch_force_disconnect(GuildId, ChId, UserId, ConnId, State),
            []
    end.

-spec maybe_clear_self_stream(
    channel_id(), voice_state(), voice_utils:voice_permissions(), guild_state()
) -> ok.
maybe_clear_self_stream(ChId, VoiceState, #{can_stream := false}, State) ->
    SelfStream = maps:get(<<"self_stream">>, VoiceState, false),
    SelfVideo = maps:get(<<"self_video">>, VoiceState, false),
    case type_conv:is_true(SelfStream) orelse type_conv:is_true(SelfVideo) of
        false ->
            ok;
        true ->
            Cleared = VoiceState#{
                <<"self_stream">> => false,
                <<"self_video">> => false
            },
            guild_voice_broadcast:broadcast_voice_state_update(
                Cleared, State, integer_to_binary(ChId)
            )
    end;
maybe_clear_self_stream(_ChId, _VoiceState, _VoicePermissions, _State) ->
    ok.

-spec user_has_base_voice_access(user_id(), channel_id(), guild_state()) -> boolean().
user_has_base_voice_access(UserId, ChannelId, State) ->
    case guild_virtual_channel_access:has_voice_access(UserId, ChannelId, State) of
        true ->
            true;
        false ->
            Permissions = guild_permissions:get_member_permissions(UserId, ChannelId, State),
            ViewPerm = constants:view_channel_permission(),
            ConnectPerm = constants:connect_permission(),
            HasView = bitset:has(Permissions, ViewPerm),
            HasConnect = bitset:has(Permissions, ConnectPerm),
            HasView andalso HasConnect
    end.

-spec dispatch_force_disconnect(
    integer(), channel_id(), user_id(), binary(), guild_state()
) -> ok.
dispatch_force_disconnect(GuildId, ChannelId, UserId, ConnectionId, State) ->
    case maps:get(test_permission_sync_fun, State, undefined) of
        Fun when is_function(Fun, 5) ->
            _ = Fun(GuildId, ChannelId, UserId, ConnectionId, #{disconnected => true}),
            ok;
        _ ->
            spawn_force_disconnect(GuildId, ChannelId, UserId, ConnectionId),
            ok
    end.

-spec spawn_force_disconnect(integer(), channel_id(), user_id(), binary()) -> pid().
spawn_force_disconnect(GuildId, ChannelId, UserId, ConnectionId) ->
    spawn(fun() ->
        guild_voice_disconnect_user:force_disconnect_participant(
            GuildId, ChannelId, UserId, ConnectionId
        )
    end).

-spec dispatch_permission_update(
    integer(), channel_id(), user_id(), binary(), voice_utils:voice_permissions(), guild_state()
) ->
    [target()].
dispatch_permission_update(GuildId, ChannelId, UserId, ConnectionId, VoicePermissions, State) ->
    case maps:get(test_permission_sync_fun, State, undefined) of
        Fun when is_function(Fun, 5) ->
            _ = Fun(GuildId, ChannelId, UserId, ConnectionId, VoicePermissions),
            [];
        _ ->
            [{UserId, ConnectionId}]
    end.

-spec dispatch_livekit_resync(integer(), [target()]) -> ok.
dispatch_livekit_resync(_GuildId, []) ->
    ok;
dispatch_livekit_resync(GuildId, Targets) ->
    GuildPid = self(),
    _ = spawn(fun() ->
        VoicePid = guild_voice_server:resolve(GuildId, GuildPid),
        Request = #{livekit_resync => Targets},
        try gen_server:call(VoicePid, {update_member_voice, Request}, ?RESYNC_TIMEOUT_MS) of
            _ -> ok
        catch
            exit:_ -> ok
        end
    end),
    ok.

-spec sync_roles_after_change([integer()], guild_state(), guild_state()) -> ok.
sync_roles_after_change([], _OldState, _State) ->
    ok;
sync_roles_after_change(RoleIds, OldState, State) ->
    case state_guild_id(State) of
        undefined ->
            ok;
        GuildId ->
            Affected = affected_role_users(GuildId, RoleIds, OldState, State),
            Targets = sync_affected_voice_states(GuildId, Affected, State),
            dispatch_livekit_resync(GuildId, Targets)
    end.

-spec sync_affected_voice_states(integer(), all | sets:set(user_id()), guild_state()) ->
    [target()].
sync_affected_voice_states(GuildId, Affected, State) ->
    maps:fold(
        fun(_ConnId, VoiceState, Acc) ->
            maybe_sync_affected_voice_state(GuildId, Affected, VoiceState, State) ++ Acc
        end,
        [],
        guild_voice_lifecycle:authoritative_voice_states(State)
    ).

-spec affected_role_users(integer(), [integer()], guild_state(), guild_state()) ->
    all | sets:set(user_id()).
affected_role_users(GuildId, RoleIds, OldState, State) ->
    case lists:member(GuildId, RoleIds) of
        true ->
            all;
        false ->
            OldIndex = guild_data_index_members:member_role_index(
                maps:get(data, OldState, #{})
            ),
            NewIndex = guild_data_index_members:member_role_index(maps:get(data, State, #{})),
            sets:from_list(
                lists:append([
                    maps:keys(maps:get(RoleId, OldIndex, #{})) ++
                        maps:keys(maps:get(RoleId, NewIndex, #{}))
                 || RoleId <- RoleIds
                ])
            )
    end.

-spec maybe_sync_affected_voice_state(
    integer(), all | sets:set(user_id()), voice_state(), guild_state()
) -> [target()].
maybe_sync_affected_voice_state(GuildId, Affected, VoiceState, State) ->
    case voice_state_utils:voice_state_user_id(VoiceState) of
        undefined -> [];
        UserId -> sync_if_affected(GuildId, UserId, Affected, VoiceState, State)
    end.

-spec sync_if_affected(
    integer(), user_id(), all | sets:set(user_id()), voice_state(), guild_state()
) -> [target()].
sync_if_affected(GuildId, UserId, Affected, VoiceState, State) ->
    case is_affected(UserId, Affected) of
        true -> sync_voice_state_permissions(GuildId, UserId, VoiceState, State);
        false -> []
    end.

-spec is_affected(user_id(), all | sets:set(user_id())) -> boolean().
is_affected(_UserId, all) -> true;
is_affected(UserId, Affected) -> sets:is_element(UserId, Affected).

-spec state_guild_id(guild_state()) -> integer() | undefined.
state_guild_id(State) ->
    snowflake_id:parse_maybe(
        maps:get(id, State, undefined)
    ).
