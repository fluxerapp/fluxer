%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_move_execute).
-typing([eqwalizer]).

-export([
    handle_move/7,
    send_single_voice_server_update/4
]).

-export_type([
    guild_state/0,
    voice_state/0,
    voice_state_map/0,
    voice_reply/0
]).

-type guild_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.
-type voice_reply() :: {reply, map() | {error, atom(), atom()}, guild_state()}.

-spec handle_move(
    voice_state_map(),
    integer() | null,
    integer(),
    integer(),
    binary() | null,
    voice_state_map(),
    guild_state()
) -> voice_reply().
handle_move(
    ConnectionsToMove, ChannelId, UserId, ModeratorId, ConnectionId, VoiceStates, State
) ->
    case maps:size(ConnectionsToMove) of
        0 ->
            Error = move_error(ConnectionId),
            {reply, Error, State};
        _ ->
            dispatch_move(
                ConnectionsToMove,
                ChannelId,
                UserId,
                ModeratorId,
                ConnectionId,
                VoiceStates,
                State
            )
    end.

-spec move_error(binary() | null) -> map() | {error, atom(), atom()}.
move_error(null) ->
    gateway_errors:error(voice_user_not_in_voice);
move_error(_ConnectionId) ->
    gateway_errors:error(voice_connection_not_found).

-spec dispatch_move(
    voice_state_map(),
    integer() | null,
    integer(),
    integer(),
    binary() | null,
    voice_state_map(),
    guild_state()
) -> voice_reply().
dispatch_move(Conns, null, UserId, _ModId, ConnectionId, VoiceStates, State) ->
    logger:debug(
        "Disconnect move requested",
        #{user_id => UserId, connection_id => ConnectionId}
    ),
    handle_disconnect_move(Conns, UserId, VoiceStates, State);
dispatch_move(Conns, ChannelIdValue, UserId, ModeratorId, ConnectionId, VoiceStates, State) ->
    logger:debug(
        "Channel move requested",
        #{user_id => UserId, channel_id => ChannelIdValue, connection_id => ConnectionId}
    ),
    handle_channel_move(Conns, ChannelIdValue, UserId, ModeratorId, VoiceStates, State).

-spec handle_disconnect_move(voice_state_map(), integer(), voice_state_map(), guild_state()) ->
    {reply, map(), guild_state()}.
handle_disconnect_move(ConnectionsToMove, UserId, VoiceStates, State) ->
    ok = guild_voice_disconnect_broadcast:purge_count_cache(maps:keys(ConnectionsToMove)),
    NewVoiceStates = remove_connections(ConnectionsToMove, VoiceStates),
    NewState = State#{voice_states => NewVoiceStates},
    BroadcastSnapshot = build_broadcast_snapshot(NewState),
    spawn(fun() -> broadcast_disconnects(ConnectionsToMove, BroadcastSnapshot) end),
    Reply = #{success => true, user_id => UserId, connections_moved => ConnectionsToMove},
    {reply, Reply, NewState}.

-spec remove_connections(voice_state_map(), voice_state_map()) -> voice_state_map().
remove_connections(ToRemove, VoiceStates) ->
    maps:fold(
        fun(ConnId, _VS, Acc) -> maps:remove(ConnId, Acc) end,
        VoiceStates,
        ToRemove
    ).

-spec build_broadcast_snapshot(guild_state()) -> map().
build_broadcast_snapshot(State) ->
    maps:with([id, sessions, guild_pid, data, virtual_channel_access], State).

-spec broadcast_disconnects(voice_state_map(), guild_state()) -> ok.
broadcast_disconnects(ConnectionsToMove, NewState) ->
    maps:foreach(
        fun(_ConnId, VoiceState) ->
            OldChannelIdBin = maps:get(<<"channel_id">>, VoiceState, null),
            DisconnectVS = VoiceState#{<<"channel_id">> => null},
            guild_voice_broadcast:broadcast_voice_state_update(
                DisconnectVS, NewState, OldChannelIdBin
            )
        end,
        ConnectionsToMove
    ).

-spec handle_channel_move(
    voice_state_map(), integer(), integer(), integer(), voice_state_map(), guild_state()
) -> voice_reply().
handle_channel_move(Conns, ChannelIdValue, UserId, ModeratorId, VoiceStates, State) ->
    Channel = guild_voice_member:find_channel_by_id(ChannelIdValue, State),
    case Channel of
        undefined ->
            {reply, gateway_errors:error(voice_channel_not_found), State};
        _ ->
            handle_valid_channel(
                Conns, ChannelIdValue, UserId, ModeratorId, VoiceStates, Channel, State
            )
    end.

-spec handle_valid_channel(
    voice_state_map(),
    integer(),
    integer(),
    integer(),
    voice_state_map(),
    map(),
    guild_state()
) -> voice_reply().
handle_valid_channel(Conns, ChannelId, UserId, ModId, VoiceStates, Channel, State) ->
    StateWithPending = mark_pending_states(UserId, ChannelId, State),
    ChannelType = map_utils:get_integer(Channel, <<"type">>, undefined),
    case ChannelType of
        2 ->
            check_perms_and_execute(
                Conns, ChannelId, UserId, ModId, VoiceStates, StateWithPending
            );
        _ ->
            {reply, gateway_errors:error(voice_channel_not_voice), State}
    end.

-spec mark_pending_states(integer(), integer(), guild_state()) -> guild_state().
mark_pending_states(UserId, ChannelId, State) ->
    guild_virtual_channel_access:mark_transition_flags(UserId, ChannelId, State).

-spec check_perms_and_execute(
    voice_state_map(), integer(), integer(), integer(), voice_state_map(), guild_state()
) -> voice_reply().
check_perms_and_execute(Conns, ChannelId, UserId, ModId, VoiceStates, State) ->
    ViewPerm = constants:view_channel_permission(),
    ConnectPerm = constants:connect_permission(),
    ModPerms = guild_permissions:get_member_permissions(ModId, ChannelId, State),
    ModHasConnect = permission_bits:has(ModPerms, ConnectPerm),
    ModHasView = permission_bits:has(ModPerms, ViewPerm),
    case ModHasConnect andalso ModHasView of
        false ->
            {reply, gateway_errors:error(voice_moderator_missing_connect), State};
        true ->
            execute_move(Conns, ChannelId, UserId, VoiceStates, State)
    end.

-spec execute_move(voice_state_map(), integer(), integer(), voice_state_map(), guild_state()) ->
    {reply, map(), guild_state()}.
execute_move(ConnectionsToMove, ChannelIdValue, UserId, VoiceStates, State) ->
    StatePending = mark_pending_states(UserId, ChannelIdValue, State),
    logger:debug(
        "Executing voice channel move",
        #{user_id => UserId, channel_id => ChannelIdValue}
    ),
    ok = guild_voice_disconnect_broadcast:purge_count_cache(maps:keys(ConnectionsToMove)),
    NewVoiceStates = remove_connections(ConnectionsToMove, VoiceStates),
    StateAfterDisconnect = StatePending#{voice_states => NewVoiceStates},
    StateWithVA = maybe_add_virtual_access(UserId, ChannelIdValue, StateAfterDisconnect),
    StateCleaned = cleanup_stale_virtual_access(UserId, StateWithVA),
    BroadcastSnapshot = build_broadcast_snapshot(StateCleaned),
    spawn(fun() -> broadcast_disconnects(ConnectionsToMove, BroadcastSnapshot) end),
    SessionData = extract_session_data(ConnectionsToMove),
    Reply = #{
        success => true,
        needs_token => true,
        session_data => SessionData,
        connections_to_move => ConnectionsToMove
    },
    {reply, Reply, StateCleaned}.

-spec cleanup_stale_virtual_access(integer(), guild_state()) -> guild_state().
cleanup_stale_virtual_access(UserId, State) ->
    guild_voice_disconnect_user:cleanup_virtual_channel_access_for_user(UserId, State).

-spec extract_session_data(voice_state_map()) -> [map()].
extract_session_data(ConnectionsToMove) ->
    {_ConnIds, SessionData} = maps:fold(
        fun(ConnId, VoiceState, {AccIds, AccData}) ->
            Info = guild_voice_state:extract_session_info_from_voice_state(ConnId, VoiceState),
            {[ConnId | AccIds], [Info | AccData]}
        end,
        {[], []},
        ConnectionsToMove
    ),
    SessionData.

-spec maybe_add_virtual_access(integer(), integer(), guild_state()) -> guild_state().
maybe_add_virtual_access(UserId, ChannelId, State) ->
    Member = guild_permissions:find_member_by_user_id(UserId, State),
    case Member of
        undefined -> State;
        _ -> maybe_grant_virtual_access(UserId, ChannelId, State)
    end.

-spec maybe_grant_virtual_access(integer(), integer(), guild_state()) -> guild_state().
maybe_grant_virtual_access(UserId, ChannelId, State) ->
    Permissions = guild_permissions:get_member_permissions(UserId, ChannelId, State),
    ViewPerm = constants:view_channel_permission(),
    ConnectPerm = constants:connect_permission(),
    HasView = permission_bits:has(Permissions, ViewPerm),
    HasConnect = permission_bits:has(Permissions, ConnectPerm),
    case HasView andalso HasConnect of
        true ->
            State;
        false ->
            NewState = guild_virtual_channel_access:add_virtual_access(
                UserId, ChannelId, State
            ),
            guild_virtual_channel_access:dispatch_channel_visibility_change(
                UserId, ChannelId, add, NewState
            ),
            NewState
    end.

-spec send_single_voice_server_update(integer(), integer(), map(), pid()) -> ok.
send_single_voice_server_update(GuildId, ChannelId, SessionInfo, GuildPid) ->
    Member = maps:get(member, SessionInfo),
    case member_user_id(Member) of
        undefined ->
            ok;
        UserId ->
            fetch_state_and_send(GuildId, ChannelId, SessionInfo, GuildPid, UserId, Member)
    end.

-spec member_user_id(map()) -> integer() | undefined.
member_user_id(Member) ->
    User = map_utils:ensure_map(
        maps:get(<<"user">>, map_utils:ensure_map(Member), #{})
    ),
    guild_voice_connection_normalize:normalize_positive_snowflake(
        maps:get(<<"id">>, User, undefined)
    ).

-spec fetch_state_and_send(integer(), integer(), map(), pid(), integer(), map()) -> ok.
fetch_state_and_send(GuildId, ChannelId, SessionInfo, GuildPid, UserId, Member) ->
    try guild_voice_server_state:guild_state_call(GuildPid, 10000) of
        StateData when is_map(StateData) ->
            request_and_broadcast(
                GuildId, ChannelId, SessionInfo, GuildPid, UserId, Member, StateData
            );
        _ ->
            ok
    catch
        exit:_Reason -> ok;
        error:_Reason -> ok
    end.

-define(MOVE_PENDING_TTL_MS, 30000).

-spec request_and_broadcast(integer(), integer(), map(), pid(), integer(), map(), map()) -> ok.
request_and_broadcast(GuildId, ChannelId, SessionInfo, GuildPid, UserId, Member, StateData) ->
    OldConnectionId = maps:get(connection_id, SessionInfo, null),
    VoicePerms = voice_utils:compute_voice_permissions(UserId, ChannelId, StateData),
    TokenNonce = voice_utils:generate_token_nonce(),
    Latitude = maps:get(latitude, SessionInfo, undefined),
    Longitude = maps:get(longitude, SessionInfo, undefined),
    case
        guild_voice_connection:request_voice_token(
            GuildId,
            ChannelId,
            UserId,
            OldConnectionId,
            VoicePerms,
            TokenNonce,
            Latitude,
            Longitude
        )
    of
        {ok, TokenData} ->
            do_broadcast(
                GuildId,
                ChannelId,
                SessionInfo,
                GuildPid,
                UserId,
                Member,
                StateData,
                TokenData,
                VoicePerms,
                TokenNonce
            );
        {error, _Reason} ->
            ok
    end.

-spec do_broadcast(
    integer(),
    integer(),
    map(),
    pid(),
    integer(),
    map(),
    map(),
    map(),
    voice_utils:voice_permissions(),
    binary()
) -> ok.
do_broadcast(
    GuildId,
    ChannelId,
    SI,
    GuildPid,
    UserId,
    Member,
    StateData,
    TokenData,
    VoicePerms,
    TokenNonce
) ->
    Token = maps:get(token, TokenData),
    Endpoint = maps:get(endpoint, TokenData),
    NewConnId = maps:get(connection_id, TokenData),
    SessionId = maps:get(session_id, SI),
    Suppress =
        case maps:get(can_speak, VoicePerms, true) of
            false -> true;
            _ -> false
        end,
    PendingMeta = build_pending_metadata(
        GuildId, ChannelId, SI, UserId, Member, SessionId, Suppress, TokenData, TokenNonce
    ),
    _ = store_pending_connection(GuildId, GuildPid, NewConnId, PendingMeta),
    guild_voice_broadcast:broadcast_voice_server_update_to_session(
        GuildId, ChannelId, SessionId, Token, Endpoint, NewConnId, StateData
    ).

-spec build_pending_metadata(
    integer(), integer(), map(), integer(), map(), binary(), boolean(), map(), binary()
) -> map().
build_pending_metadata(
    GuildId, ChannelId, SI, UserId, Member, SessionId, Suppress, TokenData, TokenNonce
) ->
    Now = erlang:system_time(millisecond),
    #{
        user_id => UserId,
        guild_id => GuildId,
        channel_id => ChannelId,
        session_id => SessionId,
        self_mute => maps:get(self_mute, SI, false),
        self_deaf => maps:get(self_deaf, SI, false),
        self_video => maps:get(self_video, SI, false),
        self_stream => maps:get(self_stream, SI, false),
        is_mobile => maps:get(is_mobile, SI, false),
        suppress => Suppress,
        server_mute => maps:get(<<"mute">>, Member, false),
        server_deaf => maps:get(<<"deaf">>, Member, false),
        member => Member,
        latitude => maps:get(latitude, SI, undefined),
        longitude => maps:get(longitude, SI, undefined),
        viewer_stream_keys => [],
        e2ee_capable => maps:get(e2ee_capable, SI, false),
        region_id => maps:get(region_id, TokenData, undefined),
        server_id => maps:get(server_id, TokenData, undefined),
        token_nonce => TokenNonce,
        created_at => Now,
        expires_at => Now + ?MOVE_PENDING_TTL_MS
    }.

-spec store_pending_connection(integer(), pid(), binary(), map()) -> ok.
store_pending_connection(GuildId, GuildPid, ConnectionId, Metadata) ->
    TargetPid = guild_voice_server:resolve(GuildId, GuildPid),
    gen_server:call(TargetPid, {store_pending_connection, ConnectionId, Metadata}, 10000).
