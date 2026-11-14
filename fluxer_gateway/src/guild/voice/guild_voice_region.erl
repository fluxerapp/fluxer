%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_region).

-typing([eqwalizer]).

-export([switch_voice_region_handler/2]).
-export([switch_voice_region/3]).
-export([switch_to_sfu/2]).

-export_type([
    guild_state/0,
    guild_reply/1,
    voice_state/0
]).

-type guild_state() :: map().
-type guild_reply(T) :: {reply, T | {error, atom(), atom()}, guild_state()}.
-type voice_state() :: map().

-spec switch_voice_region_handler(map(), guild_state()) -> guild_reply(map()).
switch_voice_region_handler(Request, State) ->
    #{channel_id := ChannelId} = Request,
    Channel = guild_permissions_check:find_channel_by_id(ChannelId, State),
    case Channel of
        undefined ->
            {reply, gateway_errors:error(voice_channel_not_found), State};
        _ ->
            switch_voice_region_channel_reply(Channel, ChannelId, State)
    end.

-spec switch_voice_region_channel_reply(map(), integer(), guild_state()) -> guild_reply(map()).
switch_voice_region_channel_reply(Channel, ChannelId, State) ->
    case map_utils:get_integer(Channel, <<"type">>, undefined) of
        2 -> {reply, #{success => true}, convert_to_sfu(Channel, ChannelId, State)};
        _ -> {reply, gateway_errors:error(voice_channel_not_voice), State}
    end.

-spec switch_to_sfu(integer(), guild_state()) -> guild_state().
switch_to_sfu(ChannelId, State) ->
    Channel = guild_permissions_check:find_channel_by_id(ChannelId, State),
    NewState = convert_to_sfu(Channel, ChannelId, State),
    ok = spawn_switch_if_converted(ChannelId, State, NewState),
    NewState.

-spec spawn_switch_if_converted(integer(), guild_state(), guild_state()) -> ok.
spawn_switch_if_converted(_ChannelId, State, State) ->
    ok;
spawn_switch_if_converted(ChannelId, _State, #{id := GuildId} = NewState) ->
    GuildPid = maps:get(guild_pid, NewState, self()),
    spawn(fun() -> switch_voice_region(GuildId, ChannelId, GuildPid) end),
    ok.

-spec convert_to_sfu(map() | undefined, integer(), guild_state()) -> guild_state().
convert_to_sfu(Channel, ChannelId, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    P2pVoiceStates = maps:filter(
        fun(_ConnectionId, VoiceState) -> voice_p2p:is_p2p(VoiceState) end,
        voice_state_utils:channel_voice_states(ChannelId, VoiceStates)
    ),
    case voice_p2p:pinned(Channel) orelse maps:size(P2pVoiceStates) =:= 0 of
        true -> State;
        false -> mark_voice_states_sfu(P2pVoiceStates, VoiceStates, State)
    end.

-spec mark_voice_states_sfu(map(), map(), guild_state()) -> guild_state().
mark_voice_states_sfu(P2pVoiceStates, VoiceStates, State) ->
    Converted = maps:map(
        fun(_ConnectionId, VoiceState) -> voice_p2p:to_sfu(VoiceState) end,
        P2pVoiceStates
    ),
    NewState = State#{voice_states => maps:merge(VoiceStates, Converted)},
    maps:foreach(
        fun(_ConnectionId, VoiceState) ->
            guild_voice_broadcast:broadcast_voice_state_update(
                VoiceState, NewState, maps:get(<<"channel_id">>, VoiceState, null)
            )
        end,
        Converted
    ),
    NewState.

-spec switch_voice_region(integer(), integer(), pid()) -> ok.
switch_voice_region(GuildId, ChannelId, GuildPid) ->
    case guild_voice_server_state:guild_state_call(GuildPid, 10000) of
        State when is_map(State) ->
            do_switch_voice_region(GuildId, ChannelId, GuildPid, State);
        _ ->
            ok
    end.

-spec do_switch_voice_region(integer(), integer(), pid(), map()) -> ok.
do_switch_voice_region(GuildId, ChannelId, GuildPid, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    UsersInChannel = collect_users_in_channel(VoiceStates, ChannelId),
    lists:foreach(
        fun(UserInfo) ->
            maybe_send_region_switch_update(GuildId, ChannelId, GuildPid, State, UserInfo)
        end,
        UsersInChannel
    ).

-spec collect_users_in_channel(map(), integer()) ->
    [{integer(), binary() | undefined, binary(), voice_state()}].
collect_users_in_channel(VoiceStates, ChannelId) ->
    maps:fold(
        fun(ConnectionId, VoiceState, Acc) ->
            collect_user_in_channel(ConnectionId, VoiceState, ChannelId, Acc)
        end,
        [],
        VoiceStates
    ).

-spec maybe_send_region_switch_update(
    integer(),
    integer(),
    pid(),
    map(),
    {integer(), binary() | undefined, binary(), voice_state()}
) -> ok.
maybe_send_region_switch_update(
    _GuildId, _ChannelId, _GuildPid, _State, {_UserId, undefined, _ConnId, _VS}
) ->
    ok;
maybe_send_region_switch_update(
    GuildId, ChannelId, GuildPid, State, {UserId, SessionId, ExistingConnectionId, VoiceState}
) ->
    request_and_broadcast_region_switch(
        GuildId,
        ChannelId,
        UserId,
        SessionId,
        ExistingConnectionId,
        VoiceState,
        GuildPid,
        State
    ).

-spec collect_user_in_channel(binary(), voice_state(), integer(), list()) -> list().
collect_user_in_channel(ConnectionId, VoiceState, ChannelId, Acc) ->
    case voice_state_utils:voice_state_channel_id(VoiceState) of
        ChannelId -> collect_user_voice_state(ConnectionId, VoiceState, Acc);
        _ -> Acc
    end.

-spec collect_user_voice_state(binary(), voice_state(), list()) -> list().
collect_user_voice_state(ConnectionId, VoiceState, Acc) ->
    case {voice_p2p:is_p2p(VoiceState), voice_state_utils:voice_state_user_id(VoiceState)} of
        {true, _UserId} ->
            Acc;
        {false, undefined} ->
            Acc;
        {false, UserId} ->
            SessionId = maps:get(<<"session_id">>, VoiceState, undefined),
            [{UserId, SessionId, ConnectionId, VoiceState} | Acc]
    end.

-spec request_and_broadcast_region_switch(
    integer(),
    integer(),
    integer(),
    binary(),
    binary(),
    voice_state(),
    pid(),
    map()
) -> ok.
request_and_broadcast_region_switch(
    GuildId,
    ChannelId,
    UserId,
    SessionId,
    ExistingConnectionId,
    ExistingVoiceState,
    GuildPid,
    State
) ->
    VoicePerms = voice_utils:compute_voice_permissions(UserId, ChannelId, State),
    TokenNonce = voice_utils:generate_token_nonce(),
    Lat = maps:get(<<"latitude">>, ExistingVoiceState, undefined),
    Lng = maps:get(<<"longitude">>, ExistingVoiceState, undefined),
    TokenResult = guild_voice_connection_token:request_voice_token(
        GuildId,
        ChannelId,
        UserId,
        ExistingConnectionId,
        VoicePerms,
        TokenNonce,
        Lat,
        Lng
    ),
    Switch = #{
        guild_id => GuildId,
        channel_id => ChannelId,
        user_id => UserId,
        session_id => SessionId,
        existing_voice_state => ExistingVoiceState,
        guild_pid => GuildPid,
        state => State,
        token_nonce => TokenNonce
    },
    handle_region_token_result(TokenResult, Switch).

-spec handle_region_token_result({ok, map()} | {error, term()}, map()) -> ok.
handle_region_token_result({ok, TokenData}, Switch) ->
    broadcast_region_switch(
        maps:get(guild_id, Switch),
        maps:get(channel_id, Switch),
        maps:get(user_id, Switch),
        maps:get(session_id, Switch),
        maps:get(existing_voice_state, Switch),
        maps:get(guild_pid, Switch),
        maps:get(state, Switch),
        maps:get(token_nonce, Switch),
        TokenData
    );
handle_region_token_result({error, _Reason}, _Switch) ->
    ok.

-spec broadcast_region_switch(
    integer(), integer(), integer(), binary(), voice_state(), pid(), map(), binary(), map()
) -> ok.
broadcast_region_switch(
    GuildId,
    ChannelId,
    UserId,
    SessionId,
    ExistingVoiceState,
    GuildPid,
    State,
    TokenNonce,
    TokenData
) ->
    Token = maps:get(token, TokenData),
    Endpoint = maps:get(endpoint, TokenData),
    ConnectionId = maps:get(connection_id, TokenData),
    PendingMetadata = build_pending_metadata(
        UserId, GuildId, ChannelId, SessionId, ExistingVoiceState, TokenNonce, TokenData
    ),
    _ = store_pending_connection(GuildId, GuildPid, ConnectionId, PendingMetadata),
    guild_voice_broadcast:broadcast_voice_server_update_to_session(
        GuildId, ChannelId, SessionId, Token, Endpoint, ConnectionId, State
    ).

-spec build_pending_metadata(
    integer(), integer(), integer(), binary(), voice_state(), binary(), map()
) -> map().
build_pending_metadata(
    UserId, GuildId, ChannelId, SessionId, ExistingVoiceState, TokenNonce, TokenData
) ->
    Now = erlang:system_time(millisecond),
    #{
        user_id => UserId,
        guild_id => GuildId,
        channel_id => ChannelId,
        session_id => SessionId,
        self_mute => maps:get(<<"self_mute">>, ExistingVoiceState, false),
        self_deaf => maps:get(<<"self_deaf">>, ExistingVoiceState, false),
        self_video => maps:get(<<"self_video">>, ExistingVoiceState, false),
        self_stream => maps:get(<<"self_stream">>, ExistingVoiceState, false),
        is_mobile => maps:get(<<"is_mobile">>, ExistingVoiceState, false),
        server_mute => maps:get(<<"mute">>, ExistingVoiceState, false),
        server_deaf => maps:get(<<"deaf">>, ExistingVoiceState, false),
        member => maps:get(<<"member">>, ExistingVoiceState, #{}),
        latitude => maps:get(<<"latitude">>, ExistingVoiceState, undefined),
        longitude => maps:get(<<"longitude">>, ExistingVoiceState, undefined),
        region_id => maps:get(region_id, TokenData, undefined),
        server_id => maps:get(server_id, TokenData, undefined),
        viewer_stream_keys => [],
        token_nonce => TokenNonce,
        created_at => Now,
        expires_at => Now + 30000
    }.

-spec store_pending_connection(integer(), pid(), binary(), map()) -> ok.
store_pending_connection(GuildId, GuildPid, ConnectionId, Metadata) ->
    TargetPid = guild_voice_server:resolve(GuildId, GuildPid),
    gen_server:call(
        TargetPid,
        {store_pending_connection, ConnectionId, Metadata},
        10000
    ).
