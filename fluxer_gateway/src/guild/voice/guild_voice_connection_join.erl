%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_connection_join).

-typing([eqwalizer]).

-export([handle_new_connection/5]).

-export_type([
    guild_state/0,
    voice_state/0,
    voice_state_map/0,
    context/0
]).

-type guild_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.
-type context() :: guild_voice_connection_util:context().

-spec handle_new_connection(context(), map(), map(), voice_state_map(), guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
handle_new_connection(Context, Member, Channel, VoiceStates, State) ->
    ChannelIdValue = maps:get(channel_id, Context),
    GuildId = guild_voice_connection_normalize:normalize_positive_snowflake(
        maps:get(id, State, undefined)
    ),
    ViewerKeyResult = guild_voice_connection_util:resolve_viewer_stream_keys(
        Context, GuildId, ChannelIdValue, VoiceStates, #{}
    ),
    normal_new_connection(Context, Member, Channel, VoiceStates, State, ViewerKeyResult).

-spec normal_new_connection(
    context(), map(), map(), voice_state_map(), guild_state(), {ok, list()} | {error, atom()}
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
normal_new_connection(Context, Member, Channel, VoiceStates, State, ViewerKeyResult) ->
    UserId = maps:get(user_id, Context),
    ChannelIdValue = maps:get(channel_id, Context),
    PermCheck = guild_voice_permissions:check_voice_permissions_and_limits(
        UserId, ChannelIdValue, Channel, VoiceStates, State, false
    ),
    case PermCheck of
        {error, _Category, ErrorAtom} ->
            {reply, gateway_errors:error(ErrorAtom), State};
        {ok, allowed} ->
            check_e2ee_join(Context, Member, VoiceStates, State, ViewerKeyResult)
    end.

-spec check_e2ee_join(
    context(), map(), voice_state_map(), guild_state(), {ok, list()} | {error, atom()}
) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
check_e2ee_join(Context, Member, VoiceStates, State, ViewerKeyResult) ->
    UserId = maps:get(user_id, Context),
    ChannelIdValue = maps:get(channel_id, Context),
    E2EECapable = maps:get(e2ee_capable, Context, false),
    Bot = maps:get(bot, Context, false),
    E2EEEnforced = guild_voice_e2ee:is_e2ee_enabled_for_guild(State),
    E2eeResult = check_e2ee_enforced(
        E2EEEnforced, ChannelIdValue, E2EECapable, Bot, VoiceStates
    ),
    case E2eeResult of
        {error, E2eeErrorAtom} ->
            log_e2ee_rejection(UserId, ChannelIdValue, E2EECapable, Bot),
            {reply, gateway_errors:error(E2eeErrorAtom), State};
        ok ->
            maybe_signal_e2ee_downgrade(
                E2EEEnforced, UserId, ChannelIdValue, E2EECapable, Bot, VoiceStates
            ),
            check_camera_and_viewer(Context, Member, VoiceStates, State, ViewerKeyResult)
    end.

-spec maybe_signal_e2ee_downgrade(
    boolean(), integer(), integer(), boolean(), boolean(), voice_state_map()
) -> ok.
maybe_signal_e2ee_downgrade(false, _UserId, _ChannelIdValue, _E2EECapable, _Bot, _VoiceStates) ->
    ok;
maybe_signal_e2ee_downgrade(true, UserId, ChannelIdValue, E2EECapable, Bot, VoiceStates) ->
    case guild_voice_e2ee:join_downgrades_e2ee(ChannelIdValue, E2EECapable, Bot, VoiceStates) of
        true ->
            logger:warning(
                "voice_e2ee_downgrade: user_id=~p channel_id=~p e2ee_capable=~p bot=~p",
                [UserId, ChannelIdValue, E2EECapable, Bot]
            );
        false ->
            ok
    end.

-spec check_e2ee_enforced(boolean(), integer(), boolean(), boolean(), voice_state_map()) ->
    ok | {error, atom()}.
check_e2ee_enforced(true, ChannelIdValue, E2EECapable, Bot, VoiceStates) ->
    guild_voice_e2ee:check_join_allowed_guild(ChannelIdValue, E2EECapable, Bot, VoiceStates);
check_e2ee_enforced(false, _ChannelIdValue, _E2EECapable, _Bot, _VoiceStates) ->
    ok.

-spec log_e2ee_rejection(integer(), integer(), boolean(), boolean()) -> ok.
log_e2ee_rejection(UserId, ChannelIdValue, E2EECapable, Bot) ->
    logger:debug(
        "voice_e2ee_join_rejected: user_id=~p channel_id=~p e2ee_capable=~p bot=~p",
        [UserId, ChannelIdValue, E2EECapable, Bot]
    ).

-spec check_camera_and_viewer(
    context(), map(), voice_state_map(), guild_state(), {ok, list()} | {error, atom()}
) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
check_camera_and_viewer(Context, Member, VoiceStates, State, ViewerKeyResult) ->
    ChannelIdValue = maps:get(channel_id, Context),
    case
        guild_voice_connection_util:check_camera_user_limit(
            Context, ChannelIdValue, VoiceStates
        )
    of
        {error, CameraErrorAtom} ->
            {reply, gateway_errors:error(CameraErrorAtom), State};
        ok ->
            check_viewer_key_result(Context, Member, State, ViewerKeyResult)
    end.

-spec check_viewer_key_result(
    context(), map(), guild_state(), {ok, list()} | {error, atom()}
) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
check_viewer_key_result(_Context, _Member, State, {error, ErrorAtom}) ->
    {reply, gateway_errors:error(ErrorAtom), State};
check_viewer_key_result(Context, Member, State, {ok, ParsedViewerKey}) ->
    get_voice_token_and_create_state(Context, Member, ParsedViewerKey, State).

-spec get_voice_token_and_create_state(context(), map(), list(), guild_state()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
get_voice_token_and_create_state(Context, Member, ParsedViewerStreamKey, State) ->
    ChannelIdValue = maps:get(channel_id, Context),
    UserId = maps:get(user_id, Context),
    State0 = clear_join_flags(UserId, ChannelIdValue, State),
    case guild_voice_connection_util:resolve_guild_identity(State0) of
        {error, ErrorAtom} ->
            {reply, gateway_errors:error(ErrorAtom), State0};
        {ok, GuildId, GuildIdBin} ->
            request_token_and_build(
                Context, Member, ParsedViewerStreamKey, State0, GuildId, GuildIdBin
            )
    end.

-spec clear_join_flags(integer(), integer(), guild_state()) -> guild_state().
clear_join_flags(UserId, ChannelIdValue, State) ->
    guild_virtual_channel_access:clear_transition_flags(UserId, ChannelIdValue, State).

-spec request_token_and_build(context(), map(), list(), guild_state(), integer(), binary()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
request_token_and_build(Context, Member, ParsedViewerStreamKey, State, GuildId, GuildIdBin) ->
    UserId = maps:get(user_id, Context),
    ChannelIdValue = maps:get(channel_id, Context),
    VoicePermissions = voice_utils:compute_voice_permissions(UserId, ChannelIdValue, State),
    TokenNonce = voice_utils:generate_token_nonce(),
    Latitude = maps:get(latitude, Context, undefined),
    Longitude = maps:get(longitude, Context, undefined),
    case
        guild_voice_connection_token:request_voice_token(
            GuildId,
            ChannelIdValue,
            UserId,
            null,
            VoicePermissions,
            TokenNonce,
            Latitude,
            Longitude
        )
    of
        {ok, TokenData} ->
            build_new_voice_state(#{
                context => Context,
                member => Member,
                viewer_stream_keys => ParsedViewerStreamKey,
                state => State,
                guild_id => GuildId,
                guild_id_bin => GuildIdBin,
                token_data => TokenData,
                voice_permissions => VoicePermissions,
                token_nonce => TokenNonce,
                latitude => Latitude,
                longitude => Longitude
            });
        {error, _Reason} ->
            {reply, gateway_errors:error(voice_token_failed), State}
    end.

-spec build_new_voice_state(
    map()
) -> {reply, map(), guild_state()}.
build_new_voice_state(Build) ->
    Context = maps:get(context, Build),
    State = maps:get(state, Build),
    TokenData = maps:get(token_data, Build),
    Token = maps:get(token, TokenData),
    Endpoint = maps:get(endpoint, TokenData),
    ConnectionId = maps:get(connection_id, TokenData),
    ChannelIdValue = maps:get(channel_id, Context),
    VoiceBuild = voice_build_fields(Build),
    VoiceState = create_and_decorate_voice_state(VoiceBuild#{connection_id => ConnectionId}),
    PendingMetadata = build_pending_metadata(VoiceBuild#{
        connection_id => ConnectionId,
        voice_state => VoiceState
    }),
    State2 = guild_voice_connection_pending:store_pending(ConnectionId, PendingMetadata, State),
    {State3, E2EEKeyForReply} = guild_voice_e2ee:maybe_room_key_for_reply_guild(
        Context, ChannelIdValue, State2
    ),
    BaseReply = #{
        success => true,
        token => Token,
        endpoint => Endpoint,
        connection_id => ConnectionId,
        voice_state => voice_state_utils:external_voice_state(VoiceState)
    },
    {reply,
        guild_voice_connection_util:maybe_attach_e2ee_key_to_reply(BaseReply, E2EEKeyForReply),
        State3}.

-spec voice_build_fields(map()) -> map().
voice_build_fields(Build) ->
    Context = maps:get(context, Build),
    Member = maps:get(member, Build),
    VoicePermissions = maps:get(voice_permissions, Build),
    UserId = maps:get(user_id, Context),
    ChannelIdValue = maps:get(channel_id, Context),
    #{
        user_id => UserId,
        guild_id => maps:get(guild_id, Build),
        guild_id_bin => maps:get(guild_id_bin, Build),
        channel_id => ChannelIdValue,
        channel_id_bin => integer_to_binary(ChannelIdValue),
        user_id_bin => integer_to_binary(UserId),
        flags => guild_voice_connection_util:voice_flags_for_permissions(
            Context, VoicePermissions
        ),
        server_mute => maps:get(<<"mute">>, Member, false),
        server_deaf => maps:get(<<"deaf">>, Member, false),
        member => Member,
        latitude => maps:get(latitude, Build),
        longitude => maps:get(longitude, Build),
        viewer_stream_keys => maps:get(viewer_stream_keys, Build),
        e2ee_capable => e2ee_capable(Context, maps:get(state, Build)),
        token_data => maps:get(token_data, Build),
        token_nonce => maps:get(token_nonce, Build),
        session_id => normalize_context_session_id(Context)
    }.

-spec e2ee_capable(context(), guild_state()) -> boolean().
e2ee_capable(Context, State) ->
    guild_voice_e2ee:context_e2ee_capable_guild(Context, State).

-spec normalize_context_session_id(context()) -> binary() | undefined.
normalize_context_session_id(Context) ->
    guild_voice_connection_util:normalize_session_id(maps:get(session_id, Context, undefined)).

-spec create_and_decorate_voice_state(map()) -> voice_state().
create_and_decorate_voice_state(Build) ->
    TokenData = maps:get(token_data, Build),
    VS0 = guild_voice_state:create_voice_state(
        #{
            guild_id => maps:get(guild_id_bin, Build),
            channel_id => maps:get(channel_id_bin, Build),
            user_id => maps:get(user_id_bin, Build),
            connection_id => maps:get(connection_id, Build),
            server_mute => maps:get(server_mute, Build),
            server_deaf => maps:get(server_deaf, Build),
            viewer_stream_keys => maps:get(viewer_stream_keys, Build),
            e2ee_capable => maps:get(e2ee_capable, Build)
        },
        maps:get(flags, Build)
    ),
    VS1 = guild_voice_connection_util:maybe_attach_voice_routing_metadata(
        VS0,
        maps:get(region_id, TokenData, undefined),
        maps:get(server_id, TokenData, undefined)
    ),
    VS2 = guild_voice_connection_util:maybe_attach_geolocation(
        VS1, maps:get(latitude, Build), maps:get(longitude, Build)
    ),
    VS3 = guild_voice_connection_util:maybe_attach_session_id(VS2, maps:get(session_id, Build)),
    guild_voice_connection_util:maybe_attach_member(VS3, maps:get(member, Build)).

-spec build_pending_metadata(map()) -> map().
build_pending_metadata(Build) ->
    Now = erlang:system_time(millisecond),
    Flags = maps:get(flags, Build),
    #{
        user_id => maps:get(user_id, Build),
        guild_id => maps:get(guild_id, Build),
        channel_id => maps:get(channel_id, Build),
        session_id => maps:get(session_id, Build),
        self_mute => maps:get(self_mute, Flags),
        self_deaf => maps:get(self_deaf, Flags),
        self_video => maps:get(self_video, Flags),
        self_stream => maps:get(self_stream, Flags),
        is_mobile => maps:get(is_mobile, Flags),
        suppress => maps:get(suppress, Flags),
        server_mute => maps:get(server_mute, Build),
        server_deaf => maps:get(server_deaf, Build),
        member => maps:get(member, Build),
        latitude => maps:get(latitude, Build),
        longitude => maps:get(longitude, Build),
        viewer_stream_keys => maps:get(viewer_stream_keys, Build),
        e2ee_capable => maps:get(e2ee_capable, Build),
        voice_state => maps:get(voice_state, Build),
        token_nonce => maps:get(token_nonce, Build),
        created_at => Now,
        expires_at => Now + 30000
    }.
