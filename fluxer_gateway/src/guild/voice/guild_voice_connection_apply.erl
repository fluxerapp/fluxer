%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_connection_apply).
-typing([eqwalizer]).

-export([continue_update/8]).

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

-spec continue_update(
    context(),
    integer(),
    map(),
    voice_state_map(),
    guild_state(),
    boolean(),
    {ok, list()} | {error, atom()},
    voice_state()
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
continue_update(
    Context,
    ChannelIdValue,
    Member,
    VoiceStates,
    State,
    IsChannelChange,
    ViewerKeyResult,
    ExistingVS
) ->
    CameraCheck = guild_voice_connection_util:check_camera_user_limit(
        Context, ChannelIdValue, VoiceStates
    ),
    case CameraCheck of
        {error, CameraErrorAtom} ->
            guild_voice_connection_util:maybe_error_reply(
                Context, ExistingVS, State, ChannelIdValue, CameraErrorAtom
            );
        ok ->
            continue_after_camera_check(
                Context,
                ChannelIdValue,
                Member,
                VoiceStates,
                State,
                IsChannelChange,
                ViewerKeyResult,
                ExistingVS
            )
    end.

-spec continue_after_camera_check(
    context(),
    integer(),
    map(),
    voice_state_map(),
    guild_state(),
    boolean(),
    {ok, list()} | {error, atom()},
    voice_state()
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
continue_after_camera_check(
    Context,
    ChannelIdValue,
    Member,
    VoiceStates,
    State,
    IsChannelChange,
    ViewerKeyResult,
    ExistingVS
) ->
    UserId = maps:get(user_id, Context),
    VoicePerms = voice_utils:compute_voice_permissions(UserId, ChannelIdValue, State),
    Update = #{
        context => Context,
        channel_id => ChannelIdValue,
        member => Member,
        connection_id => maps:get(connection_id, Context),
        voice_states => VoiceStates,
        state => State,
        is_channel_change => IsChannelChange,
        voice_permissions => VoicePerms,
        existing_voice_state => ExistingVS
    },
    apply_with_viewer_key(Update, ViewerKeyResult).

-spec apply_with_viewer_key(
    map(),
    {ok, list()} | {error, atom()}
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
apply_with_viewer_key(Update, {error, ErrorAtom}) ->
    Context = maps:get(context, Update),
    ChannelIdValue = maps:get(channel_id, Update),
    State = maps:get(state, Update),
    ExistingVS = maps:get(existing_voice_state, Update),
    guild_voice_connection_util:maybe_error_reply(
        Context, ExistingVS, State, ChannelIdValue, ErrorAtom
    );
apply_with_viewer_key(#{is_channel_change := true} = Update, {ok, _ParsedViewerKey}) ->
    apply_channel_move(Update);
apply_with_viewer_key(#{is_channel_change := false} = Update, {ok, ParsedViewerKey}) ->
    apply_same_channel_update(Update, ParsedViewerKey).

-spec apply_channel_move(map()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
apply_channel_move(Update) ->
    guild_voice_connection_move:handle_client_channel_move(
        maps:get(context, Update),
        maps:get(channel_id, Update),
        maps:get(member, Update),
        maps:get(connection_id, Update),
        maps:get(voice_states, Update),
        maps:get(state, Update)
    ).

-spec apply_same_channel_update(map(), list()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
apply_same_channel_update(Update, ParsedViewerKey) ->
    Context = maps:get(context, Update),
    ChannelIdValue = maps:get(channel_id, Update),
    VoicePerms = maps:get(voice_permissions, Update),
    Flags = guild_voice_connection_util:voice_flags_for_permissions(Context, VoicePerms),
    UpdateResult = guild_voice_state:update_voice_state_data(#{
        connection_id => maps:get(connection_id, Update),
        channel_id => integer_to_binary(ChannelIdValue),
        flags => Flags,
        member => maps:get(member, Update),
        existing_voice_state => maps:get(existing_voice_state, Update),
        voice_states => maps:get(voice_states, Update),
        state => maps:get(state, Update),
        needs_token => false,
        viewer_stream_keys => ParsedViewerKey
    }),
    UpdateResult.
