%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_connection_update).

-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/voice_state.hrl").

-export([handle_update_connection/6]).

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

-spec handle_update_connection(
    context(), integer(), map(), map(), voice_state_map(), guild_state()
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
handle_update_connection(Context, ChannelIdValue, Member, Channel, VoiceStates, State) ->
    ConnectionId = maps:get(connection_id, Context),
    UserId = maps:get(user_id, Context),
    case maps:get(ConnectionId, VoiceStates, undefined) of
        undefined ->
            handle_missing_connection(
                Context,
                ConnectionId,
                ChannelIdValue,
                UserId,
                Member,
                Channel,
                VoiceStates,
                State
            );
        ExistingVoiceState ->
            handle_existing_connection(
                Context,
                ChannelIdValue,
                UserId,
                Member,
                Channel,
                VoiceStates,
                State,
                ExistingVoiceState
            )
    end.

-spec handle_missing_connection(
    context(), binary(), integer(), integer(), map(), map(), voice_state_map(), guild_state()
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
handle_missing_connection(
    Context, ConnectionId, ChannelIdValue, UserId, Member, Channel, VoiceStates, State
) ->
    case
        guild_voice_connection_pending:maybe_restore_pending_connection(
            ConnectionId, ChannelIdValue, UserId, VoiceStates, State
        )
    of
        {ok, UpdatedVoiceStates, UpdatedState} ->
            log_restored(ConnectionId, UserId, ChannelIdValue),
            proceed_after_restore(
                Context, ChannelIdValue, Member, Channel, UpdatedVoiceStates, UpdatedState
            );
        {error, ErrorAtom} ->
            log_restore_failed(ConnectionId, UserId, ChannelIdValue, ErrorAtom),
            guild_voice_connection_util:maybe_error_reply(
                Context, #{}, State, ChannelIdValue, ErrorAtom
            )
    end.

-spec log_restored(binary(), integer(), integer()) -> ok.
log_restored(ConnectionId, UserId, ChannelIdValue) ->
    logger:debug(
        "Restored pending voice connection during update",
        #{connection_id => ConnectionId, user_id => UserId, channel_id => ChannelIdValue}
    ).

-spec log_restore_failed(binary(), integer(), integer(), atom()) -> ok.
log_restore_failed(ConnectionId, UserId, ChannelIdValue, ErrorAtom) ->
    logger:debug(
        "Failed to restore pending voice connection during update",
        #{
            connection_id => ConnectionId,
            user_id => UserId,
            channel_id => ChannelIdValue,
            error => ErrorAtom
        }
    ).

-spec proceed_after_restore(
    context(), integer(), map(), map(), voice_state_map(), guild_state()
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
proceed_after_restore(
    Context, ChannelIdValue, Member, Channel, UpdatedVoiceStates, UpdatedState
) ->
    ConnectionId = maps:get(connection_id, Context),
    ExistingVS = maps:get(ConnectionId, UpdatedVoiceStates),
    ExistingChannelIdBin = maps:get(<<"channel_id">>, ExistingVS, null),
    NewChannelIdBin = integer_to_binary(ChannelIdValue),
    IsChannelChange = ExistingChannelIdBin =/= NewChannelIdBin,
    GuildId = guild_voice_connection_normalize:normalize_positive_snowflake(
        maps:get(id, UpdatedState, undefined)
    ),
    ViewerKeyResult = guild_voice_connection_util:resolve_viewer_stream_keys(
        Context, GuildId, ChannelIdValue, UpdatedVoiceStates, ExistingVS
    ),
    normal_update(
        Context,
        ChannelIdValue,
        Member,
        Channel,
        UpdatedVoiceStates,
        UpdatedState,
        IsChannelChange,
        ViewerKeyResult
    ).

-spec handle_existing_connection(
    context(),
    integer(),
    integer(),
    map(),
    map(),
    voice_state_map(),
    guild_state(),
    voice_state()
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
handle_existing_connection(
    Context, ChannelIdValue, UserId, Member, Channel, VoiceStates, State, ExistingVoiceState
) ->
    case guild_voice_state:user_matches_voice_state(ExistingVoiceState, UserId) of
        false ->
            {reply, gateway_errors:error(voice_user_mismatch), State};
        true ->
            ExistingChannelIdBin = maps:get(<<"channel_id">>, ExistingVoiceState, null),
            NewChannelIdBin = integer_to_binary(ChannelIdValue),
            IsChannelChange = ExistingChannelIdBin =/= NewChannelIdBin,
            GuildId = guild_voice_connection_normalize:normalize_positive_snowflake(
                maps:get(id, State, undefined)
            ),
            ViewerKeyResult = guild_voice_connection_util:resolve_viewer_stream_keys(
                Context, GuildId, ChannelIdValue, VoiceStates, ExistingVoiceState
            ),
            normal_update(
                Context,
                ChannelIdValue,
                Member,
                Channel,
                VoiceStates,
                State,
                IsChannelChange,
                ViewerKeyResult
            )
    end.

-spec normal_update(
    context(),
    integer(),
    map(),
    map(),
    voice_state_map(),
    guild_state(),
    boolean(),
    {ok, list()} | {error, atom()}
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
normal_update(
    Context,
    ChannelIdValue,
    Member,
    Channel,
    VoiceStates,
    State,
    IsChannelChange,
    ViewerKeyResult
) ->
    ConnectionId = maps:get(connection_id, Context),
    ExistingVS = maps:get(ConnectionId, VoiceStates, #{}),
    check_permissions(
        Context,
        ChannelIdValue,
        Member,
        Channel,
        VoiceStates,
        State,
        IsChannelChange,
        ViewerKeyResult,
        ExistingVS
    ).

-spec check_permissions(
    context(),
    integer(),
    map(),
    map(),
    voice_state_map(),
    guild_state(),
    boolean(),
    {ok, list()} | {error, atom()},
    voice_state()
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
check_permissions(
    Context,
    ChannelIdValue,
    Member,
    Channel,
    VoiceStates,
    State,
    IsChannelChange,
    ViewerKeyResult,
    ExistingVS
) ->
    UserId = maps:get(user_id, Context),
    PermCheck = perm_check_if_channel_change(
        IsChannelChange, UserId, ChannelIdValue, Channel, VoiceStates, State
    ),
    Update = #{
        context => Context,
        channel_id => ChannelIdValue,
        member => Member,
        voice_states => VoiceStates,
        state => State,
        is_channel_change => IsChannelChange,
        viewer_key_result => ViewerKeyResult,
        existing_voice_state => ExistingVS
    },
    apply_perm_check(PermCheck, Update).

-spec apply_perm_check(
    {ok, allowed} | {error, atom(), atom()},
    map()
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
apply_perm_check({error, _Category, ErrorAtom}, Update) ->
    Context = maps:get(context, Update),
    ChannelIdValue = maps:get(channel_id, Update),
    State = maps:get(state, Update),
    ExistingVS = maps:get(existing_voice_state, Update),
    guild_voice_connection_util:maybe_error_reply(
        Context, ExistingVS, State, ChannelIdValue, ErrorAtom
    );
apply_perm_check({ok, allowed}, Update) ->
    E2eeJoinCheck = e2ee_check_if_channel_change(
        maps:get(is_channel_change, Update),
        maps:get(channel_id, Update),
        maps:get(context, Update),
        maps:get(voice_states, Update),
        maps:get(state, Update)
    ),
    apply_e2ee_check(E2eeJoinCheck, Update).

-spec apply_e2ee_check(
    ok | {error, atom()},
    map()
) -> {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
apply_e2ee_check({error, E2eeErrorAtom}, Update) ->
    Context = maps:get(context, Update),
    ChannelIdValue = maps:get(channel_id, Update),
    State = maps:get(state, Update),
    ExistingVS = maps:get(existing_voice_state, Update),
    guild_voice_connection_util:maybe_error_reply(
        Context, ExistingVS, State, ChannelIdValue, E2eeErrorAtom
    );
apply_e2ee_check(ok, Update) ->
    continue_update(
        maps:get(context, Update),
        maps:get(channel_id, Update),
        maps:get(member, Update),
        maps:get(voice_states, Update),
        maps:get(state, Update),
        maps:get(is_channel_change, Update),
        maps:get(viewer_key_result, Update),
        maps:get(existing_voice_state, Update)
    ).

-spec perm_check_if_channel_change(
    boolean(), integer(), integer(), map(), voice_state_map(), guild_state()
) ->
    {ok, allowed} | {error, atom(), atom()}.
perm_check_if_channel_change(true, UserId, ChannelIdValue, Channel, VoiceStates, State) ->
    guild_voice_permissions:check_voice_permissions_and_limits(
        UserId, ChannelIdValue, Channel, VoiceStates, State, false
    );
perm_check_if_channel_change(false, _UserId, _ChannelIdValue, _Channel, _VoiceStates, _State) ->
    {ok, allowed}.

-spec e2ee_check_if_channel_change(
    boolean(), integer(), context(), voice_state_map(), guild_state()
) ->
    ok | {error, atom()}.
e2ee_check_if_channel_change(true, ChannelIdValue, Context, VoiceStates, State) ->
    case guild_voice_e2ee:is_e2ee_enabled_for_guild(State) of
        true ->
            guild_voice_e2ee:check_join_allowed_guild(
                ChannelIdValue,
                maps:get(e2ee_capable, Context, false),
                maps:get(bot, Context, false),
                VoiceStates
            );
        false ->
            ok
    end;
e2ee_check_if_channel_change(false, _ChannelIdValue, _Context, _VoiceStates, _State) ->
    ok.

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
    guild_voice_connection_apply:continue_update(
        Context,
        ChannelIdValue,
        Member,
        VoiceStates,
        State,
        IsChannelChange,
        ViewerKeyResult,
        ExistingVS
    ).
