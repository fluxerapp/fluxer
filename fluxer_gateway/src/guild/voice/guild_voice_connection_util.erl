%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_connection_util).

-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/voice_state.hrl").

-export([
    build_context/1,
    voice_flags_for_permissions/2,
    resolve_guild_identity/1,
    resolve_viewer_stream_keys/5,
    check_camera_user_limit/3,
    maybe_attach_session_id/2,
    maybe_attach_member/2,
    maybe_attach_geolocation/3,
    maybe_attach_voice_routing_metadata/3,
    maybe_attach_e2ee_key_to_reply/2,
    normalize_session_id/1,
    normalize_optional_binary/1,
    applied_mutation_reply/3,
    maybe_error_reply/5,
    rejected_mutation_reply/6,
    clear_virtual_access_flags/2
]).

-type guild_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.
-type context() :: map().

-export_type([
    guild_state/0,
    voice_state/0,
    voice_state_map/0,
    context/0,
    voice_flags/0
]).

-spec build_context(map()) -> context().
build_context(Request0) ->
    Request = map_utils:ensure_map(Request0),
    Norm = fun guild_voice_connection_normalize:normalize_boolean/1,
    Coord = fun guild_voice_connection_normalize:normalize_coordinate/1,
    #{
        user_id => guild_voice_connection_normalize:normalize_user_id(
            maps:get(user_id, Request, undefined)
        ),
        channel_id => guild_voice_connection_normalize:normalize_channel_id_value(
            maps:get(channel_id, Request, null)
        ),
        session_id => maps:get(session_id, Request, undefined),
        connection_id => guild_voice_connection_normalize:normalize_connection_id(
            maps:get(connection_id, Request, undefined)
        ),
        self_mute => Norm(maps:get(self_mute, Request, false)),
        self_deaf => Norm(maps:get(self_deaf, Request, false)),
        self_video => Norm(maps:get(self_video, Request, false)),
        self_stream => Norm(maps:get(self_stream, Request, false)),
        is_mobile => Norm(maps:get(is_mobile, Request, false)),
        viewer_stream_keys => maps:get(viewer_stream_keys, Request, undefined),
        latitude => Coord(maps:get(latitude, Request, undefined)),
        longitude => Coord(maps:get(longitude, Request, undefined)),
        mutation_id => maps:get(mutation_id, Request, undefined),
        runtime_epoch => maps:get(runtime_epoch, Request, undefined),
        base_version => maps:get(base_version, Request, undefined),
        e2ee_capable => Norm(maps:get(e2ee_capable, Request, false)),
        bot => Norm(maps:get(bot, Request, false))
    }.

-spec voice_flags_for_permissions(context(), voice_utils:voice_permissions()) -> voice_flags().
voice_flags_for_permissions(Context, VoicePermissions) ->
    voice_utils:apply_voice_permissions_to_flags(
        voice_state_utils:voice_flags_from_context(Context), VoicePermissions
    ).

-spec resolve_guild_identity(guild_state()) ->
    {ok, integer(), binary()} | {error, atom()}.
resolve_guild_identity(State) ->
    Data = guild_data(State),
    DataGuildIdBin = maps:get(<<"id">>, Data, undefined),
    StateGuildId = guild_voice_connection_normalize:normalize_positive_snowflake(
        maps:get(id, State, undefined)
    ),
    GuildMeta = map_utils:ensure_map(maps:get(<<"guild">>, Data, #{})),
    GuildMetaIdBin = maps:get(<<"id">>, GuildMeta, undefined),
    resolve_guild_id_priority([DataGuildIdBin, StateGuildId, GuildMetaIdBin]).

-spec resolve_viewer_stream_keys(
    context(), integer() | undefined, integer(), voice_state_map(), voice_state()
) -> {ok, list()} | {error, atom()}.
resolve_viewer_stream_keys(Context, GuildId, ChannelIdValue, VoiceStates, ExistingVS) ->
    RawKeys = maps:get(viewer_stream_keys, Context, undefined),
    case RawKeys of
        undefined ->
            {ok, existing_viewer_stream_keys(ExistingVS)};
        null ->
            {ok, []};
        Keys when is_list(Keys) ->
            validate_viewer_keys(Keys, GuildId, ChannelIdValue, VoiceStates, []);
        _ ->
            {error, voice_invalid_state}
    end.

-spec check_camera_user_limit(context(), integer(), voice_state_map()) ->
    ok | {error, atom()}.
check_camera_user_limit(#{self_video := true} = Context, ChannelIdValue, VoiceStates) ->
    CameraUserIds = count_camera_users(ChannelIdValue, VoiceStates),
    camera_user_limit_result(add_requesting_user(Context, CameraUserIds));
check_camera_user_limit(_Context, _ChannelIdValue, _VoiceStates) ->
    ok.

-spec add_requesting_user(context(), sets:set(integer())) -> sets:set(integer()).
add_requesting_user(Context, UserIds) ->
    case maps:get(user_id, Context, undefined) of
        UserId when is_integer(UserId) -> sets:add_element(UserId, UserIds);
        _ -> UserIds
    end.

-spec camera_user_limit_result(sets:set(integer())) -> ok | {error, atom()}.
camera_user_limit_result(UserIds) ->
    case sets:size(UserIds) > constants:voice_channel_camera_user_limit() of
        true -> {error, voice_camera_user_limit};
        false -> ok
    end.

-spec maybe_attach_session_id(voice_state(), binary() | undefined) -> voice_state().
maybe_attach_session_id(VoiceState, undefined) ->
    VoiceState;
maybe_attach_session_id(VoiceState, SessionId) when is_binary(SessionId) ->
    VoiceState#{<<"session_id">> => SessionId}.

-spec maybe_attach_member(voice_state(), map()) -> voice_state().
maybe_attach_member(VoiceState, Member) when is_map(Member) ->
    case maps:size(Member) of
        0 -> VoiceState;
        _ -> VoiceState#{<<"member">> => Member}
    end.

-spec maybe_attach_geolocation(voice_state(), binary() | undefined, binary() | undefined) ->
    voice_state().
maybe_attach_geolocation(VoiceState, Lat, Long) when is_binary(Lat), is_binary(Long) ->
    VoiceState#{<<"latitude">> => Lat, <<"longitude">> => Long};
maybe_attach_geolocation(VoiceState, _Lat, _Long) ->
    VoiceState.

-spec maybe_attach_voice_routing_metadata(voice_state(), term(), term()) -> voice_state().
maybe_attach_voice_routing_metadata(VoiceState, RegionIdRaw, ServerIdRaw) ->
    RegionId = guild_voice_connection_normalize:normalize_optional_binary(RegionIdRaw),
    ServerId = guild_voice_connection_normalize:normalize_optional_binary(ServerIdRaw),
    VS1 = maybe_put_field(VoiceState, <<"region_id">>, RegionId),
    maybe_put_field(VS1, <<"server_id">>, ServerId).

-spec maybe_attach_e2ee_key_to_reply(map(), binary() | undefined) -> map().
maybe_attach_e2ee_key_to_reply(Reply, undefined) ->
    Reply;
maybe_attach_e2ee_key_to_reply(Reply, Key) when is_binary(Key) ->
    Reply#{e2ee_key => Key}.

-spec normalize_session_id(term()) -> binary() | undefined.
normalize_session_id(Value) ->
    guild_voice_connection_normalize:normalize_session_id(Value).

-spec normalize_optional_binary(term()) -> binary() | undefined.
normalize_optional_binary(Value) ->
    guild_voice_connection_normalize:normalize_optional_binary(Value).

-spec applied_mutation_reply({reply, map(), guild_state()}, context(), integer()) ->
    {reply, map(), guild_state()}.
applied_mutation_reply({reply, BaseReply, NewState}, Context, ChannelIdValue) ->
    GuildId = guild_voice_connection_normalize:normalize_positive_snowflake(
        maps:get(id, NewState, undefined)
    ),
    NewVoiceState = maps:get(voice_state, BaseReply, #{}),
    NewVersion = voice_state_utils:voice_state_version(NewVoiceState),
    NormalizedConnId = normalize_conn_id_for_ack(Context),
    Ack = guild_voice_mutation:build_ack(
        maps:get(mutation_id, Context, undefined),
        maps:get(runtime_epoch, Context, undefined),
        NormalizedConnId,
        GuildId,
        ChannelIdValue,
        #{
            status => <<"applied">>,
            server_version => NewVersion,
            canonical_state => voice_state_utils:external_voice_state(NewVoiceState)
        }
    ),
    Reply = merge_ack(BaseReply, Ack),
    {reply, Reply, NewState}.

-spec maybe_error_reply(context(), voice_state(), guild_state(), integer(), atom()) ->
    {reply, map(), guild_state()} | {reply, {error, atom(), atom()}, guild_state()}.
maybe_error_reply(Context, ExistingVoiceState, State, ChannelIdValue, ErrorAtom) ->
    case maps:get(mutation_id, Context, undefined) of
        undefined ->
            {reply, gateway_errors:error(ErrorAtom), State};
        _ ->
            rejected_mutation_reply(
                Context, ExistingVoiceState, State, ChannelIdValue, <<"rejected">>, ErrorAtom
            )
    end.

-spec rejected_mutation_reply(
    context(), voice_state(), guild_state(), integer(), binary(), atom() | binary()
) -> {reply, map(), guild_state()}.
rejected_mutation_reply(Context, ExistingVS, State, ChannelIdValue, Status, Error) ->
    GuildId = guild_voice_connection_normalize:normalize_positive_snowflake(
        maps:get(id, State, undefined)
    ),
    CurrentVersion = voice_state_utils:voice_state_version(ExistingVS),
    NormalizedConnId = normalize_conn_id_for_ack(Context),
    Ack = guild_voice_mutation:build_ack(
        maps:get(mutation_id, Context, undefined),
        maps:get(runtime_epoch, Context, undefined),
        NormalizedConnId,
        GuildId,
        ChannelIdValue,
        #{
            status => Status,
            server_version => CurrentVersion,
            canonical_state => canonical_state_for_ack(ExistingVS),
            error_code => rejection_error_code(Error),
            error_message => rejection_error_message(Error)
        }
    ),
    {reply, #{success => false, ack => Ack}, State}.

-spec canonical_state_for_ack(voice_state()) -> map().
canonical_state_for_ack(VoiceState) when is_map(VoiceState), map_size(VoiceState) > 0 ->
    voice_state_utils:external_voice_state(VoiceState);
canonical_state_for_ack(_) ->
    #{}.

-spec clear_virtual_access_flags(voice_state(), guild_state()) -> guild_state().
clear_virtual_access_flags(VoiceState, State) when is_map(VoiceState) ->
    UserId = voice_state_utils:voice_state_user_id(VoiceState),
    ChannelId = voice_state_utils:voice_state_channel_id(VoiceState),
    case is_integer(UserId) andalso is_integer(ChannelId) of
        true ->
            guild_virtual_channel_access:clear_transition_flags(UserId, ChannelId, State);
        false ->
            State
    end.

-spec normalize_conn_id_for_ack(context()) -> binary() | null.
normalize_conn_id_for_ack(Context) ->
    case maps:get(connection_id, Context, undefined) of
        undefined -> null;
        C -> C
    end.

-spec merge_ack(map(), term()) -> map().
merge_ack(BaseReply, undefined) -> BaseReply;
merge_ack(BaseReply, Ack) -> BaseReply#{ack => Ack}.

-spec rejection_error_code(atom() | binary()) -> binary().
rejection_error_code(ErrorAtom) when is_atom(ErrorAtom) -> gateway_errors:error_code(ErrorAtom);
rejection_error_code(ErrorCode) when is_binary(ErrorCode) -> ErrorCode.

-spec rejection_error_message(atom() | binary()) -> binary().
rejection_error_message(ErrorAtom) when is_atom(ErrorAtom) ->
    gateway_errors:error_message(ErrorAtom);
rejection_error_message(ErrorMessage) when is_binary(ErrorMessage) -> ErrorMessage.

-spec guild_data(guild_state()) -> map().
guild_data(State) ->
    map_utils:ensure_map(maps:get(data, State, #{})).

-spec resolve_guild_id_priority([term()]) -> {ok, integer(), binary()} | {error, atom()}.
resolve_guild_id_priority([]) ->
    {error, voice_guild_id_missing};
resolve_guild_id_priority([undefined | Rest]) ->
    resolve_guild_id_priority(Rest);
resolve_guild_id_priority([Value | _]) ->
    guild_voice_connection_normalize:normalize_guild_id(Value).

-spec existing_viewer_stream_keys(voice_state()) -> list().
existing_viewer_stream_keys(ExistingVS) ->
    case maps:get(<<"viewer_stream_keys">>, ExistingVS, []) of
        Keys when is_list(Keys) -> Keys;
        _ -> []
    end.

-spec maybe_put_field(voice_state(), binary(), binary() | undefined) -> voice_state().
maybe_put_field(VoiceState, _Key, undefined) -> VoiceState;
maybe_put_field(VoiceState, Key, Value) -> VoiceState#{Key => Value}.

-spec count_camera_users(integer(), voice_state_map()) -> sets:set(integer()).
count_camera_users(ChannelIdValue, VoiceStates) ->
    maps:fold(
        fun(_ConnId, VS, Acc) ->
            add_camera_user_if_channel_matches(VS, ChannelIdValue, Acc)
        end,
        sets:new(),
        VoiceStates
    ).

-spec add_camera_user_if_channel_matches(voice_state(), integer(), sets:set(integer())) ->
    sets:set(integer()).
add_camera_user_if_channel_matches(VS, ChannelIdValue, Acc) ->
    case voice_state_utils:voice_state_channel_id(VS) of
        ChannelIdValue -> add_user_if_camera_on(VS, Acc);
        _ -> Acc
    end.

-spec add_user_if_camera_on(voice_state(), sets:set(integer())) -> sets:set(integer()).
add_user_if_camera_on(VS, Acc) ->
    case maps:get(<<"self_video">>, VS, false) of
        true -> add_user_to_set(VS, Acc);
        _ -> Acc
    end.

-spec add_user_to_set(voice_state(), sets:set(integer())) -> sets:set(integer()).
add_user_to_set(VS, Acc) ->
    case voice_state_utils:voice_state_user_id(VS) of
        undefined -> Acc;
        UserId -> sets:add_element(UserId, Acc)
    end.

-spec validate_viewer_keys(list(), integer() | undefined, integer(), voice_state_map(), list()) ->
    {ok, list()} | {error, atom()}.
validate_viewer_keys([], _GuildId, _ChId, _VS, Acc) ->
    {ok, lists:reverse(Acc)};
validate_viewer_keys([Key | Rest], GuildId, ChId, VS, Acc) ->
    case validate_single_key(Key, GuildId, ChId, VS) of
        {ok, ValidKey} -> validate_viewer_keys(Rest, GuildId, ChId, VS, [ValidKey | Acc]);
        {error, _} = Error -> Error
    end.

-spec validate_single_key(term(), integer() | undefined, integer(), voice_state_map()) ->
    {ok, binary()} | {error, atom()}.
validate_single_key(RawKey, _GuildId, _ChId, _VS) when not is_binary(RawKey) ->
    {error, voice_invalid_state};
validate_single_key(RawKey, GuildId, ChId, VS) ->
    case voice_state_utils:parse_stream_key(RawKey) of
        {error, _} -> {error, voice_invalid_state};
        {ok, Parsed} -> validate_parsed_key(RawKey, Parsed, GuildId, ChId, VS)
    end.

-spec validate_parsed_key(binary(), map(), integer() | undefined, integer(), voice_state_map()) ->
    {ok, binary()} | {error, atom()}.
validate_parsed_key(
    RawKey,
    #{scope := guild, guild_id := PGId, channel_id := PChId, connection_id := SConnId},
    GuildId,
    ChId,
    VS
) when
    is_integer(ChId), PChId =:= ChId
->
    validate_guild_scope_key(RawKey, SConnId, ChId, VS, check_guild_scope(GuildId, PGId));
validate_parsed_key(_RawKey, _Parsed, _GuildId, _ChId, _VS) ->
    {error, voice_invalid_state}.

-spec check_guild_scope(integer() | undefined, integer()) -> ok | error.
check_guild_scope(undefined, _) -> ok;
check_guild_scope(GuildId, GuildId) -> ok;
check_guild_scope(_, _) -> error.

-spec validate_guild_scope_key(binary(), binary(), integer(), voice_state_map(), ok | error) ->
    {ok, binary()} | {error, atom()}.
validate_guild_scope_key(RawKey, SConnId, ChId, VS, ok) ->
    validate_stream_conn(RawKey, SConnId, ChId, VS);
validate_guild_scope_key(_RawKey, _SConnId, _ChId, _VS, error) ->
    {error, voice_invalid_state}.

-spec validate_stream_conn(binary(), binary(), integer(), voice_state_map()) ->
    {ok, binary()} | {error, atom()}.
validate_stream_conn(RawKey, StreamConnId, ChId, VS) ->
    case maps:get(StreamConnId, VS, undefined) of
        undefined ->
            {error, voice_connection_not_found};
        StreamVS ->
            validate_stream_conn_channel(RawKey, StreamVS, ChId)
    end.

-spec validate_stream_conn_channel(binary(), voice_state(), integer()) ->
    {ok, binary()} | {error, atom()}.
validate_stream_conn_channel(RawKey, StreamVS, ChId) ->
    case voice_state_utils:voice_state_channel_id(StreamVS) of
        ChId -> {ok, RawKey};
        _ -> {error, voice_invalid_state}
    end.
