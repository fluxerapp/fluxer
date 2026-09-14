%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(voice_utils).
-typing([eqwalizer]).

-export([
    build_voice_token_rpc_request/6,
    build_voice_token_rpc_request/7,
    build_voice_token_rpc_request/8,
    build_force_disconnect_rpc_request/4,
    build_list_participants_rpc_request/4,
    build_update_participant_rpc_request/5,
    build_update_participant_rpc_request/6,
    build_update_participant_permissions_rpc_request/5,
    add_geolocation_to_request/3,
    add_rtc_region_to_request/2,
    apply_voice_permissions_to_flags/2,
    compute_voice_permissions/3,
    generate_token_nonce/0
]).

-type guild_state() :: map().
-type coordinate_input() :: term().
-type voice_permissions() :: #{
    can_speak := boolean(),
    can_stream := boolean(),
    can_video := boolean(),
    deaf => boolean()
}.
-export_type([
    guild_state/0,
    coordinate_input/0,
    voice_permissions/0
]).

-spec apply_voice_permissions_to_flags(map(), voice_permissions() | map()) -> map().
apply_voice_permissions_to_flags(Flags, VoicePermissions) ->
    Suppress =
        case maps:get(can_speak, VoicePermissions, true) of
            false -> true;
            _ -> false
        end,
    SelfStream =
        case maps:get(can_stream, VoicePermissions, true) of
            false -> false;
            _ -> maps:get(self_stream, Flags, false) =:= true
        end,
    SelfVideo =
        case maps:get(can_video, VoicePermissions, true) of
            false -> false;
            _ -> maps:get(self_video, Flags, false) =:= true
        end,
    Flags#{
        suppress => Suppress,
        self_stream => SelfStream,
        self_video => SelfVideo
    }.

-spec build_voice_token_rpc_request(
    integer() | null,
    integer(),
    integer(),
    binary() | integer() | null,
    coordinate_input(),
    coordinate_input()
) -> map().
build_voice_token_rpc_request(GuildId, ChannelId, UserId, ConnectionId, Latitude, Longitude) ->
    BaseReq0 = #{
        <<"type">> => <<"voice_get_token">>,
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"user_id">> => integer_to_binary(UserId)
    },
    BaseReq =
        case GuildId of
            null -> BaseReq0;
            _ -> BaseReq0#{<<"guild_id">> => integer_to_binary(GuildId)}
        end,
    WithConnection = add_connection_id_to_request(BaseReq, ConnectionId),
    add_geolocation_to_request(WithConnection, Latitude, Longitude).

-spec add_geolocation_to_request(
    map(),
    coordinate_input(),
    coordinate_input()
) -> map().
add_geolocation_to_request(RequestMap, Latitude, Longitude) ->
    case {normalise_coordinate(Latitude), normalise_coordinate(Longitude)} of
        {Lat, Long} when is_binary(Lat) andalso is_binary(Long) ->
            RequestMap#{
                <<"latitude">> => Lat,
                <<"longitude">> => Long
            };
        _ ->
            RequestMap
    end.

-spec normalise_coordinate(coordinate_input()) -> binary() | undefined.
normalise_coordinate(undefined) ->
    undefined;
normalise_coordinate(null) ->
    undefined;
normalise_coordinate(Value) when is_binary(Value) ->
    Value;
normalise_coordinate(Value) when is_integer(Value) ->
    integer_to_binary(Value);
normalise_coordinate(Value) when is_float(Value) ->
    float_to_binary(Value, [short]);
normalise_coordinate(Value) when is_list(Value) ->
    guild_voice_connection_normalize:normalize_optional_binary(Value);
normalise_coordinate(_Value) ->
    undefined.

-spec add_rtc_region_to_request(map(), binary() | null) -> map().
add_rtc_region_to_request(RequestMap, Region) ->
    case Region of
        RegionBin when is_binary(RegionBin) ->
            RequestMap#{<<"rtc_region">> => RegionBin};
        _ ->
            RequestMap
    end.

-spec add_connection_id_to_request(map(), binary() | integer() | null) -> map().
add_connection_id_to_request(RequestMap, ConnectionId) ->
    case ConnectionId of
        null ->
            RequestMap;
        ConnectionIdBin when is_binary(ConnectionIdBin) ->
            RequestMap#{<<"connection_id">> => ConnectionIdBin};
        ConnectionIdInt when is_integer(ConnectionIdInt) ->
            RequestMap#{<<"connection_id">> => integer_to_binary(ConnectionIdInt)};
        _ ->
            RequestMap
    end.

-spec build_force_disconnect_rpc_request(integer() | null, integer(), integer(), binary()) ->
    map().
build_force_disconnect_rpc_request(GuildId, ChannelId, UserId, ConnectionId) ->
    BaseReq = #{
        <<"type">> => <<"voice_force_disconnect_participant">>,
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"user_id">> => integer_to_binary(UserId),
        <<"connection_id">> => ConnectionId
    },
    case GuildId of
        null ->
            BaseReq;
        _ ->
            BaseReq#{<<"guild_id">> => integer_to_binary(GuildId)}
    end.

-spec build_list_participants_rpc_request(
    integer() | null, integer(), binary(), binary()
) -> map().
build_list_participants_rpc_request(GuildId, ChannelId, RegionId, ServerId) ->
    BaseReq = #{
        <<"type">> => <<"voice_list_participants">>,
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"region_id">> => RegionId,
        <<"server_id">> => ServerId
    },
    case GuildId of
        null ->
            BaseReq;
        _ ->
            BaseReq#{<<"guild_id">> => integer_to_binary(GuildId)}
    end.

-spec build_update_participant_rpc_request(
    integer() | null, integer(), integer(), boolean(), boolean()
) -> map().
build_update_participant_rpc_request(GuildId, ChannelId, UserId, Mute, Deaf) ->
    build_update_participant_rpc_request(
        GuildId,
        ChannelId,
        UserId,
        Mute,
        Deaf,
        #{can_speak => true, can_stream => true, can_video => true}
    ).

-spec build_update_participant_rpc_request(
    integer() | null, integer(), integer(), boolean(), boolean(), voice_permissions()
) -> map().
build_update_participant_rpc_request(GuildId, ChannelId, UserId, Mute, Deaf, VoicePermissions) ->
    BaseReq = #{
        <<"type">> => <<"voice_update_participant">>,
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"user_id">> => integer_to_binary(UserId),
        <<"mute">> => Mute,
        <<"deaf">> => Deaf,
        <<"can_speak">> => maps:get(can_speak, VoicePermissions, true),
        <<"can_stream">> => maps:get(can_stream, VoicePermissions, true),
        <<"can_video">> => maps:get(can_video, VoicePermissions, true)
    },
    case GuildId of
        null ->
            BaseReq;
        _ ->
            BaseReq#{<<"guild_id">> => integer_to_binary(GuildId)}
    end.

-spec build_update_participant_permissions_rpc_request(
    integer() | null, integer(), integer(), binary(), voice_permissions()
) -> map().
build_update_participant_permissions_rpc_request(
    GuildId, ChannelId, UserId, ConnectionId, VoicePermissions
) ->
    BaseReq = #{
        <<"type">> => <<"voice_update_participant_permissions">>,
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"user_id">> => integer_to_binary(UserId),
        <<"connection_id">> => ConnectionId,
        <<"can_speak">> => maps:get(can_speak, VoicePermissions, true),
        <<"can_stream">> => maps:get(can_stream, VoicePermissions, true),
        <<"can_video">> => maps:get(can_video, VoicePermissions, true),
        <<"deaf">> => maps:get(deaf, VoicePermissions, false)
    },
    case GuildId of
        null ->
            BaseReq;
        _ ->
            BaseReq#{<<"guild_id">> => integer_to_binary(GuildId)}
    end.

-spec compute_voice_permissions(integer(), integer(), guild_state()) -> voice_permissions().
compute_voice_permissions(UserId, ChannelId, State) ->
    Permissions = guild_permissions:get_member_permissions(UserId, ChannelId, State),
    SpeakPerm = constants:speak_permission(),
    StreamPerm = constants:stream_permission(),
    AdminPerm = constants:administrator_permission(),
    IsAdmin = permission_bits:has(Permissions, AdminPerm),
    CanSpeak = IsAdmin orelse permission_bits:has(Permissions, SpeakPerm),
    CanStream = IsAdmin orelse permission_bits:has(Permissions, StreamPerm),
    HasVirtualAccess = guild_virtual_channel_access:has_virtual_access(
        UserId, ChannelId, State
    ),
    FinalCanSpeak = CanSpeak orelse HasVirtualAccess,
    FinalCanStream = CanStream orelse HasVirtualAccess,
    #{
        can_speak => FinalCanSpeak,
        can_stream => FinalCanStream,
        can_video => FinalCanStream
    }.

-spec build_voice_token_rpc_request(
    integer() | null,
    integer(),
    integer(),
    binary() | integer() | null,
    coordinate_input(),
    coordinate_input(),
    voice_permissions()
) -> map().
build_voice_token_rpc_request(
    GuildId, ChannelId, UserId, ConnectionId, Latitude, Longitude, VoicePermissions
) ->
    build_voice_token_rpc_request(
        GuildId, ChannelId, UserId, ConnectionId, Latitude, Longitude, VoicePermissions, null
    ).

-spec build_voice_token_rpc_request(
    integer() | null,
    integer(),
    integer(),
    binary() | integer() | null,
    coordinate_input(),
    coordinate_input(),
    voice_permissions(),
    binary() | null
) -> map().
build_voice_token_rpc_request(
    GuildId, ChannelId, UserId, ConnectionId, Latitude, Longitude, VoicePermissions, TokenNonce
) ->
    BaseReq = build_voice_token_rpc_request(
        GuildId, ChannelId, UserId, ConnectionId, Latitude, Longitude
    ),
    Req0 = BaseReq#{
        <<"can_speak">> => maps:get(can_speak, VoicePermissions, true),
        <<"can_stream">> => maps:get(can_stream, VoicePermissions, true),
        <<"can_video">> => maps:get(can_video, VoicePermissions, true)
    },
    case TokenNonce of
        null -> Req0;
        undefined -> Req0;
        _ when is_binary(TokenNonce) -> Req0#{<<"token_nonce">> => TokenNonce};
        _ -> Req0
    end.

-spec generate_token_nonce() -> binary().
generate_token_nonce() ->
    Bytes = crypto:strong_rand_bytes(16),
    binary:encode_hex(Bytes, lowercase).
