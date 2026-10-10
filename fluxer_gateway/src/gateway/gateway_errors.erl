%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_errors).
-typing([eqwalizer]).

-compile({no_auto_import, [error/1]}).

-export([
    error/1,
    error_category/1,
    voice_error_info/1
]).
-export_type([error_atom/0, error_category/0]).

-type error_atom() ::
    voice_connection_not_found
    | voice_channel_not_found
    | voice_channel_not_voice
    | voice_member_not_found
    | voice_user_not_in_voice
    | voice_guild_not_found
    | voice_permission_denied
    | voice_member_timed_out
    | voice_channel_full
    | voice_connection_limit_reached
    | voice_missing_connection_id
    | voice_invalid_user_id
    | voice_invalid_channel_id
    | voice_invalid_state
    | voice_user_mismatch
    | voice_token_failed
    | voice_guild_id_missing
    | voice_invalid_guild_id
    | voice_moderator_missing_connect
    | voice_unclaimed_account
    | voice_update_rate_limited
    | voice_nonce_mismatch
    | voice_pending_expired
    | voice_camera_user_limit
    | voice_e2ee_required
    | voice_p2p_consent_required
    | voice_p2p_unavailable
    | dm_channel_not_found
    | dm_not_recipient
    | dm_invalid_channel_type
    | validation_invalid_snowflake
    | validation_null_snowflake
    | validation_invalid_snowflake_list
    | validation_expected_list
    | validation_expected_map
    | validation_missing_field
    | validation_invalid_params
    | internal_error
    | timeout
    | unknown_error
    | atom().

-type error_category() ::
    not_found
    | validation_error
    | permission_denied
    | voice_error
    | rate_limited
    | timeout
    | unknown
    | auth_failed.

-type voice_error_info() :: {binary(), binary(), error_category()}.

-spec error(error_atom()) -> {error, error_category(), error_atom()}.
error(ErrorAtom) ->
    {error, error_category(ErrorAtom), ErrorAtom}.

-spec error_category(error_atom()) -> error_category().
error_category(ErrorAtom) ->
    case voice_error_category(ErrorAtom) of
        undefined -> common_error_category(ErrorAtom);
        Category -> Category
    end.

-spec voice_error_category(error_atom()) -> error_category() | undefined.
voice_error_category(ErrorAtom) ->
    case voice_error_info(ErrorAtom) of
        {_Code, _Message, Category} -> Category;
        undefined -> undefined
    end.

-spec voice_error_info(error_atom()) -> voice_error_info() | undefined.
voice_error_info(ErrorAtom) ->
    P2pInfo = maps:get(ErrorAtom, voice_p2p_error_infos(), undefined),
    maps:get(ErrorAtom, voice_error_infos(), P2pInfo).

-spec voice_error_infos() -> #{error_atom() => voice_error_info()}.
voice_error_infos() ->
    #{
        voice_connection_not_found =>
            {<<"VOICE_CONNECTION_NOT_FOUND">>, <<"Voice connection not found">>, not_found},
        voice_channel_not_found =>
            {<<"VOICE_CHANNEL_NOT_FOUND">>, <<"Voice channel not found">>, not_found},
        voice_channel_not_voice =>
            {<<"VOICE_INVALID_CHANNEL_TYPE">>, <<"Channel is not a voice channel">>,
                validation_error},
        voice_member_not_found =>
            {<<"VOICE_MEMBER_NOT_FOUND">>, <<"Member not found">>, not_found},
        voice_user_not_in_voice =>
            {<<"VOICE_USER_NOT_IN_VOICE">>, <<"User is not in a voice channel">>, not_found},
        voice_guild_not_found =>
            {<<"VOICE_GUILD_NOT_FOUND">>, <<"Guild not found">>, not_found},
        voice_permission_denied =>
            {<<"VOICE_PERMISSION_DENIED">>, <<"Missing voice permissions">>, permission_denied},
        voice_member_timed_out =>
            {<<"VOICE_MEMBER_TIMED_OUT">>, <<"Voice member is timed out">>, permission_denied},
        voice_channel_full =>
            {<<"VOICE_CHANNEL_FULL">>, <<"Voice channel is full">>, permission_denied},
        voice_connection_limit_reached =>
            {<<"VOICE_CONNECTION_LIMIT_REACHED">>, <<"Voice connection limit reached">>,
                permission_denied},
        voice_missing_connection_id =>
            {<<"VOICE_MISSING_CONNECTION_ID">>, <<"Connection ID is required">>,
                validation_error},
        voice_invalid_user_id =>
            {<<"VOICE_INVALID_USER_ID">>, <<"Invalid user ID">>, validation_error},
        voice_invalid_channel_id =>
            {<<"VOICE_INVALID_CHANNEL_ID">>, <<"Invalid channel ID">>, validation_error},
        voice_invalid_state =>
            {<<"VOICE_INVALID_STATE">>, <<"Invalid voice state">>, validation_error},
        voice_user_mismatch =>
            {<<"VOICE_USER_MISMATCH">>, <<"User does not match connection">>, validation_error},
        voice_token_failed =>
            {<<"VOICE_TOKEN_FAILED">>, <<"Failed to obtain voice token">>, voice_error},
        voice_guild_id_missing =>
            {<<"VOICE_GUILD_ID_MISSING">>, <<"Guild ID is required">>, validation_error},
        voice_invalid_guild_id =>
            {<<"VOICE_INVALID_GUILD_ID">>, <<"Invalid guild ID">>, validation_error},
        voice_moderator_missing_connect =>
            {<<"VOICE_PERMISSION_DENIED">>, <<"Moderator missing connect permission">>,
                permission_denied},
        voice_unclaimed_account =>
            {<<"VOICE_UNCLAIMED_ACCOUNT">>, <<"Claim your account to join voice">>,
                permission_denied},
        voice_update_rate_limited =>
            {<<"VOICE_UPDATE_RATE_LIMITED">>, <<"Voice updates are rate limited">>,
                rate_limited},
        voice_nonce_mismatch =>
            {<<"VOICE_NONCE_MISMATCH">>, <<"Voice token nonce mismatch">>, validation_error},
        voice_pending_expired =>
            {<<"VOICE_PENDING_EXPIRED">>, <<"Voice pending connection expired">>,
                validation_error},
        voice_camera_user_limit =>
            {<<"VOICE_CAMERA_USER_LIMIT">>, <<"Too many users in channel to enable camera">>,
                permission_denied},
        voice_e2ee_required =>
            {<<"VOICE_E2EE_REQUIRED">>, voice_e2ee_required_message(), permission_denied}
    }.

-spec voice_p2p_error_infos() -> #{error_atom() => voice_error_info()}.
voice_p2p_error_infos() ->
    #{
        voice_p2p_consent_required =>
            {<<"VOICE_P2P_CONSENT_REQUIRED">>,
                <<"This call is peer-to-peer and requires agreement to join">>,
                permission_denied},
        voice_p2p_unavailable =>
            {<<"VOICE_P2P_UNAVAILABLE">>, <<"Peer-to-peer voice is unavailable for this join">>,
                voice_error}
    }.

-spec voice_e2ee_required_message() -> binary().
voice_e2ee_required_message() ->
    iolist_to_binary([
        "This voice channel is end-to-end encrypted and requires ",
        "a client with E2EE support"
    ]).

-spec common_error_category(error_atom()) -> error_category().
common_error_category(dm_channel_not_found) -> not_found;
common_error_category(dm_not_recipient) -> permission_denied;
common_error_category(dm_invalid_channel_type) -> validation_error;
common_error_category(validation_invalid_snowflake) -> validation_error;
common_error_category(validation_null_snowflake) -> validation_error;
common_error_category(validation_invalid_snowflake_list) -> validation_error;
common_error_category(validation_expected_list) -> validation_error;
common_error_category(validation_expected_map) -> validation_error;
common_error_category(validation_missing_field) -> validation_error;
common_error_category(validation_invalid_params) -> validation_error;
common_error_category(internal_error) -> unknown;
common_error_category(timeout) -> timeout;
common_error_category(unknown_error) -> unknown;
common_error_category(_) -> unknown.
