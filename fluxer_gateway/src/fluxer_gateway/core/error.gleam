// SPDX-License-Identifier: AGPL-3.0-or-later

pub type CloseCode {
  CloseUnknownError
  UnknownOpcode
  DecodeError
  NotAuthenticated
  AuthenticationFailed
  AlreadyAuthenticated
  InvalidSeq
  CloseRateLimited
  SessionTimeout
  InvalidShard
  ShardingRequired
  InvalidApiVersion
}

pub fn close_code_to_int(code: CloseCode) -> Int {
  case code {
    CloseUnknownError -> 4000
    UnknownOpcode -> 4001
    DecodeError -> 4002
    NotAuthenticated -> 4003
    AuthenticationFailed -> 4004
    AlreadyAuthenticated -> 4005
    InvalidSeq -> 4007
    CloseRateLimited -> 4008
    SessionTimeout -> 4009
    InvalidShard -> 4010
    ShardingRequired -> 4011
    InvalidApiVersion -> 4012
  }
}

pub type GatewayError {
  VoiceConnectionNotFound
  VoiceChannelNotFound
  VoiceChannelNotVoice
  VoiceMemberNotFound
  VoiceUserNotInVoice
  VoiceGuildNotFound
  VoicePermissionDenied
  VoiceMemberTimedOut
  VoiceChannelFull
  VoiceConnectionLimitReached
  VoiceMissingConnectionId
  VoiceInvalidUserId
  VoiceInvalidChannelId
  VoiceInvalidState
  VoiceUserMismatch
  VoiceTokenFailed
  VoiceGuildIdMissing
  VoiceInvalidGuildId
  VoiceModeratorMissingConnect
  VoiceUnclaimedAccount
  VoiceAccountVerificationRequired
  VoiceUpdateRateLimited
  VoiceNonceMismatch
  VoicePendingExpired
  VoiceCameraUserLimit
  VoiceE2eeRequired
  VoiceP2pConsentRequired
  VoiceP2pUnavailable
  DmChannelNotFound
  DmNotRecipient
  DmInvalidChannelType
  ValidationInvalidSnowflake
  ValidationNullSnowflake
  ValidationInvalidSnowflakeList
  ValidationExpectedList
  ValidationExpectedMap
  ValidationMissingField
  ValidationInvalidParams
  InternalError
  TimeoutError
  UnknownError
  UnlistedError
}

pub type VoiceErrorInfo {
  VoiceErrorInfo(code: String, message: String)
}

pub fn voice_info(error: GatewayError) -> Result(VoiceErrorInfo, Nil) {
  case error {
    VoiceConnectionNotFound ->
      info("VOICE_CONNECTION_NOT_FOUND", "Voice connection not found")
    VoiceChannelNotFound ->
      info("VOICE_CHANNEL_NOT_FOUND", "Voice channel not found")
    VoiceChannelNotVoice ->
      info("VOICE_INVALID_CHANNEL_TYPE", "Channel is not a voice channel")
    VoiceMemberNotFound -> info("VOICE_MEMBER_NOT_FOUND", "Member not found")
    VoiceUserNotInVoice ->
      info("VOICE_USER_NOT_IN_VOICE", "User is not in a voice channel")
    VoiceGuildNotFound -> info("VOICE_GUILD_NOT_FOUND", "Guild not found")
    VoiceMissingConnectionId ->
      info("VOICE_MISSING_CONNECTION_ID", "Connection ID is required")
    VoiceInvalidUserId -> info("VOICE_INVALID_USER_ID", "Invalid user ID")
    VoiceInvalidChannelId ->
      info("VOICE_INVALID_CHANNEL_ID", "Invalid channel ID")
    VoiceInvalidState -> info("VOICE_INVALID_STATE", "Invalid voice state")
    VoiceUserMismatch ->
      info("VOICE_USER_MISMATCH", "User does not match connection")
    VoiceTokenFailed ->
      info("VOICE_TOKEN_FAILED", "Failed to obtain voice token")
    VoiceGuildIdMissing ->
      info("VOICE_GUILD_ID_MISSING", "Guild ID is required")
    VoiceInvalidGuildId -> info("VOICE_INVALID_GUILD_ID", "Invalid guild ID")
    VoiceUpdateRateLimited ->
      info("VOICE_UPDATE_RATE_LIMITED", "Voice updates are rate limited")
    VoiceNonceMismatch ->
      info("VOICE_NONCE_MISMATCH", "Voice token nonce mismatch")
    VoicePendingExpired ->
      info("VOICE_PENDING_EXPIRED", "Voice pending connection expired")
    VoicePermissionDenied ->
      info("VOICE_PERMISSION_DENIED", "Missing voice permissions")
    VoiceMemberTimedOut ->
      info("VOICE_MEMBER_TIMED_OUT", "Voice member is timed out")
    VoiceChannelFull -> info("VOICE_CHANNEL_FULL", "Voice channel is full")
    VoiceConnectionLimitReached ->
      info("VOICE_CONNECTION_LIMIT_REACHED", "Voice connection limit reached")
    VoiceModeratorMissingConnect ->
      info("VOICE_PERMISSION_DENIED", "Moderator missing connect permission")
    VoiceUnclaimedAccount ->
      info("VOICE_UNCLAIMED_ACCOUNT", "Claim your account to join voice")
    VoiceAccountVerificationRequired ->
      info(
        "VOICE_ACCOUNT_VERIFICATION_REQUIRED",
        "Confirm an email address to join voice",
      )
    VoiceCameraUserLimit ->
      info(
        "VOICE_CAMERA_USER_LIMIT",
        "Too many users in channel to enable camera",
      )
    VoiceE2eeRequired ->
      info(
        "VOICE_E2EE_REQUIRED",
        "This voice channel is end-to-end encrypted and requires a client with E2EE support",
      )
    VoiceP2pConsentRequired ->
      info(
        "VOICE_P2P_CONSENT_REQUIRED",
        "This call is peer-to-peer and requires agreement to join",
      )
    VoiceP2pUnavailable ->
      info(
        "VOICE_P2P_UNAVAILABLE",
        "Peer-to-peer voice is unavailable for this join",
      )
    _ -> Error(Nil)
  }
}

fn info(code: String, message: String) -> Result(VoiceErrorInfo, Nil) {
  Ok(VoiceErrorInfo(code:, message:))
}

pub type RpcError {
  RpcFailed(message: String)
  InvalidParams
  RpcTimeout
  RpcInternalError
  UnknownMethod(method: String)
}

pub fn rpc_error_message(error: RpcError) -> String {
  case error {
    RpcFailed(message) -> message
    InvalidParams -> "invalid_params"
    RpcTimeout -> "timeout"
    RpcInternalError -> "internal_error"
    UnknownMethod(method) -> "Unknown method: " <> method
  }
}

pub type Unavailable {
  Unavailable(retry_after_ms: Int)
  Overloaded(retry_after_ms: Int)
}
