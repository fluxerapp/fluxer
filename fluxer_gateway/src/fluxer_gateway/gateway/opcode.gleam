// SPDX-License-Identifier: AGPL-3.0-or-later

pub type Opcode {
  Dispatch
  Heartbeat
  Identify
  PresenceUpdate
  VoiceStateUpdate
  VoiceServerPing
  Resume
  Reconnect
  RequestGuildMembers
  InvalidSession
  Hello
  HeartbeatAck
  GatewayError
  VoiceSignal
  LazyRequest
  RequestGuildCounts
  RequestChannelMemberCounts
  RequestForumUnreads
  Unknown
}

pub fn from_int(value: Int) -> Opcode {
  case value {
    0 -> Dispatch
    1 -> Heartbeat
    2 -> Identify
    3 -> PresenceUpdate
    4 -> VoiceStateUpdate
    5 -> VoiceServerPing
    6 -> Resume
    7 -> Reconnect
    8 -> RequestGuildMembers
    9 -> InvalidSession
    10 -> Hello
    11 -> HeartbeatAck
    12 -> GatewayError
    13 -> VoiceSignal
    14 -> LazyRequest
    15 -> RequestGuildCounts
    16 -> RequestChannelMemberCounts
    28 -> RequestForumUnreads
    _ -> Unknown
  }
}

pub fn to_int(opcode: Opcode) -> Result(Int, Nil) {
  case opcode {
    Dispatch -> Ok(0)
    Heartbeat -> Ok(1)
    Identify -> Ok(2)
    PresenceUpdate -> Ok(3)
    VoiceStateUpdate -> Ok(4)
    VoiceServerPing -> Ok(5)
    Resume -> Ok(6)
    Reconnect -> Ok(7)
    RequestGuildMembers -> Ok(8)
    InvalidSession -> Ok(9)
    Hello -> Ok(10)
    HeartbeatAck -> Ok(11)
    GatewayError -> Ok(12)
    VoiceSignal -> Ok(13)
    LazyRequest -> Ok(14)
    RequestGuildCounts -> Ok(15)
    RequestChannelMemberCounts -> Ok(16)
    RequestForumUnreads -> Ok(28)
    Unknown -> Error(Nil)
  }
}
