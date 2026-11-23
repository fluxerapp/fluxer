// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/option.{type Option}

pub const version = 1

pub type CallerV1 {
  CallerV1(user_id: Int, name: String, avatar: Option(String))
}

pub type VoiceStateV1 {
  VoiceStateV1(
    channel_id: Int,
    user_id: Int,
    connection_id: String,
    seq: Int,
    entry: BitArray,
  )
}

pub type MemberV1 {
  MemberV1(
    session_id: String,
    user_id: Int,
    inbox: BitArray,
    owner: Option(BitArray),
    detached_at: Option(Int),
  )
}

pub type PendingJoinV1 {
  PendingJoinV1(
    connection_id: String,
    user_id: Int,
    session_id: String,
    joined_at: Int,
  )
}

pub type LocatedV1 {
  LocatedV1(connection_id: String, geo: BitArray)
}

pub type CallTailV1 {
  CallTailV1(
    channel_id: Int,
    message_id: Int,
    region: Option(String),
    ringing: List(Int),
    pending_ringing: List(Int),
    recipients: List(Int),
    caller: Option(CallerV1),
    voice_states: List(VoiceStateV1),
    next_seq: Int,
    members: List(MemberV1),
    pending: List(PendingJoinV1),
    initiator_ready: Bool,
    created_at: Int,
    history: List(Int),
    last_event: Option(BitArray),
    room_key: Option(String),
    located: List(LocatedV1),
    nodes: List(String),
  )
}

pub type InboxV1 {
  InboxV1(nodes: List(String), message: BitArray)
}

pub type TargetV1 {
  TargetV1(nodes: List(String), subject: BitArray)
}
