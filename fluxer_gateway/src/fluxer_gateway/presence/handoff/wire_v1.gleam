// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/option.{type Option}

pub const version = 1

pub type SubjectV1 {
  SubjectV1(node: String, term: BitArray)
}

pub type EpochV1 {
  EpochV1(node: String, minted_ns: Int, unique: Int)
}

pub type VersionV1 {
  VersionV1(epoch: EpochV1, seq: Int)
}

pub type DispatchIdV1 {
  DispatchIdV1(node: String, serial: Int)
}

pub type CustomStatusV1 {
  CustomStatusV1(
    wire: BitArray,
    expires_at_ms: Option(Int),
    payload: List(#(String, BitArray)),
  )
}

pub type ProfileV1 {
  ProfileV1(fields: List(#(String, BitArray)), restricted: Bool, bot: Bool)
}

pub type EventV1 {
  EventV1(
    user_updated: Bool,
    user: Int,
    version: VersionV1,
    display: Int,
    custom_status: BitArray,
    user_wire: BitArray,
    wire: BitArray,
  )
}

pub type PublishedV1 {
  PublishedV1(
    display: Int,
    custom_status_key: BitArray,
    custom_status_wire: BitArray,
  )
}

pub type SessionRefV1 {
  SessionRefV1(session_id: String, user_id: Int, inbox: SubjectV1)
}

pub type CarriedSessionV1 {
  CarriedSessionV1(
    session: SessionRefV1,
    status: String,
    afk: Bool,
    mobile: Bool,
  )
}

pub type PendingPushV1 {
  PendingPushV1(
    channel_id: Int,
    message_id: Int,
    data: BitArray,
    author_id: Int,
    buffered_at_ms: Int,
  )
}

pub type PushBufferV1 {
  PushBufferV1(
    entries: List(PendingPushV1),
    read_marks: List(#(Int, Int)),
    eligible: Bool,
  )
}

pub type DoomV1 {
  DoomV1(session_id: String, matching: Option(List(String)), at_ms: Int)
}

pub type CarriedV1 {
  CarriedV1(
    version: VersionV1,
    last: Option(PublishedV1),
    current: Option(EventV1),
    temporary_guilds: List(Int),
    push: PushBufferV1,
    sessions: List(CarriedSessionV1),
    validating: Option(CustomStatusV1),
    delivered: List(DispatchIdV1),
    doomed: List(DoomV1),
  )
}

pub type PresenceBlobV1 {
  PresenceBlobV1(
    user_id: Int,
    profile: ProfileV1,
    custom_status: Option(CustomStatusV1),
    settings_invisible: Bool,
    guilds: List(Int),
    targets: List(Int),
    push_max_entries: Int,
    push_max_bytes: Int,
    carried: Option(CarriedV1),
  )
}

pub type SubscriberV1 {
  PresenceSubscriberV1(user: Int, inbox: SubjectV1)
  GuildSubscriberV1(guild: Int, inbox: SubjectV1)
}

pub type CatchupV1 {
  EverythingV1
  ChangedSinceV1(ms: Int, stamp: Int)
}

pub type OpValueV1 {
  OpAbsentV1
  OpNullV1
  OpMapV1(status: CustomStatusV1)
  OpOtherV1
}

pub type SettingsValueV1 {
  SettingsAbsentV1
  SettingsMapV1(status: CustomStatusV1)
  SettingsOtherV1
}

pub type DmMessageV1 {
  DmMessageV1(
    author: Int,
    channel: Option(Int),
    message: Option(Int),
    suppressed: Bool,
    data: BitArray,
  )
}

pub type UserEventV1 {
  UserUpdateV1(profile: ProfileV1)
  UserSettingsUpdateV1(custom_status: SettingsValueV1, invisible: Option(Bool))
  MessageCreateV1(message: Option(DmMessageV1))
  MessageAckV1(channel: Option(Int), message: Option(Int))
  OtherEventV1
}

pub type GuildMembershipV1 {
  JoinGuildV1(guild: Int)
  LeaveGuildV1(guild: Int)
  AddTemporaryGuildV1(guild: Int)
  RemoveTemporaryGuildV1(guild: Int)
}

pub type InboxV1 {
  ConnectSessionV1(
    session: SessionRefV1,
    status: String,
    afk: Bool,
    mobile: Bool,
    guilds: List(Int),
    targets: List(Int),
    reply: SubjectV1,
  )
  DisconnectV1(session_id: String, owner: SubjectV1)
  UpdateV1(
    session_id: String,
    status: String,
    afk: Bool,
    mobile: Bool,
    custom_status: OpValueV1,
  )
  SubscribeTargetsV1(users: List(Int))
  UnsubscribeTargetsV1(users: List(Int))
  ForeignV1(event: EventV1)
  ResendV1(to: SubscriberV1)
  ResendNodeV1(node: String)
  ResendSinceV1(to: SubscriberV1, since_ms: Int)
  JoinedV1(to: SubscriberV1, catchup: CatchupV1)
  UserDispatchV1(
    id: DispatchIdV1,
    event: String,
    data: BitArray,
    parsed: UserEventV1,
  )
  GuildV1(operation: GuildMembershipV1)
  VisitStartV1(guild: Int, operator: Bool)
  VisitEndV1(guild: Int)
  TerminateSessionsV1(hashes: List(String), reply: SubjectV1)
  TerminateAllV1
  RejoinV1
}
