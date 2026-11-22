// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId, type RoleId, type UserId}
import fluxer_gateway/core/permissions.{type Permissions}
import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/int
import gleam/option.{type Option, None}

pub opaque type Channel {
  Channel(
    id: ChannelId,
    kind: ChannelKind,
    parent: Option(ChannelId),
    position: Int,
    overwrites: List(Overwrite),
    nsfw: Nsfw,
    named: Bool,
    voice: VoiceLimits,
    last_message: Option(Int),
    raw: RawJson,
  )
}

pub type VoiceLimits {
  VoiceLimits(user_limit: Int, connection_limit: Int, rtc_p2p: Bool)
}

const default_connection_limit = 5

const max_connection_limit = 100

pub opaque type Overwrite {
  Overwrite(target: OverwriteTarget, allow: Permissions, deny: Permissions)
}

pub type ChannelKind {
  Text
  Dm
  Voice
  GroupDm
  Category
  Announcement
  AnnouncementThread
  PublicThread
  PrivateThread
  Forum
  Media
  OtherKind(type_code: Int)
}

pub type OverwriteTarget {
  RoleTarget(RoleId)
  MemberTarget(UserId)
}

pub type Nsfw {
  InheritNsfw
  SafeForWork
  AdultOnly
}

pub fn kind_from_int(code: Int) -> ChannelKind {
  case code {
    0 -> Text
    1 -> Dm
    2 -> Voice
    3 -> GroupDm
    4 -> Category
    5 -> Announcement
    10 -> AnnouncementThread
    11 -> PublicThread
    12 -> PrivateThread
    15 -> Forum
    16 -> Media
    other -> OtherKind(other)
  }
}

pub fn kind_to_int(kind: ChannelKind) -> Int {
  case kind {
    Text -> 0
    Dm -> 1
    Voice -> 2
    GroupDm -> 3
    Category -> 4
    Announcement -> 5
    AnnouncementThread -> 10
    PublicThread -> 11
    PrivateThread -> 12
    Forum -> 15
    Media -> 16
    OtherKind(code) -> code
  }
}

pub fn is_thread_kind(kind: ChannelKind) -> Bool {
  case kind {
    AnnouncementThread | PublicThread | PrivateThread -> True
    _ -> False
  }
}

pub fn is_thread_only_kind(kind: ChannelKind) -> Bool {
  case kind {
    Forum | Media -> True
    _ -> False
  }
}

pub fn new(
  id id: ChannelId,
  kind kind: ChannelKind,
  parent parent: Option(ChannelId),
  position position: Int,
  overwrites overwrites: List(Overwrite),
  raw raw: RawJson,
) -> Channel {
  Channel(
    id:,
    kind:,
    parent:,
    position:,
    overwrites:,
    nsfw: InheritNsfw,
    named: True,
    voice: default_voice_limits(),
    last_message: None,
    raw:,
  )
}

pub fn default_voice_limits() -> VoiceLimits {
  VoiceLimits(
    user_limit: 0,
    connection_limit: default_connection_limit,
    rtc_p2p: False,
  )
}

pub fn voice_limits(
  user_limit user_limit: Int,
  connection_limit connection_limit: Int,
  rtc_p2p rtc_p2p: Bool,
) -> VoiceLimits {
  VoiceLimits(
    user_limit: int.max(user_limit, 0),
    connection_limit: case connection_limit {
      limit if limit >= 1 && limit <= max_connection_limit -> limit
      limit if limit > max_connection_limit -> max_connection_limit
      _ -> default_connection_limit
    },
    rtc_p2p:,
  )
}

pub fn overwrite(
  target target: OverwriteTarget,
  allow allow: Permissions,
  deny deny: Permissions,
) -> Overwrite {
  Overwrite(target:, allow:, deny:)
}

pub fn id(channel: Channel) -> ChannelId {
  channel.id
}

pub fn kind(channel: Channel) -> ChannelKind {
  channel.kind
}

pub fn parent(channel: Channel) -> Option(ChannelId) {
  channel.parent
}

pub fn position(channel: Channel) -> Int {
  channel.position
}

pub fn overwrites(channel: Channel) -> List(Overwrite) {
  channel.overwrites
}

pub fn nsfw(channel: Channel) -> Nsfw {
  channel.nsfw
}

pub fn is_named(channel: Channel) -> Bool {
  channel.named
}

pub fn voice(channel: Channel) -> VoiceLimits {
  channel.voice
}

pub fn last_message(channel: Channel) -> Option(Int) {
  channel.last_message
}

pub fn raw(channel: Channel) -> RawJson {
  channel.raw
}

pub fn set_overwrites(
  channel: Channel,
  overwrites: List(Overwrite),
) -> Channel {
  Channel(..channel, overwrites:)
}

pub fn set_nsfw(channel: Channel, nsfw: Nsfw) -> Channel {
  Channel(..channel, nsfw:)
}

pub fn set_named(channel: Channel, named: Bool) -> Channel {
  Channel(..channel, named:)
}

pub fn set_voice(channel: Channel, voice: VoiceLimits) -> Channel {
  Channel(..channel, voice:)
}

pub fn set_last_message(
  channel: Channel,
  last_message: Option(Int),
) -> Channel {
  Channel(..channel, last_message:)
}

pub fn set_raw(channel: Channel, raw: RawJson) -> Channel {
  Channel(..channel, raw:)
}

pub fn overwrite_target(overwrite: Overwrite) -> OverwriteTarget {
  overwrite.target
}

pub fn overwrite_allow(overwrite: Overwrite) -> Permissions {
  overwrite.allow
}

pub fn overwrite_deny(overwrite: Overwrite) -> Permissions {
  overwrite.deny
}
