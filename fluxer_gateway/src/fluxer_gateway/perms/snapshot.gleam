// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/id.{type ChannelId, type GuildId, type UserId}
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/role.{type Role}
import fluxer_gateway/perms/cutoff.{type Cutoff}
import fluxer_gateway/perms/event.{type Event}
import fluxer_gateway/perms/native
import gleam/list
import gleam/option.{type Option, None}

pub opaque type Snapshot {
  Snapshot(
    guild: GuildId,
    owner: Option(UserId),
    roles: List(Role),
    channels: List(Channel),
    members: List(Event),
    virtual: List(Event),
    cutoff: Cutoff,
    nsfw: Option(Bool),
    nsfw_level: Int,
  )
}

pub fn new(guild: GuildId) -> Snapshot {
  Snapshot(
    guild:,
    owner: None,
    roles: [],
    channels: [],
    members: [],
    virtual: [],
    cutoff: cutoff.Missing,
    nsfw: None,
    nsfw_level: 0,
  )
}

pub fn set_owner(snapshot: Snapshot, owner: Option(UserId)) -> Snapshot {
  Snapshot(..snapshot, owner:)
}

pub fn set_roles(snapshot: Snapshot, roles: List(Role)) -> Snapshot {
  Snapshot(..snapshot, roles:)
}

pub fn add_channel(snapshot: Snapshot, channel: Channel) -> Snapshot {
  Snapshot(..snapshot, channels: [channel, ..snapshot.channels])
}

pub fn add_member(snapshot: Snapshot, source: Member) -> Snapshot {
  Snapshot(..snapshot, members: [
    event.member_upsert(source),
    ..snapshot.members
  ])
}

pub fn add_malformed_member(snapshot: Snapshot, user: UserId) -> Snapshot {
  Snapshot(..snapshot, members: [
    event.malformed_member(user),
    ..snapshot.members
  ])
}

pub fn add_virtual_access(
  snapshot: Snapshot,
  user: UserId,
  view: List(ChannelId),
) -> Snapshot {
  Snapshot(..snapshot, virtual: [
    event.virtual_access(user, view),
    ..snapshot.virtual
  ])
}

pub fn set_cutoff(snapshot: Snapshot, cutoff: Cutoff) -> Snapshot {
  Snapshot(..snapshot, cutoff:)
}

pub fn set_nsfw(
  snapshot: Snapshot,
  nsfw: Option(Bool),
  level: Int,
) -> Snapshot {
  Snapshot(..snapshot, nsfw:, nsfw_level: level)
}

@internal
pub fn to_native(snapshot: Snapshot) -> native.PermSnapshot {
  let channels = list.reverse(snapshot.channels)
  let events =
    list.flatten([
      [event.roles(snapshot.roles), event.owner(snapshot.owner)],
      list.map(channels, event.channel_upsert(_, event.Created)),
      [event.channel_order(list.map(channels, channel.id))],
      list.reverse(snapshot.members),
      list.reverse(snapshot.virtual),
      [
        event.history_cutoff(snapshot.cutoff),
        event.guild_nsfw(snapshot.nsfw, snapshot.nsfw_level),
      ],
    ])
  native.Snapshot(
    guild: id.guild_id_to_int(snapshot.guild),
    events: list.map(events, event.to_native),
  )
}
