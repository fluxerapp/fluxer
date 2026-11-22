// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{
  type Channel, type ChannelKind, type Overwrite,
}
import fluxer_gateway/core/id
import fluxer_gateway/core/permissions
import fluxer_gateway/core/role.{type Role}
import fluxer_gateway/perms/native
import gleam/list
import gleam/option

pub fn of_role(source: Role) -> native.PermRole {
  native.Role(
    id: id.role_id_to_int(role.id(source)),
    permissions: permissions.to_int(role.permissions(source)),
    position: role.position(source),
    hoist: role.hoist(source),
  )
}

pub fn of_roles(sources: List(Role)) -> List(native.PermRole) {
  list.map(sources, of_role)
}

pub fn of_channel(source: Channel) -> native.PermChannel {
  native.Channel(
    id: id.channel_id_to_int(channel.id(source)),
    kind: of_kind(channel.kind(source)),
    parent: option.map(channel.parent(source), id.channel_id_to_int),
    overwrites: list.map(channel.overwrites(source), overwrite),
    nsfw: channel.nsfw(source),
    named: channel.is_named(source),
  )
}

pub fn of_kind(source: ChannelKind) -> native.ChannelKind {
  case source {
    channel.Text -> native.Text
    channel.Voice -> native.Voice
    channel.Category -> native.Category
    channel.Announcement -> native.Announcement
    channel.Forum -> native.Forum
    channel.Media -> native.Media
    channel.AnnouncementThread | channel.PublicThread | channel.PrivateThread ->
      native.ThreadKind(channel.kind_to_int(source))
    channel.Dm | channel.GroupDm | channel.OtherKind(_) ->
      native.OtherKind(channel.kind_to_int(source))
  }
}

fn overwrite(source: Overwrite) -> native.PermOverwrite {
  let target = case channel.overwrite_target(source) {
    channel.RoleTarget(target) -> native.RoleTarget(id.role_id_to_int(target))
    channel.MemberTarget(target) ->
      native.MemberTarget(id.user_id_to_int(target))
  }
  native.Overwrite(
    target:,
    allow: permissions.to_int(channel.overwrite_allow(source)),
    deny: permissions.to_int(channel.overwrite_deny(source)),
  )
}
