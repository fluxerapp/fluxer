// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/id.{
  type ChannelId, type GuildId, type RoleId, type UserId,
}
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/permissions
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/role.{type Role}
import fluxer_gateway/core/user
import gleam/int
import gleam/option.{type Option, None}

pub fn guild(value: Int) -> GuildId {
  let assert Ok(found) = id.guild_id(value)
  found
}

pub fn user_id(value: Int) -> UserId {
  let assert Ok(found) = id.user_id(value)
  found
}

pub fn channel_id(value: Int) -> ChannelId {
  let assert Ok(found) = id.channel_id(value)
  found
}

pub fn role_id(value: Int) -> RoleId {
  let assert Ok(found) = id.role_id(value)
  found
}

pub fn make_role(value: Int, bits: Int) -> Role {
  let assert Ok(granted) = permissions.from_int(bits)
  role.new(
    id: role_id(value),
    name: "role",
    permissions: granted,
    position: role.Position(0),
    hoist: role.NotHoisted,
    raw: raw_json.wrap(<<"{}">>),
  )
}

pub fn make_channel(
  value: Int,
  kind: channel.ChannelKind,
  parent: Option(Int),
  overwrites: List(channel.Overwrite),
) -> Channel {
  channel.new(
    id: channel_id(value),
    kind:,
    parent: option.map(parent, channel_id),
    position: 0,
    overwrites:,
    raw: raw_json.wrap(<<
      "{\"id\":\"":utf8,
      int.to_string(value):utf8,
      "\"}":utf8,
    >>),
  )
}

pub fn make_overwrite(
  target: channel.OverwriteTarget,
  allow: Int,
  deny: Int,
) -> channel.Overwrite {
  let assert Ok(allow) = permissions.from_int(allow)
  let assert Ok(deny) = permissions.from_int(deny)
  channel.overwrite(target:, allow:, deny:)
}

pub fn make_member(
  value: Int,
  nick: Option(String),
  user_json: String,
) -> Member {
  member.new(
    user: user.new(
      id: user_id(value),
      bot: False,
      flags: 0,
      restricted: False,
      wire: raw_json.wrap(<<user_json:utf8>>),
    ),
    nick:,
    roles: [role_id(7), role_id(3)],
    joined_at_ms: None,
    timed_out_until_ms: None,
    wire: raw_json.wrap(<<"{\"nick\":null}">>),
  )
}
