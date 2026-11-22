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
import fluxer_gateway/nif
import fluxer_gateway/perms
import fluxer_gateway/perms/snapshot.{type Snapshot}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None}

pub const view = 1024

pub const view_members = 18_014_398_509_481_984

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

pub fn role(value: Int, bits: Int, position: Int) -> Role {
  let assert Ok(granted) = permissions.from_int(bits)
  role.new(
    id: role_id(value),
    name: "r" <> int.to_string(value),
    permissions: granted,
    position: role.Position(position),
    hoist: role.NotHoisted,
    raw: raw_json.wrap(<<"{}">>),
  )
}

pub fn text(value: Int, overwrites: List(channel.Overwrite)) -> Channel {
  channel.new(
    id: channel_id(value),
    kind: channel.Text,
    parent: None,
    position: 0,
    overwrites:,
    raw: raw_json.wrap(<<
      "{\"id\":\"":utf8,
      int.to_string(value):utf8,
      "\",\"name\":\"c\"}":utf8,
    >>),
  )
}

pub fn deny_role(role: Int, bits: Int) -> channel.Overwrite {
  let assert Ok(deny) = permissions.from_int(bits)
  channel.overwrite(
    target: channel.RoleTarget(role_id(role)),
    allow: permissions.none(),
    deny:,
  )
}

pub fn allow_role(role: Int, bits: Int) -> channel.Overwrite {
  let assert Ok(allow) = permissions.from_int(bits)
  channel.overwrite(
    target: channel.RoleTarget(role_id(role)),
    allow:,
    deny: permissions.none(),
  )
}

pub fn member(value: Int, roles: List(Int)) -> Member {
  member.new(
    user: user.new(
      id: user_id(value),
      bot: False,
      flags: 0,
      restricted: False,
      wire: raw_json.wrap(<<
        "{\"id\":\"":utf8,
        int.to_string(value):utf8,
        "\",\"username\":\"u":utf8,
        int.to_string(value):utf8,
        "\"}":utf8,
      >>),
    ),
    nick: None,
    roles: list.map(roles, role_id),
    joined_at_ms: None,
    timed_out_until_ms: None,
    wire: raw_json.wrap(<<
      "{\"user\":{\"id\":\"":utf8,
      int.to_string(value):utf8,
      "\",\"username\":\"u":utf8,
      int.to_string(value):utf8,
      "\"}}":utf8,
    >>),
  )
}

pub fn loaded(source: Snapshot) -> #(perms.Perms, perms.Reader) {
  let assert Ok(Nil) = nif.load()
  let #(handle, reader) = perms.new(guild(100), process.self())
  let assert Ok(_) = perms.load(handle, source)
  #(handle, reader)
}

pub fn base(roles, channels: List(Channel)) -> Snapshot {
  list.fold(
    channels,
    snapshot.new(guild(100)) |> snapshot.set_roles(roles),
    snapshot.add_channel,
  )
}

pub fn subset_roles(mask: Int, first_role: Int, count: Int) -> List(Int) {
  int.range(0, count, [], fn(acc, bit) {
    case int.bitwise_and(int.bitwise_shift_right(mask, bit), 1) {
      1 -> [first_role + bit, ..acc]
      _ -> acc
    }
  })
}

pub fn overwrite_member_deny(user: Int, bits: Int) -> channel.Overwrite {
  let assert Ok(deny) = permissions.from_int(bits)
  channel.overwrite(
    target: channel.MemberTarget(user_id(user)),
    allow: permissions.none(),
    deny:,
  )
}
