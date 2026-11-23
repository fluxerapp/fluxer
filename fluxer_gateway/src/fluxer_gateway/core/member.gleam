// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type RoleId, type UserId}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/user.{type User} as core_user
import gleam/option.{type Option, None, Some}

pub opaque type Member {
  Member(
    user: User,
    nick: Option(String),
    roles: List(RoleId),
    joined_at_ms: Option(Int),
    timed_out_until_ms: Option(Int),
    wire: RawJson,
  )
}

pub fn new(
  user user: User,
  nick nick: Option(String),
  roles roles: List(RoleId),
  joined_at_ms joined_at_ms: Option(Int),
  timed_out_until_ms timed_out_until_ms: Option(Int),
  wire wire: RawJson,
) -> Member {
  Member(
    user:,
    nick: option.map(nick, raw_json.detach_string),
    roles:,
    joined_at_ms:,
    timed_out_until_ms:,
    wire:,
  )
}

pub fn user(member: Member) -> User {
  member.user
}

pub fn user_id(member: Member) -> UserId {
  core_user.id(member.user)
}

pub fn nick(member: Member) -> Option(String) {
  member.nick
}

pub fn roles(member: Member) -> List(RoleId) {
  member.roles
}

pub fn joined_at_ms(member: Member) -> Option(Int) {
  member.joined_at_ms
}

pub fn timed_out_until_ms(member: Member) -> Option(Int) {
  member.timed_out_until_ms
}

pub fn is_timed_out(member: Member, now_ms now_ms: Int) -> Bool {
  case member.timed_out_until_ms {
    Some(until) -> until > now_ms
    None -> False
  }
}

pub fn wire(member: Member) -> RawJson {
  member.wire
}
