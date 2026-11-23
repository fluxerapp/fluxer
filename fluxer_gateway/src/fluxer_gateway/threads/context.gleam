// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/id.{type ChannelId, type GuildId, type UserId}
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/perms.{type Reader}
import fluxer_gateway/threads/message.{type ThreadMsg}
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option}

pub type Context {
  Context(
    guild: GuildId,
    reader: Reader,
    member: fn(UserId) -> Option(Member),
    channel: fn(ChannelId) -> Option(Channel),
    channels: List(Channel),
    passive: fn(String) -> Bool,
    small_guild: Bool,
    presence: fn(UserId) -> Option(RawJson),
    unavailable_for: fn(UserId) -> Bool,
    history_cutoff_ms: Option(Int),
    self: Subject(ThreadMsg),
  )
}
