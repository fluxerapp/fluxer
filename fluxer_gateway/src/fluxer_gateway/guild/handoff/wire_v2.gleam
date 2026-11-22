// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/guild/handoff/wire_v1.{type EventV1, type SessionV1}
import gleam/option.{type Option}

pub const version = 2

pub type GuildBulkV2 {
  GuildBulkV2(
    guild_id: Int,
    event_index: Int,
    guild: BitArray,
    roles: BitArray,
    channels: BitArray,
    emojis: BitArray,
    stickers: BitArray,
    member_count: Int,
    members: BitArray,
  )
}

pub type GuildTailV2 {
  GuildTailV2(
    guild_id: Int,
    event_index: Int,
    loaded: Option(GuildBulkV2),
    sessions: List(SessionV1),
    threads: BitArray,
    voice: BitArray,
    delta: List(EventV1),
    table: BitArray,
  )
}
