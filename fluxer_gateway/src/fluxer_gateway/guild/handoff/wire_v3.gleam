// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/message.{type MoveId}
import fluxer_gateway/guild/handoff/wire_v1.{type EventV1, type SessionV1}
import fluxer_gateway/guild/handoff/wire_v2.{type GuildBulkV2}
import gleam/option.{type Option}

pub const version = 3

pub type GuildDeltaV3 {
  GuildDeltaV3(
    move: MoveId,
    round: Int,
    changed: GuildBulkV2,
    removed: List(Int),
  )
}

pub type GuildTailV3 {
  GuildTailV3(
    guild_id: Int,
    event_index: Int,
    loaded: Option(GuildBulkV2),
    staged: Option(GuildDeltaV3),
    sessions: List(SessionV1),
    threads: BitArray,
    voice: BitArray,
    delta: List(EventV1),
    table: BitArray,
  )
}
