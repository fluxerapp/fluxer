// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session.{type SessionInbox}
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option}

pub const version = 1

pub type GuildBulkV1 {
  GuildBulkV1(
    guild_id: Int,
    event_index: Int,
    guild: BitArray,
    roles: BitArray,
    channels: BitArray,
    emojis: BitArray,
    stickers: BitArray,
    members: List(BitArray),
  )
}

pub type SessionV1 {
  SessionV1(session_id: String, user_id: Int, inbox: Subject(SessionInbox))
}

pub type EventV1 {
  EventV1(name: String, payload: BitArray)
}

pub type GuildTailV1 {
  GuildTailV1(
    guild_id: Int,
    event_index: Int,
    loaded: Option(GuildBulkV1),
    sessions: List(SessionV1),
    threads: BitArray,
    voice: BitArray,
    delta: List(EventV1),
    table: BitArray,
  )
}

pub type InboxV1 {
  InboxV1(message: BitArray)
}
