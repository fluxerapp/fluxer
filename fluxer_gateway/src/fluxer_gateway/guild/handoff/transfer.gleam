// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/guild/data.{type Loaded}
import fluxer_gateway/threads/store.{type Handoff}
import fluxer_gateway/voice/guild_handler.{type GuildVoice}

pub fn moved_timeout_ms() -> Int {
  durations.guild().transfer_moved_timeout_ms
}

pub fn awaiting_replay_ms() -> Int {
  durations.guild().transfer_awaiting_replay_ms
}

pub fn freeze_deadline_ms() -> Int {
  durations.guild().transfer_freeze_deadline_ms
}

pub type Transfer {
  Transfer(
    loaded: Loaded,
    sessions: List(SessionRef),
    threads: Handoff,
    voice: GuildVoice,
  )
}
