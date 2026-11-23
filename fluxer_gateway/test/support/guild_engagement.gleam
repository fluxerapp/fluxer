// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/guild/push/engagement
import fluxer_gateway/guild/sessions as guild_sessions
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Pid}

@external(erlang, "sys", "get_state")
fn get_state(pid: Pid) -> Dynamic

@external(erlang, "gleam_stdlib", "identity")
fn as_guild_state(value: Dynamic) -> GuildState

pub fn engaged(guild: Pid, user: Int) -> Bool {
  let assert Ok(raw) =
    decode.run(get_state(guild), decode.at([1], decode.dynamic))
  let state = as_guild_state(raw)
  let #(_, snapshot) =
    engagement.snapshot(
      guild_state.push(state).engagement,
      guild_sessions.session_refs(guild_state.sessions(state)),
    )
  engagement.is_engaged(snapshot, user)
}
