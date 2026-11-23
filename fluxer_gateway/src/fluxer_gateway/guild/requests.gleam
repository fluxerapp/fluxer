// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session.{type ClientOp, type SessionRef}
import fluxer_gateway/guild/requests/counts
import fluxer_gateway/guild/requests/forum_unreads
import fluxer_gateway/guild/requests/members
import fluxer_gateway/guild/state.{type GuildState}

pub fn handle(state: GuildState, ref: SessionRef, op: ClientOp) -> GuildState {
  case op {
    session.RequestGuildMembers(data) -> members.handle(state, ref, data)
    session.RequestChannelMemberCounts(data) ->
      counts.channel_counts(state, ref, data)
    session.RequestForumUnreads(data) -> forum_unreads.handle(state, ref, data)
    _ -> state
  }
}
