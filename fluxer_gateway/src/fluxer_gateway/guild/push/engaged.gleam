// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/session.{type SessionId}
import fluxer_gateway/guild/push/engagement
import fluxer_gateway/guild/push/state.{type GuildPush, GuildPush}
import gleam/list

pub type EngagementChange {
  EngagementChange(user: UserId, engaged: Bool)
}

pub fn on_change(
  push: GuildPush,
  changes: List(EngagementChange),
) -> GuildPush {
  let engagement =
    list.fold(changes, push.engagement, fn(acc, change) {
      engagement.set_active(acc, id.user_id_to_int(change.user), change.engaged)
    })
  GuildPush(..push, engagement:)
}

pub fn set_session_hold(
  push: GuildPush,
  session: SessionId,
  hold: Bool,
) -> GuildPush {
  GuildPush(
    ..push,
    engagement: engagement.set_hold(push.engagement, session, hold),
  )
}
