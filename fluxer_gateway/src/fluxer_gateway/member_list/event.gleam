// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/presence.{type Status}
import fluxer_gateway/core/session.{type SessionId}
import fluxer_gateway/member_list/item
import fluxer_gateway/member_list/native
import gleam/bit_array

pub opaque type Event {
  Event(native.MlEvent)
}

pub type Change {
  Changed
  Unchanged
}

pub type Timing {
  Immediate
  Deferred
}

pub fn member_upsert(source: Member, change: Change) -> Event {
  Event(native.MemberUpsert(
    id.user_id_to_int(member.user_id(source)),
    item.display_name(source),
    native_change(change),
  ))
}

pub fn member_remove(user: UserId) -> Event {
  Event(native.MemberRemove(id.user_id_to_int(user)))
}

pub fn presence(
  user: UserId,
  status: Status,
  change: Change,
  timing: Timing,
) -> Event {
  let timing = case timing {
    Immediate -> native.Immediate
    Deferred -> native.Deferred
  }
  Event(native.Presence(
    id.user_id_to_int(user),
    status,
    native_change(change),
    timing,
  ))
}

pub fn connected(user: UserId, presence_change: Change) -> Event {
  Event(native.Connection(
    id.user_id_to_int(user),
    native.Connected(native_change(presence_change)),
  ))
}

pub fn disconnected(user: UserId) -> Event {
  Event(native.Connection(id.user_id_to_int(user), native.Disconnected))
}

pub fn view_gained(session: SessionId, channel: ChannelId) -> Event {
  Event(native.ViewGained(session_key(session), id.channel_id_to_int(channel)))
}

pub fn flush() -> Event {
  Event(native.Flush)
}

pub fn updates(on: Bool) -> Event {
  case on {
    True -> Event(native.SetUpdates(native.UpdatesOn))
    False -> Event(native.SetUpdates(native.UpdatesOff))
  }
}

@internal
pub fn to_native(event: Event) -> native.MlEvent {
  let Event(found) = event
  found
}

@internal
pub fn session_key(source: SessionId) -> BitArray {
  bit_array.from_string(session.session_id_to_string(source))
}

fn native_change(change: Change) -> native.Change {
  case change {
    Changed -> native.Changed
    Unchanged -> native.Unchanged
  }
}
