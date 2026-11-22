// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/packed_ids.{type PackedIds}
import fluxer_gateway/core/session.{type SessionId, type SessionRef}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}

pub type Snapshot {
  Snapshot(
    engaged: PackedIds,
    connected: PackedIds,
    held: Dict(Int, List(SessionRef)),
  )
}

pub opaque type Engagement {
  Engagement(
    holds: Dict(String, Bool),
    active: Set(Int),
    version: Int,
    cached: Option(Cached),
  )
}

type Cached {
  Cached(version: Int, sessions: List(SessionRef), snapshot: Snapshot)
}

pub fn new() -> Engagement {
  Engagement(holds: dict.new(), active: set.new(), version: 0, cached: None)
}

pub fn set_active(
  engagement: Engagement,
  user: Int,
  active: Bool,
) -> Engagement {
  let current = set.contains(engagement.active, user)
  case current == active {
    True -> engagement
    False ->
      Engagement(
        ..engagement,
        active: case active {
          True -> set.insert(engagement.active, user)
          False -> set.delete(engagement.active, user)
        },
        version: engagement.version + 1,
      )
  }
}

pub fn set_hold(
  engagement: Engagement,
  holder: SessionId,
  hold: Bool,
) -> Engagement {
  let key = session.session_id_to_string(holder)
  case dict.get(engagement.holds, key) {
    Ok(existing) if existing == hold -> engagement
    _ ->
      Engagement(
        ..engagement,
        holds: dict.insert(engagement.holds, key, hold),
        version: engagement.version + 1,
      )
  }
}

pub fn released_holds(
  engagement: Engagement,
  sessions: List(SessionId),
) -> List(SessionId) {
  list.filter(sessions, fn(item) {
    dict.get(engagement.holds, session.session_id_to_string(item)) == Ok(False)
  })
}

fn session_engaged(engagement: Engagement, ref: SessionRef) -> Bool {
  case dict.get(engagement.holds, session.session_id_to_string(ref.id)) {
    Ok(hold) -> hold
    Error(Nil) -> set.contains(engagement.active, id.user_id_to_int(ref.user))
  }
}

pub fn snapshot(
  engagement: Engagement,
  sessions: List(SessionRef),
) -> #(Engagement, Snapshot) {
  case engagement.cached {
    Some(Cached(version:, sessions: cached_sessions, snapshot: built))
      if version == engagement.version && cached_sessions == sessions
    -> #(engagement, built)
    _ -> {
      let built = build(engagement, sessions)
      let present =
        list.fold(sessions, set.new(), fn(acc, ref) {
          set.insert(acc, session.session_id_to_string(ref.id))
        })
      let holds =
        dict.filter(engagement.holds, fn(key, _) { set.contains(present, key) })
      #(
        Engagement(
          ..engagement,
          holds:,
          cached: Some(Cached(engagement.version, sessions, built)),
        ),
        built,
      )
    }
  }
}

fn build(engagement: Engagement, sessions: List(SessionRef)) -> Snapshot {
  let #(held, connected) =
    list.fold(sessions, #(dict.new(), set.new()), fn(acc, ref) {
      let #(held, connected) = acc
      let user = id.user_id_to_int(ref.user)
      let connected = set.insert(connected, user)
      case session_engaged(engagement, ref) {
        True -> #(
          dict.upsert(held, user, fn(existing) {
            [ref, ..option.unwrap(existing, [])]
          }),
          connected,
        )
        False -> #(held, connected)
      }
    })
  Snapshot(
    engaged: packed(dict.keys(held)),
    connected: packed(set.to_list(connected)),
    held:,
  )
}

fn packed(users: List(Int)) -> PackedIds {
  users |> list.sort(int.compare) |> packed_ids.from_list
}

pub fn is_engaged(snapshot: Snapshot, user: Int) -> Bool {
  packed_ids.contains_sorted(snapshot.engaged, user)
}

pub fn is_connected(snapshot: Snapshot, user: Int) -> Bool {
  packed_ids.contains_sorted(snapshot.connected, user)
}

pub fn connected_set(snapshot: Snapshot) -> Set(Int) {
  set.from_list(packed_ids.to_list(snapshot.connected))
}
