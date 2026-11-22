// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session.{type SessionId}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/set.{type Set}

const max_events = 1000

pub opaque type CatchUp {
  Idle
  Waiting(
    sessions: Set(SessionId),
    events: List(#(String, Dynamic)),
    count: Int,
    overflowed: Bool,
    until_ms: Int,
  )
}

pub fn none() -> CatchUp {
  Idle
}

pub fn new(sessions: List(SessionId), now_ms: Int, window_ms: Int) -> CatchUp {
  case sessions {
    [] -> Idle
    _ ->
      Waiting(
        sessions: set.from_list(sessions),
        events: [],
        count: 0,
        overflowed: False,
        until_ms: now_ms + window_ms,
      )
  }
}

pub fn is_waiting(catch_up: CatchUp) -> Bool {
  case catch_up {
    Idle -> False
    Waiting(..) -> True
  }
}

pub fn waiting(catch_up: CatchUp) -> List(SessionId) {
  case catch_up {
    Idle -> []
    Waiting(sessions:, ..) -> set.to_list(sessions)
  }
}

pub fn record(
  catch_up: CatchUp,
  event: String,
  payload: Dynamic,
  now_ms: Int,
) -> CatchUp {
  case catch_up {
    Idle -> Idle
    Waiting(until_ms:, ..) if now_ms > until_ms -> Idle
    Waiting(events:, count:, ..) if count >= 2 * max_events ->
      Waiting(
        ..catch_up,
        events: [#(event, payload), ..list.take(events, max_events - 1)],
        count: max_events,
        overflowed: True,
      )
    Waiting(events:, count:, overflowed:, ..) ->
      Waiting(
        ..catch_up,
        events: [#(event, payload), ..events],
        count: count + 1,
        overflowed: overflowed || count >= max_events,
      )
  }
}

pub type Taken {
  Taken(catch_up: CatchUp, events: List(#(String, Dynamic)), complete: Bool)
}

pub fn take(catch_up: CatchUp, session: SessionId, now_ms: Int) -> Taken {
  case catch_up {
    Idle -> Taken(Idle, [], True)
    Waiting(until_ms:, ..) if now_ms > until_ms -> Taken(Idle, [], True)
    Waiting(sessions:, events:, overflowed:, ..) ->
      case set.contains(sessions, session) {
        False -> Taken(catch_up, [], True)
        True -> {
          let left = set.delete(sessions, session)
          let next = case set.is_empty(left) {
            True -> Idle
            False -> Waiting(..catch_up, sessions: left)
          }
          Taken(next, list.reverse(list.take(events, max_events)), !overflowed)
        }
      }
  }
}

pub fn expire(catch_up: CatchUp, now_ms: Int) -> CatchUp {
  case catch_up {
    Waiting(until_ms:, ..) if now_ms > until_ms -> Idle
    _ -> catch_up
  }
}
