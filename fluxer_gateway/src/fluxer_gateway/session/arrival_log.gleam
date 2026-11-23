// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/util/table
import gleam/int
import gleam/list
import logging

pub type Reason {
  NotAdopted
  UnlinkedAtSource
  NoMoveState
}

pub type Arrival {
  Arrival(
    session_id: String,
    source_node: String,
    guilds: Int,
    expected: Int,
    adopted: Int,
    reason: Reason,
  )
}

pub const window_ms = 10_000

pub const per_window = 20

fn warnings() -> table.Table {
  table.named("fluxer_gateway_arrival_warnings")
}

pub fn classify(
  guilds guilds: Int,
  expected expected: Int,
  adopted adopted: Int,
  moved moved: Bool,
) -> Result(Reason, Nil) {
  case guilds > adopted, moved, adopted < expected {
    False, _, _ -> Error(Nil)
    True, False, _ -> Ok(NoMoveState)
    True, True, True -> Ok(NotAdopted)
    True, True, False -> Ok(UnlinkedAtSource)
  }
}

pub fn warn(arrival: Arrival) -> Nil {
  case admit(clock.monotonic_ms()) {
    True -> logging.log(logging.Warning, line(arrival))
    False -> Nil
  }
}

pub fn line(arrival: Arrival) -> String {
  "session_arrival_rejoining: session_id="
  <> arrival.session_id
  <> " source_node="
  <> arrival.source_node
  <> " guilds="
  <> int.to_string(arrival.guilds)
  <> " expected="
  <> int.to_string(arrival.expected)
  <> " adopted="
  <> int.to_string(arrival.adopted)
  <> " unlinked_at_source="
  <> int.to_string(arrival.guilds - arrival.expected)
  <> " reason="
  <> reason_name(arrival.reason)
}

pub fn reason_name(reason: Reason) -> String {
  case reason {
    NotAdopted -> "not_adopted"
    UnlinkedAtSource -> "unlinked_at_source"
    NoMoveState -> "no_move_state"
  }
}

pub fn admit(now_ms: Int) -> Bool {
  let window = now_ms / window_ms
  case table.ensure_public(warnings()) {
    False -> False
    True ->
      case table.update_counter(warnings(), window, 1, #(window, 0)) {
        Ok(1) -> {
          forget_before(window)
          True
        }
        Ok(count) -> count <= per_window
        Error(Nil) -> False
      }
  }
}

fn forget_before(window: Int) -> Nil {
  let rows: List(#(Int, Int)) = table.to_list(warnings())
  list.each(rows, fn(row) {
    case row.0 < window {
      True -> table.delete(warnings(), row.0)
      False -> Nil
    }
  })
}
