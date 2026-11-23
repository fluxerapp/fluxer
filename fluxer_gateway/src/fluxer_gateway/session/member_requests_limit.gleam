// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/gateway/limits
import fluxer_gateway/util/table
import gleam/erlang/atom.{type Atom}
import gleam/list

fn window_ms() -> Int {
  durations.session().member_requests_limit_window_ms
}

const max_events = 12

fn cleanup_interval_ms() -> Int {
  durations.session().member_requests_limit_cleanup_interval_ms
}

const cleanup_key = "cleanup"

fn rows() -> table.Table {
  table.named("fluxer_gateway_request_members_rate_limit")
}

pub fn admit(user: UserId) -> Bool {
  case limits.rate_limits_disabled() {
    True -> True
    False -> {
      let found = rows()
      let _ = table.ensure_public(found)
      let now = clock.system_ms()
      maybe_cleanup(found, now)
      let key = id.user_id_to_int(user)
      let recent = case table.lookup(found, key) {
        [#(_, _, stamps), ..] ->
          list.filter(stamps, fn(stamp) { now - stamp < window_ms() })
        [] -> []
      }
      case list.length(recent) >= max_events {
        True -> False
        False -> {
          table.insert(found, #(key, now, [now, ..recent]))
          True
        }
      }
    }
  }
}

fn maybe_cleanup(found: table.Table, now: Int) -> Nil {
  let due = case table.lookup(found, cleanup_key) {
    [#(_, last, _), ..] -> now - last >= cleanup_interval_ms()
    [] -> True
  }
  case due {
    False -> Nil
    True -> {
      table.insert(found, #(cleanup_key, now, []))
      let _ = table.select_delete(found, stale_spec(now - window_ms()))
      Nil
    }
  }
}

fn stale_spec(
  cutoff: Int,
) -> List(#(#(Atom, Atom, Atom), List(#(Atom, Atom, Int)), List(Bool))) {
  let any = atom.create("_")
  let newest = atom.create("$1")
  [#(#(any, newest, any), [#(atom.create("<"), newest, cutoff)], [True])]
}
