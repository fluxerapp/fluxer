// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/presence.{
  type Status, Dnd, Idle, Invisible, Offline, Online,
}
import fluxer_gateway/presence/status as status_rules
import gleam/list

pub opaque type Display {
  Display(value: Int)
}

pub type SessionState {
  SessionState(status: Status, afk: Bool, mobile: Bool)
}

fn status_index(status: Status) -> Int {
  case status {
    Online -> 0
    Idle -> 1
    Dnd -> 2
    Invisible -> 3
    Offline -> 4
  }
}

fn bit(flag: Bool, weight: Int) -> Int {
  case flag {
    True -> weight
    False -> 0
  }
}

pub fn new(
  status status: Status,
  mobile mobile: Bool,
  afk afk: Bool,
) -> Display {
  Display(status_index(status) * 4 + bit(mobile, 2) + bit(afk, 1))
}

pub fn status(display: Display) -> Status {
  case display.value / 4 {
    0 -> Online
    1 -> Idle
    2 -> Dnd
    3 -> Invisible
    _ -> Offline
  }
}

pub fn mobile(display: Display) -> Bool {
  display.value % 4 >= 2
}

pub fn afk(display: Display) -> Bool {
  display.value % 2 == 1
}

pub fn to_int(display: Display) -> Int {
  display.value
}

pub fn from_int(value: Int) -> Result(Display, Nil) {
  case value >= 0 && value < 20 {
    True -> Ok(Display(value))
    False -> Error(Nil)
  }
}

pub fn offline() -> Display {
  new(status: Offline, mobile: False, afk: False)
}

pub fn is_visible(display: Display) -> Bool {
  status_rules.is_visible(status(display))
}

pub fn external(display: Display) -> Display {
  case is_visible(display) {
    True -> display
    False -> offline()
  }
}

pub fn effective(session: SessionState, invisible: Bool) -> SessionState {
  case invisible, session.status {
    True, Offline -> session
    True, _ -> SessionState(..session, status: Invisible)
    False, _ -> session
  }
}

pub opaque type Flat {
  Flat(best: Status, online_mobile: Bool, all_afk: Bool, seen: Bool)
}

pub fn flat() -> Flat {
  Flat(best: Offline, online_mobile: False, all_afk: True, seen: False)
}

pub fn add(flat: Flat, session: SessionState, invisible: Bool) -> Flat {
  let session = effective(session, invisible)
  Flat(
    best: status_rules.stronger(session.status, flat.best),
    online_mobile: flat.online_mobile
      || { session.status == Online && session.mobile },
    all_afk: flat.all_afk && session.afk,
    seen: True,
  )
}

pub fn finish(flat: Flat) -> Display {
  let is_mobile = flat.best == Online && flat.online_mobile
  let is_afk = !is_mobile && flat.seen && flat.all_afk
  new(status: flat.best, mobile: is_mobile, afk: is_afk)
}

pub fn flatten(
  sessions: List(SessionState),
  invisible invisible: Bool,
) -> Display {
  list.fold(sessions, flat(), fn(acc, session) { add(acc, session, invisible) })
  |> finish
}
