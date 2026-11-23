// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Monitor, type Pid}
import gleam/list
import gleam/option.{type Option, None, Some}

pub opaque type Hold(request, successor) {
  Hold(
    events: List(#(String, Dynamic)),
    voice: List(request),
    successor: successor,
    owners: List(Monitor),
    token: Int,
  )
}

pub type OwnerDown(request, successor) {
  NotOwner
  StillHeld(Hold(request, successor))
  Abandoned(Hold(request, successor))
}

pub fn freeze(
  previous: Option(Hold(request, successor)),
  owners: List(Pid),
  successor: successor,
  token: Int,
) -> Hold(request, successor) {
  let #(events, voice) = case previous {
    Some(held) -> {
      list.each(held.owners, process.demonitor_process)
      #(held.events, held.voice)
    }
    None -> #([], [])
  }
  Hold(
    events:,
    voice:,
    successor:,
    owners: list.map(list.unique(owners), process.monitor),
    token:,
  )
}

pub fn record_event(
  held: Hold(request, successor),
  event: String,
  payload: Dynamic,
) -> Hold(request, successor) {
  Hold(..held, events: [#(event, payload), ..held.events])
}

pub fn hold_voice(
  held: Hold(request, successor),
  request: request,
) -> Hold(request, successor) {
  Hold(..held, voice: [request, ..held.voice])
}

pub fn events(held: Hold(request, successor)) -> List(#(String, Dynamic)) {
  list.reverse(held.events)
}

pub fn successor(held: Hold(request, successor)) -> successor {
  held.successor
}

pub fn token(held: Hold(request, successor)) -> Int {
  held.token
}

pub fn release(held: Hold(request, successor)) -> List(request) {
  list.each(held.owners, process.demonitor_process)
  list.reverse(held.voice)
}

pub fn owner_down(
  held: Hold(request, successor),
  monitor: Monitor,
) -> OwnerDown(request, successor) {
  case list.contains(held.owners, monitor) {
    False -> NotOwner
    True ->
      case list.filter(held.owners, fn(owner) { owner != monitor }) {
        [] -> Abandoned(Hold(..held, owners: []))
        owners -> StillHeld(Hold(..held, owners:))
      }
  }
}
