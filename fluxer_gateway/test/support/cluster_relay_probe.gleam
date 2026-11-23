// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/relay
import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionInbox, type SessionRef}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list

pub type Collector {
  Collector(inbox: Subject(SessionInbox), query: Subject(Subject(List(String))))
}

pub fn start_collector() -> Collector {
  let handshake = process.new_subject()
  process.spawn_unlinked(fn() {
    let inbox = process.new_subject()
    let query = process.new_subject()
    process.send(handshake, Collector(inbox:, query:))
    collect(inbox, query, [])
  })
  let assert Ok(collector) = process.receive(handshake, 5000)
  collector
}

fn collect(
  inbox: Subject(SessionInbox),
  query: Subject(Subject(List(String))),
  seen: List(String),
) -> Nil {
  let selector =
    process.new_selector()
    |> process.select_map(inbox, Ok)
    |> process.select_map(query, Error)
  case process.selector_receive_forever(selector) {
    Ok(session.Dispatch(event:, ..)) -> collect(inbox, query, [event, ..seen])
    Ok(_) -> collect(inbox, query, seen)
    Error(reply) -> {
      process.send(reply, list.reverse(seen))
      collect(inbox, query, seen)
    }
  }
}

pub fn collected(collector: Collector) -> List(String) {
  process.call(collector.query, 5000, fn(reply) { reply })
}

pub fn session_ref(collector: Collector, index: Int) -> SessionRef {
  let assert Ok(user) = id.user_id(1000 + index)
  session.SessionRef(
    id: session.new_session_id(),
    user:,
    inbox: collector.inbox,
  )
}

pub fn events(count: Int) -> List(String) {
  int.range(from: count, to: 0, with: [], run: fn(acc, index) {
    ["E" <> int.to_string(index), ..acc]
  })
}

pub fn send_events(targets: List(Collector), count: Int) -> Nil {
  let refs =
    list.index_map(targets, fn(target, index) { session_ref(target, index) })
  list.each(events(count), fn(event) {
    relay.send(refs, session.Dispatch(event:, payload: raw_json.null()))
  })
}
