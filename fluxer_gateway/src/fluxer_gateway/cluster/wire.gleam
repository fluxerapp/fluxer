// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/wire_epoch
import bouncer_proto/worker.{type Entry, Ready}
import fluxer_gateway/cluster/links
import fluxer_gateway/config/build_info
import fluxer_gateway/config/settings
import fluxer_gateway/core/node.{type Node}
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/rollout_metrics
import fluxer_gateway/util/table
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import gleam/option
import gleam/order
import gleam/string
import logging

pub type Bridge {
  Bridge(from: Int, types: List(#(String, String)))
}

pub const bridges: List(Bridge) = [
  Bridge(
    from: 4,
    types: [
      #("src/fluxer_gateway/core/guild.gleam", "GuildRequest"),
      #("src/fluxer_gateway/core/session.gleam", "SessionInbox"),
      #("src/fluxer_gateway/session/control.gleam", "Control"),
      #("src/fluxer_gateway/presence/protocol.gleam", "Input"),
      #("src/fluxer_gateway/voice/call/manager.gleam", "Message"),
      #("src/fluxer_gateway/voice/call/server.gleam", "Message"),
      #("src/fluxer_gateway/voice/call/server.gleam", "MoveSignal"),
      #("src/fluxer_gateway/voice/call/state.gleam", "Member"),
      #("src/fluxer_gateway/voice/call/state.gleam", "MemberNote"),
      #("src/fluxer_gateway/push/server.gleam", "Message"),
      #("src/fluxer_gateway/push/outbox.gleam", "Message"),
      #("src/fluxer_gateway/push/outbox/state.gleam", "Carried"),
      #("src/fluxer_gateway/handoff/precopy.gleam", "Envelope"),
      #("src/fluxer_gateway/handoff/precopy.gleam", "Ack"),
    ],
  ),
]

pub const move_forms_epoch = 5

pub type Assessment {
  Pending
  Accepted
  Mismatch(local: Int, peer: Int)
}

pub const refused_metric = "fluxer_gateway_wire_epoch_refused_peers_total"

@external(erlang, "erlang", "node")
fn pid_node(pid: Pid) -> Atom

fn reported_table() -> table.Table {
  table.named("fluxer_gateway_wire_refused")
}

pub fn floor() -> Int {
  wire_epoch.floor(build_info.wire_epoch)
}

pub fn epoch_of(peer: Node) -> Int {
  case peer == node.self(), links.entry(peer) {
    True, _ -> build_info.wire_epoch
    False, Ok(found) -> found.profile.wire_epoch
    False, Error(Nil) ->
      case option.is_some(settings.get().bouncer) {
        True -> floor()
        False -> build_info.wire_epoch
      }
  }
}

pub fn speaks(peer: Node, since: Int) -> Bool {
  epoch_of(peer) >= since
}

pub fn pid_speaks(pid: Pid, since: Int) -> Bool {
  speaks(node.from_atom(pid_node(pid)), since)
}

pub fn own(entries: List(Entry)) -> Result(Entry, Nil) {
  let self = node.to_atom(node.self())
  list.find(entries, fn(found) { found.worker.node == self })
}

pub fn refused(mine: Entry, peers: List(Entry)) -> List(Entry) {
  peers
  |> list.filter(fn(found) {
    found.status == Ready
    && found.worker.node != mine.worker.node
    && !wire_epoch.compatible(found.profile.wire_epoch, mine.profile.wire_epoch)
  })
  |> list.sort(fn(a, b) {
    let gap = fn(found: Entry) {
      int.absolute_value(found.profile.wire_epoch - mine.profile.wire_epoch)
    }
    case int.compare(gap(b), gap(a)) {
      order.Eq ->
        string.compare(
          atom.to_string(a.worker.node),
          atom.to_string(b.worker.node),
        )
      other -> other
    }
  })
}

pub fn assess(mine: Entry, peers: List(Entry)) -> Assessment {
  case refused(mine, peers) {
    [] -> Accepted
    [first, ..] -> Mismatch(mine.profile.wire_epoch, first.profile.wire_epoch)
  }
}

pub fn current() -> Assessment {
  let entries = links.all()
  case own(entries) {
    Error(Nil) -> Pending
    Ok(mine) -> assess(mine, entries)
  }
}

pub fn observe(entries: List(Entry)) -> Int {
  case own(entries), table.ensure_public(reported_table()) {
    Ok(mine), True ->
      refused(mine, entries)
      |> list.filter(fn(found) {
        table.insert_new(reported_table(), #(
          #(found.worker.node, found.worker.incarnation),
          found.profile.wire_epoch,
        ))
      })
      |> list.map(fn(found) { report(mine, found) })
      |> list.length
    _, _ -> 0
  }
}

fn report(mine: Entry, peer: Entry) -> Nil {
  let local = int.to_string(mine.profile.wire_epoch)
  let theirs = int.to_string(peer.profile.wire_epoch)
  metrics.counter(
    refused_metric,
    [#("local_epoch", local), #("peer_epoch", theirs)],
    1,
  )
  rollout_metrics.count(
    rollout_metrics.wire_refusals,
    [
      #("reason", case peer.profile.wire_epoch < mine.profile.wire_epoch {
        True -> "older"
        False -> "newer"
      }),
    ],
    1,
  )
  logging.log(
    logging.Error,
    "EPOCH_MISMATCH: this node speaks wire_epoch "
      <> local
      <> " and peer "
      <> atom.to_string(peer.worker.node)
      <> " speaks "
      <> theirs
      <> ", more than one epoch apart or across a breaking bump, so the peer is refused and this rollout cannot proceed",
  )
}

pub fn refused_now() -> List(Entry) {
  let entries = links.all()
  case own(entries) {
    Ok(mine) -> refused(mine, entries)
    Error(Nil) -> []
  }
}

pub fn bridged(mine: Entry, peers: List(Entry)) -> List(Entry) {
  list.filter(peers, fn(found) {
    found.status == Ready
    && found.worker.node != mine.worker.node
    && found.profile.wire_epoch != mine.profile.wire_epoch
    && wire_epoch.compatible(found.profile.wire_epoch, mine.profile.wire_epoch)
  })
}

pub fn bridged_now() -> List(Entry) {
  let entries = links.all()
  case own(entries) {
    Ok(mine) -> bridged(mine, entries)
    Error(Nil) -> []
  }
}
