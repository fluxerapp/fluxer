// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/util/table
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/list
import gleam/result

pub type Reading(key) {
  Reading(key: key, sent: Int, received: Int, queued: Int)
}

pub type Traffic {
  Traffic(
    sent: Int,
    received: Int,
    queued: Int,
    visible_nodes: Int,
    hidden_nodes: Int,
  )
}

type Key {
  Link(port: Dynamic)
  Accumulated
}

type SystemItem {
  DistCtrl
}

type Stat {
  RecvOct
  SendOct
}

type PortItem {
  QueueSize
}

type NodeKind {
  Visible
  Hidden
}

@external(erlang, "erlang", "system_info")
fn dist_ctrl(item: SystemItem) -> List(#(Atom, Dynamic))

@external(erlang, "erlang", "is_port")
fn is_port(value: Dynamic) -> Bool

@external(erlang, "inet", "getstat")
fn getstat(port: Dynamic, stats: List(Stat)) -> Dynamic

@external(erlang, "erlang", "port_info")
fn port_info(port: Dynamic, item: PortItem) -> Dynamic

@external(erlang, "erlang", "nodes")
fn nodes(kind: NodeKind) -> List(Atom)

fn readings_table() -> table.Table {
  table.named("fluxer_gateway_dist_readings")
}

pub fn advance(
  last: List(Reading(key)),
  current: List(Reading(key)),
) -> #(Int, Int) {
  list.fold(current, #(0, 0), fn(sum, now) {
    let before =
      list.find(last, fn(found) { found.key == now.key })
      |> result.unwrap(Reading(now.key, 0, 0, 0))
    #(
      sum.0 + int.max(0, now.sent - before.sent),
      sum.1 + int.max(0, now.received - before.received),
    )
  })
}

pub fn traffic() -> Traffic {
  let current = readings()
  let queued = list.fold(current, 0, fn(sum, found) { sum + found.queued })
  let #(sent, received) = case table.ensure_public(readings_table()) {
    False -> #(0, 0)
    True -> accumulate(current)
  }
  Traffic(
    sent:,
    received:,
    queued:,
    visible_nodes: list.length(nodes(Visible)),
    hidden_nodes: list.length(nodes(Hidden)),
  )
}

fn accumulate(current: List(Reading(Key))) -> #(Int, Int) {
  let rows: List(#(Key, Int, Int)) = table.to_list(readings_table())
  let #(total_sent, total_received) =
    list.find(rows, fn(row) { row.0 == Accumulated })
    |> result.map(fn(row) { #(row.1, row.2) })
    |> result.unwrap(#(0, 0))
  let last =
    list.filter_map(rows, fn(row) {
      case row.0 {
        Link(_) -> Ok(Reading(row.0, row.1, row.2, 0))
        Accumulated -> Error(Nil)
      }
    })
  let #(sent, received) = advance(last, current)
  list.each(last, fn(gone) {
    case list.any(current, fn(found) { found.key == gone.key }) {
      True -> Nil
      False -> table.delete(readings_table(), gone.key)
    }
  })
  list.each(current, fn(found) {
    table.insert(readings_table(), #(found.key, found.sent, found.received))
  })
  let totals = #(total_sent + sent, total_received + received)
  table.insert(readings_table(), #(Accumulated, totals.0, totals.1))
  totals
}

fn readings() -> List(Reading(Key)) {
  dist_ctrl(DistCtrl)
  |> list.filter_map(fn(entry) {
    let #(_, controller) = entry
    case is_port(controller) {
      False -> Error(Nil)
      True -> {
        use #(received, sent) <- result.try(octets(controller))
        Ok(Reading(Link(controller), sent, received, queue_size(controller)))
      }
    }
  })
}

fn octets(port: Dynamic) -> Result(#(Int, Int), Nil) {
  let pair = {
    use key <- decode.field(0, atom.decoder())
    use value <- decode.field(1, decode.int)
    decode.success(#(atom.to_string(key), value))
  }
  use found <- result.try(
    decode.run(
      getstat(port, [RecvOct, SendOct]),
      decode.at([1], decode.list(pair)),
    )
    |> result.replace_error(Nil),
  )
  use received <- result.try(list.key_find(found, "recv_oct"))
  use sent <- result.try(list.key_find(found, "send_oct"))
  Ok(#(received, sent))
}

fn queue_size(port: Dynamic) -> Int {
  decode.run(port_info(port, QueueSize), decode.at([1], decode.int))
  |> result.unwrap(0)
}
