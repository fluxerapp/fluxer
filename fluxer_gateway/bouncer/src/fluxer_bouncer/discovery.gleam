// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_bouncer/config.{type Config}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import gleam/result

type RecordType {
  A
  Aaaa
}

type NodeKind {
  Visible
  Hidden
}

const resolve_timeout_ms = 500

@external(erlang, "inet_res", "getbyname")
fn getbyname(
  name: Charlist,
  kind: RecordType,
  timeout: Int,
) -> Result(Dynamic, Dynamic)

@external(erlang, "erlang", "nodes")
fn nodes(kinds: List(NodeKind)) -> List(Atom)

@external(erlang, "erlang", "is_alive")
fn is_alive() -> Bool

@external(erlang, "net_kernel", "connect_node")
fn connect_node(node: Atom) -> Dynamic

pub fn host(config: Config, ordinal: Int) -> String {
  config.basename <> "-" <> int.to_string(ordinal) <> "." <> config.dns_name
}

pub fn peers(config: Config) -> List(#(Int, Atom)) {
  int.range(from: 0, to: config.count, with: [], run: fn(acc, ordinal) {
    case ordinal == config.ordinal {
      True -> acc
      False -> [#(ordinal, config.peer_node(config, ordinal)), ..acc]
    }
  })
}

pub fn resolves(name: String) -> Bool {
  let name = charlist.from_string(name)
  result.is_ok(getbyname(name, A, resolve_timeout_ms))
  || result.is_ok(getbyname(name, Aaaa, resolve_timeout_ms))
}

pub fn start(config: Config) -> Result(Pid, Nil) {
  case is_alive() {
    False -> Error(Nil)
    True -> Ok(process.spawn(fn() { loop(config, peers(config)) }))
  }
}

fn loop(config: Config, peers: List(#(Int, Atom))) -> Nil {
  let connected = nodes([Visible, Hidden])
  list.each(peers, fn(peer) {
    let #(ordinal, node) = peer
    case list.contains(connected, node) || !resolves(host(config, ordinal)) {
      True -> Nil
      False -> {
        let _ = connect_node(node)
        Nil
      }
    }
  })
  process.sleep(config.timings.beat_interval_ms)
  loop(config, peers)
}
