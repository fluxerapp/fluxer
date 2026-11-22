// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/node
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list

fn remote_timeout_ms() -> Int {
  durations.util().liveness_remote_timeout_ms
}

@external(erlang, "erlang", "node")
fn pid_node(pid: Pid) -> Atom

@external(erlang, "erlang", "nodes")
fn connected_nodes() -> List(Atom)

@external(erlang, "rpc", "call")
fn rpc_call(
  node: Atom,
  module: Atom,
  function: Atom,
  args: List(Pid),
  timeout_ms: Int,
) -> Dynamic

pub fn is_local(pid: Pid) -> Bool {
  pid_node(pid) == node.to_atom(node.self())
}

pub fn is_alive(pid: Pid) -> Bool {
  case is_local(pid) {
    True -> process.is_alive(pid)
    False -> remote_is_alive(pid)
  }
}

fn remote_is_alive(pid: Pid) -> Bool {
  let target = pid_node(pid)
  case list.contains(connected_nodes(), target) {
    False -> False
    True -> {
      let reply =
        rpc_call(
          target,
          atom.create("erlang"),
          atom.create("is_process_alive"),
          [pid],
          remote_timeout_ms(),
        )
      decode.run(reply, decode.bool) == Ok(True)
    }
  }
}
