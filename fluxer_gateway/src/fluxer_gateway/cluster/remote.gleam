// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/links
import fluxer_gateway/config/settings
import fluxer_gateway/core/node.{type Node}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Monitor, type Name, type Pid}
import gleam/list
import gleam/option
import gleam/result

pub type SendFailure {
  NoSuspend
  NoConnect
}

type SendOption {
  Noconnect
  Nosuspend
}

type Sent

@external(erlang, "erlang", "send")
fn send_with_options(
  destination: #(Name(message), Atom),
  message: #(Name(message), message),
  options: List(SendOption),
) -> Sent

@external(erlang, "gleam_stdlib", "identity")
fn refused(option: SendOption) -> Sent

@external(erlang, "erlang", "send")
fn raw_send(
  pid: Pid,
  message: #(Name(message), message),
) -> #(Name(message), message)

@external(erlang, "net_kernel", "connect_node")
fn net_connect(node: Atom) -> Atom

type MonitorKind {
  Process
}

@external(erlang, "erlang", "monitor")
fn monitor_registered(
  kind: MonitorKind,
  target: #(Name(message), Atom),
) -> Monitor

@external(erlang, "erlang", "nodes")
fn connected_nodes() -> List(Atom)

fn monitor_named(on: Node, name: Name(message)) -> Monitor {
  monitor_registered(Process, #(name, node.to_atom(on)))
}

pub fn ask_named(
  to: Node,
  name: Name(message),
  message: message,
) -> Result(Monitor, SendFailure) {
  case list.contains(connected_nodes(), node.to_atom(to)) {
    True -> {
      let monitor = monitor_named(to, name)
      case send_named(to, name, message) {
        Ok(Nil) -> Ok(monitor)
        Error(failure) -> {
          process.demonitor_process(monitor)
          Error(failure)
        }
      }
    }
    False ->
      send_named(to, name, message)
      |> result.map(fn(_) { monitor_named(to, name) })
  }
}

pub fn send_named(
  to: Node,
  name: Name(message),
  message: message,
) -> Result(Nil, SendFailure) {
  case send_named_once(to, name, message) {
    Error(NoConnect) -> linked(to, fn() { send_named_once(to, name, message) })
    outcome -> outcome
  }
}

fn linked(
  to: Node,
  again: fn() -> Result(Nil, SendFailure),
) -> Result(Nil, SendFailure) {
  case option.is_some(settings.get().bouncer) && links.ensure(to) {
    True -> again()
    False -> Error(NoConnect)
  }
}

fn send_named_once(
  to: Node,
  name: Name(message),
  message: message,
) -> Result(Nil, SendFailure) {
  case to == node.self() {
    True -> {
      let _ = send_local(name, message)
      Ok(Nil)
    }
    False -> {
      let outcome =
        send_with_options(#(name, node.to_atom(to)), #(name, message), [
          Noconnect,
          Nosuspend,
        ])
      case outcome == refused(Nosuspend), outcome == refused(Noconnect) {
        True, _ -> Error(NoSuspend)
        _, True -> Error(NoConnect)
        False, False -> Ok(Nil)
      }
    }
  }
}

pub fn deliver_named(
  to: Node,
  name: Name(message),
  message: message,
) -> Result(Nil, SendFailure) {
  case deliver_named_once(to, name, message) {
    Error(NoConnect) ->
      linked(to, fn() { deliver_named_once(to, name, message) })
    outcome -> outcome
  }
}

fn deliver_named_once(
  to: Node,
  name: Name(message),
  message: message,
) -> Result(Nil, SendFailure) {
  case to == node.self() {
    True -> {
      let _ = send_local(name, message)
      Ok(Nil)
    }
    False -> {
      let outcome =
        send_with_options(#(name, node.to_atom(to)), #(name, message), [
          Noconnect,
        ])
      case outcome == refused(Noconnect) {
        True -> Error(NoConnect)
        False -> Ok(Nil)
      }
    }
  }
}

pub fn send_local(name: Name(message), message: message) -> Result(Nil, Nil) {
  use pid <- result.map(process.named(name))
  raw_send(pid, #(name, message))
  Nil
}

pub fn broadcast_named(
  nodes: List(Node),
  name: Name(message),
  message: message,
) -> Nil {
  list.each(nodes, fn(peer) { send_named(peer, name, message) })
}

pub fn connect(peer: Node) -> Bool {
  atom.to_string(net_connect(node.to_atom(peer))) == "true"
}
