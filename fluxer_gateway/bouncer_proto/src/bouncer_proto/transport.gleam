// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/message.{type ToBouncer, type ToMover, type ToWorker}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Name}
import gleam/int

pub type SendFailure {
  Busy
  NotConnected
}

type SendOption {
  Noconnect
  Nosuspend
}

type Sent

@external(erlang, "erlang", "binary_to_atom")
fn bouncer_name_atom(name: String) -> Name(ToBouncer)

@external(erlang, "erlang", "binary_to_atom")
fn worker_name_atom(name: String) -> Name(ToWorker)

@external(erlang, "erlang", "binary_to_atom")
fn mover_name_atom(name: String) -> Name(ToMover)

@external(erlang, "erlang", "send")
fn send_with_options(
  destination: #(Name(message), Atom),
  message: #(Name(message), message),
  options: List(SendOption),
) -> Sent

@external(erlang, "gleam_stdlib", "identity")
fn refused(option: SendOption) -> Sent

@external(erlang, "net_kernel", "connect_node")
fn net_connect(node: Atom) -> Atom

pub fn bouncer_name() -> Name(ToBouncer) {
  bouncer_name_atom("fluxer_bouncer")
}

pub fn worker_name() -> Name(ToWorker) {
  worker_name_atom("fluxer_gateway_bouncer_client")
}

pub fn mover_name() -> Name(ToMover) {
  mover_name_atom("fluxer_gateway_handoff")
}

pub fn send(
  to node: Atom,
  name name: Name(message),
  message message: message,
) -> Result(Nil, SendFailure) {
  let outcome =
    send_with_options(#(name, node), #(name, message), [Noconnect, Nosuspend])
  case outcome == refused(Nosuspend), outcome == refused(Noconnect) {
    True, _ -> Error(Busy)
    _, True -> Error(NotConnected)
    False, False -> Ok(Nil)
  }
}

pub fn deliver(
  to node: Atom,
  name name: Name(message),
  message message: message,
) -> Result(Nil, SendFailure) {
  let outcome = send_with_options(#(name, node), #(name, message), [Noconnect])
  case outcome == refused(Noconnect) {
    True -> Error(NotConnected)
    False -> Ok(Nil)
  }
}

pub fn connect(node: Atom) -> Bool {
  atom.to_string(net_connect(node)) == "true"
}

pub fn bouncer_node(
  basename basename: String,
  dns_name dns_name: String,
  ordinal ordinal: Int,
) -> Atom {
  atom.create(
    basename
    <> "@"
    <> basename
    <> "-"
    <> int.to_string(ordinal)
    <> "."
    <> dns_name,
  )
}
