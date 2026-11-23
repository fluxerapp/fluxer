// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/remote
import fluxer_gateway/core/node
import fluxer_gateway/util/spawn
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Monitor, type Name, type Subject}
import gleam/list
import gleam/result

pub type CallError {
  CallTimeout
  CallUnavailable
  CallBusy
}

@external(erlang, "erpc", "multicall")
fn erpc_multicall(
  nodes: List(Atom),
  module: Atom,
  function: Atom,
  args: List(a),
  timeout: Int,
) -> List(Result(reply, Dynamic))

@external(erlang, "erlang", "node")
fn pid_node(pid: process.Pid) -> Atom

@external(erlang, "erlang", "send")
fn raw_send(
  pid: process.Pid,
  message: #(Name(message), message),
) -> #(Name(message), message)

pub fn send_as(pid: process.Pid, name: Name(message), message: message) -> Nil {
  let _ = raw_send(pid, #(name, message))
  Nil
}

type SendOption {
  Noconnect
  Nosuspend
}

@external(erlang, "erlang", "send")
fn send_with_options(
  destination: #(Name(target), Atom),
  message: #(Name(message), message),
  options: List(SendOption),
) -> Atom

pub fn send_tagged(
  target: Atom,
  name: Name(destination),
  tag: Name(message),
  message: message,
) -> Result(Nil, CallError) {
  case target == self_atom() {
    True ->
      case process.named(name) {
        Ok(pid) -> Ok(send_as(pid, tag, message))
        Error(Nil) -> Error(CallUnavailable)
      }
    False ->
      case
        atom.to_string(
          send_with_options(#(name, target), #(tag, message), [
            Noconnect,
            Nosuspend,
          ]),
        )
      {
        "nosuspend" -> Error(CallBusy)
        "noconnect" -> Error(CallUnavailable)
        _ -> Ok(Nil)
      }
  }
}

pub fn send_registered(
  name: Name(message),
  message: message,
) -> Result(Nil, Nil) {
  case process.named(name) {
    Ok(pid) -> Ok(send_as(pid, name, message))
    Error(Nil) -> Error(Nil)
  }
}

pub fn self_atom() -> Atom {
  node.to_atom(node.self())
}

pub fn node_of(subject: Subject(a)) -> Atom {
  case process.subject_owner(subject) {
    Ok(pid) -> pid_node(pid)
    Error(Nil) -> self_atom()
  }
}

pub fn send(
  target: Atom,
  name: Name(message),
  message: message,
) -> Result(Nil, CallError) {
  case target == self_atom() {
    True ->
      send_registered(name, message) |> result.replace_error(CallUnavailable)
    False ->
      case remote.send_named(node.from_atom(target), name, message) {
        Ok(Nil) -> Ok(Nil)
        Error(remote.NoSuspend) -> Error(CallBusy)
        Error(remote.NoConnect) -> Error(CallUnavailable)
      }
  }
}

pub fn call(
  target: Atom,
  name: Name(message),
  timeout_ms: Int,
  make: fn(Subject(reply)) -> message,
) -> Result(reply, CallError) {
  isolated(fn() { call_from_disposable(target, name, timeout_ms, make) })
}

pub fn call_from_disposable(
  target: Atom,
  name: Name(message),
  timeout_ms: Int,
  make: fn(Subject(reply)) -> message,
) -> Result(reply, CallError) {
  let reply = process.new_subject()
  case target == self_atom() {
    True -> {
      let callee = process.named(name)
      use Nil <- result.try(send(target, name, make(reply)))
      await(reply, result.map(callee, process.monitor), timeout_ms)
    }
    False ->
      case remote.ask_named(node.from_atom(target), name, make(reply)) {
        Ok(monitor) -> await(reply, Ok(monitor), timeout_ms)
        Error(remote.NoSuspend) -> Error(CallBusy)
        Error(remote.NoConnect) -> Error(CallUnavailable)
      }
  }
}

pub fn call_subject(
  subject: Subject(message),
  timeout_ms: Int,
  make: fn(Subject(reply)) -> message,
) -> Result(reply, CallError) {
  isolated(fn() { call_subject_from_disposable(subject, timeout_ms, make) })
}

pub fn call_subject_from_disposable(
  subject: Subject(message),
  timeout_ms: Int,
  make: fn(Subject(reply)) -> message,
) -> Result(reply, CallError) {
  let reply = process.new_subject()
  process.send(subject, make(reply))
  await(reply, watch_owner(subject), timeout_ms)
}

pub fn await_from_disposable(
  reply: Subject(reply),
  sender: Subject(message),
  timeout_ms: Int,
) -> Result(reply, CallError) {
  await(reply, watch_owner(sender), timeout_ms)
}

fn watch_owner(subject: Subject(message)) -> Result(Monitor, Nil) {
  process.subject_owner(subject) |> result.map(process.monitor)
}

fn isolated(run: fn() -> Result(reply, CallError)) -> Result(reply, CallError) {
  let outcome = process.new_subject()
  let #(_, monitor) = spawn.monitored(fn() { process.send(outcome, run()) })
  let received =
    process.new_selector()
    |> process.select(outcome)
    |> process.select_specific_monitor(monitor, fn(_) { Error(CallUnavailable) })
    |> process.selector_receive_forever
  process.demonitor_process(monitor)
  received
}

fn await(
  reply: Subject(reply),
  watch: Result(Monitor, Nil),
  timeout_ms: Int,
) -> Result(reply, CallError) {
  let selector =
    process.new_selector()
    |> process.select_map(reply, Ok)
  let selector = case watch {
    Ok(monitor) ->
      process.select_specific_monitor(selector, monitor, fn(_) {
        Error(CallUnavailable)
      })
    Error(Nil) -> selector
  }
  let outcome =
    process.selector_receive(selector, timeout_ms)
    |> result.unwrap(Error(CallTimeout))
  case watch {
    Ok(monitor) -> process.demonitor_process(monitor)
    Error(Nil) -> Nil
  }
  outcome
}

pub fn multicall_each(
  requests: List(#(Atom, a)),
  module: String,
  function: String,
  timeout_ms: Int,
) -> List(#(Atom, Result(reply, Nil))) {
  let replies = process.new_subject()
  let module = atom.create(module)
  let function = atom.create(function)
  let pending =
    list.fold(requests, dict.new(), fn(pending, request) {
      let #(node, argument) = request
      let #(_, monitor) =
        spawn.monitored(fn() {
          let outcome = case
            erpc_multicall([node], module, function, [argument], timeout_ms)
          {
            [Ok(reply)] -> Ok(reply)
            _ -> Error(Nil)
          }
          process.send(replies, #(node, outcome))
        })
      dict.insert(pending, node, monitor)
    })
  gather(replies, pending, [])
}

fn gather(
  replies: Subject(#(Atom, Result(reply, Nil))),
  pending: Dict(Atom, Monitor),
  gathered: List(#(Atom, Result(reply, Nil))),
) -> List(#(Atom, Result(reply, Nil))) {
  case dict.is_empty(pending) {
    True -> list.reverse(gathered)
    False -> {
      let selector =
        dict.fold(
          pending,
          process.new_selector() |> process.select(replies),
          fn(selector, node, monitor) {
            process.select_specific_monitor(selector, monitor, fn(_) {
              #(node, Error(Nil))
            })
          },
        )
      let #(node, outcome) = process.selector_receive_forever(selector)
      case dict.get(pending, node) {
        Ok(monitor) -> process.demonitor_process(monitor)
        Error(Nil) -> Nil
      }
      gather(replies, dict.delete(pending, node), [#(node, outcome), ..gathered])
    }
  }
}
