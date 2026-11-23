// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/result

pub opaque type Paused {
  Paused(supervisor: Pid, id: Dynamic)
}

@external(erlang, "application", "get_supervisor")
fn get_supervisor(app: Atom) -> Result(Pid, Dynamic)

@external(erlang, "supervisor", "which_children")
fn which_children(supervisor: Pid) -> List(Dynamic)

@external(erlang, "supervisor", "terminate_child")
fn terminate_child(supervisor: Pid, id: Dynamic) -> Dynamic

@external(erlang, "supervisor", "restart_child")
fn restart_child(supervisor: Pid, id: Dynamic) -> Dynamic

@external(erlang, "erlang", "whereis")
fn whereis(name: Atom) -> Dynamic

@external(erlang, "erlang", "is_pid")
fn is_pid(value: Dynamic) -> Bool

@external(erlang, "gleam_stdlib", "identity")
fn coerce(value: a) -> b

pub fn registered(name: String) -> Result(Pid, Nil) {
  let found = whereis(atom.create(name))
  case is_pid(found) {
    True -> Ok(coerce(found))
    False -> Error(Nil)
  }
}

pub fn pause(name: String) -> Result(Paused, Nil) {
  use pid <- result.try(registered(name))
  use supervisor <- result.try(
    get_supervisor(atom.create("fluxer_gateway")) |> result.replace_error(Nil),
  )
  use id <- result.map(
    which_children(supervisor)
    |> list.find_map(fn(child) {
      let pid_at =
        decode.run(child, decode.at([1], decode.dynamic))
        |> result.unwrap(dynamic.nil())
      case is_pid(pid_at) && coerce(pid_at) == pid {
        True ->
          decode.run(child, decode.at([0], decode.dynamic))
          |> result.replace_error(Nil)
        False -> Error(Nil)
      }
    }),
  )
  terminate_child(supervisor, id)
  Paused(supervisor:, id:)
}

pub fn resume(paused: Paused) -> Nil {
  restart_child(paused.supervisor, paused.id)
  Nil
}

pub fn stop_linked(pid: Pid) -> Nil {
  process.unlink(pid)
  let monitor = process.monitor(pid)
  process.kill(pid)
  let _ =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { Nil })
    |> process.selector_receive(5000)
  Nil
}
