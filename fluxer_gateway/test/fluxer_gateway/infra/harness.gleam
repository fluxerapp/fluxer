// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/supervisor.{type Child}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/result
import support/isolation

type TraceFlag {
  Receive
}

@external(erlang, "application", "get_supervisor")
fn get_supervisor(app: Atom) -> Result(Pid, Dynamic)

@external(erlang, "supervisor", "which_children")
fn which_children(supervisor: Pid) -> List(Dynamic)

@external(erlang, "supervisor", "terminate_child")
fn terminate_child(supervisor: Pid, id: Int) -> Dynamic

@external(erlang, "supervisor", "restart_child")
fn restart_child(supervisor: Pid, id: Int) -> Dynamic

@external(erlang, "erlang", "trace")
fn trace(pid: Pid, on: Bool, flags: List(TraceFlag)) -> Int

@external(erlang, "erlang", "is_pid")
fn is_pid(value: Dynamic) -> Bool

@external(erlang, "gleam_stdlib", "identity")
fn coerce(value: a) -> b

pub fn app_supervisor() -> Pid {
  let assert Ok(supervisor) = get_supervisor(atom.create("fluxer_gateway"))
  supervisor
}

pub fn child_id(child: Child) -> Int {
  let assert Ok(#(_, index)) =
    supervisor.plan(isolation.pristine_settings())
    |> list.index_map(fn(planned, index) { #(planned, index) })
    |> list.find(fn(entry) { entry.0 == child })
  index
}

pub fn running() -> List(#(Int, Pid)) {
  which_children(app_supervisor())
  |> list.filter_map(fn(row) {
    use id <- result.try(
      decode.run(row, decode.at([0], decode.int)) |> result.replace_error(Nil),
    )
    use pid <- result.map(
      decode.run(row, decode.at([1], decode.dynamic))
      |> result.replace_error(Nil)
      |> result.try(as_pid),
    )
    #(id, pid)
  })
}

pub fn pid_of(child: Child) -> Result(Pid, Nil) {
  list.key_find(running(), child_id(child))
}

pub fn cycle(child: Child) -> Pid {
  let id = child_id(child)
  terminate_child(app_supervisor(), id)
  restart_child(app_supervisor(), id)
  let assert Ok(pid) = pid_of(child)
  pid
}

pub fn received_during(pid: Pid, window_ms: Int) -> List(Dynamic) {
  trace(pid, True, [Receive])
  process.sleep(window_ms)
  let _ = trace(pid, False, [Receive])
  collect([])
}

fn collect(acc: List(Dynamic)) -> List(Dynamic) {
  let selector =
    process.new_selector()
    |> process.select_record(atom.create("trace"), 3, fn(message) {
      decode.run(message, decode.at([3], decode.dynamic))
      |> result.unwrap(dynamic.nil())
    })
  case process.selector_receive(selector, 50) {
    Ok(message) -> collect([message, ..acc])
    Error(Nil) -> list.reverse(acc)
  }
}

pub fn tagged(message: Dynamic, tag: String) -> Bool {
  let payload =
    decode.run(message, decode.at([1], decode.dynamic))
    |> result.unwrap(dynamic.nil())
  let name = fn(term) {
    decode.run(term, atom.decoder()) |> result.map(atom.to_string)
  }
  name(payload) == Ok(tag)
  || {
    decode.run(payload, decode.at([0], decode.dynamic))
    |> result.replace_error(Nil)
    |> result.try(fn(head) { name(head) |> result.replace_error(Nil) })
  }
  == Ok(tag)
}

pub fn count_tagged(messages: List(Dynamic), tag: String) -> Int {
  list.count(messages, tagged(_, tag))
}

fn as_pid(value: Dynamic) -> Result(Pid, Nil) {
  case is_pid(value) {
    True -> Ok(coerce(value))
    False -> Error(Nil)
  }
}
