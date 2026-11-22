// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import support/wait

type TraceFlag {
  Receive
}

@external(erlang, "erlang", "trace")
fn trace(pid: Pid, on: Bool, flags: List(TraceFlag)) -> Int

@external(erlang, "sys", "resume")
fn sys_resume(pid: Pid) -> Dynamic

@external(erlang, "erlang", "binary_to_atom")
fn name_of(name: String) -> process.Name(a)

pub fn whereis(name: String) -> Result(Pid, Nil) {
  process.named(name_of(name))
}

pub fn inject(name: String, tag: String) -> Nil {
  process.send(process.named_subject(name_of(name)), atom.create(tag))
}

pub fn hide(name: String) -> Result(Pid, Nil) {
  case whereis(name) {
    Ok(pid) ->
      case process.unregister(name_of(name)) {
        Ok(Nil) -> Ok(pid)
        Error(Nil) -> Error(Nil)
      }
    Error(Nil) -> Error(Nil)
  }
}

pub fn reveal(pid: Pid, name: String) -> Result(Nil, Nil) {
  process.register(pid, name_of(name))
}

pub fn resume(pid: Pid) -> Nil {
  let _ = sys_resume(pid)
  Nil
}

pub fn restart(name: String, timeout_ms: Int) -> Result(Pid, Nil) {
  case whereis(name) {
    Error(Nil) -> Error(Nil)
    Ok(before) -> {
      process.kill(before)
      let _ =
        wait.until_every(timeout_ms, 1, fn() {
          case whereis(name) {
            Ok(after) -> after != before
            Error(Nil) -> False
          }
        })
      case whereis(name) {
        Ok(after) if after != before -> Ok(after)
        _ -> Error(Nil)
      }
    }
  }
}

pub fn count_received(pid: Pid, tag: String, window_ms: Int) -> Int {
  let wanted = atom.create(tag)
  let _ = trace(pid, True, [Receive])
  let selector =
    process.new_selector()
    |> process.select_record(atom.create("trace"), 3, fn(found) { found })
  let counted = collect(selector, wanted, wait_deadline(window_ms), 0)
  let _ = trace(pid, False, [Receive])
  flush(selector)
  counted
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic(unit: Atom) -> Int

fn now_ms() -> Int {
  monotonic(atom.create("millisecond"))
}

fn wait_deadline(window_ms: Int) -> Int {
  now_ms() + window_ms
}

fn collect(
  selector: process.Selector(Dynamic),
  wanted: Atom,
  deadline: Int,
  counted: Int,
) -> Int {
  let left = deadline - now_ms()
  case left > 0 {
    False -> counted
    True ->
      case process.selector_receive(selector, left) {
        Error(Nil) -> counted
        Ok(found) ->
          case is_tick(found, wanted) {
            True -> collect(selector, wanted, deadline, counted + 1)
            False -> collect(selector, wanted, deadline, counted)
          }
      }
  }
}

fn is_tick(found: Dynamic, wanted: Atom) -> Bool {
  let message = decode.at([3], decode.dynamic)
  case decode.run(found, message) {
    Ok(inner) ->
      decode.run(inner, decode.at([1], atom.decoder())) == Ok(wanted)
      || decode.run(inner, atom.decoder()) == Ok(wanted)
    Error(_) -> False
  }
}

fn flush(selector: process.Selector(Dynamic)) -> Nil {
  case process.selector_receive(selector, 20) {
    Ok(_) -> flush(selector)
    Error(Nil) -> Nil
  }
}
