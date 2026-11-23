// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/util/liveness
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type Mfa {
  Mfa(module: String, function: String, arity: Int)
}

type Item {
  Memory
  MessageQueueLen
  Reductions
  CurrentFunction
  InitialCall
  RegisteredName
}

type GcOption {
  Type(GcType)
}

type GcType {
  Major
}

@external(erlang, "erlang", "process_info")
fn erlang_process_info(pid: Pid, item: Item) -> Dynamic

@external(erlang, "erlang", "process_info")
fn erlang_process_dictionary_item(pid: Pid, item: #(Atom, Atom)) -> Dynamic

@external(erlang, "erlang", "process_info")
fn erlang_process_info_list(pid: Pid, items: List(Item)) -> Dynamic

@external(erlang, "erlang", "processes")
fn erlang_processes() -> List(Pid)

@external(erlang, "erlang", "registered")
fn erlang_registered() -> List(Atom)

@external(erlang, "erlang", "whereis")
fn erlang_whereis(name: Atom) -> Dynamic

@external(erlang, "erlang", "garbage_collect")
fn erlang_garbage_collect(pid: Pid, options: List(GcOption)) -> Dynamic

@external(erlang, "erlang", "is_pid")
fn is_pid(value: Dynamic) -> Bool

@external(erlang, "gleam_erlang_ffi", "identity")
fn dynamic_to_pid(value: Dynamic) -> Pid

@external(erlang, "erlang", "pid_to_list")
fn pid_to_list(pid: Pid) -> Charlist

@external(erlang, "rand", "uniform")
fn uniform(range: Int) -> Int

pub fn pid_decoder() -> Decoder(Pid) {
  decode.new_primitive_decoder("Pid", fn(data) {
    case is_pid(data) {
      True -> Ok(dynamic_to_pid(data))
      False -> Error(process.self())
    }
  })
}

pub fn memory(pid: Pid) -> Result(Int, Nil) {
  read(pid, Memory, decode.int)
}

pub fn message_queue_len(pid: Pid) -> Result(Int, Nil) {
  read(pid, MessageQueueLen, decode.int)
}

pub fn memory_and_queue(pid: Pid) -> Result(#(Int, Int), Nil) {
  case liveness.is_local(pid) {
    False -> Error(Nil)
    True ->
      case
        decode.run(
          erlang_process_info_list(pid, [Memory, MessageQueueLen]),
          decode.list(decode.at([1], decode.int)),
        )
      {
        Ok([memory, queued]) -> Ok(#(memory, queued))
        _ -> Error(Nil)
      }
  }
}

pub fn reductions(pid: Pid) -> Result(Int, Nil) {
  read(pid, Reductions, decode.int)
}

pub fn current_function(pid: Pid) -> Result(Mfa, Nil) {
  read(pid, CurrentFunction, mfa_decoder())
}

pub fn initial_call(pid: Pid) -> Result(Mfa, Nil) {
  read(pid, InitialCall, mfa_decoder())
}

pub fn proc_lib_initial_call(pid: Pid) -> Result(Mfa, Nil) {
  case liveness.is_local(pid) {
    False -> Error(Nil)
    True -> {
      let key = #(atom.create("dictionary"), atom.create("$initial_call"))
      case
        decode.run(
          erlang_process_dictionary_item(pid, key),
          decode.at([1], mfa_decoder()),
        )
      {
        Ok(mfa) -> Ok(mfa)
        Error(_) -> initial_call(pid)
      }
    }
  }
}

pub fn is_spawned_fun(pid: Pid) -> Bool {
  case initial_call(pid) {
    Ok(Mfa("erlang", "apply", 2)) -> True
    Ok(Mfa("proc_lib", "init_p", _)) ->
      case proc_lib_initial_call(pid) {
        Ok(Mfa(module, function, _)) ->
          module != "gleam@otp@actor" && string.starts_with(function, "-")
        Error(Nil) -> False
      }
    _ -> False
  }
}

pub fn registered_name(pid: Pid) -> Result(String, Nil) {
  read(pid, RegisteredName, atom.decoder())
  |> result.map(atom.to_string)
}

pub fn scan_pids(limit: Int) -> List(Pid) {
  let pids = erlang_processes()
  let count = list.length(pids)
  case count <= limit {
    True -> pids
    False -> {
      let #(front, back) = list.split(pids, uniform(count) - 1)
      list.append(back, front) |> list.take(limit)
    }
  }
}

fn registered() -> List(#(String, Pid)) {
  erlang_registered()
  |> list.filter_map(fn(name) {
    decode.run(erlang_whereis(name), pid_decoder())
    |> result.map(fn(pid) { #(atom.to_string(name), pid) })
    |> result.replace_error(Nil)
  })
}

pub fn gateway_processes() -> List(#(String, Pid)) {
  registered()
  |> list.filter(fn(entry) { string.starts_with(entry.0, "fluxer_") })
  |> list.map(fn(entry) {
    let #(name, pid) = entry
    case string.split_once(name, "$") {
      Ok(#(prefix, suffix)) ->
        case id.parse_decimal(suffix) {
          Ok(_) -> #(prefix, pid)
          Error(Nil) -> entry
        }
      Error(Nil) -> entry
    }
  })
}

pub fn major_gc(pid: Pid) -> Nil {
  case liveness.is_local(pid) {
    True -> {
      let _ = erlang_garbage_collect(pid, [Type(Major)])
      Nil
    }
    False -> Nil
  }
}

pub fn mfa_to_string(mfa: Mfa) -> String {
  mfa.module <> ":" <> mfa.function <> "/" <> int.to_string(mfa.arity)
}

fn read(pid: Pid, item: Item, decoder: Decoder(a)) -> Result(a, Nil) {
  case liveness.is_local(pid) {
    False -> Error(Nil)
    True ->
      decode.run(erlang_process_info(pid, item), decode.at([1], decoder))
      |> result.replace_error(Nil)
  }
}

fn mfa_decoder() -> Decoder(Mfa) {
  use module <- decode.field(0, atom.decoder())
  use function <- decode.field(1, atom.decoder())
  use arity <- decode.field(2, decode.int)
  decode.success(Mfa(atom.to_string(module), atom.to_string(function), arity))
}

pub fn pid_text(pid: Pid) -> String {
  charlist.to_string(pid_to_list(pid))
}

pub fn stop(pid: Pid, wait_ms: Int, kill_wait_ms: Int) -> Nil {
  let monitor = process.monitor(pid)
  let selector =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { Nil })
  process.send_abnormal_exit(pid, atom.create("shutdown"))
  case process.selector_receive(selector, wait_ms) {
    Ok(Nil) -> Nil
    Error(Nil) -> {
      process.kill(pid)
      case process.selector_receive(selector, kill_wait_ms) {
        Ok(Nil) -> Nil
        Error(Nil) -> process.demonitor_process(monitor)
      }
    }
  }
}
