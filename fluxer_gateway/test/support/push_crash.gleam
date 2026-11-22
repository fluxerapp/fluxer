// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list

type InfoItem {
  CurrentStacktrace
  MessageQueueLen
}

@external(erlang, "erlang", "processes")
fn processes() -> List(Pid)

@external(erlang, "erlang", "process_info")
fn process_info(pid: Pid, item: InfoItem) -> Dynamic

@external(erlang, "sys", "suspend")
fn sys_suspend(pid: Pid) -> Dynamic

type AtomicsRef

type AtomicsOption {
  Signed(Bool)
}

@external(erlang, "atomics", "new")
fn atomics_new(arity: Int, options: List(AtomicsOption)) -> AtomicsRef

@external(erlang, "atomics", "add_get")
fn atomics_add_get(ref: AtomicsRef, index: Int, value: Int) -> Int

pub fn once() -> fn() -> Bool {
  let ref = atomics_new(1, [Signed(True)])
  fn() { atomics_add_get(ref, 1, 1) == 1 }
}

pub fn suspend(pid: Pid) -> Nil {
  let _ = sys_suspend(pid)
  Nil
}

pub fn running(module: String, function: String) -> List(Pid) {
  let module = atom.create(module)
  let function = atom.create(function)
  let me = process.self()
  processes()
  |> list.filter(fn(pid) {
    pid != me
    && list.any(frames(pid), fn(frame) { frame == #(module, function) })
  })
}

@external(erlang, "erlang", "suspend_process")
fn suspend_process(pid: Pid) -> Bool

pub fn freeze_running(module: String, function: String) -> Int {
  let found = running(module, function)
  list.each(found, fn(pid) {
    let _ = suspend_process(pid)
    Nil
  })
  list.length(found)
}

pub fn kill_running(module: String, function: String) -> Int {
  let found = running(module, function)
  list.each(found, process.kill)
  list.length(found)
}

fn frames(pid: Pid) -> List(#(Atom, Atom)) {
  let frame = {
    use module <- decode.field(0, atom.decoder())
    use function <- decode.field(1, atom.decoder())
    decode.success(#(module, function))
  }
  case
    decode.run(
      process_info(pid, CurrentStacktrace),
      decode.at([1], decode.list(frame)),
    )
  {
    Ok(found) -> found
    Error(_) -> []
  }
}

pub fn mailbox_size(pid: Pid) -> Int {
  case
    decode.run(process_info(pid, MessageQueueLen), decode.at([1], decode.int))
  {
    Ok(size) -> size
    Error(_) -> 0
  }
}
