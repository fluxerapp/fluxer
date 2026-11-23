// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/permit
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import support/wait

pub type Command {
  Release
  Stop
}

pub type Report {
  Acquired(holder: Pid, result: Result(Nil, permit.DropReason))
  Released(holder: Pid)
}

pub type Holder {
  Holder(pid: Pid, commands: Subject(Command))
}

type Row =
  #(#(Dynamic, Int), Pid, Dynamic, Bool, Int)

@external(erlang, "ets", "tab2list")
fn ets_rows(table: Atom) -> List(Row)

@external(erlang, "erlang", "binary_to_atom")
fn to_atom(name: String) -> Atom

pub fn server_pid() -> Result(Pid, Nil) {
  process.named(process_name("fluxer_gateway_push_permits"))
}

@external(erlang, "erlang", "binary_to_atom")
fn process_name(name: String) -> process.Name(a)

pub fn rows_of(pid: Pid) -> List(Bool) {
  ets_rows(to_atom("fluxer_gateway_push_permit_rows"))
  |> list.filter(fn(row) { row.1 == pid })
  |> list.map(fn(row) { row.3 })
}

pub fn held_count() -> Int {
  ets_rows(to_atom("fluxer_gateway_push_permit_rows"))
  |> list.count(fn(row) { row.3 })
}

pub fn waiting_count() -> Int {
  ets_rows(to_atom("fluxer_gateway_push_permit_rows"))
  |> list.count(fn(row) { !row.3 })
}

pub fn spawn_holder(
  kind: permit.PoolKind,
  wait_ms: Int,
  reports: Subject(Report),
) -> Holder {
  let handoff = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let commands = process.new_subject()
      process.send(handoff, commands)
      let me = process.self()
      case permit.acquire(kind, wait_ms) {
        Ok(held) -> {
          process.send(reports, Acquired(me, Ok(Nil)))
          serve(commands, fn() {
            permit.release(held)
            process.send(reports, Released(me))
          })
        }
        Error(reason) -> {
          process.send(reports, Acquired(me, Error(reason)))
          serve(commands, fn() { Nil })
        }
      }
    })
  let assert Ok(commands) = process.receive(handoff, 1000)
  Holder(pid:, commands:)
}

fn serve(commands: Subject(Command), release: fn() -> Nil) -> Nil {
  case process.receive_forever(commands) {
    Release -> {
      release()
      serve(commands, fn() { Nil })
    }
    Stop -> Nil
  }
}

pub fn eventually(check: fn() -> Bool, tries: Int) -> Bool {
  wait.until(tries * 10, check)
}
