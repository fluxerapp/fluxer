// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/guild/table
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/erlang/reference.{type Reference}
import gleam/int

type TraceSession

@external(erlang, "trace", "session_create")
fn session_create(name: Atom, tracer: Pid, options: List(Nil)) -> TraceSession

@external(erlang, "trace", "session_destroy")
fn session_destroy(session: TraceSession) -> Bool

@external(erlang, "trace", "process")
fn trace_process(
  session: TraceSession,
  pid: Pid,
  enable: Bool,
  flags: List(Atom),
) -> Int

@external(erlang, "trace", "function")
fn trace_function(
  session: TraceSession,
  mfa: #(Atom, Atom, Int),
  spec: List(#(List(Atom), List(Nil), List(Nil))),
  flags: List(Atom),
) -> Int

@external(erlang, "trace", "delivered")
fn trace_delivered(session: TraceSession, tracee: Pid) -> Reference

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(value: a) -> Dynamic

@external(erlang, "erlang", "element")
fn element(index: Int, tuple: Dynamic) -> Dynamic

fn size_reads(run: fn() -> Nil) -> Int {
  let ready = process.new_subject()
  let done = process.new_subject()
  let worker =
    process.spawn_unlinked(fn() {
      let go = process.new_subject()
      process.send(ready, go)
      let _ = process.receive(go, 5000)
      run()
      process.send(done, Nil)
    })
  let assert Ok(go) = process.receive(ready, 5000)
  let session =
    session_create(atom.create("guild_table_exists"), process.self(), [])
  let _ =
    trace_function(
      session,
      #(atom.create("ets"), atom.create("info"), 2),
      [#([atom.create("_"), atom.create("size")], [], [])],
      [atom.create("global")],
    )
  let _ = trace_process(session, worker, True, [atom.create("call")])
  process.send(go, Nil)
  let assert Ok(Nil) = process.receive(done, 5000)
  let delivered = trace_delivered(session, worker)
  let _ =
    process.new_selector()
    |> process.select_record(atom.create("trace_delivered"), 2, fn(message) {
      element(3, message) == to_dynamic(delivered)
    })
    |> process.selector_receive(5000)
  let _ = session_destroy(session)
  count_traced(0)
}

fn count_traced(count: Int) -> Int {
  let selector =
    process.new_selector()
    |> process.select_other(fn(_) { Nil })
  case process.selector_receive(selector, 0) {
    Ok(Nil) -> count_traced(count + 1)
    Error(Nil) -> count
  }
}

pub fn exists_never_reads_the_size_of_a_shared_table_test() {
  let rows =
    table.new("guild_table_exists_probe", [
      table.OrderedSet,
      table.Public,
      table.WriteConcurrency(True),
    ])
  let reads =
    size_reads(fn() {
      int.range(1, 51, Nil, fn(_, key) {
        table.insert(rows, key, key)
        let _ = table.exists(rows)
        let _ = table.lookup(rows, key)
        Nil
      })
    })
  assert reads == 0
  table.drop(rows)
}

pub fn exists_tells_a_live_table_from_a_dropped_one_test() {
  let rows = table.new("guild_table_exists_dropped", [table.Set, table.Public])
  assert table.exists(rows)
  table.drop(rows)
  assert !table.exists(rows)
  assert !table.exists(table.named("guild_table_exists_never_created"))
}
