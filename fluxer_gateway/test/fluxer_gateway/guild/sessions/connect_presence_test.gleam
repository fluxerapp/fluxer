// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/presence.{Dnd}
import fluxer_gateway/guild/presence/row
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/erlang/reference.{type Reference}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake
import support/presence_scenario as scn
import support/wait

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
  spec: List(#(Atom, List(Nil), List(Nil))),
  flags: List(Atom),
) -> Int

@external(erlang, "trace", "delivered")
fn trace_delivered(session: TraceSession, tracee: Pid) -> Reference

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(value: a) -> Dynamic

@external(erlang, "gleam_stdlib", "identity")
fn unsafe_state(value: Dynamic) -> GuildState

@external(erlang, "erlang", "element")
fn element(index: Int, tuple: Dynamic) -> Dynamic

@external(erlang, "sys", "get_state")
fn server_state(pid: Pid) -> Dynamic

fn presence_reads(pid: Pid, run: fn() -> Nil) -> Int {
  let session =
    session_create(atom.create("guild_connect_presence"), process.self(), [])
  let _ =
    trace_function(
      session,
      #(
        atom.create("fluxer_gateway@presence@cache"),
        atom.create("lookup_many"),
        1,
      ),
      [#(atom.create("_"), [], [])],
      [atom.create("local")],
    )
  let _ = trace_process(session, pid, True, [atom.create("call")])
  run()
  let delivered = trace_delivered(session, pid)
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
    |> process.select_record(atom.create("trace"), 3, fn(_) { Nil })
  case process.selector_receive(selector, 0) {
    Ok(Nil) -> count_traced(count + 1)
    Error(Nil) -> count
  }
}

pub fn a_connecting_member_costs_the_guild_no_presence_read_test() {
  use api <- guild_api.with_api
  let found =
    int.range(2, 42, fixture.new(1) |> fixture.with_text(10), fn(acc, user) {
      fixture.with_member(acc, user, [])
    })
  let handle = fixture.started(api, found)
  let pid = handle.pid
  let sessions = list.map(int.range(2, 42, [], list.prepend), fake.start)
  let reads =
    presence_reads(pid, fn() {
      list.each(sessions, fn(session) {
        assert fake.is_joined(fake.join(session, handle))
      })
      fixture.sync(handle)
    })
  assert reads == 0
  list.each(sessions, fake.stop)
  fixture.stop(found)
}

pub fn a_connecting_member_gets_the_presence_its_owner_holds_test() {
  use api <- guild_api.with_api
  let user = 2_000_000_000 + int.random(1_000_000_000)
  let found =
    fixture.new(1) |> fixture.with_text(10) |> fixture.with_member(user, [])
  let device = scn.fake_session(user, int.random(1_000_000_000))
  let _link = scn.connect(device, Dnd, [])
  scn.sync(user)
  let handle = fixture.started(api, found)
  let session = fake.start(user)
  assert fake.is_joined(fake.join(session, handle))
  let pid = handle.pid
  assert wait.until(3000, fn() {
    let state = unsafe_state(element(2, server_state(pid)))
    case table.presence_row(guild_state.sessions(state), user) {
      Some(found) -> row.status(found) == Dnd
      None -> False
    }
  })
  fake.stop(session)
  scn.exit_session(device, scn.closed_reason())
  fixture.stop(found)
}
