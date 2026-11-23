// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
import fluxer_gateway/core/node
import fluxer_gateway/core/presence.{Dnd}
import fluxer_gateway/guild/presence/reconcile
import fluxer_gateway/guild/presence/row
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/presence/route
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/option.{None, Some}
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake
import support/presence_scenario as scn
import support/wait

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(value: a) -> Dynamic

@external(erlang, "gleam_stdlib", "identity")
fn unsafe_state(value: Dynamic) -> GuildState

@external(erlang, "gleam_stdlib", "identity")
fn unsafe_reconciled(value: Dynamic) -> Subject(reconcile.Mismatches)

@external(erlang, "erlang", "element")
fn element(index: Int, tuple: Dynamic) -> Dynamic

@external(erlang, "sys", "get_state")
fn server_state(pid: Pid) -> Dynamic

@external(erlang, "persistent_term", "get")
fn term_get(key: Atom, default: Dynamic) -> Dynamic

@external(erlang, "persistent_term", "put")
fn term_put(key: Atom, value: Dynamic) -> Nil

fn guild_state_of(pid: Pid) -> GuildState {
  unsafe_state(element(2, server_state(pid)))
}

fn shown_status(state: GuildState, user: Int) -> Result(presence.Status, Nil) {
  case table.presence_row(guild_state.sessions(state), user) {
    Some(found) -> Ok(row.status(found))
    None -> Error(Nil)
  }
}

fn without_presence_owner(run: fn() -> a) -> a {
  let key = atom.create("fluxer_presence_slots")
  let slots = term_get(key, to_dynamic(None))
  let before = settings.get()
  settings.store(settings.Settings(..before, role: node.Guilds))
  term_put(key, to_dynamic(None))
  let result = run()
  settings.store(before)
  term_put(key, slots)
  result
}

pub fn an_unroutable_presence_owner_keeps_a_connected_member_online_test() {
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
  assert wait.until(3000, fn() {
    shown_status(guild_state_of(handle.pid), user) == Ok(Dnd)
  })
  let state = guild_state_of(handle.pid)
  let guild = unsafe_reconciled(element(7, server_state(handle.pid)))
  let delivered =
    without_presence_owner(fn() {
      assert is_error(route.owner(user))
      let done = process.new_subject()
      reconcile.start(state, fn(found) {
        process.send(guild, found)
        process.send(done, Nil)
      })
      process.receive(done, 10_000)
    })
  assert delivered == Ok(Nil)
  fixture.sync(handle)
  assert shown_status(guild_state_of(handle.pid), user) == Ok(Dnd)
  fake.stop(session)
  scn.exit_session(device, scn.closed_reason())
  fixture.stop(found)
}

fn is_error(value: Result(a, b)) -> Bool {
  case value {
    Ok(_) -> False
    Error(_) -> True
  }
}
