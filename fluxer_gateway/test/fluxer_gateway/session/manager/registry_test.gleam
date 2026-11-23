// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/session/manager/registry
import gleam/erlang/process
import gleam/list
import gleeunit/should

fn session_ref(user: Int) -> SessionRef {
  let assert Ok(user) = id.user_id(user)
  session.SessionRef(
    id: session.new_session_id(),
    user:,
    inbox: process.new_subject(),
  )
}

fn dead_session_ref(user: Int) -> SessionRef {
  let parent = process.new_subject()
  let owner =
    process.spawn_unlinked(fn() { process.send(parent, session_ref(user)) })
  let monitor = process.monitor(owner)
  let assert Ok(ref) = process.receive(parent, 1000)
  let downs =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(d) { d })
  let assert Ok(_) = process.selector_receive(downs, 1000)
  ref
}

fn key(ref: SessionRef) -> String {
  session.session_id_to_string(ref.id)
}

pub fn dead_owner_is_hidden_and_swept_test() {
  registry.create_tables()
  let live = session_ref(1)
  let dead = dead_session_ref(2)
  registry.insert(live)
  registry.insert(dead)

  registry.lookup(key(dead)) |> should.equal(Error(Nil))
  registry.lookup(key(live)) |> should.equal(Ok(live))
  registry.all() |> list.contains(dead) |> should.be_false

  let swept = registry.sweep_dead()
  swept |> list.map(fn(row) { row.0 }) |> list.contains(dead) |> should.be_true
  swept |> list.map(fn(row) { row.0 }) |> list.contains(live) |> should.be_false
  registry.sweep_dead()
  |> list.map(fn(row) { row.0 })
  |> list.contains(dead)
  |> should.be_false
  registry.lookup(key(live)) |> should.equal(Ok(live))

  registry.remove(live)
}

pub fn a_departing_session_is_held_but_not_owned_test() {
  registry.create_tables()
  let staying = session_ref(3)
  let leaving = session_ref(4)
  registry.insert(staying)
  registry.insert(leaving)
  let held = registry.count()
  let owned = registry.owned()

  registry.depart(leaving)
  registry.count() |> should.equal(held)
  registry.owned() |> should.equal(owned - 1)
  registry.lookup(key(leaving)) |> should.equal(Ok(leaving))

  let returned = session.SessionRef(..leaving, inbox: process.new_subject())
  registry.insert(returned)
  registry.owned() |> should.equal(owned)

  registry.remove(leaving)
  registry.count() |> should.equal(held)
  registry.owned() |> should.equal(owned)

  registry.remove(returned)
  registry.remove(staying)
  registry.count() |> should.equal(held - 2)
  registry.owned() |> should.equal(owned - 2)
}
