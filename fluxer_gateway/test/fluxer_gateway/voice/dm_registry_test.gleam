// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/session.{type SessionRef, SessionRef}
import fluxer_gateway/voice/dm/server as dm_server
import fluxer_gateway/voice/registry
import gleam/erlang/process
import gleam/list

fn session_ref() -> SessionRef {
  let assert Ok(user) = id.user_id(7)
  SessionRef(id: session.new_session_id(), user:, inbox: process.new_subject())
}

fn wait_down(subject: process.Subject(a)) -> Nil {
  let assert Ok(pid) = process.subject_owner(subject)
  let monitor = process.monitor(pid)
  let assert Ok(_) =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(down) { down })
    |> process.selector_receive(5000)
  Nil
}

pub fn concurrent_ensure_starts_one_actor_per_session_test() {
  registry.create_tables()
  let session = session_ref()
  let results = process.new_subject()
  let racers = 32
  list.repeat(Nil, racers)
  |> list.each(fn(_) {
    process.spawn(fn() { process.send(results, dm_server.ensure(session)) })
  })
  let found =
    list.repeat(Nil, racers)
    |> list.map(fn(_) {
      let assert Ok(Ok(subject)) = process.receive(results, 5000)
      subject
    })
    |> list.unique
  let assert [only] = found
  assert dm_server.lookup(session) == Ok(only)
  dm_server.detach(only, session)
  wait_down(only)
}

pub fn a_dead_registration_is_replaced_and_released_by_its_owner_test() {
  registry.create_tables()
  let session = session_ref()
  let assert Ok(first) = dm_server.ensure(session)
  let assert Ok(pid) = process.subject_owner(first)
  process.kill(pid)
  wait_down(first)
  let assert Ok(second) = dm_server.ensure(session)
  assert second != first
  assert dm_server.lookup(session) == Ok(second)
  dm_server.detach(second, session)
  wait_down(second)
  assert dm_server.lookup(session) == Error(Nil)
}
