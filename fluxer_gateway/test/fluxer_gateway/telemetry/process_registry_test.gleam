// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/session
import fluxer_gateway/telemetry/process_registry
import gleam/erlang/process
import gleam/list

const id_of_guild = 910_000_000_000_000_001

const sweep_dead_user = 910_000_000_000_000_002

const sweep_alive_user = 910_000_000_000_000_003

const clear_voice = 910_000_000_000_000_004

const clear_guild = 910_000_000_000_000_005

pub fn id_of_finds_the_id_registered_for_a_pid_test() {
  let pid = process.spawn_unlinked(fn() { process.sleep_forever() })
  let other = process.spawn_unlinked(fn() { process.sleep_forever() })
  let session_id = session.new_session_id()
  assert process_registry.register(process_registry.guilds, id_of_guild, pid)
    == Ok(Nil)
  assert process_registry.register(process_registry.sessions, session_id, other)
    == Ok(Nil)
  assert process_registry.id_of(process_registry.guilds, pid) == Ok(id_of_guild)
  assert process_registry.id_of(process_registry.guilds, other) == Error(Nil)
  assert process_registry.id_of(process_registry.sessions, other)
    == Ok(session_id)
  process_registry.unregister_pid(process_registry.guilds, id_of_guild, pid)
  process_registry.unregister_pid(process_registry.sessions, session_id, other)
  assert process_registry.id_of(process_registry.guilds, pid) == Error(Nil)
  process.kill(pid)
  process.kill(other)
}

pub fn sweep_dead_removes_rows_of_dead_processes_only_test() {
  let dead = process.spawn_unlinked(fn() { Nil })
  let alive = process.spawn_unlinked(fn() { process.sleep_forever() })
  assert process_registry.register(
      process_registry.presences,
      sweep_dead_user,
      dead,
    )
    == Ok(Nil)
  assert process_registry.register(
      process_registry.presences,
      sweep_alive_user,
      alive,
    )
    == Ok(Nil)
  process.kill(dead)
  let _ = process_registry.sweep_dead()
  let ids =
    process_registry.entries(process_registry.presences)
    |> list.map(fn(entry) { entry.0 })
  assert !list.contains(ids, sweep_dead_user)
  assert list.contains(ids, sweep_alive_user)
  process_registry.unregister_pid(
    process_registry.presences,
    sweep_alive_user,
    alive,
  )
  process.kill(alive)
}

pub fn clear_drops_every_row_of_one_kind_test() {
  let pid = process.spawn_unlinked(fn() { process.sleep_forever() })
  assert process_registry.register(process_registry.voices, clear_voice, pid)
    == Ok(Nil)
  assert process_registry.register(process_registry.guilds, clear_guild, pid)
    == Ok(Nil)
  assert process_registry.clear(process_registry.voices) >= 1
  assert process_registry.entries(process_registry.voices) == []
  assert process_registry.id_of(process_registry.guilds, pid) == Ok(clear_guild)
  process_registry.unregister_pid(process_registry.guilds, clear_guild, pid)
  process.kill(pid)
}
