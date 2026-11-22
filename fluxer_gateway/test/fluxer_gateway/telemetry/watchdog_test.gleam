// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/process_registry
import fluxer_gateway/telemetry/watchdog
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/set

pub fn watched_pids_covers_every_registered_guild_test() {
  let pid = process.spawn_unlinked(fn() { process.sleep_forever() })
  let ids =
    int.range(from: 1, to: 6001, with: [], run: list.prepend)
    |> list.map(fn(n) { 920_000_000_000_000_000 + n })
  list.each(ids, fn(id) {
    assert process_registry.register(process_registry.guilds, id, pid)
      == Ok(Nil)
  })
  let labels =
    watchdog.watched_pids()
    |> list.map(fn(entry) { entry.1 })
    |> set.from_list
  assert list.all(ids, fn(id) {
    set.contains(labels, "guild:" <> int.to_string(id))
  })
  list.each(ids, fn(id) {
    process_registry.unregister_pid(process_registry.guilds, id, pid)
  })
  process.kill(pid)
}
