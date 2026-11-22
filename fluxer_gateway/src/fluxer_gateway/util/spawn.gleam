// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/erlang/process.{type Monitor, type Pid}

type SpawnOption {
  Monitor
}

@external(erlang, "proc_lib", "spawn_opt")
fn spawn_opt(run: fn() -> a, options: List(SpawnOption)) -> #(Pid, Monitor)

pub fn monitored(run: fn() -> a) -> #(Pid, Monitor) {
  spawn_opt(run, [Monitor])
}
