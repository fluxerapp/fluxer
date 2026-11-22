// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{Durations, PushDurations}
import fluxer_gateway/push/permit
import gleam/erlang/process.{type Pid}
import gleam/list
import support/isolation
import support/push_crash
import support/push_permit_probe.{Acquired, Release, Released} as probe
import support/tick_trace
import support/wait

const module = "fluxer_gateway@push@permit"

fn release_timeout(ms: Int) -> Nil {
  isolation.set_durations(fn(durations) {
    Durations(
      ..durations,
      push: PushDurations(
        ..durations.push,
        permit_release_timeout_ms: ms,
        permit_init_timeout_ms: 5000,
      ),
    )
  })
}

fn retrying() -> Int {
  ["watch_release", "release_until", "next_server"]
  |> list.flat_map(push_crash.running(module, _))
  |> list.unique
  |> list.length
}

fn hold(reports) -> probe.Holder {
  let holder = probe.spawn_holder(permit.SettingsFetch, 2000, reports)
  let assert Ok(Acquired(pid, Ok(Nil))) = process.receive(reports, 1000)
  assert pid == holder.pid
  assert probe.rows_of(holder.pid) == [True]
  holder
}

fn release(holder: probe.Holder, reports) -> Nil {
  process.send(holder.commands, Release)
  let assert Ok(Released(pid)) = process.receive(reports, 1000)
  assert pid == holder.pid
  Nil
}

const server_name = "fluxer_gateway_push_permits"

fn server_away(run: fn() -> Nil) -> Pid {
  let assert Ok(server) = tick_trace.hide(server_name)
  assert probe.server_pid() == Error(Nil)
  run()
  server
}

fn back(server: Pid) -> Nil {
  let assert Ok(Nil) = tick_trace.reveal(server, server_name)
  Nil
}

pub fn a_release_to_a_busy_server_gives_up_at_the_release_timeout_test() {
  release_timeout(200)
  let reports = process.new_subject()
  let holder = hold(reports)
  let assert Ok(server) = probe.server_pid()
  push_crash.suspend(server)
  release(holder, reports)
  let assert True = wait.until(500, fn() { retrying() >= 1 })
  let assert True = wait.until(1500, fn() { retrying() == 0 })
  tick_trace.resume(server)
  let assert True = wait.until(1000, fn() { probe.rows_of(holder.pid) == [] })
  process.kill(holder.pid)
}

pub fn a_release_while_the_server_is_away_lands_when_it_returns_test() {
  release_timeout(3000)
  let reports = process.new_subject()
  let holder = hold(reports)
  let server = server_away(fn() { release(holder, reports) })
  process.sleep(300)
  assert retrying() >= 1
  assert probe.rows_of(holder.pid) == [True]
  back(server)
  let assert True = wait.until(1000, fn() { probe.rows_of(holder.pid) == [] })
  let assert True = wait.until(1000, fn() { retrying() == 0 })
  assert process.is_alive(holder.pid)
  process.kill(holder.pid)
}

pub fn a_release_retry_stops_at_the_release_timeout_while_the_server_stays_away_test() {
  release_timeout(150)
  let reports = process.new_subject()
  let holder = hold(reports)
  let server = server_away(fn() { release(holder, reports) })
  let assert True = wait.until(1000, fn() { retrying() == 0 })
  back(server)
  process.sleep(50)
  assert probe.rows_of(holder.pid) == [True]
  process.kill(holder.pid)
  let assert True = wait.until(1000, fn() { probe.rows_of(holder.pid) == [] })
}

pub fn the_release_timeout_is_its_own_knob_test() {
  release_timeout(1)
  let reports = process.new_subject()
  let holder = hold(reports)
  let assert Ok(server) = probe.server_pid()
  push_crash.suspend(server)
  release(holder, reports)
  let assert True = wait.until(1000, fn() { retrying() == 0 })
  tick_trace.resume(server)
  let assert True = wait.until(1000, fn() { probe.rows_of(holder.pid) == [] })
  let other = hold(reports)
  release(other, reports)
  let assert True = wait.until(1000, fn() { probe.rows_of(other.pid) == [] })
  process.kill(holder.pid)
  process.kill(other.pid)
}

pub fn keeper_evictions_do_not_multiply_across_restarts_test() {
  isolation.set_durations(fn(durations) {
    Durations(
      ..durations,
      push: PushDurations(..durations.push, server_evict_interval_ms: 50),
    )
  })
  let name = "fluxer_gateway_push_permit_keeper"
  tick_trace.inject(name, "evict_caches")
  process.sleep(20)
  let assert Ok(_) = tick_trace.restart(name, 2000)
  let assert True = wait.until(2000, fn() { probe.server_pid() != Error(Nil) })
  let assert Ok(keeper) = tick_trace.whereis(name)
  let evictions = tick_trace.count_received(keeper, "evict_caches", 600)
  assert evictions <= 15
}
