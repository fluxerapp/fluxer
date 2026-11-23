// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/permit
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list
import support/push_crash
import support/push_permit_probe.{
  type Holder, type Report, Acquired, Release, Released,
} as probe

const capacity = 64

fn fill(reports: Subject(Report)) -> List(Holder) {
  int.range(0, capacity, [], fn(acc, _) {
    let holder = probe.spawn_holder(permit.SettingsFetch, 5000, reports)
    let assert Ok(Acquired(pid, Ok(Nil))) = process.receive(reports, 1000)
    assert pid == holder.pid
    [holder, ..acc]
  })
  |> list.reverse
}

fn gone(pid: Pid) -> Bool {
  probe.rows_of(pid) == []
}

fn queued(holder: Holder) -> Bool {
  probe.rows_of(holder.pid) == [False]
}

fn cleanup(holders: List(Holder)) -> Nil {
  list.each(holders, fn(holder) { process.kill(holder.pid) })
  let assert True =
    probe.eventually(
      fn() { list.all(holders, fn(holder) { gone(holder.pid) }) },
      200,
    )
  Nil
}

fn restarted(before: Pid) -> Bool {
  probe.eventually(
    fn() {
      case probe.server_pid() {
        Ok(after) -> after != before
        Error(Nil) -> False
      }
    },
    300,
  )
}

fn freeze_then_kill(run: fn() -> Nil) -> Nil {
  let assert Ok(server) = probe.server_pid()
  push_crash.suspend(server)
  run()
  process.kill(server)
  let assert True = restarted(server)
  Nil
}

pub fn a_release_lost_with_the_server_is_sent_again_test() {
  let reports = process.new_subject()
  let holders = fill(reports)
  let assert [releasing, ..rest] = holders
  freeze_then_kill(fn() {
    process.send(releasing.commands, Release)
    let assert Ok(Released(_)) = process.receive(reports, 1000)
    Nil
  })
  let assert True = probe.eventually(fn() { gone(releasing.pid) }, 200)
  assert process.is_alive(releasing.pid)
  let waiter = probe.spawn_holder(permit.SettingsFetch, 2000, reports)
  let assert Ok(Acquired(pid, Ok(Nil))) = process.receive(reports, 1000)
  assert pid == waiter.pid
  cleanup([releasing, waiter, ..rest])
}

pub fn a_waiter_that_gave_up_during_a_restart_is_not_granted_later_test() {
  let reports = process.new_subject()
  let holders = fill(reports)
  let quitter = probe.spawn_holder(permit.SettingsFetch, 300, reports)
  let assert True = probe.eventually(fn() { queued(quitter) }, 100)
  freeze_then_kill(fn() {
    let assert Ok(Acquired(pid, Error(permit.PermitTimeout))) =
      process.receive(reports, 1000)
    assert pid == quitter.pid
  })
  let assert True = probe.eventually(fn() { gone(quitter.pid) }, 200)
  let assert [releasing, ..] = holders
  process.send(releasing.commands, Release)
  let assert Ok(Released(_)) = process.receive(reports, 1000)
  process.sleep(50)
  assert gone(quitter.pid)
  assert probe.held_count() == capacity - 1
  cleanup([quitter, ..holders])
}

pub fn a_claim_lost_with_the_server_is_made_again_test() {
  let reports = process.new_subject()
  let waiter = freeze_claim(reports)
  let assert Ok(Acquired(pid, Ok(Nil))) = process.receive(reports, 1000)
  assert pid == waiter.pid
  assert probe.rows_of(waiter.pid) == [True]
  cleanup([waiter])
}

fn freeze_claim(reports: Subject(Report)) -> Holder {
  let assert Ok(server) = probe.server_pid()
  push_crash.suspend(server)
  let waiter = probe.spawn_holder(permit.SettingsFetch, 3000, reports)
  process.sleep(50)
  process.kill(server)
  let assert True = restarted(server)
  waiter
}

type TableInfo {
  Owner
}

@external(erlang, "ets", "info")
fn ets_owner(table: Atom, item: TableInfo) -> Pid

fn keeper() -> Result(Pid, Nil) {
  process.named(keeper_name("fluxer_gateway_push_permit_keeper"))
}

@external(erlang, "erlang", "binary_to_atom")
fn keeper_name(name: String) -> process.Name(a)

pub fn a_keeper_restart_brings_back_both_tables_test() {
  let reports = process.new_subject()
  let holder = probe.spawn_holder(permit.SettingsFetch, 1000, reports)
  let assert Ok(Acquired(_, Ok(Nil))) = process.receive(reports, 1000)
  let assert Ok(old_keeper) = keeper()
  let assert Ok(old_server) = probe.server_pid()
  process.kill(old_keeper)
  let assert True = restarted(old_server)
  let assert Ok(new_keeper) = keeper()
  assert new_keeper != old_keeper
  let registry = atom.create(permit.profiles_registry)
  assert ets_owner(registry, Owner) == new_keeper
  process.send(holder.commands, Release)
  let assert Ok(Released(_)) = process.receive(reports, 1000)
  let fresh = fill(reports)
  let extra = probe.spawn_holder(permit.SettingsFetch, 5000, reports)
  let assert True = probe.eventually(fn() { queued(extra) }, 100)
  assert probe.held_count() == capacity
  let assert Ok(server) = probe.server_pid()
  assert process.is_alive(server)
  cleanup([holder, extra, ..fresh])
}
