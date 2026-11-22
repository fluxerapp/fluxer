// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/permit
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list
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

fn queued(holder: Holder) -> Bool {
  probe.rows_of(holder.pid) == [False]
}

fn holding(holder: Holder) -> Bool {
  probe.rows_of(holder.pid) == [True]
}

fn gone(pid: Pid) -> Bool {
  probe.rows_of(pid) == []
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

pub fn queued_claims_are_granted_in_order_as_slots_free_test() {
  let reports = process.new_subject()
  let holders = fill(reports)
  let first = probe.spawn_holder(permit.SettingsFetch, 5000, reports)
  let assert True = probe.eventually(fn() { queued(first) }, 100)
  let second = probe.spawn_holder(permit.SettingsFetch, 5000, reports)
  let assert True = probe.eventually(fn() { queued(second) }, 100)
  let assert [releasing, dying, ..] = holders
  process.send(releasing.commands, Release)
  let assert Ok(Released(_)) = process.receive(reports, 1000)
  let assert Ok(Acquired(granted, Ok(Nil))) = process.receive(reports, 1000)
  assert granted == first.pid
  assert queued(second)
  process.kill(dying.pid)
  let assert Ok(Acquired(granted, Ok(Nil))) = process.receive(reports, 1000)
  assert granted == second.pid
  assert gone(dying.pid)
  cleanup([first, second, ..holders])
}

pub fn a_waiter_that_gave_up_leaves_no_claim_behind_test() {
  let reports = process.new_subject()
  let holders = fill(reports)
  let quitter = probe.spawn_holder(permit.SettingsFetch, 100, reports)
  let assert Ok(Acquired(pid, Error(permit.PermitTimeout))) =
    process.receive(reports, 1000)
  assert pid == quitter.pid
  let assert True = probe.eventually(fn() { gone(quitter.pid) }, 100)
  let assert [releasing, ..] = holders
  process.send(releasing.commands, Release)
  let assert Ok(Released(_)) = process.receive(reports, 1000)
  process.sleep(50)
  assert gone(quitter.pid)
  assert probe.held_count() == capacity - 1
  cleanup([quitter, ..holders])
}

pub fn a_zero_wait_claim_that_times_out_holds_nothing_test() {
  let reports = process.new_subject()
  let caller = probe.spawn_holder(permit.SettingsFetch, 0, reports)
  let assert Ok(Acquired(_, result)) = process.receive(reports, 1000)
  case result {
    Ok(Nil) -> {
      process.send(caller.commands, Release)
      let assert Ok(Released(_)) = process.receive(reports, 1000)
      Nil
    }
    Error(_) -> Nil
  }
  process.sleep(50)
  assert gone(caller.pid)
  assert process.is_alive(caller.pid)
  cleanup([caller])
}

pub fn permits_survive_a_server_restart_test() {
  let reports = process.new_subject()
  let holders = fill(reports)
  let waiter = probe.spawn_holder(permit.SettingsFetch, 10_000, reports)
  let assert True = probe.eventually(fn() { queued(waiter) }, 100)
  let assert Ok(before) = probe.server_pid()
  let assert [releasing, dying, lost, ..] = holders
  process.kill(before)
  process.kill(lost.pid)
  let assert True =
    probe.eventually(
      fn() {
        case probe.server_pid() {
          Ok(after) -> after != before
          Error(Nil) -> False
        }
      },
      300,
    )
  let assert True = probe.eventually(fn() { gone(lost.pid) }, 100)
  let assert Ok(Acquired(granted, Ok(Nil))) = process.receive(reports, 1000)
  assert granted == waiter.pid
  assert holding(waiter)
  assert holding(releasing)
  process.send(releasing.commands, Release)
  let assert Ok(Released(_)) = process.receive(reports, 1000)
  let assert True = probe.eventually(fn() { gone(releasing.pid) }, 100)
  assert holding(dying)
  let fresh = probe.spawn_holder(permit.SettingsFetch, 1000, reports)
  let assert Ok(Acquired(pid, Ok(Nil))) = process.receive(reports, 1000)
  assert pid == fresh.pid
  let late = probe.spawn_holder(permit.SettingsFetch, 5000, reports)
  let assert True = probe.eventually(fn() { queued(late) }, 100)
  process.kill(dying.pid)
  let assert Ok(Acquired(pid, Ok(Nil))) = process.receive(reports, 1000)
  assert pid == late.pid
  cleanup([waiter, fresh, late, ..holders])
}
