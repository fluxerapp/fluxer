// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
import fluxer_gateway/session/join_budget
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list
import gleeunit/should
import support/isolation
import support/session_fake_guild as fake_guild
import support/session_harness as h

fn holders(guild: Int, count: Int) -> List(Pid) {
  list.repeat(Nil, count)
  |> list.map(fn(_) {
    let pid = process.spawn_unlinked(fn() { process.sleep(30_000) })
    join_budget.try_send(guild, pid, h.now_ms())
    |> should.equal(join_budget.Send)
    pid
  })
}

fn release(pids: List(Pid)) -> Nil {
  list.each(pids, fn(pid) {
    join_budget.release(pid)
    process.kill(pid)
  })
}

fn probe(guild: Int) -> join_budget.Admit {
  let pid = process.spawn_unlinked(fn() { process.sleep(1000) })
  let admit = join_budget.try_send(guild, pid, h.now_ms())
  join_budget.release(pid)
  admit
}

fn is_over(admit: join_budget.Admit) -> Bool {
  case admit {
    join_budget.OverQuota(_) -> True
    join_budget.Send -> False
  }
}

pub fn full_guild_defers_join_until_quota_frees_test() {
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Accept)
  let others = holders(guild, 32)
  is_over(probe(guild)) |> should.be_true
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), False, [guild])),
  )
  let socket = h.fake_socket()
  let assert Ok(ref) = h.start_session(h.identify("tok-q1"), socket)
  fake_guild.joins(fake, 300) |> should.equal(Error(Nil))
  release(others)
  let assert Ok(_) = fake_guild.joins(fake, 3000)
  h.await_event(socket, "READY", 8000) |> should.be_ok
  is_over(probe(guild)) |> should.be_false
  h.terminate(ref)
  fake_guild.crash(fake)
}

fn holds_quota_until(stop: fn(h.FakeSocket, _) -> Nil) -> Nil {
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Hold)
  let others = holders(guild, 31)
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), False, [guild])),
  )
  let socket = h.fake_socket()
  let assert Ok(ref) = h.start_session(h.identify("tok-q2"), socket)
  let assert Ok(_) = fake_guild.joins(fake, 2000)
  is_over(probe(guild)) |> should.be_true
  let pid = h.session_pid(ref)
  stop(socket, ref)
  h.await_exit(pid, 2000) |> should.be_true
  h.wait_until(fn() { !is_over(probe(guild)) }, 2000) |> should.be_true
  release(others)
  fake_guild.crash(fake)
}

pub fn terminated_session_returns_its_join_slot_test() {
  holds_quota_until(fn(_, ref) { h.terminate(ref) })
}

pub fn crashed_session_returns_its_join_slot_test() {
  holds_quota_until(fn(_, ref) { process.kill(h.session_pid(ref)) })
}

pub fn left_guild_returns_its_join_slot_test() {
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Hold)
  let others = holders(guild, 31)
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), False, [guild])),
  )
  let socket = h.fake_socket()
  let assert Ok(ref) = h.start_session(h.identify("tok-q3"), socket)
  let assert Ok(_) = fake_guild.joins(fake, 2000)
  is_over(probe(guild)) |> should.be_true
  h.control(ref, h.leave(guild))
  h.wait_until(fn() { !is_over(probe(guild)) }, 2000) |> should.be_true
  release(others)
  h.terminate(ref)
  fake_guild.crash(fake)
}

fn last_slot(guild: Int, rejects: Int) -> Int {
  let pid = process.spawn_unlinked(fn() { process.sleep(1000) })
  list.repeat(Nil, rejects)
  |> list.fold(0, fn(_, _) {
    case join_budget.try_send(guild, pid, h.now_ms()) {
      join_budget.OverQuota(retry_after_ms) -> retry_after_ms
      join_budget.Send -> -1
    }
  })
}

pub fn burst_over_quota_is_paced_well_inside_the_retry_cap_test() {
  let guild = h.unique_user()
  let others = holders(guild, 32)
  let slot = last_slot(guild, 300)
  release(others)
  assert slot > 0
  assert slot <= 1000
}

pub fn measured_serve_rate_shortens_the_pacing_test() {
  let guild = h.unique_user()
  let pid = process.spawn_unlinked(fn() { process.sleep(30_000) })
  list.repeat(Nil, 5000)
  |> list.each(fn(_) {
    join_budget.try_send(guild, pid, h.now_ms())
    |> should.equal(join_budget.Send)
    join_budget.settle(guild, pid)
  })
  process.sleep(1100)
  let others = holders(guild, 32)
  let slot = last_slot(guild, 300)
  release(others)
  process.kill(pid)
  assert slot > 0
  assert slot <= 400
}

type Tid

@external(erlang, "ets", "whereis")
fn ets_whereis(name: Atom) -> Tid

@external(erlang, "erlang", "is_reference")
fn is_table(tid: Tid) -> Bool

@external(erlang, "ets", "member")
fn ets_member(table: Atom, key: key) -> Bool

fn rejected(guild: Int, owner: Pid) -> Bool {
  let queued = atom.create("fluxer_gateway_session_join_queued")
  let rejects = atom.create("fluxer_gateway_session_join_rejects")
  case is_table(ets_whereis(queued)), is_table(ets_whereis(rejects)) {
    True, _ -> ets_member(queued, #(owner, guild))
    False, True -> ets_member(rejects, guild)
    False, False -> False
  }
}

pub fn freed_slot_wakes_the_queued_join_at_once_test() {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      session: settings.SessionDurations(
        ..d.session,
        join_budget_retry_min_ms: 10_000,
        join_budget_retry_max_ms: 15_000,
      ),
    )
  })
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Accept)
  let others = holders(guild, 32)
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), False, [guild])),
  )
  let socket = h.fake_socket()
  let assert Ok(ref) = h.start_session(h.identify("tok-q4"), socket)
  let pid = h.session_pid(ref)
  h.wait_until(fn() { rejected(guild, pid) }, 5000) |> should.be_true
  fake_guild.joins(fake, 0) |> should.equal(Error(Nil))
  let assert [first, ..rest] = others
  join_budget.settle(guild, first)
  let assert Ok(_) = fake_guild.joins(fake, 3000)
  release([first, ..rest])
  h.terminate(ref)
  fake_guild.crash(fake)
}

pub fn queued_waiter_is_woken_when_a_holder_settles_test() {
  let guild = h.unique_user()
  let others = holders(guild, 32)
  let woken = process.new_subject()
  let waiter = process.spawn_unlinked(fn() { process.sleep(5000) })
  join_budget.try_send(guild, waiter, h.now_ms()) |> is_over |> should.be_true
  join_budget.wait(guild, waiter, fn() { process.send(woken, Nil) })
  join_budget.wait(guild, waiter, fn() { process.send(woken, Nil) })
  process.receive(woken, 50) |> should.equal(Error(Nil))
  let assert [first, ..] = others
  join_budget.settle(guild, first)
  process.receive(woken, 100) |> should.equal(Ok(Nil))
  process.receive(woken, 100) |> should.equal(Error(Nil))
  release(others)
  process.kill(waiter)
}

pub fn a_waiter_woken_and_refused_again_keeps_its_place_test() {
  let guild = h.unique_user()
  let others = holders(guild, 32)
  let woken = process.new_subject()
  let early = process.spawn_unlinked(fn() { process.sleep(5000) })
  let late = process.spawn_unlinked(fn() { process.sleep(5000) })
  join_budget.wait(guild, early, fn() { process.send(woken, early) })
  join_budget.wait(guild, late, fn() { process.send(woken, late) })
  let assert [first, second, ..] = others
  join_budget.settle(guild, first)
  process.receive(woken, 100) |> should.equal(Ok(early))
  let assert join_budget.Send = join_budget.try_send(guild, first, h.now_ms())
  join_budget.try_send(guild, early, h.now_ms()) |> is_over |> should.be_true
  join_budget.wait(guild, early, fn() { process.send(woken, early) })
  join_budget.settle(guild, second)
  process.receive(woken, 100) |> should.equal(Ok(early))
  release(others)
  process.kill(early)
  process.kill(late)
}
