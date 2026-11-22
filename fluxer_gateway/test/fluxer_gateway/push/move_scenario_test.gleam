// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/key.{Key}
import fluxer_gateway/config/draining
import fluxer_gateway/config/settings.{Durations, PushDurations}
import fluxer_gateway/core/node
import fluxer_gateway/handoff/adapter.{type Arrival, type Frozen}
import fluxer_gateway/push/handoff/adapter as push_adapter
import fluxer_gateway/push/job
import fluxer_gateway/push/outbox
import fluxer_gateway/push/outbox/state
import fluxer_gateway/push/rule
import fluxer_gateway/telemetry/metrics
import gleam/bit_array
import gleam/erlang/process.{type Name, type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/otp/static_supervisor as supervisor
import gleam/otp/supervision
import gleam/string
import support/isolation
import support/move_sink as sink
import support/wait

type Sent {
  Sent(body: String, answer: Subject(state.SendResult))
}

type Side {
  Side(site: push_adapter.Site, sent: Subject(Sent), supervisor: Pid)
}

const moving_channel = 9_410_001

fn other_channel() -> Int {
  find_other(moving_channel + 1)
}

fn find_other(candidate: Int) -> Int {
  case key.bucket(candidate) == key.bucket(moving_channel) {
    True -> find_other(candidate + 1)
    False -> candidate
  }
}

fn long_retries() -> Nil {
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      push: PushDurations(
        ..d.push,
        outbox_state_retry_base_ms: 60_000,
        outbox_state_retry_max_ms: 60_000,
      ),
    )
  })
}

fn sender(sent: Subject(Sent)) -> outbox.Sender {
  fn(_subject, bytes, _timeout) {
    let assert Ok(body) = bit_array.to_string(bytes)
    let answer = process.new_subject()
    process.send(sent, Sent(body, answer))
    case process.receive(answer, 20_000) {
      Ok(result) -> result
      Error(Nil) -> state.Failed("test_gave_up")
    }
  }
}

fn start_side(prefix: String) -> Side {
  let sent = process.new_subject()
  let name: Name(outbox.Message) = process.new_name(prefix)
  let assert Ok(started) =
    supervisor.new(supervisor.OneForOne)
    |> supervisor.add(
      supervision.worker(fn() {
        outbox.start(name, settings.get(), sender(sent))
      }),
    )
    |> supervisor.start
  process.unlink(started.pid)
  Side(
    site: push_adapter.Site(
      outbox: name,
      server: process.new_name(prefix <> "_server"),
    ),
    sent:,
    supervisor: started.pid,
  )
}

fn job_for(channel: Int, user: Int) -> job.Job {
  let assert Ok(built) =
    job.for_users(
      job.template([], channel:, message: user + 1000),
      rule.Direct,
      [
        user,
      ],
    )
  built
}

fn enqueue(side: Side, channel: Int, user: Int) -> Nil {
  let assert Ok(Nil) =
    outbox.enqueue_to(side.site.outbox, job_for(channel, user))
  Nil
}

fn user_of(found: Sent) -> Int {
  case
    list.find([1, 2, 3, 4, 5, 6], fn(user) {
      string.contains(found.body, "\"" <> int.to_string(user) <> "\"")
    })
  {
    Ok(user) -> user
    Error(Nil) -> 0
  }
}

fn retries() -> Int {
  metrics.value(metrics.read_all(), "fluxer_gateway_push_outbox_retries_total", [
    #("class", "direct"),
  ])
}

fn fail(found: Sent) -> Nil {
  let before = retries()
  process.send(found.answer, state.Failed("busy"))
  let assert True = wait.until(5000, fn() { retries() > before })
  Nil
}

fn next_send(side: Side) -> Sent {
  let assert Ok(found) = process.receive(side.sent, 10_000)
  found
}

fn quiet(side: Side, ms: Int) -> Bool {
  process.receive(side.sent, ms) == Error(Nil)
}

fn entity() -> key.Key {
  let bucket = key.bucket(moving_channel)
  let assert Ok(id) = push_adapter.entity_of(bucket)
  Key(key.push, id)
}

fn freeze(side: Side, serial: Int) -> Frozen {
  let assert Ok(frozen) =
    push_adapter.freeze(
      side.site,
      entity(),
      sink.move_id(serial),
      1,
      sink.at(5),
    )
  frozen
}

fn install(side: Side, frozen: Frozen) -> Arrival {
  let assert Ok(arrival) =
    push_adapter.install(
      side.site,
      entity(),
      sink.source(),
      sink.at(6),
      frozen.tail,
    )
  arrival
}

fn stop(side: Side) -> Nil {
  process.kill(side.supervisor)
}

pub fn canonical_entities_cover_every_bucket_once_test() {
  let ids =
    int.range(0, key.bucket_count, [], fn(acc, bucket) {
      let assert Ok(id) = push_adapter.entity_of(bucket)
      assert key.bucket(id) == bucket
      [id, ..acc]
    })
  assert list.length(list.unique(ids)) == key.bucket_count
  assert push_adapter.entity_of(key.bucket_count) == Error(Nil)
}

pub fn every_push_job_is_moved_or_finished_exactly_once_test() {
  long_retries()
  let source = start_side("push_move_source")
  let target = start_side("push_move_target")
  let moves = sink.start()
  let bucket = key.bucket(moving_channel)
  enqueue(source, moving_channel, 1)
  let first = next_send(source)
  assert user_of(first) == 1
  fail(first)
  enqueue(source, moving_channel, 2)
  let in_flight = next_send(source)
  assert user_of(in_flight) == 2
  enqueue(source, other_channel(), 3)
  let other = next_send(source)
  assert user_of(other) == 3
  fail(other)
  enqueue(source, moving_channel, 4)
  let failing = next_send(source)
  assert user_of(failing) == 4
  assert push_adapter.entities_in(source.site, bucket) == [entity().id]
  let frozen = freeze(source, 21)
  fail(failing)
  assert quiet(source, 300)
  let arrival = install(target, frozen)
  assert quiet(target, 300)
  let assert Ok(Nil) =
    push_adapter.committed(frozen.stub, arrival.target_ref, sink.at(6))
  let assert [] = sink.forwarded_until_sealed(moves)
  let assert Ok(Nil) = push_adapter.seal(arrival.entity, 0)
  let moved = next_send(target)
  assert user_of(moved) == 1
  process.send(moved.answer, state.Delivered)
  process.send(in_flight.answer, state.Delivered)
  enqueue(source, moving_channel, 5)
  let late = next_send(target)
  assert user_of(late) == 5
  process.send(late.answer, state.Delivered)
  assert quiet(source, 300)
  assert quiet(target, 300)
  assert outbox.busy_buckets(target.site.outbox) == Ok([bucket])
  assert !outbox.is_idle(target.site.outbox)
  assert outbox.busy_buckets(source.site.outbox)
    == Ok([key.bucket(other_channel())])
  assert push_adapter.entities_in(source.site, bucket) == []
  assert !process.is_alive(frozen.stub)
  assert !process.is_alive(arrival.entity)
  sink.stop(moves)
  stop(source)
  stop(target)
}

pub fn an_aborted_push_move_gives_every_job_back_to_the_source_test() {
  long_retries()
  let source = start_side("push_abort_source")
  let target = start_side("push_abort_target")
  let moves = sink.start()
  enqueue(source, moving_channel, 6)
  let first = next_send(source)
  fail(first)
  let frozen = freeze(source, 22)
  assert push_adapter.freeze(
      source.site,
      entity(),
      sink.move_id(23),
      1,
      sink.at(5),
    )
    == Error(adapter.EntityBusy(entity()))
  let arrival = install(target, frozen)
  push_adapter.abort_arrival(arrival.entity)
  let assert Ok(Nil) = push_adapter.thaw(frozen.stub, [])
  let again = next_send(source)
  assert user_of(again) == 6
  process.send(again.answer, state.Delivered)
  assert quiet(target, 300)
  assert quiet(source, 300)
  assert outbox.is_idle(source.site.outbox)
  assert outbox.is_idle(target.site.outbox)
  sink.stop(moves)
  stop(source)
  stop(target)
}

pub fn a_drained_outbox_hands_every_job_to_a_peer_test() {
  long_retries()
  draining.set_draining()
  let source = start_side("push_drain_source")
  let target = start_side("push_drain_target")
  enqueue(source, moving_channel, 1)
  let first = next_send(source)
  fail(first)
  enqueue(source, other_channel(), 2)
  let in_flight = next_send(source)
  outbox.hand_all(
    source.site.outbox,
    outbox.target_at(node.self(), target.site.outbox),
  )
  let moved = next_send(target)
  assert user_of(moved) == 1
  process.send(moved.answer, state.Delivered)
  enqueue(source, moving_channel, 3)
  let late = next_send(target)
  assert user_of(late) == 3
  process.send(late.answer, state.Delivered)
  assert !outbox.is_idle(source.site.outbox)
  fail(in_flight)
  assert quiet(source, 300)
  assert quiet(target, 300)
  assert outbox.is_idle(source.site.outbox)
  assert outbox.busy_buckets(target.site.outbox)
    == Ok([key.bucket(other_channel())])
  stop(source)
  stop(target)
}

@external(erlang, "erlang", "suspend_process")
fn suspend_process(pid: Pid) -> Bool

@external(erlang, "erlang", "resume_process")
fn resume_process(pid: Pid) -> Bool

pub fn a_fenced_push_move_gives_its_jobs_back_and_keeps_the_bucket_open_test() {
  long_retries()
  let source = start_side("push_fence_source")
  enqueue(source, moving_channel, 1)
  let first = next_send(source)
  fail(first)
  let frozen = freeze(source, 24)
  enqueue(source, moving_channel, 2)
  assert quiet(source, 300)
  push_adapter.abort_arrival(frozen.stub)
  let returned = [next_send(source), next_send(source)]
  assert list.sort(list.map(returned, user_of), int.compare) == [1, 2]
  list.each(returned, fn(found) { process.send(found.answer, state.Delivered) })
  enqueue(source, moving_channel, 3)
  let later = next_send(source)
  assert user_of(later) == 3
  process.send(later.answer, state.Delivered)
  stop(source)
}

pub fn a_slow_outbox_keeps_its_jobs_and_never_reads_as_idle_test() {
  long_retries()
  let source = start_side("push_slow_source")
  let bucket = key.bucket(moving_channel)
  enqueue(source, moving_channel, 4)
  let first = next_send(source)
  fail(first)
  let assert Ok(pid) = process.named(source.site.outbox)
  assert suspend_process(pid)
  assert !outbox.is_idle(source.site.outbox)
  assert outbox.export_bucket(source.site.outbox, bucket) == Error(Nil)
  assert resume_process(pid)
  assert outbox.busy_buckets(source.site.outbox) == Ok([bucket])
  let frozen = freeze(source, 25)
  let assert Ok(Nil) = push_adapter.thaw(frozen.stub, [])
  let again = next_send(source)
  assert user_of(again) == 4
  process.send(again.answer, state.Delivered)
  stop(source)
}
