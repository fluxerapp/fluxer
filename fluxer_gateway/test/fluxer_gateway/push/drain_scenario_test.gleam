// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/draining
import fluxer_gateway/config/settings.{
  type PushDurations, Durations, PushDurations,
}
import fluxer_gateway/core/clock
import fluxer_gateway/core/node
import fluxer_gateway/push/drain.{Peer} as push_drain
import fluxer_gateway/push/handoff/adapter as push_adapter
import fluxer_gateway/push/job
import fluxer_gateway/push/message.{type MessageCreate, MessageCreate}
import fluxer_gateway/push/metrics as push_metrics
import fluxer_gateway/push/outbox
import fluxer_gateway/push/outbox/state
import fluxer_gateway/push/rule
import fluxer_gateway/push/server
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/rollout_metrics
import gleam/bit_array
import gleam/erlang/process.{type Name, type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/otp/static_supervisor as supervisor
import gleam/otp/supervision
import gleam/string
import support/push_job_fixture as fixture
import support/wait

const channel = 9_420_001

type Sent {
  Sent(user: Int, answer: Subject(state.SendResult))
}

type Fetch {
  Fetching(user: Int)
  Gated(user: Int, open: Bool, resume: Subject(Nil))
}

type Side {
  Side(site: push_adapter.Site, sent: Subject(Sent), supervisor: Pid)
}

type Behaviour {
  Publish
  Hang(Subject(Fetch))
  Gate(Subject(Fetch))
}

fn timings() -> Nil {
  let current = settings.get()
  settings.store(
    settings.Settings(
      ..current,
      push_outbox_request_timeout_ms: 60_000,
      durations: Durations(
        ..current.durations,
        push: PushDurations(
          ..current.durations.push,
          outbox_state_drain_window_ms: 300,
          outbox_state_retry_base_ms: 2000,
          outbox_state_retry_max_ms: 2000,
          server_pending_drain_interval_ms: 10,
        ),
      ),
    ),
  )
  Nil
}

fn user_in(body: String) -> Int {
  case
    list.find([1, 2, 3, 4, 5, 6, 7, 8, 9], fn(user) {
      string.contains(body, "\"" <> int.to_string(user) <> "\"")
    })
  {
    Ok(user) -> user
    Error(Nil) -> 0
  }
}

fn sender(sent: Subject(Sent)) -> outbox.Sender {
  fn(_subject, bytes, _timeout) {
    let assert Ok(body) = bit_array.to_string(bytes)
    let answer = process.new_subject()
    process.send(sent, Sent(user_in(body), answer))
    case process.receive(answer, 20_000) {
      Ok(result) -> result
      Error(Nil) -> state.Failed("test_gave_up")
    }
  }
}

fn job_for(user: Int) -> job.Job {
  class_job_for(user, rule.Direct)
}

fn class_job_for(user: Int, class: rule.Class) -> job.Job {
  let assert Ok(built) =
    job.for_users(job.template([], channel:, message: user + 1000), class, [
      user,
    ])
  built
}

fn work(at: Name(outbox.Message), behaviour: Behaviour) -> server.Work {
  fn(create: MessageCreate, may_publish: fn() -> Bool) {
    let user = user_of(create)
    case behaviour {
      Publish -> publish(at, user, may_publish())
      Hang(events) -> {
        process.send(events, Fetching(user))
        process.sleep_forever()
      }
      Gate(events) -> {
        let open = may_publish()
        let resume = process.new_subject()
        process.send(events, Gated(user, open, resume))
        let _ = process.receive(resume, 20_000)
        publish(at, user, open)
      }
    }
  }
}

fn publish(at: Name(outbox.Message), user: Int, open: Bool) -> Nil {
  case open {
    True -> {
      let _ = outbox.enqueue_to(at, job_for(user))
      Nil
    }
    False -> Nil
  }
}

fn user_of(create: MessageCreate) -> Int {
  case create.recipients {
    [user, ..] -> user
    [] -> 0
  }
}

fn start_side(prefix: String, behaviour: Behaviour) -> Side {
  let sent = process.new_subject()
  let outbox_name: Name(outbox.Message) = process.new_name(prefix)
  let server_name: Name(server.Message) = process.new_name(prefix <> "_server")
  let assert Ok(started) =
    supervisor.new(supervisor.OneForOne)
    |> supervisor.add(
      supervision.worker(fn() {
        outbox.start(outbox_name, settings.get(), sender(sent))
      }),
    )
    |> supervisor.add(
      supervision.worker(fn() {
        server.start(server_name, work(outbox_name, behaviour))
      }),
    )
    |> supervisor.start
  process.unlink(started.pid)
  Side(
    site: push_adapter.Site(outbox: outbox_name, server: server_name),
    sent:,
    supervisor: started.pid,
  )
}

fn stop(side: Side) -> Nil {
  process.kill(side.supervisor)
}

fn peer(side: Side) -> fn() -> Result(push_drain.Peer, Nil) {
  fn() { Ok(Peer(node: node.self(), site: side.site)) }
}

fn always() -> Bool {
  True
}

fn create_for(user: Int) -> server.Waiting {
  server.Waiting(
    age_ms: 0,
    create: MessageCreate(
      ..fixture.guild_message(),
      recipients: [user],
      channel:,
      message: user + 2000,
    ),
  )
}

fn fetching(events: Subject(Fetch), count: Int) -> List(Int) {
  list.repeat(Nil, count)
  |> list.map(fn(_) {
    let assert Ok(Fetching(user)) = process.receive(events, 10_000)
    user
  })
}

fn next_send(side: Side) -> Sent {
  let assert Ok(found) = process.receive(side.sent, 10_000)
  found
}

fn quiet(side: Side, ms: Int) -> Bool {
  process.receive(side.sent, ms) == Error(Nil)
}

fn deliver(side: Side, count: Int) -> List(Int) {
  list.repeat(Nil, count)
  |> list.map(fn(_) {
    let found = next_send(side)
    process.send(found.answer, state.Delivered)
    found.user
  })
  |> list.sort(int.compare)
}

fn drained(outcome: String) -> Int {
  metrics.value(metrics.read_all(), rollout_metrics.push_jobs_drain, [
    #("outcome", outcome),
  ])
}

fn retried() -> Int {
  metrics.value(metrics.read_all(), "fluxer_gateway_push_outbox_retries_total", [
    #("class", "direct"),
  ])
}

fn fail(found: Sent) -> Nil {
  let before = retried()
  process.send(found.answer, state.Failed("busy"))
  let assert True = wait.until(5000, fn() { retried() > before })
  Nil
}

pub fn a_drain_hands_every_in_flight_job_to_the_live_owner_test() {
  timings()
  draining.set_draining()
  let events = process.new_subject()
  let source = start_side("push_drain_live_source", Hang(events))
  let target = start_side("push_drain_live_target", Publish)
  let assert Ok(Nil) = outbox.enqueue_to(source.site.outbox, job_for(1))
  fail(next_send(source))
  let assert Ok(Nil) = outbox.enqueue_to(source.site.outbox, job_for(2))
  let assert Ok(Nil) = outbox.enqueue_to(source.site.outbox, job_for(3))
  let in_flight = [next_send(source), next_send(source)]
  assert list.sort(list.map(in_flight, fn(found) { found.user }), int.compare)
    == [2, 3]
  server.requeue_local(source.site.server, list.map([4, 5, 6], create_for))
  assert list.sort(fetching(events, 3), int.compare) == [4, 5, 6]
  let moved = drained("moved")
  let lost = drained("lost")
  let outcome =
    push_drain.run(
      source.site,
      peer(target),
      always,
      clock.monotonic_ms() + 10_000,
      50,
    )
  assert outcome == push_drain.Drained
  assert push_drain.is_idle(source.site)
  assert deliver(target, 6) == [1, 2, 3, 4, 5, 6]
  assert quiet(target, 300)
  assert quiet(source, 0)
  assert push_drain.is_idle(target.site)
  assert drained("moved") - moved == 6
  assert drained("lost") == lost
  stop(source)
  stop(target)
}

pub fn a_worker_past_its_publish_gate_finishes_once_through_the_owner_test() {
  timings()
  draining.set_draining()
  let events = process.new_subject()
  let source = start_side("push_drain_gate_source", Gate(events))
  let target = start_side("push_drain_gate_target", Publish)
  server.requeue_local(source.site.server, [create_for(7)])
  let assert Ok(Gated(user: 7, open: True, resume:)) =
    process.receive(events, 10_000)
  assert push_drain.hand(source.site, Peer(node.self(), target.site)) == 0
  process.send(resume, Nil)
  assert deliver(target, 1) == [7]
  assert quiet(target, 300)
  assert quiet(source, 0)
  assert wait.until(5000, fn() { push_drain.is_idle(source.site) })
  stop(source)
  stop(target)
}

pub fn a_drain_into_a_draining_owner_reaches_the_next_live_one_test() {
  timings()
  draining.set_draining()
  let events = process.new_subject()
  let source = start_side("push_drain_chain_source", Hang(events))
  let middle = start_side("push_drain_chain_middle", Publish)
  let last = start_side("push_drain_chain_last", Publish)
  let assert push_drain.Drained =
    push_drain.run(
      middle.site,
      peer(last),
      always,
      clock.monotonic_ms() + 1000,
      50,
    )
  let assert Ok(Nil) = outbox.enqueue_to(source.site.outbox, job_for(1))
  fail(next_send(source))
  let assert Ok(Nil) = outbox.enqueue_to(source.site.outbox, job_for(2))
  let held = next_send(source)
  server.requeue_local(source.site.server, list.map([3, 4], create_for))
  assert list.sort(fetching(events, 2), int.compare) == [3, 4]
  let lost = drained("lost")
  let outcome =
    push_drain.run(
      source.site,
      peer(middle),
      always,
      clock.monotonic_ms() + 10_000,
      50,
    )
  assert outcome == push_drain.Drained
  assert deliver(last, 4) == [1, 2, 3, 4]
  assert quiet(last, 300)
  assert quiet(middle, 0)
  assert quiet(source, 0)
  assert push_drain.is_idle(middle.site)
  assert drained("lost") == lost
  process.send(held.answer, state.Delivered)
  stop(source)
  stop(middle)
  stop(last)
}

pub fn a_drain_with_no_live_owner_fails_at_its_deadline_test() {
  timings()
  let events = process.new_subject()
  let source = start_side("push_drain_alone_source", Hang(events))
  let assert Ok(Nil) = outbox.enqueue_to(source.site.outbox, job_for(1))
  let held = next_send(source)
  server.requeue_local(source.site.server, [create_for(2)])
  assert fetching(events, 1) == [2]
  draining.set_draining()
  push_metrics.open_drain()
  let lost = drained("lost")
  let finished = drained("finished")
  let started = clock.monotonic_ms()
  let outcome =
    push_drain.run(source.site, fn() { Error(Nil) }, always, started + 500, 50)
  let took = clock.monotonic_ms() - started
  assert outcome == push_drain.Failed(left: 2)
  assert took >= 500
  assert took < 2000
  assert drained("lost") - lost == 2
  assert push_drain.close(source.site, fn() { Error(Nil) }, always, 50)
    == push_drain.Failed(left: 2)
  process.send(held.answer, state.Delivered)
  assert wait.until(5000, fn() { push_drain.left(source.site) == 1 })
  assert drained("lost") - lost == 2
  assert drained("finished") == finished
  stop(source)
}

pub fn a_hand_over_to_a_missing_owner_keeps_the_jobs_here_test() {
  timings()
  let source = start_side("push_drain_missing_source", Publish)
  let assert Ok(Nil) = outbox.enqueue_to(source.site.outbox, job_for(1))
  fail(next_send(source))
  let lost = drained("lost")
  let missing: Name(outbox.Message) = process.new_name("push_drain_missing")
  outbox.hand_all(source.site.outbox, outbox.target_at(node.self(), missing))
  assert deliver(source, 1) == [1]
  assert quiet(source, 300)
  assert drained("lost") == lost
  assert wait.until(5000, fn() { push_drain.is_idle(source.site) })
  stop(source)
}

pub fn an_undrained_outbox_runs_new_jobs_here_until_it_drains_again_test() {
  timings()
  let source = start_side("push_undrain_source", Publish)
  let target = start_side("push_undrain_target", Publish)
  let to = Peer(node.self(), target.site)
  draining.set_draining()
  assert push_drain.hand(source.site, to) == 0
  let assert Ok(Nil) = outbox.enqueue_to(source.site.outbox, job_for(1))
  assert deliver(target, 1) == [1]
  draining.clear_draining()
  let assert Ok(Nil) = outbox.enqueue_to(source.site.outbox, job_for(2))
  fail(next_send(source))
  assert quiet(target, 300)
  draining.set_draining()
  assert push_drain.hand(source.site, to) == 0
  assert deliver(target, 1) == [2]
  let assert Ok(Nil) = outbox.enqueue_to(source.site.outbox, job_for(3))
  assert deliver(target, 1) == [3]
  assert quiet(source, 300)
  assert quiet(target, 0)
  assert push_drain.is_idle(source.site)
  assert wait.until(5000, fn() { push_drain.is_idle(target.site) })
  stop(source)
  stop(target)
}

fn outbox_count(name: String) -> Int {
  metrics.value(metrics.read_all(), "fluxer_gateway_push_outbox_" <> name, [])
}

pub fn outbox_counters_show_where_jobs_run_across_an_undrain_test() {
  timings()
  let source = start_side("push_where_source", Publish)
  let target = start_side("push_where_target", Publish)
  let to = Peer(node.self(), target.site)
  draining.set_draining()
  assert push_drain.hand(source.site, to) == 0
  let handed = outbox_count("handed_total")
  let attempts = outbox_count("attempts_total")
  let assert Ok(Nil) = outbox.enqueue_to(source.site.outbox, job_for(1))
  assert deliver(target, 1) == [1]
  assert outbox_count("handed_total") == handed + 1
  assert outbox_count("attempts_total") == attempts + 1
  draining.clear_draining()
  let assert Ok(Nil) = outbox.enqueue_to(source.site.outbox, job_for(2))
  assert deliver(source, 1) == [2]
  assert quiet(target, 100)
  assert outbox_count("handed_total") == handed + 1
  assert outbox_count("attempts_total") == attempts + 2
  assert wait.until(5000, fn() { push_drain.is_idle(source.site) })
  assert wait.until(5000, fn() { push_drain.is_idle(target.site) })
  stop(source)
  stop(target)
}

fn with_push(change: fn(PushDurations) -> PushDurations) -> Nil {
  let current = settings.get()
  settings.store(
    settings.Settings(
      ..current,
      durations: Durations(
        ..current.durations,
        push: change(current.durations.push),
      ),
    ),
  )
  Nil
}

fn dropped(reason: String, class: String) -> Int {
  metrics.value(metrics.read_all(), "fluxer_gateway_push_outbox_dropped_total", [
    #("kind", "message"),
    #("reason", reason),
    #("class", class),
  ])
}

pub fn a_bulk_job_expiring_in_flight_is_not_a_drain_loss_test() {
  timings()
  with_push(fn(push) { PushDurations(..push, outbox_state_bulk_age_ms: 200) })
  draining.set_draining()
  push_metrics.open_drain()
  let source = start_side("push_drain_expiry_source", Publish)
  let target = start_side("push_drain_expiry_target", Publish)
  let lost = drained("lost")
  let expired = dropped("expired", "bulk")
  let assert Ok(Nil) =
    outbox.enqueue_to(source.site.outbox, class_job_for(1, rule.Bulk))
  let held = next_send(source)
  let outcome =
    push_drain.run(
      source.site,
      peer(target),
      always,
      clock.monotonic_ms() + 10_000,
      50,
    )
  assert outcome == push_drain.Drained
  assert push_drain.is_idle(source.site)
  assert quiet(target, 300)
  assert drained("lost") - lost == 0
  assert dropped("expired", "bulk") - expired == 1
  process.send(held.answer, state.Delivered)
  stop(source)
  stop(target)
}

pub fn a_gated_worker_at_the_deadline_is_handed_not_lost_test() {
  timings()
  draining.set_draining()
  push_metrics.open_drain()
  let events = process.new_subject()
  let source = start_side("push_drain_late_gate_source", Gate(events))
  let target = start_side("push_drain_late_gate_target", Publish)
  server.requeue_local(source.site.server, [create_for(8)])
  let assert Ok(Gated(user: 8, open: True, resume:)) =
    process.receive(events, 10_000)
  let lost = drained("lost")
  let deadline = clock.monotonic_ms() + 200
  process.spawn(fn() {
    process.sleep(500)
    process.send(resume, Nil)
  })
  let outcome = push_drain.run(source.site, peer(target), always, deadline, 50)
  assert clock.monotonic_ms() >= deadline
  assert outcome == push_drain.Drained
  assert deliver(target, 1) == [8]
  assert quiet(target, 300)
  assert quiet(source, 0)
  assert wait.until(5000, fn() { push_drain.is_idle(source.site) })
  assert drained("lost") - lost == 0
  stop(source)
  stop(target)
}

pub fn a_close_with_no_live_owner_counts_answered_jobs_at_once_test() {
  timings()
  with_push(fn(push) {
    PushDurations(
      ..push,
      outbox_state_drain_grace_ms: 30_000,
      server_init_timeout_ms: 200,
    )
  })
  let source = start_side("push_close_alone_source", Publish)
  let assert Ok(Nil) = outbox.enqueue_to(source.site.outbox, job_for(1))
  let held = next_send(source)
  let deaf = process.spawn_unlinked(process.sleep_forever)
  let silent: Name(server.Message) = process.new_name("push_close_deaf")
  let assert Ok(Nil) = process.register(deaf, silent)
  let site = push_adapter.Site(outbox: source.site.outbox, server: silent)
  draining.set_draining()
  push_metrics.open_drain()
  let lost = drained("lost")
  let started = clock.monotonic_ms()
  let outcome = push_drain.close(site, fn() { Error(Nil) }, always, 50)
  let took = clock.monotonic_ms() - started
  assert outcome == push_drain.Failed(left: 1)
  assert took < 5000
  assert drained("lost") - lost == 1
  assert push_drain.left(site) == 2
  process.send(held.answer, state.Delivered)
  process.kill(deaf)
  stop(source)
}
