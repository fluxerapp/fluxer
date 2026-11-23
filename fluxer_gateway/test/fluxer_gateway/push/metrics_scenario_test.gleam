// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
import fluxer_gateway/push/job
import fluxer_gateway/push/metrics as push_metrics
import fluxer_gateway/push/outbox
import fluxer_gateway/push/outbox/state
import fluxer_gateway/push/rule
import fluxer_gateway/push/server
import fluxer_gateway/telemetry/metrics
import gleam/erlang/process
import gleam/list
import gleam/result
import support/push_job_fixture as fixture
import support/wait

fn value(name: String, labels: List(#(String, String))) -> Int {
  metrics.value(metrics.read_all(), name, labels)
}

fn timed(name: String, labels: List(#(String, String))) -> Int {
  metrics.histograms()
  |> list.find(fn(series) { series.name == name && series.labels == labels })
  |> result.map(fn(series) { series.count })
  |> result.unwrap(0)
}

fn message_job(user: Int) -> job.Job {
  let assert Ok(built) =
    job.for_users(job.template([], channel: 78, message: 9302), rule.Direct, [
      user,
    ])
  built
}

fn deal(
  inbox: process.Subject(process.Subject(state.SendResult)),
  answers: List(state.SendResult),
) -> Nil {
  let reply = process.receive_forever(inbox)
  case answers {
    [first, ..rest] -> {
      process.send(reply, first)
      deal(inbox, rest)
    }
    [] -> {
      process.send(reply, state.Delivered)
      deal(inbox, [])
    }
  }
}

const results = "fluxer_gateway_push_outbox_attempt_results_total"

const attempt_duration = "fluxer_gateway_push_outbox_attempt_duration_ms"

pub fn outbox_attempts_are_counted_and_timed_by_kind_and_result_test() {
  let name = process.new_name("push_outbox_metrics")
  let ready = process.new_subject()
  let dealer =
    process.spawn_unlinked(fn() {
      let inbox = process.new_subject()
      process.send(ready, inbox)
      deal(inbox, [state.Failed("rejected:quota")])
    })
  let assert Ok(inbox) = process.receive(ready, 1000)
  let sender = fn(_subject, _body, _timeout) {
    process.call(inbox, 1000, fn(reply) { reply })
  }
  let assert Ok(started) = outbox.start(name, settings.get(), sender)
  process.unlink(started.pid)
  let delivered = [#("kind", "message"), #("result", "delivered")]
  let rejected = [#("kind", "message"), #("result", "rejected")]
  let before_delivered = value(results, delivered)
  let before_rejected = value(results, rejected)
  let before_timed = timed(attempt_duration, [#("kind", "message")])
  let assert Ok(Nil) = outbox.enqueue_to(name, message_job(7101))
  assert wait.until(5000, fn() { value(results, delivered) > before_delivered })
  assert value(results, rejected) == before_rejected + 1
  assert timed(attempt_duration, [#("kind", "message")]) >= before_timed + 2
  process.kill(started.pid)
  process.kill(dealer)
}

pub fn attempt_results_fold_into_a_closed_set_test() {
  assert push_metrics.attempt_result(state.Failed("rejected:anything"))
    == "rejected"
  assert push_metrics.attempt_result(state.Failed("timeout")) == "timeout"
  assert push_metrics.attempt_result(state.Failed("1234")) == "other"
  assert push_metrics.attempt_result(state.InFlightElsewhere)
    == "in_flight_elsewhere"
  assert job.subject_label(job.subject(job.RingJob)) == "ring"
  assert job.subject_label("push.job.unknown") == "other"
}

pub fn push_workers_are_timed_from_queue_to_finish_test() {
  let name = process.new_name("push_server_metrics")
  let done = process.new_subject()
  let assert Ok(started) =
    server.start(name, fn(_create, _may_publish) {
      process.sleep(20)
      process.send(done, Nil)
    })
  process.unlink(started.pid)
  let waited = timed("fluxer_gateway_push_job_wait_ms", [])
  let finished = timed("fluxer_gateway_push_job_duration_ms", [])
  server.requeue_local(name, [
    server.Waiting(age_ms: 30, create: fixture.guild_message()),
    server.Waiting(age_ms: 5, create: fixture.dm_message()),
  ])
  let assert Ok(Nil) = process.receive(done, 2000)
  let assert Ok(Nil) = process.receive(done, 2000)
  assert wait.until(2000, fn() {
    timed("fluxer_gateway_push_job_duration_ms", []) == finished + 2
  })
  assert timed("fluxer_gateway_push_job_wait_ms", []) == waited + 2
  assert value("fluxer_gateway_push_pending_jobs", []) == 0
  process.kill(started.pid)
}

pub fn message_creates_reaching_the_push_server_are_counted_test() {
  let kind = [#("kind", "message_create")]
  let before = value("fluxer_gateway_push_server_received_total", kind)
  server.handle_message_create(fixture.dm_message())
  assert wait.until(2000, fn() {
    value("fluxer_gateway_push_server_received_total", kind) == before + 1
  })
}
