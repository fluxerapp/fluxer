// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
import fluxer_gateway/push/job
import fluxer_gateway/push/outbox
import fluxer_gateway/push/outbox/state
import fluxer_gateway/push/rule
import fluxer_gateway/telemetry/metrics
import gleam/bit_array
import gleam/erlang/process.{type Name, type Pid, type Subject}
import gleam/otp/static_supervisor as supervisor
import gleam/otp/supervision
import gleam/string
import support/push_permit_probe as probe

type Sent {
  Sent(body: String, answer: Subject(state.SendResult))
}

fn sender(sent: Subject(Sent)) -> outbox.Sender {
  fn(_subject, bytes, _timeout) {
    let assert Ok(body) = bit_array.to_string(bytes)
    let answer = process.new_subject()
    process.send(sent, Sent(body, answer))
    case process.receive(answer, 10_000) {
      Ok(result) -> result
      Error(Nil) -> state.Failed("test_gave_up")
    }
  }
}

fn start(sent: Subject(Sent)) -> #(Name(outbox.Message), Pid) {
  let name = process.new_name("push_outbox_scenario")
  let assert Ok(started) =
    supervisor.new(supervisor.OneForOne)
    |> supervisor.add(
      supervision.worker(fn() {
        outbox.start(name, settings.get(), sender(sent))
      }),
    )
    |> supervisor.start
  process.unlink(started.pid)
  #(name, started.pid)
}

fn message_job(user: Int, message: Int) -> job.Job {
  let assert Ok(built) =
    job.for_users(job.template([], channel: 77, message:), rule.Direct, [
      user,
    ])
  built
}

fn counter(name: String) -> Int {
  metrics.value(metrics.read_all(), "fluxer_gateway_push_outbox_" <> name, [
    #("class", "direct"),
  ])
}

fn next_send(sent: Subject(Sent), user: Int) -> Sent {
  let assert Ok(found) = process.receive(sent, 3000)
  assert string.contains(found.body, "\"" <> int_text(user) <> "\"")
  found
}

fn int_text(value: Int) -> String {
  string.inspect(value)
}

pub fn a_reply_from_before_a_restart_does_not_finish_a_new_attempt_test() {
  let sent = process.new_subject()
  let #(name, sup) = start(sent)
  let assert Ok(Nil) = outbox.enqueue_to(name, message_job(501, 9001))
  let old = next_send(sent, 501)
  let assert Ok(before) = process.named(name)
  process.kill(before)
  let assert True =
    probe.eventually(
      fn() {
        case process.named(name) {
          Ok(after) -> after != before
          Error(Nil) -> False
        }
      },
      300,
    )
  let assert Ok(Nil) = outbox.enqueue_to(name, message_job(502, 9002))
  let current = next_send(sent, 502)
  let delivered = counter("delivered_total")
  let retries = counter("retries_total")
  process.send(old.answer, state.Delivered)
  process.sleep(100)
  assert counter("delivered_total") == delivered
  process.send(current.answer, state.Failed("busy"))
  let assert True =
    probe.eventually(fn() { counter("retries_total") == retries + 1 }, 100)
  let retried = next_send(sent, 502)
  process.send(retried.answer, state.Delivered)
  let assert True =
    probe.eventually(fn() { counter("delivered_total") == delivered + 1 }, 100)
  process.kill(sup)
}

pub fn an_attempt_in_flight_elsewhere_is_not_resent_at_once_test() {
  let sent = process.new_subject()
  let #(name, sup) = start(sent)
  let retries = counter("retries_total")
  let assert Ok(Nil) = outbox.enqueue_to(name, message_job(601, 9101))
  let first = next_send(sent, 601)
  process.send(first.answer, state.InFlightElsewhere)
  let assert True =
    probe.eventually(fn() { counter("retries_total") == retries + 1 }, 100)
  let assert Error(Nil) = process.receive(sent, 2500)
  process.kill(sup)
}
