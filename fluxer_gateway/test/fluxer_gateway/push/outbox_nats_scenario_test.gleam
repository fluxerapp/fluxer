// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{Durations, PushDurations}
import fluxer_gateway/nats/pool
import fluxer_gateway/push/job
import fluxer_gateway/push/outbox
import fluxer_gateway/push/outbox/attempt
import fluxer_gateway/push/rule
import fluxer_gateway/telemetry/metrics
import gleam/erlang/process.{type Name, type Pid}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/static_supervisor as supervisor
import gleam/otp/supervision
import gleam/string
import support/cluster_app
import support/cluster_fake_nats.{type FakeNats, type Published} as fake
import support/isolation

const slots = 16

fn with_nats(run: fn(FakeNats) -> Nil) -> Nil {
  let server = fake.start()
  let assert Ok(paused) = cluster_app.pause("fluxer_gateway_nats_pool")
  let configured =
    settings.Settings(..settings.get(), nats_core_url: fake.url(server))
  let assert Ok(_) = pool.supervised(configured).start()
  assert fake.wait_until(5000, fn() {
    list.length(fake.connections(server)) == slots
  })
  assert fake.wait_until(5000, pool.has_ready_connection)
  let push_service = process.new_subject()
  let assert Ok(_) =
    pool.subscribe("push.job.message", Some("push"), push_service)
  assert fake.wait_until(2000, fn() {
    fake.subs_on(server, "push.job.message") != []
  })
  run(server)
  let assert Ok(current) = pool.pool_pid()
  cluster_app.stop_linked(current)
  cluster_app.resume(paused)
}

fn start_outbox() -> #(Name(outbox.Message), Pid) {
  let name = process.new_name("push_outbox_nats")
  let assert Ok(started) =
    supervisor.new(supervisor.OneForOne)
    |> supervisor.add(
      supervision.worker(fn() {
        outbox.start(name, settings.get(), attempt.send)
      }),
    )
    |> supervisor.start
  process.unlink(started.pid)
  #(name, started.pid)
}

fn message_job(user: Int, message: Int) -> job.Job {
  let assert Ok(built) =
    job.for_users(job.template([], channel: 78, message:), rule.Direct, [
      user,
    ])
  built
}

fn counter(name: String) -> Int {
  metrics.value(metrics.read_all(), "fluxer_gateway_push_outbox_" <> name, [
    #("class", "direct"),
  ])
}

fn sent_for(server: FakeNats, user: Int) -> List(Published) {
  fake.published_on(server, "push.job.message")
  |> list.filter(fn(entry) {
    string.contains(entry.payload, "\"" <> int.to_string(user) <> "\"")
  })
}

fn nth_send(server: FakeNats, user: Int, nth: Int) -> Published {
  assert fake.wait_until(3000, fn() {
    list.length(sent_for(server, user)) >= nth
  })
  let assert Ok(found) = list.last(list.take(sent_for(server, user), nth))
  found
}

fn answer(server: FakeNats, request: Published, body: String) -> Nil {
  let assert Some(inbox) = request.reply_to
  fake.inject(server, inbox, None, body)
}

pub fn a_job_is_delivered_through_a_nats_request_test() {
  use server <- with_nats
  let #(name, sup) = start_outbox()
  let delivered = counter("delivered_total")
  let assert Ok(Nil) = outbox.enqueue_to(name, message_job(711, 9301))
  let request = nth_send(server, 711, 1)
  assert string.contains(request.payload, "\"user_ids\":[\"711\"]")
  answer(server, request, "{\"ok\":true}")
  assert fake.wait_until(2000, fn() {
    counter("delivered_total") == delivered + 1
  })
  process.sleep(200)
  assert list.length(sent_for(server, 711)) == 1
  process.kill(sup)
}

pub fn an_in_flight_answer_waits_for_the_floor_then_resends_test() {
  isolation.set_durations(fn(durations) {
    Durations(
      ..durations,
      push: PushDurations(
        ..durations.push,
        outbox_state_in_flight_floor_ms: 600,
        outbox_state_retry_base_ms: 10,
      ),
    )
  })
  use server <- with_nats
  let #(name, sup) = start_outbox()
  let delivered = counter("delivered_total")
  let assert Ok(Nil) = outbox.enqueue_to(name, message_job(712, 9302))
  let first = nth_send(server, 712, 1)
  answer(server, first, "{\"ok\":false,\"error\":\"in_flight\"}")
  process.sleep(400)
  assert list.length(sent_for(server, 712)) == 1
  let second = nth_send(server, 712, 2)
  answer(server, second, "{\"ok\":true}")
  assert fake.wait_until(2000, fn() {
    counter("delivered_total") == delivered + 1
  })
  process.kill(sup)
}

pub fn a_connection_lost_mid_request_is_retried_test() {
  isolation.set_durations(fn(durations) {
    Durations(
      ..durations,
      push: PushDurations(..durations.push, outbox_state_retry_base_ms: 10),
    )
  })
  use server <- with_nats
  let #(name, sup) = start_outbox()
  let delivered = counter("delivered_total")
  let retries = counter("retries_total")
  let assert Ok(Nil) = outbox.enqueue_to(name, message_job(713, 9303))
  let first = nth_send(server, 713, 1)
  fake.drop(server, first.conn)
  let second = nth_send(server, 713, 2)
  assert second.reply_to != first.reply_to
  assert counter("retries_total") == retries + 1
  answer(server, second, "{\"ok\":true}")
  assert fake.wait_until(2000, fn() {
    counter("delivered_total") == delivered + 1
  })
  process.kill(sup)
}

pub fn a_restarted_outbox_ignores_answers_to_the_old_one_test() {
  use server <- with_nats
  let #(name, sup) = start_outbox()
  let delivered = counter("delivered_total")
  let assert Ok(Nil) = outbox.enqueue_to(name, message_job(714, 9304))
  let orphan = nth_send(server, 714, 1)
  let assert Ok(before) = process.named(name)
  process.kill(before)
  assert fake.wait_until(2000, fn() {
    case process.named(name) {
      Ok(after) -> after != before
      Error(Nil) -> False
    }
  })
  let assert Ok(Nil) = outbox.enqueue_to(name, message_job(715, 9305))
  let current = nth_send(server, 715, 1)
  answer(server, orphan, "{\"ok\":true}")
  process.sleep(200)
  assert counter("delivered_total") == delivered
  answer(server, current, "{\"ok\":true}")
  assert fake.wait_until(2000, fn() {
    counter("delivered_total") == delivered + 1
  })
  assert list.length(sent_for(server, 714)) == 1
  let assert Ok(after) = process.named(name)
  assert process.is_alive(after)
  process.kill(sup)
}
