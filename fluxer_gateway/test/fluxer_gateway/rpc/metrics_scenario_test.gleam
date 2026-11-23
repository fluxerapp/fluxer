// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
import fluxer_gateway/core/node
import fluxer_gateway/nats/metrics as nats_metrics
import fluxer_gateway/nats/pool
import fluxer_gateway/rpc/metrics as rpc_metrics
import fluxer_gateway/rpc/server
import fluxer_gateway/telemetry/metrics
import gleam/erlang/process
import gleam/list
import gleam/option.{Some}
import gleam/result
import support/cluster_app
import support/cluster_fake_nats.{type FakeNats} as fake

const slots = 16

fn value(name: String, labels: List(#(String, String))) -> Int {
  metrics.value(metrics.read_all(), name, labels)
}

fn timed(family: String) -> Int {
  metrics.histograms()
  |> list.find(fn(series) {
    series.name == rpc_metrics.duration
    && series.labels == [#("family", family)]
  })
  |> result.map(fn(series) { series.count })
  |> result.unwrap(0)
}

fn with_rpc(run: fn(FakeNats) -> Nil) -> Nil {
  let server_side = fake.start()
  let assert Ok(paused_rpc) = cluster_app.pause("fluxer_gateway_nats_rpc")
  let assert Ok(paused_pool) = cluster_app.pause("fluxer_gateway_nats_pool")
  let configured =
    settings.Settings(
      ..settings.get(),
      nats_core_url: fake.url(server_side),
      role: node.Guilds,
      nats_rpc_enabled: True,
    )
  let assert Ok(_) = pool.supervised(configured).start()
  assert fake.wait_until(5000, fn() {
    list.length(fake.connections(server_side)) == slots
  })
  let assert Ok(started) = server.supervised(configured).start()
  process.unlink(started.pid)
  assert fake.wait_until(5000, fn() {
    fake.subs_on(server_side, "rpc.gateway.process.>") != []
  })
  run(server_side)
  cluster_app.stop_linked(started.pid)
  let assert Ok(current) = pool.pool_pid()
  cluster_app.stop_linked(current)
  cluster_app.resume(paused_pool)
  cluster_app.resume(paused_rpc)
}

fn ask(on: FakeNats, subject: String, inbox: String, body: String) -> Nil {
  fake.inject(on, subject, Some(inbox), body)
  assert fake.wait_until(5000, fn() { fake.published_on(on, inbox) != [] })
}

pub fn handled_nats_rpcs_are_counted_and_timed_by_method_test() {
  use on <- with_rpc
  let ok = [#("method", "process.node_stats"), #("outcome", "ok")]
  let unknown = [#("method", "other"), #("outcome", "error")]
  let replies = [#("kind", "reply"), #("outcome", "ok")]
  let before_ok = value(rpc_metrics.requests, ok)
  let before_unknown = value(rpc_metrics.requests, unknown)
  let before_timed = timed("process")
  let before_replies = value(nats_metrics.publishes, replies)
  let before_in = value(nats_metrics.messages, [#("direction", "in")])
  let before_out = value(nats_metrics.messages, [#("direction", "out")])
  let before_bytes = value(nats_metrics.bytes, [#("direction", "out")])
  ask(on, "rpc.gateway.process.node_stats", "_INBOX.metrics.1", "{}")
  ask(on, "rpc.gateway.process.no_such_method", "_INBOX.metrics.2", "{}")
  assert fake.wait_until(2000, fn() {
    value(rpc_metrics.requests, unknown) > before_unknown
  })
  assert value(rpc_metrics.requests, ok) > before_ok
  assert timed("process") > before_timed
  assert value(nats_metrics.publishes, replies) >= before_replies + 2
  assert value(nats_metrics.messages, [#("direction", "in")]) >= before_in + 2
  assert value(nats_metrics.messages, [#("direction", "out")]) >= before_out + 2
  assert value(nats_metrics.bytes, [#("direction", "out")]) > before_bytes
  assert value(rpc_metrics.requests, [
      #("method", "process.no_such_method"),
      #("outcome", "error"),
    ])
    == 0
  assert fake.wait_until(2000, fn() { value(rpc_metrics.handlers, []) == 0 })
}

pub fn the_nats_pool_reports_slots_events_and_subscriptions_test() {
  let connected =
    value(nats_metrics.connection_events, [#("event", "connected")])
  let lost = value(nats_metrics.connection_events, [#("event", "lost")])
  use on <- with_rpc
  assert value(nats_metrics.connections, [#("state", "ready")]) == slots
  assert value(nats_metrics.connections, [#("state", "down")]) == 0
  assert value(nats_metrics.connection_events, [#("event", "connected")])
    >= connected + slots
  assert value(nats_metrics.subscriptions, []) >= 3
  let assert [first, ..] = fake.connections(on)
  fake.drop(on, first)
  assert fake.wait_until(5000, fn() {
    value(nats_metrics.connection_events, [#("event", "lost")]) > lost
  })
}

pub fn rpc_methods_fold_into_a_closed_set_test() {
  assert rpc_metrics.method("guild.dispatch") == "guild.dispatch"
  assert rpc_metrics.method("guild.12345") == "other"
  assert rpc_metrics.family("presence.dispatch") == "presence"
  assert rpc_metrics.family("anything") == "other"
  assert list.length(rpc_metrics.methods)
    == list.length(list.unique(rpc_metrics.methods))
}
