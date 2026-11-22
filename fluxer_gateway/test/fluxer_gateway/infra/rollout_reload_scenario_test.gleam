// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/supervisor
import fluxer_gateway/config/rollout
import fluxer_gateway/config/settings
import fluxer_gateway/gateway/start_gate
import fluxer_gateway/infra/harness
import fluxer_gateway/nats/pool
import gleam/dict
import gleam/dynamic
import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import support/cluster_app
import support/cluster_fake_nats as fake
import support/gateway_api_stub as stub
import support/isolation

type Reply {
  Set(status: Int, body: String)
  Get(process.Subject(#(Int, String)))
}

type RolloutKey {
  FluxerGatewayRolloutConfig
}

@external(erlang, "persistent_term", "put")
fn put_rollout(key: RolloutKey, value: Option(rollout.RolloutConfig)) -> a

fn replies() -> process.Subject(Reply) {
  let assert Ok(started) =
    actor.new(#(200, config_body("{}")))
    |> actor.on_message(fn(current, message) {
      case message {
        Set(status, body) -> actor.continue(#(status, body))
        Get(reply) -> {
          process.send(reply, current)
          actor.continue(current)
        }
      }
    })
    |> actor.start
  process.unlink(started.pid)
  started.data
}

fn config_body(config: String) -> String {
  "{\"data\":{\"config\":" <> config <> "}}"
}

fn serve(
  replies: process.Subject(Reply),
) -> fn(String, String) -> #(Int, String) {
  fn(kind, _body) {
    case kind {
      "get_gateway_rollout_config" -> process.call(replies, 1000, Get)
      _ -> #(404, "{}")
    }
  }
}

fn fast_fetches() -> Nil {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      config: settings.ConfigDurations(
        ..d.config,
        rollout_fetch_delay_ms: 20,
        rollout_fetch_retry_ms: 40,
        rollout_reconcile_interval_ms: 40,
      ),
    )
  })
}

fn next_change(changes: process.Subject(rollout.RolloutChanged)) -> Int {
  let assert Ok(rollout.RolloutChanged(version:)) =
    process.receive(changes, 3000)
  version
}

fn quiet(changes: process.Subject(rollout.RolloutChanged)) -> Bool {
  process.receive(changes, 400) == Error(Nil)
}

pub fn api_reload_applies_valid_config_and_keeps_it_through_bad_replies_test() {
  let replies = replies()
  let api = stub.start(serve(replies))
  fast_fetches()
  process.send(
    replies,
    Set(200, config_body("{\"max_concurrent_guild_starts\":7}")),
  )
  let server = harness.cycle(supervisor.Rollout)
  let changes = process.new_subject()
  rollout.subscribe_changes(changes)
  assert next_change(changes) == 1
  assert rollout.max_concurrent_guild_starts() == 7
  process.send(replies, Set(200, config_body("{\"rpc_request_timeout_ms\":5}")))
  assert quiet(changes)
  assert rollout.max_concurrent_guild_starts() == 7
  assert rollout.rpc_request_timeout_ms() == 10_000
  process.send(replies, Set(200, "{\"data\":{\"config\":[1]}}"))
  assert quiet(changes)
  process.send(
    replies,
    Set(200, config_body("{\"max_concurrent_guild_starts\":7}")),
  )
  assert quiet(changes)
  process.send(
    replies,
    Set(200, config_body("{\"max_concurrent_guild_starts\":9}")),
  )
  assert next_change(changes) == 2
  assert rollout.max_concurrent_guild_starts() == 9
  process.send(replies, Set(503, "{}"))
  assert quiet(changes)
  assert rollout.max_concurrent_guild_starts() == 9
  assert harness.pid_of(supervisor.Rollout) == Ok(server)
  stub.stop(api)
}

pub fn dead_subscribers_are_dropped_and_others_still_notified_test() {
  let replies = replies()
  let api = stub.start(serve(replies))
  fast_fetches()
  let server = harness.cycle(supervisor.Rollout)
  let changes = process.new_subject()
  rollout.subscribe_changes(changes)
  let gone =
    process.spawn_unlinked(fn() {
      rollout.subscribe_changes(process.new_subject())
      rollout.subscribe_changes(process.new_subject())
    })
  let monitor = process.monitor(gone)
  let assert Ok(_) =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(down) { down })
    |> process.selector_receive(1000)
  process.send(
    replies,
    Set(
      200,
      config_body("{\"relay_unknown\":1,\"max_concurrent_session_starts\":3}"),
    ),
  )
  assert next_change(changes) == 1
  rollout.unsubscribe_changes(changes)
  process.send(
    replies,
    Set(200, config_body("{\"max_concurrent_session_starts\":4}")),
  )
  assert quiet(changes)
  assert harness.pid_of(supervisor.Rollout) == Ok(server)
  stub.stop(api)
}

pub fn restart_keeps_the_last_good_config_test() {
  let replies = replies()
  let api = stub.start(serve(replies))
  fast_fetches()
  process.send(
    replies,
    Set(200, config_body("{\"max_concurrent_guild_starts\":11}")),
  )
  let _ = harness.cycle(supervisor.Rollout)
  let changes = process.new_subject()
  rollout.subscribe_changes(changes)
  assert next_change(changes) == 1
  process.send(replies, Set(503, "{}"))
  let _ = harness.cycle(supervisor.Rollout)
  process.sleep(200)
  assert rollout.max_concurrent_guild_starts() == 11
  stub.stop(api)
}

pub fn isolation_reset_restores_the_rollout_config_test() {
  let assert Ok(config) =
    rollout.validate(
      dict.from_list([
        #("voice_e2ee_scope", dynamic.string("platform_wide")),
        #("max_concurrent_guild_starts", dynamic.int(3)),
      ]),
    )
  put_rollout(FluxerGatewayRolloutConfig, Some(config))
  assert rollout.voice_e2ee_scope() == rollout.PlatformWide
  isolation.reset()
  assert rollout.voice_e2ee_scope() == rollout.GuildFeatureOnly
  assert rollout.max_concurrent_guild_starts() == 256
  put_rollout(FluxerGatewayRolloutConfig, None)
  isolation.reset()
  assert rollout.max_concurrent_guild_starts() == 256
}

fn start_pool(server: fake.FakeNats) -> process.Pid {
  let configured =
    settings.Settings(..settings.get(), nats_core_url: fake.url(server))
  let assert Ok(started) = pool.supervised(configured).start()
  process.unlink(started.pid)
  assert fake.wait_until(5000, pool.has_ready_connection)
  started.pid
}

fn subscribed(server: fake.FakeNats) -> Bool {
  fake.subs_on(server, "config.gateway.rollout") != []
}

pub fn nats_reload_rejects_malformed_payloads_and_survives_a_pool_restart_test() {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      config: settings.ConfigDurations(
        ..d.config,
        rollout_fetch_delay_ms: 60_000,
        rollout_nats_subscribe_retry_ms: 50,
      ),
    )
  })
  let server = fake.start()
  let assert Ok(paused) = cluster_app.pause("fluxer_gateway_nats_pool")
  let _ = harness.cycle(supervisor.Rollout)
  let first = start_pool(server)
  assert fake.wait_until(3000, fn() { subscribed(server) })
  let changes = process.new_subject()
  rollout.subscribe_changes(changes)
  fake.inject(
    server,
    "config.gateway.rollout",
    None,
    "{\"type\":\"gateway_rollout_config\",\"config\":{\"max_concurrent_guild_starts\":21}}",
  )
  assert next_change(changes) == 1
  assert rollout.max_concurrent_guild_starts() == 21
  fake.inject(server, "config.gateway.rollout", None, "{\"config\":5}")
  fake.inject(server, "config.gateway.rollout", None, "{\"config\":null}")
  fake.inject(server, "config.gateway.rollout", None, "not json")
  fake.inject(server, "config.gateway.rollout", None, "[1,2]")
  assert quiet(changes)
  assert rollout.max_concurrent_guild_starts() == 21
  fake.inject(
    server,
    "config.gateway.rollout",
    None,
    "{\"max_concurrent_guild_starts\":22}",
  )
  assert next_change(changes) == 2
  cluster_app.stop_linked(first)
  assert fake.wait_until(3000, fn() { !subscribed(server) })
  let second = start_pool(server)
  assert fake.wait_until(3000, fn() { subscribed(server) })
  fake.inject(
    server,
    "config.gateway.rollout",
    None,
    "{\"config\":{\"max_concurrent_guild_starts\":23}}",
  )
  assert next_change(changes) == 3
  assert rollout.max_concurrent_guild_starts() == 23
  cluster_app.stop_linked(second)
  cluster_app.resume(paused)
}

pub fn subscribers_still_hear_changes_after_a_rollout_restart_test() {
  let replies = replies()
  let api = stub.start(serve(replies))
  fast_fetches()
  process.send(
    replies,
    Set(200, config_body("{\"max_concurrent_guild_starts\":31}")),
  )
  let _ = harness.cycle(supervisor.Rollout)
  let changes = process.new_subject()
  rollout.subscribe_changes(changes)
  let _ = next_change(changes)
  let _ = harness.cycle(supervisor.Rollout)
  assert quiet(changes)
  process.send(
    replies,
    Set(200, config_body("{\"max_concurrent_guild_starts\":32}")),
  )
  let _ = next_change(changes)
  assert rollout.max_concurrent_guild_starts() == 32
  let restarted = harness.cycle(supervisor.Rollout)
  process.kill(restarted)
  assert fake.wait_until(3000, fn() {
    case harness.pid_of(supervisor.Rollout) {
      Ok(pid) -> pid != restarted
      Error(Nil) -> False
    }
  })
  process.send(
    replies,
    Set(200, config_body("{\"max_concurrent_guild_starts\":33}")),
  )
  let _ = next_change(changes)
  assert rollout.max_concurrent_guild_starts() == 33
  rollout.unsubscribe_changes(changes)
  stub.stop(api)
}

pub fn start_gate_admits_a_waiter_when_limits_rise_after_a_rollout_restart_test() {
  let replies = replies()
  let api = stub.start(serve(replies))
  fast_fetches()
  process.send(
    replies,
    Set(200, config_body("{\"max_concurrent_session_starts\":1}")),
  )
  let _ = harness.cycle(supervisor.StartGate)
  let _ = harness.cycle(supervisor.Rollout)
  assert fake.wait_until(3000, fn() {
    rollout.max_concurrent_session_starts() == 1
  })
  let _ = harness.cycle(supervisor.Rollout)
  process.sleep(200)
  let assert Ok(first) = start_gate.try_session_start()
  let assert Error(_) = start_gate.try_session_start()
  let waiter = process.new_subject()
  start_gate.wait_session_start(waiter)
  assert process.receive(waiter, 300) == Error(Nil)
  process.send(
    replies,
    Set(200, config_body("{\"max_concurrent_session_starts\":2}")),
  )
  let assert Ok(second) = process.receive(waiter, 3000)
  start_gate.release_session_start(first)
  start_gate.release_session_start(second)
  stub.stop(api)
}

pub fn rollout_retries_its_nats_subscription_until_the_pool_is_back_test() {
  let retry_ms = 60
  let window_ms = 600
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      config: settings.ConfigDurations(
        ..d.config,
        rollout_fetch_delay_ms: 60_000,
        rollout_nats_subscribe_retry_ms: retry_ms,
      ),
    )
  })
  let server = fake.start()
  let assert Ok(paused) = cluster_app.pause("fluxer_gateway_nats_pool")
  let rollout_pid = harness.cycle(supervisor.Rollout)
  let attempts =
    harness.received_during(rollout_pid, window_ms)
    |> harness.count_tagged("subscribe_nats")
  let expected = window_ms / retry_ms
  assert attempts >= expected / 2
  assert attempts <= expected + expected / 2
  assert !subscribed(server)
  let changes = process.new_subject()
  rollout.subscribe_changes(changes)
  let pool_pid = start_pool(server)
  assert fake.wait_until(3000, fn() { subscribed(server) })
  process.sleep(100)
  fake.inject(
    server,
    "config.gateway.rollout",
    None,
    "{\"config\":{\"max_concurrent_guild_starts\":41}}",
  )
  let _ = next_change(changes)
  assert rollout.max_concurrent_guild_starts() == 41
  assert harness.received_during(rollout_pid, 400)
    |> harness.count_tagged("subscribe_nats")
    == 0
  assert harness.pid_of(supervisor.Rollout) == Ok(rollout_pid)
  rollout.unsubscribe_changes(changes)
  cluster_app.stop_linked(pool_pid)
  cluster_app.resume(paused)
}
