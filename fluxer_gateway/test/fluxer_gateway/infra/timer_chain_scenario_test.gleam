// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/supervisor.{type Child}
import fluxer_gateway/config/settings
import fluxer_gateway/infra/harness
import support/cluster_app
import support/isolation

const interval_ms = 60

const window_ms = 600

const cycles = 3

fn chains_after_restarts(child: Child, tag: String) -> Int {
  list_cycle(child, cycles)
  let pid = harness.cycle(child)
  harness.received_during(pid, window_ms)
  |> harness.count_tagged(tag)
}

fn list_cycle(child: Child, remaining: Int) -> Nil {
  case remaining {
    0 -> Nil
    _ -> {
      let _ = harness.cycle(child)
      list_cycle(child, remaining - 1)
    }
  }
}

fn assert_single_chain(child: Child, tag: String) -> Nil {
  let received = chains_after_restarts(child, tag)
  let expected = window_ms / interval_ms
  assert received >= expected / 2
  assert received <= expected + expected / 2
}

pub fn counts_cache_sweep_does_not_multiply_after_restarts_test() {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      guild: settings.GuildDurations(
        ..d.guild,
        counts_cache_sweep_interval_ms: interval_ms,
      ),
    )
  })
  assert_single_chain(supervisor.GuildCountsCache, "sweep")
}

pub fn guild_health_tick_does_not_multiply_after_restarts_test() {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      guild: settings.GuildDurations(..d.guild, health_interval_ms: interval_ms),
    )
  })
  assert_single_chain(supervisor.GuildHealth, "tick")
}

pub fn session_transfer_cleanup_does_not_multiply_after_restarts_test() {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      session: settings.SessionDurations(
        ..d.session,
        transfer_cleanup_interval_ms: interval_ms,
      ),
    )
  })
  assert_single_chain(supervisor.SessionTransfer, "cleanup")
}

pub fn start_gate_prune_does_not_multiply_after_restarts_test() {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      gateway: settings.GatewayDurations(
        ..d.gateway,
        rate_limit_shared_cleanup_interval_ms: interval_ms,
        ip_connections_owner_sweep_interval_ms: interval_ms,
      ),
    )
  })
  let received = chains_after_restarts(supervisor.StartGate, "prune_rates")
  let expected = window_ms / interval_ms
  assert received >= expected / 2
  assert received <= expected + expected / 2
  let pid = harness.cycle(supervisor.StartGate)
  let sweeps =
    harness.received_during(pid, window_ms)
    |> harness.count_tagged("sweep_owners")
  assert sweeps <= expected + expected / 2
}

pub fn rollout_nats_retry_does_not_multiply_after_restarts_test() {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      config: settings.ConfigDurations(
        ..d.config,
        rollout_nats_subscribe_retry_ms: interval_ms,
        rollout_fetch_delay_ms: 60_000,
      ),
    )
  })
  let received = chains_after_restarts(supervisor.Rollout, "subscribe_nats")
  assert received <= window_ms / interval_ms + window_ms / interval_ms / 2
}

pub fn push_permit_keeper_eviction_does_not_multiply_after_restarts_test() {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      push: settings.PushDurations(
        ..d.push,
        server_evict_interval_ms: interval_ms,
      ),
    )
  })
  list_cycle(supervisor.PushPermit, cycles)
  let _ = harness.cycle(supervisor.PushPermit)
  let assert Ok(keeper) =
    cluster_app.registered("fluxer_gateway_push_permit_keeper")
  let received =
    harness.received_during(keeper, window_ms)
    |> harness.count_tagged("evict_caches")
  let expected = window_ms / interval_ms
  assert received >= expected / 2
  assert received <= expected + expected / 2
}
