// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/rollout
import gleam/erlang/process
import gleam/int
import gleam/list
import support/cluster_app
import support/cluster_fake_nats as fake
import support/cluster_relay_probe as probe

fn collectors(count: Int) -> List(probe.Collector) {
  list.repeat(Nil, count) |> list.map(fn(_) { probe.start_collector() })
}

fn all_received(
  targets: List(probe.Collector),
  expected: List(String),
) -> Bool {
  fake.wait_until(5000, fn() {
    list.all(targets, fn(target) { probe.collected(target) == expected })
  })
}

pub fn every_session_gets_every_event_in_order_test() {
  let targets = collectors(40)
  probe.send_events(targets, 300)
  assert all_received(targets, probe.events(300))
}

pub fn relay_keeps_delivering_while_workers_restart_test() {
  let targets = collectors(24)
  let workers =
    int.range(
      from: 0,
      to: int.min(3, rollout.dispatch_relay_shards()),
      with: [],
      run: fn(acc, index) {
        ["fluxer_gateway_dispatch_relay_" <> int.to_string(index), ..acc]
      },
    )
  let killed =
    list.filter_map(workers, fn(name) {
      case cluster_app.registered(name) {
        Ok(pid) -> {
          process.kill(pid)
          Ok(pid)
        }
        Error(Nil) -> Error(Nil)
      }
    })
  assert fake.wait_until(2000, fn() { !list.any(killed, process.is_alive) })
  probe.send_events(targets, 200)
  assert all_received(targets, probe.events(200))
  assert fake.wait_until(3000, fn() {
    list.all(workers, fn(name) { cluster_app.registered(name) != Error(Nil) })
  })
  let fresh = collectors(24)
  probe.send_events(fresh, 200)
  assert all_received(fresh, probe.events(200))
}
