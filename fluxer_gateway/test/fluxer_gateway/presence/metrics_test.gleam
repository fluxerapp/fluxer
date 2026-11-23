// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/presence.{Dnd, Online}
import fluxer_gateway/presence/client
import fluxer_gateway/presence/counters
import fluxer_gateway/presence/manager
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/resident
import gleam/list
import gleam/option.{None}
import gleam/result
import support/presence_scenario as scn

fn value(name: String, labels: List(#(String, String))) -> Int {
  metrics.value(metrics.read_all(), name, labels)
}

fn observed(name: String, labels: List(#(String, String))) -> Int {
  metrics.histograms()
  |> list.find(fn(series) {
    series.name == name && series.labels == metrics.normalise(labels)
  })
  |> result.map(fn(series) { series.count })
  |> result.unwrap(0)
}

fn sample_labels(name: String) -> List(List(#(String, String))) {
  metrics.read_all()
  |> list.filter(fn(sample) { sample.name == name })
  |> list.map(fn(sample) { sample.labels })
}

fn kind(value: String) -> List(#(String, String)) {
  [#("kind", value)]
}

fn target(value: String) -> List(#(String, String)) {
  [#("target", value)]
}

fn outcome(value: String) -> List(#(String, String)) {
  [#("outcome", value)]
}

fn started_total() -> Int {
  list.fold(["found", "called"], 0, fn(total, label) {
    total + observed(counters.start_metric, outcome(label))
  })
}

pub fn connect_and_update_count_inputs_publishes_and_deliveries_test() {
  let user = 7_100_000_000_100_001
  let guild = 7_100_000_000_100_901
  let connects = value(counters.inputs_metric, kind("connect"))
  let updates = value(counters.inputs_metric, kind("update"))
  let published = value(counters.published_metric, [])
  let local = value(counters.deliveries_metric, target("local"))
  let starts = started_total()
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 1)
  let link = scn.connect(session, Online, [])
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  client.session_update(link, Dnd, False, False, None)
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  scn.sync(user)
  assert value(counters.inputs_metric, kind("connect")) > connects
  assert value(counters.inputs_metric, kind("update")) > updates
  assert value(counters.published_metric, []) >= published + 2
  assert value(counters.deliveries_metric, target("local")) >= local + 2
  assert started_total() == starts + 1
  assert sample_labels(counters.inputs_metric)
    == list.map(counters.input_kinds, kind)
  assert sample_labels(counters.deliveries_metric)
    == list.map(counters.delivery_targets, target)
  client.session_disconnect(link)
  scn.stop_observing(guild)
}

pub fn lookups_are_timed_by_outcome_test() {
  let user = 7_100_000_000_100_002
  let found = observed(counters.lookup_metric, outcome("found"))
  let missing = observed(counters.lookup_metric, outcome("not_found"))
  let session = scn.fake_session(user, 2)
  let link = scn.connect(session, Online, [])
  scn.sync(user)
  let assert Ok(_) = manager.lookup_all(user, 1000)
  let assert Error(manager.NotFound) =
    manager.lookup_all(7_100_000_000_100_999, 1000)
  assert observed(counters.lookup_metric, outcome("found")) == found + 1
  assert observed(counters.lookup_metric, outcome("not_found")) == missing + 1
  client.session_disconnect(link)
}

pub fn presence_tables_and_mailboxes_reach_the_resident_families_test() {
  let user = 7_100_000_000_100_003
  let session = scn.fake_session(user, 3)
  let link = scn.connect(session, Online, [])
  scn.sync(user)
  let tables = resident.tables()
  let table = fn(name) { list.find(tables, fn(held) { held.name == name }) }
  let assert Ok(state) = table("fluxer_presence_state")
  assert state.count >= 1
  assert state.bytes > 0
  let assert Ok(pids) = table("fluxer_presence_pids")
  assert pids.count >= 1
  assert result.is_ok(table("fluxer_presence_subs"))
  assert result.is_ok(table("fluxer_presence_lane_table"))
  let processes = list.map(resident.singletons(), fn(held) { held.name })
  list.each(
    [
      "fluxer_presence_manager", "fluxer_presence_registry",
      "fluxer_presence_relay", "fluxer_presence_lane",
    ],
    fn(name) {
      assert list.contains(processes, name)
    },
  )
  client.session_disconnect(link)
}
