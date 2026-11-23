// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/metrics.{Sample}
import fluxer_gateway/telemetry/prometheus
import fluxer_gateway/voice/count_table
import gleam/list
import gleam/string

pub fn counter_accumulates_test() {
  metrics.counter("test_metrics_counter_total", [#("b", "2"), #("a", "1")], 2)
  metrics.counter("test_metrics_counter_total", [#("a", "1"), #("b", "2")], 3)
  let samples = metrics.read_all()
  assert metrics.value(samples, "test_metrics_counter_total", [
      #("a", "1"),
      #("b", "2"),
    ])
    == 5
}

pub fn gauge_replaces_test() {
  metrics.gauge("test_metrics_gauge", [], 7)
  metrics.gauge("test_metrics_gauge", [], 4)
  assert metrics.value(metrics.read_all(), "test_metrics_gauge", []) == 4
}

pub fn collector_value_wins_test() {
  metrics.gauge("test_metrics_collected", [], 1)
  metrics.register_collector("test_metrics_collector", fn() {
    [Sample("test_metrics_collected", [], 9)]
  })
  assert metrics.value(metrics.read_all(), "test_metrics_collected", []) == 9
  metrics.unregister_collector("test_metrics_collector")
}

pub fn value_defaults_to_zero_test() {
  assert metrics.value([], "missing", []) == 0
}

pub fn histogram_buckets_test() {
  metrics.observe_ms("test_metrics_latency_ms", [], 3)
  metrics.observe_ms("test_metrics_latency_ms", [], 20_000)
  let assert [series, ..] =
    metrics.histograms()
    |> list.filter(fn(series) { series.name == "test_metrics_latency_ms" })
  assert series.count == 2
  assert series.sum == 20_003
  assert series.overflow == 1
}

pub fn render_keeps_original_series_test() {
  let labelled = count_table.regions() != [] || count_table.servers() != []
  let text = prometheus.render()
  assert string.contains(
    text,
    "# HELP fluxer_gateway_sessions_total Connected WebSocket sessions\n# TYPE fluxer_gateway_sessions_total gauge\n",
  )
  assert string.contains(
    text,
    "fluxer_gateway_cluster_membership_transitions_total{direction=\"up\"} ",
  )
  assert string.contains(text, "fluxer_gateway_processes{type=\"guild\"} ")
  assert string.contains(
    text,
    "fluxer_gateway_guild_visitor_evicted_total{reason=\"age_restricted\"} ",
  )
  assert string.contains(text, "erlang_vm_memory_bytes{type=\"total\"} ")
  assert string.contains(text, "# TYPE erlang_vm_process_count gauge\n")
  assert string.contains(text, "fluxer_gateway_voice_connections{") == labelled
}

pub fn render_includes_lane_series_test() {
  metrics.counter("test_render_lane_total", [#("kind", "x")], 1)
  let text = prometheus.render()
  assert string.contains(text, "# TYPE test_render_lane_total counter\n")
  assert string.contains(text, "test_render_lane_total{kind=\"x\"} ")
}
