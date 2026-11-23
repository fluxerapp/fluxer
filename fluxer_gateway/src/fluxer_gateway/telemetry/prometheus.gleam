// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/metrics.{
  type HistogramSeries, type Kind, type Sample, Counter, Gauge, Histogram,
}
import fluxer_gateway/telemetry/native_metrics
import fluxer_gateway/telemetry/process_registry
import fluxer_gateway/telemetry/vm
import gleam/int
import gleam/list
import gleam/set.{type Set}
import gleam/string
import gleam/string_tree.{type StringTree}

type Shape {
  Plain
  Fixed(label: String, values: List(String))
  Open
}

type Spec {
  Spec(name: String, kind: Kind, help: String, shape: Shape)
}

pub fn current_samples() -> List(Sample) {
  list.append(process_registry.samples(), metrics.read_all())
}

pub fn render() -> String {
  let kinds = metrics.registry_samples()
  let samples = current_samples()
  let specs = catalogue(samples)
  let known = list.map(specs, fn(spec) { spec.name }) |> set.from_list
  [
    list.map(specs, render_spec(_, samples)),
    [render_vm()],
    list.map(native_metrics.samples(), render_native),
    render_others(samples, kinds, known),
    render_histograms(metrics.histograms()),
  ]
  |> list.flatten
  |> string_tree.concat
  |> string_tree.to_string
}

fn catalogue(samples: List(Sample)) -> List(Spec) {
  let push_outbox = case
    list.any(samples, fn(sample) {
      string.starts_with(sample.name, "fluxer_gateway_push_outbox_")
    })
  {
    True -> push_outbox_specs()
    False -> []
  }
  list.flatten([
    gateway_specs(),
    cluster_specs(),
    [
      Spec(
        "fluxer_gateway_processes",
        Gauge,
        "Registered processes by type",
        Fixed(
          "type",
          list.map(process_registry.kinds, process_registry.kind_name),
        ),
      ),
    ],
    channel_threads_specs(),
    visitor_specs(),
    push_outbox,
  ])
}

fn gateway_specs() -> List(Spec) {
  [
    Spec(
      "fluxer_gateway_sessions_total",
      Gauge,
      "Connected WebSocket sessions",
      Plain,
    ),
    Spec(
      "fluxer_gateway_session_resumes_total",
      Counter,
      "Successful WebSocket session resumes",
      Plain,
    ),
    Spec("fluxer_gateway_guilds_total", Gauge, "Locally loaded guilds", Plain),
    Spec(
      "fluxer_gateway_websocket_dispatches_total",
      Counter,
      "WebSocket dispatch frames handed to the transport, including replays",
      Plain,
    ),
    Spec(
      "fluxer_gateway_websocket_dispatch_drops_total",
      Counter,
      "WebSocket dispatches dropped due to encoding or compression errors",
      Plain,
    ),
    Spec(
      "fluxer_gateway_voice_connections_total",
      Gauge,
      "Total voice connections",
      Plain,
    ),
    Spec(
      "fluxer_gateway_voice_connections",
      Gauge,
      "Voice connections by dimension",
      Open,
    ),
    Spec("fluxer_gateway_calls_total", Gauge, "Active voice calls", Plain),
    Spec(
      "fluxer_gateway_push_workers_active",
      Gauge,
      "Active push notification workers",
      Plain,
    ),
    Spec(
      "fluxer_gateway_concurrent_session_starts",
      Gauge,
      "In-flight session start operations",
      Plain,
    ),
    Spec(
      "fluxer_gateway_queued_session_starts",
      Gauge,
      "Session starts waiting for admission",
      Plain,
    ),
    Spec(
      "fluxer_gateway_concurrent_guild_starts",
      Gauge,
      "In-flight guild start operations",
      Plain,
    ),
  ]
}

fn cluster_specs() -> List(Spec) {
  [
    Spec(
      "fluxer_gateway_cluster_member_count",
      Gauge,
      "Alive cluster members",
      Plain,
    ),
    Spec(
      "fluxer_gateway_cluster_discovery_resolve_failures_total",
      Counter,
      "DNS discovery resolve failures",
      Plain,
    ),
    Spec(
      "fluxer_gateway_cluster_membership_transitions_total",
      Counter,
      "Cluster membership transitions",
      Fixed("direction", ["up", "down"]),
    ),
    Spec(
      "fluxer_gateway_cluster_owner_resolutions_total",
      Counter,
      "Owner resolution outcomes",
      Fixed("result", ["self", "peer"]),
    ),
  ]
}

fn channel_threads_specs() -> List(Spec) {
  [
    Spec(
      "fluxer_gateway_thread_store_threads",
      Gauge,
      "Active threads held in guild thread stores on this node",
      Plain,
    ),
    Spec(
      "fluxer_gateway_thread_dispatch_total",
      Counter,
      "Thread-gated events delivered to sessions by event and variant",
      Open,
    ),
    Spec(
      "fluxer_gateway_thread_list_sync_total",
      Counter,
      "THREAD_LIST_SYNC sends by reason",
      Fixed("reason", ["full", "access"]),
    ),
    Spec(
      "fluxer_gateway_thread_collection_fetch_total",
      Counter,
      "Thread collection fetches by result",
      Fixed("result", ["ok", "error", "refetch"]),
    ),
    Spec(
      "fluxer_gateway_thread_permission_unknown_parent_total",
      Counter,
      "Thread permission checks whose parent channel was unknown",
      Plain,
    ),
  ]
}

fn visitor_specs() -> List(Spec) {
  [
    Spec(
      "fluxer_gateway_guild_visitor_sessions",
      Gauge,
      "Live guild visitor sessions on this node",
      Plain,
    ),
    Spec(
      "fluxer_gateway_guild_visitor_admitted_total",
      Counter,
      "Guild visitor sessions admitted",
      Plain,
    ),
    Spec(
      "fluxer_gateway_guild_visitor_converted_total",
      Counter,
      "Guild visitor sessions converted to member sessions",
      Plain,
    ),
    Spec(
      "fluxer_gateway_guild_visitor_rejected_total",
      Counter,
      "Guild visitor connects rejected by reason",
      Fixed("reason", ["disabled", "age_restricted", "unavailable", "busy"]),
    ),
    Spec(
      "fluxer_gateway_guild_visitor_evicted_total",
      Counter,
      "Guild visitor sessions evicted by reason",
      Fixed("reason", [
        "busy", "banned", "disabled", "age_restricted", "removed", "error",
        "unavailable",
      ]),
    ),
  ]
}

fn push_outbox_specs() -> List(Spec) {
  let by_class = Fixed("class", ["direct", "bulk"])
  [
    Spec(
      "fluxer_gateway_push_outbox_depth",
      Gauge,
      "Push jobs queued in the outbox",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_inflight",
      Gauge,
      "Push job requests awaiting a reply",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_unknown",
      Gauge,
      "Push job requests whose outcome is unknown",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_delivered_total",
      Counter,
      "Push jobs acknowledged by the push service",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_retries_total",
      Counter,
      "Push job requests scheduled for retry",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_sheds_total",
      Counter,
      "Earliest queued push jobs shed at outbox capacity",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_truncations_total",
      Counter,
      "Queued recipients dropped because they read the channel",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_skipped_active_total",
      Counter,
      "Queued recipients skipped because they became active",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_followup_clears_total",
      Counter,
      "Clears queued after an in-flight push job whose recipient read the channel",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_dropped_total",
      Counter,
      "Push jobs dropped undelivered by kind and reason",
      Open,
    ),
  ]
}

fn render_spec(spec: Spec, samples: List(Sample)) -> StringTree {
  case spec.shape {
    Plain ->
      family(spec.name, kind_name(spec.kind), spec.help, [
        #([], plain_value(spec.name, samples)),
      ])
    Fixed(label, values) ->
      family(
        spec.name,
        kind_name(spec.kind),
        spec.help,
        list.map(values, fn(value) {
          let labels = [#(label, value)]
          #(labels, metrics.value(samples, spec.name, labels))
        }),
      )
    Open ->
      family(
        spec.name,
        kind_name(spec.kind),
        spec.help,
        labelled_series(samples, spec.name),
      )
  }
}

fn plain_value(name: String, samples: List(Sample)) -> Int {
  let value = metrics.value(samples, name, [])
  case name {
    "fluxer_gateway_guild_visitor_sessions" -> int.max(0, value)
    _ -> value
  }
}

fn labelled_series(
  samples: List(Sample),
  name: String,
) -> List(#(List(#(String, String)), Int)) {
  samples
  |> list.filter(fn(sample) { sample.name == name && sample.labels != [] })
  |> list.map(fn(sample) { metrics.normalise(sample.labels) })
  |> list.unique
  |> list.map(fn(labels) { #(labels, metrics.value(samples, name, labels)) })
}

fn render_vm() -> StringTree {
  string_tree.concat([
    family(
      "erlang_vm_memory_bytes",
      "gauge",
      "Erlang VM memory usage in bytes",
      list.map(["total", "processes", "binary", "ets", "atom"], fn(kind) {
        #([#("type", kind)], vm.memory(of: kind))
      }),
    ),
    family("erlang_vm_process_count", "gauge", "Erlang VM process count", [
      #([], vm.process_count()),
    ]),
    family("erlang_vm_port_count", "gauge", "Erlang VM port count", [
      #([], vm.port_count()),
    ]),
    family("erlang_vm_atom_count", "gauge", "Erlang VM atom count", [
      #([], vm.atom_count()),
    ]),
    family("erlang_vm_scheduler_count", "gauge", "Erlang VM online schedulers", [
      #([], vm.schedulers_online()),
    ]),
  ])
}

fn render_native(sample: Sample) -> StringTree {
  family(sample.name, "gauge", "Rust NIF runtime metric " <> sample.name, [
    #([], sample.value),
  ])
}

fn render_others(
  samples: List(Sample),
  kinds: List(#(Kind, Sample)),
  known: Set(String),
) -> List(StringTree) {
  samples
  |> list.map(fn(sample) { sample.name })
  |> list.unique
  |> list.filter(fn(name) { !set.contains(known, name) })
  |> list.map(fn(name) {
    let series =
      samples
      |> list.filter(fn(sample) { sample.name == name })
      |> list.map(fn(sample) { metrics.normalise(sample.labels) })
      |> list.unique
      |> list.map(fn(labels) { #(labels, metrics.value(samples, name, labels)) })
    family(name, kind_name(registry_kind(kinds, name)), name, series)
  })
}

fn registry_kind(kinds: List(#(Kind, Sample)), name: String) -> Kind {
  let found =
    list.find(kinds, fn(entry) {
      let #(_, sample) = entry
      sample.name == name
    })
  case found {
    Ok(#(kind, _)) -> kind
    Error(Nil) -> Gauge
  }
}

fn render_histograms(series: List(HistogramSeries)) -> List(StringTree) {
  series
  |> list.map(fn(entry) { entry.name })
  |> list.unique
  |> list.map(fn(name) {
    let lines =
      series
      |> list.filter(fn(entry) { entry.name == name })
      |> list.map(histogram_lines)
    string_tree.concat([header(name, "histogram", name), ..lines])
  })
}

fn histogram_lines(series: HistogramSeries) -> StringTree {
  let #(_, bucket_lines) =
    list.zip(metrics.bucket_bounds_ms, series.buckets)
    |> list.map_fold(0, fn(running, pair) {
      let #(bound, count) = pair
      let total = running + count
      #(
        total,
        line(
          series.name <> "_bucket",
          list.append(series.labels, [#("le", int.to_string(bound))]),
          total,
        ),
      )
    })
  string_tree.concat(
    list.flatten([
      bucket_lines,
      [
        line(
          series.name <> "_bucket",
          list.append(series.labels, [#("le", "+Inf")]),
          series.count,
        ),
        line(series.name <> "_sum", series.labels, series.sum),
        line(series.name <> "_count", series.labels, series.count),
      ],
    ]),
  )
}

fn family(
  name: String,
  kind: String,
  help: String,
  series: List(#(List(#(String, String)), Int)),
) -> StringTree {
  case series {
    [] -> string_tree.new()
    _ ->
      string_tree.concat([
        header(name, kind, help),
        ..list.map(series, fn(entry) {
          let #(labels, value) = entry
          line(name, labels, value)
        })
      ])
  }
}

fn header(name: String, kind: String, help: String) -> StringTree {
  string_tree.from_strings([
    "# HELP ", name, " ", help, "\n", "# TYPE ", name, " ", kind, "\n",
  ])
}

fn line(
  name: String,
  labels: List(#(String, String)),
  value: Int,
) -> StringTree {
  string_tree.from_strings([
    name,
    label_text(labels),
    " ",
    int.to_string(value),
    "\n",
  ])
}

fn label_text(labels: List(#(String, String))) -> String {
  case labels {
    [] -> ""
    _ -> {
      let pairs =
        labels
        |> list.map(fn(label) {
          let #(key, value) = label
          key <> "=\"" <> escape(value) <> "\""
        })
        |> string.join(",")
      "{" <> pairs <> "}"
    }
  }
}

fn escape(value: String) -> String {
  value
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
}

fn kind_name(kind: Kind) -> String {
  case kind {
    Counter -> "counter"
    Gauge -> "gauge"
    Histogram -> "histogram"
  }
}
