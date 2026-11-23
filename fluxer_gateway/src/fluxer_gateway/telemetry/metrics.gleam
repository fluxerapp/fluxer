// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/fixed_counters
import fluxer_gateway/util/table
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/order
import gleam/string

pub type Sample {
  Sample(name: String, labels: List(#(String, String)), value: Int)
}

pub type Kind {
  Counter
  Gauge
  Histogram
}

pub type HistogramSeries {
  HistogramSeries(
    name: String,
    labels: List(#(String, String)),
    buckets: List(Int),
    overflow: Int,
    sum: Int,
    count: Int,
  )
}

type Slot {
  Value
  Bucket(index: Int)
  Overflow
  Sum
  Count
}

type MetricKey {
  MetricKey(
    kind: Kind,
    name: String,
    labels: List(#(String, String)),
    slot: Slot,
  )
}

type CollectorKey {
  CollectorKey(name: String)
}

type HistogramCollectorKey {
  HistogramCollectorKey(name: String)
}

pub const bucket_bounds_ms = [
  1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10_000,
]

fn registry() -> table.Table {
  table.named("fluxer_gateway_metrics")
}

fn collectors() -> table.Table {
  table.named("fluxer_gateway_metric_collectors")
}

fn histogram_collectors() -> table.Table {
  table.named("fluxer_gateway_metric_histogram_collectors")
}

pub fn counter(name: String, labels: List(#(String, String)), by: Int) -> Nil {
  add(MetricKey(Counter, name, normalise(labels), Value), by)
}

pub fn gauge(name: String, labels: List(#(String, String)), value: Int) -> Nil {
  case table.ensure_public(registry()) {
    True ->
      table.insert(registry(), #(
        MetricKey(Gauge, name, normalise(labels), Value),
        value,
      ))
    False -> Nil
  }
}

pub fn remove(
  kind: Kind,
  name: String,
  labels: List(#(String, String)),
) -> Nil {
  table.delete(registry(), MetricKey(kind, name, normalise(labels), Value))
}

pub fn observe_ms(
  name: String,
  labels: List(#(String, String)),
  value_ms: Int,
) -> Nil {
  let labels = normalise(labels)
  let value_ms = int.max(0, value_ms)
  add(MetricKey(Histogram, name, labels, bucket_slot(value_ms)), 1)
  add(MetricKey(Histogram, name, labels, Sum), value_ms)
  add(MetricKey(Histogram, name, labels, Count), 1)
}

pub fn register_collector(name: String, collect: fn() -> List(Sample)) -> Nil {
  case table.ensure_public(collectors()) {
    True -> table.insert(collectors(), #(CollectorKey(name), collect))
    False -> Nil
  }
}

pub fn unregister_collector(name: String) -> Nil {
  table.delete(collectors(), CollectorKey(name))
}

pub fn register_histogram_collector(
  name: String,
  collect: fn() -> List(HistogramSeries),
) -> Nil {
  case table.ensure_public(histogram_collectors()) {
    True ->
      table.insert(histogram_collectors(), #(
        HistogramCollectorKey(name),
        collect,
      ))
    False -> Nil
  }
}

pub fn bucket_of(value: Int) -> Int {
  case bucket_slot(int.max(0, value)) {
    Bucket(index) -> index
    _ -> list.length(bucket_bounds_ms)
  }
}

pub fn registry_samples() -> List(#(Kind, Sample)) {
  rows()
  |> list.filter_map(fn(row) {
    let #(key, value) = row
    case key.kind, key.slot {
      Counter, Value | Gauge, Value ->
        Ok(#(key.kind, Sample(key.name, key.labels, value)))
      _, _ -> Error(Nil)
    }
  })
  |> list.sort(by_sample_name)
}

fn by_sample_name(a: #(Kind, Sample), b: #(Kind, Sample)) -> order.Order {
  let #(_, left) = a
  let #(_, right) = b
  string.compare(left.name, right.name)
}

fn collected_samples() -> List(Sample) {
  let entries: List(#(CollectorKey, fn() -> List(Sample))) =
    table.to_list(collectors())
  entries
  |> list.sort(fn(a, b) {
    let #(CollectorKey(left), _) = a
    let #(CollectorKey(right), _) = b
    string.compare(left, right)
  })
  |> list.flat_map(fn(entry) {
    let #(_, collect) = entry
    collect()
  })
}

pub fn read_all() -> List(Sample) {
  list.flatten([
    list.map(registry_samples(), fn(entry) {
      let #(_, sample) = entry
      sample
    }),
    collected_samples(),
    list.map(fixed_counters.rows(), fn(row) { Sample(row.0, row.1, row.2) }),
  ])
}

pub fn histograms() -> List(HistogramSeries) {
  rows()
  |> list.filter(fn(row) {
    let #(key, _) = row
    key.kind == Histogram
  })
  |> list.group(fn(row) {
    let #(key, _) = row
    #(key.name, key.labels)
  })
  |> to_series
  |> list.append(collected_histograms())
  |> list.sort(fn(a, b) { string.compare(a.name, b.name) })
}

fn collected_histograms() -> List(HistogramSeries) {
  let entries: List(#(HistogramCollectorKey, fn() -> List(HistogramSeries))) =
    table.to_list(histogram_collectors())
  entries
  |> list.sort(fn(a, b) {
    let #(HistogramCollectorKey(left), _) = a
    let #(HistogramCollectorKey(right), _) = b
    string.compare(left, right)
  })
  |> list.flat_map(fn(entry) {
    let #(_, collect) = entry
    collect()
  })
}

pub fn value(
  samples: List(Sample),
  name: String,
  labels: List(#(String, String)),
) -> Int {
  let labels = normalise(labels)
  list.fold(samples, 0, fn(found, sample) {
    case sample.name == name && normalise(sample.labels) == labels {
      True -> sample.value
      False -> found
    }
  })
}

pub fn total(
  name: String,
  matching: fn(List(#(String, String))) -> Bool,
) -> Int {
  list.fold(rows(), 0, fn(sum, row) {
    let #(key, value) = row
    case
      key.name == name
      && key.slot == Value
      && key.kind != Histogram
      && matching(key.labels)
    {
      True -> sum + value
      False -> sum
    }
  })
}

pub fn normalise(labels: List(#(String, String))) -> List(#(String, String)) {
  list.sort(labels, fn(a, b) { string.compare(a.0, b.0) })
}

fn add(key: MetricKey, by: Int) -> Nil {
  case table.ensure_public(registry()) {
    True -> {
      let _ = table.update_counter(registry(), key, by, #(key, 0))
      Nil
    }
    False -> Nil
  }
}

fn rows() -> List(#(MetricKey, Int)) {
  table.to_list(registry())
}

fn bucket_slot(value_ms: Int) -> Slot {
  bucket_index(bucket_bounds_ms, value_ms, 0)
}

fn bucket_index(bounds: List(Int), value_ms: Int, index: Int) -> Slot {
  case bounds {
    [] -> Overflow
    [bound, ..rest] ->
      case value_ms <= bound {
        True -> Bucket(index)
        False -> bucket_index(rest, value_ms, index + 1)
      }
  }
}

fn to_series(
  groups: Dict(#(String, List(#(String, String))), List(#(MetricKey, Int))),
) -> List(HistogramSeries) {
  groups
  |> dict.to_list
  |> list.sort(fn(a, b) {
    let #(#(left, _), _) = a
    let #(#(right, _), _) = b
    string.compare(left, right)
  })
  |> list.map(fn(group) {
    let #(#(name, labels), group_rows) = group
    let slot_value = fn(wanted: Slot) {
      list.fold(group_rows, 0, fn(total, row) {
        let #(key, count) = row
        case key.slot == wanted {
          True -> total + count
          False -> total
        }
      })
    }
    HistogramSeries(
      name:,
      labels:,
      buckets: list.index_map(bucket_bounds_ms, fn(_, index) {
        slot_value(Bucket(index))
      }),
      overflow: slot_value(Overflow),
      sum: slot_value(Sum),
      count: slot_value(Count),
    )
  })
}
