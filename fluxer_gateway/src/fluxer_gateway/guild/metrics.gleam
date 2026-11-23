// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{type JoinClass, Background, Focused, Resume}
import fluxer_gateway/guild/counts_cache/shard as counts_shard
import fluxer_gateway/guild/handoff/roster
import fluxer_gateway/telemetry/metrics.{
  type HistogramSeries, type Sample, HistogramSeries, Sample,
}
import fluxer_gateway/util/table.{type Table}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

pub const starts = "fluxer_gateway_guild_starts_total"

pub const start_failures = "fluxer_gateway_guild_start_failures_total"

pub const crash_restarts = "fluxer_gateway_guild_crash_restarts_total"

pub const stops = "fluxer_gateway_guild_stops_total"

pub const load_ms = "fluxer_gateway_guild_load_ms"

pub const loading_joins_expired =
  "fluxer_gateway_guild_loading_joins_expired_total"

pub const events = "fluxer_gateway_guild_events_total"

pub const deliveries = "fluxer_gateway_guild_dispatch_deliveries_total"

pub const fanout = "fluxer_gateway_guild_dispatch_fanout_sessions"

pub const dispatch_ms = "fluxer_gateway_guild_dispatch_ms"

pub const member_list_ops = "fluxer_gateway_guild_member_list_ops_total"

pub const member_list_ranges = "fluxer_gateway_guild_member_list_ranges_total"

pub const member_list_ms = "fluxer_gateway_guild_member_list_ms"

pub const joins_admitted = "fluxer_gateway_guild_joins_admitted_total"

pub const join_wait_ms = "fluxer_gateway_guild_join_wait_ms"

pub const join_batch_ms = "fluxer_gateway_guild_join_batch_ms"

pub const members_held = "fluxer_gateway_guild_members_held"

pub const members_online = "fluxer_gateway_guild_members_online"

pub const sessions_held = "fluxer_gateway_guild_sessions_held"

pub const in_mode = "fluxer_gateway_guild_processes_in_mode"

pub const origins = ["fetched", "arrived", "transferred"]

pub const failure_reasons = [
  "start_gate_full", "fetch_timeout", "fetch_crashed", "bad_collection",
  "unknown_guild", "api_error", "init_failed", "start_crashed",
]

pub const stop_reasons = [
  "idle", "shutdown", "moved", "handed_off", "aborted", "crashed",
]

pub const modes = ["staged", "frozen", "handing", "arriving"]

pub const member_list_op_names = ["subscribe", "unsubscribe", "denied"]

pub const join_classes = ["resume", "focused", "background"]

pub const families = [
  "message_create", "message", "reaction", "typing", "presence", "member_list",
  "member", "role", "channel", "thread", "voice", "expressions", "invite",
  "guild", "other",
]

pub type Origin {
  FromApi
  FromArrival
  FromTransfer
}

pub type Mode {
  Serving
  Staged
  Frozen
  Handing
  Arriving
}

type Counters

type CountersOption {
  WriteConcurrency
}

type Key {
  FluxerGatewayGuildMetricCounters
}

type Stored

@external(erlang, "counters", "new")
fn counters_new(size: Int, options: List(CountersOption)) -> Counters

@external(erlang, "counters", "add")
fn counters_add(counters: Counters, index: Int, by: Int) -> Stored

@external(erlang, "counters", "get")
fn counters_get(counters: Counters, index: Int) -> Int

@external(erlang, "persistent_term", "get")
fn get_counters(key: Key, default: Option(Counters)) -> Option(Counters)

@external(erlang, "persistent_term", "put")
fn put_counters(key: Key, value: Option(Counters)) -> Stored

const family_count = 15

const histogram_width = 15

const events_base = 0

const deliveries_base = 15

const dispatch_base = 30

const fanout_base = 255

const member_list_base = 270

const join_joined_base = 285

const join_expired_base = 300

const join_batch_base = 315

const member_list_ops_base = 330

const member_list_ranges_slot = 333

const joins_admitted_base = 334

const slot_count = 337

const contents_collector = "fluxer_gateway_guild_contents"

const histogram_collector = "fluxer_gateway_guild_histograms"

fn counters() -> Counters {
  case get_counters(FluxerGatewayGuildMetricCounters, None) {
    Some(found) -> found
    None -> {
      let created = counters_new(slot_count, [WriteConcurrency])
      put_counters(FluxerGatewayGuildMetricCounters, Some(created))
      created
    }
  }
}

fn add(slot: Int, by: Int) -> Nil {
  let _ = counters_add(counters(), slot + 1, by)
  Nil
}

fn read(found: Counters, slot: Int) -> Int {
  counters_get(found, slot + 1)
}

fn observe(base: Int, value: Int) -> Nil {
  let value = int.max(0, value)
  let found = counters()
  let _ = counters_add(found, base + metrics.bucket_of(value) + 1, 1)
  let _ = counters_add(found, base + histogram_width - 1, value)
  let _ = counters_add(found, base + histogram_width, 1)
  Nil
}

fn mode_rows() -> Table {
  table.named("fluxer_gateway_guild_modes")
}

pub fn register() -> Nil {
  let _ = counters()
  metrics.register_collector(contents_collector, contents)
  metrics.register_histogram_collector(histogram_collector, histograms)
}

fn contents() -> List(Sample) {
  let found = counters()
  let #(members, online) = counts_shard.totals()
  let held = held_modes()
  list.flatten([
    [
      Sample(members_held, [], members),
      Sample(members_online, [], online),
      Sample(sessions_held, [], roster.size()),
      Sample(member_list_ranges, [], read(found, member_list_ranges_slot)),
    ],
    list.map(modes, fn(mode) {
      Sample(
        in_mode,
        [#("mode", mode)],
        list.count(held, fn(seen) { seen == mode }),
      )
    }),
    fixed(
      found,
      member_list_ops,
      "op",
      member_list_op_names,
      member_list_ops_base,
    ),
    fixed(found, joins_admitted, "class", join_classes, joins_admitted_base),
    by_family(found, events, events_base),
    by_family(found, deliveries, deliveries_base),
  ])
}

fn fixed(
  found: Counters,
  name: String,
  label: String,
  values: List(String),
  base: Int,
) -> List(Sample) {
  list.index_map(values, fn(value, index) {
    Sample(name, [#(label, value)], read(found, base + index))
  })
}

fn by_family(found: Counters, name: String, base: Int) -> List(Sample) {
  list.index_map(families, fn(family, index) {
    Sample(name, [#("family", family)], read(found, base + index))
  })
  |> list.filter(fn(sample) { sample.value > 0 })
}

fn histograms() -> List(HistogramSeries) {
  let found = counters()
  list.flatten([
    list.index_map(families, fn(family, index) {
      histogram(
        found,
        dispatch_ms,
        [#("family", family)],
        dispatch_base + index * histogram_width,
      )
    }),
    [
      histogram(found, fanout, [], fanout_base),
      histogram(found, member_list_ms, [], member_list_base),
      histogram(found, join_wait_ms, [#("outcome", "joined")], join_joined_base),
      histogram(
        found,
        join_wait_ms,
        [#("outcome", "expired")],
        join_expired_base,
      ),
      histogram(found, join_batch_ms, [], join_batch_base),
    ],
  ])
  |> list.filter(fn(series) { series.count > 0 })
}

fn histogram(
  found: Counters,
  name: String,
  labels: List(#(String, String)),
  base: Int,
) -> HistogramSeries {
  let buckets = list.length(metrics.bucket_bounds_ms)
  HistogramSeries(
    name:,
    labels:,
    buckets: int.range(0, buckets, [], fn(acc, index) {
      [read(found, base + index), ..acc]
    })
      |> list.reverse,
    overflow: read(found, base + buckets),
    sum: read(found, base + histogram_width - 2),
    count: read(found, base + histogram_width - 1),
  )
}

fn held_modes() -> List(String) {
  let rows: List(#(Pid, String)) = table.to_list(mode_rows())
  list.filter_map(rows, fn(row) {
    case process.is_alive(row.0) {
      True -> Ok(row.1)
      False -> {
        table.delete_object(mode_rows(), row)
        Error(Nil)
      }
    }
  })
}

pub fn enter(mode: Mode) -> Nil {
  case mode {
    Serving -> table.delete(mode_rows(), process.self())
    Staged -> hold_mode("staged")
    Frozen -> hold_mode("frozen")
    Handing -> hold_mode("handing")
    Arriving -> hold_mode("arriving")
  }
}

fn hold_mode(name: String) -> Nil {
  case table.ensure_public(mode_rows()) {
    True -> table.insert(mode_rows(), #(process.self(), name))
    False -> Nil
  }
}

pub fn started(origin: Origin) -> Nil {
  let origin = case origin {
    FromApi -> "fetched"
    FromArrival -> "arrived"
    FromTransfer -> "transferred"
  }
  metrics.counter(starts, [#("origin", origin)], 1)
}

pub fn start_failed(reason: String) -> Nil {
  metrics.counter(start_failures, [#("reason", reason)], 1)
}

pub fn crash_restarted() -> Nil {
  metrics.counter(crash_restarts, [], 1)
}

pub fn stopped(reason: String) -> Nil {
  metrics.counter(stops, [#("reason", reason)], 1)
}

pub fn loaded(ok: Bool, elapsed_ms: Int) -> Nil {
  let outcome = case ok {
    True -> "ok"
    False -> "error"
  }
  metrics.observe_ms(load_ms, [#("outcome", outcome)], elapsed_ms)
}

pub fn loading_joins_dropped(count: Int) -> Nil {
  case count {
    0 -> Nil
    _ -> metrics.counter(loading_joins_expired, [], count)
  }
}

fn family_index(event: String) -> Int {
  case event {
    "MESSAGE_CREATE" -> 0
    "TYPING_START" -> 3
    "PRESENCE_UPDATE" -> 4
    "GUILD_MEMBER_LIST_UPDATE" -> 5
    "WEBHOOKS_UPDATE" -> 8
    "GUILD_EMOJIS_UPDATE" | "GUILD_STICKERS_UPDATE" -> 11
    "MESSAGE_REACTION_" <> _ -> 2
    "MESSAGE_" <> _ -> 1
    "GUILD_MEMBER" <> _ -> 6
    "GUILD_ROLE_" <> _ -> 7
    "CHANNEL_" <> _ -> 8
    "THREAD_" <> _ -> 9
    "VOICE_" <> _ -> 10
    "INVITE_" <> _ -> 12
    "GUILD_" <> _ -> 13
    _ -> family_count - 1
  }
}

pub fn event_handled(event: String, elapsed_ms: Int) -> Nil {
  let index = family_index(event)
  add(events_base + index, 1)
  observe(dispatch_base + index * histogram_width, elapsed_ms)
}

pub fn fanned_out(recipients: Int) -> Nil {
  observe(fanout_base, recipients)
}

pub fn delivered(event: String, count: Int) -> Nil {
  case count {
    0 -> Nil
    _ -> add(deliveries_base + family_index(event), count)
  }
}

pub type MemberListOp {
  Subscribe
  Unsubscribe
  Denied
}

pub fn member_list_op(op: MemberListOp) -> Nil {
  let index = case op {
    Subscribe -> 0
    Unsubscribe -> 1
    Denied -> 2
  }
  add(member_list_ops_base + index, 1)
}

pub fn member_list_subscribed(ranges: Int, elapsed_ms: Int) -> Nil {
  member_list_op(Subscribe)
  add(member_list_ranges_slot, ranges)
  observe(member_list_base, elapsed_ms)
}

pub fn join_admitted(class: JoinClass, waited_ms: Int) -> Nil {
  let index = case class {
    Resume -> 0
    Focused -> 1
    Background -> 2
  }
  add(joins_admitted_base + index, 1)
  observe(join_joined_base, waited_ms)
}

pub fn join_expired(waited_ms: Int) -> Nil {
  observe(join_expired_base, waited_ms)
}

pub fn join_batch(elapsed_ms: Int) -> Nil {
  observe(join_batch_base, elapsed_ms)
}
