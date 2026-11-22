// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/node.{type Role}
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/util/table
import gleam/dict.{type Dict}
import gleam/erlang/atom
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import gleam/string_tree.{type StringTree}

pub type Kind {
  Counter
  Gauge
  Histogram(bounds: List(Int))
}

pub type Source {
  Stored
  Computed
}

pub type Scope {
  Everywhere
  Runs(roles: List(Role))
}

pub type Seed {
  NoSeed
  Fixed(series: List(List(#(String, String))))
  PerKind(series: List(List(#(String, String))))
}

pub type Family {
  Family(
    name: String,
    kind: Kind,
    source: Source,
    help: String,
    labels: List(#(String, List(String))),
    scope: Scope,
    seed: Seed,
  )
}

pub type Sample {
  Sample(name: String, labels: List(#(String, String)), value: Int)
}

type Slot {
  Le(bound: Int)
  Inf
  Sum
  Count
}

type HistogramKey {
  HistogramKey(name: String, labels: List(#(String, String)), slot: Slot)
}

pub const allowed_labels = [
  "role", "pool", "outcome", "size_class", "reason", "kind",
]

pub const kinds = ["guild", "presence", "session", "call", "push", "other"]

pub const size_classes = ["under_64k", "under_1m", "under_8m", "over_8m"]

pub const drain_outcomes = "fluxer_gateway_drain_outcomes_total"

pub const drain_duration = "fluxer_gateway_drain_duration_ms"

pub const drain_entities = "fluxer_gateway_drain_entities_total"

pub const drain_left = "fluxer_gateway_drain_left"

pub const moves_started = "fluxer_gateway_moves_started_total"

pub const moves_committed = "fluxer_gateway_moves_committed_total"

pub const moves_aborted = "fluxer_gateway_moves_aborted_total"

pub const move_freeze = "fluxer_gateway_move_freeze_ms"

pub const move_duration = "fluxer_gateway_move_duration_ms"

pub const precopy_outcomes = "fluxer_gateway_precopy_total"

pub const precopy_rounds = "fluxer_gateway_precopy_rounds_total"

pub const precopy_bytes = "fluxer_gateway_precopy_bytes_total"

pub const precopy_delta_bytes = "fluxer_gateway_precopy_delta_bytes_total"

pub const stub_hits = "fluxer_gateway_forward_stub_hits_total"

pub const session_moves = "fluxer_gateway_session_moves_total"

pub const session_rebinds = "fluxer_gateway_session_rebinds_total"

pub const rebind_fallbacks = "fluxer_gateway_rebind_fallbacks_total"

pub const held_messages = "fluxer_gateway_session_move_held_messages_total"

pub const identify_after_crash = "fluxer_gateway_identify_after_crash_total"

pub const call_moves = "fluxer_gateway_call_moves_total"

pub const push_jobs_drain = "fluxer_gateway_push_jobs_drain_total"

pub const buckets_held = "fluxer_gateway_buckets_held"

pub const lease_lapses = "fluxer_gateway_lease_lapses_total"

pub const fences = "fluxer_gateway_bouncer_fences_total"

pub const unfences = "fluxer_gateway_bouncer_unfences_total"

pub const wire_refusals = "fluxer_gateway_wire_refusals_total"

pub const ready_reason = "fluxer_gateway_ready_reason"

pub const time_to_ready = "fluxer_gateway_time_to_ready_ms"

pub const op7_sent = "fluxer_gateway_op7_sent_total"

pub const resumed = "fluxer_gateway_resumed_total"

pub const invalid_session = "fluxer_gateway_invalid_session_total"

pub const socket_closes = "fluxer_gateway_socket_closes_total"

pub const dispatch_gaps = "fluxer_gateway_dispatch_gaps_total"

pub const joins_expired = "gateway_guild_joins_expired_total"

pub const presence_broadcast_drops =
  "fluxer_gateway_guild_presence_broadcast_dropped_total"

pub const abort_reasons = [
  "fenced", "lease_lost", "freeze_failed", "thawed", "refused",
  "admission_timeout", "aborted_early", "no_codec", "no_adapter", "timeout",
]

pub const frozen_abort_reasons = [
  "fenced", "lease_lost", "freeze_failed", "thawed",
]

pub const ready_reasons = [
  "ok", "draining", "epoch_mismatch", "joining", "nats_unavailable",
]

pub const fallback_reasons = [
  "not_rebound", "expired", "source_down", "not_adopted",
]

pub const close_reasons = [
  "4000", "4001", "4002", "4003", "4004", "4005", "4007", "4008", "4009", "4010",
  "4011", "4012",
]

const freeze_bounds = [50, 100, 250, 500, 1000, 2000, 5000]

const move_bounds = [100, 500, 1000, 2500, 5000, 15_000, 60_000]

const drain_bounds = [5000, 15_000, 30_000, 60_000, 120_000, 300_000, 600_000]

const ready_bounds = [5000, 15_000, 30_000, 60_000, 120_000, 300_000]

const movers = [node.Guilds, node.Presence, node.Calls, node.Push]

const sockets = [node.Websocket]

const sessions = [node.Sessions]

fn by_kind() -> #(String, List(String)) {
  #("kind", kinds)
}

fn one(label: String, values: List(String)) -> List(List(#(String, String))) {
  list.map(values, fn(value) { [#(label, value)] })
}

pub fn families() -> List(Family) {
  list.flatten([
    drain_families(),
    move_families(),
    session_families(),
    call_push_families(),
    lease_families(),
    readiness_families(),
    client_families(),
  ])
}

fn drain_families() -> List(Family) {
  [
    Family(
      drain_outcomes,
      Counter,
      Stored,
      "Finished drains by outcome and reason",
      [
        #("outcome", ["drained", "failed"]),
        #("reason", ["none", "fenced", "deadline"]),
      ],
      Everywhere,
      Fixed([
        [#("outcome", "drained"), #("reason", "none")],
        [#("outcome", "failed"), #("reason", "fenced")],
        [#("outcome", "failed"), #("reason", "deadline")],
      ]),
    ),
    Family(
      drain_duration,
      Histogram(drain_bounds),
      Stored,
      "Drain duration from start to outcome in milliseconds",
      [#("outcome", ["drained", "failed"])],
      Everywhere,
      Fixed(one("outcome", ["drained", "failed"])),
    ),
    Family(
      drain_entities,
      Counter,
      Stored,
      "Entities moved or failed while this pod drained, by kind",
      [by_kind(), #("outcome", ["moved", "failed"])],
      Everywhere,
      NoSeed,
    ),
    Family(
      drain_left,
      Gauge,
      Computed,
      "Entities and buckets this pod still holds, by kind",
      [by_kind()],
      Everywhere,
      NoSeed,
    ),
  ]
}

fn move_families() -> List(Family) {
  [
    Family(
      moves_started,
      Counter,
      Stored,
      "Entity moves started from this pod",
      [by_kind()],
      Runs(movers),
      PerKind([[]]),
    ),
    Family(
      moves_committed,
      Counter,
      Stored,
      "Entity moves committed at their target",
      [by_kind()],
      Runs(movers),
      PerKind([[]]),
    ),
    Family(
      moves_aborted,
      Counter,
      Stored,
      "Entity moves that did not commit, by kind and reason",
      [by_kind(), #("reason", abort_reasons)],
      Runs(movers),
      PerKind(one("reason", frozen_abort_reasons)),
    ),
    Family(
      move_freeze,
      Histogram(freeze_bounds),
      Stored,
      "Freeze of a committed move in milliseconds, by kind and tail size class",
      [by_kind(), #("size_class", size_classes)],
      Runs(movers),
      PerKind(one("size_class", size_classes)),
    ),
    Family(
      move_duration,
      Histogram(move_bounds),
      Stored,
      "Whole move from start to outcome in milliseconds, by kind",
      [by_kind()],
      Runs(movers),
      PerKind([[]]),
    ),
    Family(
      precopy_outcomes,
      Counter,
      Stored,
      "Pre-copy runs by kind and outcome",
      [by_kind(), #("outcome", ["staged", "failed"])],
      Runs(movers),
      NoSeed,
    ),
    Family(
      precopy_rounds,
      Counter,
      Stored,
      "Pre-copy rounds sent before a freeze",
      [by_kind()],
      Runs(movers),
      NoSeed,
    ),
    Family(
      precopy_bytes,
      Counter,
      Stored,
      "Bytes staged at the target by pre-copy",
      [by_kind()],
      Runs(movers),
      NoSeed,
    ),
    Family(
      precopy_delta_bytes,
      Counter,
      Stored,
      "Tail bytes sent during the freeze of a pre-copied move",
      [by_kind()],
      Runs(movers),
      NoSeed,
    ),
    Family(
      stub_hits,
      Counter,
      Stored,
      "Messages that reached a moving or moved entity and were passed on or dropped",
      [by_kind(), #("outcome", ["forwarded", "dropped"])],
      Runs(movers),
      NoSeed,
    ),
  ]
}

fn session_families() -> List(Family) {
  [
    Family(
      session_moves,
      Counter,
      Stored,
      "Session moves started by a drain, by outcome",
      [#("outcome", ["moved", "failed"])],
      Runs(sessions),
      Fixed(one("outcome", ["moved", "failed"])),
    ),
    Family(
      session_rebinds,
      Counter,
      Stored,
      "Guild and presence links of an arriving session, rebound or fallen back to a rejoin",
      [#("kind", ["guild", "presence"]), #("outcome", ["rebound", "fallback"])],
      Runs(sessions),
      Fixed([
        [#("kind", "guild"), #("outcome", "rebound")],
        [#("kind", "guild"), #("outcome", "fallback")],
        [#("kind", "presence"), #("outcome", "rebound")],
        [#("kind", "presence"), #("outcome", "fallback")],
      ]),
    ),
    Family(
      rebind_fallbacks,
      Counter,
      Stored,
      "Rebind fallbacks that replay GUILD_CREATE or reconnect presence, by kind and reason",
      [#("kind", ["guild", "presence"]), #("reason", fallback_reasons)],
      Runs(sessions),
      Fixed(
        list.flat_map(["guild", "presence"], fn(kind) {
          list.map(fallback_reasons, fn(reason) {
            [#("kind", kind), #("reason", reason)]
          })
        }),
      ),
    ),
    Family(
      held_messages,
      Counter,
      Stored,
      "Messages an arriving session held until its move settled",
      [],
      Runs(sessions),
      Fixed([[]]),
    ),
  ]
}

fn call_push_families() -> List(Family) {
  [
    Family(
      call_moves,
      Counter,
      Stored,
      "Calls that survived a move or ended cold at the target",
      [#("outcome", ["survived", "ended"])],
      Runs([node.Calls]),
      Fixed(one("outcome", ["survived", "ended"])),
    ),
    Family(
      push_jobs_drain,
      Counter,
      Stored,
      "Push jobs during a drain, moved to a peer, finished here or lost",
      [#("outcome", ["moved", "finished", "lost"])],
      Runs([node.Calls, node.Push]),
      Fixed(one("outcome", ["moved", "finished", "lost"])),
    ),
  ]
}

fn lease_families() -> List(Family) {
  [
    Family(
      buckets_held,
      Gauge,
      Computed,
      "Buckets this worker holds a lease on, by kind",
      [by_kind()],
      Runs(movers),
      NoSeed,
    ),
    Family(
      lease_lapses,
      Counter,
      Stored,
      "Times this worker lost sight of a bouncer majority while holding leases",
      [],
      Everywhere,
      Fixed([[]]),
    ),
    Family(
      fences,
      Counter,
      Stored,
      "Times this worker fenced its leases, by reason",
      [#("reason", ["isolation_cap", "majority_sees_bouncers"])],
      Everywhere,
      Fixed(one("reason", ["isolation_cap", "majority_sees_bouncers"])),
    ),
    Family(
      unfences,
      Counter,
      Stored,
      "Times a fenced worker was welcomed again by a bouncer majority, by reason",
      [#("reason", ["rewelcomed"])],
      Everywhere,
      Fixed(one("reason", ["rewelcomed"])),
    ),
    Family(
      wire_refusals,
      Counter,
      Stored,
      "Peers refused for an incompatible wire epoch, by whether theirs is older or newer",
      [#("reason", ["older", "newer"])],
      Everywhere,
      Fixed(one("reason", ["older", "newer"])),
    ),
    Family(
      joins_expired,
      Counter,
      Stored,
      "Guild joins that expired before the guild answered",
      [],
      Runs([node.Guilds]),
      Fixed([[]]),
    ),
  ]
}

fn readiness_families() -> List(Family) {
  [
    Family(
      ready_reason,
      Gauge,
      Computed,
      "1 for the reason readiness gives now, 0 for the others",
      [#("reason", ready_reasons)],
      Everywhere,
      NoSeed,
    ),
    Family(
      time_to_ready,
      Histogram(ready_bounds),
      Stored,
      "Milliseconds from VM start to the first ready answer",
      [],
      Everywhere,
      NoSeed,
    ),
  ]
}

fn client_families() -> List(Family) {
  [
    Family(
      op7_sent,
      Counter,
      Stored,
      "op 7 RECONNECT frames sent to clients, by reason",
      [#("reason", ["drain", "replaced", "move_unconfirmed", "replay_gap"])],
      Runs([node.Websocket, node.Sessions]),
      Fixed(
        one("reason", ["drain", "replaced", "move_unconfirmed", "replay_gap"]),
      ),
    ),
    Family(
      resumed,
      Counter,
      Stored,
      "RESUME requests answered with a resumed session",
      [],
      Runs(sockets),
      Fixed([[]]),
    ),
    Family(
      invalid_session,
      Counter,
      Stored,
      "INVALID_SESSION frames sent, by reason",
      [#("reason", ["bad_session_id", "session_gone", "session_down"])],
      Runs(sockets),
      Fixed(one("reason", ["bad_session_id", "session_gone", "session_down"])),
    ),
    Family(
      identify_after_crash,
      Counter,
      Stored,
      "Sockets told to IDENTIFY again because their session process crashed",
      [],
      Runs(sockets),
      Fixed([[]]),
    ),
    Family(
      socket_closes,
      Counter,
      Stored,
      "Sockets closed by the gateway, by close code",
      [#("reason", close_reasons)],
      Runs(sockets),
      NoSeed,
    ),
    Family(
      presence_broadcast_drops,
      Counter,
      Stored,
      "Guild PRESENCE_UPDATE broadcasts shed because the guild broadcaster was backlogged",
      [],
      Runs([node.Guilds]),
      Fixed([[]]),
    ),
    Family(
      dispatch_gaps,
      Counter,
      Stored,
      "Reattaches whose sequence could not be replayed, by reason",
      [#("reason", ["ahead", "acked", "evicted"])],
      Runs(sessions),
      NoSeed,
    ),
  ]
}

type CatalogueKey {
  FluxerGatewayRolloutCatalogue
}

@external(erlang, "persistent_term", "get")
fn cached(
  key: CatalogueKey,
  default: Dict(String, Family),
) -> Dict(String, Family)

@external(erlang, "persistent_term", "put")
fn cache(key: CatalogueKey, value: Dict(String, Family)) -> Nil

fn catalogue() -> Dict(String, Family) {
  let found = cached(FluxerGatewayRolloutCatalogue, dict.new())
  case dict.is_empty(found) {
    False -> found
    True -> {
      let built =
        families()
        |> list.map(fn(entry) { #(entry.name, entry) })
        |> dict.from_list
      cache(FluxerGatewayRolloutCatalogue, built)
      built
    }
  }
}

pub fn family(name: String) -> Result(Family, Nil) {
  dict.get(catalogue(), name)
}

fn clean(
  found: Family,
  labels: List(#(String, String)),
) -> List(#(String, String)) {
  labels
  |> list.filter_map(fn(label) {
    let #(key, value) = label
    case list.key_find(found.labels, key) {
      Ok(values) ->
        case list.contains(values, value) {
          True -> Ok(#(key, value))
          False -> Ok(#(key, "other"))
        }
      Error(Nil) -> Error(Nil)
    }
  })
  |> metrics.normalise
}

pub fn count(name: String, labels: List(#(String, String)), by: Int) -> Nil {
  case family(name), by > 0 {
    Ok(Family(kind: Counter, source: Stored, ..) as found), True ->
      metrics.counter(name, clean(found, labels), by)
    _, _ -> Nil
  }
}

pub fn set(name: String, labels: List(#(String, String)), value: Int) -> Nil {
  case family(name) {
    Ok(Family(kind: Gauge, source: Stored, ..) as found) ->
      metrics.gauge(name, clean(found, labels), value)
    _ -> Nil
  }
}

fn histogram_table() -> table.Table {
  table.named("fluxer_gateway_rollout_histograms")
}

pub fn observe(
  name: String,
  labels: List(#(String, String)),
  value: Int,
) -> Nil {
  case family(name), table.ensure_public(histogram_table()) {
    Ok(Family(kind: Histogram(bounds), ..) as found), True -> {
      let labels = clean(found, labels)
      let value = int.max(0, value)
      let bump = fn(slot, by) {
        let key = HistogramKey(name, labels, slot)
        let _ = table.update_counter(histogram_table(), key, by, #(key, 0))
        Nil
      }
      list.each(bounds, fn(bound) {
        case value <= bound {
          True -> bump(Le(bound), 1)
          False -> Nil
        }
      })
      bump(Inf, 1)
      bump(Sum, value)
      bump(Count, 1)
    }
    _, _ -> Nil
  }
}

type ScrapeKey {
  LastScrape
}

fn scrape_table() -> table.Table {
  table.named("fluxer_gateway_rollout_scrapes")
}

pub fn mark_scraped(now_ms: Int) -> Nil {
  case table.ensure_public(scrape_table()) {
    True -> table.insert(scrape_table(), #(LastScrape, now_ms))
    False -> Nil
  }
}

pub fn last_scrape_ms() -> Int {
  case table.ensure_public(scrape_table()) {
    True ->
      case table.lookup(scrape_table(), LastScrape) {
        [#(LastScrape, at), ..] -> at
        [] -> 0
      }
    False -> 0
  }
}

type ReadyKey {
  FluxerGatewayReadySeen
}

@external(erlang, "persistent_term", "get")
fn ready_latched(key: ReadyKey, default: Bool) -> Bool

@external(erlang, "persistent_term", "put")
fn latch_ready(key: ReadyKey, value: Bool) -> Nil

@external(erlang, "erlang", "statistics")
fn statistics(item: atom.Atom) -> #(Int, Int)

pub fn uptime_ms() -> Int {
  statistics(atom.create("wall_clock")).0
}

pub fn ready_seen(ready: Bool) -> Nil {
  case ready && !ready_latched(FluxerGatewayReadySeen, False) {
    True -> {
      latch_ready(FluxerGatewayReadySeen, True)
      observe(time_to_ready, [], uptime_ms())
    }
    False -> Nil
  }
}

pub fn kind_of_ns(ns: Int) -> String {
  case ns {
    0 -> "guild"
    1 -> "session"
    2 -> "presence"
    3 -> "call"
    4 -> "push"
    _ -> "other"
  }
}

pub fn size_class(bytes: Int) -> String {
  case bytes {
    _ if bytes < 65_536 -> "under_64k"
    _ if bytes < 1_048_576 -> "under_1m"
    _ if bytes < 8_388_608 -> "under_8m"
    _ -> "over_8m"
  }
}

pub fn role_kinds(role: Role) -> List(String) {
  [
    #(node.Guilds, "guild"),
    #(node.Presence, "presence"),
    #(node.Calls, "call"),
    #(node.Push, "push"),
  ]
  |> list.filter_map(fn(pair) {
    case node.runs(role, pair.0) {
      True -> Ok(pair.1)
      False -> Error(Nil)
    }
  })
}

pub fn in_scope(found: Family, role: Role) -> Bool {
  case found.scope {
    Everywhere -> True
    Runs(roles) -> list.any(roles, fn(wanted) { node.runs(role, wanted) })
  }
}

fn seeded(found: Family, role: Role) -> List(List(#(String, String))) {
  case found.seed, in_scope(found, role) {
    _, False | NoSeed, True -> []
    Fixed(series), True -> list.map(series, metrics.normalise)
    PerKind(series), True ->
      list.flat_map(role_kinds(role), fn(kind) {
        list.map(series, fn(labels) {
          metrics.normalise([#("kind", kind), ..labels])
        })
      })
  }
}

fn stored(found: Family) -> List(#(List(#(String, String)), Int)) {
  metrics.registry_samples()
  |> list.filter_map(fn(entry) {
    let #(_, sample) = entry
    case sample.name == found.name {
      True -> Ok(#(metrics.normalise(sample.labels), sample.value))
      False -> Error(Nil)
    }
  })
}

pub fn series(
  found: Family,
  role: Role,
  computed: List(Sample),
) -> List(#(List(#(String, String)), Int)) {
  case found.kind, found.source {
    Histogram(_), _ -> []
    _, Computed ->
      computed
      |> list.filter(fn(sample) { sample.name == found.name })
      |> list.map(fn(sample) { #(clean(found, sample.labels), sample.value) })
    _, Stored -> {
      let present = case in_scope(found, role) {
        True -> stored(found)
        False -> []
      }
      let missing =
        seeded(found, role)
        |> list.filter(fn(labels) {
          !result.is_ok(list.key_find(present, labels))
        })
        |> list.map(fn(labels) { #(labels, 0) })
      list.append(present, missing)
      |> list.sort(fn(a, b) {
        string.compare(label_text([], a.0), label_text([], b.0))
      })
    }
  }
}

type HistogramRow =
  #(HistogramKey, Int)

pub fn observed(name: String, labels: List(#(String, String))) -> Int {
  let labels = metrics.normalise(labels)
  histogram_rows(name)
  |> list.fold(0, fn(total, row) {
    case { row.0 }.labels == labels && { row.0 }.slot == Count {
      True -> total + row.1
      False -> total
    }
  })
}

pub fn total(name: String, labels: List(#(String, String))) -> Int {
  metrics.total(name, fn(found) {
    list.all(labels, fn(label) { list.contains(found, label) })
  })
}

fn histogram_rows(name: String) -> List(HistogramRow) {
  let rows: List(HistogramRow) = table.to_list(histogram_table())
  list.filter(rows, fn(row) { { row.0 }.name == name })
}

fn histogram_lines(
  found: Family,
  bounds: List(Int),
  constant: List(#(String, String)),
  seeds: List(List(#(String, String))),
) -> List(StringTree) {
  let rows = histogram_rows(found.name)
  rows
  |> list.map(fn(row) { { row.0 }.labels })
  |> list.append(seeds)
  |> list.unique
  |> list.sort(fn(a, b) { string.compare(label_text([], a), label_text([], b)) })
  |> list.flat_map(fn(labels) {
    let slot = fn(wanted: Slot) {
      list.fold(rows, 0, fn(total, row) {
        case { row.0 }.labels == labels && { row.0 }.slot == wanted {
          True -> total + row.1
          False -> total
        }
      })
    }
    list.flatten([
      list.map(bounds, fn(bound) {
        line(
          found.name <> "_bucket",
          constant,
          list.append(labels, [#("le", int.to_string(bound))]),
          slot(Le(bound)),
        )
      }),
      [
        line(
          found.name <> "_bucket",
          constant,
          list.append(labels, [#("le", "+Inf")]),
          slot(Inf),
        ),
        line(found.name <> "_sum", constant, labels, slot(Sum)),
        line(found.name <> "_count", constant, labels, slot(Count)),
      ],
    ])
  })
}

pub fn render_family(
  found: Family,
  role: Role,
  constant: List(#(String, String)),
  computed: List(Sample),
) -> StringTree {
  let body = case found.kind {
    Histogram(bounds) ->
      case in_scope(found, role) {
        True -> histogram_lines(found, bounds, constant, seeded(found, role))
        False -> []
      }
    Counter | Gauge ->
      series(found, role, computed)
      |> list.map(fn(entry) { line(found.name, constant, entry.0, entry.1) })
  }
  string_tree.concat([header(found.name, found.kind, found.help), ..body])
}

pub fn render(
  catalogue: List(Family),
  role: Role,
  constant: List(#(String, String)),
  computed: List(Sample),
) -> String {
  catalogue
  |> list.map(render_family(_, role, constant, computed))
  |> string_tree.concat
  |> string_tree.to_string
}

fn kind_name(kind: Kind) -> String {
  case kind {
    Counter -> "counter"
    Gauge -> "gauge"
    Histogram(_) -> "histogram"
  }
}

fn header(name: String, kind: Kind, help: String) -> StringTree {
  string_tree.from_strings([
    "# HELP ",
    name,
    " ",
    help,
    "\n",
    "# TYPE ",
    name,
    " ",
    kind_name(kind),
    "\n",
  ])
}

fn line(
  name: String,
  constant: List(#(String, String)),
  labels: List(#(String, String)),
  value: Int,
) -> StringTree {
  string_tree.from_strings([
    name,
    label_text(constant, labels),
    " ",
    int.to_string(value),
    "\n",
  ])
}

fn label_text(
  constant: List(#(String, String)),
  labels: List(#(String, String)),
) -> String {
  case list.append(constant, labels) {
    [] -> ""
    found ->
      "{"
      <> found
      |> list.map(fn(label) { label.0 <> "=\"" <> escape(label.1) <> "\"" })
      |> string.join(",")
      <> "}"
  }
}

fn escape(value: String) -> String {
  value
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
}
