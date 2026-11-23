// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/status
import fluxer_gateway/core/node
import fluxer_gateway/push/metrics as push_metrics
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/prometheus
import fluxer_gateway/telemetry/rollout_metrics
import gleam/bit_array
import gleam/bytes_tree
import gleam/dict.{type Dict}
import gleam/http
import gleam/http/request
import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string
import glisten

const roles = [
  node.AllRoles,
  node.Websocket,
  node.Sessions,
  node.WebsocketSessions,
  node.Presence,
  node.Guilds,
  node.Calls,
  node.Push,
]

type Series {
  Series(name: String, labels: List(#(String, String)), value: Int)
}

type Exposition {
  Exposition(families: Dict(String, String), series: List(Series))
}

type Parser {
  Parser(
    families: Dict(String, String),
    order: List(String),
    helped: Set(String),
    current: Result(#(String, String), Nil),
    keys: Set(#(String, List(#(String, String)))),
    series: List(Series),
  )
}

fn scrape() -> String {
  let response =
    request.new()
    |> request.set_method(http.Get)
    |> request.set_path("/_metrics")
    |> request.set_body(glisten.IpV4(10, 0, 0, 7))
    |> status.handle
  assert response.status == 200
  let assert Ok(text) =
    bit_array.to_string(bytes_tree.to_bit_array(response.body))
  text
}

fn strict(text: String) -> Result(Exposition, String) {
  use <- guard(string.ends_with(text, "\n"), "no final newline")
  let lines = string.split(string.drop_end(text, 1), "\n")
  let start = Parser(dict.new(), [], set.new(), Error(Nil), set.new(), [])
  use parsed <- result.try(list.try_fold(lines, start, step))
  use <- guard(
    set.size(parsed.helped) == dict.size(parsed.families),
    "a family has TYPE without HELP",
  )
  let claimed =
    dict.to_list(parsed.families)
    |> list.flat_map(fn(family) {
      case family.1 {
        "histogram" -> [
          family.0,
          family.0 <> "_bucket",
          family.0 <> "_sum",
          family.0 <> "_count",
        ]
        _ -> [family.0]
      }
    })
  use <- guard(
    list.length(list.unique(claimed)) == list.length(claimed),
    "two families claim one name",
  )
  Ok(Exposition(parsed.families, list.reverse(parsed.series)))
}

fn guard(holds: Bool, reason: String, next: fn() -> Result(a, String)) {
  case holds {
    True -> next()
    False -> Error(reason)
  }
}

fn step(parser: Parser, line: String) -> Result(Parser, String) {
  case string.split_once(line, " ") {
    Ok(#("#", rest)) -> comment(parser, rest, line)
    Ok(#(series, value)) -> sample(parser, series, value, line)
    Error(Nil) -> Error("not a sample: " <> line)
  }
}

fn comment(
  parser: Parser,
  rest: String,
  line: String,
) -> Result(Parser, String) {
  case string.split_once(rest, " ") {
    Ok(#("HELP", body)) -> {
      let #(name, help) =
        result.unwrap(string.split_once(body, " "), #(body, ""))
      use <- guard(metric_name(name), "bad name: " <> line)
      use <- guard(help != "", "empty HELP: " <> line)
      use <- guard(!set.contains(parser.helped, name), "HELP twice: " <> name)
      use <- guard(
        !dict.has_key(parser.families, name),
        "HELP after TYPE: " <> name,
      )
      Ok(
        Parser(
          ..parser,
          helped: set.insert(parser.helped, name),
          current: Error(Nil),
        ),
      )
    }
    Ok(#("TYPE", body)) -> {
      use #(name, kind) <- result.try(
        string.split_once(body, " ")
        |> result.replace_error("bad TYPE: " <> line),
      )
      use <- guard(metric_name(name), "bad name: " <> line)
      use <- guard(
        list.contains(["counter", "gauge", "histogram"], kind),
        "bad kind: " <> line,
      )
      use <- guard(
        !dict.has_key(parser.families, name),
        "family twice: " <> name,
      )
      use <- guard(
        set.contains(parser.helped, name),
        "TYPE before HELP: " <> name,
      )
      Ok(
        Parser(
          ..parser,
          families: dict.insert(parser.families, name, kind),
          order: [name, ..parser.order],
          current: Ok(#(name, kind)),
        ),
      )
    }
    _ -> Error("stray comment: " <> line)
  }
}

fn sample(
  parser: Parser,
  series: String,
  value: String,
  line: String,
) -> Result(Parser, String) {
  use parsed <- result.try(
    int.parse(value) |> result.replace_error("bad value: " <> line),
  )
  use #(name, labels) <- result.try(split_series(series, line))
  use #(family, kind) <- result.try(
    parser.current |> result.replace_error("sample before TYPE: " <> line),
  )
  let suffixes = case kind {
    "histogram" -> ["_bucket", "_sum", "_count"]
    _ -> [""]
  }
  use <- guard(
    list.any(suffixes, fn(suffix) { name == family <> suffix }),
    "sample outside its family " <> family <> ": " <> line,
  )
  let keys = list.map(labels, fn(label) { label.0 })
  use <- guard(list.unique(keys) == keys, "label twice: " <> line)
  use <- guard(
    kind != "histogram"
      || { name == family <> "_bucket" } == list.contains(keys, "le"),
    "le misplaced: " <> line,
  )
  let key = #(name, metrics.normalise(labels))
  use <- guard(!set.contains(parser.keys, key), "series twice: " <> line)
  Ok(
    Parser(..parser, keys: set.insert(parser.keys, key), series: [
      Series(name, labels, parsed),
      ..parser.series
    ]),
  )
}

fn split_series(
  series: String,
  line: String,
) -> Result(#(String, List(#(String, String))), String) {
  case string.split_once(series, "{") {
    Error(Nil) ->
      case metric_name(series) {
        True -> Ok(#(series, []))
        False -> Error("bad name: " <> line)
      }
    Ok(#(name, rest)) -> {
      use <- guard(metric_name(name), "bad name: " <> line)
      use <- guard(string.ends_with(rest, "}"), "unclosed labels: " <> line)
      label_pairs(string.drop_end(rest, 1), [], line)
      |> result.map(fn(labels) { #(name, labels) })
    }
  }
}

fn label_pairs(
  text: String,
  found: List(#(String, String)),
  line: String,
) -> Result(List(#(String, String)), String) {
  use #(key, rest) <- result.try(
    string.split_once(text, "=\"")
    |> result.replace_error("bad label: " <> line),
  )
  use <- guard(
    label_name(key) && !string.starts_with(key, "__"),
    "bad label name: " <> line,
  )
  use #(value, rest) <- result.try(quoted(rest, "", line))
  let found = [#(key, value), ..found]
  case rest {
    "" -> Ok(list.reverse(found))
    "," <> more -> label_pairs(more, found, line)
    _ -> Error("bad label separator: " <> line)
  }
}

fn quoted(
  text: String,
  value: String,
  line: String,
) -> Result(#(String, String), String) {
  case text {
    "\"" <> rest -> Ok(#(value, rest))
    "\\\\" <> rest -> quoted(rest, value <> "\\", line)
    "\\\"" <> rest -> quoted(rest, value <> "\"", line)
    "\\n" <> rest -> quoted(rest, value <> "\n", line)
    "\\" <> _ -> Error("bad escape: " <> line)
    "\n" <> _ -> Error("raw newline: " <> line)
    _ ->
      case string.pop_grapheme(text) {
        Ok(#(char, rest)) -> quoted(rest, value <> char, line)
        Error(Nil) -> Error("unterminated label value: " <> line)
      }
  }
}

fn metric_name(name: String) -> Bool {
  identifier(name, True)
}

fn label_name(name: String) -> Bool {
  identifier(name, False)
}

fn identifier(name: String, colon: Bool) -> Bool {
  case string.to_utf_codepoints(name) {
    [] -> False
    [first, ..rest] -> {
      let start = fn(code) {
        { code >= 65 && code <= 90 }
        || { code >= 97 && code <= 122 }
        || code == 95
        || { colon && code == 58 }
      }
      let first = string.utf_codepoint_to_int(first)
      start(first)
      && list.all(rest, fn(point) {
        let code = string.utf_codepoint_to_int(point)
        start(code) || { code >= 48 && code <= 57 }
      })
    }
  }
}

fn parsed(text: String) -> Exposition {
  case strict(text) {
    Ok(found) -> found
    Error(reason) -> {
      assert reason == ""
      Exposition(dict.new(), [])
    }
  }
}

fn value_of(
  found: Exposition,
  name: String,
  labels: List(#(String, String)),
) -> Int {
  let labels = metrics.normalise(labels)
  found.series
  |> list.find(fn(entry) {
    entry.name == name
    && metrics.normalise(
      list.filter(entry.labels, fn(label) {
        label.0 != "role" && label.0 != "pool"
      }),
    )
    == labels
  })
  |> result.map(fn(entry) { entry.value })
  |> result.unwrap(0)
}

pub fn push_counters_reach_the_status_port_test() {
  let dropped = [
    #("kind", "message"),
    #("reason", "expired"),
    #("class", "direct"),
  ]
  let before = parsed(scrape())
  push_metrics.count("push_outbox_attempts_total", 3)
  push_metrics.count("push_outbox_handed_total", 2)
  push_metrics.count_labelled("push_outbox_dropped_total", dropped, 1)
  let after = parsed(scrape())
  assert dict.get(after.families, "fluxer_gateway_push_outbox_attempts_total")
    == Ok("counter")
  assert dict.get(after.families, "fluxer_gateway_push_outbox_dropped_total")
    == Ok("counter")
  assert value_of(after, "fluxer_gateway_push_outbox_attempts_total", [])
    == value_of(before, "fluxer_gateway_push_outbox_attempts_total", []) + 3
  assert value_of(after, "fluxer_gateway_push_outbox_handed_total", [])
    == value_of(before, "fluxer_gateway_push_outbox_handed_total", []) + 2
  assert value_of(after, "fluxer_gateway_push_outbox_dropped_total", dropped)
    == value_of(before, "fluxer_gateway_push_outbox_dropped_total", dropped) + 1
}

fn seed_overlaps() -> Nil {
  rollout_metrics.count(
    rollout_metrics.push_jobs_drain,
    [#("outcome", "lost")],
    1,
  )
  rollout_metrics.count(rollout_metrics.joins_expired, [], 1)
  metrics.counter(rollout_metrics.socket_closes, [#("reason", "4000")], 1)
  metrics.gauge("fluxer_gateway_status_ready", [], 1)
  metrics.observe_ms("fluxer_gateway_status_metrics_fixture_ms", [], 7)
  push_metrics.count("push_outbox_attempts_total", 1)
}

pub fn the_status_port_serves_every_family_once_for_every_role_test() {
  seed_overlaps()
  let general =
    parsed(prometheus.render()).families
    |> dict.keys
    |> list.filter(fn(name) { !list.contains(status.unscraped, name) })
  list.each(roles, fn(role) {
    let found = parsed(status.render_metrics(status.snapshot(), role))
    list.each(general, fn(name) {
      assert dict.has_key(found.families, name)
    })
    list.each(status.catalogue(status.snapshot()), fn(family) {
      assert dict.has_key(found.families, family.name)
    })
  })
  let served = parsed(scrape())
  assert dict.has_key(served.families, "erlang_vm_memory_bytes")
  assert dict.has_key(served.families, "fluxer_gateway_processes")
  assert dict.has_key(served.families, rollout_metrics.push_jobs_drain)
}

pub fn only_the_loopback_port_serves_the_unscraped_families_test() {
  let peer = node.to_string(node.self())
  metrics.counter(
    "fluxer_gateway_handoff_moves_total",
    [#("ns", "0"), #("outcome", "moved")],
    1,
  )
  metrics.gauge(
    "fluxer_gateway_cluster_peer_state",
    [#("peer", peer), #("state", "up")],
    1,
  )
  metrics.counter(
    "fluxer_gateway_cluster_nodedown_total",
    [#("peer", peer), #("reason", "other")],
    1,
  )
  metrics.counter(
    "fluxer_gateway_wire_epoch_refused_peers_total",
    [#("local_epoch", "5"), #("peer_epoch", "999")],
    1,
  )
  metrics.counter(
    "fluxer_gateway_bouncer_beat_mismatch_total",
    [#("bouncer", "7")],
    1,
  )
  metrics.counter("fluxer_gateway_handoff_retries_total", [#("ns", "0")], 1)
  metrics.observe_ms(
    "fluxer_gateway_handoff_freeze_ms",
    [#("ns", "0"), #("size", "under_64k")],
    3,
  )
  let loopback = parsed(prometheus.render())
  let text = scrape()
  let served = parsed(text)
  list.each(
    [
      "fluxer_gateway_handoff_moves_total",
      "fluxer_gateway_cluster_peer_state",
      "fluxer_gateway_cluster_nodedown_total",
      "fluxer_gateway_wire_epoch_refused_peers_total",
      "fluxer_gateway_bouncer_beat_mismatch_total",
      "fluxer_gateway_handoff_retries_total",
      "fluxer_gateway_handoff_freeze_ms",
    ],
    fn(name) {
      assert list.contains(status.unscraped, name)
      assert dict.has_key(loopback.families, name)
      assert !dict.has_key(served.families, name)
    },
  )
  assert !string.contains(text, peer)
  assert !string.contains(text, "ns=")
}

pub fn the_strict_parser_refuses_duplicates_and_junk_test() {
  let family = "# HELP a_total a\n# TYPE a_total counter\na_total 1\n"
  assert result.is_ok(strict(family))
  assert result.is_error(strict(family <> family))
  assert result.is_error(strict(family <> "a_total 2\n"))
  assert result.is_error(strict("a_total 1\n"))
  assert result.is_error(strict("# TYPE a_total counter\na_total 1\n"))
  assert result.is_error(strict(
    "# HELP a_total a\n# TYPE a_total counter\na_total{x=\"1\",x=\"2\"} 1\n",
  ))
  assert result.is_error(strict(
    "# HELP a_total a\n# TYPE a_total counter\nb_total 1\n",
  ))
  assert result.is_error(strict(
    "# HELP a_total a\n# TYPE a_total counter\na_total{x=\"1} 1\n",
  ))
  assert result.is_error(strict(
    "# HELP a_total a\n# TYPE a_total counter\na_total one\n",
  ))
  assert result.is_error(strict(string.drop_end(family, 1)))
  assert result.is_error(strict(
    "# HELP a a\n# TYPE a histogram\na_count 1\n# HELP a_count c\n# TYPE a_count counter\na_count 1\n",
  ))
}
