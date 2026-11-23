// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/metrics as api_metrics
import fluxer_gateway/app/status
import fluxer_gateway/core/node
import fluxer_gateway/gateway/socket_metrics
import fluxer_gateway/guild/metrics as guild_metrics
import fluxer_gateway/nats/metrics as nats_metrics
import fluxer_gateway/presence/counters as presence_counters
import fluxer_gateway/push/metrics as push_metrics
import fluxer_gateway/push/outbox/state as outbox_state
import fluxer_gateway/rpc/metrics as rpc_metrics
import fluxer_gateway/session/session_metrics
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/prometheus
import fluxer_gateway/telemetry/resident
import fluxer_gateway/telemetry/rollout_metrics
import fluxer_gateway/telemetry/vm
import fluxer_gateway/telemetry/watchdog
import fluxer_gateway/voice/metrics as voice_metrics
import gleam/bit_array
import gleam/bytes_tree
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
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

@external(erlang, "erlang", "register")
fn register(name: Atom, pid: Pid) -> Bool

@external(erlang, "erlang", "send")
fn raw_send(pid: Pid, message: a) -> a

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

fn has_series(
  found: Exposition,
  name: String,
  labels: List(#(String, String)),
) -> Bool {
  let labels = metrics.normalise(labels)
  list.any(found.series, fn(entry) {
    entry.name == name
    && metrics.normalise(
      list.filter(entry.labels, fn(label) {
        label.0 != "role" && label.0 != "pool"
      }),
    )
    == labels
  })
}

pub fn socket_and_session_families_reach_the_status_port_test() {
  socket_metrics.track(process.self(), socket_metrics.Attached)
  socket_metrics.dispatch_sent("READY")
  socket_metrics.identify_to_ready(42)
  socket_metrics.resume_to_resumed(7)
  session_metrics.register()
  session_metrics.replayed(session_metrics.ResumeReplay, 3)
  session_metrics.moved(True, 120)
  let served = parsed(scrape())
  list.each(socket_metrics.counter_families(), fn(entry) {
    let #(family, _) = entry
    assert dict.get(served.families, family.name) == Ok("counter")
    list.each(family.values, fn(found) {
      assert has_series(served, family.name, [#(family.label, found)])
    })
  })
  let states = socket_metrics.connection_states()
  assert dict.get(served.families, states.name) == Ok("gauge")
  list.each(states.values, fn(found) {
    assert has_series(served, states.name, [#(states.label, found)])
  })
  assert value_of(served, states.name, [#("state", "attached")]) >= 1
  assert dict.get(served.families, socket_metrics.send_failures)
    == Ok("counter")
  list.each(
    [
      socket_metrics.process_memory,
      socket_metrics.mailbox_messages,
      socket_metrics.mailbox_max_messages,
      session_metrics.detached,
      session_metrics.process_memory,
      session_metrics.mailbox_messages,
      session_metrics.mailbox_max_messages,
    ],
    fn(name) {
      assert dict.get(served.families, name) == Ok("gauge")
    },
  )
  assert value_of(served, socket_metrics.process_memory, []) > 0
  list.each(
    [session_metrics.replays, session_metrics.replayed_dispatches],
    fn(name) {
      assert dict.get(served.families, name) == Ok("counter")
      list.each(session_metrics.replay_paths, fn(path) {
        assert has_series(served, name, [#("path", path)])
      })
    },
  )
  list.each(
    [
      socket_metrics.identify_ready_ms,
      socket_metrics.resume_ms,
      session_metrics.move_ms,
    ],
    fn(name) {
      assert dict.get(served.families, name) == Ok("histogram")
    },
  )
  socket_metrics.untrack(process.self())
}

pub fn guild_families_reach_the_status_port_test() {
  guild_metrics.register()
  let family = [#("family", "message_create")]
  let before = parsed(scrape())
  guild_metrics.event_handled("MESSAGE_CREATE", 4)
  guild_metrics.fanned_out(12)
  guild_metrics.delivered("MESSAGE_CREATE", 12)
  guild_metrics.join_batch(2)
  let after = parsed(scrape())
  assert dict.get(after.families, guild_metrics.events) == Ok("counter")
  assert dict.get(after.families, guild_metrics.deliveries) == Ok("counter")
  assert dict.get(after.families, guild_metrics.dispatch_ms) == Ok("histogram")
  assert dict.get(after.families, guild_metrics.fanout) == Ok("histogram")
  assert dict.get(after.families, guild_metrics.members_held) == Ok("gauge")
  assert dict.get(after.families, guild_metrics.in_mode) == Ok("gauge")
  assert dict.get(after.families, guild_metrics.starts) == Ok("counter")
  assert dict.get(after.families, guild_metrics.member_list_ops)
    == Ok("counter")
  assert value_of(after, guild_metrics.events, family)
    == value_of(before, guild_metrics.events, family) + 1
  assert value_of(after, guild_metrics.deliveries, family)
    == value_of(before, guild_metrics.deliveries, family) + 12
  assert value_of(after, guild_metrics.dispatch_ms <> "_count", family)
    == value_of(before, guild_metrics.dispatch_ms <> "_count", family) + 1
  assert value_of(after, guild_metrics.fanout <> "_bucket", [#("le", "25")])
    == value_of(before, guild_metrics.fanout <> "_bucket", [#("le", "25")]) + 1
  assert value_of(after, guild_metrics.fanout <> "_bucket", [#("le", "10")])
    == value_of(before, guild_metrics.fanout <> "_bucket", [#("le", "10")])
  list.each(guild_metrics.modes, fn(mode) {
    assert list.any(after.series, fn(entry) {
      entry.name == guild_metrics.in_mode
      && list.contains(entry.labels, #("mode", mode))
    })
  })
  list.each(guild_metrics.origins, fn(origin) {
    assert list.any(after.series, fn(entry) {
      entry.name == guild_metrics.starts
      && list.contains(entry.labels, #("origin", origin))
    })
  })
}

pub fn presence_and_voice_families_reach_the_status_port_test() {
  let before = parsed(scrape())
  presence_counters.input(presence_counters.ForeignInput)
  presence_counters.published()
  presence_counters.delivered(presence_counters.NodeDelivery, 3)
  presence_counters.started(presence_counters.StartCalled, 4)
  presence_counters.looked_up(presence_counters.LookupFound, 2)
  voice_metrics.guild_request(voice_metrics.SignalRequest)
  voice_metrics.server_updates(voice_metrics.CallScope, 2)
  voice_metrics.call_started()
  voice_metrics.call_create(voice_metrics.CallCreated, 6)
  voice_metrics.counts_synced(1)
  let after = parsed(scrape())
  list.each(
    [
      #(presence_counters.inputs_metric, "counter"),
      #(presence_counters.published_metric, "counter"),
      #(presence_counters.deliveries_metric, "counter"),
      #(presence_counters.start_metric, "histogram"),
      #(presence_counters.lookup_metric, "histogram"),
      #(voice_metrics.guild_requests_metric, "counter"),
      #(voice_metrics.server_updates_metric, "counter"),
      #(voice_metrics.calls_started_metric, "counter"),
      #(voice_metrics.call_create_ms, "histogram"),
      #(voice_metrics.counts_sync_ms, "histogram"),
    ],
    fn(family) {
      assert dict.get(after.families, family.0) == Ok(family.1)
    },
  )
  let delta = fn(name, labels) {
    value_of(after, name, labels) - value_of(before, name, labels)
  }
  assert delta(presence_counters.inputs_metric, [#("kind", "foreign")]) == 1
  assert delta(presence_counters.published_metric, []) == 1
  assert delta(presence_counters.deliveries_metric, [#("target", "node")]) == 3
  assert delta(presence_counters.start_metric <> "_count", [
      #("outcome", "called"),
    ])
    == 1
  assert delta(presence_counters.start_metric <> "_bucket", [
      #("outcome", "called"),
      #("le", "5"),
    ])
    == 1
  assert delta(voice_metrics.guild_requests_metric, [#("request", "signal")])
    == 1
  assert delta(voice_metrics.server_updates_metric, [#("scope", "call")]) == 2
  assert delta(voice_metrics.calls_started_metric, []) == 1
  assert delta(voice_metrics.call_create_ms <> "_sum", [
      #("outcome", "created"),
    ])
    == 6
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

pub fn outbound_families_reach_the_status_port_with_their_labels_test() {
  let before = parsed(scrape())
  api_metrics.request("session", "ok", 12, True)
  api_metrics.retry("session")
  rpc_metrics.answered("guild.get_counts", True, 4)
  rpc_metrics.handled("guild.get_counts", "overloaded")
  nats_metrics.published("reply", "ok")
  nats_metrics.sent(40)
  nats_metrics.received(2, 90)
  push_metrics.received("clear")
  push_metrics.job_waited(3)
  push_metrics.job_finished(8)
  push_metrics.attempted("push.job.clear", outbox_state.Delivered, 6)
  let after = parsed(scrape())
  list.each(
    [
      #(api_metrics.requests, "counter"),
      #(api_metrics.retries, "counter"),
      #(api_metrics.request_duration, "histogram"),
      #(api_metrics.in_flight, "gauge"),
      #(api_metrics.breaker_state, "gauge"),
      #(api_metrics.breaker_transitions, "counter"),
      #(rpc_metrics.requests, "counter"),
      #(rpc_metrics.duration, "histogram"),
      #(rpc_metrics.handlers, "gauge"),
      #(nats_metrics.connections, "gauge"),
      #(nats_metrics.connection_events, "counter"),
      #(nats_metrics.subscriptions, "gauge"),
      #(nats_metrics.publishes, "counter"),
      #(nats_metrics.messages, "counter"),
      #(nats_metrics.bytes, "counter"),
      #("fluxer_gateway_push_server_received_total", "counter"),
      #("fluxer_gateway_push_pending_jobs", "gauge"),
      #("fluxer_gateway_push_job_wait_ms", "histogram"),
      #("fluxer_gateway_push_job_duration_ms", "histogram"),
      #("fluxer_gateway_push_outbox_attempt_results_total", "counter"),
      #("fluxer_gateway_push_outbox_attempt_duration_ms", "histogram"),
    ],
    fn(family) {
      assert dict.get(after.families, family.0) == Ok(family.1)
    },
  )
  let grew = fn(name, labels) {
    value_of(after, name, labels) == value_of(before, name, labels) + 1
  }
  assert grew(api_metrics.requests, [
    #("request_type", "session"),
    #("outcome", "ok"),
  ])
  assert grew(api_metrics.retries, [#("request_type", "session")])
  assert grew(api_metrics.request_duration <> "_count", [
    #("request_type", "session"),
  ])
  assert grew(rpc_metrics.requests, [
    #("method", "guild.get_counts"),
    #("outcome", "overloaded"),
  ])
  assert grew(rpc_metrics.duration <> "_count", [#("family", "guild")])
  assert grew(nats_metrics.publishes, [#("kind", "reply"), #("outcome", "ok")])
  assert grew(nats_metrics.messages, [#("direction", "out")])
  assert value_of(after, nats_metrics.messages, [#("direction", "in")])
    == value_of(before, nats_metrics.messages, [#("direction", "in")]) + 2
  assert grew("fluxer_gateway_push_server_received_total", [#("kind", "clear")])
  assert grew("fluxer_gateway_push_job_wait_ms_count", [])
  assert grew("fluxer_gateway_push_outbox_attempt_results_total", [
    #("kind", "clear"),
    #("result", "delivered"),
  ])
  assert grew("fluxer_gateway_push_outbox_attempt_duration_ms_count", [
    #("kind", "clear"),
  ])
  assert list.all(after.series, fn(entry) {
    list.all(entry.labels, fn(label) {
      label.0 != "request_type"
      || label.1 == "other"
      || list.contains(api_metrics.request_types, label.1)
    })
  })
}

fn labels_of(found: Exposition, name: String, key: String) -> List(String) {
  found.series
  |> list.filter(fn(entry) { entry.name == name })
  |> list.filter_map(fn(entry) { list.key_find(entry.labels, key) })
  |> list.unique
  |> list.sort(string.compare)
}

pub fn the_status_port_serves_vm_health_families_test() {
  vm.track_scheduler_wall_time()
  let found = parsed(scrape())
  let types = ["dirty_cpu", "dirty_io", "normal"]
  list.each(
    [
      #("erlang_vm_memory_bytes", "gauge"),
      #("erlang_vm_process_limit", "gauge"),
      #("erlang_vm_port_limit", "gauge"),
      #("erlang_vm_atom_limit", "gauge"),
      #("erlang_vm_ets_table_count", "gauge"),
      #("erlang_vm_ets_table_limit", "gauge"),
      #("erlang_vm_dirty_scheduler_count", "gauge"),
      #("erlang_vm_run_queue_length", "gauge"),
      #("erlang_vm_scheduler_active_time_total", "counter"),
      #("erlang_vm_scheduler_wall_time_total", "counter"),
      #("erlang_vm_reductions_total", "counter"),
      #("erlang_vm_context_switches_total", "counter"),
      #("erlang_vm_gc_runs_total", "counter"),
      #("erlang_vm_gc_words_reclaimed_total", "counter"),
      #("erlang_vm_io_bytes_total", "counter"),
      #("erlang_vm_uptime_seconds", "gauge"),
      #("erlang_vm_persistent_term_count", "gauge"),
      #("erlang_vm_persistent_term_bytes", "gauge"),
    ],
    fn(family) {
      assert dict.get(found.families, family.0) == Ok(family.1)
    },
  )
  assert labels_of(found, "erlang_vm_memory_bytes", "type")
    == list.sort(vm.memory_types, string.compare)
  assert labels_of(found, "erlang_vm_run_queue_length", "type") == types
  assert labels_of(found, "erlang_vm_scheduler_active_time_total", "type")
    == types
  assert labels_of(found, "erlang_vm_scheduler_wall_time_total", "type")
    == types
  assert labels_of(found, "erlang_vm_dirty_scheduler_count", "type")
    == ["dirty_cpu", "dirty_io"]
  assert labels_of(found, "erlang_vm_io_bytes_total", "direction")
    == ["in", "out"]
  assert value_of(found, "erlang_vm_scheduler_wall_time_total", [
      #("type", "normal"),
    ])
    > 0
  assert value_of(found, "erlang_vm_process_limit", [])
    > value_of(found, "erlang_vm_process_count", [])
  assert value_of(found, "erlang_vm_reductions_total", []) > 0
  assert value_of(found, "erlang_vm_ets_table_count", []) > 0
  assert value_of(found, "erlang_vm_persistent_term_count", []) > 0
  assert value_of(found, "erlang_vm_memory_bytes", [#("type", "system")]) > 0
  let later = parsed(scrape())
  assert value_of(later, "erlang_vm_reductions_total", [])
    >= value_of(found, "erlang_vm_reductions_total", [])
}

pub fn the_status_port_serves_cluster_and_resident_families_test() {
  let queued = 7
  let ready = process.new_subject()
  let holder =
    process.spawn_unlinked(fn() {
      let _ = register(atom.create("fluxer_test_resident_3"), process.self())
      process.send(ready, Nil)
      process.sleep_forever()
    })
  let assert Ok(Nil) = process.receive(ready, 1000)
  list.each(list.repeat(Nil, queued), fn(_) {
    raw_send(holder, Nil)
    Nil
  })
  let found = parsed(scrape())
  list.each(
    [
      #("fluxer_gateway_cluster_connected_nodes", "gauge"),
      #("fluxer_gateway_cluster_dist_bytes_total", "counter"),
      #("fluxer_gateway_cluster_dist_queue_bytes", "gauge"),
      #("fluxer_gateway_ets_table_rows", "gauge"),
      #("fluxer_gateway_ets_table_bytes", "gauge"),
      #("fluxer_gateway_singleton_mailbox_messages", "gauge"),
      #("fluxer_gateway_singleton_mailbox_max_messages", "gauge"),
      #("fluxer_gateway_singleton_memory_bytes", "gauge"),
      #("fluxer_gateway_bouncer_beat_acks_total", "counter"),
      #("fluxer_gateway_watchdog_actions_total", "counter"),
      #("fluxer_gateway_watchdog_mailbox_max_messages", "gauge"),
    ],
    fn(family) {
      assert dict.get(found.families, family.0) == Ok(family.1)
    },
  )
  assert labels_of(
      found,
      "fluxer_gateway_cluster_connected_nodes",
      "visibility",
    )
    == ["hidden", "visible"]
  assert labels_of(
      found,
      "fluxer_gateway_cluster_dist_bytes_total",
      "direction",
    )
    == ["received", "sent"]
  assert labels_of(found, "fluxer_gateway_bouncer_beat_acks_total", "outcome")
    == ["matched", "mismatched"]
  assert labels_of(found, "fluxer_gateway_watchdog_actions_total", "action")
    == list.sort(watchdog.action_names, string.compare)
  assert labels_of(
      found,
      "fluxer_gateway_watchdog_mailbox_max_messages",
      "kind",
    )
    == watchdog.holder_kinds
  let tables = labels_of(found, "fluxer_gateway_ets_table_rows", "table")
  assert list.contains(tables, "fluxer_gateway_dist_readings")
  assert tables == labels_of(found, "fluxer_gateway_ets_table_bytes", "table")
  assert list.all(tables, fn(name) { resident.family(name) == name })
  assert value_of(found, "fluxer_gateway_ets_table_rows", [
      #("table", "fluxer_gateway_dist_readings"),
    ])
    > 0
  assert value_of(found, "fluxer_gateway_singleton_mailbox_max_messages", [
      #("process", "fluxer_test_resident"),
    ])
    == queued
  assert value_of(found, "fluxer_gateway_singleton_mailbox_messages", [
      #("process", "fluxer_test_resident"),
    ])
    == queued
  assert value_of(found, "fluxer_gateway_singleton_memory_bytes", [
      #("process", "fluxer_test_resident"),
    ])
    > 0
  assert !list.contains(
    labels_of(found, "fluxer_gateway_singleton_mailbox_max_messages", "process"),
    "fluxer_test_resident_3",
  )
  process.kill(holder)
}
