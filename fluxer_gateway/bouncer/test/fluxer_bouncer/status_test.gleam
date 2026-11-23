// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_bouncer/config
import fluxer_bouncer/core.{Status}
import fluxer_bouncer/status.{Incomplete, Malformed, Reply, Request}
import gleam/bit_array
import gleam/bytes_tree
import gleam/dict
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/string

fn healthy() -> core.Status {
  Status(
    ordinal: 1,
    count: 3,
    quorum: 2,
    synced: True,
    recovering: False,
    leaving: False,
    buckets: 1024,
    buckets_synced: 1024,
    peers_seen: 2,
    peers_synced: 2,
    workers: 7,
    moves_in_flight: 2,
    refusals: core.no_refusals(),
  )
}

fn status_of(found: Result(core.Status, Nil)) -> status.Snapshot {
  status.Snapshot(version: "v1", revision: "abc", status: found)
}

const number_keys = [
  "answered", "ready", "synced", "ordinal", "bouncers_configured",
  "bouncers_seen", "quorum", "bouncer_quorum", "recovering", "leaving",
  "peers_seen", "peers_synced", "workers", "buckets", "buckets_synced",
  "record_sync_lag", "moves_in_flight",
]

pub fn ready_reason_names_why_a_bouncer_is_not_ready_test() {
  let unsynced = Status(..healthy(), synced: False)
  assert status.ready_reason(Ok(healthy())) == "OK"
  assert status.ready_reason(Error(Nil)) == "STARTING"
  assert status.ready_reason(Ok(Status(..unsynced, leaving: True))) == "LEAVING"
  assert status.ready_reason(Ok(Status(..unsynced, recovering: True)))
    == "RECOVERING"
  assert status.ready_reason(Ok(Status(..unsynced, peers_seen: 0)))
    == "NO_QUORUM"
  assert status.ready_reason(Ok(unsynced)) == "SYNCING"
}

pub fn numbers_are_stable_and_numeric_test() {
  let found = dict.from_list(status.numbers(status_of(Ok(healthy()))))
  assert list.sort(dict.keys(found), string.compare)
    == list.sort(number_keys, string.compare)
  assert dict.get(found, "answered") == Ok(1)
  assert dict.get(found, "ready") == Ok(1)
  assert dict.get(found, "bouncers_seen") == Ok(3)
  assert dict.get(found, "bouncer_quorum") == Ok(1)
  assert dict.get(found, "workers") == Ok(7)
  assert dict.get(found, "buckets") == Ok(1024)
  assert dict.get(found, "moves_in_flight") == Ok(2)
  let silent = dict.from_list(status.numbers(status_of(Error(Nil))))
  assert dict.get(silent, "answered") == Ok(0)
  assert dict.get(silent, "ready") == Ok(0)
  assert dict.get(silent, "bouncer_quorum") == Ok(0)
  let lonely =
    dict.from_list(
      status.numbers(
        status_of(Ok(Status(..healthy(), synced: False, peers_seen: 0))),
      ),
    )
  assert dict.get(lonely, "bouncer_quorum") == Ok(0)
  assert dict.get(lonely, "ready") == Ok(0)
}

pub fn json_carries_identity_reason_and_analysis_test() {
  let text =
    status.to_json(
      status_of(Ok(Status(..healthy(), synced: False, recovering: True))),
    )
  let shape = {
    use role <- decode.field("role", decode.string)
    use version <- decode.field("version", decode.string)
    use revision <- decode.field("revision", decode.string)
    use reason <- decode.field("ready_reason", decode.string)
    use seen <- decode.field("bouncers_seen", decode.int)
    use analysis <- decode.field(
      "analysis",
      decode.dict(decode.string, decode.int),
    )
    decode.success(#(role, version, revision, reason, seen, analysis))
  }
  let assert Ok(#(role, version, revision, reason, seen, analysis)) =
    json.parse(text, shape)
  assert role == "bouncer"
  assert version == "v1"
  assert revision == "abc"
  assert reason == "RECOVERING"
  assert seen == 3
  assert dict.get(analysis, "recovering") == Ok(1)
  assert list.length(dict.keys(analysis)) == list.length(number_keys)
}

pub fn metrics_are_gauges_with_only_the_role_label_test() {
  let text = status.metrics_text(status_of(Ok(healthy())))
  list.each(number_keys, fn(key) {
    assert string.contains(
      text,
      "# TYPE fluxer_bouncer_status_"
        <> key
        <> " gauge\nfluxer_bouncer_status_"
        <> key
        <> "{role=\"bouncer\"} ",
    )
  })
  assert !string.contains(text, "@")
  assert sample_labels(text)
    |> list.all(fn(labels) {
      list.all(labels, fn(label) { list.contains(status.allowed_labels, label) })
    })
}

fn sample_labels(text: String) -> List(List(String)) {
  string.split(text, "\n")
  |> list.filter(fn(line) { line != "" && !string.starts_with(line, "#") })
  |> list.map(fn(line) {
    let assert Ok(#(series, value)) = string.split_once(line, " ")
    let assert Ok(_) = int.parse(value)
    case string.split_once(series, "{") {
      Error(Nil) -> []
      Ok(#(_, rest)) ->
        string.drop_end(rest, 1)
        |> string.split(",")
        |> list.map(fn(pair) {
          let assert Ok(#(key, _)) = string.split_once(pair, "=")
          key
        })
    }
  })
}

pub fn record_sync_lag_and_refusals_are_exported_test() {
  let behind =
    Status(
      ..healthy(),
      buckets_synced: 1000,
      refusals: core.Refusals(..core.no_refusals(), superseded: 2, lost: 1),
    )
  let text = status.metrics_text(status_of(Ok(behind)))
  assert string.contains(
    text,
    "fluxer_bouncer_status_record_sync_lag{role=\"bouncer\"} 24\n",
  )
  assert string.contains(
    text,
    "fluxer_bouncer_write_refusals_total{role=\"bouncer\",reason=\"superseded\"} 2\n",
  )
  assert string.contains(
    text,
    "fluxer_bouncer_write_refusals_total{role=\"bouncer\",reason=\"holder_heard\"} 0\n",
  )
  assert string.contains(
    text,
    "fluxer_bouncer_writes_lost_total{role=\"bouncer\"} 1\n",
  )
  let silent = status.metrics_text(status_of(Error(Nil)))
  assert string.contains(
    silent,
    "fluxer_bouncer_writes_lost_total{role=\"bouncer\"} 0\n",
  )
}

pub fn only_reads_are_served_test() {
  let reader = fn() { Ok(healthy()) }
  let assert Reply(200, "application/json", body) =
    status.answer("GET", "/_status", reader)
  assert string.contains(body, "\"role\":\"bouncer\"")
  let assert Reply(200, _, metrics) = status.answer("GET", "/_metrics", reader)
  assert string.contains(
    metrics,
    "fluxer_bouncer_status_ready{role=\"bouncer\"} 1",
  )
  assert status.answer("HEAD", "/_status", reader).code == 200
  assert status.answer("POST", "/_status", reader).code == 405
  assert status.answer("PUT", "/_metrics", reader).code == 405
  assert status.answer("GET", "/_health", reader).code == 200
  let assert Reply(200, _, "OK") =
    status.answer("GET", "/_health/ready", reader)
  let assert Reply(503, _, "SYNCING") =
    status.answer("GET", "/_health/ready", fn() {
      Ok(Status(..healthy(), synced: False))
    })
  let assert Reply(503, _, "STARTING") =
    status.answer("GET", "/_health/ready", fn() { Error(Nil) })
  assert status.answer("POST", "/_health/ready", reader).code == 405
  assert status.answer("GET", "/leave", reader).code == 404
  assert status.answer("POST", "/", reader).code == 404
}

pub fn requests_are_parsed_once_the_head_is_complete_test() {
  assert status.parse(<<"GET /_status HTTP/1.1\r\nHost: x":utf8>>) == Incomplete
  assert status.parse(<<"GET /_status?x=1 HTTP/1.1\r\nHost: x\r\n\r\n":utf8>>)
    == Request("GET", "/_status")
  assert status.parse(<<"HEAD /_metrics HTTP/1.0\r\n\r\n":utf8>>)
    == Request("HEAD", "/_metrics")
  assert status.parse(<<"nonsense\r\n\r\n":utf8>>) == Malformed
  assert status.parse(<<0:size(65_544)>>) == Malformed
}

pub fn replies_close_and_count_bytes_test() {
  let reply = Reply(200, "application/json", "{\"a\":1}")
  let assert Ok(text) =
    bit_array.to_string(bytes_tree.to_bit_array(status.encode(reply, "GET")))
  assert string.starts_with(text, "HTTP/1.1 200 OK\r\n")
  assert string.contains(text, "content-length: 7\r\n")
  assert string.contains(text, "connection: close\r\n")
  assert string.ends_with(text, "\r\n\r\n{\"a\":1}")
  let assert Ok(head) =
    bit_array.to_string(bytes_tree.to_bit_array(status.encode(reply, "HEAD")))
  assert string.ends_with(head, "\r\n\r\n")
  assert string.contains(head, "content-length: 7\r\n")
}

pub fn the_status_port_comes_from_its_variable_test() {
  let assert Ok(built) =
    config.build(
      hostname: "gateway-bouncer-0",
      count: "3",
      basename: "gateway-bouncer",
      dns_name: "d",
    )
  assert built.status_port == 8082
  assert config.with_status_port(built, "0")
    == Ok(config.Config(..built, status_port: 0))
  assert config.with_status_port(built, " 9200 ")
    == Ok(config.Config(..built, status_port: 9200))
  assert config.with_status_port(built, "port")
    == Error(config.Invalid("FLUXER_BOUNCER_STATUS_PORT", "port"))
  assert config.with_status_port(built, "70000")
    == Error(config.Invalid("FLUXER_BOUNCER_STATUS_PORT", "70000"))
}
