// SPDX-License-Identifier: AGPL-3.0-or-later

import envoy
import fluxer_bouncer/core.{type Status}
import gleam/bit_array
import gleam/bytes_tree
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Selector, type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{Some}
import gleam/otp/actor
import gleam/otp/static_supervisor.{type Supervisor}
import gleam/result
import gleam/string
import glisten

const listen_interface = "0.0.0.0"

const max_request_bytes = 8192

const request_timeout_ms = 2000

const metrics_content_type = "text/plain; version=0.0.4; charset=utf-8"

pub type Reader =
  fn() -> Result(Status, Nil)

pub type Snapshot {
  Snapshot(version: String, revision: String, status: Result(Status, Nil))
}

pub type Reply {
  Reply(code: Int, content_type: String, body: String)
}

type TableOption {
  NamedTable
  Public
  Set
  ReadConcurrency(Bool)
}

@external(erlang, "ets", "new")
fn ets_new(name: Atom, options: List(TableOption)) -> Atom

@external(erlang, "ets", "whereis")
fn ets_whereis(name: Atom) -> Dynamic

@external(erlang, "ets", "insert")
fn ets_insert(table: Atom, row: #(Atom, Status)) -> Bool

@external(erlang, "ets", "lookup")
fn ets_lookup(table: Atom, key: Atom) -> List(#(Atom, Status))

fn table() -> Atom {
  atom.create("fluxer_bouncer_status")
}

fn row_key() -> Atom {
  atom.create("status")
}

fn exists() -> Bool {
  case decode.run(ets_whereis(table()), atom.decoder()) {
    Ok(found) -> atom.to_string(found) != "undefined"
    Error(_) -> True
  }
}

pub fn init() -> Nil {
  case exists() {
    True -> Nil
    False -> {
      let _ = ets_new(table(), [NamedTable, Public, Set, ReadConcurrency(True)])
      Nil
    }
  }
}

pub fn publish(status: Status) -> Nil {
  case exists() {
    True -> {
      let _ = ets_insert(table(), #(row_key(), status))
      Nil
    }
    False -> Nil
  }
}

pub fn current() -> Result(Status, Nil) {
  case exists() {
    False -> Error(Nil)
    True ->
      case ets_lookup(table(), row_key()) {
        [#(_, status), ..] -> Ok(status)
        [] -> Error(Nil)
      }
  }
}

pub fn read(reader: Reader) -> Snapshot {
  Snapshot(
    version: envoy.get("BUILD_VERSION") |> result.unwrap("dev"),
    revision: envoy.get("BUILD_REVISION") |> result.unwrap("dev"),
    status: reader(),
  )
}

pub fn ready_reason(found: Result(Status, Nil)) -> String {
  case found {
    Error(Nil) -> "STARTING"
    Ok(status) if status.synced -> "OK"
    Ok(status) if status.leaving -> "LEAVING"
    Ok(status) if status.recovering -> "RECOVERING"
    Ok(status) ->
      case bouncers_seen(status) < status.quorum {
        True -> "NO_QUORUM"
        False -> "SYNCING"
      }
  }
}

fn bouncers_seen(status: Status) -> Int {
  case status.leaving {
    True -> status.peers_seen
    False -> status.peers_seen + 1
  }
}

fn flag(value: Bool) -> Int {
  case value {
    True -> 1
    False -> 0
  }
}

pub fn numbers(snapshot: Snapshot) -> List(#(String, Int)) {
  let answered = result.is_ok(snapshot.status)
  let status =
    result.unwrap(
      snapshot.status,
      core.Status(
        ordinal: -1,
        count: 0,
        quorum: 0,
        synced: False,
        recovering: False,
        leaving: False,
        buckets: 0,
        buckets_synced: 0,
        peers_seen: 0,
        peers_synced: 0,
        workers: 0,
        moves_in_flight: 0,
        refusals: core.no_refusals(),
      ),
    )
  [
    #("answered", flag(answered)),
    #("ready", flag(answered && status.synced)),
    #("synced", flag(answered && status.synced)),
    #("ordinal", status.ordinal),
    #("bouncers_configured", status.count),
    #("bouncers_seen", case answered {
      True -> bouncers_seen(status)
      False -> 0
    }),
    #("quorum", status.quorum),
    #(
      "bouncer_quorum",
      flag(answered && bouncers_seen(status) >= status.quorum),
    ),
    #("recovering", flag(answered && status.recovering)),
    #("leaving", flag(status.leaving)),
    #("peers_seen", status.peers_seen),
    #("peers_synced", status.peers_synced),
    #("workers", status.workers),
    #("buckets", status.buckets),
    #("buckets_synced", status.buckets_synced),
    #("record_sync_lag", int.max(0, status.buckets - status.buckets_synced)),
    #("moves_in_flight", status.moves_in_flight),
  ]
}

pub const allowed_labels = ["role", "reason"]

pub fn refusals(snapshot: Snapshot) -> List(#(String, Int)) {
  let found = case snapshot.status {
    Ok(status) -> status.refusals
    Error(Nil) -> core.no_refusals()
  }
  [
    #("superseded", found.superseded),
    #("holder_heard", found.holder_heard),
    #("roster_alive", found.roster_alive),
    #("still_recovering", found.still_recovering),
  ]
}

fn writes_lost(snapshot: Snapshot) -> Int {
  case snapshot.status {
    Ok(status) -> status.refusals.lost
    Error(Nil) -> 0
  }
}

pub fn to_json(snapshot: Snapshot) -> String {
  let fields =
    list.map(numbers(snapshot), fn(field) { #(field.0, json.int(field.1)) })
  json.object(
    list.flatten([
      [
        #("role", json.string("bouncer")),
        #("version", json.string(snapshot.version)),
        #("revision", json.string(snapshot.revision)),
        #("ready_reason", json.string(ready_reason(snapshot.status))),
      ],
      fields,
      [#("analysis", json.object(fields))],
    ]),
  )
  |> json.to_string
}

const role_label = "role=\"bouncer\""

fn family(name: String, kind: String, help: String) -> String {
  "# HELP " <> name <> " " <> help <> "\n# TYPE " <> name <> " " <> kind <> "\n"
}

fn sample(name: String, labels: String, value: Int) -> String {
  name <> "{" <> labels <> "} " <> int.to_string(value) <> "\n"
}

pub fn metrics_text(snapshot: Snapshot) -> String {
  let gauges =
    numbers(snapshot)
    |> list.map(fn(field) {
      let name = "fluxer_bouncer_status_" <> field.0
      family(name, "gauge", "Bouncer status field " <> field.0)
      <> sample(name, role_label, field.1)
    })
  let refused = "fluxer_bouncer_write_refusals_total"
  let lost = "fluxer_bouncer_writes_lost_total"
  string.concat(
    list.flatten([
      gauges,
      [
        family(
          refused,
          "counter",
          "Lease record writes a peer refused, by reason, superseded is a compare and set conflict",
        ),
      ],
      list.map(refusals(snapshot), fn(entry) {
        sample(refused, role_label <> ",reason=\"" <> entry.0 <> "\"", entry.1)
      }),
      [
        family(
          lost,
          "counter",
          "Lease record writes this bouncer gave up because too many peers refused",
        ),
        sample(lost, role_label, writes_lost(snapshot)),
      ],
    ]),
  )
}

fn text(code: Int, body: String) -> Reply {
  Reply(code: code, content_type: "text/plain", body: body)
}

pub fn readiness(found: Result(Status, Nil)) -> Reply {
  case ready_reason(found) {
    "OK" -> text(200, "OK")
    reason -> text(503, reason)
  }
}

pub fn answer(method: String, path: String, reader: Reader) -> Reply {
  let reading = method == "GET" || method == "HEAD"
  case path, reading {
    "/_health", True -> text(200, "OK")
    "/_health/ready", True -> readiness(reader())
    "/_status", True -> Reply(200, "application/json", to_json(read(reader)))
    "/_metrics", True ->
      Reply(200, metrics_content_type, metrics_text(read(reader)))
    "/_health", False
    | "/_health/ready", False
    | "/_status", False
    | "/_metrics", False
    -> text(405, "METHOD_NOT_ALLOWED")
    _, _ -> text(404, "NOT_FOUND")
  }
}

pub type Parsed {
  Incomplete
  Request(method: String, path: String)
  Malformed
}

pub fn parse(buffer: BitArray) -> Parsed {
  case bit_array.byte_size(buffer) > max_request_bytes {
    True -> Malformed
    False ->
      case bit_array.to_string(buffer) {
        Error(Nil) -> Incomplete
        Ok(text) ->
          case string.split_once(text, "\r\n\r\n") {
            Error(Nil) -> Incomplete
            Ok(#(head, _)) -> request_line(first_line(head))
          }
      }
  }
}

fn first_line(head: String) -> String {
  case string.split_once(head, "\r\n") {
    Ok(#(line, _)) -> line
    Error(Nil) -> head
  }
}

fn request_line(line: String) -> Parsed {
  case string.split(line, " ") {
    [method, target, "HTTP/1.1"] | [method, target, "HTTP/1.0"] ->
      Request(method:, path: path_of(target))
    _ -> Malformed
  }
}

fn path_of(target: String) -> String {
  case string.split_once(target, "?") {
    Ok(#(path, _)) -> path
    Error(Nil) -> target
  }
}

pub fn encode(reply: Reply, method: String) -> bytes_tree.BytesTree {
  let body = case method {
    "HEAD" -> ""
    _ -> reply.body
  }
  bytes_tree.from_string(
    "HTTP/1.1 "
    <> int.to_string(reply.code)
    <> " "
    <> reason(reply.code)
    <> "\r\ncontent-type: "
    <> reply.content_type
    <> "\r\ncontent-length: "
    <> int.to_string(string.byte_size(reply.body))
    <> "\r\ncache-control: no-store\r\nconnection: close\r\n\r\n"
    <> body,
  )
}

fn reason(code: Int) -> String {
  case code {
    200 -> "OK"
    400 -> "Bad Request"
    404 -> "Not Found"
    405 -> "Method Not Allowed"
    503 -> "Service Unavailable"
    _ -> "Error"
  }
}

type Connection {
  Connection(buffer: BitArray)
}

type Timeout {
  Timeout
}

fn on_init(
  _connection: glisten.Connection(Timeout),
) -> #(Connection, option.Option(Selector(Timeout))) {
  let timer: Subject(Timeout) = process.new_subject()
  process.send_after(timer, request_timeout_ms, Timeout)
  #(Connection(<<>>), Some(process.new_selector() |> process.select(timer)))
}

fn loop(
  reader: Reader,
  state: Connection,
  message: glisten.Message(Timeout),
  connection: glisten.Connection(Timeout),
) -> glisten.Next(Connection, glisten.Message(Timeout)) {
  case message {
    glisten.User(Timeout) -> glisten.stop()
    glisten.Packet(data) -> {
      let buffer = <<state.buffer:bits, data:bits>>
      case parse(buffer) {
        Incomplete -> glisten.continue(Connection(buffer))
        Malformed -> {
          let _ =
            glisten.send(
              connection,
              encode(Reply(400, "text/plain", "BAD_REQUEST"), "GET"),
            )
          glisten.stop()
        }
        Request(method:, path:) -> {
          let _ =
            glisten.send(
              connection,
              encode(answer(method, path, reader), method),
            )
          glisten.stop()
        }
      }
    }
  }
}

pub fn start(
  port: Int,
  name,
  reader: Reader,
) -> Result(actor.Started(Supervisor), actor.StartError) {
  glisten.new(on_init, fn(state, message, connection) {
    loop(reader, state, message, connection)
  })
  |> glisten.bind(listen_interface)
  |> glisten.with_listener_name(name)
  |> glisten.start(port)
}

pub fn port_of(name) -> Int {
  glisten.get_server_info(name, request_timeout_ms).port
}
