// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/presence.{type Status, Dnd, Idle, Offline, Online}
import fluxer_gateway/core/session
import fluxer_gateway/presence/sessions
import gleam/bit_array
import gleam/bytes_tree
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/string

fn session_id(n: Int) -> session.SessionId {
  let text = string.pad_start(int.to_string(n), 32, "0")
  let assert Ok(found) = session.session_id_from_string(text)
  found
}

fn table(rows: List(#(Int, Status, Bool, Bool))) -> sessions.Sessions(Nil) {
  let assert Ok(user) = id.user_id(1_000_000_000_000_000)
  list.fold(rows, sessions.empty(), fn(acc, row) {
    let #(n, status, afk, mobile) = row
    sessions.upsert(
      acc,
      sessions.entry(
        session: session.SessionRef(
          id: session_id(n),
          user:,
          inbox: process.new_subject(),
        ),
        status:,
        afk:,
        mobile:,
        monitor: Nil,
      ),
    ).0
  })
}

fn row(id: String, afk: Bool, mobile: Bool, status: String) -> String {
  "{\"afk\":"
  <> bool(afk)
  <> ",\"mobile\":"
  <> bool(mobile)
  <> ",\"session_id\":\""
  <> id
  <> "\",\"status\":\""
  <> status
  <> "\"}"
}

fn bool(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn padded(n: Int) -> String {
  string.pad_start(int.to_string(n), 32, "0")
}

fn wire(found: sessions.Sessions(Nil), invisible: Bool) -> String {
  let assert Ok(text) =
    bit_array.to_string(
      bytes_tree.to_bit_array(sessions.replace_wire(found, invisible:)),
    )
  text
}

pub fn replace_wire_sorts_descending_and_flattens_test() {
  let found =
    table([
      #(2, Online, False, True),
      #(3, Idle, True, False),
      #(1, Dnd, False, False),
    ])
  assert wire(found, False)
    == "["
    <> row("all", False, False, "dnd")
    <> ","
    <> row(padded(3), True, False, "idle")
    <> ","
    <> row(padded(2), False, True, "online")
    <> ","
    <> row(padded(1), False, False, "dnd")
    <> "]"
}

pub fn replace_wire_reports_mobile_online_test() {
  let found = table([#(5, Online, False, True), #(4, Idle, True, False)])
  assert wire(found, False)
    == "["
    <> row("all", False, True, "online")
    <> ","
    <> row(padded(5), False, True, "online")
    <> ","
    <> row(padded(4), True, False, "idle")
    <> "]"
}

pub fn replace_wire_marks_invisible_sessions_test() {
  let found = table([#(1, Online, True, False), #(2, Offline, True, False)])
  assert wire(found, True)
    == "["
    <> row("all", True, False, "invisible")
    <> ","
    <> row(padded(2), True, False, "offline")
    <> ","
    <> row(padded(1), True, False, "invisible")
    <> "]"
}

pub fn replace_wire_of_no_sessions_test() {
  assert wire(sessions.empty(), False)
    == "[" <> row("all", False, False, "offline") <> "]"
}

pub fn replace_wire_sorts_many_sessions_test() {
  let numbers = [7, 12, 3, 9, 1, 11, 5, 2, 10, 4, 8, 6]
  let found = table(list.map(numbers, fn(n) { #(n, Idle, False, False) }))
  let rows =
    list.sort(numbers, int.compare)
    |> list.reverse
    |> list.map(fn(n) { "," <> row(padded(n), False, False, "idle") })
  assert wire(found, False)
    == "[" <> row("all", False, False, "idle") <> string.concat(rows) <> "]"
}
