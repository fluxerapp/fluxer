// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/wire_epoch
import fluxer_gateway/cluster/wire.{type Bridge, Bridge}
import fluxer_gateway/config/build_info
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/result
import gleam/string

const history_path = "test/fixtures/wire_epoch.txt"

const types_path = "test/fixtures/wire_epoch_types.txt"

const cross_node = [
  #("src/fluxer_gateway/core/guild.gleam", "GuildRequest"),
  #("src/fluxer_gateway/core/session.gleam", "SessionInbox"),
  #("src/fluxer_gateway/core/session.gleam", "SocketMsg"),
  #("src/fluxer_gateway/session/control.gleam", "Control"),
  #("src/fluxer_gateway/guild/inbox.gleam", "Control"),
  #("src/fluxer_gateway/guild/manager/shard.gleam", "Message"),
  #("src/fluxer_gateway/guild/handoff/signal.gleam", "Signal"),
  #("src/fluxer_gateway/presence/protocol.gleam", "Input"),
  #("src/fluxer_gateway/presence/protocol.gleam", "ManagerMsg"),
  #("src/fluxer_gateway/presence/handoff/signal.gleam", "Signal"),
  #("src/fluxer_gateway/cluster/relay.gleam", "RemoteMessage"),
  #("src/fluxer_gateway/voice/call/manager.gleam", "Message"),
  #("src/fluxer_gateway/voice/call/server.gleam", "Message"),
  #("src/fluxer_gateway/voice/call/server.gleam", "MoveSignal"),
  #("src/fluxer_gateway/voice/call/state.gleam", "Member"),
  #("src/fluxer_gateway/voice/call/state.gleam", "MemberNote"),
  #("src/fluxer_gateway/voice/dm/server.gleam", "Message"),
  #("src/fluxer_gateway/push/server.gleam", "Message"),
  #("src/fluxer_gateway/push/outbox.gleam", "Message"),
  #("src/fluxer_gateway/push/outbox/state.gleam", "Carried"),
  #("src/fluxer_gateway/handoff/precopy.gleam", "Envelope"),
  #("src/fluxer_gateway/handoff/precopy.gleam", "Ack"),
]

@external(erlang, "file", "read_file")
fn read_file(path: String) -> Result(BitArray, Dynamic)

@external(erlang, "erlang", "phash2")
fn phash2(term: a, range: Int) -> Int

fn read(path: String) -> String {
  let assert Ok(bytes) = read_file(path)
  let assert Ok(text) = bit_array.to_string(bytes)
  text
}

fn is_header(line: String, name: String) -> Bool {
  list.any(["pub type ", "pub opaque type ", "type "], fn(prefix) {
    string.starts_with(line, prefix <> name <> " {")
    || string.starts_with(line, prefix <> name <> "(")
  })
}

fn block(path: String, name: String) -> List(String) {
  read(path)
  |> string.split("\n")
  |> list.drop_while(fn(line) { !is_header(line, name) })
  |> list.take_while(fn(line) { line != "}" })
  |> list.map(fn(line) {
    string.split(line, " ") |> list.filter(fn(word) { word != "" })
  })
  |> list.map(string.join(_, " "))
}

pub fn type_fingerprint(entry: #(String, String)) -> Int {
  phash2(#(entry.1, block(entry.0, entry.1)), 4_294_967_295)
}

pub fn recorded_types(
  text: String,
  epoch: Int,
) -> List(#(#(String, String), Int)) {
  text
  |> string.split("\n")
  |> list.filter_map(fn(line) {
    case string.split(string.trim(line), " ") {
      [at, path, name, value] -> {
        use at <- result.try(int.parse(at))
        use value <- result.try(int.parse(value))
        case at == epoch {
          True -> Ok(#(#(path, name), value))
          False -> Error(Nil)
        }
      }
      _ -> Error(Nil)
    }
  })
}

pub fn changed_since(
  recorded: List(#(#(String, String), Int)),
  current: fn(#(String, String)) -> Int,
  known: List(#(String, String)),
) -> List(#(String, String)) {
  list.filter(known, fn(entry) {
    list.key_find(recorded, entry) != Ok(current(entry))
  })
}

pub fn fingerprint() -> Int {
  cross_node
  |> list.map(fn(entry) { #(entry.1, block(entry.0, entry.1)) })
  |> phash2(4_294_967_295)
}

pub type Compat {
  Unstated
  Bridged
  Breaking(reason: String)
  Unknown(words: String)
}

pub type Row {
  Row(epoch: Int, value: Int, compat: Compat)
}

fn compat(words: List(String)) -> Compat {
  case words {
    [] -> Unstated
    ["bridged"] -> Bridged
    ["breaking", first, ..rest] -> Breaking(string.join([first, ..rest], " "))
    _ -> Unknown(string.join(words, " "))
  }
}

pub fn parse(text: String) -> List(Row) {
  text
  |> string.split("\n")
  |> list.filter_map(fn(line) {
    case string.split(string.trim(line), " ") {
      [epoch, value, ..words] -> {
        use epoch <- result.try(int.parse(epoch))
        use value <- result.map(int.parse(value))
        Row(epoch, value, compat(words))
      }
      _ -> Error(Nil)
    }
  })
}

fn entry_rule(previous: Row, row: Row) -> Result(Nil, String) {
  let epoch = int.to_string(row.epoch)
  case row.epoch < wire_epoch.first_bridged, row.compat {
    True, Bridged ->
      Error("wire_epoch " <> epoch <> " is older than the first bridged epoch")
    True, _ -> Ok(Nil)
    False, Bridged if row.epoch == previous.epoch + 1 -> Ok(Nil)
    False, Bridged ->
      Error(
        "wire_epoch "
        <> epoch
        <> " is bridged but does not follow the previous epoch by one",
      )
    False, Breaking(_) if row.epoch >= previous.epoch + 2 -> Ok(Nil)
    False, Breaking(_) ->
      Error(
        "wire_epoch "
        <> epoch
        <> " is breaking, so it must skip an epoch to stay refused by every older peer",
      )
    False, Unstated ->
      Error(
        "wire_epoch "
        <> epoch
        <> " has no compatibility entry, end its line with bridged or breaking <reason>",
      )
    False, Unknown(words) ->
      Error(
        "wire_epoch "
        <> epoch
        <> " has an unknown compatibility entry "
        <> words,
      )
  }
}

fn entries(rows: List(Row)) -> Result(Nil, String) {
  case rows {
    [previous, row, ..rest] ->
      case row.epoch > previous.epoch {
        False ->
          Error("wire_epoch history must list each epoch once, ascending")
        True -> {
          use Nil <- result.try(entry_rule(previous, row))
          entries([row, ..rest])
        }
      }
    _ -> Ok(Nil)
  }
}

fn bridge_rule(
  last: Row,
  bridges: List(Bridge),
  known: List(#(String, String)),
) -> Result(Nil, String) {
  case last.compat, bridges {
    Bridged, [Bridge(from:, types:)] if from == last.epoch - 1 && types != [] ->
      case list.all(types, list.contains(known, _)) {
        True -> Ok(Nil)
        False -> Error("the bridge names a type that is not fingerprinted")
      }
    Bridged, _ ->
      Error(
        "wire_epoch "
        <> int.to_string(last.epoch)
        <> " is bridged, so cluster/wire.bridges must hold one bridge from "
        <> int.to_string(last.epoch - 1)
        <> " naming the changed types",
      )
    _, [] -> Ok(Nil)
    _, _ ->
      Error(
        "cluster/wire.bridges holds a bridge the current epoch does not use",
      )
  }
}

pub fn check(
  rows: List(Row),
  epoch: Int,
  value: Int,
  bridges: List(Bridge),
  known: List(#(String, String)),
) -> Result(Nil, String) {
  use Nil <- result.try(entries(rows))
  case list.last(rows) {
    Ok(last) if last.epoch == epoch && last.value == value ->
      bridge_rule(last, bridges, known)
    Ok(last) if last.epoch == epoch ->
      Error(
        "the cross-node types changed, so bump build_info.wire_epoch and add the line "
        <> int.to_string(epoch + 1)
        <> " "
        <> int.to_string(value)
        <> " bridged (or skip one epoch with breaking <reason>)",
      )
    _ ->
      Error(
        "wire_epoch "
        <> int.to_string(epoch)
        <> " "
        <> int.to_string(value)
        <> " is not the last line of the history",
      )
  }
}

pub fn the_bridge_names_exactly_the_types_changed_since_the_previous_epoch_test() {
  let previous = recorded_types(read(types_path), build_info.wire_epoch - 1)
  assert previous != []
  let changed = changed_since(previous, type_fingerprint, cross_node)
  let bridged = list.flat_map(wire.bridges, fn(bridge) { bridge.types })
  assert list.sort(changed, compare_entry) == list.sort(bridged, compare_entry)
}

pub fn a_changed_type_missing_from_the_bridge_is_caught_test() {
  let recorded = [#(shard, 1), #(#("a.gleam", "B"), 2)]
  let current = fn(entry) {
    case entry == shard {
      True -> 1
      False -> 3
    }
  }
  assert changed_since(recorded, current, [shard, #("a.gleam", "B")])
    == [#("a.gleam", "B")]
  assert changed_since([], current, [shard]) == [shard]
}

fn compare_entry(a: #(String, String), b: #(String, String)) {
  string.compare(a.0 <> " " <> a.1, b.0 <> " " <> b.1)
}

pub fn every_cross_node_type_is_found_test() {
  list.each(cross_node, fn(entry) {
    assert block(entry.0, entry.1) != []
  })
}

pub fn cross_node_messages_change_only_with_a_wire_epoch_bump_test() {
  let recorded = parse(read(history_path))
  assert check(
      recorded,
      build_info.wire_epoch,
      fingerprint(),
      wire.bridges,
      cross_node,
    )
    == Ok(Nil)
}

const shard = #("src/fluxer_gateway/guild/manager/shard.gleam", "Message")

fn rows(text: String) -> List(Row) {
  parse(text)
}

pub fn a_fingerprint_change_without_an_epoch_line_fails_test() {
  assert check(rows("3 10\n4 20"), 4, 21, [], [shard]) |> result.is_error
  assert check(rows("3 10\n4 20"), 4, 20, [], [shard]) == Ok(Nil)
}

pub fn an_epoch_bump_without_a_compatibility_entry_fails_test() {
  assert check(rows("4 20\n5 30"), 5, 30, [], [shard]) |> result.is_error
  assert check(rows("4 20\n5 30 maybe"), 5, 30, [], [shard])
    |> result.is_error
  assert check(rows("4 20\n5 30 breaking"), 5, 30, [], [shard])
    |> result.is_error
}

pub fn a_bridged_bump_needs_its_bridge_test() {
  let history = rows("4 20\n5 30 bridged")
  assert check(history, 5, 30, [Bridge(4, [shard])], [shard]) == Ok(Nil)
  assert check(history, 5, 30, [], [shard]) |> result.is_error
  assert check(history, 5, 30, [Bridge(3, [shard])], [shard])
    |> result.is_error
  assert check(history, 5, 30, [Bridge(4, [])], [shard]) |> result.is_error
  assert check(history, 5, 30, [Bridge(4, [#("x.gleam", "Y")])], [shard])
    |> result.is_error
}

pub fn a_bridge_cannot_outlive_its_epoch_or_skip_one_test() {
  assert check(rows("4 20\n5 30 bridged\n7 40 breaking v2 codec"), 7, 40, [], [
      shard,
    ])
    == Ok(Nil)
  assert check(
      rows("4 20\n5 30 bridged\n7 40 breaking v2 codec"),
      7,
      40,
      [Bridge(6, [shard])],
      [shard],
    )
    |> result.is_error
  assert check(rows("4 20\n6 30 bridged"), 6, 30, [Bridge(5, [shard])], [
      shard,
    ])
    |> result.is_error
  assert check(rows("4 20\n5 30 breaking new framing"), 5, 30, [], [shard])
    |> result.is_error
  assert check(rows("3 10\n4 20 bridged"), 4, 20, [Bridge(3, [shard])], [
      shard,
    ])
    |> result.is_error
}

pub fn history_must_ascend_and_end_on_the_current_epoch_test() {
  assert check(rows("4 20\n4 21"), 4, 21, [], [shard]) |> result.is_error
  assert check(rows("4 20\n6 30 breaking x\n5 25 bridged"), 5, 25, [], [shard])
    |> result.is_error
  assert check(rows("4 20\n6 30 breaking x"), 4, 20, [], [shard])
    |> result.is_error
}

pub fn the_skip_rule_keeps_breaking_bumps_refused_by_older_peers_test() {
  assert wire_epoch.compatible(5, 6)
  assert !wire_epoch.compatible(5, 7)
}
