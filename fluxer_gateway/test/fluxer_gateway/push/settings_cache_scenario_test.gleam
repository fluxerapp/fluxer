// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/push/fetch
import fluxer_gateway/push/server
import fluxer_gateway/push/settings
import fluxer_gateway/push/settings_cache
import fluxer_gateway/push/throttle
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import support/push_api_stub.{type Call, Reply} as stub
import support/push_throttle

type Held {
  Held(call: Call, go: Subject(Nil))
}

fn rows_json(rows: List(json.Json)) -> stub.Reply {
  stub.data(
    json.object([#("user_guild_settings", json.preprocessed_array(rows))]),
  )
}

fn good_row() -> json.Json {
  json.object([
    #("mobile_push", json.bool(True)),
    #("message_notifications", json.int(1)),
  ])
}

fn rows_for(call: Call) -> stub.Reply {
  rows_json(list.map(stub.user_ids(call), fn(_) { good_row() }))
}

fn recording(seen: Subject(Call)) -> fn(Call) -> stub.Reply {
  stub.only("get_user_guild_settings", fn(call) {
    process.send(seen, call)
    rows_for(call)
  })
}

fn holding(held: Subject(Held)) -> fn(Call) -> stub.Reply {
  fn(call) {
    let go = process.new_subject()
    process.send(held, Held(call, go))
    let _ = process.receive(go, 5000)
    rows_for(call)
  }
}

fn resolve(guild: Int, users: List(Int)) -> Dict(Int, fetch.Row) {
  fetch.resolve(guild:, users:, author: 0, deadline: clock.deadline_in(3000))
}

fn resolved(rows: Dict(Int, fetch.Row), user: Int) -> Bool {
  case dict.get(rows, user) {
    Ok(fetch.Resolved(found)) -> settings.mobile_push(found)
    _ -> False
  }
}

fn reason(
  rows: Dict(Int, fetch.Row),
  user: Int,
) -> Result(fetch.Unresolved, Nil) {
  case dict.get(rows, user) {
    Ok(fetch.Unresolved(why)) -> Ok(why)
    _ -> Error(Nil)
  }
}

fn asked(call: Call) -> List(Int) {
  list.sort(stub.user_ids(call), int.compare)
}

pub fn settings_are_fetched_once_then_served_from_cache_test() {
  push_throttle.calm(throttle.UserGuildSettings)
  let seen = process.new_subject()
  let api = stub.start(recording(seen))
  let rows = resolve(7_000_001, [11, 12, 13])
  assert list.all([11, 12, 13], resolved(rows, _))
  let assert Ok(call) = process.receive(seen, 100)
  assert call.kind == "get_user_guild_settings"
  assert asked(call) == [11, 12, 13]
  let again = resolve(7_000_001, [11, 12, 13, 14])
  assert list.all([11, 12, 13, 14], resolved(again, _))
  let assert Ok(call) = process.receive(seen, 100)
  assert asked(call) == [14]
  let assert Error(Nil) = process.receive(seen, 100)
  stub.stop(api)
}

pub fn an_invalidation_during_a_fetch_keeps_the_stale_row_out_test() {
  push_throttle.calm(throttle.UserGuildSettings)
  let held = process.new_subject()
  let api = stub.start(holding(held))
  let done = process.new_subject()
  process.spawn(fn() { process.send(done, resolve(7_000_002, [21, 22])) })
  let assert Ok(first) = process.receive(held, 1000)
  assert asked(first.call) == [21, 22]
  server.invalidate_user_guild_settings_local(21, 7_000_002)
  process.send(first.go, Nil)
  let assert Ok(rows) = process.receive(done, 3000)
  assert resolved(rows, 21) && resolved(rows, 22)
  process.spawn(fn() { process.send(done, resolve(7_000_002, [21, 22])) })
  let assert Ok(second) = process.receive(held, 1000)
  assert asked(second.call) == [21]
  process.send(second.go, Nil)
  let assert Ok(_) = process.receive(done, 3000)
  stub.stop(api)
}

pub fn a_failed_fetch_is_remembered_for_one_second_test() {
  push_throttle.calm(throttle.UserGuildSettings)
  let seen = process.new_subject()
  let api =
    stub.start(
      stub.only("get_user_guild_settings", fn(call) {
        process.send(seen, call)
        Reply(400, "{\"code\":\"NOPE\"}")
      }),
    )
  let rows = resolve(7_000_003, [31])
  assert reason(rows, 31) == Ok(fetch.ApiError)
  let assert Ok(_) = process.receive(seen, 100)
  let rows = resolve(7_000_003, [31])
  assert reason(rows, 31) == Ok(fetch.FailedEntryReason)
  let assert Error(Nil) = process.receive(seen, 100)
  stub.stop(api)
  let api = stub.start(recording(seen))
  process.sleep(1000)
  let rows = resolve(7_000_003, [31])
  assert resolved(rows, 31)
  let assert Ok(call) = process.receive(seen, 100)
  assert asked(call) == [31]
  stub.stop(api)
}

pub fn a_short_reply_leaves_nothing_reserved_test() {
  push_throttle.calm(throttle.UserGuildSettings)
  let seen = process.new_subject()
  let api =
    stub.start(
      stub.only("get_user_guild_settings", fn(call) {
        process.send(seen, call)
        rows_json([good_row()])
      }),
    )
  let rows = resolve(7_000_004, [41, 42])
  assert reason(rows, 41) == Ok(fetch.ShapeMismatch)
  assert reason(rows, 42) == Ok(fetch.ShapeMismatch)
  let assert Ok(_) = process.receive(seen, 100)
  stub.stop(api)
  let api = stub.start(recording(seen))
  let rows = resolve(7_000_004, [41, 42])
  assert resolved(rows, 41) && resolved(rows, 42)
  let assert Ok(call) = process.receive(seen, 100)
  assert asked(call) == [41, 42]
  stub.stop(api)
}

pub fn a_malformed_row_is_refetched_alone_test() {
  push_throttle.calm(throttle.UserGuildSettings)
  let seen = process.new_subject()
  let api =
    stub.start(
      stub.only("get_user_guild_settings", fn(call) {
        process.send(seen, call)
        rows_json([good_row(), json.string("garbage"), json.null()])
      }),
    )
  let rows = resolve(7_000_005, [51, 52, 53])
  assert resolved(rows, 51)
  assert reason(rows, 52) == Ok(fetch.MalformedRow)
  assert resolved(rows, 53)
  let assert Ok(_) = process.receive(seen, 100)
  stub.stop(api)
  let api = stub.start(recording(seen))
  let rows = resolve(7_000_005, [51, 52, 53])
  assert list.all([51, 52, 53], resolved(rows, _))
  let assert Ok(call) = process.receive(seen, 100)
  assert asked(call) == [52]
  stub.stop(api)
}

pub fn a_saturated_throttle_skips_the_api_test() {
  push_throttle.saturate(throttle.UserGuildSettings)
  let seen = process.new_subject()
  let api = stub.start(recording(seen))
  let outcomes =
    int.range(1, 41, [], fn(acc, index) {
      [reason(resolve(7_100_000 + index, [61]), 61), ..acc]
    })
  let throttled =
    list.count(outcomes, fn(found) { found == Ok(fetch.Throttled) })
  let calls = drain(seen, 0)
  assert throttled + calls == 40
  assert throttled >= 20
  stub.stop(api)
  push_throttle.calm(throttle.UserGuildSettings)
  let api = stub.start(recording(seen))
  let rows = resolve(7_100_100, [61])
  assert resolved(rows, 61)
  stub.stop(api)
}

fn drain(seen: Subject(Call), count: Int) -> Int {
  case process.receive(seen, 50) {
    Ok(_) -> drain(seen, count + 1)
    Error(Nil) -> count
  }
}

type TimeUnit {
  Millisecond
  Second
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

@external(erlang, "erlang", "system_time")
fn system_time(unit: TimeUnit) -> Int

@external(erlang, "ets", "insert")
fn ets_insert(table: Atom, row: row) -> Bool

@external(erlang, "ets", "lookup")
fn ets_lookup(table: Atom, key: key) -> List(Dynamic)

fn settings_table() -> Atom {
  atom.create("push_user_guild_settings")
}

fn blocked_table() -> Atom {
  atom.create("push_blocked_ids")
}

fn put_settings_row(user: Int, guild: Int, entry: entry) -> Nil {
  let _ =
    ets_insert(settings_table(), #(
      atom.create("settings_row"),
      #(user, guild),
      entry,
    ))
  Nil
}

pub fn an_expired_settings_row_is_refetched_and_replaced_test() {
  push_throttle.calm(throttle.UserGuildSettings)
  let past = monotonic_time(Millisecond) - 1
  put_settings_row(71, 7_000_006, #(
    atom.create("known"),
    settings.defaults(),
    past,
  ))
  let seen = process.new_subject()
  let api = stub.start(recording(seen))
  let rows = resolve(7_000_006, [71])
  assert resolved(rows, 71)
  let assert Ok(call) = process.receive(seen, 100)
  assert asked(call) == [71]
  let _ = resolve(7_000_006, [71])
  let assert Error(Nil) = process.receive(seen, 100)
  stub.stop(api)
}

pub fn eviction_removes_only_expired_and_abandoned_rows_test() {
  let now = monotonic_time(Millisecond)
  put_settings_row(81, 7_000_007, #(
    atom.create("known"),
    settings.defaults(),
    now - 1,
  ))
  put_settings_row(82, 7_000_007, #(
    atom.create("known"),
    settings.defaults(),
    now + 60_000,
  ))
  put_settings_row(83, 7_000_007, #(atom.create("failed"), now - 1))
  put_settings_row(84, 7_000_007, #(atom.create("pending"), 1, now - 121_000))
  put_settings_row(85, 7_000_007, #(atom.create("pending"), 2, now))
  let now_s = system_time(Second)
  let _ =
    ets_insert(blocked_table(), #(
      atom.create("blocked_row"),
      8_300_001,
      #(atom.create("blocked_fetched"), [1], now_s - 1),
    ))
  let _ =
    ets_insert(blocked_table(), #(
      atom.create("blocked_row"),
      8_300_002,
      #(atom.create("blocked_fetched"), [1], now_s + 60),
    ))
  let _ =
    ets_insert(blocked_table(), #(
      atom.create("blocked_row"),
      8_300_003,
      #(atom.create("blocked_pending"), 3, now - 121_000),
    ))
  settings_cache.evict()
  let present = fn(user) {
    ets_lookup(settings_table(), #(user, 7_000_007)) != []
  }
  assert [81, 82, 83, 84, 85] |> list.map(present)
    == [False, True, False, False, True]
  let blocked = fn(user) { ets_lookup(blocked_table(), user) != [] }
  assert [8_300_001, 8_300_002, 8_300_003] |> list.map(blocked)
    == [False, True, False]
  assert settings_cache.blocked_ids(8_300_002) == option.Some([1])
}
