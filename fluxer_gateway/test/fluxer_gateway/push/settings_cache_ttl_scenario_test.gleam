// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{
  type PushDurations, Durations, PushDurations,
}
import fluxer_gateway/core/clock
import fluxer_gateway/push/fetch
import fluxer_gateway/push/recipients
import fluxer_gateway/push/settings_cache
import fluxer_gateway/push/throttle
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json
import gleam/list
import support/isolation
import support/push_api_stub.{type Call, Reply} as stub
import support/push_throttle
import support/wait

fn ttl(change: fn(PushDurations) -> PushDurations) -> Nil {
  isolation.set_durations(fn(durations) {
    Durations(..durations, push: change(durations.push))
  })
}

fn settings_rows(seen: Subject(List(Int))) -> fn(Call) -> stub.Reply {
  use call <- stub.only("get_user_guild_settings")
  process.send(seen, stub.user_ids(call))
  stub.data(
    json.object([
      #(
        "user_guild_settings",
        json.preprocessed_array(
          list.map(stub.user_ids(call), fn(_) {
            json.object([#("mobile_push", json.bool(True))])
          }),
        ),
      ),
    ]),
  )
}

fn resolve(guild: Int, user: Int) -> Dict(Int, fetch.Row) {
  fetch.resolve(
    guild:,
    users: [user],
    author: 0,
    deadline: clock.deadline_in(3000),
  )
}

fn is_resolved(rows: Dict(Int, fetch.Row), user: Int) -> Bool {
  case dict.get(rows, user) {
    Ok(fetch.Resolved(_)) -> True
    _ -> False
  }
}

pub fn cached_settings_are_fetched_again_once_their_ttl_passes_test() {
  push_throttle.calm(throttle.UserGuildSettings)
  ttl(fn(push) { PushDurations(..push, settings_cache_settings_ttl_ms: 300) })
  let seen = process.new_subject()
  let api = stub.start(settings_rows(seen))
  assert is_resolved(resolve(7_300_001, 101), 101)
  let assert Ok([101]) = process.receive(seen, 100)
  assert is_resolved(resolve(7_300_001, 101), 101)
  let assert Error(Nil) = process.receive(seen, 100)
  process.sleep(300)
  assert is_resolved(resolve(7_300_001, 101), 101)
  let assert Ok([101]) = process.receive(seen, 100)
  stub.stop(api)
}

pub fn a_failure_entry_lasts_as_long_as_configured_test() {
  push_throttle.calm(throttle.UserGuildSettings)
  ttl(fn(push) { PushDurations(..push, settings_cache_failure_entry_ms: 300) })
  let seen = process.new_subject()
  let api =
    stub.start({
      use call <- stub.only("get_user_guild_settings")
      process.send(seen, stub.user_ids(call))
      Reply(500, "{}")
    })
  let assert Ok(fetch.Unresolved(fetch.ApiError)) =
    dict.get(resolve(7_300_002, 102), 102)
  let assert Ok([102]) = process.receive(seen, 100)
  let assert Ok(fetch.Unresolved(fetch.FailedEntryReason)) =
    dict.get(resolve(7_300_002, 102), 102)
  let assert Error(Nil) = process.receive(seen, 100)
  stub.stop(api)
  process.sleep(250)
  let api = stub.start(settings_rows(seen))
  assert is_resolved(resolve(7_300_002, 102), 102)
  let assert Ok([102]) = process.receive(seen, 100)
  stub.stop(api)
}

pub fn an_abandoned_reservation_is_evicted_after_its_ttl_test() {
  push_throttle.calm(throttle.UserGuildSettings)
  ttl(fn(push) { PushDurations(..push, settings_cache_reservation_ttl_ms: 200) })
  settings_cache.reserve_settings(settings_cache.new_fill(), [103], 7_300_003)
  let seen = process.new_subject()
  let api = stub.start(settings_rows(seen))
  assert is_resolved(resolve(7_300_003, 103), 103)
  let assert Ok([103]) = process.receive(seen, 100)
  assert is_resolved(resolve(7_300_003, 103), 103)
  let assert Ok([103]) = process.receive(seen, 100)
  settings_cache.evict()
  assert is_resolved(resolve(7_300_003, 103), 103)
  let assert Ok([103]) = process.receive(seen, 100)
  process.sleep(250)
  settings_cache.evict()
  assert is_resolved(resolve(7_300_003, 103), 103)
  let assert Ok([103]) = process.receive(seen, 100)
  assert is_resolved(resolve(7_300_003, 103), 103)
  let assert Error(Nil) = process.receive(seen, 100)
  stub.stop(api)
}

pub fn blocked_ids_expire_after_their_ttl_test() {
  push_throttle.calm(throttle.BlockedIds)
  ttl(fn(push) {
    PushDurations(..push, settings_cache_blocked_ids_ttl_seconds: 2)
  })
  let seen = process.new_subject()
  let api =
    stub.start({
      use call <- stub.only("get_user_blocked_ids")
      process.send(seen, stub.user_ids(call))
      stub.data(
        json.object(
          list.map(stub.user_ids(call), fn(user) {
            #(int.to_string(user), json.array(["9"], json.string))
          }),
        ),
      )
    })
  let expected = dict.from_list([#(8_500_001, [9])])
  assert recipients.known_blocked([8_500_001]) == expected
  let assert Ok([8_500_001]) = process.receive(seen, 100)
  assert recipients.known_blocked([8_500_001]) == expected
  let assert Error(Nil) = process.receive(seen, 100)
  process.sleep(2100)
  assert recipients.known_blocked([8_500_001]) == expected
  let assert Ok([8_500_001]) = process.receive(seen, 100)
  stub.stop(api)
}

@external(erlang, "erlang", "binary_to_atom")
fn process_name(name: String) -> process.Name(a)

pub fn the_caches_outlive_a_push_server_crash_test() {
  push_throttle.calm(throttle.UserGuildSettings)
  let seen = process.new_subject()
  let api = stub.start(settings_rows(seen))
  assert is_resolved(resolve(7_300_004, 104), 104)
  let assert Ok([104]) = process.receive(seen, 100)
  let push = process_name("fluxer_gateway_push")
  let assert Ok(before) = process.named(push)
  process.kill(before)
  let lookups =
    int.range(0, 2000, 0, fn(acc, _) {
      case settings_cache.lookup_settings(104, 7_300_004) {
        settings_cache.Cached(_) -> acc + 1
        _ -> acc
      }
    })
  assert lookups == 2000
  let assert True =
    wait.until(3000, fn() {
      case process.named(push) {
        Ok(after) -> after != before
        Error(Nil) -> False
      }
    })
  assert is_resolved(resolve(7_300_004, 104), 104)
  let assert Error(Nil) = process.receive(seen, 100)
  stub.stop(api)
}
