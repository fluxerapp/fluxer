// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{Durations, PushDurations}
import fluxer_gateway/core/clock
import fluxer_gateway/push/fetch
import fluxer_gateway/push/recipients
import fluxer_gateway/push/settings as push_settings
import fluxer_gateway/push/throttle
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json
import gleam/list
import support/isolation
import support/push_api_stub.{type Call} as stub
import support/push_crash
import support/push_permit_probe as probe
import support/push_throttle

const fetch_module = "fluxer_gateway@push@fetch"

const recipients_module = "fluxer_gateway@push@recipients"

fn settings_rows(call: Call) -> stub.Reply {
  stub.data(
    json.object([
      #(
        "user_guild_settings",
        json.preprocessed_array(
          list.map(stub.user_ids(call), fn(_) {
            json.object([
              #("mobile_push", json.bool(True)),
              #("message_notifications", json.int(1)),
            ])
          }),
        ),
      ),
    ]),
  )
}

fn blocked_rows(call: Call) -> stub.Reply {
  stub.data(
    json.object(
      list.map(stub.user_ids(call), fn(user) {
        #(
          int.to_string(user),
          json.array([7], fn(other) { json.string(int.to_string(other)) }),
        )
      }),
    ),
  )
}

type Seen {
  Seen(kind: String, users: List(Int))
}

fn first_call_crashes(
  seen: Subject(Seen),
  kind: String,
  module: String,
  function: String,
  reply: fn(Call) -> stub.Reply,
) -> fn(Call) -> stub.Reply {
  let first = push_crash.once()
  use call <- stub.only(kind)
  process.send(
    seen,
    Seen(call.kind, list.sort(stub.user_ids(call), int.compare)),
  )
  case first() {
    True -> {
      let assert True = push_crash.kill_running(module, function) > 0
      Nil
    }
    False -> Nil
  }
  reply(call)
}

fn resolve(guild: Int, users: List(Int), wait_ms: Int) -> Dict(Int, fetch.Row) {
  fetch.resolve(guild:, users:, author: 0, deadline: clock.deadline_in(wait_ms))
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

fn resolved(rows: Dict(Int, fetch.Row), user: Int) -> Bool {
  case dict.get(rows, user) {
    Ok(fetch.Resolved(found)) -> push_settings.mobile_push(found)
    _ -> False
  }
}

pub fn a_settings_fetcher_that_dies_mid_request_spares_the_caller_test() {
  push_throttle.calm(throttle.UserGuildSettings)
  let held = probe.held_count()
  let seen = process.new_subject()
  let api =
    stub.start(first_call_crashes(
      seen,
      "get_user_guild_settings",
      fetch_module,
      "fetch_chunk",
      settings_rows,
    ))
  let rows = resolve(7_200_001, [91, 92], 3000)
  assert reason(rows, 91) == Ok(fetch.WorkerLost)
  assert reason(rows, 92) == Ok(fetch.WorkerLost)
  let assert Ok(Seen(_, [91, 92])) = process.receive(seen, 100)
  let assert True = probe.eventually(fn() { probe.held_count() == held }, 100)
  let again = resolve(7_200_001, [91, 92], 3000)
  assert resolved(again, 91) && resolved(again, 92)
  let assert Ok(Seen(_, [91, 92])) = process.receive(seen, 100)
  let cached = resolve(7_200_001, [91, 92], 3000)
  assert resolved(cached, 91) && resolved(cached, 92)
  let assert Error(Nil) = process.receive(seen, 100)
  stub.stop(api)
}

pub fn a_fetch_past_its_deadline_reaps_its_workers_test() {
  push_throttle.calm(throttle.UserGuildSettings)
  isolation.set_durations(fn(durations) {
    Durations(
      ..durations,
      push: PushDurations(..durations.push, fetch_result_slack_ms: 50),
    )
  })
  let held = probe.held_count()
  let seen = process.new_subject()
  let api =
    stub.start({
      use call <- stub.only("get_user_guild_settings")
      process.send(seen, Seen(call.kind, stub.user_ids(call)))
      let _ = push_crash.freeze_running(fetch_module, "fetch_chunk")
      settings_rows(call)
    })
  let started = clock.monotonic_ms()
  let rows = resolve(7_200_002, [93], 200)
  assert clock.monotonic_ms() - started < 1000
  assert reason(rows, 93) == Ok(fetch.Timeout)
  assert push_crash.running(fetch_module, "fetch_chunk") == []
  let assert True = probe.eventually(fn() { probe.held_count() == held }, 100)
  let assert Ok(_) = process.receive(seen, 100)
  stub.stop(api)
  let api =
    stub.start({
      use call <- stub.only("get_user_guild_settings")
      process.send(seen, Seen(call.kind, stub.user_ids(call)))
      settings_rows(call)
    })
  let again = resolve(7_200_002, [93], 3000)
  assert resolved(again, 93)
  let assert Ok(Seen(_, [93])) = process.receive(seen, 100)
  let cached = resolve(7_200_002, [93], 3000)
  assert resolved(cached, 93)
  let assert Error(Nil) = process.receive(seen, 100)
  assert push_crash.mailbox_size(process.self()) == 0
  stub.stop(api)
}

pub fn a_blocked_ids_fetcher_that_dies_spares_the_caller_test() {
  push_throttle.calm(throttle.BlockedIds)
  let seen = process.new_subject()
  let api =
    stub.start(first_call_crashes(
      seen,
      "get_user_blocked_ids",
      recipients_module,
      "fetch_blocked_chunk",
      blocked_rows,
    ))
  let caller = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      process.send(caller, recipients.known_blocked([8_400_001, 8_400_002]))
    })
  let assert Ok(lost) = process.receive(caller, 3000)
  assert lost == dict.new()
  let assert True = probe.eventually(fn() { !process.is_alive(pid) }, 100)
  let assert Ok(Seen(_, [8_400_001, 8_400_002])) = process.receive(seen, 100)
  let again = recipients.known_blocked([8_400_001, 8_400_002])
  assert again == dict.from_list([#(8_400_001, [7]), #(8_400_002, [7])])
  let assert Ok(Seen(_, [8_400_001, 8_400_002])) = process.receive(seen, 100)
  let cached = recipients.known_blocked([8_400_001, 8_400_002])
  assert cached == again
  let assert Error(Nil) = process.receive(seen, 100)
  stub.stop(api)
}
