// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/push/recipients
import fluxer_gateway/push/server
import fluxer_gateway/push/throttle
import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/otp/actor
import support/push_api_stub.{Reply} as stub
import support/push_throttle

type Gauge {
  Begin
  End
  Peak(reply: Subject(#(Int, Int)))
}

fn start_gauge() -> Subject(Gauge) {
  let assert Ok(started) =
    actor.new(#(0, 0, 0))
    |> actor.on_message(fn(state, message) {
      let #(now, peak, total) = state
      case message {
        Begin -> actor.continue(#(now + 1, int.max(peak, now + 1), total + 1))
        End -> actor.continue(#(now - 1, peak, total))
        Peak(reply) -> {
          process.send(reply, #(peak, total))
          actor.continue(state)
        }
      }
    })
    |> actor.start
  started.data
}

fn blocked_reply(
  users: List(Int),
  blocked_by: fn(Int) -> List(Int),
) -> stub.Reply {
  stub.data(
    json.object(
      list.map(users, fn(user) {
        #(
          int.to_string(user),
          json.array(blocked_by(user), fn(other) {
            json.string(int.to_string(other))
          }),
        )
      }),
    ),
  )
}

fn none(_user: Int) -> List(Int) {
  []
}

fn users(from: Int, count: Int) -> List(Int) {
  int.range(from, from + count, [], fn(acc, user) { [user, ..acc] })
  |> list.reverse
}

pub fn every_chunk_is_fetched_with_at_most_eight_in_flight_test() {
  push_throttle.calm(throttle.BlockedIds)
  let gauge = start_gauge()
  let api =
    stub.start(
      stub.only("get_user_blocked_ids", fn(call) {
        process.send(gauge, Begin)
        process.sleep(100)
        process.send(gauge, End)
        blocked_reply(stub.user_ids(call), fn(user) {
          case user == 8_000_007 {
            True -> [42]
            False -> []
          }
        })
      }),
    )
  let everyone = users(8_000_000, 4500)
  let known = recipients.known_blocked(everyone)
  assert dict.size(known) == 4500
  assert dict.get(known, 8_000_007) == Ok([42])
  assert dict.get(known, 8_004_499) == Ok([])
  let #(peak, total) = process.call(gauge, 1000, Peak)
  assert total == 9
  assert peak <= 8
  let again = recipients.known_blocked(everyone)
  assert dict.size(again) == 4500
  let #(_, total) = process.call(gauge, 1000, Peak)
  assert total == 9
  stub.stop(api)
}

pub fn a_failed_chunk_does_not_lose_the_others_test() {
  push_throttle.calm(throttle.BlockedIds)
  let seen = process.new_subject()
  let api =
    stub.start(
      stub.only("get_user_blocked_ids", fn(call) {
        let asked = stub.user_ids(call)
        process.send(seen, asked)
        case list.contains(asked, 8_100_000) {
          True -> Reply(400, "{\"code\":\"NOPE\"}")
          False -> blocked_reply(asked, none)
        }
      }),
    )
  let everyone = users(8_100_000, 1200)
  let known = recipients.known_blocked(everyone)
  assert dict.size(known) == 700
  assert dict.has_key(known, 8_100_000) == False
  assert dict.has_key(known, 8_100_500)
  assert dict.has_key(known, 8_101_199)
  let assert 3 = drain(seen, 0)
  let known = recipients.known_blocked(everyone)
  assert dict.size(known) == 700
  let assert 1 = drain(seen, 0)
  stub.stop(api)
}

pub fn an_unblock_during_a_fetch_is_not_overwritten_test() {
  push_throttle.calm(throttle.BlockedIds)
  let held = process.new_subject()
  let api =
    stub.start(
      stub.only("get_user_blocked_ids", fn(call) {
        let go = process.new_subject()
        process.send(held, #(stub.user_ids(call), go))
        let _ = process.receive(go, 5000)
        blocked_reply(stub.user_ids(call), fn(_) { [99] })
      }),
    )
  let done = process.new_subject()
  process.spawn(fn() {
    process.send(done, recipients.known_blocked([8_200_001, 8_200_002]))
  })
  let assert Ok(#(_, go)) = process.receive(held, 1000)
  let assert Ok(user) = id.user_id(8_200_001)
  server.invalidate_blocked_ids(user)
  process.sleep(50)
  process.send(go, Nil)
  let assert Ok(first) = process.receive(done, 3000)
  assert dict.get(first, 8_200_001) == Ok([99])
  process.spawn(fn() {
    process.send(done, recipients.known_blocked([8_200_001, 8_200_002]))
  })
  let assert Ok(#(asked, go)) = process.receive(held, 1000)
  assert asked == [8_200_001]
  process.send(go, Nil)
  let assert Ok(second) = process.receive(done, 3000)
  assert dict.size(second) == 2
  let assert Error(Nil) = process.receive(held, 100)
  stub.stop(api)
}

fn drain(seen: Subject(a), count: Int) -> Int {
  case process.receive(seen, 50) {
    Ok(_) -> drain(seen, count + 1)
    Error(Nil) -> count
  }
}
