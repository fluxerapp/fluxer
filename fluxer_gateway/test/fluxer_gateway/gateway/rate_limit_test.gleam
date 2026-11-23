// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/opcode
import fluxer_gateway/gateway/rate_limit
import gleam/list

pub fn window_test() {
  assert rate_limit.window([], 100, 1000, 2) == Ok([100])
  assert rate_limit.window([50], 100, 1000, 2) == Ok([100, 50])
  assert rate_limit.window([60, 50], 100, 1000, 2) == Error(Nil)
  assert rate_limit.window([60, 50], 1055, 1000, 2) == Ok([1055, 60])
}

fn run(rates, op, times) {
  list.fold(times, #(rates, []), fn(acc, now) {
    let #(rates, verdicts) = acc
    case rate_limit.check_connection(rates, op, now) {
      rate_limit.Allowed(next) -> #(next, ["allowed", ..verdicts])
      rate_limit.OpcodeLimited(next) -> #(next, ["op", ..verdicts])
      rate_limit.ConnectionLimited -> #(rates, ["connection", ..verdicts])
    }
  })
}

pub fn presence_limit_test() {
  let #(_, verdicts) =
    run(rate_limit.new(), opcode.PresenceUpdate, [1, 2, 3, 4, 5, 6])
  assert list.reverse(verdicts)
    == ["allowed", "allowed", "allowed", "allowed", "allowed", "op"]
}

pub fn presence_window_slides_test() {
  let #(_, verdicts) =
    run(rate_limit.new(), opcode.PresenceUpdate, [1, 2, 3, 4, 5, 20_001])
  assert list.reverse(verdicts)
    == ["allowed", "allowed", "allowed", "allowed", "allowed", "allowed"]
}

pub fn connection_limit_test() {
  let times = list.repeat(1, 601)
  let #(_, verdicts) = run(rate_limit.new(), opcode.Heartbeat, times)
  assert list.first(verdicts) == Ok("connection")
  assert list.count(verdicts, fn(v) { v == "allowed" }) == 600
}
