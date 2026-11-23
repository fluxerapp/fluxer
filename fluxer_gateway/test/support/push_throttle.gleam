// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/throttle
import gleam/int

pub fn calm(kind: throttle.RequestType) -> Nil {
  calm_loop(kind, 1000)
}

fn calm_loop(kind: throttle.RequestType, batch: Int) -> Nil {
  let assert True = batch < 100_000_000
  accept(kind, batch)
  case admitted(kind, 200, 0) {
    200 -> accept(kind, batch)
    _ -> calm_loop(kind, batch * 2)
  }
}

fn accept(kind: throttle.RequestType, count: Int) -> Nil {
  int.range(0, count, Nil, fn(_, _) { throttle.record_accept(kind) })
}

pub fn admitted(kind: throttle.RequestType, offers: Int, acc: Int) -> Int {
  case offers {
    0 -> acc
    _ ->
      case throttle.offer(kind) {
        throttle.Admitted -> admitted(kind, offers - 1, acc + 1)
        throttle.Rejected -> admitted(kind, offers - 1, acc)
      }
  }
}

pub fn saturate(kind: throttle.RequestType) -> Nil {
  saturate_loop(kind, 2000)
}

fn saturate_loop(kind: throttle.RequestType, rounds: Int) -> Nil {
  let assert True = rounds > 0
  case admitted(kind, 1000, 0) < 200 {
    True -> Nil
    False -> saturate_loop(kind, rounds - 1)
  }
}
