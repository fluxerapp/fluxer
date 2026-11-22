// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/throttle
import gleam/float
import gleam/int

pub fn probability_is_zero_while_accepts_keep_up_test() {
  let assert True = throttle.probability_of(10, 5) == 0.0
  let assert True = throttle.probability_of(0, 0) == 0.0
}

pub fn probability_is_capped_test() {
  let assert True = throttle.probability_of(1000, 0) == throttle.max_rejection
}

pub fn probability_formula_test() {
  let p = throttle.probability_of(9, 2)
  let assert True = float.loosely_equals(p, 0.5, 0.000001)
}

pub fn window_forgets_old_buckets_test() {
  let window =
    int.range(1, 101, throttle.new_window(), fn(window, _) {
      let #(_, window) = throttle.offer_window(window, 0, 1.0)
      window
    })
  let assert #(100, 0) = throttle.totals(window, 0)
  let assert #(100, 0) = throttle.totals(window, 119_999)
  let assert #(0, 0) = throttle.totals(window, 120_000)
}

type Lcg {
  Lcg(state: Int)
}

fn draw(rng: Lcg) -> #(Float, Lcg) {
  let state = { rng.state * 1_103_515_245 + 12_345 } % 2_147_483_648
  #(int.to_float(state) /. 2_147_483_648.0, Lcg(state))
}

fn replay(
  window: throttle.Window,
  rng: Lcg,
  now_ms: Int,
  until_ms: Int,
  step_ms: Int,
  backend_up: Bool,
  attempts: Int,
) -> #(throttle.Window, Lcg, Int) {
  case now_ms >= until_ms {
    True -> #(window, rng, attempts)
    False -> {
      let #(value, rng) = draw(rng)
      let #(decision, window) = throttle.offer_window(window, now_ms, value)
      let #(window, attempts) = case decision, backend_up {
        throttle.Admitted, True -> #(
          throttle.accept_window(window, now_ms),
          attempts + 1,
        )
        throttle.Admitted, False -> #(window, attempts + 1)
        throttle.Rejected, _ -> #(window, attempts)
      }
      replay(
        window,
        rng,
        now_ms + step_ms,
        until_ms,
        step_ms,
        backend_up,
        attempts,
      )
    }
  }
}

pub fn outage_keeps_a_trickle_and_recovers_test() {
  let #(window, rng, _) =
    replay(throttle.new_window(), Lcg(1), 0, 600_000, 100, False, 0)
  let #(_, _, attempts) = replay(window, rng, 600_000, 660_000, 100, False, 0)
  let fraction = int.to_float(attempts) /. 600.0
  let assert True = fraction >=. 0.05 && fraction <=. 0.15
  let #(recovered, _, _) = replay(window, rng, 600_000, 900_000, 100, True, 0)
  let assert True = throttle.rejection_probability(recovered, 900_000) <. 0.1
}
