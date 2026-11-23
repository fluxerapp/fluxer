// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import gleam/int
import gleam/list

fn default_window_ms() -> Int {
  durations.api().breaker_window_ms
}

fn default_bucket_ms() -> Int {
  durations.api().breaker_bucket_ms
}

pub type Config {
  Config(
    window_ms: Int,
    bucket_ms: Int,
    min_calls: Int,
    min_failures: Int,
    probes: Int,
  )
}

pub fn default_config() -> Config {
  Config(
    window_ms: default_window_ms(),
    bucket_ms: default_bucket_ms(),
    min_calls: 20,
    min_failures: 0,
    probes: 4,
  )
}

pub type State {
  Closed
  Open(until: Int)
  HalfOpen(succeeded: Int)
}

pub type Outcome {
  Success
  Failure
}

pub opaque type Breaker {
  Breaker(config: Config, state: State, buckets: List(Bucket))
}

type Bucket {
  Bucket(index: Int, calls: Int, failures: Int)
}

pub fn new(config: Config) -> Breaker {
  Breaker(config:, state: Closed, buckets: [])
}

pub fn reset(breaker: Breaker) -> Breaker {
  new(breaker.config)
}

pub fn state(breaker: Breaker) -> State {
  breaker.state
}

pub fn expire(breaker: Breaker, now_ms: Int) -> Breaker {
  case breaker.state {
    Open(until) if now_ms >= until ->
      Breaker(..breaker, state: HalfOpen(0), buckets: [])
    _ -> breaker
  }
}

pub fn record(
  breaker: Breaker,
  outcome: Outcome,
  now_ms: Int,
  open_for_ms: Int,
) -> Breaker {
  case breaker.state, outcome {
    Closed, _ -> {
      let breaker = Breaker(..breaker, buckets: add(breaker, outcome, now_ms))
      case tripped(breaker) {
        True -> open(breaker, now_ms, open_for_ms)
        False -> breaker
      }
    }
    Open(_), _ -> breaker
    HalfOpen(_), Failure -> open(breaker, now_ms, open_for_ms)
    HalfOpen(succeeded), Success ->
      case succeeded + 1 >= breaker.config.probes {
        True -> Breaker(..breaker, state: Closed, buckets: [])
        False -> Breaker(..breaker, state: HalfOpen(succeeded + 1))
      }
  }
}

fn open(breaker: Breaker, now_ms: Int, open_for_ms: Int) -> Breaker {
  Breaker(..breaker, state: Open(now_ms + int.max(open_for_ms, 0)), buckets: [])
}

fn add(breaker: Breaker, outcome: Outcome, now_ms: Int) -> List(Bucket) {
  let config = breaker.config
  let index = now_ms / config.bucket_ms
  let oldest = index - config.window_ms / config.bucket_ms + 1
  let failed = case outcome {
    Failure -> 1
    Success -> 0
  }
  let kept = list.filter(breaker.buckets, fn(bucket) { bucket.index >= oldest })
  case kept {
    [Bucket(current, calls, failures), ..rest] if current == index -> [
      Bucket(current, calls + 1, failures + failed),
      ..rest
    ]
    _ -> [Bucket(index, 1, failed), ..kept]
  }
}

fn tripped(breaker: Breaker) -> Bool {
  let #(calls, failures) =
    list.fold(breaker.buckets, #(0, 0), fn(acc, bucket) {
      #(acc.0 + bucket.calls, acc.1 + bucket.failures)
    })
  calls >= breaker.config.min_calls
  && failures >= breaker.config.min_failures
  && failures * 2 > calls
}
