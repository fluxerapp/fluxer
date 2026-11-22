// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/float
import gleam/int
import gleam/list
import gleam/result

fn bucket_ms() -> Int {
  durations.push().throttle_bucket_ms
}

const bucket_count = 24

const accept_multiplier = 2

pub const max_rejection = 0.9

const empty_stamp = -4_611_686_018_427_387_904

const rotating_stamp = -4_611_686_018_427_387_903

const rotate_spins = 1000

pub type Decision {
  Admitted
  Rejected
}

pub type RequestType {
  UserGuildSettings
  BlockedIds
  AdultContent
  ReadState
}

type Bucket {
  Bucket(requests: Int, accepts: Int)
}

pub opaque type Window {
  Window(buckets: Dict(Int, Bucket))
}

pub fn new_window() -> Window {
  Window(buckets: dict.new())
}

fn slot(now_ms: Int) -> Int {
  case now_ms >= 0 {
    True -> now_ms / bucket_ms()
    False -> { now_ms - bucket_ms() + 1 } / bucket_ms()
  }
}

fn live(window: Window, now_ms: Int) -> Window {
  let current = slot(now_ms)
  let oldest = current - bucket_count + 1
  Window(
    buckets: dict.filter(window.buckets, fn(index, _) {
      index >= oldest && index <= current
    }),
  )
}

pub fn totals(window: Window, now_ms: Int) -> #(Int, Int) {
  let Window(buckets) = live(window, now_ms)
  dict.fold(buckets, #(0, 0), fn(acc, _, bucket) {
    #(acc.0 + bucket.requests, acc.1 + bucket.accepts)
  })
}

pub fn probability_of(requests: Int, accepts: Int) -> Float {
  let excess = requests - accept_multiplier * accepts
  case excess <= 0 {
    True -> 0.0
    False ->
      float.min(
        max_rejection,
        int.to_float(excess) /. int.to_float(requests + 1),
      )
  }
}

pub fn rejection_probability(window: Window, now_ms: Int) -> Float {
  let #(requests, accepts) = totals(window, now_ms)
  probability_of(requests, accepts)
}

fn bump(window: Window, now_ms: Int, requests: Int, accepts: Int) -> Window {
  let Window(buckets) = live(window, now_ms)
  let index = slot(now_ms)
  let current =
    dict.get(buckets, index) |> result.unwrap(Bucket(requests: 0, accepts: 0))
  Window(buckets: dict.insert(
    buckets,
    index,
    Bucket(
      requests: current.requests + requests,
      accepts: current.accepts + accepts,
    ),
  ))
}

pub fn offer_window(
  window: Window,
  now_ms: Int,
  draw: Float,
) -> #(Decision, Window) {
  let p = rejection_probability(window, now_ms)
  let counted = bump(window, now_ms, 1, 0)
  case draw <. p {
    True -> #(Rejected, counted)
    False -> #(Admitted, counted)
  }
}

pub fn accept_window(window: Window, now_ms: Int) -> Window {
  bump(window, now_ms, 0, 1)
}

type AtomicsRef

type AtomicsOption {
  Signed(Bool)
}

type TimeUnit {
  Millisecond
}

type Key {
  FluxerGatewayPushThrottle(RequestType)
}

@external(erlang, "atomics", "new")
fn atomics_new(arity: Int, options: List(AtomicsOption)) -> AtomicsRef

@external(erlang, "atomics", "get")
fn atomics_get(ref: AtomicsRef, index: Int) -> Int

@external(erlang, "atomics", "add")
fn atomics_add(ref: AtomicsRef, index: Int, value: Int) -> Atom

@external(erlang, "atomics", "put")
fn atomics_put(ref: AtomicsRef, index: Int, value: Int) -> Atom

@external(erlang, "atomics", "compare_exchange")
fn atomics_compare_exchange(
  ref: AtomicsRef,
  index: Int,
  expected: Int,
  desired: Int,
) -> Dynamic

@external(erlang, "persistent_term", "get")
fn term_get(key: Key, default: List(AtomicsRef)) -> List(AtomicsRef)

@external(erlang, "persistent_term", "put")
fn term_put(key: Key, value: List(AtomicsRef)) -> Atom

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

@external(erlang, "rand", "uniform")
fn uniform() -> Float

pub fn setup() -> Nil {
  list.each([UserGuildSettings, BlockedIds, AdultContent, ReadState], fn(kind) {
    let _ = cells(kind)
    Nil
  })
}

@internal
pub fn reset() -> Nil {
  list.each([UserGuildSettings, BlockedIds, AdultContent, ReadState], fn(kind) {
    case term_get(FluxerGatewayPushThrottle(kind), []) {
      [ref, ..] ->
        int.range(0, bucket_count, Nil, fn(_, bucket) {
          let _ = atomics_put(ref, bucket * 3 + 1, empty_stamp)
          let _ = atomics_put(ref, bucket * 3 + 2, 0)
          let _ = atomics_put(ref, bucket * 3 + 3, 0)
          Nil
        })
      [] -> Nil
    }
  })
}

@internal
pub fn counted(kind: RequestType) -> #(Int, Int) {
  node_totals(cells(kind), monotonic_time(Millisecond))
}

fn cells(kind: RequestType) -> AtomicsRef {
  case term_get(FluxerGatewayPushThrottle(kind), []) {
    [ref, ..] -> ref
    [] -> {
      let ref = atomics_new(bucket_count * 3, [Signed(True)])
      int.range(0, bucket_count, Nil, fn(_, bucket) {
        let _ = atomics_put(ref, bucket * 3 + 1, empty_stamp)
        Nil
      })
      let _ = term_put(FluxerGatewayPushThrottle(kind), [ref])
      ref
    }
  }
}

fn current_bucket(ref: AtomicsRef, now_ms: Int) -> Int {
  let current = slot(now_ms)
  let bucket = int.modulo(current, bucket_count) |> result.unwrap(0)
  claim_bucket(ref, bucket * 3 + 1, current, rotate_spins)
  bucket
}

fn claim_bucket(
  ref: AtomicsRef,
  stamp_index: Int,
  current: Int,
  spins: Int,
) -> Nil {
  let stamp = atomics_get(ref, stamp_index)
  case stamp == current, stamp == rotating_stamp && spins > 0 {
    True, _ -> Nil
    False, True -> claim_bucket(ref, stamp_index, current, spins - 1)
    False, False ->
      case stamp > current && stamp != rotating_stamp {
        True -> Nil
        False ->
          case swapped(ref, stamp_index, stamp, rotating_stamp) {
            False -> claim_bucket(ref, stamp_index, current, spins)
            True -> {
              let _ = atomics_put(ref, stamp_index + 1, 0)
              let _ = atomics_put(ref, stamp_index + 2, 0)
              let _ = atomics_put(ref, stamp_index, current)
              Nil
            }
          }
      }
  }
}

fn swapped(ref: AtomicsRef, index: Int, expected: Int, desired: Int) -> Bool {
  atomics_compare_exchange(ref, index, expected, desired)
  == atom.to_dynamic(atom.create("ok"))
}

fn node_totals(ref: AtomicsRef, now_ms: Int) -> #(Int, Int) {
  let current = slot(now_ms)
  int.range(0, bucket_count, #(0, 0), fn(acc, bucket) {
    let stamp = atomics_get(ref, bucket * 3 + 1)
    case stamp > current - bucket_count && stamp <= current {
      True -> #(
        acc.0 + atomics_get(ref, bucket * 3 + 2),
        acc.1 + atomics_get(ref, bucket * 3 + 3),
      )
      False -> acc
    }
  })
}

pub fn offer(kind: RequestType) -> Decision {
  let ref = cells(kind)
  let now_ms = monotonic_time(Millisecond)
  let #(requests, accepts) = node_totals(ref, now_ms)
  let p = probability_of(requests, accepts)
  let bucket = current_bucket(ref, now_ms)
  let _ = atomics_add(ref, bucket * 3 + 2, 1)
  case uniform() <. p {
    True -> Rejected
    False -> Admitted
  }
}

pub fn record_accept(kind: RequestType) -> Nil {
  let ref = cells(kind)
  let bucket = current_bucket(ref, monotonic_time(Millisecond))
  let _ = atomics_add(ref, bucket * 3 + 3, 1)
  Nil
}

pub fn kinds() -> List(RequestType) {
  [UserGuildSettings, BlockedIds, AdultContent, ReadState]
}

pub fn kind_label(kind: RequestType) -> String {
  case kind {
    UserGuildSettings -> "user_guild_settings"
    BlockedIds -> "blocked_ids"
    AdultContent -> "adult_content"
    ReadState -> "read_state"
  }
}

pub fn kind_of(label: String) -> Result(RequestType, Nil) {
  list.find(kinds(), fn(kind) { kind_label(kind) == label })
}

pub fn snapshot() -> List(#(RequestType, Int, Int)) {
  let now_ms = monotonic_time(Millisecond)
  list.map(kinds(), fn(kind) {
    let #(requests, accepts) = node_totals(cells(kind), now_ms)
    #(kind, requests, accepts)
  })
}

pub fn absorb(kind: RequestType, requests: Int, accepts: Int) -> Nil {
  let ref = cells(kind)
  let now_ms = monotonic_time(Millisecond)
  let #(have_requests, have_accepts) = node_totals(ref, now_ms)
  let bucket = current_bucket(ref, now_ms)
  let _ = atomics_add(ref, bucket * 3 + 2, int.max(0, requests - have_requests))
  let _ = atomics_add(ref, bucket * 3 + 3, int.max(0, accepts - have_accepts))
  Nil
}
