// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/key.{type BucketId}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/string

const hash_range = 4_294_967_295

pub type Homes {
  Homes(home: Int, successor: Option(Int))
}

@external(erlang, "erlang", "phash2")
fn phash2(term: a, range: Int) -> Int

fn weight(key: Int, candidate: Atom) -> Int {
  phash2(#(key, candidate), hash_range)
}

pub fn select(key: Int, candidates: List(Atom)) -> Result(Atom, Nil) {
  case candidates {
    [] -> Error(Nil)
    [only] -> Ok(only)
    [first, ..rest] -> {
      let #(best, _) =
        list.fold(rest, #(first, weight(key, first)), fn(best, candidate) {
          let candidate_weight = weight(key, candidate)
          case beats(candidate, candidate_weight, best.0, best.1) {
            True -> #(candidate, candidate_weight)
            False -> best
          }
        })
      Ok(best)
    }
  }
}

fn beats(a: Atom, a_weight: Int, b: Atom, b_weight: Int) -> Bool {
  a_weight > b_weight
  || {
    a_weight == b_weight
    && string.compare(atom.to_string(a), atom.to_string(b)) == order.Lt
  }
}

fn ordinal_weight(bucket_id: BucketId, ordinal: Int) -> Int {
  phash2(#(bucket_id.ns, bucket_id.bucket, ordinal), hash_range)
}

pub fn rank(bucket_id: BucketId, ordinals: List(Int)) -> List(Int) {
  ordinals
  |> list.unique
  |> list.map(fn(ordinal) { #(ordinal_weight(bucket_id, ordinal), ordinal) })
  |> list.sort(fn(a, b) {
    case int.compare(b.0, a.0) {
      order.Eq -> int.compare(a.1, b.1)
      other -> other
    }
  })
  |> list.map(fn(pair) { pair.1 })
}

pub fn homes(bucket_id: BucketId, live: List(Int)) -> Result(Homes, Nil) {
  case rank(bucket_id, live) {
    [] -> Error(Nil)
    [home] -> Ok(Homes(home: home, successor: None))
    [home, successor, ..] -> Ok(Homes(home: home, successor: Some(successor)))
  }
}
