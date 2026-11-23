// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/dispatch_facts.{type DispatchFacts}
import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/list
import gleam/option.{type Option}
import gleam/set.{type Set}

const max_held = 2048

pub type Held {
  Held(
    user: Option(Int),
    guild: Option(Int),
    payload: RawJson,
    facts: DispatchFacts,
  )
}

pub opaque type PendingPresences {
  PendingPresences(newest_first: List(Held), count: Int)
}

pub fn new() -> PendingPresences {
  PendingPresences(newest_first: [], count: 0)
}

pub fn size(pending: PendingPresences) -> Int {
  pending.count
}

pub fn hold(pending: PendingPresences, held: Held) -> PendingPresences {
  let kept = case pending.count >= max_held {
    True -> list.take(pending.newest_first, max_held - 1)
    False -> pending.newest_first
  }
  let newest_first = [held, ..kept]
  PendingPresences(newest_first:, count: list.length(newest_first))
}

pub fn release(
  pending: PendingPresences,
  when ready: fn(Held) -> Bool,
) -> #(PendingPresences, List(Held)) {
  let #(released, kept) =
    pending.newest_first |> list.reverse |> list.partition(ready)
  #(
    PendingPresences(newest_first: list.reverse(kept), count: list.length(kept)),
    released,
  )
}

pub fn release_user(
  pending: PendingPresences,
  user: Int,
) -> #(PendingPresences, List(Held)) {
  release(pending, fn(held) { held.user == option.Some(user) })
}

pub fn drop_users(
  pending: PendingPresences,
  users: Set(Int),
) -> PendingPresences {
  case set.is_empty(users) {
    True -> pending
    False -> {
      let kept =
        list.filter(pending.newest_first, fn(held) {
          case held.user {
            option.Some(user) -> !set.contains(users, user)
            option.None -> True
          }
        })
      PendingPresences(newest_first: kept, count: list.length(kept))
    }
  }
}
