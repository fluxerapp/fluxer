// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}

pub opaque type Epoch {
  Epoch(node: Atom, minted_ns: Int, unique: Int)
}

pub opaque type Version {
  Version(epoch: Epoch, seq: Int)
}

pub type Freshness {
  Newer
  Stale
  OtherEpoch
}

type UniqueOption {
  Positive
  Monotonic
}

type TimeUnit {
  Nanosecond
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(UniqueOption)) -> Int

@external(erlang, "erlang", "system_time")
fn system_time(unit: TimeUnit) -> Int

@external(erlang, "erlang", "node")
fn self_node() -> Atom

pub fn new_epoch() -> Epoch {
  Epoch(
    node: self_node(),
    minted_ns: system_time(Nanosecond),
    unique: unique_integer([Positive, Monotonic]),
  )
}

pub fn stamp() -> Int {
  unique_integer([Positive, Monotonic])
}

pub fn first(epoch: Epoch) -> Version {
  Version(epoch:, seq: 1)
}

pub fn next(version: Version) -> Version {
  Version(..version, seq: version.seq + 1)
}

pub fn epoch(version: Version) -> Epoch {
  version.epoch
}

pub fn seq(version: Version) -> Int {
  version.seq
}

pub fn compare(incoming: Version, seen: Version) -> Freshness {
  case incoming.epoch == seen.epoch, incoming.seq > seen.seq {
    False, _ -> OtherEpoch
    True, True -> Newer
    True, False -> Stale
  }
}

pub opaque type Seen {
  Seen(versions: Dict(Int, Version))
}

pub fn empty_seen() -> Seen {
  Seen(dict.new())
}

pub fn observe(seen: Seen, user: Int, incoming: Version) -> #(Seen, Bool) {
  case dict.get(seen.versions, user) {
    Error(Nil) -> #(Seen(dict.insert(seen.versions, user, incoming)), True)
    Ok(last) ->
      case compare(incoming, last) {
        Newer | OtherEpoch -> #(
          Seen(dict.insert(seen.versions, user, incoming)),
          True,
        )
        Stale -> #(seen, False)
      }
  }
}

pub fn superseded(seen: Seen, user: Int, incoming: Version) -> Bool {
  case dict.get(seen.versions, user) {
    Ok(last) -> incoming.epoch == last.epoch && incoming.seq < last.seq
    Error(Nil) -> False
  }
}

pub fn forget(seen: Seen, users: List(Int)) -> Seen {
  Seen(dict.drop(seen.versions, users))
}

pub fn seen_size(seen: Seen) -> Int {
  dict.size(seen.versions)
}

pub fn epoch_parts(epoch: Epoch) -> #(Atom, Int, Int) {
  #(epoch.node, epoch.minted_ns, epoch.unique)
}

pub fn restore_epoch(node: Atom, minted_ns: Int, unique: Int) -> Epoch {
  Epoch(node:, minted_ns:, unique:)
}

pub fn restore(epoch: Epoch, seq: Int) -> Version {
  Version(epoch:, seq:)
}
