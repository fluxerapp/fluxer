// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type BucketId}
import bouncer_proto/lease as proto_lease
import fluxer_gateway/cluster/buckets.{type Placement}
import fluxer_gateway/cluster/lease
import fluxer_gateway/cluster/links
import fluxer_gateway/cluster/router
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/node.{type Node}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, Some}

pub type Verdict {
  Owned
  Pending
  Unowned(owner: Node)
}

pub type Sweep {
  Sweep(first_seen: Dict(Int, Int), due: List(Int), unowned: Int)
}

pub fn decide(
  held: Bool,
  placement: Placement,
  live: fn(Node) -> Bool,
) -> Verdict {
  case held, placement {
    True, _ -> Owned
    False, buckets.There(node: owner, ..) ->
      case live(owner) {
        True -> Unowned(owner)
        False -> Pending
      }
    False, buckets.Here(_) | False, buckets.Unplaced(_) -> Pending
  }
}

pub fn of(guild: GuildId, arrival: Bool) -> Verdict {
  case router.bouncer_mode() {
    False -> Owned
    True -> {
      let entity = router.entity_key(router.GuildKey(guild))
      let bucket = buckets.bucket_for(entity)
      case arrival && moving_here(bucket) {
        True -> Pending
        False ->
          decide(
            lease.held_epoch(bucket) |> is_ok,
            buckets.placement(entity),
            links.is_live,
          )
      }
    }
  }
}

pub fn served_epoch(guild: GuildId) -> Result(Epoch, Nil) {
  router.GuildKey(guild)
  |> router.entity_key
  |> buckets.bucket_for
  |> lease.held_epoch
}

pub fn holds(guild: GuildId) -> Bool {
  served_epoch(guild) |> is_ok
}

pub fn lapsed(served: Option(Epoch), now: Result(Epoch, Nil)) -> Bool {
  case served, now {
    Some(before), Ok(current) -> before != current
    _, _ -> False
  }
}

fn moving_here(bucket: BucketId) -> Bool {
  case buckets.record(bucket) {
    Ok(proto_lease.BucketRecord(state: proto_lease.Moving(to:, ..), ..)) ->
      node.from_atom(to.node) == node.self()
    _ -> False
  }
}

fn is_ok(found: Result(a, b)) -> Bool {
  case found {
    Ok(_) -> True
    Error(_) -> False
  }
}

pub fn sweep(
  first_seen: Dict(Int, Int),
  unowned: List(Int),
  now: Int,
  grace_ms: Int,
  force: Bool,
) -> Sweep {
  let kept =
    list.fold(unowned, dict.new(), fn(acc, key) {
      dict.insert(acc, key, case dict.get(first_seen, key) {
        Ok(at) -> at
        Error(Nil) -> now
      })
    })
  let due =
    dict.fold(kept, [], fn(acc, key, at) {
      case force || now - at >= grace_ms {
        True -> [key, ..acc]
        False -> acc
      }
    })
    |> list.sort(int.compare)
  Sweep(
    first_seen: list.fold(due, kept, dict.delete),
    due:,
    unowned: list.length(unowned),
  )
}
