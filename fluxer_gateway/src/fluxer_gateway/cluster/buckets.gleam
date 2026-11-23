// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type BucketId, type Key}
import bouncer_proto/lease.{type BucketRecord, Free, Held, Moving}
import bouncer_proto/worker.{type WorkerId}
import fluxer_gateway/config/durations
import fluxer_gateway/config/settings
import fluxer_gateway/core/clock
import fluxer_gateway/core/id
import fluxer_gateway/core/node.{type Node}
import fluxer_gateway/util/table
import gleam/list
import gleam/option.{None, Some}
import gleam/set

pub type Placement {
  Here(epoch: Epoch)
  There(node: Node, epoch: Epoch)
  Unplaced(retry_after_ms: Int)
}

pub type BucketsError {
  TableMissing
  UnknownBucket(bucket: BucketId)
}

type RouteRow =
  #(Key, Node, Epoch, Int)

fn records_table() -> table.Table {
  table.named("fluxer_gateway_buckets")
}

fn routes_table() -> table.Table {
  table.named("fluxer_gateway_bucket_routes")
}

fn watch_table() -> table.Table {
  table.named("fluxer_gateway_bucket_watch")
}

pub fn start() -> Result(Nil, BucketsError) {
  case
    table.ensure_public(records_table())
    && table.ensure_public(routes_table())
    && table.ensure_public(watch_table())
  {
    True -> Ok(Nil)
    False -> Error(TableMissing)
  }
}

pub fn bucket_for(entity: Key) -> BucketId {
  case entity.ns == key.guild {
    False -> key.bucket_of(entity)
    True ->
      case pinned_index(settings.get().pinned_guild_ids, entity.id, 0) {
        Ok(index) ->
          case key.pinned_bucket(key.first_pinned, index) {
            Ok(pinned) -> pinned
            Error(Nil) -> key.bucket_of(entity)
          }
        Error(Nil) -> key.bucket_of(entity)
      }
  }
}

fn pinned_index(
  pins: List(id.GuildId),
  wanted: Int,
  index: Int,
) -> Result(Int, Nil) {
  case pins {
    [] -> Error(Nil)
    [first, ..rest] ->
      case id.guild_id_to_int(first) == wanted {
        True -> Ok(index)
        False -> pinned_index(rest, wanted, index + 1)
      }
  }
}

pub fn apply(records: List(BucketRecord)) -> Result(Nil, BucketsError) {
  case start() {
    Error(error) -> Error(error)
    Ok(Nil) -> {
      list.each(records, fn(incoming) {
        case record(incoming.bucket) {
          Ok(current) ->
            case lease.newer(incoming, current) {
              True ->
                table.insert(records_table(), #(incoming.bucket, incoming))
              False -> Nil
            }
          Error(_) ->
            table.insert(records_table(), #(incoming.bucket, incoming))
        }
      })
      Ok(Nil)
    }
  }
}

pub fn record(bucket: BucketId) -> Result(BucketRecord, BucketsError) {
  let rows: List(#(BucketId, BucketRecord)) =
    table.lookup(records_table(), bucket)
  case rows {
    [#(_, found), ..] -> Ok(found)
    [] -> Error(UnknownBucket(bucket))
  }
}

pub fn records() -> List(BucketRecord) {
  let rows: List(#(BucketId, BucketRecord)) = table.to_list(records_table())
  list.map(rows, fn(row) { row.1 })
}

pub fn clear() -> Nil {
  let rows: List(#(BucketId, BucketRecord)) = table.to_list(records_table())
  list.each(rows, fn(row) { table.delete(records_table(), row.0) })
  let routes: List(RouteRow) = table.to_list(routes_table())
  list.each(routes, fn(row) { table.delete(routes_table(), row.0) })
}

pub fn put_route(
  key: Key,
  target: Node,
  epoch: Epoch,
  expires_at_ms: Int,
) -> Result(Nil, BucketsError) {
  case start() {
    Error(error) -> Error(error)
    Ok(Nil) ->
      Ok(table.insert(routes_table(), #(key, target, epoch, expires_at_ms)))
  }
}

pub fn expire_routes(now_mono: Int) -> Int {
  let rows: List(RouteRow) = table.to_list(routes_table())
  list.fold(rows, 0, fn(count, row) {
    case row.3 <= now_mono {
      True -> {
        table.delete_object(routes_table(), row)
        count + 1
      }
      False -> count
    }
  })
}

pub fn watch(bucket: BucketId) -> Result(Nil, BucketsError) {
  case start() {
    Error(error) -> Error(error)
    Ok(Nil) -> Ok(table.insert(watch_table(), #(bucket)))
  }
}

pub fn watched() -> List(BucketId) {
  let rows: List(#(BucketId)) = table.to_list(watch_table())
  list.map(rows, fn(row) { row.0 })
}

pub fn placement(entity: Key) -> Placement {
  case route_row(entity) {
    Ok(#(_, target, route_epoch, _)) -> at(target, route_epoch)
    Error(Nil) ->
      case record(bucket_for(entity)) {
        Ok(found) -> place(found, entity.id)
        Error(_) -> unplaced()
      }
  }
}

fn route_row(entity: Key) -> Result(RouteRow, Nil) {
  let rows: List(RouteRow) = table.lookup(routes_table(), entity)
  case rows {
    [row, ..] ->
      case row.3 > clock.monotonic_ms() {
        True -> Ok(row)
        False -> Error(Nil)
      }
    [] -> Error(Nil)
  }
}

pub fn place(found: BucketRecord, id: Int) -> Placement {
  case found.holder, found.state {
    None, _ | _, Free -> unplaced()
    Some(holder), Held -> worker_at(holder, found.epoch)
    Some(holder), Moving(to:, to_epoch:, pending:, commits:) ->
      case
        list.any(commits, fn(commit) { commit.id == id }),
        set.contains(pending, id)
      {
        True, _ -> worker_at(to, to_epoch)
        False, True -> worker_at(holder, found.epoch)
        False, False -> worker_at(to, to_epoch)
      }
  }
}

fn worker_at(holder: WorkerId, at_epoch: Epoch) -> Placement {
  at(node.from_atom(holder.node), at_epoch)
}

fn at(target: Node, at_epoch: Epoch) -> Placement {
  case target == node.self() {
    True -> Here(at_epoch)
    False -> There(target, at_epoch)
  }
}

fn unplaced() -> Placement {
  Unplaced(durations.bouncer().unplaced_retry_ms)
}
