// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/id
import fluxer_gateway/core/packed_ids.{type PackedIds}
import fluxer_gateway/core/presence.{type Presence}
import fluxer_gateway/presence/counters
import fluxer_gateway/presence/event.{type PresenceEvent, type Row}
import fluxer_gateway/presence/id_set.{type IdSet}
import fluxer_gateway/presence/owner
import fluxer_gateway/presence/remote
import fluxer_gateway/presence/route
import fluxer_gateway/presence/table
import gleam/dict
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/static_supervisor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result

fn remote_read_timeout_ms() -> Int {
  durations.presence().cache_remote_read_timeout_ms
}

fn table_for(user: Int) -> Atom {
  shard_table(route.shard(user))
}

fn shard_table(shard: Int) -> Atom {
  route.table(route.StateTable, shard)
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  static_supervisor.new(static_supervisor.OneForOne)
  |> static_supervisor.add(owner.tables(list.map(route.shards(), shard_table)))
  |> static_supervisor.supervised
  |> supervision.map_data(fn(_) { Nil })
}

pub fn put(user: Int, row: Row) -> Nil {
  table.insert(table_for(user), #(user, row))
}

pub fn delete(user: Int) -> Nil {
  table.delete(table_for(user), user)
}

fn get_local(user: Int) -> Option(Row) {
  case table.lookup(table_for(user), user) {
    [#(_, row), ..] -> Some(row)
    [] -> None
  }
}

pub fn published_ms(user: Int) -> Result(Int, Nil) {
  case get_local(user) {
    Some(row) -> Ok(row.published_ms)
    None -> Error(Nil)
  }
}

pub fn take_local(user: Int) -> Option(Row) {
  let found = get_local(user)
  delete(user)
  found
}

pub fn bulk_get_local(users: BitArray) -> List(#(Int, Row)) {
  case packed_ids.from_bits(users) {
    Ok(ids) ->
      packed_ids.fold(ids, [], fn(acc, user) {
        case get_local(user) {
          Some(row) -> [#(user, row), ..acc]
          None -> acc
        }
      })
      |> list.reverse
    Error(Nil) -> []
  }
}

fn group_by_owner(users: List(Int)) -> #(List(#(Atom, List(Int))), List(Int)) {
  let #(groups, unroutable) =
    list.fold(users, #(dict.new(), []), fn(acc, user) {
      let #(groups, unroutable) = acc
      case route.owner(user) {
        Ok(found) -> #(
          dict.upsert(groups, found, fn(existing) {
            [user, ..option.unwrap(existing, [])]
          }),
          unroutable,
        )
        Error(_) -> #(groups, [user, ..unroutable])
      }
    })
  #(dict.to_list(groups), unroutable)
}

pub fn lookup_events(users: IdSet) -> List(PresenceEvent) {
  read_events(users).0
}

fn read_events(users: IdSet) -> #(List(PresenceEvent), List(Int)) {
  let self = route.self_atom()
  let #(groups, unroutable) =
    users
    |> id_set.to_list
    |> group_by_owner
  let #(local_groups, remote_groups) =
    list.partition(groups, fn(group) { group.0 == self })
  let local =
    local_groups
    |> list.flat_map(fn(group) {
      list.filter_map(group.1, fn(user) {
        get_local(user)
        |> option.map(event.from_row(user, _))
        |> option.to_result(Nil)
      })
    })
  let owned_by = dict.from_list(remote_groups)
  let replies =
    remote_groups
    |> list.map(fn(group) {
      #(
        group.0,
        packed_ids.to_bits(packed_ids.from_list(list.reverse(group.1))),
      )
    })
    |> read_remote
  let remote_events =
    list.flat_map(replies, fn(reply) {
      case reply.1 {
        Ok(rows) ->
          list.map(rows, fn(pair: #(Int, Row)) {
            event.from_row(pair.0, pair.1)
          })
        Error(Nil) -> []
      }
    })
  let failed =
    list.flat_map(replies, fn(reply) {
      case reply.1, dict.get(owned_by, reply.0) {
        Error(Nil), Ok(owned) -> owned
        _, _ -> []
      }
    })
  #(list.append(local, remote_events), list.append(unroutable, failed))
}

fn read_remote(
  requests: List(#(Atom, BitArray)),
) -> List(#(Atom, Result(List(#(Int, Row)), Nil))) {
  let replies =
    remote.multicall_each(
      requests,
      "fluxer_gateway@presence@cache",
      "bulk_get_local",
      remote_read_timeout_ms(),
    )
  list.each(replies, fn(reply) {
    case reply.1 {
      Ok(_) -> Nil
      Error(Nil) -> counters.incr(counters.RemoteReadTimeout)
    }
  })
  replies
}

pub fn absent(users: List(Int)) -> List(Int) {
  let self = route.self_atom()
  let #(local_groups, remote_groups) =
    group_by_owner(users).0
    |> list.partition(fn(group) { group.0 == self })
  let local =
    list.flat_map(local_groups, fn(group) {
      list.filter(group.1, fn(user) { get_local(user) == None })
    })
  let owned_by = dict.from_list(remote_groups)
  let remote_absent =
    remote_groups
    |> list.map(fn(group) {
      #(group.0, packed_ids.to_bits(packed_ids.from_list(group.1)))
    })
    |> read_remote
    |> list.flat_map(fn(reply) {
      case reply.1, dict.get(owned_by, reply.0) {
        Ok(rows), Ok(owned) -> {
          let present =
            list.map(rows, fn(pair: #(Int, Row)) { pair.0 })
            |> id_set.from_list
          list.filter(owned, fn(user) { !id_set.contains(present, user) })
        }
        _, _ -> []
      }
    })
  list.append(local, remote_absent)
}

pub fn lookup_many(users: PackedIds) -> List(Presence) {
  lookup_known(users).0
}

pub fn lookup_known(users: PackedIds) -> #(List(Presence), List(Int)) {
  let #(events, unknown) =
    users
    |> id_set.from_packed
    |> read_events
  let found =
    list.filter_map(events, fn(found) {
      id.user_id(found.user)
      |> result.map(event.to_presence(found, _))
      |> result.replace_error(Nil)
    })
  #(found, unknown)
}
