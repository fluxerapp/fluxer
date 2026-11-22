// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type RoleId, type UserId}
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/guild/table.{type Table}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

pub opaque type MembersTable {
  MembersTable(table: Table(Int, Member), journal: Option(Table(Int, Nil)))
}

pub type Changes {
  Changes(upserted: List(Member), removed: List(Int))
}

pub fn new(heir: Option(Pid)) -> MembersTable {
  let heir_option = case heir {
    Some(pid) -> [table.Heir(pid, Nil)]
    None -> []
  }
  MembersTable(
    table: table.new("guild_members", [
      table.Set,
      table.Public,
      table.ReadConcurrency(True),
      ..heir_option
    ]),
    journal: None,
  )
}

pub fn track(members: MembersTable) -> MembersTable {
  case members.journal {
    Some(_) -> members
    None ->
      MembersTable(
        ..members,
        journal: Some(table.new("guild_members_journal", [table.Set])),
      )
  }
}

pub fn untrack(members: MembersTable) -> MembersTable {
  case members.journal {
    Some(journal) -> {
      table.drop(journal)
      MembersTable(..members, journal: None)
    }
    None -> members
  }
}

pub fn take_changes(members: MembersTable) -> Changes {
  case members.journal {
    None -> Changes(upserted: [], removed: [])
    Some(journal) -> {
      let touched = table.to_list(journal)
      table.clear(journal)
      list.fold(touched, Changes(upserted: [], removed: []), fn(acc, row) {
        case table.lookup(members.table, row.0) {
          Ok(found) -> Changes(..acc, upserted: [found, ..acc.upserted])
          Error(Nil) -> Changes(..acc, removed: [row.0, ..acc.removed])
        }
      })
    }
  }
}

fn touch(members: MembersTable, user: Int) -> Nil {
  case members.journal {
    Some(journal) -> table.insert(journal, user, Nil)
    None -> Nil
  }
}

pub fn is_alive(members: MembersTable) -> Bool {
  table.exists(members.table)
}

pub fn get(members: MembersTable, user: UserId) -> Option(Member) {
  table.lookup(members.table, id.user_id_to_int(user)) |> option.from_result
}

pub fn has(members: MembersTable, user: UserId) -> Bool {
  table.lookup(members.table, id.user_id_to_int(user)) |> result.is_ok
}

pub fn put(members: MembersTable, row: Member) -> Nil {
  let user = id.user_id_to_int(member.user_id(row))
  table.insert(members.table, user, row)
  touch(members, user)
}

pub fn put_many(members: MembersTable, rows: List(Member)) -> Nil {
  rows
  |> list.sized_chunk(1000)
  |> list.each(fn(chunk) {
    let rows =
      list.map(chunk, fn(row) { #(id.user_id_to_int(member.user_id(row)), row) })
    table.insert_many(members.table, rows)
    list.each(rows, fn(row) { touch(members, row.0) })
  })
}

pub fn remove(members: MembersTable, user: UserId) -> Nil {
  let key = id.user_id_to_int(user)
  table.delete(members.table, key)
  touch(members, key)
}

pub fn count(members: MembersTable) -> Int {
  table.size(members.table)
}

pub fn fold(
  members: MembersTable,
  from initial: acc,
  with step: fn(acc, Member) -> acc,
) -> acc {
  table.fold(members.table, initial, fn(acc, _key, member) { step(acc, member) })
}

pub fn fold_until(
  members: MembersTable,
  from initial: acc,
  with step: fn(acc, Member) -> list.ContinueOrStop(acc),
) -> acc {
  table.fold_until(members.table, initial, fn(acc, _key, member) {
    step(acc, member)
  })
}

pub fn to_list(members: MembersTable) -> List(Member) {
  members.table
  |> table.to_list
  |> list.map(fn(row) { row.1 })
}

pub fn sorted_user_ints(members: MembersTable) -> List(Int) {
  members.table
  |> table.to_list
  |> list.map(fn(row) { row.0 })
  |> list.sort(int.compare)
}

pub fn with_role(members: MembersTable, role: RoleId) -> List(Int) {
  members
  |> fold([], fn(acc, row) {
    case list.contains(member.roles(row), role) {
      True -> [id.user_id_to_int(member.user_id(row)), ..acc]
      False -> acc
    }
  })
  |> list.sort(int.compare)
}

pub fn with_any_role(members: MembersTable, roles: List(RoleId)) -> List(Int) {
  case roles {
    [] -> []
    _ ->
      members
      |> fold([], fn(acc, row) {
        case
          list.any(member.roles(row), fn(role) { list.contains(roles, role) })
        {
          True -> [id.user_id_to_int(member.user_id(row)), ..acc]
          False -> acc
        }
      })
      |> list.sort(int.compare)
  }
}

pub fn replace_all(members: MembersTable, rows: List(Member)) -> Nil {
  let keep =
    list.fold(rows, set.new(), fn(acc, row) {
      set.insert(acc, id.user_id_to_int(member.user_id(row)))
    })
  members.table
  |> table.to_list
  |> list.each(fn(row) {
    case set.contains(keep, row.0) {
      True -> Nil
      False -> {
        table.delete(members.table, row.0)
        touch(members, row.0)
      }
    }
  })
  put_many(members, rows)
}

pub fn drop(members: MembersTable) -> Nil {
  let _ = untrack(members)
  table.drop(members.table)
}
