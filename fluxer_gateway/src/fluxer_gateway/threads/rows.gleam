// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/threads/thread.{type Thread, type ThreadMember}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}

pub const max_threads = 5000

pub opaque type Rows {
  Rows(
    threads: Dict(Int, Thread),
    members: Dict(Int, Dict(Int, ThreadMember)),
    users: Dict(Int, Set(Int)),
    parents: Dict(Int, Set(Int)),
  )
}

pub fn empty() -> Rows {
  Rows(
    threads: dict.new(),
    members: dict.new(),
    users: dict.new(),
    parents: dict.new(),
  )
}

pub fn load(
  rows: Rows,
  threads: List(Thread),
  members: List(ThreadMember),
) -> Rows {
  let rows =
    list.fold(threads, rows, fn(acc, entry) { insert_thread(acc, entry).0 })
  let rows = trim(rows, thread_count(rows) - max_threads)
  list.fold(members, rows, fn(acc, member) {
    case has_thread(acc, id.channel_id_to_int(thread.member_thread(member))) {
      True -> put_member(acc, member)
      False -> acc
    }
  })
}

pub fn put_thread(rows: Rows, entry: Thread) -> Rows {
  case insert_thread(rows, entry) {
    #(rows, True) -> trim(rows, thread_count(rows) - max_threads)
    #(rows, False) -> rows
  }
}

fn insert_thread(rows: Rows, entry: Thread) -> #(Rows, Bool) {
  let key = thread.id_int(entry)
  let previous = dict.get(rows.threads, key)
  let parents = case previous {
    Ok(old) -> unlink_parent(rows.parents, thread.parent(old), key)
    Error(Nil) -> rows.parents
  }
  let parents = case thread.parent(entry) {
    Some(parent) -> add_index(parents, id.channel_id_to_int(parent), key)
    None -> parents
  }
  #(
    Rows(..rows, threads: dict.insert(rows.threads, key, entry), parents:),
    previous == Error(Nil),
  )
}

fn trim(rows: Rows, excess: Int) -> Rows {
  case excess <= 0 {
    True -> rows
    False ->
      thread_ids(rows)
      |> list.take(excess)
      |> list.fold(rows, remove_thread)
  }
}

pub fn has_thread(rows: Rows, thread_id: Int) -> Bool {
  dict.has_key(rows.threads, thread_id)
}

pub fn get_thread(rows: Rows, thread_id: Int) -> Option(Thread) {
  dict.get(rows.threads, thread_id) |> option.from_result
}

pub fn remove_thread(rows: Rows, thread_id: Int) -> Rows {
  case dict.get(rows.threads, thread_id) {
    Error(Nil) -> rows
    Ok(stored) -> {
      let rows =
        list.fold(member_ids(rows, thread_id), rows, fn(acc, user) {
          remove_member(acc, thread_id, user)
        })
      Rows(
        ..rows,
        threads: dict.delete(rows.threads, thread_id),
        members: dict.delete(rows.members, thread_id),
        parents: unlink_parent(rows.parents, thread.parent(stored), thread_id),
      )
    }
  }
}

pub fn put_member(rows: Rows, member: ThreadMember) -> Rows {
  let thread_id = id.channel_id_to_int(thread.member_thread(member))
  let user = id.user_id_to_int(thread.member_user(member))
  let thread_members =
    dict.get(rows.members, thread_id)
    |> option.from_result
    |> option.unwrap(dict.new())
    |> dict.insert(user, member)
  Rows(
    ..rows,
    members: dict.insert(rows.members, thread_id, thread_members),
    users: add_index(rows.users, user, thread_id),
  )
}

pub fn get_member(
  rows: Rows,
  thread_id: Int,
  user: Int,
) -> Option(ThreadMember) {
  case dict.get(rows.members, thread_id) {
    Ok(thread_members) -> dict.get(thread_members, user) |> option.from_result
    Error(Nil) -> None
  }
}

pub fn remove_member(rows: Rows, thread_id: Int, user: Int) -> Rows {
  let members = case dict.get(rows.members, thread_id) {
    Ok(thread_members) ->
      dict.insert(rows.members, thread_id, dict.delete(thread_members, user))
    Error(Nil) -> rows.members
  }
  Rows(..rows, members:, users: remove_index(rows.users, user, thread_id))
}

pub fn is_member(rows: Rows, thread_id: Int, user: Int) -> Bool {
  case dict.get(rows.members, thread_id) {
    Ok(thread_members) -> dict.has_key(thread_members, user)
    Error(Nil) -> False
  }
}

pub fn member_ids(rows: Rows, thread_id: Int) -> List(Int) {
  case dict.get(rows.members, thread_id) {
    Ok(thread_members) -> dict.keys(thread_members) |> list.sort(int.compare)
    Error(Nil) -> []
  }
}

pub fn members(rows: Rows, thread_id: Int) -> List(#(Int, ThreadMember)) {
  case dict.get(rows.members, thread_id) {
    Ok(thread_members) ->
      dict.to_list(thread_members)
      |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
    Error(Nil) -> []
  }
}

pub fn user_thread_ids(rows: Rows, user: Int) -> List(Int) {
  sorted_index(rows.users, user)
}

pub fn parent_thread_ids(rows: Rows, parent: Int) -> List(Int) {
  sorted_index(rows.parents, parent)
}

pub fn thread_ids(rows: Rows) -> List(Int) {
  dict.keys(rows.threads) |> list.sort(int.compare)
}

pub fn threads(rows: Rows) -> List(Thread) {
  list.filter_map(thread_ids(rows), fn(key) { dict.get(rows.threads, key) })
}

pub fn thread_count(rows: Rows) -> Int {
  dict.size(rows.threads)
}

pub fn row_count(rows: Rows) -> Int {
  let member_rows =
    dict.fold(rows.members, 0, fn(acc, _, thread_members) {
      acc + dict.size(thread_members)
    })
  let parent_rows =
    dict.fold(rows.parents, 0, fn(acc, _, ids) { acc + set.size(ids) })
  thread_count(rows) + 2 * member_rows + parent_rows
}

pub fn remove_user(rows: Rows, user: Int) -> #(Rows, List(Int)) {
  let thread_ids = user_thread_ids(rows, user)
  #(
    list.fold(thread_ids, rows, fn(acc, thread_id) {
      remove_member(acc, thread_id, user)
    }),
    thread_ids,
  )
}

pub fn update_thread(
  rows: Rows,
  thread_id: Int,
  change: fn(Thread) -> Thread,
) -> Rows {
  case dict.get(rows.threads, thread_id) {
    Ok(stored) -> put_thread(rows, change(stored))
    Error(Nil) -> rows
  }
}

fn sorted_index(index: Dict(Int, Set(Int)), key: Int) -> List(Int) {
  case dict.get(index, key) {
    Ok(ids) -> set.to_list(ids) |> list.sort(int.compare)
    Error(Nil) -> []
  }
}

fn add_index(
  index: Dict(Int, Set(Int)),
  key: Int,
  entry: Int,
) -> Dict(Int, Set(Int)) {
  dict.upsert(index, key, fn(existing) {
    case existing {
      Some(ids) -> set.insert(ids, entry)
      None -> set.from_list([entry])
    }
  })
}

fn remove_index(
  index: Dict(Int, Set(Int)),
  key: Int,
  entry: Int,
) -> Dict(Int, Set(Int)) {
  case dict.get(index, key) {
    Ok(ids) -> {
      let ids = set.delete(ids, entry)
      case set.is_empty(ids) {
        True -> dict.delete(index, key)
        False -> dict.insert(index, key, ids)
      }
    }
    Error(Nil) -> index
  }
}

fn unlink_parent(
  parents: Dict(Int, Set(Int)),
  parent: Option(id.ChannelId),
  thread_id: Int,
) -> Dict(Int, Set(Int)) {
  case parent {
    Some(parent) ->
      remove_index(parents, id.channel_id_to_int(parent), thread_id)
    None -> parents
  }
}
