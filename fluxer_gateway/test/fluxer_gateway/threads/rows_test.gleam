// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/json_tree
import fluxer_gateway/threads/rows
import fluxer_gateway/threads/thread
import fluxer_gateway/threads/value.{type Object}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should

fn thread_object(thread_id: Int, parent: Int) -> Object {
  [
    #("id", json_tree.String(int.to_string(thread_id))),
    #("parent_id", json_tree.String(int.to_string(parent))),
    #("type", json_tree.Int(11)),
    #("name", json_tree.String("t")),
  ]
}

fn member_object(thread_id: Int, user: Int) -> Object {
  [
    #("id", json_tree.String(int.to_string(thread_id))),
    #("user_id", json_tree.String(int.to_string(user))),
    #("flags", json_tree.Int(0)),
  ]
}

fn make_thread(thread_id: Int, parent: Int) -> thread.Thread {
  let assert Ok(entry) = thread.from_event(thread_object(thread_id, parent))
  entry
}

fn make_member(thread_id: Int, user: Int) -> thread.ThreadMember {
  let assert Ok(entry) =
    thread.member_from_object(member_object(thread_id, user))
  entry
}

pub fn load_skips_members_of_unknown_threads_test() {
  let table =
    rows.load(rows.empty(), [make_thread(10, 1)], [
      make_member(10, 100),
      make_member(11, 100),
    ])
  rows.member_ids(table, 10) |> should.equal([100])
  rows.member_ids(table, 11) |> should.equal([])
  rows.user_thread_ids(table, 100) |> should.equal([10])
}

pub fn remove_thread_clears_indexes_test() {
  let table =
    rows.load(rows.empty(), [make_thread(10, 1), make_thread(12, 1)], [
      make_member(10, 100),
    ])
  rows.parent_thread_ids(table, 1) |> should.equal([10, 12])
  let table = rows.remove_thread(table, 10)
  rows.parent_thread_ids(table, 1) |> should.equal([12])
  rows.user_thread_ids(table, 100) |> should.equal([])
  rows.get_thread(table, 10) |> should.equal(None)
}

pub fn moving_a_thread_moves_its_parent_link_test() {
  let table = rows.put_thread(rows.empty(), make_thread(10, 1))
  let table = rows.put_thread(table, make_thread(10, 2))
  rows.parent_thread_ids(table, 1) |> should.equal([])
  rows.parent_thread_ids(table, 2) |> should.equal([10])
}

pub fn cap_drops_lowest_ids_first_test() {
  let threads =
    int.range(1, rows.max_threads + 3, [], list.prepend)
    |> list.reverse
    |> list.map(fn(n) { make_thread(n, 1) })
  let table = rows.load(rows.empty(), threads, [])
  rows.thread_count(table) |> should.equal(rows.max_threads)
  rows.has_thread(table, 1) |> should.be_false
  rows.has_thread(table, 2) |> should.be_false
  rows.has_thread(table, 3) |> should.be_true
}

pub fn remove_user_returns_threads_test() {
  let table =
    rows.load(rows.empty(), [make_thread(10, 1), make_thread(11, 1)], [
      make_member(10, 7),
      make_member(11, 7),
      make_member(11, 8),
    ])
  let #(table, removed) = rows.remove_user(table, 7)
  removed |> should.equal([10, 11])
  rows.member_ids(table, 11) |> should.equal([8])
}

pub fn archived_flag_is_read_test() {
  let object =
    thread_object(10, 1)
    |> value.insert(
      "thread_metadata",
      json_tree.Object([#("archived", json_tree.Bool(True))]),
    )
  case thread.from_event(object) {
    Ok(entry) -> thread.is_archived(entry) |> should.be_true
    Error(Nil) -> should.fail()
  }
  thread.name(make_thread(10, 1)) |> should.equal(Some("t"))
}
