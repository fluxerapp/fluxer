// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/threads/rows
import fluxer_gateway/threads/state
import fluxer_gateway/threads/store
import fluxer_gateway/threads/thread
import fluxer_gateway/threads/value.{type Object}
import gleam/int
import gleam/option.{None, Some}
import gleeunit/should

fn object(pairs: List(#(String, JsonTree))) -> Object {
  pairs
}

fn sid(n: Int) -> JsonTree {
  json_tree.String(int.to_string(n))
}

fn create(thread_id: Int, parent: Int) -> Object {
  object([
    #("id", sid(thread_id)),
    #("parent_id", sid(parent)),
    #("type", json_tree.Int(11)),
    #(
      "_fluxer_members",
      json_tree.Array([
        json_tree.Object(
          object([#("id", sid(thread_id)), #("user_id", sid(5))]),
        ),
      ]),
    ),
  ])
}

fn channel(n: Int) -> id.ChannelId {
  let assert Ok(found) = id.channel_id(n)
  found
}

pub fn create_adds_thread_and_members_test() {
  let threads =
    store.apply(store.new(), "THREAD_CREATE", create(10, 1), store.Live)
  case store.thread(threads, channel(10)) {
    Some(found) -> thread.id_int(found) |> should.equal(10)
    None -> should.fail()
  }
  rows.member_ids(store.rows(threads), 10) |> should.equal([5])
  let #(_, effects) = store.take_effects(threads)
  effects
  |> should.equal([
    state.ForumLastMessage(parent: channel(1), thread: channel(10)),
  ])
}

pub fn archived_update_removes_thread_test() {
  let threads =
    store.apply(store.new(), "THREAD_CREATE", create(10, 1), store.Live)
  let archived =
    create(10, 1)
    |> value.insert(
      "thread_metadata",
      json_tree.Object(object([#("archived", json_tree.Bool(True))])),
    )
  let threads = store.apply(threads, "THREAD_UPDATE", archived, store.Live)
  store.thread(threads, channel(10)) |> should.equal(None)
}

pub fn message_create_counts_messages_test() {
  let threads =
    store.apply(store.new(), "THREAD_CREATE", create(10, 1), store.Live)
  let message =
    object([
      #("id", sid(20)),
      #("channel_id", sid(10)),
      #("type", json_tree.Int(0)),
    ])
  let threads = store.apply(threads, "MESSAGE_CREATE", message, store.Live)
  let starter =
    object([
      #("id", sid(10)),
      #("channel_id", sid(10)),
      #("type", json_tree.Int(0)),
    ])
  let threads = store.apply(threads, "MESSAGE_CREATE", starter, store.Live)
  case store.thread(threads, channel(10)) {
    Some(found) -> {
      thread.counter(found, "message_count") |> should.equal(1)
      thread.counter(found, "total_message_sent") |> should.equal(1)
      thread.last_message_id(found) |> should.equal(Some(20))
    }
    None -> should.fail()
  }
  let delete = object([#("id", sid(20)), #("channel_id", sid(10))])
  let threads = store.apply(threads, "MESSAGE_DELETE", delete, store.Live)
  case store.thread(threads, channel(10)) {
    Some(found) -> thread.counter(found, "message_count") |> should.equal(0)
    None -> should.fail()
  }
}

pub fn channel_delete_removes_child_threads_test() {
  let threads =
    store.apply(store.new(), "THREAD_CREATE", create(10, 1), store.Live)
  let threads =
    store.apply(
      threads,
      "CHANNEL_DELETE",
      object([#("id", sid(1))]),
      store.Live,
    )
  store.thread(threads, channel(10)) |> should.equal(None)
}

pub fn member_remove_drops_memberships_test() {
  let threads =
    store.apply(store.new(), "THREAD_CREATE", create(10, 1), store.Live)
  let removal = object([#("user", json_tree.Object(object([#("id", sid(5))])))])
  let threads = store.apply(threads, "GUILD_MEMBER_REMOVE", removal, store.Live)
  rows.member_ids(store.rows(threads), 10) |> should.equal([])
}

pub fn members_update_adds_and_removes_test() {
  let threads =
    store.apply(store.new(), "THREAD_CREATE", create(10, 1), store.Live)
  let update =
    object([
      #("id", sid(10)),
      #(
        "added_members",
        json_tree.Array([
          json_tree.Object(object([#("id", sid(10)), #("user_id", sid(6))])),
        ]),
      ),
      #("removed_member_ids", json_tree.Array([sid(5)])),
      #("member_count", json_tree.Int(1)),
    ])
  let threads =
    store.apply(threads, "THREAD_MEMBERS_UPDATE", update, store.Live)
  rows.member_ids(store.rows(threads), 10) |> should.equal([6])
  case store.thread(threads, channel(10)) {
    Some(found) -> thread.counter(found, "member_count") |> should.equal(1)
    None -> should.fail()
  }
}
