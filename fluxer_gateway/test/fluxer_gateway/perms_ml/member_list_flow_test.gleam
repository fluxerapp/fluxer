// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/presence
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/string
import support/perms_ml_harness as h

const guild = 100

const text = 11

const hidden = 12

const view_and_send = 3072

const view = 1024

const view_members_bit = 18_014_398_509_481_984

fn everyone() -> String {
  h.role_json(guild, view_and_send, 0)
}

fn basic() -> h.Guild {
  h.start(
    guild,
    1,
    [everyone(), h.hoisted_role_json(500, 0, 5)],
    [
      h.channel_json(text, 0, "[]"),
      h.channel_json(
        hidden,
        0,
        "[" <> h.overwrite_json(guild, 0, 0, view) <> "]",
      ),
    ],
    [
      h.member_json(1, "owner", []),
      h.member_json(2, "bravo", [500]),
      h.member_json(3, "charlie", []),
    ],
  )
}

fn subscribed(g: h.Guild, user: Int) -> #(h.Client, h.Frame) {
  let client = h.join(g, user)
  h.subscribe(g, client, text, [#(0, 99)])
  let assert Ok(first) = list.last(h.list_updates(h.settle(g, client)))
  #(client, first)
}

fn assert_consistent(g: h.Guild, view: h.Frame) -> Nil {
  let fresh = h.fresh_view(g, 1, text, [#(0, 99)])
  assert h.rows(view) == h.rows(fresh)
  assert view.text == fresh.text
}

pub fn subscribe_sends_sync_test() {
  let g = basic()
  let #(_, first) = subscribed(g, 1)
  let assert [op] = h.ops(first)
  assert op.op == "SYNC"
  assert op.range == [0, 99]
  assert h.rows(first) == ["g:online", "1", "g:offline", "2", "3"]
  assert h.list_counts(first) == #(3, 1)
  h.stop(g)
}

pub fn presence_moves_member_into_hoisted_group_test() {
  let g = basic()
  let #(a, first) = subscribed(g, 1)
  let b = h.join(g, 2)
  h.set_presence(g, h.status(2, presence.Idle))
  process.sleep(400)
  let current = h.last_list(h.settle(g, a), first)
  assert h.rows(current) == ["g:500", "2", "g:online", "1", "g:offline", "3"]
  assert h.list_counts(current) == #(3, 2)
  assert_consistent(g, current)
  h.leave(g, b)
  process.sleep(400)
  let current = h.last_list(h.settle(g, a), current)
  assert h.rows(current) == ["g:online", "1", "g:offline", "2", "3"]
  assert_consistent(g, current)
  h.stop(g)
}

pub fn member_add_and_remove_resync_subscribers_test() {
  let g = basic()
  let #(a, first) = subscribed(g, 1)
  h.dispatch(
    g,
    "GUILD_MEMBER_ADD",
    "{\"guild_id\":\"100\","
      <> string.drop_start(h.member_json(4, "alpha", []), 1),
  )
  let current = h.last_list(h.settle(g, a), first)
  assert h.rows(current) == ["g:online", "1", "g:offline", "4", "2", "3"]
  assert h.list_counts(current) == #(4, 1)
  assert_consistent(g, current)
  h.dispatch(
    g,
    "GUILD_MEMBER_REMOVE",
    "{\"guild_id\":\"100\",\"user\":{\"id\":\"2\"}}",
  )
  let current = h.last_list(h.settle(g, a), current)
  assert h.rows(current) == ["g:online", "1", "g:offline", "4", "3"]
  assert h.list_counts(current) == #(3, 1)
  assert_consistent(g, current)
  h.stop(g)
}

pub fn overwrite_hiding_channel_drops_members_from_its_list_test() {
  let g = basic()
  let #(a, first) = subscribed(g, 1)
  h.dispatch(
    g,
    "CHANNEL_UPDATE",
    h.channel_json(text, 0, "[" <> h.overwrite_json(500, 0, 0, view) <> "]"),
  )
  let current = h.last_list(h.settle(g, a), first)
  assert h.rows(current) == ["g:online", "1", "g:offline", "3"]
  assert_consistent(g, current)
  h.stop(g)
}

pub fn losing_view_sends_channel_delete_and_regaining_sends_create_test() {
  let g = basic()
  let c = h.join(g, 3)
  h.dispatch(
    g,
    "CHANNEL_UPDATE",
    h.channel_json(text, 0, "[" <> h.overwrite_json(guild, 0, 0, view) <> "]"),
  )
  let frames = h.settle(g, c)
  assert h.channel_ids(frames, "CHANNEL_DELETE") == [int.to_string(text)]
  h.dispatch(g, "CHANNEL_UPDATE", h.channel_json(text, 0, "[]"))
  let frames = h.settle(g, c)
  assert h.channel_ids(frames, "CHANNEL_CREATE") == [int.to_string(text)]
  h.stop(g)
}

pub fn role_grant_reveals_hidden_channel_test() {
  let g = basic()
  let c = h.join(g, 3)
  h.dispatch(
    g,
    "GUILD_ROLE_CREATE",
    "{\"guild_id\":\"100\",\"role\":" <> h.role_json(600, 0, 1) <> "}",
  )
  h.dispatch(
    g,
    "CHANNEL_UPDATE",
    h.channel_json(
      hidden,
      0,
      "["
        <> h.overwrite_json(guild, 0, 0, view)
        <> ","
        <> h.overwrite_json(600, 0, view, 0)
        <> "]",
    ),
  )
  let _ = h.settle(g, c)
  h.dispatch(
    g,
    "GUILD_MEMBER_UPDATE",
    "{\"guild_id\":\"100\","
      <> string.drop_start(h.member_json(3, "charlie", [600]), 1),
  )
  let frames = h.settle(g, c)
  assert h.channel_ids(frames, "CHANNEL_CREATE") == [int.to_string(hidden)]
  h.dispatch(
    g,
    "GUILD_MEMBER_UPDATE",
    "{\"guild_id\":\"100\","
      <> string.drop_start(h.member_json(3, "charlie", []), 1),
  )
  let frames = h.settle(g, c)
  assert h.channel_ids(frames, "CHANNEL_DELETE") == [int.to_string(hidden)]
  h.stop(g)
}

pub fn subscribe_without_view_members_is_ignored_test() {
  let g =
    h.start(
      guild,
      1,
      [h.role_json(guild, view_and_send, 0)],
      [
        h.channel_json(
          text,
          0,
          "[" <> h.overwrite_json(guild, 0, 0, view_members_bit) <> "]",
        ),
      ],
      [h.member_json(1, "owner", []), h.member_json(3, "charlie", [])],
    )
  let c = h.join(g, 3)
  h.subscribe(g, c, text, [#(0, 99)])
  assert h.list_updates(h.settle(g, c)) == []
  let a = h.join(g, 1)
  h.subscribe(g, a, text, [#(0, 99)])
  assert h.list_updates(h.settle(g, a)) != []
  h.stop(g)
}

pub fn nickname_change_reorders_list_test() {
  let g = basic()
  let #(a, first) = subscribed(g, 1)
  h.dispatch(
    g,
    "GUILD_MEMBER_UPDATE",
    "{\"guild_id\":\"100\",\"nick\":\"aaron\","
      <> string.drop_start(h.member_json(3, "charlie", []), 1),
  )
  let current = h.last_list(h.settle(g, a), first)
  assert h.rows(current) == ["g:online", "1", "g:offline", "3", "2"]
  assert_consistent(g, current)
  h.stop(g)
}

fn member_event(user: Int, name: String, roles: List(Int)) -> String {
  "{\"guild_id\":\"100\","
  <> string.drop_start(h.member_json(user, name, roles), 1)
}

fn online_bravo(g: h.Guild) -> h.Client {
  let b = h.join(g, 2)
  h.set_presence(g, h.status(2, presence.Online))
  h.set_presence(g, h.status(2, presence.Idle))
  process.sleep(400)
  b
}

pub fn role_hoist_change_regroups_subscribed_lists_test() {
  let g = basic()
  let #(a, first) = subscribed(g, 1)
  let _ = online_bravo(g)
  let current = h.last_list(h.settle(g, a), first)
  assert h.rows(current) == ["g:500", "2", "g:online", "1", "g:offline", "3"]
  h.dispatch(
    g,
    "GUILD_ROLE_UPDATE",
    "{\"guild_id\":\"100\",\"role\":" <> h.role_json(500, 0, 5) <> "}",
  )
  let current = h.last_list(h.settle(g, a), current)
  assert h.rows(current) == ["g:online", "2", "1", "g:offline", "3"]
  assert_consistent(g, current)
  h.dispatch(
    g,
    "GUILD_ROLE_DELETE",
    "{\"guild_id\":\"100\",\"role_id\":\"500\"}",
  )
  let current = h.last_list(h.settle(g, a), current)
  assert_consistent(g, current)
  h.stop(g)
}

pub fn channel_delete_and_create_keep_lists_consistent_test() {
  let g = basic()
  let #(a, first) = subscribed(g, 1)
  h.dispatch(
    g,
    "CHANNEL_CREATE",
    h.channel_json(13, 0, "[" <> h.overwrite_json(500, 0, 0, view) <> "]"),
  )
  let _ = h.settle(g, a)
  h.subscribe(g, a, 13, [#(0, 99)])
  let frames = h.settle(g, a)
  let assert Ok(on_new) = list.last(h.list_updates(frames))
  assert h.rows(on_new) == ["g:online", "1", "g:offline", "3"]
  h.dispatch(g, "CHANNEL_DELETE", h.channel_json(text, 0, "[]"))
  let frames = h.settle(g, a)
  assert h.channel_ids(frames, "CHANNEL_DELETE") == [int.to_string(text)]
  let _ = first
  h.dispatch(g, "CHANNEL_UPDATE", h.channel_json(13, 0, "[]"))
  let current = h.last_list(h.settle(g, a), on_new)
  assert h.rows(current) == ["g:online", "1", "g:offline", "2", "3"]
  let fresh = h.fresh_view(g, 1, 13, [#(0, 99)])
  assert current.text == fresh.text
  h.stop(g)
}

pub fn reload_rebuilds_lists_for_subscribers_test() {
  let g = basic()
  let #(a, first) = subscribed(g, 1)
  h.reload(
    g,
    h.loaded(
      guild,
      1,
      [everyone(), h.hoisted_role_json(500, 0, 5)],
      [h.channel_json(text, 0, "[]")],
      [h.member_json(1, "owner", []), h.member_json(3, "charlie", [])],
    ),
  )
  let current = h.last_list(h.settle(g, a), first)
  assert h.rows(current) == ["g:online", "1", "g:offline", "3"]
  assert_consistent(g, current)
  h.dispatch(g, "GUILD_MEMBER_ADD", member_event(5, "echo", []))
  let current = h.last_list(h.settle(g, a), current)
  assert h.rows(current) == ["g:online", "1", "g:offline", "3", "5"]
  assert_consistent(g, current)
  h.stop(g)
}

fn crowd(count: Int) -> h.Guild {
  h.start(
    guild,
    1,
    [everyone()],
    [h.channel_json(text, 0, "[]")],
    int.range(1, count + 1, [], fn(acc, user) {
      [h.member_json(user, "m" <> pad(user), []), ..acc]
    }),
  )
}

fn pad(value: Int) -> String {
  string.pad_start(int.to_string(value), 4, "0")
}

pub fn range_changes_sync_only_the_new_part_test() {
  let g = crowd(250)
  let a = h.join(g, 1)
  h.subscribe(g, a, text, [#(0, 99)])
  let assert [first] = h.list_updates(h.settle(g, a))
  assert list.map(h.ops(first), fn(op) { op.range }) == [[0, 99]]
  h.subscribe(g, a, text, [#(0, 99), #(100, 199)])
  let assert [grown] = h.list_updates(h.settle(g, a))
  assert list.map(h.ops(grown), fn(op) { op.range }) == [[100, 199]]
  h.subscribe(g, a, text, [#(0, 99)])
  assert h.list_updates(h.settle(g, a)) == []
  h.subscribe(g, a, text, [#(0, 99)])
  let assert [again] = h.list_updates(h.settle(g, a))
  assert list.map(h.ops(again), fn(op) { op.range }) == [[0, 99]]
  h.subscribe(g, a, text, [])
  let _ = h.settle(g, a)
  h.dispatch(g, "GUILD_MEMBER_ADD", member_event(900, "zzz", []))
  assert h.list_updates(h.settle(g, a)) == []
  h.stop(g)
}

pub fn switching_lists_moves_the_subscription_test() {
  let g =
    h.start(
      guild,
      1,
      [everyone()],
      [h.channel_json(text, 0, "[]"), h.channel_json(13, 0, "[]")],
      [h.member_json(1, "owner", []), h.member_json(3, "charlie", [])],
    )
  let a = h.join(g, 1)
  h.subscribe(g, a, text, [#(0, 99)])
  let _ = h.settle(g, a)
  h.subscribe(g, a, 13, [#(0, 99)])
  let assert [moved] = h.list_updates(h.settle(g, a))
  assert h.ops(moved) != []
  h.dispatch(g, "GUILD_MEMBER_ADD", member_event(4, "delta", []))
  let updates = h.list_updates(h.settle(g, a))
  assert list.length(updates) == 1
  let assert [only] = updates
  assert string.contains(only.text, "\"channel_id\":\"13\"")
  h.stop(g)
}

pub fn role_churn_compaction_keeps_guild_views_right_test() {
  let roles =
    int.range(0, 13, [], fn(acc, i) { [h.role_json(200 + i, 0, 2 + i), ..acc] })
  let g =
    h.start(
      guild,
      1,
      [everyone(), h.role_json(900, 0, 1), ..roles],
      [
        h.channel_json(text, 0, "[]"),
        h.channel_json(
          hidden,
          0,
          "["
            <> h.overwrite_json(guild, 0, 0, view)
            <> ","
            <> h.overwrite_json(900, 0, view, 0)
            <> "]",
        ),
      ],
      [
        h.member_json(1, "owner", []),
        h.member_json(3, "charlie", [900]),
        h.member_json(4, "churn", []),
      ],
    )
  let c = h.join(g, 3)
  let #(a, first) = subscribed(g, 1)
  h.dispatch_many(
    g,
    int.range(1, 4300, [], fn(acc, mask) {
      [
        #("GUILD_MEMBER_UPDATE", member_event(4, "churn", subset_roles(mask))),
        ..acc
      ]
    })
      |> list.reverse,
  )
  let _ = h.settle(g, c)
  h.dispatch(
    g,
    "CHANNEL_UPDATE",
    h.channel_json(hidden, 0, "[" <> h.overwrite_json(guild, 0, 0, view) <> "]"),
  )
  let frames = h.settle(g, c)
  assert h.channel_ids(frames, "CHANNEL_DELETE") == [int.to_string(hidden)]
  h.dispatch(
    g,
    "MESSAGE_CREATE",
    "{\"id\":\"77\",\"channel_id\":\"11\",\"content\":\"hi\",\"author\":{\"id\":\"1\",\"username\":\"owner\"}}",
  )
  let frames = h.settle(g, c)
  assert list.length(h.named(frames, "MESSAGE_CREATE")) == 1
  h.dispatch(g, "GUILD_MEMBER_UPDATE", member_event(4, "churn", [900]))
  let current = h.last_list(h.settle(g, a), first)
  assert_consistent(g, current)
  h.stop(g)
}

fn subset_roles(mask: Int) -> List(Int) {
  int.range(0, 13, [], fn(acc, bit) {
    case int.bitwise_and(int.bitwise_shift_right(mask, bit), 1) {
      1 -> [200 + bit, ..acc]
      _ -> acc
    }
  })
}
