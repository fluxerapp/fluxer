// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/presence
import gleam/int
import gleam/list
import gleam/string
import support/perms_ml_harness as h

const guild = 100

const text = 11

const hidden = 12

const view_and_send = 3072

const view_members = 18_014_398_509_481_984

const view = 1024

const hoisted = 500

const insider = 600

fn everyone() -> String {
  h.role_json(guild, view_and_send + view_members, 0)
}

fn member_event(user: Int, name: String, roles: List(Int)) -> String {
  "{\"guild_id\":\"100\","
  <> string.drop_start(h.member_json(user, name, roles), 1)
}

fn role_event(name: String, role: String) -> #(String, String) {
  #(name, "{\"guild_id\":\"100\",\"role\":" <> role <> "}")
}

fn hidden_channel() -> String {
  h.channel_json(
    hidden,
    0,
    "["
      <> h.overwrite_json(guild, 0, 0, view)
      <> ","
      <> h.overwrite_json(insider, 0, view, 0)
      <> "]",
  )
}

fn latest(g: h.Guild, client: h.Client, previous: h.Frame) -> h.Frame {
  h.last_list(h.settle(g, client), previous)
}

fn same_as_fresh(
  g: h.Guild,
  found: h.Frame,
  viewer: Int,
  channel: Int,
  ranges: List(#(Int, Int)),
) -> Nil {
  let fresh = h.fresh_view(g, viewer, channel, ranges)
  assert h.rows(found) == h.rows(fresh)
  assert found.text == fresh.text
}

pub fn regaining_view_sends_a_single_list_sync_test() {
  let g =
    h.start(
      guild,
      1,
      [everyone(), h.role_json(insider, 0, 1)],
      [h.channel_json(text, 0, "[]"), hidden_channel()],
      [
        h.member_json(1, "owner", []),
        h.member_json(3, "charlie", [insider]),
        h.member_json(4, "delta", [insider]),
      ],
    )
  let c = h.join(g, 3)
  h.subscribe(g, c, hidden, [#(0, 99)])
  let assert [_] = h.list_updates(h.settle(g, c))
  h.dispatch(g, "GUILD_MEMBER_UPDATE", member_event(3, "charlie", []))
  let frames = h.settle(g, c)
  assert h.channel_ids(frames, "CHANNEL_DELETE") == [int.to_string(hidden)]
  assert h.list_updates(frames) == []
  h.dispatch(g, "GUILD_MEMBER_UPDATE", member_event(3, "charlie", [insider]))
  let frames = h.settle(g, c)
  assert h.channel_ids(frames, "CHANNEL_CREATE") == [int.to_string(hidden)]
  let updates = h.list_updates(frames)
  let assert [synced] = updates
  same_as_fresh(g, synced, 3, hidden, [#(0, 99)])
  h.stop(g)
}

pub fn channel_reveal_syncs_each_subscriber_once_test() {
  let g =
    h.start(
      guild,
      1,
      [everyone(), h.role_json(insider, 0, 1)],
      [h.channel_json(text, 0, "[]"), hidden_channel()],
      [
        h.member_json(1, "owner", []),
        h.member_json(3, "charlie", [insider]),
        h.member_json(4, "delta", [insider]),
        h.member_json(5, "echo", []),
      ],
    )
  let c = h.join(g, 3)
  let d = h.join(g, 4)
  h.subscribe(g, c, hidden, [#(0, 99)])
  h.subscribe(g, d, hidden, [#(0, 99)])
  let _ = h.settle(g, c)
  let _ = h.drain(d)
  h.dispatch(
    g,
    "CHANNEL_UPDATE",
    h.channel_json(hidden, 0, "[" <> h.overwrite_json(guild, 0, 0, view) <> "]"),
  )
  let _ = h.settle(g, c)
  let _ = h.drain(d)
  h.dispatch(g, "CHANNEL_UPDATE", hidden_channel())
  let from_c = h.settle(g, c)
  let from_d = h.drain(d)
  let assert [synced_c] = h.list_updates(from_c)
  let assert [synced_d] = h.list_updates(from_d)
  assert synced_c.text == synced_d.text
  same_as_fresh(g, synced_c, 3, hidden, [#(0, 99)])
  h.stop(g)
}

fn crowd(total: Int) -> List(String) {
  int.range(total + 10, 10, [], fn(acc, user) {
    let roles = case user % 7 {
      0 -> [hoisted]
      _ -> []
    }
    [h.member_json(user, "m" <> int.to_string(user), roles), ..acc]
  })
}

pub fn large_guild_role_change_resyncs_every_subscriber_consistently_test() {
  let g =
    h.start(
      guild,
      1,
      [everyone(), h.role_json(hoisted, 0, 5)],
      [h.channel_json(text, 0, "[]")],
      [h.member_json(1, "owner", []), ..crowd(20_000)],
    )
  let online = int.range(11, 31, [], fn(acc, user) { [h.join(g, user), ..acc] })
  let assert [watcher, ..] = online
  h.subscribe(g, watcher, text, [#(0, 99)])
  let assert Ok(first) = list.last(h.list_updates(h.settle(g, watcher)))
  let others = h.join(g, 40)
  h.subscribe(g, others, text, [#(0, 99)])
  let _ = h.settle(g, others)
  let _ = h.drain(watcher)
  h.dispatch(
    g,
    "GUILD_ROLE_UPDATE",
    "{\"guild_id\":\"100\",\"role\":"
      <> h.hoisted_role_json(hoisted, 0, 5)
      <> "}",
  )
  let from_watcher = h.settle(g, watcher)
  let from_others = h.drain(others)
  let assert [synced] = h.list_updates(from_watcher)
  let assert [also] = h.list_updates(from_others)
  assert synced.text == also.text
  assert synced.text != first.text
  assert list.contains(h.rows(synced), "g:" <> int.to_string(hoisted))
  same_as_fresh(g, synced, 30, text, [#(0, 99)])
  h.dispatch(
    g,
    "GUILD_ROLE_UPDATE",
    "{\"guild_id\":\"100\",\"role\":" <> h.role_json(hoisted, 0, 5) <> "}",
  )
  let back = latest(g, watcher, synced)
  assert !list.contains(h.rows(back), "g:" <> int.to_string(hoisted))
  same_as_fresh(g, back, 30, text, [#(0, 99)])
  h.stop(g)
}

pub fn role_compaction_during_presence_and_member_churn_keeps_lists_right_test() {
  let churn_roles =
    int.range(0, 13, [], fn(acc, i) { [h.role_json(200 + i, 0, 2 + i), ..acc] })
  let g =
    h.start(
      guild,
      1,
      [everyone(), h.hoisted_role_json(hoisted, 0, 1), ..churn_roles],
      [h.channel_json(text, 0, "[]"), hidden_channel()],
      [
        h.member_json(1, "owner", []),
        h.member_json(3, "charlie", [insider]),
        h.member_json(4, "churn", []),
        ..crowd(40)
      ],
    )
  let a = h.join(g, 1)
  h.subscribe(g, a, text, [#(0, 99)])
  let assert Ok(first) = list.last(h.list_updates(h.settle(g, a)))
  let c = h.join(g, 3)
  h.subscribe(g, c, hidden, [#(0, 99)])
  let _ = h.settle(g, c)
  let joined = int.range(11, 31, [], fn(acc, user) { [h.join(g, user), ..acc] })
  let churn =
    int.range(1, 4300, [], fn(acc, mask) {
      let update = #(
        "GUILD_MEMBER_UPDATE",
        member_event(4, "churn", subset_roles(mask)),
      )
      let extra = case mask % 500 {
        0 -> [
          #(
            "GUILD_MEMBER_UPDATE",
            member_event(11 + mask / 500, "moved", [hoisted]),
          ),
          role_event(
            "GUILD_ROLE_UPDATE",
            h.hoisted_role_json(hoisted, 0, 1 + mask / 500),
          ),
        ]
        250 -> [
          #(
            "GUILD_MEMBER_ADD",
            member_event(9000 + mask / 500, "late", [insider]),
          ),
        ]
        _ -> []
      }
      list.append(list.reverse(extra), [update, ..acc])
    })
    |> list.reverse
  h.dispatch_many(g, churn)
  list.each(int.range(11, 21, [], fn(acc, user) { [user, ..acc] }), fn(user) {
    h.set_presence(g, h.status(user, presence.Dnd))
  })
  let _ = h.settle(g, c)
  let current = latest(g, a, first)
  same_as_fresh(g, current, 1, text, [#(0, 99)])
  h.dispatch(g, "GUILD_MEMBER_UPDATE", member_event(4, "churn", [insider]))
  let current = latest(g, a, current)
  same_as_fresh(g, current, 1, text, [#(0, 99)])
  let hidden_view = latest(g, c, first)
  same_as_fresh(g, hidden_view, 3, hidden, [#(0, 99)])
  assert list.contains(h.rows(hidden_view), "4")
  list.each(joined, h.leave(g, _))
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

pub fn reenabling_list_updates_resyncs_subscribers_test() {
  let g =
    h.start(guild, 1, [everyone()], [h.channel_json(text, 0, "[]")], [
      h.member_json(1, "owner", []),
      h.member_json(3, "charlie", []),
    ])
  let a = h.join(g, 1)
  h.subscribe(g, a, text, [#(0, 99)])
  let assert [_] = h.list_updates(h.settle(g, a))
  h.dispatch(
    g,
    "GUILD_UPDATE",
    "{\"id\":\"100\",\"disabled_operations\":\"64\"}",
  )
  let _ = h.settle(g, a)
  h.dispatch(g, "GUILD_MEMBER_ADD", member_event(4, "alpha", []))
  assert h.list_updates(h.settle(g, a)) == []
  h.dispatch(
    g,
    "GUILD_UPDATE",
    "{\"id\":\"100\",\"disabled_operations\":\"0\"}",
  )
  let assert [synced] = h.list_updates(h.settle(g, a))
  assert list.contains(h.rows(synced), "4")
  same_as_fresh(g, synced, 1, text, [#(0, 99)])
  h.stop(g)
}

pub fn reload_that_hides_a_subscribed_channel_sends_no_list_test() {
  let roles = [everyone(), h.role_json(insider, 0, 1)]
  let members = [
    h.member_json(1, "owner", []),
    h.member_json(3, "charlie", [insider]),
    h.member_json(4, "delta", []),
  ]
  let g =
    h.start(
      guild,
      1,
      roles,
      [h.channel_json(text, 0, "[]"), hidden_channel()],
      members,
    )
  let c = h.join(g, 3)
  h.subscribe(g, c, hidden, [#(0, 99)])
  let assert [_] = h.list_updates(h.settle(g, c))
  h.reload(
    g,
    h.loaded(
      guild,
      1,
      roles,
      [
        h.channel_json(text, 0, "[]"),
        h.channel_json(
          hidden,
          0,
          "[" <> h.overwrite_json(guild, 0, 0, view) <> "]",
        ),
      ],
      members,
    ),
  )
  assert h.list_updates(h.settle(g, c)) == []
  h.dispatch(g, "CHANNEL_UPDATE", hidden_channel())
  let frames = h.settle(g, c)
  let assert [synced] = h.list_updates(frames)
  same_as_fresh(g, synced, 3, hidden, [#(0, 99)])
  h.dispatch(g, "GUILD_MEMBER_ADD", member_event(5, "echo", [insider]))
  let current = latest(g, c, synced)
  assert list.contains(h.rows(current), "5")
  same_as_fresh(g, current, 3, hidden, [#(0, 99)])
  h.stop(g)
}
