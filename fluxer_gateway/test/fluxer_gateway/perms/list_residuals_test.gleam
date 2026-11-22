// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/guild/data
import fluxer_gateway/guild/data/decode as data_decode
import gleam/dict
import gleam/list
import gleam/string
import support/perms_ml_harness as h

const guild = 100

const text = 11

const hidden = 12

const view_and_send = 3072

const view_members = 18_014_398_509_481_984

const view = 1024

const insider = 600

fn everyone() -> String {
  h.role_json(guild, view_and_send + view_members, 0)
}

fn member_event(user: Int, name: String, roles: List(Int)) -> String {
  "{\"guild_id\":\"100\","
  <> string.drop_start(h.member_json(user, name, roles), 1)
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

fn loaded_disabled(
  roles: List(String),
  channels: List(String),
  members: List(String),
) -> data.Loaded {
  let collected =
    dict.from_list([
      #(
        "guild",
        h.whole(
          "{\"id\":\"100\",\"name\":\"g\",\"owner_id\":\"1\",\"disabled_operations\":\"64\"}",
        ),
      ),
      #("roles", h.whole("[" <> string.join(roles, ",") <> "]")),
      #("channels", h.whole("[" <> string.join(channels, ",") <> "]")),
      #("members", h.whole("[" <> string.join(members, ",") <> "]")),
    ])
  let assert Ok(found) = data_decode.collections(h.guild_id(guild), collected)
  found
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

fn disable(g: h.Guild) -> Nil {
  h.dispatch(
    g,
    "GUILD_UPDATE",
    "{\"id\":\"100\",\"disabled_operations\":\"64\"}",
  )
}

fn enable(g: h.Guild) -> Nil {
  h.dispatch(
    g,
    "GUILD_UPDATE",
    "{\"id\":\"100\",\"disabled_operations\":\"0\"}",
  )
}

pub fn subscribing_while_updates_are_off_gets_the_list_when_they_return_test() {
  let g =
    h.start(guild, 1, [everyone()], [h.channel_json(text, 0, "[]")], [
      h.member_json(1, "owner", []),
      h.member_json(3, "charlie", []),
    ])
  let a = h.join(g, 1)
  disable(g)
  let _ = h.settle(g, a)
  h.subscribe(g, a, text, [#(0, 99)])
  assert h.list_updates(h.settle(g, a)) == []
  h.dispatch(g, "GUILD_MEMBER_ADD", member_event(4, "alpha", []))
  assert h.list_updates(h.settle(g, a)) == []
  enable(g)
  let assert [synced] = h.list_updates(h.settle(g, a))
  assert list.contains(h.rows(synced), "4")
  same_as_fresh(g, synced, 1, text, [#(0, 99)])
  h.stop(g)
}

pub fn unsubscribing_while_updates_are_off_is_kept_test() {
  let g =
    h.start(guild, 1, [everyone()], [h.channel_json(text, 0, "[]")], [
      h.member_json(1, "owner", []),
      h.member_json(3, "charlie", []),
    ])
  let a = h.join(g, 1)
  h.subscribe(g, a, text, [#(0, 99)])
  let assert [_] = h.list_updates(h.settle(g, a))
  disable(g)
  h.subscribe(g, a, text, [])
  let _ = h.settle(g, a)
  enable(g)
  assert h.list_updates(h.settle(g, a)) == []
  h.stop(g)
}

pub fn reload_while_updates_are_off_keeps_subscriptions_test() {
  let roles = [everyone()]
  let channels = [h.channel_json(text, 0, "[]")]
  let members = [h.member_json(1, "owner", []), h.member_json(3, "charlie", [])]
  let g = h.start(guild, 1, roles, channels, members)
  let a = h.join(g, 1)
  h.subscribe(g, a, text, [#(0, 99)])
  let assert [_] = h.list_updates(h.settle(g, a))
  disable(g)
  let _ = h.settle(g, a)
  h.reload(
    g,
    loaded_disabled(roles, channels, [h.member_json(5, "echo", []), ..members]),
  )
  assert h.list_updates(h.settle(g, a)) == []
  enable(g)
  let assert [synced] = h.list_updates(h.settle(g, a))
  assert list.contains(h.rows(synced), "5")
  same_as_fresh(g, synced, 1, text, [#(0, 99)])
  h.stop(g)
}

pub fn parked_subscription_returns_when_a_role_change_restores_view_test() {
  let roles = [everyone(), h.role_json(insider, 0, 1)]
  let channels = [h.channel_json(text, 0, "[]"), hidden_channel()]
  let g =
    h.start(guild, 1, roles, channels, [
      h.member_json(1, "owner", []),
      h.member_json(3, "charlie", [insider]),
      h.member_json(4, "delta", [insider]),
    ])
  let c = h.join(g, 3)
  h.subscribe(g, c, hidden, [#(0, 99)])
  let assert [_] = h.list_updates(h.settle(g, c))
  h.reload(
    g,
    h.loaded(guild, 1, roles, channels, [
      h.member_json(1, "owner", []),
      h.member_json(3, "charlie", []),
      h.member_json(4, "delta", [insider]),
    ]),
  )
  assert h.list_updates(h.settle(g, c)) == []
  h.dispatch(g, "GUILD_MEMBER_UPDATE", member_event(3, "charlie", [insider]))
  let assert [synced] = h.list_updates(h.settle(g, c))
  same_as_fresh(g, synced, 3, hidden, [#(0, 99)])
  h.dispatch(g, "GUILD_MEMBER_ADD", member_event(5, "echo", [insider]))
  let current = h.last_list(h.settle(g, c), synced)
  assert list.contains(h.rows(current), "5")
  same_as_fresh(g, current, 3, hidden, [#(0, 99)])
  h.stop(g)
}

pub fn parked_subscription_of_a_departed_session_is_not_replayed_test() {
  let roles = [everyone(), h.role_json(insider, 0, 1)]
  let channels = [h.channel_json(text, 0, "[]"), hidden_channel()]
  let g =
    h.start(guild, 1, roles, channels, [
      h.member_json(1, "owner", []),
      h.member_json(3, "charlie", [insider]),
    ])
  let c = h.join(g, 3)
  h.subscribe(g, c, hidden, [#(0, 99)])
  let assert [_] = h.list_updates(h.settle(g, c))
  h.reload(
    g,
    h.loaded(guild, 1, roles, channels, [
      h.member_json(1, "owner", []),
      h.member_json(3, "charlie", []),
    ]),
  )
  let _ = h.settle(g, c)
  h.leave(g, c)
  h.dispatch(g, "GUILD_MEMBER_UPDATE", member_event(3, "charlie", [insider]))
  assert h.list_updates(h.settle(g, c)) == []
  let fresh = h.fresh_view(g, 3, hidden, [#(0, 99)])
  assert list.contains(h.rows(fresh), "3")
  h.stop(g)
}
